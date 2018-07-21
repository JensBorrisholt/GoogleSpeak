unit GoogleSpeakU;

interface

uses
  System.Classes, System.SysUtils, System.Threading, VCL.Forms, VCL.MPlayer, VCL.Controls, LanguagesU;

{$M+}

type
  TGoogleSpeak = class(TComponent)
  private
    MediaPlayer0: TMediaPlayer;
    MediaPlayer1: TMediaPlayer;

    CurrentMediaPlayer: TMediaPlayer;
    NextMediaPlayer: TMediaPlayer;

    FUseCache: Boolean;
    FDownloadFolder: string;
    FBuffer: TStringlist;
    FLanguage: TLanguage;
    procedure SayFirst;
    procedure SayNext;
  published
    property Language: TLanguage read FLanguage write FLanguage;
  public
    constructor Create(AOwner: TWinControl; AUseCache: Boolean = True; aLanguageCode: string = 'da'); reintroduce;
    destructor Destroy; override;

    function DownloadFile(aText: String; aLanguage: string): IFuture<string>; overload;
    function DownloadFile(aText: String; aLanguage: TLanguage = nil): IFuture<string>; overload;

    procedure Say(aText: String; aLanguage: TLanguage = nil); overload;
    procedure Say(aText: String; aLanguageCode: string); overload;
  end;

implementation

uses
  System.Types, System.IOUtils,
  IdURI, UrlMon, System.Hash, EventDispatcher;

{ TGoogleSpeak }

constructor TGoogleSpeak.Create(AOwner: TWinControl; AUseCache: Boolean; aLanguageCode: string);
  function ConstructMediaPlayer: TMediaPlayer;
  begin
    Result := TMediaPlayer.Create(Self);
    Result.OnNotify := TNotifyEventDispatcher.Construct(Result,
      procedure(Sender: TObject)
      begin
        SayNext;
      end);

    Result.Parent := AOwner;
    Result.Visible := False;
  end;

begin
  inherited Create(AOwner);
  FUseCache := AUseCache;
  FDownloadFolder := ExtractFilePath(ParamStr(0)) + 'Cache\';
  ForceDirectories(FDownloadFolder);
  MediaPlayer0 := ConstructMediaPlayer;
  MediaPlayer1 := ConstructMediaPlayer;
  FBuffer := TStringlist.Create;
  FLanguage := TLanguages.FromCode(aLanguageCode);
end;

destructor TGoogleSpeak.Destroy;
begin
  FBuffer.Free;
  if not FUseCache then
    TDirectory.Delete(FDownloadFolder);
  inherited;
end;

function TGoogleSpeak.DownloadFile(aText: String; aLanguage: string): IFuture<string>;
var
  Url: String;
begin
  if aLanguage = '' then
    aLanguage := FLanguage.Code;

  Url := 'https://translate.googleapis.com/translate_tts?ie=UTF-8&q=' + TIdURI.PathEncode(aText) + '&tl=' + aLanguage + '&total=1&idx=0&textlen=' + aText.Length.ToString + '&client=gtx';
  Result := TTask.Future<string>(
    function: string
    begin
      Result := FDownloadFolder + THashMD5.GetHashString((aLanguage + ' ' + aText).ToUpper) + '.mp3';
      if (FUseCache and TFile.Exists(Result)) then
        exit;

      URLDownloadToFile(nil, pchar(Url), pchar(Result), 0, nil);
    end).Start;
end;

function TGoogleSpeak.DownloadFile(aText: String; aLanguage: TLanguage): IFuture<string>;
begin
  if aLanguage = nil then
    aLanguage := FLanguage;

  Result := DownloadFile(aText, aLanguage.Code);
end;

procedure TGoogleSpeak.Say(aText: String; aLanguage: TLanguage = nil);
begin
  if aLanguage = nil then
    aLanguage := FLanguage;

  FBuffer.AddObject(aText, aLanguage);

  if (CurrentMediaPlayer = nil) or (not(CurrentMediaPlayer.Mode in [TMPModes.mpPlaying, TMPModes.mpSeeking, TMPModes.mpPaused])) then
    SayNext;
end;

procedure TGoogleSpeak.Say(aText, aLanguageCode: string);
begin
  Say(aText, TLanguages.FromCode(aLanguageCode));
end;

procedure TGoogleSpeak.SayFirst;
begin
  if FBuffer.Count = 0 then
    exit;

  CurrentMediaPlayer := MediaPlayer0;
  NextMediaPlayer := MediaPlayer1;

  CurrentMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0])).Value;
  CurrentMediaPlayer.Open;
  CurrentMediaPlayer.Play;
  FBuffer.Delete(0);

  if FBuffer.Count = 0 then
    exit;

  NextMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0])).Value;
  NextMediaPlayer.Open;
  FBuffer.Delete(0);
end;

procedure TGoogleSpeak.SayNext;
begin
  if CurrentMediaPlayer = nil then
  begin
    SayFirst;
    exit;
  end;

  if NextMediaPlayer.FileName <> '' then
  begin
    CurrentMediaPlayer := NextMediaPlayer;

    if CurrentMediaPlayer.FileName <> '' then
      CurrentMediaPlayer.Play;
  end;

  if FBuffer.Count = 0 then
  begin
    CurrentMediaPlayer.FileName := '';
    exit;
  end;

  if NextMediaPlayer.FileName = '' then
  begin
    CurrentMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0])).Value;
    CurrentMediaPlayer.Open;
    CurrentMediaPlayer.Play;
    FBuffer.Delete(0);
    exit;
  end;

  if FBuffer.Count = 0 then
  begin
    CurrentMediaPlayer.FileName := '';
    exit;
  end;

  if NextMediaPlayer = MediaPlayer0 then
    NextMediaPlayer := MediaPlayer1
  else
    NextMediaPlayer := MediaPlayer0;

  NextMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0])).Value;
  FBuffer.Delete(0);
  NextMediaPlayer.Open;
end;

end.
