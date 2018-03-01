unit GoogleSpeakU;

interface

uses
  System.Classes, System.SysUtils, System.Threading,  VCL.Forms, VCL.MPlayer, VCL.Controls, LanguagesU;
{$M+}

type
  TGoogleSpeak = class(TComponent)
  private
    FMediaPlayer: TMediaPlayer;
    FUseCache: Boolean;
    FDownloadFolder: string;
    FBuffer: TStringlist;
    FLanguage: TLanguage;
    procedure SayNext;
    procedure MediaPlayerNotify(Sender: TObject);
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
  IdURI, UrlMon, CRC32U;

{ TGoogleSpeak }

constructor TGoogleSpeak.Create(AOwner: TWinControl; AUseCache: Boolean; aLanguageCode: string);
begin
  inherited Create(AOwner);
  FUseCache := AUseCache;
  FDownloadFolder := ExtractFilePath(ParamStr(0)) + 'Cache\';
  ForceDirectories(FDownloadFolder);

  FBuffer := TStringlist.Create;
  FMediaPlayer := TMediaPlayer.Create(self);
  FMediaPlayer.OnNotify := MediaPlayerNotify;

  FMediaPlayer.Parent := AOwner;
  FMediaPlayer.Visible := False;
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
  Result := TTask.Future<string>(  function : string
  begin
    Result := FDownloadFolder +  TCrc32.Hash(aLanguage + ' ' + aText).ToString + '.mp3';
    if (FUseCache and TFile.Exists(Result)) then
      exit;

    URLDownloadToFile(nil, pchar(Url), pchar(Result), 0, nil);
  end
  ).Start;
end;

function TGoogleSpeak.DownloadFile(aText: String; aLanguage: TLanguage): IFuture<string>;
begin
  if aLanguage = nil then
    aLanguage = FLanguage;
  Result := DownloadFile(aText,aLanguage.Code);
end;

procedure TGoogleSpeak.MediaPlayerNotify(Sender: TObject);
begin
  SayNext;
end;

procedure TGoogleSpeak.Say(aText: String; aLanguage: TLanguage = nil);
begin
  if aLanguage = nil then
    aLanguage := FLanguage;

  FBuffer.AddObject(aText, aLanguage);
  if not(FMediaPlayer.Mode in [TMPModes.mpPlaying, TMPModes.mpSeeking, TMPModes.mpPaused]) then
    SayNext;
end;

procedure TGoogleSpeak.Say(aText, aLanguageCode: string);
var
  Language : TLanguage;
begin
  Language := TLanguages.FromCode(aLanguageCode);
  Say(aText, Language);
end;

procedure TGoogleSpeak.SayNext;
begin
  if FBuffer.Count = 0 then
    exit;

  FMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0])).Value;
  FBuffer.Delete(0);
  FMediaPlayer.Open;
  FMediaPlayer.Play;
end;

end.
