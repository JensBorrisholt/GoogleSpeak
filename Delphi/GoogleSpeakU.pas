unit GoogleSpeakU;

interface

uses
  System.Classes, System.SysUtils, VCL.Forms, VCL.MPlayer, VCL.Controls, LanguagesU;
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
    function DownloadFile(aText: String; aLanguage: TLanguage): string;
  published
    property Language: TLanguage read FLanguage write FLanguage;
  public
    constructor Create(AOwner: TWinControl; AUseCache: Boolean = True; aLanguageCode: string = 'da'); reintroduce;
    destructor Destroy; override;
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

function TGoogleSpeak.DownloadFile(aText: String; aLanguage: TLanguage): string;
var
  Url: String;
begin
  Url := 'https://translate.googleapis.com/translate_tts?ie=UTF-8&q=' + TIdURI.PathEncode(aText) + '&tl=' + aLanguage.Code + '&total=1&idx=0&textlen=' + aText.Length.ToString + '&client=gtx';
  Result := FDownloadFolder + TCrc32.Hash(aText).ToString + '.mp3';

  if (FUseCache and TFile.Exists(Result)) then
    exit;

  URLDownloadToFile(nil, pchar(Url), pchar(Result), 0, nil);
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

  FMediaPlayer.FileName := DownloadFile(FBuffer[0], TLanguage(FBuffer.Objects[0]));
  FBuffer.Delete(0);
  FMediaPlayer.Open;
  FMediaPlayer.Play;
end;

end.
