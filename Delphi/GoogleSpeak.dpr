program GoogleSpeak;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {FormMain},
  GoogleSpeakU in 'GoogleSpeakU.pas',
  CRC32U in 'CRC32U.pas',
  LanguagesU in 'LanguagesU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
