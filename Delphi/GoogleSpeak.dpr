program GoogleSpeak;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {FormMain},
  GoogleSpeakU in 'GoogleSpeakU.pas',
  LanguagesU in 'LanguagesU.pas',
  EventDispatcher in 'EventDispatcher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
