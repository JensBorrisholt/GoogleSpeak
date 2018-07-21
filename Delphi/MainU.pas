unit MainU;

interface

uses
  Vcl.Forms, GoogleSpeakU;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  with TGoogleSpeak.Create(Self) do
  begin
    Say('Her er 1 hest');
    Say('Her er 2 heste');
    Say('3 køer er der her');
    Say('og 4 æsler her');
  end;
end;

end.
