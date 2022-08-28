unit principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm5 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Teste;
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

uses IsolateTransactionThread;

{ TForm5 }

procedure TForm5.Button1Click(Sender: TObject);
begin
 ExecuteThread_Manutencaoprodutos(5000);
end;

procedure TForm5.Teste;
begin

end;

end.
