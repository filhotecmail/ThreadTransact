program ThreadTransact;

uses
  Vcl.Forms,
  principal in '..\view\principal.pas' {Form5},
  IsolateTransactionThread in '..\commom\IsolateTransactionThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
