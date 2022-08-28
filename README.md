## Uma construção de Isolate Transaction Thread
Objetivo .:

Simular transações com banco de dados Firebird, por meio de transações seguras em Thread enviadas ao processador para que se completem e terminem seu ciclo.

Ex: de uso.

 ```Delphi
 procedure TForm5.Button1Click(Sender: TObject);
begin
 ExecuteThread_Manutencaoprodutos(5000);
end;
```


