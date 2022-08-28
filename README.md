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

## O MétodoAsync.Await
Configure o Tempo de delay para inicio do procedimento.

Para isso foi criado um método Async, que vai executar a thread.

```Delphi

 Async
   .AWait(procedure
     begin
      with TTransactionData.Create do
      begin
        DatabaseUserName := 'sysdba';
        DatabasePassword := '1234';
        DatabasePort     := 3050;
        Database         := 'c:\meubanco\banco.fdb';
        Host             := '127.0.0.1';
        Start;
      end;
     end,dwMilliseconds);
     
```


