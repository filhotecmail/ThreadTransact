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
## Create da Thread
No on create da Thread iremos construir todos os recursos possíveis para se trabalhar com transações com firedac.

```Delphi
constructor TTransactionData.Create;
begin
 inherited Create(True);
 
 FreeOnTerminate:= True;
 
{$REGION 'Criação dos Componentes de transação, Nao precisa Destruir
  pois a ThreadClass é o Pai de todos , ele quem vai destruir ao terminar a Thread'}

  FCon := TFDConnection.Create( nil );
  FTransact := TFDTransaction.Create( nil );
  FQuery := TFDQuery.Create( nil );
  FPsys := TFDPhysFBDriverLink.Create( nil );
  FLogTransaction := TStringList.Create;

{$ENDREGION}

 {$REGION 'Prepara os componentesde Transação'}
 
  FPsys.ThreadSafe := True;

  FQuery.Connection := FCon;
  FTransact.Connection := FCon;

  FCon.UpdateOptions.LockWait := False;
  Fcon.UpdateOptions.AutoCommitUpdates;
  FCon.LoginPrompt := False;
  FCon.ResourceOptions.AutoConnect   := True;
  FCon.ResourceOptions.AutoReconnect := True;
  
  FTransact.Options.AutoCommit := True;
  FTransact.Options.ReadOnly := False;
  FTransact.Options.Isolation := xiReadCommitted;
  
 {$ENDREGION}

  {$REGION 'Assinatura dos Eventos do conector'}

   with FCon do
   begin
   
    AfterCommit     := oConAfterCommit;
    AfterConnect    := oConAfterConnect;
    AfterDisconnect := oConAfterDisconnect;
    AfterRollback   := oConAfterRollback;
    AfterStartTransaction  := oConAfterStartTransaction;
    BeforeCommit           := oConBeforeCommit;
    BeforeConnect          := oConBeforeConnect;
    BeforeDisconnect       := oConBeforeDisconnect;
    BeforeRollback         := oConBeforeRollback;
    BeforeStartTransaction := oConBeforeStartTransaction;
    onError                := oConError;
    onLogin                := oConLogin;
    onLost                 := oConLost;
    onRecover              := oConRecover;
    onRestored             := oConRestored;
    
   end;

  {$ENDREGION}

 OnTerminate := DoOnTerminate;
 

end;

```




