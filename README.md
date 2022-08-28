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

##Como trabalhar.
 Você vai trabalhar dentro do método DoTransactStatements, criando sua logica; 
 
 ```Delphi
 
function TTransactionData.DoTransactStatements: TTransactionData;
var
  I: Integer;
begin
 Result := Self;

  FTransactionStr := 'SELECT * FROM CADCLIENTES C WHERE CHAR_LENGTH(TRIM(C.USUARIO)) <= 0';
  FTransactValues := ['ADMINISTRADOR'];
 
 if FCon.Connected then
    FCon.CloneConnection;
    
 FTransact.StartTransaction;
 
 Assert( not FTransactionStr.Trim.IsEmpty , 'Não existe um SQL Montando de Update ou Insert !');
 Assert( Length(FTransactValues) > 0 , 'Você nao alimentou os parametros de valores');

    
    
 try 
 
  FQuery.Open(FTransactionStr);
  
 finally
 
  if (( FQuery.IsEmpty ) or ( FQuery.RecordCount <= 0 ) ) then
   begin
    WriteLog('Nao existem dados a serem atualizados!');
    raise Exception.Create('Nao existem dados a serem atualizados!');
   end;
     
 end;
 
  FQuery.First;
  FQuery.DisableControls;
  
 while not FQuery.Eof do
 begin  
 
  FQuery.Edit;
  
  FQuery.FieldValues['USUARIO'] := FTransactValues[0];
  
  FQuery.Post;
  
  FQuery.Next;   
  
 end;
 
 {$REGION 'Nao remova essas linhas'}
 
  FCon.CloneConnection;       
  TrimAppMemorySize;
  
 {$ENDREGION}
  
  
end;
 
 ```
 
 ### Dentro do método Execute você pode verificar como está sendo executado e fazer todas as validações possíveis que deseja
 
 ```Delphi
 
 
procedure TTransactionData.Execute;
begin
  inherited;

   While True do
    begin
    
     Sleep(100);  

     if FLogFilePatch.Trim.IsEmpty then
        FLogFilePatch:= ExtractFilePath(ParamStr(0));

     if FLogFilename.Trim.IsEmpty then
        FLogFilename := 'Threadlog.txt';   
    try 
    
     ValidaDadosBancodeDados
       .MontaConexao
        .DoTransactStatements; 
        
    except
    
     on E: Exception do
     begin
        WriteLog(E);
        Abort;
     end;
     
    end;
        
   end;
   
end;
 
 ```
 
 ##Exemplo de Log
 
 Se você não preencher as propriedades de LogPatch e LogFileName , o sistema automaticamente irá criar o arquivo de log na pasta da aplicação.
 ![image](https://user-images.githubusercontent.com/18727307/187077292-633d0d1d-e2f8-4fab-a749-0d18cd2b1e67.png)





