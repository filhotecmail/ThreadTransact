unit IsolateTransactionThread;

interface

Uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.SyncObjs,
  Vcl.Forms,
  ShellApi,
  Ioutils,
  WinTypes, WinProcs,
  System.Threading,
  Winapi.Windows,
  //
  Vcl.StdCtrls,
  Vcl.ComStrs,
  Vcl.ExtCtrls,
  //
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,    
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  //
  Data.DB;

Type
  TTransactionData = class( TThread )
  private

{$REGION 'Variaveis Accessors |  Mutators da classe | Acessors das propriedades de classes'}

    FDatabasePassword: String;
    FDatabaseIp      : String;
    FDatabaseUserName: String;
    FDatabaseName    : String;
    FLogFilePatch    : String;
    FLogFilename     : String;
    FDatabasePort    : word;

    FTransactValues: array of Variant;
    FTransactionStr: string;

{$ENDREGION}

  protected

{$REGION 'Componentes Globais da Aplicação, Protegidos não remova desse encapsulamento
   devido ao nivel de transação da query com a Thread'}

    FCon           : TFDConnection;
    FTransact      : TFDTransaction;
    FQuery         : TFDQuery;
    FPsys          : TFDPhysFBDriverLink;
    FLogTransaction: TStringList;

{$ENDREGION}

    function ValidaDadosBancodeDados: TTransactionData;
    function MontaConexao: TTransactionData;
    function DoTransactStatements: TTransactionData; 
    
    function WriteLog( PLogLine: Exception ): TTransactionData; overload;
    function WriteLog( PLogLine: String ): TTransactionData; overload;       

  {$REGION 'Eventos do componente'}
  
     procedure DoOnTerminate(Sender:TObject);

  {$ENDREGION}
    
  {$REGION 'Eventos da Transação'} 
  
    procedure oConAfterCommit(Sender: TObject);
    procedure oConAfterConnect(Sender: TObject);
    procedure oConAfterDisconnect(Sender: TObject);
    procedure oConAfterRollback(Sender: TObject);
    procedure oConAfterStartTransaction(Sender: TObject);
    procedure oConBeforeCommit(Sender: TObject);
    procedure oConBeforeConnect(Sender: TObject);
    procedure oConBeforeDisconnect(Sender: TObject);
    procedure oConBeforeRollback(Sender: TObject);
    procedure oConBeforeStartTransaction(Sender: TObject);
    procedure oConError(ASender, AInitiator: TObject; var AException: Exception);
    procedure oConLogin(AConnection: TFDCustomConnection; AParams: TFDConnectionDefParams);
    procedure oConLost(Sender: TObject);
    procedure oConRecover(ASender, AInitiator: TObject; AException: Exception;var AAction: TFDPhysConnectionRecoverAction);
    procedure oConRestored(Sender: TObject);
    
  {$ENDREGION}

    procedure Execute; override;

  public
    constructor Create;

    destructor Destroy; override;

{$REGION 'Propriedades do database'}

    property Database: String         read FDatabaseName     write FDatabaseName;
    property Host: String             read FDatabaseIp       write FDatabaseIp;
    property DatabaseUserName: String read FDatabaseUserName write FDatabaseUserName;
    property DatabasePassword: String read FDatabasePassword write FDatabasePassword;
    property DatabasePort: word       read FDatabasePort     write FDatabasePort;
    property LogFileName: String      read FLogFilename      write FLogFilename;
    property LogFilePatch: String     read FLogFilePatch     write FLogFilePatch;    
  

{$ENDREGION}


  published
    
  end; 

 {$REGION 'Um Await Assincrono'}

   type TAwait = class(TComponent)
  private
   class var FClock : TTimer;
   class var FArg   : TProc;
   procedure OnTimer(Sender: TObject);
  public
   class function AWait(Arg: TProc; dwMilliseconds: Longint):TAwait; static;
   procedure AfterConstruction; override;
   procedure BeforeDestruction; override;
 end;

 type Async = record
   class function AWait(Arg: TProc; dwMilliseconds: Longint):TAwait; static;
 end;
  
{$ENDREGION} 

 {$REGION 'Metodo Builder Global'}
   procedure ExecuteThread_Manutencaoprodutos(dwMilliseconds: Longint );
 {$ENDREGION}
 
implementation

procedure ExecuteThread_Manutencaoprodutos(dwMilliseconds: Longint );
begin
  Async
   .AWait(procedure
     begin
      with TTransactionData.Create do
      begin
        DatabaseUserName := 'sysdba';
        DatabasePassword := '1234';
        DatabasePort     := 3050;
        Database         := 'c:\windows\basesys\hsystem.gdb';
        Host             := '127.0.0.1';
        Start;
      end;
     end,dwMilliseconds);
end;

procedure Delay(dwMilliseconds: Longint);
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop := GetTickCount;
    Application.ProcessMessages;
  until (iStop - iStart) >= dwMilliseconds;
end;

class function Async.AWait(Arg: TProc; dwMilliseconds: Longint): TAwait;
begin
  Result := TAwait.AWait(Arg,dwMilliseconds);
end;

{ TTransactionData }

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

 procedure TrimAppMemorySize;
   var
      MainHandle: THandle;
    begin
      try
        MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessID);
        SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
        CloseHandle(MainHandle); 
      except
      end;
    end;

destructor TTransactionData.Destroy;
begin

 {$REGION 'Fecha o Log da Transação'}
 
  FLogTransaction.Add( ' ---------------------------------------------------- ' );
  
  if not DirectoryExists(FLogFilePatch) then
    if not ForceDirectories( FLogFilePatch )  then
      raise Exception.Create('Nao foi possivel criar o Arquivo de log na pasta Informada');
      
  FLogTransaction.SaveToFile( TPath.combine( LogFilePatch,FLogFilename ) );
  
 {$ENDREGION}
 
    
 {$REGION 'Destroi os componentes'}
 
  FreeAndNil( FCon );
  FreeAndNil( FTransact );
  FreeAndNil( FQuery );
  FreeAndNil( FPsys );
  FreeAndNil( FLogTransaction );
  
 {$ENDREGION}      
  
  inherited;
  
end;

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


function TTransactionData.MontaConexao: TTransactionData;
begin
  Result := Self;

{$REGION 'prepara os dados de conexao'}

  try

    FCon.Connected := false;
    FCon.LoginPrompt := false;
    FCon.Params.Clear;
    FCon.Params.Add( 'hostname=' + FDatabaseIp );
    FCon.Params.Add( 'user_name=' + FDatabaseUserName );
    FCon.Params.Add( 'password=' + FDatabasePassword );
    FCon.Params.Add( 'port=' + IntToStr( FDatabasePort ) );
    FCon.Params.Add( 'Database=' + FDatabaseName );
    FCon.Params.Add( 'DriverID=' + 'FB' );

  Except
    on E: Exception do
    begin
      WriteLog( E );
      Abort;
    end;
  end;

{$ENDREGION}

end;

procedure TTransactionData.oConAfterCommit(Sender: TObject);
begin
 WriteLog('Transação fechada com sucesso!');
end;

procedure TTransactionData.oConAfterConnect(Sender: TObject);
begin
 WriteLog('Conexão com banco de dados aberta!');
end;

procedure TTransactionData.oConAfterDisconnect(Sender: TObject);
begin
  WriteLog('Desconectado com sucesso!');
end;

procedure TTransactionData.oConAfterRollback(Sender: TObject);
begin
 WriteLog('Executando RollBack da transação!');
end;

procedure TTransactionData.oConAfterStartTransaction(Sender: TObject);
begin
 WriteLog('Iniciando transação');
end;

procedure TTransactionData.oConBeforeCommit(Sender: TObject);
begin
  WriteLog('Preparando para efetuar commit do banco de dados');
end;

procedure TTransactionData.oConBeforeConnect(Sender: TObject);
begin
  WriteLog('Conectado com o serviço de banco de dados ');
end;

procedure TTransactionData.oConBeforeDisconnect(Sender: TObject);
begin
 WriteLog('Preparando para desconectar com o serviço de banco de dados ');
end;

procedure TTransactionData.oConBeforeRollback(Sender: TObject);
begin
 WriteLog('Preparando para fazer Rollback da transação ');
end;

procedure TTransactionData.oConBeforeStartTransaction(Sender: TObject);
begin
 WriteLog('Preparando para iniciar a transação dos dados ');
end;

procedure TTransactionData.oConError(ASender, AInitiator: TObject; var AException: Exception);
begin
 WriteLog(AException);
end;

procedure TTransactionData.oConLogin(AConnection: TFDCustomConnection; AParams: TFDConnectionDefParams);
begin
 WriteLog('Conectando ao banco de dados ');
end;

procedure TTransactionData.oConLost(Sender: TObject);
begin
 WriteLog('Conexão com o banco de dados Perdida ');
end;

procedure TTransactionData.oConRecover(ASender, AInitiator: TObject; AException: Exception;
  var AAction: TFDPhysConnectionRecoverAction);
begin
  WriteLog('Tentando reassumir a conexão com o banco de dados ');
end;

procedure TTransactionData.oConRestored(Sender: TObject);
begin
 WriteLog('Conexão com o banco de dados restaurada! ');
end;

procedure TTransactionData.DoOnTerminate(Sender: TObject);
begin
 WriteLog('Thread Liberada dos processos!');
end;

function TTransactionData.ValidaDadosBancodeDados: TTransactionData;
begin
  Result := Self;

{$REGION 'Valida os dados de Conexão'}

  try
  
    Assert( not FDatabaseName.Trim.IsEmpty, 'O Patch caminho completo do Banco de dados não pode ser vazio ex: C:\meuBanco\Banco.FDB' );
    Assert( not FDatabaseIp.Trim.IsEmpty, 'O IP/Host do Banco de dados não pode ser vazio, Ex: 127.0.0.1 ' );
    Assert( not FDatabaseUserName.Trim.IsEmpty, 'O Nome de usuário do Banco de dados não pode ser vazio! ' );
    Assert( not FDatabasePassword.Trim.IsEmpty, 'O Password de usuário do Banco de dados não pode ser vazio! ' );
    Assert( FDatabasePort > 0, 'A POrta de serviços do banco de dados nao pode ser vazia! Ex: 3050' );
    
  Except
  
    on E: Exception do
    begin
    
      WriteLog( E );
      
      Abort;
      
    end;
    
  end;

{$ENDREGION}

end;

function TTransactionData.WriteLog( PLogLine: String ): TTransactionData;
var
  LDate   : String;
  LMessage: String;
begin
  Result := Self;
  
  LMessage := PLogLine;
  LDate := FormatDateTime( 'DDMMYYYY:HHMMSS', Now );

  if FLogTransaction.Count <= 0
  then
  begin
  
    FLogTransaction.Add( ' --------------------- LOG DE TRANSAÇÕES ------------------------- ' );     
    FLogTransaction.Add( '    APLICAÇÃO  ' + Application.ExeName  );
    
  end;

  FLogTransaction.Add( LDate + ' > ' + LMessage );
  
end;

function TTransactionData.WriteLog( PLogLine: Exception ): TTransactionData;
var
  LType   : String;
  LMessage: string;
begin

  Result := Self;
  
  LMessage := PLogLine.Message;
  LType := ' | EXCEPTION | ';
  WriteLog( concat( LType, LMessage ) );
  
end;

{ TAwait }

procedure TAwait.AfterConstruction;
begin
  inherited;

  FClock:= TTimer.Create(nil);
  FClock.OnTimer := OnTimer;
  FClock.Enabled := False; 
  
end;

class function TAwait.AWait(Arg: TProc; dwMilliseconds: Longint): TAwait;
begin

 Result := TAwait.Create(Application);
 TAwait.FArg:= Arg;
 TAwait.FClock.Interval:= dwMilliseconds;
 TAwait.FClock.Enabled := True;
 
end;

procedure TAwait.BeforeDestruction;
begin

   FreeAndNil(FClock);
   
 inherited;
end;

procedure TAwait.OnTimer(Sender: TObject);
begin

 FClock.Enabled := False;
 if Assigned(FArg) then FArg;
 
end;

end.
