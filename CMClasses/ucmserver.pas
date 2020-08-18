unit UCMServer;

(*
*** BEGIN LICENSE BLOCK *****
Version: MPL 1.1/GPL 2.0/LGPL 2.1

The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Claudio Martín.

Portions created by the Initial Developer are Copyright (C) 2020
the Initial Developer. All Rights Reserved.

Contributor(s):
 - ...

Alternatively, the contents of this file may be used under the terms of
either the GNU General Public License Version 2 or later (the "GPL"), or
the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
in which case the provisions of the GPL or the LGPL are applicable instead
of those above. If you wish to allow use of your version of this file only
under the terms of either the GPL or the LGPL, and not to allow others to
use your version of this file under the terms of the MPL, indicate your
decision by deleting the provisions above and replace them with the notice
and other provisions required by the GPL or the LGPL. If you do not delete
the provisions above, a recipient may use your version of this file under
the terms of any one of the MPL, the GPL or the LGPL.

***** END LICENSE BLOCK *****
*)

{$mode objfpc}{$H+}

interface

uses
  Windows,
  SysUtils,
  mORMot, UCMInterfaces, mORMotService,
  SynCommons,
  SynLog,
  mORMotSQLite3, SynSQLite3Static,
  mORMotHttpServer,
  UCMModel, UCMCommons, UCMMormot;

type
  TSQLRestServerCM = class;

  { class implementing the background Service }
  { TCMService }

  TCMService = class(TServiceSingle)
  public
    IntegratedInApp, SuccefullConnectionOK: boolean;
    ConnectionServiceConfig, ConnectionInternalServiceConfig: TConnectionServiceConfig;

    // the associated database model
    CMModelServer: TSQLModel;
    // the associated DB
    CMServerDB: TSQLRestServerCM;
    // the background Server processing all requests
    CMHttpServer: TSQLHttpServer; //HTTPServerCM: TSQLHttpServer;

    // event triggered to start the service
    // - e.g. create the Server instance
    procedure DoStart(Sender: TService);
    // event triggered to stop the service
    // - e.g. destroy the Server instance
    procedure DoStop(Sender: TService);
    // initialize the background Service
    constructor Create; reintroduce;
    // launch as Console application
    constructor CreateAsConsole; reintroduce;
    // launc integratede in App
    constructor CreateAsIntegratedInApp(pConnServConf: TConnectionServiceConfig); reintroduce;

    // release memory
    destructor Destroy; override;

  end;


  { TInjectableObjectRestCM }
  TInjectableObjectRestCM = class(TInjectableObjectRest)
    function ConnectionServiceConfig: TConnectionServiceConfig;
    function GetMsgError(err: Exception): RawUTF8;
  end;

  { TTransferFileService }
  TTransferFileService = class(TInjectableObjectRestCM, ITransferFileService)
  public
    function FileAvailable(pFileToTransfer: RawUTF8; out msg: RawUTF8): boolean;
    function ListFilesAvailables(out msg: rawUTF8): TFileNameDynArray;
  end;

  { TMenuServices }
  TMenuServices = class(TInjectableObjectRestCM, IMenuServices)
  public
    procedure RebuildNumericPathInMenus;
  end;

  { TSQLRestServerCM }
  TSQLRestServerCM = class(TSQLRestServerDB)
  published
    ConnectionServiceConfig: TConnectionServiceConfig;
    procedure sum(Ctxt: TSQLRestServerURIContext);
    procedure GetFile(Ctxt: TSQLRestServerURIContext);
  end;


implementation

{ TMenuServices }

procedure TMenuServices.RebuildNumericPathInMenus;
var
  SQLMenu, SQLMenuAux: TSQLMenu;
begin
  SQLMenu := TSQLMenu.CreateAndFillPrepare(Server,'order by IdParent, IdMenu',[]);
  try
    while SQLMenu.FillOne do
    begin
      if SQLMenu.IdParent = 0 then
        SQLMenu.NumericPath := IntToString(SQLMenu.IdMenu)
      else
      begin
        SQLMenuAux := TSQLMenu.Create(Server,'IdMenu = ?',[SQLMenu.IdParent]);
        try
          SQLMenu.NumericPath := IntToString(SQLMenuAux.IdMenu) + '.' + IntToString(SQLMenu.IdMenu);
        finally
          SQLMenuAux.free;
        end;
      end;

      Server.Update(SQLMenu);
    end;
  finally
    SQLMenu.Free;
  end;
end;

{ TSQLRestServerCM }
procedure TSQLRestServerCM.sum(Ctxt: TSQLRestServerURIContext);
begin
  with Ctxt do
    Results([InputDouble['a'] + InputDouble['b']]);
end;

procedure TSQLRestServerCM.GetFile(Ctxt: TSQLRestServerURIContext);
var
  vFileName: TFileName;
begin
  //Ctxt.ReturnFileFromFolder('C:\Temp\', True, vFileName);
  vFileName := Ctxt.InputUTF8['FileName'];
  Ctxt.ReturnFileFromFolder(ConnectionServiceConfig.UpdatableFilesFolder, True, vFileName);
end;

{ TCMService }

procedure TCMService.DoStart(Sender: TService);
var
  vConnServConfig: TConnectionServiceConfig;
  pathConfigFile: TFileName;
  Continue: boolean;
  aDataBaseFileName: String;
begin
  Continue := True;
  TSQLLog.Enter(self);
  if CMHttpServer<>nil then
    DoStop(nil); // should never happen

  if IntegratedInApp then
  begin
    vConnServConfig := ConnectionInternalServiceConfig;
    Continue := True;
  end
  else
  begin
    if Assigned(ConnectionServiceConfig) then
      vConnServConfig := ConnectionServiceConfig
    else
      Continue := False;
  end;

  if not Continue then exit;

  CMModelServer := CreateModel;
  CMServerDB := TSQLRestServerCM.Create(CMModelServer, vConnServConfig.DBNameWithPath);
  { Assign to Server, the ConnectionServiceConfig, where is defined some variables
  for the correct use for some services. See UpdatableFilesFolder in GetFile services }
  CMServerDB.ConnectionServiceConfig := vConnServConfig;
  CMServerDB.AcquireExecutionMode[execSOAByInterface] := amBackgroundORMSharedThread;
  CMServerDB.CreateMissingTables;

  { Resitration of services in server }
  //DB.ServiceDefine(TSQLRemoto,[ISQLRemoto],sicClientDriven).
    //SetOptions([], [optExecInMainThread, optFreeInMainThread]);
  CMServerDB.serviceDefine(TTransferFileService, [ITransferFileService], sicShared);
  CMServerDB.serviceDefine(TMenuServices, [IMenuServices], sicShared);

  CMHttpServer := TSQLHttpServer.Create(vConnServConfig.Port, [CMServerDB], '+', useHttpApiRegisteringURI);
  CMHttpServer.AccessControlAllowOrigin := '*'; // for AJAX requests to work

  //ServerCM := TSQLDBServerHttpApi.Create(PropConexionMormot,
  //  DatosServicio.NomBaseHTTP, DatosServicio.Puerto,
  //  DatosServicio.UsuarioHttp, DatosServicio.ClaveHttp);
  //{ Usuario y clave alternativa.... }
  //ServerCM.Protocol.Authenticate.AuthenticateUser('camu','camufle');

  TSQLLog.Add.Log(sllInfo,'CMHttpServer % started by %',[CMHttpServer.HttpServer, CMHttpServer]);

end;

procedure TCMService.DoStop(Sender: TService);
begin
  TSQLLog.Enter(self);
  if not Assigned(CMHttpServer)then
    exit;
  TSQLLog.Add.Log(sllInfo,'HTTPServerCM % stopped by %',[CMHttpServer.HttpServer, CMHttpServer]);
  FreeAndNil(CMHttpServer);
  FreeAndNil(ConnectionServiceConfig);
  FreeAndNil(CMServerDB);
  FreeAndNil(CMModelServer);
end;

constructor TCMService.Create;
var
  vConnectionServiceConfigName: TFileName;
begin
  IntegratedInApp := False;
  ConnectionInternalServiceConfig := nil;
  SuccefullConnectionOK := False;
  CMHttpServer := nil;

  vConnectionServiceConfigName :=
    ExeVersion.ProgramFilePath +  ServiceName + CONNECTION_EXTENSION;

  ConnectionServiceConfig := TConnectionServiceConfig.Create(vConnectionServiceConfigName);
  ConnectionServiceConfig.Read;

  inherited Create(ConnectionServiceConfig.ServerName, ConnectionServiceConfig.ServiceDisplayName);

  TSQLLog.Family.Level := LOG_VERBOSE;
  TSQLLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSQLLog.Enter(self);
  OnStart := {$ifdef FPC}@{$endif}DoStart;
  OnStop := {$ifdef FPC}@{$endif}DoStop;
  OnResume := {$ifdef FPC}@{$endif}DoStart; // trivial Pause/Resume actions
  OnPause := {$ifdef FPC}@{$endif}DoStop;
end;

constructor TCMService.CreateAsConsole;
begin
  IntegratedInApp := False;
  SuccefullConnectionOK := False;

  // manual switch to console mode
  AllocConsole;
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_STACKTRACE;
  end;
end;

constructor TCMService.CreateAsIntegratedInApp(pConnServConf: TConnectionServiceConfig);
begin
  CMHttpServer := nil;
  IntegratedInApp := True;
  ConnectionInternalServiceConfig := pConnServConf;

  SuccefullConnectionOK := False;

  //TSQLLog.Family.Level := LOG_VERBOSE;
  //TSQLLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  //TSQLLog.Enter(self);
end;

destructor TCMService.Destroy;
begin
  TSQLLog.Enter(self);

  if CMHttpServer<>nil then
    DoStop(nil); // should not happen

  inherited Destroy;
end;

{ TTransferFileService }

function TTransferFileService.FileAvailable(pFileToTransfer: RawUTF8; out
  msg: RawUTF8): boolean;
var
  FileToTransferPath: TFileName;
begin
  result := False;
  msg := 'Undefined';

  if DirectoryExists(ConnectionServiceConfig.UpdatableFilesFolder) then
  begin
    FileToTransferPath :=
      ConnectionServiceConfig.UpdatableFilesFolder + '\' + pFileToTransfer;
    if FileExists(FileToTransferPath) then
    begin
      Result := True;
      msg := pFileToTransfer + ' ( File available )';
    end
    else
    begin
      Result := False;
      msg := pFileToTransfer + ' ( File not available )';
    end;
  end
  else
  begin
    Result := False;
    msg := 'update path not available';
  end;
end;

function TTransferFileService.ListFilesAvailables(out msg: rawUTF8): TFileNameDynArray;
begin
  msg := 'Undefined';

  if DirectoryExists(ConnectionServiceConfig.UpdatableFilesFolder) then
  begin
    result := FindFilesDynArrayToFileNames(
      FindFiles(ConnectionServiceConfig.UpdatableFilesFolder,'*','',False,False));
  end
  else
  begin
    SetLength(Result,0);
    msg := 'update path not available';
  end;
end;


function TInjectableObjectRestCM.ConnectionServiceConfig: TConnectionServiceConfig;
begin
  if Server = nil then
  begin
    result := nil;
    raise Exception.Create('Server not ready for this service');
  end
  else
    result := TSQLRestServerCM(Server).ConnectionServiceConfig;
end;

{ TInjectableObjectRestCM }
function TInjectableObjectRestCM.GetMsgError(err: Exception): RawUTF8;
begin
  result := 'Error handled on server: ' + Err.ClassName + #13#13 + err.Message;
end;

end.

