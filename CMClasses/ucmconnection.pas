// This Unit is sharing between the Cient a Server applications.
unit UCMConnection;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SynCommons, mORMotSQLite3, mORMotHttpServer, UCMModel,
  SynSQLite3Static, UCMSimpleTypes, mORMotHttpClient;

var
  aClient: TSQLHttpClient;
  aServer: TSQLRestServerDB;
  aHttpServer: TSQLHttpServer;
  aDataBaseFileName: TFilename;

procedure CreateServers;
procedure CreateClient;

implementation

procedure CreateServers;
begin
  aDataBaseFileName := ChangeFileExt(ExeVersion.ProgramFileName, '.db3');
  CMModel := CreateModel;

  aServer := TSQLRestServerDB.Create(CMModel, aDataBaseFileName, True);
  aServer.CreateMissingTables;

  aHttpServer := TSQLHttpServer.Create(DEFAULT_SERVER_PORT, [aServer], '+', HTTP_DEFAULT_MODE);
  aHttpServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
end;

procedure CreateClient;
begin
  CMClient := TSQLHttpClient.Create('localhost', DEFAULT_SERVER_PORT, aModel);
  TSQLHttpClient(aClient).SetUser('User','synopse');
end;

end.

