// licensed under a MPL/GPL/LGPL tri-license;
program TestProyect;

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

uses
  {$I SynDprUses.inc}
  {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads,{$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFmMain, UCMConfigApp, UCMCommons,
  SynCommons, SynLog, UTestConstants, Dialogs;

{$R *.res}
{$R CMImages.res}

var
  i: integer;
  TryUpdate: boolean;
  ExecutionMode: TExecutionMode;
  MsgCanContinue: AnsiString;

  function CheckParam(ParamToCheck,CheckingStr: AnsiString): boolean;
  begin
    result :=
      SameTextU(ParamToCheck, '-' + CheckingStr) or
      SameTextU(ParamToCheck, '/' + CheckingStr);
  end;

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  TryUpdate := True;
  ExecutionMode := emClient;
  { Analize each param and set variables }
  for i := 1 to ParamCount do
  begin
    if CheckParam(ParamStr(i),CMD_LINE_PARAM_CLIENTSERVER) then
      ExecutionMode := emClientServer
    else if CheckParam(ParamStr(i),CMD_LINE_PARAM_NO_UPDATE) then
      TryUpdate := False
  end;
  Application.Title := APP_NAME;

  with TSynLog.Family do
  begin
    //Level := [sllNone];
    //Level := LOG_VERBOSE;
    Level := [sllCustom1, sllException, sllExceptionOS];
    OnArchive := @EventArchiveSynLZ;
    ArchiveAfterDays := 1; // archive after one day
  end;

  { ConfigApp create a Client, and an InternalServer if needed (in emClientServer mode) }
  ConfigApp := TConfigApp.Create(TAppFiles.Create(Application.ExeName),
    APP_NAME, APP_VERSION, APP_RELEASE, ExecutionMode);
  try
    MsgCanContinue := '';
    if not ConfigApp.CanContinue(MsgCanContinue) then
    begin
      ShowMessage(MsgCanContinue);
      exit;
    end;
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
  finally
    { ConfigApp release Client and internalSever if exist }
    ConfigApp.Free;
  end;
end.

