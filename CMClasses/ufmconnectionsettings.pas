unit UFMConnectionSettings;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, ExtCtrls, UCMCommons;

type

  { TFmConnectionSettings }

  TFmConnectionSettings = class(TForm)
    btDBName: TBitBtn;
    btNewDB: TBitBtn;
    btDBPath: TBitBtn;
    btConnect: TBitBtn;
    btCancel: TBitBtn;
    edDBName: TEdit;
    edPort: TEdit;
    edDBPath: TEdit;
    edServerName: TEdit;
    Label1: TLabel;
    lbConnectionMode: TLabel;
    lbClientServerHelp: TLabel;
    lbDBName: TLabel;
    lbPort: TLabel;
    lbDBPath: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btCancelClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btDBNameClick(Sender: TObject);
    procedure btDBPathClick(Sender: TObject);
    procedure btNewDBClick(Sender: TObject);
    procedure edDBNameExit(Sender: TObject);
    procedure edDBPathExit(Sender: TObject);
    procedure edPortExit(Sender: TObject);
    procedure edServerNameExit(Sender: TObject);
  protected
    ConnectionConfig: TConnectionConfig;
    ConnectionInternalServiceConfig: TConnectionServiceConfig;
    MsgCanContinue: AnsiString;
    ConfiguratedConnectionOK: boolean;
    InternalConfigApp: TCMSynPersistent;
    procedure ConfigureClientMode;
    procedure ConfigureClientServerMode;
    procedure ConfigureClientHeight(LastWinCtrl: TWinControl);
    procedure ProceedAccordingTo(ButtonPressed: TBitBtn);
    procedure ConfigButtons;
    procedure ConnectionConfigUpdate;
  public
    procedure AssignImages; //override;
    class function ConfinguratedConnection(var pMsgCanContinue: AnsiString;
      pConfigApp: TCMSynPersistent): Boolean;
  end;

var
  FmConnectionSettings: TFmConnectionSettings;

implementation

uses
  UCMConfigApp;

{$R *.lfm}

{ TFmConnectionSettings }

procedure TFmConnectionSettings.btConnectClick(Sender: TObject);
begin
  case TConfigApp(InternalConfigApp).ExecutionMode of
    emClient:
      try
        Screen.Cursor := crHourGlass;
        ConfiguratedConnectionOK := False;

        { Recreate CMClient with new configuration }
        TConfigApp(InternalConfigApp).RecreateClient(ConnectionConfig, ConfiguratedConnectionOK);

        if not ConfiguratedConnectionOK then
        begin
          MsgCanContinue := 'Client not connected';
          ShowMessage(MsgCanContinue + LineEnding + 'Verify configuration.');
          edServerName.SetFocus;
        end;

      finally
        Screen.Cursor := crDefault;
      end;
    emClientServer:
      begin
        ProceedAccordingTo(btConnect);
      end;
  end;

  if ConfiguratedConnectionOK then
    Close
end;

procedure TFmConnectionSettings.btDBNameClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := edDBPath.Text;
  if OpenDialog1.Execute then
  begin
    edDBName.Text := ExtractFileName(OpenDialog1.FileName);
    edDBPath.Text := ExtractFileDir(OpenDialog1.FileName);
  end
  else
    edDBName.SetFocus;

  ConfigButtons;
end;

procedure TFmConnectionSettings.btDBPathClick(Sender: TObject);
var
  myFolder: string;
begin
  if SelectDirectory('folder for DB',ExtractFileDir(Application.ExeName), myFolder) then
  begin
    edDBPath.Text := myFolder;
    ConfigButtons;
  end;
end;

procedure TFmConnectionSettings.btNewDBClick(Sender: TObject);
begin
  ProceedAccordingTo(btNewDB);

  if ConfiguratedConnectionOK then
    Close
end;

procedure TFmConnectionSettings.edDBNameExit(Sender: TObject);
begin
  ConfigButtons;
end;

procedure TFmConnectionSettings.edDBPathExit(Sender: TObject);
begin
  ConfigButtons;
end;

procedure TFmConnectionSettings.edPortExit(Sender: TObject);
begin
  ConnectionConfigUpdate;
end;

procedure TFmConnectionSettings.edServerNameExit(Sender: TObject);
begin
  ConnectionConfigUpdate;
end;

procedure TFmConnectionSettings.ConfigureClientMode;
begin
  ConnectionConfig := TConfigApp(InternalConfigApp).ConnectionConfig;

  lbConnectionMode.Caption := 'Client mode';
  lbClientServerHelp.Visible := True;
  edServerName.Text := ConnectionConfig.ServerName;
  edPort.Text:= ConnectionConfig.Port;

  lbDBPath.Visible := False;
  edDBPath.Visible := False;
  btDBPath.Visible := False;
  lbDBName.Visible := False;
  edDBName.Visible := False;
  btDBName.Visible := False;
  btNewDB.Visible  := False;

  ConfigureClientHeight(edPort);
end;

procedure TFmConnectionSettings.ConfigureClientServerMode;
begin
  ConnectionInternalServiceConfig := TConfigApp(InternalConfigApp).ConnectionInternalServiceConfig;
  lbConnectionMode.Caption := 'ClientServer mode';
  lbClientServerHelp.Visible := False;

  edServerName.Text := 'localhost';
  edSErverName.Hint := 'Hostname: ' + GetEnvironmentVariable('COMPUTERNAME');
  edServerName.Enabled := False;
  edPort.Text:= ConnectionInternalServiceConfig.Port;

  edDBPath.Text := ConnectionInternalServiceConfig.DBPath;
  edDBName.Text := ConnectionInternalServiceConfig.DBName;

  ConfigButtons;
  ConfigureClientHeight(edDBName);
end;

procedure TFmConnectionSettings.ConfigureClientHeight(LastWinCtrl: TWinControl);
begin
  ClientHeight :=
    LastWinCtrl.Top + LastWinCtrl.Height +
    lbConnectionMode.Top +
    btConnect.Height + 16;
end;

procedure TFmConnectionSettings.ProceedAccordingTo(ButtonPressed: TBitBtn);
var
  vMsg: AnsiString;
begin
  vMsg := '';
  if ButtonPressed = btNewDB then
  begin
    ConfiguratedConnectionOK := ConnectionInternalServiceConfig.CanCreateNewDB(vMsg);
    if not ConfiguratedConnectionOK then
    begin
      ShowMessage(vMsg);
      edDBName.SetFocus;
    end;
  end
  else if ButtonPressed = btConnect then
  begin
    ConfiguratedConnectionOK:= ConnectionInternalServiceConfig.CanConnectDB(vMsg);
    if not ConfiguratedConnectionOK then
    begin
      ShowMessage(vMsg);
      edDBName.SetFocus;
    end;
  end;
end;

procedure TFmConnectionSettings.ConfigButtons;
begin
  ConnectionConfigUpdate;

  if DirectoryExists(edDBPath.Text) then
    edDBPath.Font.Color := clWindowText
  else
    edDBPath.Font.Color := clRed;

  if FileExists(ConnectionInternalServiceConfig.DBNameWithPath) then
  begin
    btConnect.Enabled := True;
    btNewDB.Enabled := False;
    edDBName.Font.Color := clWindowText;
    OpenDialog1.Title := 'Connect to existing DB file';
  end
  else
    begin
    btConnect.Enabled := False;
    btNewDB.Enabled := True;
    edDBName.Font.Color := clRed;
    OpenDialog1.Title := 'Create a new DB file';
  end;

end;

procedure TFmConnectionSettings.ConnectionConfigUpdate;
begin
  edDBName.text := ChangeFileExt(edDBName.Text,'.db3');

  case TConfigApp(InternalConfigApp).ExecutionMode of
    emClient      :
      begin
        ConnectionConfig.ServerName := edServerName.Text;
        ConnectionConfig.Port := edPort.Text;
      end;
    emClientServer:
      begin
        ConnectionInternalServiceConfig.ServerName := edServerName.Text;
        ConnectionInternalServiceConfig.Port := edPort.Text;
        ConnectionInternalServiceConfig.DBPath := edDBPath.Text;
        ConnectionInternalServiceConfig.DBName := edDBName.Text;
      end;
  end;
end;

procedure TFmConnectionSettings.btCancelClick(Sender: TObject);
begin
  ConfiguratedConnectionOK := False;
  case TConfigApp(InternalConfigApp).ExecutionMode of
    emClient      : MsgCanContinue := 'Client not connected';
    emClientServer: MsgCanContinue := 'InternalServer not created';
  end;
  close;
end;

procedure TFmConnectionSettings.AssignImages;
begin
  btDBPath.Glyph := TConfigApp(InternalConfigApp).GetCMImage(TImgCarpetaVacia16);
  btDBName.Glyph := TConfigApp(InternalConfigApp).GetCMImage(TImgCarpetaLlena16);;
  btNewDB.Glyph := TConfigApp(InternalConfigApp).GetCMImage(TImgDestello16);
  btConnect.Glyph := TConfigApp(InternalConfigApp).GetCMImage(TImgAceptar16);
  btCancel.Glyph := TConfigApp(InternalConfigApp).GetCMImage(TImgCancelar16);
end;

class function TFmConnectionSettings.ConfinguratedConnection(
  var pMsgCanContinue: AnsiString; pConfigApp: TCMSynPersistent): Boolean;
begin
  Result := False;
  with self.Create(Application) do
    try
      InternalConfigApp := pConfigApp;
      case TConfigApp(InternalConfigApp).ExecutionMode of
        emClient      : ConfigureClientMode;
        emClientServer: ConfigureClientServerMode;
      end;
      AssignImages;
      MsgCanContinue := 'Connection not yet configured';
      ConfiguratedConnectionOK := False;
      Showmodal;
      Result := ConfiguratedConnectionOK;
      pMsgCanContinue := MsgCanContinue;
    finally
      Free;
    end;
end;

end.

