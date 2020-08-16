unit UCMForm;

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
  {Windows, ExtCtrls, stdCtrls,}
  Forms, Controls,Graphics, Dialogs, SysUtils,
  UCMVCL, SynCommons, mORMot, SynLog,  UCMConfigApp,
  UCMMormot, Classes;

type

  { TFormInitParam }
  TCMFormInitParam = class(TSynPersistent)
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
  end;

  { States of CMForm between create an show... }
  TCMFormStates = (cmfsNormal,
    cmfsInitializingCreate, cmfsProcessingCreate, cmfsEndingCreate,
    cmfsInitializingShow, cmfsProcessingShow, cmfsEndingShow);

  { TCMForm }
  TCMForm = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FIsAContainedForm: boolean;
    procedure setIsAContainedForm(AValue: boolean);
  protected
    NotificationWindow: TCMNotificationWindow;
    CMFormState: TCMFormStates;
    procedure InitializingCreate(Sender: TObject); virtual;
    procedure ProcessingCreate(Sender: TObject); virtual;
    procedure EndingCreate(Sender: TObject); virtual;
    procedure InitializingShow(Sender: TObject); virtual;
    procedure ProcessingShow(Sender: TObject); virtual;
    procedure EndingShow(Sender: TObject); virtual;
    procedure EndingDestroy(Sender: TObject); virtual;
    procedure RestoreCursor;
    procedure WaitingCursor;
    procedure AssignImages; virtual;
  public
    Client: TSQLHttpClientCM;
    ReleaseFormInitParam: boolean;
    AssociedTab: TObject;
    FormInitParam: TCMFormInitParam;
    IdMenu: integer;
    property IsAContainedForm: boolean read FIsAContainedForm write setIsAContainedForm;
    procedure InitializeAccordingFormInitParam;
    function VerifySecurity: boolean;
    function AsCMForm: TCMForm;
    function IsModal: boolean;

    procedure ShowNotification(pMsg: string; pTitle: string = '';
      pMilisecondsToDisplay: integer = 0;
      pNotificationColor: TColor = clInfoBk; pFixedWidthFont: boolean = False;
      pNotificationWidth: integer = 0; pNotificationHeight: Integer = 0;
      pShowNotificationMode: TShowNotificationMode = snmTopLeftAsociatedForm); overload;

    class function ShowModalAutoRelease(var pForm: TCMForm): TModalResult; overload;
    class function ShowModalAutoRelease(pFormInitParam: TCMFormInitParam = nil;
      pReleaseFormInitParam: boolean = True): TModalResult; overload;
  end;

  TCMFormClass = class of TCMForm;
  TCMFormObjArray = array of TCMForm;

  TCMFormClassArray = array of TCMFormClass;

  TMenuFunctions = class(TObject)
    public
  end;

var
  CMForm: TCMForm;

implementation

{$R *.lfm}

procedure TCMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // TSynLog.Add.Log(sllCustom1, 'Cerrando: ' + Name);
end;

procedure TCMForm.FormCreate(Sender: TObject);
begin
  inherited;
  InitializingCreate(Sender);
  ProcessingCreate(Sender);
  EndingCreate(Sender);
end;

{ TCMForm }

procedure TCMForm.FormDestroy(Sender: TObject);
begin
  inherited;
  if Assigned(FormInitParam) and ReleaseFormInitParam then
    FormInitParam.Free;
  NotificationWindow.Free;
  EndingDestroy(Sender);
  //TSynLog.Add.Log( sllCustom1, 'Destruyendo :' + ClassName + ' -- '  + Caption);
end;

procedure TCMForm.FormShow(Sender: TObject);
begin
  inherited;
  InitializingShow(Sender);
  ProcessingShow(Sender);
  EndingShow(Sender);
  CMFormState := cmfsNormal;
end;

procedure TCMForm.setIsAContainedForm(AValue: boolean);
begin
  if FIsAContainedForm=AValue then Exit;
  FIsAContainedForm:=AValue;
end;

procedure TCMForm.InitializingCreate(Sender: TObject);
begin
  Client := ConfigApp.CMClient;

  CMFormState := cmfsInitializingCreate;
  NotificationWindow := TCMNotificationWindow.Create(nil);
  NotificationWindow.AsociatedForm := Self;
  AssignImages;
  AssociedTab := nil;
  FormInitParam := nil;
  ReleaseFormInitParam := True;
  FIsAContainedForm := False;

  { IdMenu Is used in a Tabbed Interface.. }
  IdMenu := 0;
end;

procedure TCMForm.ProcessingCreate(Sender: TObject);
begin
  CMFormState := cmfsProcessingCreate;
end;

procedure TCMForm.EndingCreate(Sender: TObject);
begin
  CMFormState := cmfsEndingCreate;
end;

procedure TCMForm.InitializingShow(Sender: TObject);
begin
  CMFormState := cmfsInitializingShow;
end;

procedure TCMForm.ProcessingShow(Sender: TObject);
begin
  CMFormState := cmfsProcessingShow;
end;

procedure TCMForm.EndingShow(Sender: TObject);
begin
  CMFormState := cmfsEndingShow;

  if Assigned(FormInitParam) then
    InitializeAccordingFormInitParam;
end;

procedure TCMForm.EndingDestroy(Sender: TObject);
begin
  { Redefine in hereders }
end;

procedure TCMForm.RestoreCursor;
begin
  Screen.Cursor := crDefault;
end;

procedure TCMForm.WaitingCursor;
begin
  Screen.Cursor := crHourGlass;
end;

procedure TCMForm.AssignImages;
begin
  { Aqui se asignaran las imangenes a los botones o lo que corresponda, desde
  el archivo de recuros a travez de la funcion GetImagenGorrion. POr ejemplo:

  btCerrar.Glyph := ConfigApp.GetCMImage(TImgCerrar16); }
end;

procedure TCMForm.InitializeAccordingFormInitParam;
begin

  { Ejemplo para Administradores con FMCriterios...
  if Assigned(FmCriterios) then
    if Assigned(ParamInicial) then
    begin
      FmCriterios.Parametros.DatosLeidos := False;
      FmCriterios.Parametros.LeerDatosIni;

      FmCriterios.AlgunCtrl.Value := ParamInicial.AlgunValor;

      FmCriterios.Parametros.DatosLeidos := True;
      FmCriterios.ActualizarParametros;
    end;}

end;

function TCMForm.VerifySecurity: boolean;
begin
  result := True;
end;

function TCMForm.AsCMForm: TCMForm;
begin
  Result := TCMForm(Self);
end;

function TCMForm.IsModal: boolean;
begin
  result := (fsModal in FormState);
end;

procedure TCMForm.ShowNotification(pMsg: string; pTitle: string;
  pMilisecondsToDisplay: integer; pNotificationColor: TColor;
  pFixedWidthFont: boolean; pNotificationWidth: integer;
  pNotificationHeight: Integer; pShowNotificationMode: TShowNotificationMode);
begin
  NotificationWindow.ShowNotificationMode:= pShowNotificationMode;
  NotificationWindow.ShowNotification(pMsg, pTitle, pMilisecondsToDisplay,
    pNotificationColor, pFixedWidthFont, pNotificationWidth, pNotificationHeight);
end;

class function TCMForm.ShowModalAutoRelease(var pForm: TCMForm): TModalResult;
begin
  { Creo una instacia de la clase actual y si verifica la seguridad la
  muestra, caso contrario la libera }
  if pForm = nil then
    pForm := Create(Application);

  with pForm do
    try
    { Verifico permisos para la operación guardada en la variable Operacion y
    según el usuario que ingresó al sistema }
      if VerifySecurity then
        Result := Showmodal
      else
      begin
        Result := mrAbort;
        exit;
      end;
    finally
      Free;
    end;
end;

class function TCMForm.ShowModalAutoRelease(pFormInitParam: TCMFormInitParam;
  pReleaseFormInitParam: boolean): TModalResult;
begin
  with Create(nil) do
  try
    if not Assigned(FormInitParam) then
    begin
      FormInitParam := pFormInitParam;
      ReleaseFormInitParam := pReleaseFormInitParam;
    end;
    { Verifico permisos para la operación guardada en la variable Operacion y
    según el usuario que ingresó al sistema }
    if VerifySecurity then
      Result := Showmodal
    else
    begin
      Result := mrAbort;
      exit;
    end;
  finally
    free;
  end;

end;


{ TFormInitParam }

constructor TCMFormInitParam.Create;
begin
  inherited Create;
end;

destructor TCMFormInitParam.Destroy;
begin
  // TSynLog.Add.Log(sllCustom1, 'Releasing ' + ClassName);
  inherited Destroy;
end;

initialization
  TJSONSerializer.RegisterObjArrayForJSON(TypeInfo(TCMFormObjArray),TCMForm);
end.

