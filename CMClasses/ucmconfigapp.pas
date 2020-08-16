unit UCMConfigApp;

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
  Classes, SysUtils, Graphics, Dialogs, SynDB,
  UCMServer, UCMMormot, UCMModel, UFMConnectionSettings, UCMInterfaces,
  UCMCommons, SynCommons, mORMot, SynZip, UCMFunctions;

type
  TBitmapObjArray = array of TBitmap;

  { TCMMenu }

  TCMMenu = class(TObject)
  public
    function SelectClassToProcess(pIdMenu: integer): TObject;
  end;


  { TAppFiles }

  TAppFiles = class(TCMSynPersistent)
  private
    FConnectionConfigName: TFileName;
    FConnectionConfigServiceName: TFileName;
    FRunningApplicationName: TFileName;
    FRunningApplicationVersionName: TFileName;
  public
    constructor Create(pApplicationName: TFileName); override;
    function ExecutablePath: TFileName;
  published
    property RunningApplicationName: TFileName
      read FRunningApplicationName write FRunningApplicationName;
    property RunningApplicationVersionName: TFileName
      read FRunningApplicationVersionName write FRunningApplicationVersionName;
    property ConnectionConfigName: TFileName
      read FConnectionConfigName write FConnectionConfigName;
    property ConnectionConfigServiceName: TFileName
      read FConnectionConfigServiceName write FConnectionConfigServiceName;
  end;

  { TConfigApp }
  TConfigApp = class(TCMSynPersistent)
  private
    FColorApp: TColor;
    FExecutionMode: TExecutionMode;
    //FNeededVersion: TVersionApp;
    //FReadedVersion: TVersionApp;
    FRunningVersion: TVersionApp;
    function GetAppNameVersionRelease: string;
    procedure setColorApp(AValue: TColor);
    procedure SetExecutionMode(AValue: TExecutionMode);

    procedure CreateClient(pConnConfig: TConnectionConfig;
      var pCanContinue: boolean; var pMsgCanContinue: ansistring);

    procedure CreateInternalService(pConnServConf: TConnectionServiceConfig;
      var pCanContinue: boolean; var pMsgCanContinue: ansistring);
  public
    AppName: String;
    AppVersion: Double;
    AppRelease: Integer;
    ConnectionConfig: TConnectionConfig;
    ConnectionInternalServiceConfig: TConnectionServiceConfig;

    CMInternalService: TCMService;
    CMClient: TSQLHttpClientCM;
    CMModelClient: TSQLModel;

    CMImagesList, CMInactiveImagesList, CMActiveInactiveImagesList: TSynDictionary;
    Color01,Color02,Color03: TColor;

    AppFiles: TAppFiles;
    destructor Destroy; override;
    constructor Create(pAppFiles: TAppFiles; pAppName: string;
      pRunningVersion: Double; pRunningRelease: Integer;
      pExecutionMode: TExecutionMode); overload;

    procedure LoadCMImagesList;
    function GetCMImage(pTCMImage: TCMImage): TBitmap;
    function GetCMImageInactive(pTCMImage: TCMImage): TBitmap;
    function GetCMImageActiveInactive(pTCMImage: TCMImage): TBitmap;

    function CanContinue(var MsgCanContinue: ansistring): boolean;
    procedure RecreateClient(pConnConfig: TConnectionConfig; var pConnectionOK: boolean);
    property AppNameVersionRelease: string read GetAppNameVersionRelease;

    property ColorApp: TColor read FColorApp write setColorApp;
  published
    property ExecutionMode: TExecutionMode read FExecutionMode write SetExecutionMode;
    property RunningVersion: TVersionApp read FRunningVersion write FRunningVersion;
    //property ReadedVersion: TVersionApp read FReadedVersion write FReadedVersion;
    //property NeededVersion: TVersionApp read FNeededVersion write FNeededVersion;
  end;

var
  ConfigApp: TConfigApp;

implementation

{ TCMMenu }

function TCMMenu.SelectClassToProcess(pIdMenu: integer): TObject;
begin
  result := nil;
end;

{ TAppFiles }

constructor TAppFiles.Create(pApplicationName: TFileName);
begin
  inherited Create('AppFiles.json');
  RunningApplicationName := pApplicationName;
  RunningApplicationVersionName := ChangeFileExt(pApplicationName, VERSION_EXTENSION);
  ConnectionConfigName := ChangeFileExt(pApplicationName, CONNECTION_EXTENSION);
  ConnectionConfigServiceName :=
    ChangeFileExt(ChangeFileExt(pApplicationName, '') + 'Service', CONNECTION_EXTENSION);
  //Save;
end;

function TAppFiles.ExecutablePath: TFileName;
begin
  Result := ExtractFilePath(RunningApplicationName);
end;


{ TConfigApp }

procedure TConfigApp.SetExecutionMode(AValue: TExecutionMode);
begin
  FExecutionMode := AValue;
end;

function TConfigApp.GetAppNameVersionRelease: string;
begin
  result := AppName + ' ' + SynCommons.UTF8ToString(ConfigApp.RunningVersion.VersionRelease);
end;

procedure TConfigApp.setColorApp(AValue: TColor);
begin
  if FColorApp=AValue then Exit;
  FColorApp:=AValue;

  FColorApp := AValue;

  FColorApp := MixColors(ColorApp, clWhite, 80);
  Color01  := MixColors(ColorApp, clWhite, 50);
  Color02  := MixColors(ColorApp, clWhite, 30);
  Color03  := MixColors(ColorApp, clWhite, 10);

  //ColorizePanelButtons;
  //vtvMenu.Invalidate;
end;

procedure TConfigApp.RecreateClient(pConnConfig: TConnectionConfig;
  var pConnectionOK: boolean);
begin
  if Assigned(CMClient) then
    FreeAndNil(CMClient);

  CMClient := TSQLHttpClientCM.Create(pConnConfig.ServerName,
    pConnConfig.Port, CMModelClient);

  pConnectionOK := CMClient.ServerTimestampSynchronize;

  if pConnectionOK then
  begin
    CMClient.ServiceDefine([ITransferFileService], {sicClientDriven} sicShared);

    if not CMClient.Services['TransferFileService'].Get(CMClient.TransferFileService) then
      ShowMessage('TransferFileService do not exist as service...');
  end;
end;

procedure TConfigApp.CreateClient(pConnConfig: TConnectionConfig;
  var pCanContinue: boolean; var pMsgCanContinue: ansistring);
var
  auxMsgConnection: ansistring;
  ConnectionOK: boolean;
begin
  CMModelClient := CreateModel;
  RecreateClient(pConnConfig, ConnectionOK);

  if ConnectionOK then
  begin
    CMClient.SetUser('User', 'synopse');
    pCanContinue := True;
    pMsgCanContinue := pMsgCanContinue + LineEnding + 'OK - Client connected';
  end
  else
  begin
    pCanContinue := TFmConnectionSettings.ConfinguratedConnection(
      auxMsgConnection, Self);
    pMsgCanContinue := pMsgCanContinue + LineEnding + auxMsgConnection;
    pConnConfig.Save;
  end;
end;

procedure TConfigApp.CreateInternalService(pConnServConf: TConnectionServiceConfig;
  var pCanContinue: boolean; var pMsgCanContinue: ansistring);
var
  auxMsgConnection: ansistring;
begin
  if FileExists(pConnServConf.DBNameWithPath) then
    pCanContinue := True
  else
    pCanContinue := TFmConnectionSettings.ConfinguratedConnection(
      auxMsgConnection, Self);

  if pCanContinue then
  begin
    CMInternalService := TCMService.CreateAsIntegratedInApp(pConnServConf);
    CMInternalService.DoStart(CMInternalService);
    pConnServConf.Save;
    pMsgCanContinue := pMsgCanContinue + LineEnding + 'OK - Internal service created';
  end
  else
    pMsgCanContinue := pMsgCanContinue + LineEnding + auxMsgConnection;
end;

destructor TConfigApp.Destroy;
begin
  AppFiles.Free;
  RunningVersion.Free;
  //ReadedVersion.Free;
  //NeededVersion.Free;

  CMImagesList.Free;
  CMInactiveImagesList.Free;
  CMActiveInactiveImagesList.Free;

  if Assigned(ConnectionConfig) then
    ConnectionConfig.Free;
  if Assigned(CMClient) then
  begin
    CMClient.TransferFileService := nil;
    CMClient.Free;
  end;
  if Assigned(cMModelClient) then
    CMModelClient.Free;

  case ExecutionMode of
    emClientServer:
    begin
      if Assigned(ConnectionInternalServiceConfig) then
        ConnectionInternalServiceConfig.Free;
      if Assigned(CMInternalService) then
      begin
        CMInternalService.DoStop(CMInternalService);
        CMInternalService.Free;
      end;
    end;
  end;
  inherited Destroy;
end;

constructor TConfigApp.Create(pAppFiles: TAppFiles; pAppName: string;
  pRunningVersion: Double; pRunningRelease: Integer; pExecutionMode: TExecutionMode);
begin
  inherited Create;
  ConnectionConfig := nil;
  ConnectionInternalServiceConfig := nil;
  CMInternalService := nil;
  CMClient := nil;
  CMModelClient := nil;

  { Configure according input params... }
  AppFiles := pAppFiles;
  AppName := pAppName;
  RunningVersion := TVersionApp.Create(AppFiles.RunningApplicationVersionName);
  RunningVersion.Version := pRunningVersion;
  RunningVersion.Release := pRunningRelease;
  RunningVersion.Save; // save version running file...

  CMImagesList := TSynDictionary.Create(TypeInfo(TIntegerDynArray),
    TypeInfo(TBitmapObjArray));
  CMInactiveImagesList := TSynDictionary.Create(
    TypeInfo(TIntegerDynArray), TypeInfo(TBitmapObjArray));
  CMActiveInactiveImagesList :=
    TSynDictionary.Create(TypeInfo(TIntegerDynArray), TypeInfo(TBitmapObjArray));
  LoadCMImagesList;

  ExecutionMode := pExecutionMode;
end;

procedure TConfigApp.LoadCMImagesList;
var
  ZipRead: TZipRead;

  procedure MakeBitmapGrey(ABitmap: TBitmap; PercentWhite: byte = 40);
  var
    vPercWhite, vR, vG, vB, vGrey: byte;
    OrigColor, GreyedColor: TColor;

    { pointer to the single pixel }
    Pixel: PBitmapPixel;
    { index of the line (row) and the pixel (column) }
    LineIndex, PixelIndex: integer;
  begin

    if PercentWhite > 100 then
      vPercWhite := 100
    else
      vPercWhite := PercentWhite;

    ABitmap.BeginUpdate();
    { for each bitmap row }
    for LineIndex := 0 to ABitmap.Height - 1 do
    begin
      { get the pointer to the first pixel of the given row }
      Pixel := ABitmap.ScanLine[LineIndex];

      { for each pixel of the current row }
      for PixelIndex := 0 to ABitmap.Width - 1 do
      begin
        { modify the values of the pixel components, using the pointer to the
        current pixel }
        vR := Pixel^.R;
        vG := Pixel^.G;
        vB := Pixel^.B;

        OrigColor := RGB2TColor(vR, vG, vB);
        GreyedColor := MixColors(OrigColor, clWhite, 100 - vPercWhite);
        Color2RGB(GreyedColor, vR, vG, vB);

        vGrey := (vR + vG + vB) div 3;
        Pixel^.R := vGrey;
        Pixel^.G := vGrey;
        Pixel^.B := vGrey;
        { move (increment) the pointer to the next pixel in row }
        Inc(Pixel);
      end;
    end;
    ABitmap.EndUpdate();
  end;

  procedure LoadImage(pIndex: TCMImage; pImageName: string);
  var
    i, ImageWidth, ImageHeight: integer;
    Stream: TSynMemoryStream;
    auxBitmap, auxInactiveBitmap, auxActiveInactiveBitmap: TBitmap;

  begin
    i := ZipRead.NameToIndex(pImageName);
    if i < 0 then
      exit;
    Stream := TSynMemoryStream.Create(ZipRead.UnZip(i));
    try

      auxBitmap := TBitmap.Create;
      with auxBitmap do
      begin
        LoadFromStream(Stream);
        Transparent := True;
        TransparentMode := tmAuto;
        ImageWidth := Width;
        ImageHeight := Height;
      end;
      CMImagesList.Add(pIndex, auxBitmap);

      auxInactiveBitmap := TBitmap.Create;
      auxInactiveBitmap.Assign(auxBitmap);
      MakeBitmapGrey(auxInactiveBitmap);
      with auxInactiveBitmap do
      begin
        Transparent := True;
        TransparentMode := tmAuto;
      end;
      CMInactiveImagesList.Add(pIndex, auxInactiveBitmap);

      auxActiveInactiveBitmap := TBitmap.Create;
      with auxActiveInactiveBitmap do
      begin
        Canvas.Brush.Color := clFuchsia;
        Height := ImageHeight;
        Width := ImageWidth * 2;
        Canvas.Draw(0, 0, auxBitmap);
        Canvas.Draw(ImageWidth, 0, auxInactiveBitmap);
        TransparentColor := clFuchsia;
        Transparent := True;
        TransparentMode := tmAuto;
      end;
      CMActiveInactiveImagesList.Add(pIndex, auxActiveInactiveBitmap);

    finally
      Stream.Free;
    end;
  end;

begin
  CMImagesList.DeleteAll;
  CMInactiveImagesList.DeleteAll;
  CMActiveInactiveImagesList.DeleteAll;

  ZipRead := TZipRead.Create(HInstance, 'Zip', 'ZIP');
  with ZipRead do
    try
      LoadImage(TImgCerrarPestania, 'ImgCerrarPestania.bmp');
      LoadImage(TImgBuscar12, 'ImgBuscar12.bmp');
      LoadImage(TImgAgregar12, 'ImgAgregar12.bmp');
      LoadImage(TImgEditar12, 'ImgEditar12.bmp');
      LoadImage(TImgEliminar12, 'ImgEliminar12.bmp');
      LoadImage(TImgHistorial12, 'ImgHistorial12.bmp');
      LoadImage(TImgBuscar16, 'ImgBuscar16.bmp');
      LoadImage(TImgAgregar16, 'ImgAgregar16.bmp');
      LoadImage(TImgEditar16, 'ImgEditar16.bmp');
      LoadImage(TImgEliminar16, 'ImgEliminar16.bmp');
      LoadImage(TImgActualizar16, 'ImgActualizar16.bmp');
      LoadImage(TImgAceptar16, 'ImgAceptar16.bmp');
      LoadImage(TImgCerrar16, 'ImgCerrar16.bmp');
      LoadImage(TImgCerrar32, 'ImgCerrar32.bmp');
      LoadImage(TImgCancelar16, 'ImgCancelar16.bmp');
      LoadImage(TImgColumnas16, 'ImgColumnas16.bmp');
      LoadImage(TImgColumnasConfig16, 'ImgColumnasConfig16.bmp');
      LoadImage(TImgExcel16, 'ImgExcel16.bmp');
      LoadImage(TImgHabilitar16, 'ImgHabilitar16.bmp');
      LoadImage(TImgPagado16, 'ImgPagado16.bmp');
      LoadImage(TImgAdmin16, 'ImgAdmin16.bmp');
      LoadImage(TImgAdminReportes16, 'ImgAdminReportes16.bmp');
      LoadImage(TImgEmail16, 'ImgEmail16.bmp');
      LoadImage(TImgImpresora16, 'ImgImpresora16.bmp');
      LoadImage(TImgImpresoraConfig16, 'ImgImpresoraConfig16.bmp');
      LoadImage(TImgPrevisualizar16, 'ImgPrevisualizar16.bmp');
      LoadImage(TImgPDF16, 'ImgPDF16.bmp');
      LoadImage(TImgInformacion16, 'ImgInformacion16.bmp');
      LoadImage(TImgInformacion32, 'ImgInformacion32.bmp');
      LoadImage(TImgAtencion16, 'ImgAtencion16.bmp');
      LoadImage(TImgRegModificado13, 'ImgRegModificado13.bmp');
      LoadImage(TImgRegSinModificar13, 'ImgRegSinModificar13.bmp');
      LoadImage(TImgChek13, 'ImgChek13.bmp');
      LoadImage(TImgUnchek16, 'ImgUnchek16.bmp');
      LoadImage(TImgChek16, 'ImgChek16.bmp');
      LoadImage(TImgUnchek13, 'ImgUnchek13.bmp');
      LoadImage(TImgOrdenAZ16, 'ImgOrdenAZ16.bmp');
      LoadImage(TImgOrdenZA16, 'ImgOrdenZA16.bmp');
      LoadImage(TImgOrdenAvanzado16, 'ImgOrdenAvanzado16.bmp');
      LoadImage(TImgOrdenEliminar16, 'ImgOrdenEliminar16.bmp');
      LoadImage(TImgFiltroEmbudo16, 'ImgFiltroEmbudo16.bmp');
      LoadImage(TImgFiltroEmbudoCruz16, 'ImgFiltroEmbudoCruz16.bmp');
      LoadImage(TImgFiltroEmbudoInicializar16, 'ImgFiltroEmbudoInicializar16.bmp');
      LoadImage(TImgCancelarTexto16_VF, 'ImgCancelarTexto16_VF.bmp');
      LoadImage(TImgMasCeleste16, 'ImgMasCeleste16.bmp');
      LoadImage(TImgMenosCeleste16, 'ImgMenosCeleste16.bmp');
      LoadImage(TImgAgrupaciones32, 'ImgAgrupaciones32.bmp');
      LoadImage(TImgRegPrimero16, 'ImgRegPrimero16.bmp');
      LoadImage(TImgRegSiguiente16, 'ImgRegSiguiente16.bmp');
      LoadImage(TImgRegAnterior16, 'ImgRegAnterior16.bmp');
      LoadImage(TImgRegUltimo16, 'ImgRegUltimo16.bmp');
      LoadImage(TImgRegActualizar16, 'ImgRegActualizar16.bmp');
      LoadImage(TImgPasarSeleccionVertical16, 'ImgPasarSeleccionVertical16.bmp');
      LoadImage(TImgPasarTodoVertical16, 'ImgPasarTodoVertical16.bmp');
      LoadImage(TImgVolverSeleccionVertical16, 'ImgVolverSeleccionVertical16.bmp');
      LoadImage(TImgVolverTodoVertical16, 'ImgVolverTodoVertical16.bmp');
      LoadImage(TImgSeleccionarTodo16, 'ImgSeleccionarTodo16.bmp');
      LoadImage(TImgSeleccionarNada16, 'ImgSeleccionarNada16.bmp');
      LoadImage(TImgSeleccionarInvertido16, 'ImgSeleccionarInvertido16.bmp');
      LoadImage(TImgRegAgregar16, 'ImgRegAgregar16.bmp');
      LoadImage(TImgRegEditar16, 'ImgRegEditar16.bmp');
      LoadImage(TImgRegVer16, 'ImgRegVer16.bmp');
      LoadImage(TImgRegEliminar16, 'ImgRegEliminar16.bmp');
      LoadImage(TImgOcultarMostrarPanel16, 'ImgOcultarMostrarPanel16.bmp');
      LoadImage(TImgCalculadora16, 'ImgCalculadora16.bmp');
      LoadImage(TImgLetrasComprob32, 'ImgLetrasComprob32.bmp');
      LoadImage(TImgCandadoAbierto16, 'ImgCandadoAbierto16.bmp');
      LoadImage(TImgCandadoCerrado16, 'ImgCandadoCerrado16.bmp');
      LoadImage(TImgCarpetaVacia16, 'ImgCarpetaVacia16.bmp');
      LoadImage(TImgCarpetaLlena16, 'ImgCarpetaLlena16.bmp');
      LoadImage(TImgHojaEscrita16, 'ImgHojaEscrita16.bmp');
      LoadImage(TImgSeleccionarConjunto16, 'ImgSeleccionarConjunto16.bmp');
      LoadImage(TImgEngranajeMenu24, 'ImgEngranajeMenu24.bmp');
      LoadImage(TImgEngranaje16, 'ImgEngranaje16.bmp');
      LoadImage(TImgLuzNula14, 'ImgLuzNula14.bmp');
      LoadImage(TImgLuzGris14, 'ImgLuzGris14.bmp');
      LoadImage(TImgLuzAmarilla14, 'ImgLuzAmarilla14.bmp');
      LoadImage(TImgLuzVerde14, 'ImgLuzVerde14.bmp');
      LoadImage(TImgLuzRoja14, 'ImgLuzRoja14.bmp');
      LoadImage(TImgLuzAzul14, 'ImgLuzAzul14.bmp');
      LoadImage(TImgDestello16, 'ImgDestello16.bmp');
      LoadImage(TImgDestello27, 'ImgDestello27.bmp');
      LoadImage(TImgCopiar16, 'ImgCopiar16.bmp');
    finally
      Free;
    end;
end;

function TConfigApp.GetCMImage(pTCMImage: TCMImage): TBitmap;
begin
  if not CMImagesList.FindAndCopy(pTCMImage, Result) then
    ShowMessage('Undefined Imange in CMImages.res file');
end;

function TConfigApp.GetCMImageInactive(pTCMImage: TCMImage): TBitmap;
begin
  if not CMInactiveImagesList.FindAndCopy(pTCMImage, Result) then
    ShowMessage('Undefined Imange in CMImages.res file');
end;

function TConfigApp.GetCMImageActiveInactive(pTCMImage: TCMImage): TBitmap;
begin
  if not CMActiveInactiveImagesList.FindAndCopy(pTCMImage, Result) then
    ShowMessage('Undefined Imange in CMImages.res file');
end;

function TConfigApp.CanContinue(var MsgCanContinue: ansistring): boolean;
begin
  case ExecutionMode of
    emClientServer:
    begin
      MsgCanContinue := 'ClientServer Mode';

      ConnectionInternalServiceConfig :=
        TConnectionServiceConfig.Create(AppFiles.ConnectionConfigServiceName);
      ConnectionInternalServiceConfig.Read;
      CreateInternalService(ConnectionInternalServiceConfig, Result, MsgCanContinue);

      { If CanContinue (result = True) then create the client with it
        configuration as the internal server, to make sure you use the same configuration}
      if Result then
        CreateClient(TConnectionConfig(ConnectionInternalServiceConfig),
          Result, MsgCanContinue);
    end;
    emClient:
    begin
      MsgCanContinue := 'Client Mode';
      ConnectionConfig := TConnectionConfig.Create(AppFiles.ConnectionConfigName);
      ConnectionConfig.Read;
      CreateClient(ConnectionConfig, Result, MsgCanContinue);
    end;
  end;
end;

initialization
  TJSONSerializer.RegisterObjArrayForJSON(TypeInfo(TBitmapObjArray), TBitmap);

end.
