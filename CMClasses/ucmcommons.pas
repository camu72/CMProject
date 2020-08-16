unit UCMCommons;

{$mode objfpc}{$H+}

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

interface

uses
  Classes, SysUtils, Windows, Controls, Dialogs,
  Graphics, ActnList, Buttons, UCMFunctions, SynLog, LCLProc,
  SynCommons, mORMot;

const
  CMD_LINE_PARAM_NO_UPDATE = 'NoUpdate';
  CMD_LINE_PARAM_CLIENTSERVER = 'cs';

  VERSION_EXTENSION    = '.version';
  CONNECTION_EXTENSION  = '.connection';

  DEFAULT_SERVER_NAME  = 'localhost';
  DEFAULT_SERVER_PORT  = '888';

  HTTPSERVICENAME = 'CMHttpServerService';
  HTTPSERVICEDISPLAYNAME = 'CM Http Server Service';

type
  TShowFormMode = (sfmShowInTab, sfmShowModal, sfmShowCustom, sfmNotShow);
  TExecutionMode = (emClient, emClientServer);

  TSetOfByte = set of byte;
  TValidation = (tvExistence, tvPositive);

  TSelectionChecksMode = (schmAll, schmAnyone, schmInvert);

  TCMImage = (
    TImgNinguna, TImgCerrarPestania,
    TImgBuscar12, TImgAgregar12, TImgEditar12, TImgEliminar12, TImgHistorial12,
    TImgBuscar16, TImgAgregar16, TImgEditar16, TImgEliminar16, TImgActualizar16,
    TImgAceptar16, TImgCancelar16,
    TImgCerrar16, TImgCerrar32,
    TImgAdmin16, TImgAdminReportes16,
    TImgEmail16, TImgColumnas16, TImgColumnasConfig16, TImgExcel16,
    TImgHabilitar16,  TImgPagado16,
    TImgImpresora16,  TImgImpresoraConfig16,
    TImgPrevisualizar16,
    TImgInformacion16, TImgInformacion32,
    TImgAtencion16, TImgPDF16,
    TImgRegModificado13, TImgRegSinModificar13,
    TImgChek13, TImgUnchek13, TImgChek16, TImgUnchek16,
    TImgOrdenAZ16, TImgOrdenZA16, TImgOrdenAvanzado16, TImgOrdenEliminar16,
    TImgFiltroEmbudo16, TImgFiltroEmbudoCruz16, TImgFiltroEmbudoInicializar16,
    TImgCancelarTexto16_VF, TImgMasCeleste16, TImgMenosCeleste16,
    TImgAgrupaciones32,
    TImgRegPrimero16, TImgRegSiguiente16, TImgRegAnterior16, TImgRegUltimo16,
    TImgRegActualizar16,
    TImgPasarSeleccionVertical16, TImgPasarTodoVertical16,
    TImgVolverSeleccionVertical16, TImgVolverTodoVertical16,
    TImgSeleccionarTodo16, TImgSeleccionarNada16, TImgSeleccionarInvertido16,
    TImgRegAgregar16, TImgRegEditar16, TImgRegVer16, TImgRegEliminar16,
    TImgOcultarMostrarPanel16, TImgCalculadora16,
    TImgLetrasComprob32,
    TImgCandadoAbierto16, TImgCandadoCerrado16,
    TImgCarpetaVacia16, TImgCarpetaLlena16, TImgHojaEscrita16,
    TImgSeleccionarConjunto16, TImgEngranajeMenu24, TImgEngranaje16,
    TImgLuzNula14, TImgLuzGris14, TImgLuzAmarilla14, TImgLuzVerde14,
    TImgLuzRoja14, TImgLuzAzul14,
    TImgDestello16, TImgDestello27,
    TImgCopiar16);
  { See de unit UCMConfigApp to load the Images }

  { TCMSynPersistent }

  TCMSynPersistent = class(TSynPersistent)
  protected
    ConfigurationName: TFilename;
  public
    constructor Create(pConfigurationName: TFileName); overload; virtual;
    procedure Save;
    procedure Read;
    function AsHumanReadableJSON: RawUTF8;
  end;

  { TVersionApp }

  TVersionApp = Class(TCMSynPersistent)
  private
    fVersion: Double;
    fRelease: Integer;
    function GetVersionRelease: RawUTF8;
  published
    property VersionRelease: RawUTF8 read GetVersionRelease;
    property Version: Double read fVersion write fVersion;
    property Release: Integer read fRelease write fRelease;
  end;

  { TConnectionConfig }

  TConnectionConfig = Class(TCMSynPersistent)
  private
    FPort: RawUTF8;
    FServerName: RawUTF8;
  public
    constructor Create(pConnectionFileName: TFileName); reintroduce;
  published
    property ServerName: RawUTF8 read FServerName write FServerName;
    property Port: RawUTF8 read FPort write FPort;
  end;

  { TConnectionServiceConfig }

  TConnectionServiceConfig = Class(TConnectionConfig)
  private
    FDBName: RawUTF8;
    FDBPath: RawUTF8;
    FServiceDescription: RawUTF8;
    FServiceDisplayName: RawUTF8;
    FServiceName: RawUTF8;
    FUpdatableFilesFolder: RawUTF8;
  public
    function CanConnectDB(var pMsg: AnsiString): boolean;
    function CanCreateNewDB(var pMsg: AnsiString): boolean;
    function DBNameWithPath: RawUTF8;
    constructor Create(pConnectionFileName: TFileName); reintroduce;
    property ServiceName: RawUTF8 read FServiceName write FServiceName;
    property ServiceDisplayName: RawUTF8 read FServiceDisplayName write FServiceDisplayName;
    property ServiceDescription: RawUTF8 read FServiceDescription write FServiceDescription;
  published
    property UpdatableFilesFolder: RawUTF8 read FUpdatableFilesFolder write FUpdatableFilesFolder;
    property DBName: RawUTF8 read FDBName write FDBName;
    property DBPath: RawUTF8 read FDBPath write FDBPath;
  end;

  { TActionListCM }

  TActionListCM = class(TActionList)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TActionCM }

  TActionCM = class(TCustomAction)
  public
    destructor Destroy; override;
    procedure AssignToButton(pButton: TControl);
  end;

  { Wrapper around a TImageList, that allow get an image from the integred
  Images in the application }
  { TCMImageListManager }

  TCMImageListManager = class(TObject)
  private
    ImageList: TImageList;
    ImagesCount: integer;
    ImagesIndexArr: array of TCMImage;
    function AddImageToList(pImage: TCMImage): integer; { Reurn an Index }
  public
    function ImageIndex(pCMImage: TCMImage): integer;
    constructor create(pImageList: TImageList);
    destructor Destroy; override;
  end;

  { TActionsCM }

  TActionsCM = class(TObject)
  private
    ActionsCount: integer;
  public
    ActionListCM: TActionListCM;
    ImagListManager: TCMImageListManager;
    ImageList: TImageList;
    constructor create(Owner: TComponent);
    destructor Destroy; override;
    function CreateAction(pCaption, pHint: string; pImage: TCMImage;
      pOnExecute: TNotifyEvent; TextShortCut: string = ''): TActionCM;
  end;



{ Fuente SmootArray  ( cambia tamaños de bitmaps de forma suavizada )
https://groups.google.com/forum/#!topic/borland.public.delphi.graphics/n7vFd1RJMZI }
TRGBArray = ARRAY[0..32767] OF TRGBTRIPLE;
pRGBArray = ^TRGBArray;

procedure SmoothResize(abmp:TBitmap; NewWidth, NewHeight:integer);
procedure ResizeCanvas(abmp:TBitmap; NewWidth, NewHeight:integer);

implementation

uses
  UCMConfigApp;

{ TCMImageListManager }


procedure SmoothResize(abmp:TBitmap; NewWidth,NewHeight:integer);
var
  xscale, yscale         : Single;
  sfrom_y, sfrom_x       : Single;
  ifrom_y, ifrom_x       : Integer;
  to_y, to_x             : Integer;
  weight_x, weight_y     : array[0..1] of Single;
  weight                 : Single;
  new_red, new_green     : Integer;
  new_blue               : Integer;
  total_red, total_green : Single;
  total_blue             : Single;
  ix, iy                 : Integer;
  bTmp : TBitmap;
  sli, slo               : pRGBArray;
begin
  //abmp.PixelFormat := pf24bit;
  bTmp := TBitmap.Create;
  //bTmp.PixelFormat := pf24bit;
  bTmp.Width := NewWidth;
  bTmp.Height := NewHeight;
  xscale := bTmp.Width / (abmp.Width-1);
  yscale := bTmp.Height / (abmp.Height-1);
  for to_y := 0 to bTmp.Height-1 do begin
    sfrom_y := to_y / yscale;
    ifrom_y := Trunc(sfrom_y);
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    for to_x := 0 to bTmp.Width-1 do begin
      sfrom_x := to_x / xscale;
      ifrom_x := Trunc(sfrom_x);
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];
      total_red   := 0.0;
      total_green := 0.0;
      total_blue  := 0.0;
      for ix := 0 to 1 do begin
        for iy := 0 to 1 do begin
          sli := abmp.ScanLine[ifrom_y + iy];
          new_red := sli^[ifrom_x + ix].rgbtRed;
          new_green := sli^[ifrom_x + ix].rgbtGreen;
          new_blue := sli^[ifrom_x + ix].rgbtBlue;

          weight := weight_x[ix] * weight_y[iy];
          total_red   := total_red   + new_red   * weight;
          total_green := total_green + new_green * weight;
          total_blue  := total_blue  + new_blue  * weight;
        end;
      end;
      slo := bTmp.ScanLine[to_y];
      slo^[to_x].rgbtRed := Round(total_red);
      slo^[to_x].rgbtGreen := Round(total_green);
      slo^[to_x].rgbtBlue := Round(total_blue);
    end;
  end;
  abmp.Width := bTmp.Width;
  abmp.Height := bTmp.Height;
  abmp.Canvas.Draw(0,0,bTmp);
  bTmp.Free;
end;

procedure ResizeCanvas(abmp: TBitmap; NewWidth, NewHeight: integer);
var
  DeltaX, DeltaY         : Integer;
  bTmp: TBitmap;
  vColor: TColor;
begin
  bTmp := TBitmap.Create;
  bTmp.Width := NewWidth;
  bTmp.Height := NewHeight;

  vColor := GetColorFromBitmap(abmp,btmp.Width,btmp.Height);
  btmp.Canvas.Brush.Color := vColor;
  btmp.Canvas.FillRect(0,0,btmp.Width, btmp.Height);

  DeltaX := Round((bTmp.Width - abmp.Width)/2);
  DeltaY := Round((bTmp.Height - abmp.Height)/2);
  btmp.Canvas.Draw(DeltaX, DeltaY,abmp);

  abmp.Width := bTmp.Width;
  abmp.Height := bTmp.Height;
  abmp.Canvas.Draw(0,0,bTmp);

  abmp.TransparentMode := tmFixed;
  abmp.TransparentColor := GetColorFromBitmap(abmp,0, 0);
  abmp.Transparent := True;

  bTmp.Free;
end;

{ TActionListCM }

constructor TActionListCM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TSynLog.Add.Log( sllCustom1, 'Creating ActionListCM');
end;

destructor TActionListCM.Destroy;
begin
  TSynLog.Add.Log( sllCustom1, 'Releasing ActionListCM');
  inherited Destroy;
end;

{ TActionsCM }

constructor TActionsCM.create(Owner: TComponent);
begin
  TSynLog.Add.Log( sllCustom1, 'Creating ActionsCM...');
  ImageList := TImageList.Create(Owner);
  ImagListManager := TCMImageListManager.create(ImageList);
  ActionListCM := TActionListCM.Create(Owner);
  ActionListCM.Images := ImageList;
  ActionsCount := 0;
end;

destructor TActionsCM.Destroy;
begin
  TSynLog.Add.Log( sllCustom1, 'Releasing ActionsCM...');
  ImagListManager.Free;
  inherited Destroy;
end;

function TActionsCM.CreateAction(pCaption, pHint: string; pImage: TCMImage;
  pOnExecute: TNotifyEvent; TextShortCut: string): TActionCM;
begin
  result := TActionCM.Create(ActionListCM);
  with result do
  begin
    Name := 'Accion' + IntToStr(ActionsCount);
    Inc(ActionsCount);
    ActionList := TActionList(ActionListCM);
    Caption := pCaption;
    Hint := pHint;
    ImageIndex := ImagListManager.ImageIndex(pImage);
    OnExecute := pOnExecute;
    if TextShortCut <> '' then
      ShortCut := TextToShortCut(TextShortCut);
  end;
end;

{ TActionCM }

destructor TActionCM.Destroy;
begin
  TSynLog.Add.Log( sllCustom1, 'Releasing ActionCM: ' + Caption);
  inherited Destroy;
end;

procedure TActionCM.AssignToButton(pButton: TControl);
begin
  if pButton is TSpeedButton then
  begin
    TSpeedButton(pButton).Glyph := nil;
    TSpeedButton(pButton).Action := Self;
  end
  else if pButton is TBitBtn then
  begin
    TBitBtn(pButton).Glyph := nil;
    TBitBtn(pButton).Action := Self;
    TBitBtn(pButton).Margin := 4;
    TBitBtn(pButton).Spacing := 4;
    TBitBtn(pButton).Layout := blGlyphLeft;
  end
  else
    ShowMessage('Complete assingnment to buttons of type: ' + pButton.ClassName);

end;


function TCMImageListManager.AddImageToList(pImage: TCMImage): integer;

   function AddImageTransparentBackground(pImage: TCMImage): integer;
    var
      auxBitmap: TBitmap;
    begin
      auxBitmap := TBitmap.Create;
      auxBitmap.Assign(ConfigApp.GetCMImage(pImage));

      auxBitmap.TransparentMode := tmFixed;
      auxBitmap.TransparentColor := GetColorFromBitmap(auxBitmap,0, 0);
      auxBitmap.Transparent := True;

      { If is the 1st image added, define the size of images in ImagegsList }
      if ImagesCount = 0 then
      begin
        ImageList.Width := auxBitmap.Width;
        ImageList.Height := auxBitmap.Height;
      end
      { else if Width and Height are both minors, then resize de bitmap  }
      else if (ImageList.width > auxBitmap.Width ) and (ImageList.Height > auxBitmap.Height) then
        ResizeCanvas(auxBitmap, ImageList.Width, ImageList.Height)
      { else adjust the image size, to de ImageList size ... }
      else if (ImageList.width <> auxBitmap.Width ) or (auxBitmap.Height <> ImageList.Height) then
        SmoothResize(auxBitmap, ImageList.Width, ImageList.Height);



      ImagesCount := ImagesCount + 1;
      SetLength(ImagesIndexArr, ImagesCount);
      result := ImageList.AddMasked(auxBitmap, auxBitmap.TransparentColor);
      ImagesIndexArr[result] := pImage;
    end;
begin
  result := AddImageTransparentBackground(pImage);
end;

constructor TCMImageListManager.create(pImageList: TImageList);
begin
  inherited create;
  TSynLog.Add.Log( sllCustom1, 'Creating CMImageListMagager');
  ImageList := pImageList;
  ImageList.Clear;
  ImagesCount := 0;
end;

function TCMImageListManager.ImageIndex(pCMImage: TCMImage): integer;
var
  i: integer;
  FoundIndex: boolean;
begin
  if pCMImage = TImgNinguna then
  begin
    result := -1;
    exit;
  end;

  { If image not found, then added in runtime ...}
  FoundIndex := False;
  i := 0;
  while (i<ImagesCount) and (not FoundIndex) do
  begin
    if ImagesIndexArr[i] = pCMImage then
    begin
      result := i;
      FoundIndex := True;
    end
    else
      i := i + 1;
  end;

  if not FoundIndex then
    result := AddImageToList(pCMImage);
end;

destructor TCMImageListManager.Destroy;
begin
  TSynLog.Add.Log( sllCustom1, 'Destroying CMImageListMagager');
  inherited Destroy;
end;

{ TConnectionServiceConfig }

function TConnectionServiceConfig.CanConnectDB(var pMsg: AnsiString): boolean;
begin
  if DirectoryExists(DBPath) then
  begin
    if DBName <> '' then
    begin
      if FileExists(DBNameWithPath) then
        result := True
      else
        pMsg := 'DB filename does not exist';
    end
    else
      pMsg := 'DB filename is empty';
  end
  else
    pMsg := 'DB path does not exist';
end;

function TConnectionServiceConfig.CanCreateNewDB(var pMsg: AnsiString): boolean;
begin
  result := False;
  if DirectoryExists(DBPath) then
  begin
    if DBName <> '' then
    begin
      if not FileExists(DBNameWithPath) then
        result := True
      else
        pMsg := 'DB filename exist. Can not override.'
    end
    else
      pMsg := 'DB filename is empty';
  end
  else
    pMsg := 'DB path does not exist';
end;

function TConnectionServiceConfig.DBNameWithPath: RawUTF8;
begin
  result := DBPath + PathDelim + DBName;
end;

constructor TConnectionServiceConfig.Create(pConnectionFileName: TFileName);
begin
  inherited Create(pConnectionFileName);
  UpdatableFilesFolder := ExtractFileDir(pConnectionFileName);
  ServiceName          := HTTPSERVICENAME;
  ServiceDisplayName   := HTTPSERVICEDISPLAYNAME;
  ServiceDescription   := HTTPSERVICEDISPLAYNAME + ' - ' + ChangeFileExt(ExtractFileName(ApplicationName),'');

  DBPath           := ExtractFileDir(pConnectionFileName);
  DBName           := ChangeFileExt(ExtractFileName(ApplicationName),'.db3');
end;

{ TConnectionConfig }
constructor TConnectionConfig.Create(pConnectionFileName: TFileName);
begin
  inherited Create(pConnectionFileName);
  ServerName       := DEFAULT_SERVER_NAME;
  Port             := DEFAULT_SERVER_PORT;
end;


{ TCMSynPersistent }

constructor TCMSynPersistent.Create(pConfigurationName: TFileName);
begin
  inherited create;
  if pConfigurationName = '' then
    ConfigurationName := ExtractFilePath(ApplicationName) + ClassName + '.json'
  else
    ConfigurationName := pConfigurationName;
end;

procedure TCMSynPersistent.Save;
begin
  ObjectToJSONFile(self, ConfigurationName);
end;

procedure TCMSynPersistent.Read;
begin
  if FileExists(ConfigurationName) then
    JSONFileToObject(ConfigurationName, self)
  else
    Save;
end;

function TCMSynPersistent.AsHumanReadableJSON: RawUTF8;
begin
  Result := ObjectToJSON(self,[woHumanReadable]);
end;

{ TVersionApp }

function TVersionApp.GetVersionRelease: RawUTF8;
begin
  result := FormatFloat('#0.00#', Version) + '.r' + IntToStr(Release);
end;

end.

