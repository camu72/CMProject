unit UFmTest01;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, ExtCtrls, UCMForm, ucmlcl, VirtualTrees, UCMSearcherForm, TypInfo,
  UCMConfigApp, UCMModel, SynCommons, UCMFunctions, UCMMormot,
  SynLog, UCMCommons;

type

  { TShowNotifiWindowsShowTypes }

  TShowNotifiWindowsShowTypes = class(TCMEnumType)
    procedure OnRequestingValues(pValueType: Integer; pDefaultText: rawUTF8;
  var pTypeName, pShortName: rawUTF8); override;
    constructor Create; override;
  end;

  { TFmTest01 }

  TFmTest01 = class(TCMForm)
    btAdd: TBitBtn;
    btGetFile: TBitBtn;
    btActive: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn3: TBitBtn;
    btInactive: TBitBtn;
    BitBtn2: TBitBtn;
    btFinder: TBitBtn;
    btColorTest: TBitBtn;
    btDisable: TBitBtn;
    btStandardNotification: TButton;
    btSum: TButton;
    ColorDialog1: TColorDialog;
    edNumber1: TEdit;
    edNumber2: TEdit;
    edResult: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbFilesToTransfer: TListBox;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    TrackBar1: TTrackBar;
    procedure btGetFileClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btFinderClick(Sender: TObject);
    procedure btColorTestClick(Sender: TObject);
    procedure btSumClick(Sender: TObject);
    procedure btStandardNotificationClick(Sender: TObject);
    procedure edNumber1KeyPress(Sender: TObject; var Key: char);
    procedure TrackBar1Change(Sender: TObject);
  protected
    accAddORM: TActionCM;
    ObjShowNotificationsModes: TShowNotifiWindowsShowTypes;
    procedure InitializingCreate(Sender: TObject); override;
    procedure ProcessingCreate(Sender: TObject); override;
    procedure EndingDestroy(Sender: TObject); override;
    procedure AssignImages; override;
  public
    ActionsCM: TActionsCM;
  end;

var
  FmTest01: TFmTest01;

implementation

uses
  UFmMain;

{$R *.lfm}

{ TShowNotifiWindowsShowTypes }

procedure TShowNotifiWindowsShowTypes.OnRequestingValues(pValueType: Integer;
  pDefaultText: rawUTF8; var pTypeName, pShortName: rawUTF8);
begin
  inherited OnRequestingValues(pValueType, pDefaultText, pTypeName, pShortName);
  case TShowNotificationMode(pValueType) of
    snmBottomLeftAsociatedForm: begin
      pTypeName := 'Bottom Left - Form';
      pShortName:= 'BLF'; end;
    snmBottomRightAsociatedForm: begin
      pTypeName := 'Bottom Right - Form';
      pShortName:= 'BRF'; end;
    snmTopLeftAsociatedForm: begin
      pTypeName := 'Top Left - Form';
      pShortName:= 'TLF'; end;
    snmTopRightAsociatedForm: begin
      pTypeName := 'Top Right - Form';
      pShortName:= 'TRF'; end;
    snmBottomLeftScreen: begin
      pTypeName := 'Bottom Left - Screen';
      pShortName:= 'BLS'; end;
    snmBottomRightScreen: begin
      pTypeName := 'Bottom Right - Screen';
      pShortName:= 'BRS'; end;
    snmTopLeftScreen: begin
      pTypeName := 'Top Left - Screen';
      pShortName:= 'TLS'; end;
    snmTopRightScreen: begin
      pTypeName := 'Top Right - Screen';
      pShortName:= 'TRF'; end;
  end;
end;

constructor TShowNotifiWindowsShowTypes.Create;
begin
  inherited Create;
  EnumTypeInfo := TypeInfo(TShowNotificationMode);
  //TiposNoSeleccionables := [Ord(amSinDefinir)];
end;

{ TFmTest01 }

procedure TFmTest01.btColorTestClick(Sender: TObject);
begin
  ColorDialog1.Color := ConfigApp.ColorApp;
  ColorDialog1.Execute;
  Shape1.Brush.Color := MixColors(ColorDialog1.Color, clWhite, 100);
  Shape2.Brush.Color := MixColors(ColorDialog1.Color, clWhite, 80);
  Shape3.Brush.Color := MixColors(ColorDialog1.Color, clWhite, 60);
  Shape4.Brush.Color := MixColors(ColorDialog1.Color, clWhite, 40);
  Shape5.Brush.Color := MixColors(ColorDialog1.Color, clWhite, 20);

  ConfigApp.ColorApp := ColorDialog1.Color;

  FmMain.RefreshAccordingColorApp;
end;

procedure TFmTest01.btSumClick(Sender: TObject);
var
  a, b: double;
begin
  a := StrToFloat(edNumber1.Text);
  b := StrToFloat(edNumber2.Text);
  edResult.Text := FloatToStr(Client.Sum(a,b));
  ShowNotification('''Sum service'' invoked in server','Method based service',
    3000,clMoneyGreen,False,0,0,snmTopRightAsociatedForm);
end;

procedure TFmTest01.btAddClick(Sender: TObject);
var
  Role: TSQLRole;
begin
  Role := TSQLRole.Create;
  Role.Role := 'Cliente';
  Role.Abbreviation:= 'Cli.';
  Role.Plural:= 'Clientes';
  Client.Add(Role,True);

  ShowMessage('Agregado:' + LineEnding + ObjectToJSON(Role, [woHumanReadable]));
  Role.Free;
end;

procedure TFmTest01.btFinderClick(Sender: TObject);
begin
  TCMSearcherForm.ShowModalAutoRelease;
end;

procedure TFmTest01.btGetFileClick(Sender: TObject);
var
  SuccefullTransfer, FileAvailableInServer: boolean;
  FileToSave: TFileName;
  FileToTransfer: string;
  auxMsg: RawUTF8;
begin
  if lbFilesToTransfer.ItemIndex < 0 then exit;

  SuccefullTransfer := False;
  FileToTransfer := lbFilesToTransfer.Items[lbFilesToTransfer.ItemIndex];
  lbFilesToTransfer.Items[lbFilesToTransfer.ItemIndex];
  if Client.TransferFileService.FileAvailable( FileToTransfer, auxMsg) then
  begin
    OpenDialog1.Title := 'Save file as';
    if OpenDialog1.Execute then
      FileToSave := OpenDialog1.FileName
    else
      exit;

    WaitingCursor;
    try
      FileFromString(
        Client.GetFile(FileToTransfer, SuccefullTransfer),
        FileToSave);

      if SuccefullTransfer then
        ShowMessage('Succefull transfer.')
      else
        ShowMessage('Transference failure.');
    finally
      RestoreCursor;
    end;
  end
  else
    ShowMessage(auxMsg);
end;

procedure TFmTest01.btStandardNotificationClick(Sender: TObject);
begin
  ShowNotification('Test for standard notification');
end;


procedure TFmTest01.edNumber1KeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [#8, '0'..'9', '-', FormatSettings.DecimalSeparator]) then
  begin
    Key := #0;
  end
  else if ((Key = FormatSettings.DecimalSeparator) or (Key = '-')) and
          (Pos(Key, TEdit(Sender).Text) > 0) then
  begin
    Key := #0;
  end
  else if (Key = '-') and
          (TEdit(Sender).SelStart <> 0) then
  begin
    Key := #0;
  end;
end;

procedure TFmTest01.TrackBar1Change(Sender: TObject);
var
  vTitle, vName, vShortName: string;
begin
  vName := UTF8ToString(ObjShowNotificationsModes.EnumTypeOriginalName(TrackBar1.Position));
  vShortName := UTF8ToString(ObjShowNotificationsModes.EnumTypeShortName(TrackBar1.Position));
  vTitle := UTF8ToString(ObjShowNotificationsModes.EnumTypeName(TrackBar1.Position));

  ShowNotification(vName + ' - ' + vShortName +
    LineEnding +
    'This is a 5 seconds test with fixedwidth font.' + LineEnding +
    'Wait 5 seconds or close window', vTitle,
    5000, clMoneyGreen, True, 0, 0,
    TShowNotificationMode(TrackBar1.Position));
end;

procedure TFmTest01.InitializingCreate(Sender: TObject);
var
  Count,i : Longint;
  FileNameDA: TFileNameDynArray;
  DA: TDynArray;
  vMsg: RawUTF8;
begin
  inherited InitializingCreate(Sender);
  { Fill the list of Files availables for FileTransfer }
  FileNameDA := Client.TransferFileService.ListFilesAvailables(vMsg);
  DA.Init(TypeInfo(TFileNameDynArray),FileNameDA);
  { Only show the first 5 files }
  if DA.Count > 5 then Count := 5
  else Count := DA.Count;
  for i := 0 to Count-1 do
      lbFilesToTransfer.Items.Add(FileNameDA[i]);
  { Point the first item }
  lbFilesToTransfer.ItemIndex := 0;

  ObjShowNotificationsModes := TShowNotifiWindowsShowTypes.Create;

  ActionsCM := TActionsCM.Create(self);
  with ActionsCM do
  begin
    CreateAction('Find test ...','Finder class of framwork',
      TImgBuscar16,@btFinderClick,'Ctrl+F').AssignToButton(btFinder);

    { If the action is needed to be used in addition to the button ... }
    accAddORM := CreateAction('Add ORM test','Add automat record by ORM',
      TImgRegAgregar16, @btAddClick,'');
    accAddORM.AssignToButton(btAdd);
  end;
end;

procedure TFmTest01.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);

end;

procedure TFmTest01.EndingDestroy(Sender: TObject);
begin
  ObjShowNotificationsModes.Free;
  if Assigned(ActionsCM) then
    ActionsCM.Free;
  inherited EndingDestroy(Sender);
end;

procedure TFmTest01.AssignImages;
begin
  inherited AssignImages;
  //btAdd.Glyph := ConfigApp.GetCMImage(TImgRegAgregar16);
  //btFinder.Glyph := ConfigApp.GetCMImageInactive(TImgBuscar16);

  btActive.Glyph := ConfigApp.GetCMImage(TImgCalculadora16);
  btInactive.Glyph := ConfigApp.GetCMImageInactive(TImgCalculadora16);
  btDisable.Glyph := ConfigApp.GetCMImage(TImgCalculadora16);

  BitBtn1.Glyph := ConfigApp.GetCMImage(TImgExcel16);
  BitBtn2.Glyph := ConfigApp.GetCMImage(TImgEmail16);
  BitBtn3.Glyph := ConfigApp.GetCMImage(TImgFiltroEmbudo16);
end;

end.

