unit UCMFMMain;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Menus,
  SynCommons, VirtualTrees, UCMMenuActions, UCMModel, UCMFunctions, UCMMormot,
  UCMForm, UCMConfigApp, UCMCommons, UCMVCL, UCMVTV, UCMFMAbout;
type

  { TCMFmMain }

  TCMFmMain = class(TCMForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    btInfo: TSpeedButton;
    btExit: TSpeedButton;
    btMenu: TSpeedButton;
    edMenu: TEdit;
    ImgPanelButtons: TImage;
    ImgMenuTree: TImageList;
    lbAppMode: TLabel;
    lbAuxAppMode: TLabel;
    pnEditMenu: TPanel;
    pnInfoApp: TPanel;
    pgMenu: TPageControl;
    pnTree: TPanel;
    pnMenu: TFlowPanel;
    pnPages: TPanel;
    Separador1: TPanel;
    Separador2: TPanel;
    splitterMenu: TSplitter;
    TabMenu: TTabSheet;
    vtvMenu: TVirtualStringTree;
    procedure btMenuClick(Sender: TObject);
    procedure OnRestoreApplication(Sender: TObject);
    procedure btExitClick(Sender: TObject);
    procedure btInfoClick(Sender: TObject);
    procedure edMenuChange(Sender: TObject);
    procedure edMenuKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pnMenuResize(Sender: TObject);
    procedure vtvMenuBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtvMenuDblClick(Sender: TObject);
    procedure vtvMenuFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtvMenuGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure vtvMenuGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtvMenuGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vtvMenuInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtvMenuKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState      );
    procedure vtvMenuPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    TabIndex: integer;
    procedure ShowInTab(pMenuRecord: PvtvMenuRecord; pFmToShow: TCMForm;
      pFormInitParam: TCMFormInitParam);
    procedure ShowAsModal(pMenuRecord: PvtvMenuRecord; pFmToShow: TCMForm;
      pFormInitParam: TCMFormInitParam);
    function FocusPageIfItExists(pMenuRecord: PvtvMenuRecord; pFormInitParam: TCMFormInitParam): boolean;
    procedure ConfigurePageOnFocus(pFocusedFm: TCMForm; pFormInitParam: TCMFormInitParam);
  protected
    IdxRoot, IdxMenu, IdxGreenLigth: Integer;
    ImgsListMenu: TCMImageListManager;
    SQLMenu: TSQLMenu;
    MenuDataAux: PvtvMenuRecord;
    procedure ColorizePanelButtons;
    procedure AssignImages; override;
    procedure EndingDestroy(Sender: TObject); override;
    procedure ProcessingCreate(Sender: TObject); override;
    procedure GetNewMenuRecord(var MenuData: PvtvMenuRecord; pIdMenu: integer);
    procedure FillMenuData(var pMenuData: PvtvMenuRecord;
      pSQLMenu: TSQLMenu; pParentNode: PVirtualNode = nil);
    procedure FillMenuTree; virtual;
    procedure FillMissingMenuEntries(var pMenuList: TSynDictionary);
    procedure EndingShow(Sender: TObject); override;
  public
    MenuList: TSynDictionary;
    EditMenu: TCMEditBtCancel;
    pgMain: TPageControlCM;
    procedure RefreshAccordingColorApp;
    procedure ExecuteMenuAction(pMenuRecord: PvtvMenuRecord; pFormInitParam: TCMFormInitParam; pForceModal: boolean = False); overload;
    procedure ExecuteMenuAction(pIdMenu: Integer; pFormInitParam: TCMFormInitParam; pForceModal: boolean = False); overload;
  end;

var
  CMFmMain: TCMFmMain;

implementation

{$R *.lfm}

{ TCMFmMain }

procedure TCMFmMain.btInfoClick(Sender: TObject);
begin
  TCMFmAbout.ShowModalAutoRelease(nil);
end;

procedure TCMFmMain.edMenuChange(Sender: TObject);
begin
  FilterTree(vtvMenu, tvtvMenu, edMenu.Text);
  FocusFirstNode(vtvMenu,tvtvMenu);
end;

procedure TCMFmMain.edMenuKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MenuData: PvtvMenuRecord;
begin
  if Key <> 27 {VK_ESCAPE}then
  begin
    KeyPreview := True;
    if (Key = 40) then
    begin
      FocusNexNode(vtvMenu, tvtvMenu);
      KeyPreview := False;
      key := 0;
    end
    else if (key = 38) then
    begin
      FocusPreviousNode(vtvMenu, tvtvMenu);
      KeyPreview := False;
      key := 0;
    end
    else if (key = 13 {VK_RETURN} ) then
    begin
      key := 0;
      if assigned(vtvMenu.FocusedNode) and (vtvMenu.FullyVisible[vtvMenu.FocusedNode]) then
      begin
        MenuData := vtvMenu.GetNodeData(vtvMenu.FocusedNode);
        ExecuteMenuAction(MenuData, nil);
      end;
      FocusControl(edMenu);
    end;
  end;
end;

procedure TCMFmMain.pnMenuResize(Sender: TObject);
begin
  ColorizePanelButtons;
end;

procedure TCMFmMain.ColorizePanelButtons;
var
  auxBitmap: TBitmap;
  x,y, Porc: integer;
  auxDelta, Delta, IncPorc: Double;
  auxColor: TColor;
begin

  ImgPanelButtons.Width := pnMenu.Width - (pnInfoApp.Left + pnInfoApp.Width) - pnMenu.BorderWidth * 2;


  auxBitmap := TBitmap.Create;
  auxBitmap.Width := ImgPanelButtons.Width;
  auxBitmap.Height := ImgPanelButtons.Height;

  Delta := ImgPanelButtons.Width / 100;
  if Delta < 1 then
    IncPorc := 100 / ImgPanelButtons.Width
  else
    IncPorc := 1;

  auxDelta := 0;

  Porc := 0;
  auxDelta := 0;
  for x := 0 to auxBitmap.Width do
  begin
    if X >= auxDelta then
    begin
      auxDelta := auxDelta + Delta;
      Porc := Porc + round(IncPorc);
      auxColor := MixColors(ConfigApp.Color01, clBtnFace, Porc);
    end;
    for y := 0 to auxBitmap.Height do
    begin
      auxBitmap.Canvas.DrawPixel(x,y,TColorToFPColor(auxColor));
    end;
  end;

  ImgPanelButtons.Picture.Assign(auxBitmap);
  auxBitmap.Free;;
end;

procedure TCMFmMain.vtvMenuBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  MenuData: PvtvMenuRecord;
begin
  EraseAction := eaColor;
  MenuData := vtvMenu.GetNodeData(Node);

  if MenuData^.IMPUTABLE then
    ItemColor := ConfigApp.Color04
  else if MenuData^.LEVEL = 0 then
     ItemColor := ConfigApp.Color01
  else if MenuData^.LEVEL = 1 then
     ItemColor := ConfigApp.Color02
  else if MenuData^.LEVEL = 2 then
     ItemColor := ConfigApp.Color03;

  inherited;
end;

procedure TCMFmMain.vtvMenuDblClick(Sender: TObject);
var
  MenuData: PvtvMenuRecord;
begin
  if Assigned(vtvMenu.FocusedNode) then

    if vsVisible in vtvMenu.FocusedNode^.States then
    begin
      MenuData := vtvMenu.GetNodeData(vtvMenu.FocusedNode);
      if MenuData^.IMPUTABLE then
      begin
        ExecuteMenuAction(MenuData, nil);
      end;
    end;
end;

procedure TCMFmMain.vtvMenuFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  MenuData: PvtvMenuRecord;
begin
  inherited;
  MenuData := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^)
  // instead touching every member individually.
  if Assigned(MenuData) then
  begin
    MenuData^.LONG_TEXT := '';
    MenuData^.SHORT_TEXT := '';
    MenuData^.NUMERIC_PATH := '';
    MenuData^.DESCRIPTION := '';
    MenuData^.CLASS_NAME := '';
    MenuData^.PATH := '';
  end;
end;

procedure TCMFmMain.vtvMenuGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  MenuData: PvtvMenuRecord;
begin
  MenuData := sender.GetNodeData(Node);
  case Column of
    0: HintText := MenuData^.DESCRIPTION;
    1: begin
         if MenuData^.IMPUTABLE then
         begin
           if MenuData^.CLASS_NAME = '' then
             HintText := 'Class not asigned... check here'
           else
             HintText := 'Working OK';
         end
         else
           HintText := '';
       end;
  end;

  HintText := MenuData^.LONG_TEXT;
end;

procedure TCMFmMain.vtvMenuGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  MenuData: PvtvMenuRecord;
begin
  inherited;
  case Kind of
    ikNormal, ikSelected:
    begin
      MenuData := vtvMenu.GetNodeData(Node);
      ImageIndex := -1;
      case Column of
        0: begin
            if MenuData^.IMPUTABLE then
              ImageIndex := IdxMenu
            else
            begin
              if MenuData^.ID_PARENT = 0 then
                ImageIndex := IdxRoot
              else
                ImageIndex := 0;
              end
            end;
        1: begin
             if MenuData^.IMPUTABLE then
                ImageIndex := IdxGreenLigth
               else
                 ImageIndex := -1;
           end;
      end;
    end;
  end;
end;

procedure TCMFmMain.vtvMenuGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  MenuData: PvtvMenuRecord;
begin
  inherited;
  // A handler for the OnGetText event is always needed as it provides the
  // tree with the string data to display.
  // Note that we are always using WideString.
  MenuData := Sender.GetNodeData(Node);
  if Assigned(MenuData) then
    case Column of
      0: CellText := MenuData^.LONG_TEXT;
      1: Celltext := '';// BoolToStr(MenuData^.IMPUTABLE);
    end
end;

procedure TCMFmMain.vtvMenuInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  MenuData: PvtvMenuRecord;
begin
  inherited;
  MenuData := Sender.GetNodeData(Node);
  FillMenuData(MenuData, SQLMenu, ParentNode);
end;

procedure TCMFmMain.vtvMenuKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  MenuData: PvtvMenuRecord;
begin
  if Key = 27 {VK_Escape} then
  begin
    edMenu.Clear;
    Key := 0;
    FocusControl(edMenu);
  end
  else if (key = 13 {VK_RETURN} ) then
  begin
    if assigned(vtvMenu.FocusedNode) then
    begin
      MenuData := vtvMenu.GetNodeData(vtvMenu.FocusedNode);
      ExecuteMenuAction(MenuData, nil);
    end;
    key := 0;
  end;
end;

procedure TCMFmMain.vtvMenuPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if vtvMenu.FocusedNode = Node then
    TargetCanvas.Font.Color := clWhite
  else
    TargetCanvas.Font.Color := clWindowText;
end;

procedure TCMFmMain.ShowInTab(pMenuRecord: PvtvMenuRecord; pFmToShow: TCMForm;
  pFormInitParam: TCMFormInitParam);
var
  TabSheet: TTabSheetCM;
  auxTabTitle: string;
  //IdAutorizedUser: integer;
  //KeepUserModal: boolean;
  TClassToProcess: TCMFormClass;
begin
  try { At "finally", release if pFormInitParam exist }
    TClassToProcess := SelectClassToProcess(pMenuRecord^.ID_MENU, MenuList);
    if not Assigned(TClassToProcess) then
    begin
      showmessage('Class not confugured for ' + pMenuRecord^.SHORT_TEXT);
      if assigned(pFmToShow) then
        pFmToShow.Free;
      exit;
    end;

    { Search the TabSheet, if it exist, then focused it }
    if FocusPageIfItExists(pMenuRecord, pFormInitParam) then
    begin
      if assigned(pFmToShow) then
        pFmToShow.Free;
      exit;
    end;

    if pFmToShow = nil then
    begin
      pFmToShow := TClassToProcess.Create(nil);
    end;

    try
      pFmToShow.IdMenu := pMenuRecord^.ID_MENU;
      //Continuar := False;
      (*IdUsuarioAutorizado := miModulo.IdUsuarioActual;
      if SeguridadVerificada(pFmAMostrar.Operacion, pFmAMostrar.ContadorIntentos, IdAutorizedUser) then
      begin
        if UsuarioActivo <> IdAutorizedUser then
        begin
          pFmAMostrar.MostrarModal;
          pFmAMostrar.Free;
          miModulo.IdUsuarioActual := UsuarioActivo;
          exit;
        end;
        { Aquí sería el else y el UsuarioActivo = IdAutorizedUser y continua en (#) }
      end
      { Si la operación no está autorizada, libero el formulario creado y salgo... }
      else
      begin
        pFmAMostrar.Free;
        exit;
      end;*)

      TabIndex := TabIndex + 1;
      TabSheet := TTabSheetCM.Create(pgMain);

      if pMenuRecord^.SHORT_TEXT = '' then
        auxTabTitle := pFmToShow.Caption
      else
        auxTabTitle := pMenuRecord^.SHORT_TEXT;
      TabSheet.Caption := auxTabTitle;

      TabSheet.IdxInFormsList:= TabIndex;
      TabSheet.PageControl := pgMain;
      pgMain.ActivePage := TabSheet;
      TabSheet.ShowHint := True;
      pFmToShow.AssociedTab := TabSheet;

      pgMain.FormsList.Add(TabIndex, pFmToShow);

      { Set some properties for ContainedForms }
      with pFmToShow do
      begin
        TabSheet.Hint := Caption;
        BorderStyle := bsNone;
        Parent := TabSheet;
        IsAContainedForm := True;
        Align := alClient;
      end;

      pFmToShow.Show;
      pgMain.Invalidate;

      { Do somethning according to pFormInitParam  }
      ConfigurePageOnFocus(pFmToShow, pFormInitParam);

    except
      if Assigned(pFormInitParam) then
        pFormInitParam.Free;
      pFmToShow.Close;
      pFmToShow.Free;
    end;

  finally
    if Assigned(pFormInitParam) then
      pFormInitParam.Free;
  end;
end;

procedure TCMFmMain.ShowAsModal(pMenuRecord: PvtvMenuRecord; pFmToShow: TCMForm;
  pFormInitParam: TCMFormInitParam);
var
  //Page: TTabSheetCM;
  //auxTabTitle: string;
  //IdAutorizedUser: integer;
  //KeepUserModal: boolean;
  TClassToProcess: TCMFormClass;
begin
  try { At "finally", release if pFormInitParam exist }
    TClassToProcess := SelectClassToProcess(pMenuRecord^.ID_MENU, MenuList);
    if not Assigned(TClassToProcess) then
    begin
      showmessage('Class not confugured for ' + pMenuRecord^.SHORT_TEXT);
      if assigned(pFmToShow) then
        pFmToShow.Free;
      exit;
    end;

    TClassToProcess.ShowModalAutoRelease(pFormInitParam, False);
  finally
    if Assigned(pFormInitParam) then
      pFormInitParam.Free;
  end;

end;

function TCMFmMain.FocusPageIfItExists(pMenuRecord: PvtvMenuRecord;
  pFormInitParam: TCMFormInitParam): boolean;
var
  i: integer;
  auxFM: TCMForm;
begin
  result := False;

  i := 1;
  while (i <= TabIndex) and (result = False) do
  begin
    auxFM := nil;

    if pgMain.FormsList.FindAndCopy(i,auxFM) then
    begin
      if auxFM.IdMenu = pMenuRecord^.ID_MENU then
      begin
        result := True;
        pgMain.ActivePage := TTabSheetCM(auxFM.AssociedTab);
        ConfigurePageOnFocus(auxFM, pFormInitParam);
        RestoreCursor;
        pgMain.Invalidate;
      end;
    end;
    inc(i);
  end;
end;

procedure TCMFmMain.ConfigurePageOnFocus(pFocusedFm: TCMForm;
  pFormInitParam: TCMFormInitParam);
begin
  { For special cases, consider extend the functionality here, acting according
  the class...}
  TCMForm(pFocusedFm).FormInitParam := pFormInitParam;
  TCMForm(pFocusedFm).InitializeAccordingFormInitParam;
end;

procedure TCMFmMain.AssignImages;
begin
  inherited AssignImages;
  btMenu.Glyph := ConfigApp.GetCMImage(TImgMenu32);
  btExit.Glyph := ConfigApp.GetCMImage(TImgCerrar32);
  btInfo.Glyph := ConfigApp.GetCMImage(TImgInformacion32);
end;

procedure TCMFmMain.EndingDestroy(Sender: TObject);
begin
  EditMenu.Free;
  pgMain.Free;
  ImgsListMenu.Free;
  MenuList.free;
  inherited EndingDestroy(Sender);
end;

procedure TCMFmMain.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);

  //btMenu.Width := 60;

  { Define the size and position of the MainForm, in case you leave de
  maximized option...}
  Width := Screen.Width - 200;
  Height := Screen.Height - 200;
  Top := 100;
  Left := 100;
  { OnRestoreApplication, set de WindowState as wsNormal in case of new
  maximize, restore, mimimize....}
  Application.OnRestore := @OnRestoreApplication;

  { Define the Color Application... }
  ConfigApp.ColorApp := MixColors(clRed,clYellow,70);
  //ConfigApp.ColorApp := MixColors(clGreen,clBlue,70);
  //ConfigApp.ColorApp := MixColors(clGreen,clYellow,70);

  case ConfigApp.ExecutionMode of
    emClient:
      begin
        lbAppMode.Caption := 'Client mode';
        lbAuxAppMode.Caption:=
          ConfigApp.CMClient.Server + ':' + ConfigApp.ConnectionConfig.Port;
      end;
    emClientServer:
      begin
        lbAppMode.Caption := 'Client-Server mode';
        lbAuxAppMode.Caption := GetEnvironmentVariable('COMPUTERNAME') +
          ':' + ConfigApp.ConnectionInternalServiceConfig.Port;
      end;
  end;

  Caption := ConfigApp.AppNameVersionRelease;
  TabIndex := 0;

  pgMain := TPageControlCM.Create(nil);
  with pgMain do
  begin
    Parent := pnPages;
    Align:= alClient;
    Options := Options + [nboDoChangeOnSetIndex];
    AddTabSheetCM(True).Caption := 'Main';
  end;

  MenuList := TSynDictionary.Create(TypeInfo(TIntegerDynArray),TypeInfo(TCMFormClassArray));
  ImgsListMenu := TCMImageListManager.create(ImgMenuTree);
  IdxRoot := ImgsListMenu.ImageIndex(TImgCarpetaVacia16);
  IdxMenu := ImgsListMenu.ImageIndex(TImgHojaEscrita16);
  IdxGreenLigth := ImgsListMenu.ImageIndex(TImgLuzVerde14);

  EditMenu := TCMEditBtCancel.create(edMenu);
  EditMenu.AutomaticEvent := True;
  EditMenu.TimeBetweenAutomaticEvents := 50;

  vtvMenu.Color := ConfigApp.Color04;
  vtvMenu.NodeDataSize := SizeOf(TvtvMenuRecord);
  vtvMenu.RootNodeCount := 0;
end;

procedure TCMFmMain.GetNewMenuRecord(var MenuData: PvtvMenuRecord; pIdMenu: integer);
var
  vSQLMenu: TSQLMenu;
begin
  New(MenuData);
  vSQLMenu := TSQLMenu.Create(Client,'IdMenu = ?',[pIdMenu]);
  try
    FillMenuData(MenuData, vSQLMenu);
  finally
    vSQLMenu.Free;
  end;
end;

procedure TCMFmMain.FillMenuData(var pMenuData: PvtvMenuRecord;
  pSQLMenu: TSQLMenu; pParentNode: PVirtualNode);
var
  vMenuDataParent: PvtvMenuRecord;
begin
  if not Assigned(pSQLMenu) then exit;

  pMenuData^.ID_MENU:= pSQLMenu.IdMenu;
  pMenuData^.ID_PARENT := pSQLMenu.IdParent;
  pMenuData^.LONG_TEXT := UTF8ToString(pSQLMenu.LongText);
  pMenuData^.SHORT_TEXT := UTF8ToString(pSQLMenu.ShortText);
  pMenuData^.IMPUTABLE := pSQLMenu.Imputable;
  pMenuData^.SHOW_FORM_MODE := pSQLMenu.ShowFormMode;
  pMenuData^.DESCRIPTION := UTF8ToString(pSQLMenu.Description);
  pMenuData^.CLASS_NAME:= UTF8ToString(pSQLMenu.ClassName);
  pMenuData^.ID_IMAGE := pSQLMenu.IdImage;

  if Assigned(pParentNode) then
  begin
    vMenuDataParent := vtvMenu.GetNodeData(pParentNode);
    pMenuData^.LEVEL := vMenuDataParent^.LEVEL + 1;
    pMenuData^.NUMERIC_PATH := vMenuDataParent^.NUMERIC_PATH + '.' + IntToString(pMenuData^.LEVEL);
    pMenuData^.PATH := vMenuDataParent^.PATH + '/' + pMenuData^.LONG_TEXT;
  end
  else
  begin
    pMenuData^.LEVEL := 0;
    pMenuData^.NUMERIC_PATH := IntToString(pMenuData^.LEVEL);
    pMenuData^.PATH := '/' + pMenuData^.LONG_TEXT;
  end;

end;

procedure TCMFmMain.FillMenuTree;
var
  //aux :TvtvMenuRecord;
  //MenuData: PvtvMenuRecord;
  //Node: PVirtualNode;
  //Criteria: String;

  vParentNode, vRootNode, vCurrentNode: PVirtualNode;

  function GetParentNode(pCurrentNode: pVirtualNode; pParentID: integer): PVirtualNode;
  var
    auxData: PvtvMenuRecord;
  begin
    auxData := vtvMenu.GetNodeData(pCurrentNode);
    if Not(Assigned(auxData)) then
      result := pCurrentNode
    else
    begin
      if (auxData^.ID_MENU = pParentID) or (auxData^.ID_PARENT = 0) then
        result := pCurrentNode
      else
        result := GetParentNode(pCurrentNode^.Parent, pParentID);
    end;
  end;

begin
  inherited;
  FillMissingMenuEntries(MenuList);

  vParentNode := nil;
  vRootNode := nil;
  vtvMenu.Clear;

  SQLMenu := TSQLMenu.CreateAndFillPrepare(Client,'(IdMenu >= 0) order by IdMenu',[]);
  try
    while SQLMenu.FillOne do
    begin
      //ShowMessage(UTF8ToString(ObjectToJSON(SQLMenu,[woHumanReadable])));
      if SQLMenu.IdParent = 0 then
      begin
        vtvMenu.RootNodeCount := vtvMenu.RootNodeCount + 1;
        vRootNode := vtvMenu.GetLast(nil);
        vCurrentNode := vRootNode;
      end
      else
      begin
        vParentNode := GetParentNode(vCurrentNode, SQLMenu.IdParent);
        vtvMenu.AddChild(vParentNode);
        vtvMenu.Expanded[vParentNode] := True;
        vCurrentNode:= vtvMenu.GetLast(nil);
      end;
    end;
  finally
    SQLMenu.Free;
  end;

  vtvMenu.Invalidate;
end;

procedure TCMFmMain.FillMissingMenuEntries(var pMenuList: TSynDictionary);
begin
  UCMMenuActions.FillMissingMenuEntries(pMenuList);
  ConfigApp.CMClient.MenuServices.RebuildNumericPathInMenus;
end;

procedure TCMFmMain.EndingShow(Sender: TObject);
begin
  inherited EndingShow(Sender);
  FillMenuTree;
  FocusControl(edMenu);
end;

procedure TCMFmMain.RefreshAccordingColorApp;
begin
  ColorizePanelButtons;
  vtvMenu.Color := ConfigApp.Color04;
  vtvMenu.Invalidate;
end;

procedure TCMFmMain.ExecuteMenuAction(pMenuRecord: PvtvMenuRecord;
  pFormInitParam: TCMFormInitParam; pForceModal: boolean);
begin
  if pForceModal then
    ShowAsModal(pMenuRecord, nil, pFormInitParam)
  else
  case pMenuRecord^.SHOW_FORM_MODE of
    sfmShowInTab : ShowInTab(pMenuRecord, nil, pFormInitParam);
    sfmShowModal : ShowAsModal(pMenuRecord, nil, pFormInitParam);
    //sfmShowCustom: ShowCustom(pMenuRecord, nil, pFormInitParam);}
  else
    showmessage('Check procedure ExecuteMenuAction...');
  end;
end;

procedure TCMFmMain.ExecuteMenuAction(pIdMenu: Integer;
  pFormInitParam: TCMFormInitParam; pForceModal: boolean);
var
  MenuData: PvtvMenuRecord;
begin
  GetNewMenuRecord(MenuData, pIdMenu);
  ExecuteMenuAction(MenuData, pFormInitParam, pForceModal);
  Dispose(MenuData);
end;

procedure TCMFmMain.OnRestoreApplication(Sender: TObject);
begin
  WindowState := wsNormal;
end;

procedure TCMFmMain.btMenuClick(Sender: TObject);
begin
  pnTree.Visible := not pnTree.Visible;
  splitterMenu.Visible := pnTree.Visible;

  if self.pnTree.Visible then
  begin
    pgMenu.ActivePage := TabMenu;
    FocusControl(edMenu);
  end;
end;

procedure TCMFmMain.btExitClick(Sender: TObject);
begin
  Close;
end;

end.

