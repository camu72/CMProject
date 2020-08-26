unit UCMSearcherForm;

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
  StdCtrls, UCMForm, VirtualTrees, UCMVTV, ucmlcl, ucmconfigapp,
  UCMCommons,  Windows, SynCommons, mORMot;

type

  { TCMSearcherForm }

  TCMSearcherForm = class(TCMForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btSelect: TBitBtn;
    btCancel: TBitBtn;
    edCriteria: TEdit;
    lbCriteria: TLabel;
    lbSearcherTitle: TLabel;
    pnInfo: TPanel;
    pnHeader: TPanel;
    pnFooter: TPanel;
    vtvSearcher: TVirtualStringTree;
    procedure btCancelClick(Sender: TObject);
    procedure btSelectClick(Sender: TObject);
    procedure edCriteriaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    SelectablesNodesCount: integer;
    procedure AssignImages; override;
    procedure EndingDestroy(Sender: TObject); override;
  protected
    IncludeInactiveItems: boolean;
    AdjustableColumns: TSetOfByte;
    SelectedID: TID;
    procedure ConfigureBtSelect(pActive: boolean);
    procedure ShowActiveNode;
    procedure InitializingCreate(Sender: TObject); override;
    procedure ProcessingCreate(Sender: TObject); override;
    function GetSelectedID(pData: PvtvSearchRecord): TID; virtual;
    procedure DefineActiveTextStyle(pData: PvtvSearchRecord;
      Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; pNode: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType); virtual;
  public
    EditCriteria: TCMEditBtCancel;
    ItemSelected, ItemSelectedPath: RawUTF8;
    SelectionExist: boolean;
  end;

var
  CMSearcherForm: TCMSearcherForm;

implementation

{$R *.lfm}

{ TCMSearcherForm }

procedure TCMSearcherForm.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TCMSearcherForm.btSelectClick(Sender: TObject);
begin
  //
end;

procedure TCMSearcherForm.edCriteriaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyPreview := True;
  if (Key = 40) then
  begin
    FocusNexNode(vtvSearcher, tvtvSearcher);
    KeyPreview := False;
    key := 0;
  end
  else if (key = 38) then
  begin
    FocusPreviousNode(vtvSearcher, tvtvSearcher);
    KeyPreview := False;
    key := 0;
  end
  else if (key = VK_RETURN ) then
  begin
    key := 0;
    if assigned(vtvSearcher.FocusedNode) then
    begin
      if btSelect.Enabled then
        btSelectClick(btSelect);
    end;
    FocusControl(edCriteria);
  end;
end;

procedure TCMSearcherForm.AssignImages;
begin
  inherited AssignImages;
  btSelect.Glyph := ConfigApp.GetCMImage(TImgAceptar16);
  btCancel.Glyph := ConfigApp.GetCMImage(TImgCancelar16);
end;

procedure TCMSearcherForm.EndingDestroy(Sender: TObject);
begin
  inherited EndingDestroy(Sender);
  EditCriteria.Free;
end;

procedure TCMSearcherForm.ConfigureBtSelect(pActive: boolean);
begin
  btSelect.Enabled := pActive;
  ShowActiveNode;
end;

procedure TCMSearcherForm.ShowActiveNode;
begin
  if Assigned(vtvSearcher.FocusedNode) and (SelectablesNodesCount > 0) then
    pnInfo.Caption := ' ' + ItemSelectedPath
  else
    pnInfo.Caption := '';
end;

procedure TCMSearcherForm.InitializingCreate(Sender: TObject);
begin
  //ImgsVTV := TCMImageListManager.create(ImgsVirtualTree);
  IncludeInactiveItems := True;
  inherited InitializingCreate(Sender);
  AdjustableColumns := [0];
  SelectedID := 0;

  SelectionExist := False;
  ItemSelectedPath := '';
  ItemSelected := '';

  //ColorNivel1 := OpcionesGenerales.ColorSistemaAclarado(75);
  //ColorNivel2 := OpcionesGenerales.ColorSistemaAclarado(80);
  //ColorNivel3 := OpcionesGenerales.ColorSistemaAclarado(85);
  //ColorNivel4 := OpcionesGenerales.ColorSistemaAclarado(90);
  //ColorNivel5 := OpcionesGenerales.ColorSistemaAclarado(95);
  //ColorTextoPadre := ColorAclarado(clWindowText, 30);
  //ColorArbol := OpcionesGenerales.ColorClaro;
  //ColorInactivo := OpcionesGenerales.ColorInactivo;

  vtvSearcher.NodeDataSize := SizeOf(TvtvSearchRecord);
  vtvSearcher.RootNodeCount := 0;
end;

procedure TCMSearcherForm.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);
  EditCriteria := TCMEditBtCancel.create(edCriteria);
  EditCriteria.AutomaticEvent := True;
  EditCriteria.TimeBetweenAutomaticEvents := 50;
  //EditCriteria.OnAllowEscapeKey := DefinirUsoTeclaEscape;
  //EditCriteria.AlCambiarTexto := AlCambiarTextoCriterio;

end;

function TCMSearcherForm.GetSelectedID(pData: PvtvSearchRecord): TID;
begin
  result := pData^.ID_LEVEL0;
end;

procedure TCMSearcherForm.DefineActiveTextStyle(pData: PvtvSearchRecord;
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; pNode: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  //if pNode = Sender.FocusedNode then
  //  TargetCanvas.Font.Color :=  OpcionesGenerales.ColorTextoSeleccion
  //else
  //  TargetCanvas.Font.Color :=  OpcionesGenerales.ColorTexto;

end;

end.

