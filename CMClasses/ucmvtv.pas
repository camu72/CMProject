unit UCMVTV;

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
  LCLIntf, LCLType, Controls,
  Classes, SysUtils, VirtualTrees, ucmfunctions, SynCommons, UCMCommons;

type
  TVTVType = (tvtvUndefinided, tvtvSearcher, tvtvMenu);

  PvtvSearchRecord = ^TvtvSearchRecord;
  TvtvSearchRecord = record
    ID_LEVEL0: integer;
    ID_LEVEL1: integer;
    ID_LEVEL2: integer;
    ID_LEVEL3: integer;
    REFERENCE0: string[100];
    REFERENCE1: string[100];
    REFERENCE2: string[100];
    REFERENCE3: string[100];
    REFERENCE4: string[100];
    REFERENCE5: string[100];
    REFERENCE6: string[100];
    LEVEL: integer;
    IMPUTABLE: boolean;
    TREE_TEXT: string[100];
    PATH: string[255];
    SELECTABLE: boolean;
    ACTIVE: boolean;
  end;

  PvtvMenuRecord = ^TvtvMenuRecord;
  TvtvMenuRecord = record
    ID_MENU: integer;
    ID_PARENT: integer;
    LONG_TEXT: string[100];
    SHORT_TEXT: string[30];
    IMPUTABLE: boolean;
    NUMERIC_PATH: string[50];
    LEVEL: integer;
    CHILD_COUNT: integer;
    SHOW_FORM_MODE: TShowFormMode;
    DESCRIPTION: string[200];
    CLASS_NAME: string[50];
    ID_IMAGE: integer;
    PATH: string[255];
  end;

  function CountVisiblesNodes(pVTV: TVirtualStringTree; pVTVType: TVTVType): integer;
  function CountVisiblesAndSelectionablesNodes(pVTV: TVirtualStringTree; pVTVType: TVTVType): integer;
  function FilterTree(pVTV: TVirtualStringTree; pVTVType: TVTVType; pPattern: RawUTF8;
    pPattern2: RawUTF8 = ''; pPattern3: RawUTF8= '';
    pIncludeInactive: boolean = True; pFullExpandedForced: boolean = False): integer;
  function PatternMatches(pPattern, pStrToCompare: RawUTF8): boolean;

  function FocusFirstNode(pVTV: TVirtualStringTree; pVTVType: TVTVType): boolean;
  procedure FocusLastNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
  procedure FocusNexNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
  procedure FocusPreviousNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
  procedure FocusNodeAccordingKeyValue(pVTV: TVirtualStringTree; pVTVType: TVTVType; pKeyValue: integer);
  function IsSelectable(pNode: pVirtualNode; pVTV: TVirtualStringTree; pVTVType: TVTVType): boolean;

  function GetParentNode(pCurrentNode: pVirtualNode; pParenID: integer;
    pVTV: TVirtualStringTree; pVTVType: TVTVType): pVirtualNode;
  function GetNodoAccordingKeyValue(pVTV: TVirtualStringTree; pVTVType:
    TVTVType; pKeyValue: integer): pVirtualNode;

  procedure AutoAdjustColumnWidth(pVTV: TVirtualStringTree; ColsToAdjust: TSetOfByte = []);
  procedure AdjustColumnWidthVTV(var VirtuaTreeView: TVirtualStringTree;
    ColsToAdjust: TSetOfByte = [];
      MinWidth: Integer = 40);

implementation

function CountVisiblesNodes(pVTV: TVirtualStringTree; pVTVType: TVTVType): integer;
var
  Node: PVirtualNode;
begin
  result := 0;
  Node := pVTV.GetFirst;
  while Node <> nil do
  begin
    if pVTV.FullyVisible[Node] then
      result := result + 1;
    Node := pVTV.GetNext(Node);
  end;
end;

function CountVisiblesAndSelectionablesNodes(pVTV: TVirtualStringTree;
  pVTVType: TVTVType): integer;
var
  Node: PVirtualNode;
begin
  result := 0;
  Node := pVTV.GetFirst;
  while Node <> nil do
  begin
    if pVTV.FullyVisible[Node] then
    begin
      if IsSelectable(Node, pVTV, pVTVType) then
        result := result + 1;
    end;
    Node := pVTV.GetNext(Node);
  end;
end;

function FilterTree(pVTV: TVirtualStringTree; pVTVType: TVTVType;
  pPattern: RawUTF8; pPattern2: RawUTF8; pPattern3: RawUTF8;
  pIncludeInactive: boolean; pFullExpandedForced: boolean): integer;
var
  vNode : PVirtualNode;
  //DatosMenu: pVTVRegMenu;
  //DatosTipoMov: pVTVRegTipoMov;
  //DatosTipoMovCM: PvtvRegTipoMovCM;
  //DatosCC: pVTVRegCC;
  //DatosPers: pVTVRegPersona;
  //DatosArt: pVTVRegArticulo;

  MenuData: PvtvMenuRecord;
  SearchData: PvtvSearchRecord;
  auxWhereToLook: RawUTF8;
  ForceHide: boolean;
  VisiblesNodesCount: integer;
begin

  VisiblesNodesCount := 0;
  pVTV.BeginUpdate;
  pVTV.FullExpand(nil);
  vNode := pVTV.GetFirst;

  { If exist some Node, go on with the filter... }
  while vNode <> nil do begin
    auxWhereToLook := '';
    ForceHide := False;

    case pVTVType of
      tvtvSearcher:
        begin
          SearchData := pVTV.getNodeData(vNode);
          if pIncludeInactive then
            ForceHide := False
          else
            ForceHide := not SearchData^.ACTIVE;
          if SearchData^.PATH = '' then
            auxWhereToLook :=
              SearchData^.REFERENCE0 + ' ' + SearchData^.REFERENCE1 + ' ' +
              SearchData^.REFERENCE2 + ' ' + SearchData^.REFERENCE3 + ' ' +
              SearchData^.REFERENCE4 + ' ' + SearchData^.REFERENCE5 + ' ' +
              SearchData^.REFERENCE6
          else
            auxWhereToLook := SearchData^.PATH;
        end;
      tvtvMenu:
        begin
          MenuData := pVTV.getNodeData(vNode);
          auxWhereToLook := MenuData^.PATH;
        end;
    end;

    if ForceHide then
      pVTV.IsVisible[vNode] := False
    { Si el patrón de filtro es vacio, muestro cada vNode en forma completa }
    else if pPattern = '' then
    begin
      if not pVTV.FullyVisible[vNode] then
        pVTV.FullyVisible[vNode] := True;
    end
    { Sino, comparo y según condición muestro u oculto.... }
    else if not PatternMatches(pPattern, auxWhereToLook) then
        pVTV.IsVisible[vNode] := False
    else
    begin
      if not pVTV.FullyVisible[vNode] then
        pVTV.FullyVisible[vNode] := True;
    end;

    if pVTV.FullyVisible[vNode] then
      VisiblesNodesCount := VisiblesNodesCount + 1;

    { Aalizo el siguiente vNode del arbol... }
    vNode := pVTV.GetNext(vNode);
  end;

  { Si el patron es vacío y no estoy buscando mov. que permiten carga por lotes,
  el ciclo anterior dejó todo el arbol visible, y sólo en ese caso lo colapso a sus nodos raices... }
  if (pPattern = '') and (pPattern2 = '') and (pPattern3 = '')
    and not pFullExpandedForced then
    pVTV.FullCollapse(nil)
  else
    pVTV.FullExpand(nil);

  { Muestro siempre el arbol en su punto superior. Es decir, desplazo el scroll
  a su posición inicial... }
  pVTV.OffsetY := 0;
  pVTV.EndUpdate;
  pVTV.invalidate;

  Result := VisiblesNodesCount;
end;

function PatternMatches(pPattern, pStrToCompare: RawUTF8): boolean;
var
  i: integer;
  PartialPattern: RawUTF8;
begin
  { Recorro el patron y para cada parte del patrón me fijo si conicide en
  la cadena a comparar. Si una sola no coincide, ya es falso... }
  PartialPattern := '';
  i := 1;
  result := True;

  { while I'm not done going through the pattern}
  while (i <= length(pPattern)) and result do
  begin
    if pPattern[i] <> ' ' then
      { take each part before a space }
      PartialPattern := PartialPattern + pPattern[i]
    else
    begin
      { For each part found, I compare with the string and reset it }
      if PartialPattern <> '' then
        result := Pos(StringWithoutAccents(LowerCaseU(PartialPattern)), StringWithoutAccents(LowerCaseU(pStrToCompare)))>0;
      PartialPattern := ''
    end;
    i := i + 1;
  end;

  { Here the last part found is analyzed, since it does not depend on a space
   at the end, but it is the last armed part (or the only one if there is only one) }
  if (PartialPattern <> '') and result then
    result := Pos(StringWithoutAccents(LowerCaseU(PartialPattern)), StringWithoutAccents(LowerCaseU(pStrToCompare)))>0;
end;

function FocusFirstNode(pVTV: TVirtualStringTree; pVTVType: TVTVType): boolean;
var
  vNode: PVirtualNode;
begin
  pVTV.BeginUpdate;
  vNode := pVTV.GetFirst;

  while not IsSelectable(vNode, pVTV, pVTVType) do
    vNode := pVTV.GetNext(vNode);

  if Assigned(vNode) then
  begin
    pVTV.Selected[vNode]:= True;
    pVTV.FocusedNode := vNode;
    pVTV.Invalidate;
    result := True;
  end
  else
    result := False;

  pVTV.EndUpdate;
end;

procedure FocusLastNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
var
  vNode: PVirtualNode;
begin
  vNode := pVTV.GetLast;

  while not IsSelectable(vNode, pVTV, pVTVType) do
    vNode := pVTV.GetPrevious(vNode);

  pVTV.Selected[vNode]:=True;
  pVTV.FocusedNode := vNode;
  pVTV.Invalidate;
end;

procedure FocusNexNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
var
  vNode: PVirtualNode;
begin
  if not Assigned(pVTV.FocusedNode) then
    FocusFirstNode(pVTV, pVTVtype)
  else
  begin
    vNode := pVTV.GetNext(pVTV.FocusedNode);
    while not IsSelectable(vNode, pVTV, pVTVtype) do
      vNode := pVTV.GetNext(vNode);

    if assigned(vNode) then
    begin
      pVTV.Selected[vNode]:= True;
      pVTV.FocusedNode := vNode;
    end
    else
      FocusFirstNode(pVTV, pVTVtype);
  end;
  pVTV.Invalidate;
end;

procedure FocusPreviousNode(pVTV: TVirtualStringTree; pVTVType: TVTVType);
var
  vNode: PVirtualNode;
begin
  { If doesn't exist a focused node, then focus last }
  if not Assigned(pVTV.FocusedNode) then
    FocusLastNode(pVTV, pvTVtype)
  else
  begin
    { From a focused node, back one position }
    vNode := pVTV.GetPrevious(pVTV.FocusedNode);

    { if do not meet the selectable condition, then go on until found the next,
    or to the begining of the tree }
    while not IsSelectable(vNode, pVTV, pvTVtype) do
      vNode := pVTV.GetPrevious(vNode);

    if assigned(vNode) then
    begin
      pVTV.Selected[vNode]:=True;
      pVTV.FocusedNode := vNode;
    end
    else
      FocusLastNode(pVTV, pvTVtype);
  end;
  pVTV.Invalidate;
end;

procedure FocusNodeAccordingKeyValue(pVTV: TVirtualStringTree;
  pVTVType: TVTVType; pKeyValue: integer);
var
  vNode, vAuxNode: PVirtualNode;
  //DatosMenu: pVTVRegMenu;
  //DatosTipoMov: pVTVRegTipoMov;
  //DatosTipoMovCM: pVTVRegTipoMovCM;
  //DatosCC: pVTVRegCC;
  //DatosPers: pVTVRegPersona;
  //DatosArt: pVTVRegArticulo;
  //DatosOTA: pVTVRegOTA;
  MenuData: PvtvMenuRecord;
  SearchData: PvtvSearchRecord;
  FoundNode: boolean;
  IdtoCompare: integer;
begin
  pVTV.BeginUpdate;
  pVTV.FullExpand(nil);
  vNode := pVTV.GetFirst;
  FoundNode := False;
  { If exist some node, go on... }
  while (vNode <> nil) do
  begin
    { Make vidible all nodes. For this, the cycle must be for all the tree,
    and focus on the node I am looking for when appropriate, but continue
    the cycle despite finding it to visualize the totality of nodes }
    pVTV.IsVisible[vNode] := True;

    IdToCompare := 0;
    case pVTVType of
      tvtvSearcher:
        begin
          SearchData := pVTV.getNodeData(vNode);
          IdtoCompare := SearchData^.ID_LEVEL0;
        end;
      tvtvMenu:
        begin
          MenuData := pVTV.getNodeData(vNode);
          IdtoCompare := MenuData^.ID_MENU;
        end;
    end;

    if (pKeyValue = IdtoCompare) then
    begin
      vAuxNode := vNode;
      FoundNode := True;
    end;

    vNode := pVTV.GetNext(vNode);
  end;

  { If the KeyValue is found it, show all the tree, with the node in focus,
  else show the tree colapsed in its roots nodes...}
  if FoundNode then
    pVTV.FullExpand(nil)
  else
    pVTV.FullCollapse(nil);

  { Show de tree in its top point. I mean, offset the scrool in its initial
  position }
  //pVTV.OffsetY := 0;
  pVTV.EndUpdate;

  if FoundNode then
  begin
    pVTV.Selected[vAuxNode]:= True;
    pVTV.FocusedNode := vAuxNode;
  end;

  //pVTV.Invalidate;
end;

function IsSelectable(pNode: pVirtualNode; pVTV: TVirtualStringTree;
  pVTVType: TVTVType): boolean;
var
  MenuData: PvtvMenuRecord;
  SearchData: PvtvSearchRecord;
begin
  if assigned(pNode) then
  begin
    case pVTVType of
        tvtvSearcher:
        begin
          SearchData := pVTV.getNodeData(pNode);
          result := SearchData^.IMPUTABLE and pVTV.FullyVisible[pNode] and SearchData^.SELECTABLE;
        end;
        tvtvMenu:
        begin
          MenuData := pVTV.getNodeData(pNode);
          result := MenuData^.IMPUTABLE and pVTV.FullyVisible[pNode] {and MenuData^.SELECTABLE};
        end;
      else
        result := True;
    end;
  end
  else
    result := True;
end;

function GetParentNode(pCurrentNode: pVirtualNode; pParenID: integer;
  pVTV: TVirtualStringTree; pVTVType: TVTVType): pVirtualNode;
var
  MenuData: PvtvMenuRecord;
begin
  case pVTVType of
    tvtvMenu:
      begin
        MenuData := pVTV.getNodeData(pCurrentNode);
        if Not(Assigned(MenuData)) then
          result := pCurrentNode
        else
        begin
          if (MenuData^.ID_MENU = pParenID) or (MenuData^.ID_PARENT = 0) then
            result := pCurrentNode
          else
            result := GetParentNode(pCurrentNode^.Parent, pParenID, pVTV, pVTVType);
        end;
      end;
    tvtvSearcher:
      begin
        result := pCurrentNode;
      end;
  end;
end;

function GetNodoAccordingKeyValue(pVTV: TVirtualStringTree; pVTVType: TVTVType;
  pKeyValue: integer): pVirtualNode;
var
  vNode, vNodeAux, vCurrentNode: PVirtualNode;
  MenuData: PvtvMenuRecord;
  SearchData: PvtvSearchRecord;
  FoundNode: boolean;
  IdtoCompare: integer;
begin
  pVTV.BeginUpdate;
  vCurrentNode := pvtv.FocusedNode;
  pVTV.FullExpand(nil);
  vNode := pVTV.GetFirst;
  FoundNode := False;
  { If exist some node, then go on... }
  while (vNode <> nil) and (not FoundNode) do
  begin
    case pVTVType of
      tvtvSearcher  :
        begin
          SearchData := pVTV.GetNodeData(vNode);
          IdtoCompare := SearchData^.ID_LEVEL0;
        end;
      tvtvMenu  :
        begin
          MenuData := pVTV.GetNodeData(vNode);
          IdtoCompare := MenuData^.ID_MENU;
        end;
    end;

    if (pKeyValue = IdtoCompare) then
    begin
      vNodeAux := vNode;
      FoundNode := True;
    end;

    vNode := pVTV.GetNext(vNode);
  end;

  pVTV.FocusedNode := vCurrentNode;
  pVTV.EndUpdate;

  if FoundNode then
    result := vNodeAux
  else
    result := nil;

  //FocusNodeAccordingKeyValue(pVTV, pVTVType, pKeyValue);
end;

procedure AutoAdjustColumnWidth(pVTV: TVirtualStringTree; ColsToAdjust: TSetOfByte);
var
  TotalColumnWidth, GridClientWidth, Filler, i: Integer;
begin
  { compute total width used by grid columns and vertical lines if any }
  TotalColumnWidth := 0;
  for i := 0 to pVTV.Header.Columns.Count - 1 do
    TotalColumnWidth := TotalColumnWidth + pVTV.Header.Columns[i].Width;

  { compute grid client width by excluding vertical scroll bar, grid indicator,
  and grid border }
  GridClientWidth := pVTV.Width;

  if pVTV.BorderStyle = bsSingle then
  begin
    //if pVTV.Ctl3D then
    //  { border is sunken (vertical border is 2 pixels wide) }
    //  GridClientWidth := GridClientWidth - 4
    //else { border is one-dimensional (vertical border is one pixel wide) }
    //  GridClientWidth := GridClientWidth - 2;
    GridClientWidth := GridClientWidth - 2;
  end;

  { adjust column widths }
  Filler := (GridClientWidth - TotalColumnWidth);
  pVTV.Header.Columns[0].Width := pVTV.Header.Columns[0].Width + Filler;
end;

procedure AdjustColumnWidthVTV(var VirtuaTreeView: TVirtualStringTree;
  ColsToAdjust: TSetOfByte; MinWidth: Integer);
var
  ColumnCount, VariableColumnCont, VisibleColumnCount, TotalColumnWidth,
  TreeClientWidth, Filler, i: Integer;
begin
  ColumnCount := VirtuaTreeView.Header.Columns.Count;
  VisibleColumnCount := 0;
  VariableColumnCont := 0;
  TotalColumnWidth := 0;

  For i := 0 to ColumnCount-1 do
    if coVisible in VirtuaTreeView.Header.Columns[i].Options then
    begin
      VisibleColumnCount := VisibleColumnCount + 1;
      if VirtuaTreeView.Header.Columns[i].Index in ColsToAdjust then
        VariableColumnCont := VariableColumnCont + 1;

      TotalColumnWidth := TotalColumnWidth + VirtuaTreeView.Header.Columns[i].Width;
    end;

  if VariableColumnCont = 0 then
    Exit;

  { Calculo el ancho del cliente de la grilla, excluyendo el scroll vertical,
  la comumna indicadora y el borde de la grilla }
  TreeClientWidth := VirtuaTreeView.Width - GetSystemMetrics(SM_CXVSCROLL);

  if VirtuaTreeView.BorderStyle = bsSingle then
  begin
    //if VirtuaTreeView.Ctl3D then
    //  { border is sunken (vertical border is 2 pixels wide) }
    //  TreeClientWidth := TreeClientWidth - 4
    //else { border is one-dimensional (vertical border is one pixel wide) }
    TreeClientWidth := TreeClientWidth - 2;
  end;

  { Adjust withh of columns }
  if TotalColumnWidth < TreeClientWidth then
  begin
    Filler := (TreeClientWidth - TotalColumnWidth) div VariableColumnCont;
    for i := 0 to ColumnCount-1 do
      if coVisible in VirtuaTreeView.Header.Columns[i].Options then
        if VirtuaTreeView.Header.Columns[i].Index in ColsToAdjust then
          VirtuaTreeView.Header.Columns[i].Width := VirtuaTreeView.Header.Columns[i].Width + Filler;
  end
  else if TotalColumnWidth > TreeClientWidth then
  begin
    Filler := (TotalColumnWidth - TreeClientWidth) div VariableColumnCont;
    if (TotalColumnWidth - TreeClientWidth) mod VariableColumnCont <> 0 then
      Inc(Filler);
    for i := 0 to ColumnCount-1 do
      if coVisible in VirtuaTreeView.Header.Columns[i].Options then
      if VirtuaTreeView.Header.Columns[i].Index in ColsToAdjust then
      begin
        { Si el ancho quedara mas chico que AnchoMin, se lo deja en AnchoMin.
        Esto quizá deje visible la barra de desplazamiento }
        if (VirtuaTreeView.Header.Columns[i].Width - Filler) < Minwidth then
          //VirtuaTreeView.Header.Columns[i].Width := AnchoMin
        else
          VirtuaTreeView.Header.Columns[i].Width := VirtuaTreeView.Header.Columns[i].Width - Filler;
      end
  end;
end;

end.

