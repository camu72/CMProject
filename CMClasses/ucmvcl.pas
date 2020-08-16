unit UCMVCL;

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
  Classes, SysUtils, ComCtrls, Controls, Graphics, Forms, StdCtrls,
  LCLType, LCLIntf, SynCommons, ExtCtrls, Dialogs, Buttons,
  UCMCommons, UCMConfigApp, SynLog;

type

  { To avoid dependency on Buttons }
  TNotifierXButtonButtonState =
  (
    nbsUp,       // button is up
    nbsDown,     // button is down
    nbsHot       // button is under mouse
  );

  { TCMNotifierXButton }
  TCMNotifierXButton = class(TCustomControl)
  private
    FState: TNotifierXButtonButtonState;
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  end;

  TShowNotificationMode = (
    snmTopLeftScreen, snmTopRightScreen, snmBottomLeftScreen, snmBottomRightScreen,
    snmTopLeftAsociatedForm, snmTopRightAsociatedForm,
    snmBottomLeftAsociatedForm, snmBottomRightAsociatedForm);

  { TCMNotificationWindow }
  TCMNotificationWindow = class(THintWindow)
    lblTitle: TLabel;
    lblText: TLabel;
    imgIcon: TPicture;
    BtnX: TCMNotifierXButton;
    procedure HideForm(Sender: TObject);
    procedure HandleResize(Sender: TObject);
    procedure HandleStablishedTime(Sender: TObject);
  private
    FShowNotificationMode: TShowNotificationMode;
    function GetShowNotificationMode: TShowNotificationMode;
  public
    AsociatedForm: TForm;
    Temporizer: TTimer;
    NewNotifierFormWidth, NewNotifierFormHeight,
    NotifierFormWidth, NotifierFormHeight, NotifierSreenSpacing,
    NotifierSpacing, NotifierButtonSize: Integer;

    property ShowNotificationMode: TShowNotificationMode
      read GetShowNotificationMode write FShowNotificationMode;

    procedure DefineMeasures;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ShowNotification(pMsg: string; pTitle: string = '';
      pMilisecondsToDisplay: integer = 0;
      pNotificationColor: TColor = clInfoBk; pFixedWidthFont: boolean = False;
      pNotificationWidth: integer = 0; pNotificationHeight: Integer = 0);
  end;

  { TTabSheetCM }

  TTabSheetCM = class(TTabSheet)
  public
    RectButton: TRect;
    FixedTab: boolean;
    IdxInFormsList: integer;
    function PointInRect(X, Y: integer; pRect: TRect): boolean;
    function PointInButtonClose(X, Y: integer): boolean;
    constructor Create(TheOwner: TComponent); override;
  end;

  { TPageControlCM }

  TPageControlCM = class(TPageControl)
    procedure DoChange; override;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    FormsList: TSynDictionary;
    function AddTabSheetCM(pFixedTab: boolean = False): TTabSheetCM;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


  { TCMEditBtCancel es una envoltura ( wrapper ) al rededor de un TEdit, que
  le agrega un botón cancelar integrado que borra su contenido. Tiene las
  siguientes propiedades:
    EventoAutomatico: si está activo, se lanza un evento al tiempo transcurrido,
    por ejemplo para realizar filtros automáticos mientras se escribe
    TiempoEventoAutomatico: es un contador en milisegundos, que indica cada
    cuanto siempo se lanzará el evento, si EventoAutomatico esta activo. }
  TOnAllowEscapeKey = procedure (Sender: TObject; pAllowEscapeKey: boolean) of object;
  TCMEditBtCancel = class(TObject)
    edCritera: TEdit;
    BtCancel: TSpeedButton;
    procedure BtCancelClick(Sender: TObject);
    procedure edCriteriaChange(Sender: TObject);
    procedure edCriteriaEnter(Sender: TObject);
    procedure edCriteriaExit(Sender: TObject);
    procedure edCriteriaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnElapsedTime(Sender: TObject);
  private
    FOnChangeText: TNotifyEvent;
    FTimeBetweenAutomaticEvents: integer;
    OnChangeOriginal: TNotifyEvent;
    OnKeyDownOriginal: TKeyEvent;
    Timer: TTimer;
    fEnabled: boolean;
    FVisible: boolean;
    FOnAllowEscapeKey: TOnAllowEscapeKey;
    procedure setEnabled(const Value: boolean);
    procedure setVisible(const Value: boolean);
    procedure setTimeBetweenAutomaticEvents(const Value: integer);
    procedure setOnChangeText(AValue: TNotifyEvent);
  public
    AutomaticEventStarted: boolean;
    pnContainer: TPanel;
    AutomaticEvent: boolean;
    property TimeBetweenAutomaticEvents: integer
      read FTimeBetweenAutomaticEvents
      write setTimeBetweenAutomaticEvents;
    constructor create(pEdit: TEdit);
    destructor Destroy; override;
    property enabled: boolean read fEnabled write setEnabled;
    property visible: boolean read FVisible write setVisible;
    property OnChangeText: TNotifyEvent read FOnChangeText write setOnChangeText;
    property OnAllowEscapeKey: TOnAllowEscapeKey
      read FOnAllowEscapeKey write FOnAllowEscapeKey;
  end;

procedure FocusControl(pControl: TWinControl);
procedure CtrlReadOnly(pControl: TWinControl; pReadOnly: boolean); overload;

implementation

uses
  UCMForm;

procedure FocusControl(pControl: TWinControl);
var
  Continue: boolean;
begin
  if Assigned(pControl) then
    Continue := True
  else
    Continue := False;

  if not Continue then exit;

  if not Assigned(pControl.Parent) then
    Continue := False
  { Si el control está en un TabSheet perteneciente a un pageControl y además
  es la página activa del pageControl, y además ambos controles están activos
  entonces enfoco }
  else if pControl.Parent is TTabSheet then
  begin
    { Si además, es la tabSheet activa del PageControl... }
    if TTabSheet(pControl.Parent).PageControl.ActivePage <> TTabSheet(pControl.Parent) then
      if TTabSheet(pControl.Parent).PageControl.Enabled and TTabSheet(pControl.Parent).Enabled then
        TTabSheet(pControl.Parent).PageControl.ActivePage := TTabSheet(pControl.Parent)
      else
        Continue := False;
  end
  else if pControl.Parent.Parent is TTabSheet then
  begin
    { Si además, es la tabSheet activa del PageControl... }
    if TTabSheet(pControl.Parent.Parent).PageControl.ActivePage <> TTabSheet(pControl.Parent.Parent) then
      if TTabSheet(pControl.Parent.Parent).PageControl.Enabled and TTabSheet(pControl.Parent.Parent).Enabled then
        TTabSheet(pControl.Parent.Parent).PageControl.ActivePage := TTabSheet(pControl.Parent.Parent)
      else
        Continue := False;
  end
  { Si el control está en un parent invisible o desactivado, no continuo... }
  else
    Continue := pControl.CanFocus;


  { Si se dan todas las condiciones, intento enfocar el control }
  if Continue then
    if pControl.Enabled and pControl.Visible then
      if pControl.Parent.Visible then
      begin
        try
          pControl.SetFocus;
          //if (pControl is TDBRadioGroup)then
          //begin
          //  { seteo como foco del panel detalle el 1º control }
          //  Control.Perform(WM_NEXTDLGCTL, 0, 0);
          //end;
        except
           ShowMessage('Couldn''t focus ' + pControl.Name + ' content on ' + pControl.Parent.Name );
        end;
      end;
end;

procedure CtrlReadOnly(pControl: TWinControl; pReadOnly: boolean);
begin
  if not Assigned(pControl) then
  begin
    showmessage('The control is null');
    abort;
  end;

  pControl.Enabled := not pReadOnly;

  if (pControl.ClassType = TComboBox) then
  begin
    with TComboBox(pControl) do
    begin
      Enabled := not pReadOnly;
      if pReadOnly then
        ParentColor := True
      else
        Color := clWindow;
    end;
  end

  else if (pControl.ClassType = TEdit) then
  begin
    with TEdit(pControl) do
    begin
      ReadOnly := pReadOnly;
      if pReadOnly then
        ParentColor := True
      else
        Color := clWindow;
    end;
  end

  else if (pControl.ClassType = TCheckBox) then
  begin
    with TCheckBox(pControl) do
    begin
      Enabled := not pReadOnly;
      if pReadOnly then
        Font.Color := clDkGray
      else
        Font.Color := clWindowText;
    end;
  end

  else if (pControl.ClassType = TBitBtn) then
  begin
    with TBitBtn(pControl) do
    begin
      Enabled := not pReadOnly;
    end;
  end

  else
  begin
    Showmessage(pControl.ClassName + ' isn''t not available in the CtrlReadOnly function');
  end;

end;

{ TCMEditBtCancel }

procedure TCMEditBtCancel.BtCancelClick(Sender: TObject);
begin
  Timer.Interval := 1;
  AutomaticEventStarted := True;
  edCritera.Clear;
  FocusControl(edCritera);
end;

procedure TCMEditBtCancel.edCriteriaChange(Sender: TObject);
begin
  { Reproduzco el evento original si estaba asignado en el IDE }
  if Assigned(OnChangeOriginal) then
    OnChangeOriginal(edCritera);

  BtCancel.Enabled := (edCritera.Text <> '') and edCritera.Enabled;
  if Assigned(OnAllowEscapeKey) and edCritera.Focused then
    OnAllowEscapeKey(edCritera, BtCancel.Enabled);

  Timer.Enabled := False;
  if AutomaticEvent and AutomaticEventStarted then
    Timer.Enabled := True;
end;

procedure TCMEditBtCancel.edCriteriaEnter(Sender: TObject);
begin
  AutomaticEventStarted := False;
  if Assigned(OnAllowEscapeKey) then
    OnAllowEscapeKey(edCritera, BtCancel.Enabled);
end;

procedure TCMEditBtCancel.edCriteriaExit(Sender: TObject);
begin
  AutomaticEventStarted := False;
  if Assigned(OnAllowEscapeKey) then
    OnAllowEscapeKey(edCritera, False);
end;

procedure TCMEditBtCancel.edCriteriaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AutomaticEventStarted := True;

  { Reproduzco el evento original si estaba asignado en el IDE }
  if Assigned(OnKeyDownOriginal) then
    OnKeyDownOriginal(edCritera, Key, Shift);

  if Key = VK_Escape then
  begin
    Key := 0;
    BtCancelClick(BtCancel);
  end
end;

procedure TCMEditBtCancel.OnElapsedTime(Sender: TObject);
begin
  Timer.Enabled := False;
  if Assigned(OnChangeText) then
    OnChangeText(Self);
  Timer.Interval := TimeBetweenAutomaticEvents;
end;

procedure TCMEditBtCancel.setEnabled(const Value: boolean);
begin
  fEnabled := Value;

  if FEnabled then
  begin
    BtCancel.Enabled := (edCritera.Text <> '');
    pnContainer.Color := clWindow;
  end
  else
  begin
    pnContainer.ParentColor := True;
    BtCancel.Enabled := False;
  end;
  CtrlReadOnly(edCritera, not FEnabled);

end;

procedure TCMEditBtCancel.setOnChangeText(AValue: TNotifyEvent);
begin
  FOnChangeText := AValue;
end;

procedure TCMEditBtCancel.setVisible(const Value: boolean);
begin
  FVisible := Value;
  pnContainer.Visible := FVisible;
end;

procedure TCMEditBtCancel.setTimeBetweenAutomaticEvents(const Value: integer);
begin
  FTimeBetweenAutomaticEvents := Value;
  if Assigned(Timer) then
    Timer.Interval := FTimeBetweenAutomaticEvents;
end;

constructor TCMEditBtCancel.create(pEdit: TEdit);
begin
  AutomaticEventStarted := False;
  OnAllowEscapeKey := nil;
  OnChangeOriginal:= pEdit.OnChange;
  OnKeyDownOriginal:= pEdit.OnKeyDown;

  FOnChangeText := nil;
  FTimeBetweenAutomaticEvents := 300;
  AutomaticEvent := True;
  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.Interval := TimeBetweenAutomaticEvents;

  pnContainer := TPanel.Create(pEdit.Owner);
  with pnContainer do
  begin
    parent := pEdit.Parent;
    Top := pEdit.Top;
    Left := pEdit.Left;
    Width := pEdit.Width;
    Height := pEdit.Height;
    Anchors := pEdit.Anchors;
    BorderStyle := bsSingle;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Color := clWindow;
  end;

  BtCancel := TSpeedButton.Create(nil);
  with BtCancel do
  begin
    Parent := pnContainer;
    Flat := True;
    Width := 19;
    Height := pnContainer.Height - 2;
    Top := 1;
    left := pnContainer.Width - Width;
    Anchors := [akRight, akTop, akBottom];
    Margin := 1;
    Glyph := ConfigApp.GetCMImage(TImgCancelarTexto16_VF);
    OnClick := @BtCancelClick;
  end;

  edCritera := pEdit;
  with edCritera do
  begin
    Parent := pnContainer;
    BorderStyle := bsNone;
    left := 2;
    top := 1;
    Height := pnContainer.Height - 4;
    Width := pnContainer.Width - BtCancel.Width - 8;
    Anchors := [akLeft, akRight, akTop];
    OnChange := @edCriteriaChange;
    OnEnter := @edCriteriaEnter;
    OnExit := @edCriteriaExit;
    OnKeyDown := @edCriteriaKeyDown;
    BtCancel.Enabled := (edCritera.Text <> '');
  end;

  Timer.OnTimer := @OnElapsedTime;
end;

destructor TCMEditBtCancel.Destroy;
begin
  BtCancel.Free;
  pnContainer.Free;
  Timer.Free;
  inherited Destroy;
end;

{ TCMNotifierXButton }

procedure TCMNotifierXButton.HandleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    FState := nbsDown;
    Self.Invalidate;
  end;
end;

procedure TCMNotifierXButton.HandleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FState := nbsUp;
  Self.Invalidate;
end;

constructor TCMNotifierXButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FState := nbsUp;

  OnMouseUp := @HandleMouseUp;
  OnMouseDown := @HandleMouseDown;
end;

procedure TCMNotifierXButton.Paint;
var
  L: Integer;
begin
  Canvas.Pen.Color := cl3DDKShadow;
  Canvas.Pen.Width := 1;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(0, 0, Width, Height);

  if FState = nbsUp then
    Canvas.Brush.Color := clBtnFace
  else if FState = nbsHot then
    Canvas.Brush.Color := clRed
  else begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Color := clHighlightText;
  end;

  L := Scale96ToForm(4);
  Canvas.RoundRect(0, 0, Width, Height, L, L);

  Canvas.Pen.EndCap:=pecSquare;
  Canvas.Pen.Width := 2;

  L := Scale96ToForm(6);
  Canvas.MoveTo(L, L);
  Canvas.LineTo(Width - L, Height - L);

  Canvas.MoveTo(Width - L, L);
  Canvas.LineTo(L, Height - L);

  //if Assigned(FOnPaint) then FOnPaint(Self);
  inherited Paint;
end;


{ TCMNotificationWindow }

procedure TCMNotificationWindow.Paint;
var
  R: TRect;
begin
  DefineMeasures;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,width,height));

  { Paints the icon. We can't use a TImage because it's on ExtCtrls }
  if Assigned(imgIcon.Bitmap) then
    Canvas.Draw(NotifierSpacing, NotifierSpacing, imgIcon.Bitmap);
end;

procedure TCMNotificationWindow.ShowNotification(pMsg: string; pTitle: string;
  pMilisecondsToDisplay: integer; pNotificationColor: TColor;
  pFixedWidthFont: boolean; pNotificationWidth: integer;
  pNotificationHeight: Integer);
var
  Point: TPoint;
  MilisecondsToDisplay, OffsetV, OffsetH,
  HeightTitleBar, VBorder, HBorder: integer;
begin
  if TCMForm(AsociatedForm).IsAContainedForm then
  begin
    Point.X := AsociatedForm.Left;
    Point.Y := AsociatedForm.Top;
    Point := AsociatedForm.ClientToScreen(Point);
  end
  else
  begin
    Point.X := 0;
    Point.Y := 0;
  end;

  OffsetV := 8;
  OffsetH := 8;

  HeightTitleBar := GetSystemMetrics(SM_CYCAPTION);

  if pMilisecondsToDisplay = 0 then
    MilisecondsToDisplay := 4000
  else
    MilisecondsToDisplay := pMilisecondsToDisplay;

  NewNotifierFormWidth := pNotificationWidth;
  NewNotifierFormHeight:= pNotificationHeight;

  DefineMeasures;

  Color := pNotificationColor;
  if pTitle = '' then
    lblTitle.Caption := 'Attention!'
  else
    lblTitle.Caption := pTitle;
  lblText.Caption := pMsg;

  if pFixedWidthFont then
    lblText.Font.Name := 'Courier New'
  else
    lblText.Font.Name := 'Default';

  case ShowNotificationMode of
    snmTopLeftAsociatedForm:
      begin
        Top := AsociatedForm.Top + OffsetV + Point.Y;
        Left := AsociatedForm.Left + 8 + OffsetH + Point.X;
      end;

    snmTopRightAsociatedForm:
      begin
        Top := AsociatedForm.Top + OffsetV + Point.Y;
        Left := AsociatedForm.Left + AsociatedForm.Width - ( Width + OffsetH) + Point.X;
      end;

    snmBottomLeftAsociatedForm:
    begin
      Top := (AsociatedForm.Top + AsociatedForm.Height + HeightTitleBar) - ( Height + OffsetV) + Point.Y;
      Left := AsociatedForm.Left + 8 + OffsetH + Point.X;
    end;

    snmBottomRightAsociatedForm:
    begin
      Top := (AsociatedForm.Top + AsociatedForm.Height  + HeightTitleBar) - ( Height + OffsetV) + Point.Y;
      Left := AsociatedForm.Left + AsociatedForm.Width - ( Width + OffsetH) + Point.X;
    end;

    snmTopLeftScreen:
      begin
        Top := OffsetV;
        Left := OffsetH;
      end;

    snmTopRightScreen:
      begin
        Top := OffsetV;
        Left := Screen.Width - ( Width + OffsetH);
      end;

    snmBottomLeftScreen:
    begin
      Top := screen.Height - ( Height + OffsetV);
      Left := OffsetH;
    end;

    snmBottomRightScreen:
    begin
      Top := screen.Height - ( Height + OffsetV);
      Left := Screen.Width - ( Width + OffsetH);
    end;
  end;

  Temporizer.Enabled := False;
  Temporizer.Interval := MilisecondsToDisplay;
  Temporizer.Enabled := True;
  Show;
end;

procedure TCMNotificationWindow.HideForm(Sender: TObject);
Var
  NoValue :TCloseAction;
begin
  if Assigned(OnClose) then
    OnClose(Self,NoValue);
  Hide;
end;

procedure TCMNotificationWindow.HandleResize(Sender: TObject);
var
  IconAdjust: Integer;
  spc: Integer;
  btnsize: Integer;
begin
  DefineMeasures;
  spc := Scale96ToForm(NotifierSpacing);
  btnsize := Scale96ToForm(NotifierButtonSize);

  if (ImgIcon.Bitmap <> nil) then
    IconAdjust := spc + imgIcon.Bitmap.Width
  else
    IconAdjust := 0;

  if (lblTitle <> nil) then
  begin
    lblTitle.Left := IconAdjust + spc;
    lblTitle.Top := spc;
    lblTitle.Width := Width - (lblTitle.Left + spc);
    lblTitle.Height := Scale96ToForm(20);
  end;

  if (lblText <> nil) then
  begin
    //lblText.Left := IconAdjust + Scale96ToForm(20);
    lblText.Left := IconAdjust + spc;
    lblText.Top := LblTitle.Top + LblTitle.Height + spc;
    lblText.Width := Width - (lblText.Left + spc);
    lblText.Height := Height - (lblText.Top + spc);
  end;

  if (BtnX <> nil) then
  begin
    BtnX.Left := Width - (btnSize + spc);
    //BtnX.Left := Width - (btnSize + Scale96ToForm(5));
    BtnX.Top := spc;
    BtnX.Width := btnSize;
    BtnX.Height := btnSize;
  end;
end;

procedure TCMNotificationWindow.HandleStablishedTime(Sender: TObject);
begin
  btnX.OnClick(btnX);
  Temporizer.Enabled := False;
end;

function TCMNotificationWindow.GetShowNotificationMode: TShowNotificationMode;
begin
  { if exist AsociatedForm, take a default mode, else tranlate FormsModes in
    ScreenModes }
  if Assigned(AsociatedForm) then
    result := FShowNotificationMode
  else
    case FShowNotificationMode of
      snmTopLeftAsociatedForm: result := snmTopLeftScreen;
      snmTopRightAsociatedForm: result := snmTopRightScreen;
      snmBottomLeftAsociatedForm: result := snmBottomLeftScreen;
      snmBottomRightAsociatedForm: result := snmBottomRightScreen;
    else
      result := FShowNotificationMode;
    end;
end;

procedure TCMNotificationWindow.DefineMeasures;
begin
  //INT_NOTIFIER_FORM_WIDTH  = 325;
  //INT_NOTIFIER_FORM_HEIGHT = 125;
  //INT_NOTIFIER_SCREEN_SPACING = 10;
  //INT_NOTIFIER_SPACING = 5;
  //INT_NOTIFIER_BUTTON_SIZE = 20;

  if NewNotifierFormWidth > 0 then
    NotifierFormWidth := NewNotifierFormWidth
  else
    NotifierFormWidth := 325;
  if NewNotifierFormHeight > 0 then
    NotifierFormHeight:= NewNotifierFormHeight
  else
    NotifierFormHeight := 125;
  NotifierSreenSpacing := 10;
  NotifierSpacing := 8;
  NotifierButtonSize := 16;

  Width := Scale96ToForm(NotifierFormWidth);
  Height := Scale96ToForm(NotifierFormHeight);
end;

constructor TCMNotificationWindow.Create(AOwner: TComponent);
var
  spc: Integer;
begin
  inherited Create(AOwner);
  AsociatedForm := nil;
  BorderStyle := bsNone;
  ShowNotificationMode := snmTopLeftAsociatedForm;

  NewNotifierFormWidth := 0;
  NewNotifierFormHeight := 0;
  DefineMeasures;

  // Check for small screens. An extra spacing is necessary
  // in the Windows Mobile 5 emulator
  spc := Scale96ToForm(NotifierSreenSpacing);
  if Screen.Width - spc < Width then
    Width := Screen.Width - spc;

  ImgIcon := TPicture.Create;
  imgIcon.Assign(Application.Icon);

  lblTitle := TLabel.Create(Self);
  lblTitle.Parent := Self;
  lblTitle.AutoSize := False;
  lblTitle.Transparent := True;
  lblTitle.Font.Style := [FsBold];
  lblTitle.Caption := 'Caption';
  lblTitle.ParentColor := True;
  lblTitle.OnClick := @HideForm;

  lblText := TLabel.Create(Self);
  lblText.Parent := Self;
  lblText.AutoSize := False;
  lblText.Transparent := True;
  lblText.Caption := 'Text';
  lblText.WordWrap := True;
  lblText.ParentColor := True;
  lblText.OnClick := @HideForm;

  BtnX := TCMNotifierXButton.Create(Self);
  BtnX.Parent := Self;
  BtnX.Color :=  Color;
  btnX.OnClick := @HideForm;

  HandleResize(Self);

  Color := clInfoBk;

  Temporizer := TTimer.Create(nil);
  Temporizer.Enabled := False;
  Temporizer.OnTimer := @HandleStablishedTime;

  // Connects the methods to events
  OnClick := @HideForm;
  OnShow := @HandleResize;
end;

destructor TCMNotificationWindow.Destroy;
begin
  ImgIcon.Free;
  lblTitle.Free;
  lblText.Free;
  BtnX.Free;
  Temporizer.Free;
  inherited Destroy;
end;

{ TTabSheetCM }

function TTabSheetCM.PointInRect(X, Y: integer; pRect: TRect): boolean;
begin
  Result :=
    (X > pRect.Left) and (X < pRect.Right) and (Y > pRect.Top) and (Y < pRect.Bottom);
end;

function TTabSheetCM.PointInButtonClose(X, Y: integer): boolean;
begin
  Result := PointInRect(X, Y, RectButton);
end;

constructor TTabSheetCM.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FixedTab := False;
  IdxInFormsList := -1;
  Caption := 'Uno...';
end;

{ TPageControlCM }

procedure TPageControlCM.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  i, IdxInFormsList: integer;
  ClickedTab: TTabSheetCM;
  auxFM: TCMForm;// TObject;// TCMClaseBase;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    i := IndexOfPageAt(Point(X, Y));
    ClickedTab := TTabSheetCM(Self.Pages[i]);
    if not ClickedTab.FixedTab then
      if ClickedTab.PointInButtonClose(X, Y) then
      begin
        { If there is an associated form, I close it, release it and delete it }
        IdxInFormsList := ClickedTab.IdxInFormsList;

        if Assigned(FormsList) then
          if FormsList.FindAndCopy(IdxInFormsList, auxFM) then
          begin
            auxFM.Close;
            FormsList.Delete(IdxInFormsList);
          end;

        self.RemovePage(i);
        Cursor := crDefault;
      end;
  end;
end;

procedure TPageControlCM.MouseMove(Shift: TShiftState; X, Y: integer);
var
  i: integer;
  ClickedTab: TTabSheetCM;
begin
  inherited MouseMove(Shift, X, Y);
  i := IndexOfPageAt(Point(X, Y));
  ClickedTab := TTabSheetCM(Self.Pages[i]);
  if not ClickedTab.FixedTab then
    if ClickedTab.PointInButtonClose(X, Y) then
      Cursor := crHandPoint
    else
      Cursor := crDefault;
end;

destructor TPageControlCM.Destroy;
var
  i: Integer;
  auxFM: TCMForm;
begin
  For i:= 0 to TabIndex do
  begin
    if FormsList.FindAndCopy(TTabSheetCM(Pages[i]).IdxInFormsList, auxFM) then
    begin
      auxFm.Close;
      //TSynLog.Add.Log(sllCustom1, 'Recorriendo al finalizar pgMain: ' + auxFM.Name);
    end;
  end;

  FormsList.Free;
  inherited Destroy;
end;

procedure TPageControlCM.DoChange;
begin
  inherited DoChange;
  Invalidate;
end;

function TPageControlCM.AddTabSheetCM(pFixedTab: boolean): TTabSheetCM;
begin
  Result := TTabSheetCM.Create(Self);
  Result.FixedTab := pFixedTab;
  Result.PageControl := Self;
end;

constructor TPageControlCM.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FormsList := TSynDictionary.Create(TypeInfo(TIntegerDynArray),TypeInfo(TCMFormObjArray));
  TabWidth := 120;
  TabHeight := 23;
  Options := Options + [nboDoChangeOnSetIndex];
end;

procedure TPageControlCM.PaintWindow(DC: HDC);
var
  miRect, btnRect, btnRectInterior: TRect;
  points: array[0..1] of TPoint;
  i, Desp, DespCruz, AnchoBoton, SepVert, SepHoriz: integer;
  miCanvas: TCanvas;
begin
  inherited PaintWindow(DC);

  for i := 0 to PageCount - 1 do
  begin
    if not TTabSheetCM(Self.Pages[i]).FixedTab then
    begin
      { Get de rect correspondig to the tab }
      miRect := TabRect(i);
      AnchoBoton := 12;
      SepVert := (miRect.Height - AnchoBoton) div 2;
      SepHoriz := 8;

      { get the displacement of the button according to the tab is active or not,
      and then I get the rectangle corresponding to the button already displaced }
      if (ActivePage = Pages[I]) then
      begin
        Desp := 0;
        DespCruz := 1;
      end
      else
      begin
        Desp := 2;
        DespCruz := 1 {1};
      end;
      btnRect := TRect.Create(miRect.Right - AnchoBoton - SepHoriz,
        miRect.Top + SepVert + Desp, miRect.Right - SepHoriz,
        miRect.Top + SepVert + AnchoBoton + Desp);
      btnRectInterior := TRect.Create(btnRect.Left+1,btnRect.top+1,btnRect.Right-1,btnRect.Bottom-1);

      { Assign the RectButton to be used in MouseDown }
      TTabSheetCM(Self.Pages[i]).RectButton := btnRect;

      { Draw the button rect ...}
      RoundRect(DC, btnRect.TopLeft.x, btnRect.TopLeft.y,
        btnRect.BottomRight.x, btnRect.BottomRight.y, 2, 2);
      Brush.Color := rgb(230,204,204);
      FillRect(DC, BtnRectInterior, HBRUSH(Brush.Reference.Handle));

      { If page is inactive, draw a focusRect }
     if ActivePage <> Pages[I] then
     begin
        Brush.Color := rgb(250,250,250);
        FillRect(DC, BtnRectInterior, HBRUSH(Brush.Reference.Handle));
        DrawFocusRect(DC, btnRect);
     end;

     { I draw the diagonals of the button, just offset to the center }
      points[0].x := btnRect.TopLeft.x + 2 + DespCruz;
      points[0].y := btnRect.TopLeft.y + 2 + DespCruz;
      points[1].x := btnRect.BottomRight.x - 2 - DespCruz;
      points[1].y := btnRect.BottomRight.y - 2 - DespCruz;
      Polyline(DC, @points, 2);

      points[0].x := btnRect.TopLeft.x + 2 + DespCruz;
      points[0].y := btnRect.BottomRight.y - 3 - DespCruz;
      points[1].x := btnRect.BottomRight.x - 2 - DespCruz;
      points[1].y := btnRect.TopLeft.y + 1 + DespCruz;
      Polyline(DC, @points, 2);
    end;
  end;

end;


end.
