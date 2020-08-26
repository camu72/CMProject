unit UCMFMLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, UCMForm, UCMLCL, UCMCommons;

type

  { TCMFMLogin }

  TCMFMLogin = class(TCMForm)
    Bevel1: TBevel;
    btAcept: TBitBtn;
    btCancel: TBitBtn;
    edUserName: TEdit;
    edPassword: TEdit;
    ImgHeader: TImage;
    lbUser: TLabel;
    lbPassword: TLabel;
    lbHelp: TLabel;
    pnHeader: TPanel;
    procedure btCancelClick(Sender: TObject);
    procedure OnColorizeImageHeader(pImage: TImage);
  public
    ImageHeader: TImageColorable;
    procedure ProcessingCreate(Sender: TObject); override;
    procedure EndingDestroy(Sender: TObject); override;
    procedure AssignImages; override;

    class function Login(const aTitle, aText: string; var aUserName, aPassWord: string;
      AllowUserNameChange: boolean{; const CSVComboValues: string}): boolean;

  end;

var
  CMFMLogin: TCMFMLogin;

implementation

uses
  UCMConfigApp;

{$R *.lfm}

{ TCMFMLogin }

procedure TCMFMLogin.btCancelClick(Sender: TObject);
begin
  close;
end;

procedure TCMFMLogin.OnColorizeImageHeader(pImage: TImage);
var
  Offset: Integer;
  auxIconImage, auxBMP: TBitmap;
begin
  auxIconImage := TBitmap.Create;
  auxBMP := TBitmap.Create;
  try
     auxBMP.Assign(pImage.Picture);

     auxIconImage := ConfigApp.GetCMImage(TImgKey32);
     Offset := (ImgHeader.Height - auxIconImage.Height) div 2;

     auxBMP.Canvas.Draw(Offset, Offset, auxIconImage);

     pImage.Picture.Assign(auxBMP);
  finally
    auxBMP.Free;
    auxIconImage.Free;
  end;
end;

procedure TCMFMLogin.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);
  ImageHeader := TImageColorable.create(ImgHeader);
  ImageHeader.OnColorizeImage := @OnColorizeImageHeader;
end;

procedure TCMFMLogin.EndingDestroy(Sender: TObject);
begin
  inherited EndingDestroy(Sender);
  ImageHeader.free;
end;

procedure TCMFMLogin.AssignImages;
begin
  inherited AssignImages;
  btAcept.Glyph := ConfigApp.GetCMImage(TImgAceptar16);
  btCancel.Glyph := ConfigApp.GetCMImage(TImgCancelar16);
end;

class function TCMFMLogin.Login(const aTitle, aText: string; var aUserName,
  aPassWord: string; AllowUserNameChange: boolean): boolean;
var
  T: string;
  i: integer;
  LoginForm: TCMFMLogin;
begin
  Application.CreateForm(TCMFMLogin, LoginForm);
  with LoginForm do
  try
    lbHelp.Caption := aText;

    edUserName.Text := aUserName;
    edUserName.Enabled := AllowUserNameChange;

    { Activate this if in Debug Mode }
    edPassword.Text := aPassWord;

    if aTitle='' then
      if Application.MainForm=nil then
        T := Application.Title else
        T := Application.MainForm.Caption else begin
      T := aTitle;
      for i := 1 to length(T) do
        if T[i]<' ' then
          T[i] := ' ';
    end;
    Caption := ' '+T;
    result := (ShowModal=mrOk);
    if result then
    begin
      aPassWord := SysUtils.trim(edPassword.Text);
      if edUserName.Enabled then
        aUserName := SysUtils.trim(edUserName.Text);
    end;
  finally
    LoginForm.Free;
  end;
end;

end.

