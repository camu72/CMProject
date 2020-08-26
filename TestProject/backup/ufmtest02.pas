unit UFmTest02;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  UCMForm, Buttons, LCLTaskDialog, ucmlcl;

type

  { TFmTest02 }

  TFmTest02 = class(TCMForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Shape1: TShape;
    procedure BitBtn1Click(Sender: TObject);
  private
    procedure ProcessingCreate(Sender: TObject); override;
  public
    ImgColor: TImageColorable;
    procedure EndingDestroy(Sender: TObject); override;
  end;

var
  FmTest02: TFmTest02;

implementation

{$R *.lfm}

{ TFmTest02 }

procedure TFmTest02.BitBtn1Click(Sender: TObject);
var
  Task: TTaskDialog;
  Continuar: boolean;
  //modo: TModoTipoArticulo;
  //Periodo: TPeriodo;
begin
  inherited;
  Task.Inst := 'Atention!';
  Task.Content :='Chose an option';
  Task.Buttons := 'Not this...\nThis is not a good idea'#10 +
    'Maybe this\nI don''t know what appen here'#10 +
    'Cancelar\nJust exit';
  Task.Verify := 'Do no ask for this setting next time';
  Task.VerifyChecked := true;
  Task.Footer := 'Select Cancelar if you don''t understand anything...';
  case Task.Execute([],0,[tdfUseCommandLinks],tiInformation,tfiShield) of
    100: ShowMessage('Ok... you have a virus in your PC.. :)');
    101: ShowMessage('Your PC is clean now');
    102: ShowMessage('Just exit');
  end;

  Image1.Width := 300;
  ColorizeTImage(Image1);
end;

procedure TFmTest02.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);
  ImgColor := TImageColorable.Create(Image1);

  //ColorizeTImage(Image1);
end;

procedure TFmTest02.EndingDestroy(Sender: TObject);
begin
  inherited EndingDestroy(Sender);
  if Assigned(ImgColor) then
    ImgColor.Free;
end;

end.

