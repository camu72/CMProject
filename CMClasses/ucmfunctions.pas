unit UCMFunctions;

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
  Classes, SysUtils, Graphics, Math, SynCommons;

type
  PBitmapPixel = ^TBitmapPixel;
  TBitmapPixel = record
     B, G, R: Byte;
  end;

  { row of pixels }
  PBitmapLine = ^TBitmapLine;
  TBitmapLine = array [UInt16] of TBitmapPixel;

{ Repalce this function withReemplazar esta función, con alguna en un mejor sitio, cuando esté
  implementada la autenticacion en mORMot.....}
//function GetNombreUsuario: RawUTF8;

function StringWithoutAccents(pString: RawUTF8): RawUTF8;
function JoinToTextList(pDestinationList: rawUTF8; Separator: rawUTF8; Element: rawUTF8; AtEnd: boolean = True): rawUTF8;

{ Hndling colors functions }
procedure RGBtoHSV(R, G, B: double; var H, S, V: double);
procedure HSVtoRGB(h, s, Brillo: double; var r, g, b: byte);
procedure Color2RGB(aColor: TColor; var r: byte; var g: byte; var b: byte);
procedure Color2HSV(aColor: TColor; var h: double; var s: double; var v: double);
function RGB2TColor(r, g, b: Byte): TColor;
function MixColors(Color1, Color2: TColor; PercColor1: byte): TColor;
function LigtherColor(aColor:TColor; Percentage:integer):TColor;
function Color2R(aColor: TColor): byte;
function Color2G(aColor: TColor): byte;
function Color2B(aColor: TColor): byte;
function GetColorFromBitmap(aBitmap: TBitmap; x,y: LongInt): TColor;


implementation

function StringWithoutAccents(pString: RawUTF8): RawUTF8;
begin
  pString := StringReplaceAll(pString, 'á', 'a');
  pString := StringReplaceAll(pString, 'é', 'e');
  pString := StringReplaceAll(pString, 'í', 'i');
  pString := StringReplaceAll(pString, 'ó', 'o');
  pString := StringReplaceAll(pString, 'ú', 'u');
  pString := StringReplaceAll(pString, 'Ñ', 'ñ');
  pString := StringReplaceAll(pString, 'Ü', 'u');
  pString := StringReplaceAll(pString, 'ü', 'u');
  result := pString;
end;

function JoinToTextList(pDestinationList: rawUTF8; Separator: rawUTF8;
  Element: rawUTF8; AtEnd: boolean): rawUTF8;
begin
  if pDestinationList = '' then
    result := Element
  else if Element = '' then
    result := pDestinationList
  else
    if AtEnd then
      result := pDestinationList + Separator + Element
    else { Join the elemnet at begin }
      result :=  Element + Separator + pDestinationList;

end;

procedure RGBtoHSV(R, G, B: double; var H, S, V: double);
var
  Min_, Max_, Delta: double;
begin
  H := 0.0;
  Min_ := Min(Min(R, G), B);
  Max_ := Max(Max(R, G), B);
  Delta := (Max_ - Min_);
  V := Max_;
  if (Max_ <> 0.0) then
    S := 255.0 * Delta / Max_
  else
    S := 0.0;
  if (S <> 0.0) then
  begin
    if R = Max_ then
      H := (G - B) / Delta
    else
    if G = Max_ then
      H := 2.0 + (B - R) / Delta
    else
    if B = Max_ then
      H := 4.0 + (R - G) / Delta;
  end
  else
    H := -1.0;
  H := H * 60;

  if H < 0.0 then
    H := H + 360.0;   // Hue -> 0..360
  S := S * 100 / 255; // Saturacion -> 0..100 %
  V := V * 100 / 255; // Value -> 0..100 %
end;

procedure HSVtoRGB(h, s, Brillo: double; var r, g, b: byte);
var
  nH, nS, nL, nF, nP, nQ, nT: double;
  lH: integer;
begin
  if s > 0 then
  begin
    nH := h / 60;
    nL := Brillo / 100;
    nS := s / 100;
    lH := Round(nH);
    nF := nH - lH;
    nP := nL * (1 - nS);
    nQ := nL * (1 - nS * nF);
    nT := nL * (1 - nS * (1 - nF));
    case lH of
      0:
      begin
        R := round(nL * 255);
        G := round(nT * 255);
        b := round(nP * 255);
      end;
      1:
      begin
        R := round(nQ * 255);
        G := round(nL * 255);
        b := round(nP * 255);
      end;
      2:
      begin
        R := round(nP * 255);
        G := round(nL * 255);
        //ColorConverter - 3
        b := round(nT * 255);
      end;
      3:
      begin
        R := round(nP * 255);
        G := round(nQ * 255);
        b := round(nL * 255);
      end;
      4:
      begin
        R := round(nT * 255);
        G := round(nP * 255);
        b := round(nL * 255);
      end;
      5:
      begin
        R := round(nL * 255);
        G := round(nP * 255);
        b := round(nQ * 255);
      end;
    end;
  end
  else
  begin
    R := round((Brillo * 255) / 100);
    G := R;
    b := R;
  end;

  if R < 0 then
    R := -R;
  if G < 0 then
    G := -G;
  if B < 0 then
    B := -B;
end;

procedure Color2RGB(aColor: TColor; var r: byte; var g: byte; var b: byte);
begin
  r := aColor and $FF;
  g := (aColor and $ff00) shr 8;
  b := (aColor and $ff0000) shr 16;
end;

procedure Color2HSV(aColor: TColor; var h: double; var s: double; var v: double);
var
  r, g, b: double;
begin
  r := aColor and $FF;
  g := (aColor and $ff00) shr 8;
  b := (aColor and $ff0000) shr 16;
  RGBtoHSV(r, g, b, h, s, v);
end;

function RGB2TColor(r, g, b: Byte): TColor;
begin
  Result := b shl 16 + g shl 8 + r;
end;

function MixColors(Color1, Color2: TColor; PercColor1: byte): TColor;
var
  c1, c2: longint;
  v1, v2, A: byte;
  r, g, b: integer;
begin
  A := PercColor1;
  A := Round(2.55 * A);
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  v1 := byte(c1);
  v2 := byte(c2);
  r := A * (v1 - v2) shr 8 + v2;
  v1 := byte(c1 shr 8);
  v2 := byte(c2 shr 8);
  g := A * (v1 - v2) shr 8 + v2;
  v1 := byte(c1 shr 16);
  v2 := byte(c2 shr 16);
  b := A * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;

function LigtherColor(aColor: TColor; Percentage: integer): TColor;
var
   vR,vG,vB, r,g,b: byte;
   h,s,v: double;
begin
    Color2RGB(aColor, r, g, b);

    RGBtoHSV(r,g,b,h,s,v);
    { less Saturation and more Lightness }
    s := s - round(s * (percentage /100));
    v:= v + round((100 - v)*(percentage/100));

    HSVtoRGB(h,s,v,r,g,b);
    vR := Round(r);
    vG := Round(g);
    vB := Round(b);
    result := RGB2TColor(vR, vG, vB);
end;

function Color2R(aColor: TColor): byte;
begin
  result := aColor and $FF;
end;

function Color2G(aColor: TColor): byte;
begin
  result := (aColor and $ff00) shr 8;
end;

function Color2B(aColor: TColor): byte;
begin
  result := (aColor and $ff0000) shr 16;
end;

function GetColorFromBitmap(aBitmap: TBitmap; x, y: LongInt): TColor;
var
  Pixel: PBitmapPixel;
begin
  aBitmap.BeginUpdate;
  Pixel := aBitmap.ScanLine[y];
  result := RGB2TColor(Pixel^.R, Pixel^.G, Pixel^.B);
  aBitmap.EndUpdate;
end;

end.
