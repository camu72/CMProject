unit UCMMormot;

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
  SynCommons, TypInfo, SysUtils,
  mORMot, mORMotHTTPClient,
  UCMCommons, UCMInterfaces;

type
  { Class for enumerated types. This class should be the parent class for
  new objects for each Enumerated type used in UCMModel }

  { Name and shortname support for enumerated types  }
  TEnumTypeRec = record
    TypeName: rawUTF8;
    TypeShortName: rawUTF8;
    TypeOriginalName: rawUTF8;
  end;
  TEnumTypeRecDynArray = array of TEnumTypeRec;

  { TCMEnumType }

  TCMEnumType = class(TSynPersistent)
  private
    FEnumTypeInfo: TYPINFO.PTypeInfo;
    FTypesCount: integer;
    InternalIndex: Integer;
    EnumData: PTypeData;
    procedure setEnumTypeInfo(AValue: TYPINFO.PTypeInfo);
  protected
    EnumArray: TEnumTypeRecDynArray;
    procedure OnRequestingValues(pValueType: Integer; pDefaultText: rawUTF8;
      var pTypeName, pShortName: rawUTF8); virtual;
  public
    NonSelectablesTypes: TSetOfByte;
    property EnumTypeInfo: TYPINFO.PTypeInfo read FEnumTypeInfo write setEnumTypeInfo;
    property TypesCount: integer read FTypesCount write FTypesCount;
    function CompleteSet: TSetOfByte;
    function CompleteSetExcludingSet(vSetToExclude: TSetOfByte): TSetOfByte;
    function EnumTypeName(pOrdEnumType: integer): rawUTF8;
    function EnumTypeShortName(pOrdEnumType: integer): rawUTF8;
    function EnumTypeOriginalName(pOrdEnumType: integer): rawUTF8;
    constructor Create; override;
    //function SetAsSQL(const pSet; const Size: integer = 0): RawUTF8;
    //function DescriptionSetSQL(pSetSQL: RawUTF8): RawUTF8;
    //function ConjSQLUnitario(vElementoEnumerado: integer): RawUTF8;
  end;


  { TSQLHttpClientCM }
  TSQLHttpClientCM = class(TSQLHttpClientWinHTTP)
    function Sum(a,b: double): double;
    function GetFile(aFileName: RawUTF8; var pSuccessfulTransfer: boolean): RawByteString;
  public
    //SQLRemote: ISQLRemote;
    TransferFileService: ITransferFileService;
  end;


implementation

{ TCMEnumType }

procedure TCMEnumType.setEnumTypeInfo(AValue: TYPINFO.PTypeInfo);
var
  i: integer;
  vEnumTypeName, vEnumShortName: rawUTF8;
begin
  begin
    NonSelectablesTypes := [];
    FEnumTypeInfo := AValue;

    if FEnumTypeInfo^.Kind = typInfo.tkEnumeration then
    begin
      EnumData := GetTypeData(EnumTypeInfo);

      TypesCount := EnumData^.MaxValue + 1;
      SetLength(EnumArray, TypesCount);

      for i := EnumData^.MinValue to EnumData^.MaxValue do
      begin
        EnumArray[i].TypeOriginalName := GetEnumName(EnumTypeInfo, i);
        OnRequestingValues(i, EnumArray[i].TypeOriginalName, vEnumTypeName, vEnumShortName);
        EnumArray[i].TypeName := vEnumTypeName;
        EnumArray[i].TypeShortName := vEnumShortName;
      end;
    end
    else
      raise exception.Create('The type must be Enumerated type');
  end;

end;

procedure TCMEnumType.OnRequestingValues(pValueType: Integer;
  pDefaultText: rawUTF8; var pTypeName, pShortName: rawUTF8);
begin
  pTypeName := pDefaultText;
  pShortName := pDefaultText;

  { leave the inherited, and redefine in hereders in follow way:

  case TmyEnumeratedType(pValueEnumType) of:
    TType1: begin
      pTypeName := 'Type name1';
      pShortName := 'Typo shortname1'; end;
    TType2: begin
      pTypeName := 'Type name2';
      pShortName := 'Typo shortname2'; end;
  end; }

end;

function TCMEnumType.CompleteSet: TSetOfByte;
var
  i: Integer;
begin
  result := [];
  for i := 0 to TypesCount-1 do
  begin
    result := result + [i];
  end;
end;

function TCMEnumType.CompleteSetExcludingSet(vSetToExclude: TSetOfByte): TSetOfByte;
var
  i: Integer;
begin
  result := [];
  for i := 0 to TypesCount-1 do
  begin
    if not (i in vSetToExclude) then
      result := result + [i];
  end;
end;

function TCMEnumType.EnumTypeName(pOrdEnumType: integer): rawUTF8;
begin
  result := EnumArray[pOrdEnumType].TypeName;
end;

function TCMEnumType.EnumTypeShortName(pOrdEnumType: integer): rawUTF8;
begin
  result := EnumArray[pOrdEnumType].TypeShortName;
end;

function TCMEnumType.EnumTypeOriginalName(pOrdEnumType: integer): rawUTF8;
begin
  result := EnumArray[pOrdEnumType].TypeOriginalName;
end;

constructor TCMEnumType.Create;
begin
  inherited Create;
  InternalIndex := -1;
end;

{ TSQLHttpClientCM }

function TSQLHttpClientCM.Sum(a, b: double): double;
var
  err: integer;
begin
  val(CallBackGetResult('sum',['a',a,'b',b]),result,err);
end;

function TSQLHttpClientCM.GetFile(aFileName: RawUTF8;
  var pSuccessfulTransfer: boolean): RawByteString;
begin
  pSuccessfulTransfer :=
    (CallBackGet('GetFile',['filename',aFileName], rawUTF8(result)) = HTTP_SUCCESS);
end;

end.

