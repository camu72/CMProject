unit UCMInterfaces;

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
  SynCommons,
  mORMot, SynTable,
  SynDB,
  UCMFunctions,
  UCMCommons,
  SynLog;

//const
  //__TInterfaceServiceResult = 'Status: Byte; MsgStatus: rawUTF8; Count: Integer';

type

  TInterfaceStateResult = (isrOK, isrNoData, isrError, isrServicioNoDisponible, isrTrue, isrFalse);

  //TInterfaceServiceResult = packed record
  //  Status: TInterfaceStateResult;
  //  MsgStatus: RawUTF8;
  //  Count: Integer;
  //end;

  //ISQLRemote = interface(IInvokable)
  //  ['{07B5BCE8-9AEE-4065-818E-BBCBFB18077A}']
  //  function Execute(const aSQL: RawUTF8; aExpectResults, aExpanded: Boolean): RawJSON;
  //  function ExecuteSQLList(const pSQLList: TRawUTF8DynArray; pItemsCount: integer): integer;
  //  //function GetSiguienteID(pNomTabla, pNomCampo: rawUTF8): TID;
  //  function GetMaxValueInt(pTableName, pFieldName: rawUTF8; pWhereClausule: rawUTF8 = '' ): Integer;
  //  //function GetValor(const aSQL: RawUTF8; out Valor: RawUTF8): TInterfaceServiceResult;
  //end;

  TVersionConsultingMode = ( vcmAplication, mcdLauncher);
  ITransferFileService = interface(IInvokable)
    ['{36B02F20-546E-4CB3-80B1-380D9D79EC79}']
    //function ApplicationVersionAvailable(pMode: TVersionConsultingMode; pVerionApp: TVersionApp; out msg: RawUTF8): Boolean;
    function FileAvailable(pFileToTransfer: RawUTF8; out msg: RawUTF8): boolean;
    function ListFilesAvailables(out msg: rawUTF8): TFileNameDynArray;
  end;

  { TDTOSearcherReg }

  TDTOSearcherReg = class(TSynPersistent)
  private
    FLevel: integer;
    FImputable: boolean;
    FReference0: RawUTF8;
    FReference1: RawUTF8;
    FReference2: RawUTF8;
    FReference3: RawUTF8;
    FReference5: RawUTF8;
    FReference4: RawUTF8;
    FReference6: RawUTF8;
    FIdLevel0: TID;
    FIdLevel1: TID;
    FIdLevel3: TID;
    FIdLevel2: TID;
    FSelectable: boolean;
    FActive: boolean;
  public
    function TextTree: RawUTF8;
    function Path: RawUTF8;
  published
    property IdLevel0: TID read FIdLevel0 write FIdLevel0;
    property IdLevel1: TID read FIdLevel1 write FIdLevel1;
    property IdLevel2: TID read FIdLevel2 write FIdLevel2;
    property IdLevel3: TID read FIdLevel3 write FIdLevel3;
    property Reference0: RawUTF8 read FReference0 write FReference0;
    property Reference1: RawUTF8 read FReference1 write FReference1;
    property Reference2: RawUTF8 read FReference2 write FReference2;
    property Reference3: RawUTF8 read FReference3 write FReference3;
    property Reference4: RawUTF8 read FReference4 write FReference4;
    property Reference5: RawUTF8 read FReference5 write FReference5;
    property Reference6: RawUTF8 read FReference6 write FReference6;
    property Level: integer read FLevel write FLevel;
    property Imputable: boolean read FImputable write FImputable;
    property Selectable: boolean read FSelectable write FSelectable;
    property Active: boolean read FActive write FActive;
  end;
  TDTOSearcherRegs = array of TDTOSearcherReg;

implementation

{ TDTOSearcherReg }

function TDTOSearcherReg.TextTree: RawUTF8;
begin
  case Level of
    0: result := Reference0;
    1: result := Reference1;
    2: result := Reference2;
    3: result := Reference3;
    4: result := Reference4;
    5: result := Reference5;
    6: result := Reference6;
  end;
end;

function TDTOSearcherReg.Path: RawUTF8;
begin
  Result := JoinToTextList(Result, '/', Reference0);
  Result := JoinToTextList(Result, '/', Reference1);
  Result := JoinToTextList(Result, '/', Reference2);
  Result := JoinToTextList(Result, '/', Reference3);
  Result := JoinToTextList(Result, '/', Reference4);
  Result := JoinToTextList(Result, '/', Reference5);
  Result := JoinToTextList(Result, '/', Reference6);
end;

initialization

  { This is for direct using of variables, ie, use ISQLRemoto and not TypeInfo(ISQLRemoto) }
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(ISQLRemote)]);
  TInterfaceFactory.RegisterInterfaces([TypeInfo(ITransferFileService)]);

  { Comentar desde aquí para compilar Lanzador }
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServicePeriodos)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServicePersonas)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServicePrecios)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServicePlanesCuentas)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceFuncionesDeterminaciones)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceFuncionesAgricolas)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceFuncionesGenerales)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceBuscadores)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceTiposMovs)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceAdministraciones)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceMediosPagos)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceFondos)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceComprobantesV1)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceMovimientos)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceTalonarios)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceItems)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceEmpleados)]);
  //TInterfaceFactory.RegisterInterfaces([TypeInfo(IServiceOrdenesTrabajo)]);
  { Hasta aquí comentado para compilar lanzador ... }

  //TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TInterfaceServiceResult),
  //  __TInterfaceServiceResult).Options := [soReadIgnoreUnknownFields,soWriteHumanReadable];

  { Registro de TJSonSerializer para serializar objetos persistentes }
  TJSONSerializer.RegisterObjArrayForJSON(TypeInfo(TDTOSearcherRegs), TDTOSearcherReg);
end.

