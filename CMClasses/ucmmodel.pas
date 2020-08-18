unit UCMModel;

{$mode objfpc}{$H+}

interface

uses
  SynCommons,
  mORMot,
  UCMCommons,
  UCMMormot;

{ Function to easyly create a Model for server and client applications }
function CreateModel: TSQLModel;

type

  //{ TSQLMenu }
  //
  //TSQLMenu = class(TSQLRecord)
  //private
  //  FDescription: RawUTF8;
  //  FIdImage: TID;
  //  FIdMenu: TID;
  //  FIdParent: TID;
  //  FImputable: boolean;
  //  FLongText: RawUTF8;
  //  FNumericPath: RawUTF8;
  //  FShortText: RawUTF8;
  //  FShowFormMode: TShowFormMode;
  //published
  //  property IdMenu: TID read FIdMenu write FIdMenu;
  //  property IdParent: TID read FIdParent write FIdParent;
  //  property LongText: RawUTF8 read FLongText write FLongText;
  //  property ShortText: RawUTF8 read FShortText write FShortText;
  //  property Imputable: boolean read FImputable write FImputable;
  //  property NumericPath: RawUTF8 read FNumericPath write FNumericPath;
  //  property ShowFormMode: TShowFormMode read FShowFormMode write FShowFormMode;
  //  property Description: RawUTF8 read FDescription write FDescription;
  //  property IdImage: TID read FIdImage write FIdImage;
  //end;

  { TSQLRoles }
  TSQLRole = class(TSQLRecord)
  private
    FAbbreviation: RawUTF8;
    FPlural: RawUTF8;
    FRole: RawUTF8;
  published
    property Role: RawUTF8 read FRole write FRole;
    property Abbreviation: RawUTF8 read FAbbreviation write FAbbreviation;
    property Plural: RawUTF8 read FPlural write FPlural;
  end;

implementation

function CreateModel: TSQLModel;
begin
  Result := TSQLModel.Create([TSQLMenu, TSQLRole]);
end;

end.

