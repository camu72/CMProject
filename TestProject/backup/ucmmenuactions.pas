unit UCMMenuActions;

{$mode objfpc}{$H+}

interface

uses
  SynCommons, mORMot,
  UCMConfigApp, UCMCommons, UCMModel, UCMForm,
  UFmTest01, UFmTest02;

const
  { Menu Configuración general }
  IdMenuTest01InTab                              = 101;
  IdMenuTest01Modal                              = 102;
  IdMenuTest02InTab                              = 103;

function SelectClassToProcess(pIdMenu: integer): TCMFormClass;
procedure FillMissingMenuEntries(var pMenuList: TSynDictionary);

implementation

function SelectClassToProcess(pIdMenu: integer): TCMFormClass;
begin
  result := nil;
  Case pIdMenu of
    IdMenuTest01InTab                   : result := TFmTest01;
    IdMenuTest01Modal                   : result := TFmTest01;
    IdMenuTest02InTab                   : result := TFmTest02;
  end;
end;

procedure FillMissingMenuEntries(var pMenuList: TSynDictionary);
var
  SQLMenu: TSQLMenu;

  procedure AddOrUpdateMenuEntry(
    pFormClass: TCMFormClass;
    pIdMenu: Integer;
    pIdParent: Integer;
    pLongText: RawUTF8;
    pShortText: RawUTF8;
    pImputable: boolean;
    //pNumericPath: RawUTF8;
    //TextPath: RawUTF8;
    //Level: Integer;
    //ChildCount: Integer;
    pShowFormMode: TShowFormMode;
    pDescription: RawUTF8;
    pIdImage: TID);
  var
    vSQLMenu: TSQLMenu;
  begin
    pMenuList.Add(pIdMenu, pFormClass);


    vSQLMenu := TSQLMenu.Create(ConfigApp.CMClient,'IdMenu = ?',[pIdMenu]);
    try

      if vSQLMenu.ID = 0 then
      begin
        with vSQLMenu do
        begin
          IdMenu := pIdMenu;
          IdParent := pIdParent;
          LongText := pLongText;
          ShortText := pShortText;
          Imputable := pImputable;
          ShowFormMode := pShowFormMode;
          Description := pDescription;
          pIdImage := IdImage;

          NumericPath := '';
          TextPath := '';
          ChildCount := 0;
        end;
        ConfigApp.CMClient.Add(vSQLMenu, True);
      end;
    finally

      vSQLMenu.Free;
    end;
  end;

begin
  SQLMenu := TSQLMenu.Create;

  try
    AddOrUpdateMenuEntry(nil, 100,0,'Test menu','Test', False, sfmNotShow,'',0);
    AddOrUpdateMenuEntry(TFmTest01, IdMenuTest01InTab,0,'TestForm 01','Test 01', True, sfmShowInTab,'',0);
    AddOrUpdateMenuEntry(TFmTest01, IdMenuTest01Modal,100,'TestForm 01','Test 01', True, sfmShowModal,'',0);
    AddOrUpdateMenuEntry(TFmTest02, IdMenuTest02InTab,100,'TestForm 02','Test 02', True, sfmShowInTab,'',0);
  finally
    SQLMenu.Free;
  end;
end;

end.

