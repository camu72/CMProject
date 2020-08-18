unit UCMMenuActions;

{$mode objfpc}{$H+}

interface

uses
  SynCommons, mORMot, UCMMormot,
  UCMConfigApp, UCMCommons, UCMModel, UCMForm,
  UFmTest01, UFmTest02;

function SelectClassToProcess(pIdMenu: integer; pMenuList: TSynDictionary): TCMFormClass;
procedure FillMissingMenuEntries(var pMenuList: TSynDictionary);

implementation

function SelectClassToProcess(pIdMenu: integer; pMenuList: TSynDictionary): TCMFormClass;
begin
  result := nil;
  pMenuList.FindAndCopy(pIdMenu,result);
  result := TCMFormClass(result);
end;

procedure FillMissingMenuEntries(var pMenuList: TSynDictionary);

  procedure AddOrUpdateSQLMenu(pIdMenu, pIdParent, pIdImage: Integer;
    pLongText, pShortText, pDescription: RawUTF8; pImputable: boolean;
    pShowFormMode: TShowFormMode);
  var
    vSQLMenu: TSQLMenu;
  begin

    vSQLMenu := TSQLMenu.Create(ConfigApp.CMClient,'IdMenu = ?',[pIdMenu]);
    try
      with vSQLMenu do
      begin
        IdMenu := pIdMenu;
        IdParent := pIdParent;
        LongText := pLongText;
        ShortText := pShortText;
        Imputable := pImputable;
        ShowFormMode := pShowFormMode;
        Description := pDescription;
        pIdImage := pIdImage;
      end;

      { If not exist, then Add else Update }
      if vSQLMenu.ID = 0 then
        ConfigApp.CMClient.Add(vSQLMenu, True)
      else
        ConfigApp.CMClient.Update(vSQLMenu);

    finally
      vSQLMenu.Free;
    end;
  end;

  procedure AddOrUpdateMenuCategEntry(
    pIdMenu: Integer;
    pIdParent: Integer;
    pLongText: RawUTF8;
    pIdImage: TID);
  var
    vFormClass: TCMFormClass;
  begin
    vFormClass := nil;
    pMenuList.Add(pIdMenu, vFormClass);
    AddOrUpdateSQLMenu(pIdMenu,pIdParent,pIdImage,pLongText,'','',False,sfmNotShow);
  end;

  procedure AddOrUpdateMenuEntry(
    pFormClass: TCMFormClass;
    pIdMenu: Integer;
    pIdParent: Integer;
    pLongText: RawUTF8;
    pShortText: RawUTF8;
    pShowFormMode: TShowFormMode;
    pDescription: RawUTF8;
    pIdImage: TID);
  begin
    pMenuList.Add(pIdMenu, pFormClass);
    AddOrUpdateSQLMenu(pIdMenu,pIdParent,pIdImage,pLongText,pShortText,pDescription,True,pShowFormMode);
  end;

begin
  AddOrUpdateMenuCategEntry(100,0,'Initial menu test',0);
  AddOrUpdateMenuEntry(TFmTest01, 101, 100, 'TestForm 01 in tab','Test 01', sfmShowInTab,'',0);
  AddOrUpdateMenuEntry(TFmTest01, 102, 100, 'TestForm 01 modal','Test 01', sfmShowModal,'',0);
  AddOrUpdateMenuEntry(TFmTest02, 103, 100, 'TestForm 02 in tab','Test 02', sfmShowInTab,'',0);

  AddOrUpdateMenuCategEntry(200,0,'Second menu test',0);
  AddOrUpdateMenuCategEntry(210,200,'Submenu test',0);
  AddOrUpdateMenuEntry(TFmTest02, 211,210,'TestForm 02 modal','Test 02', sfmShowModal,'',0);
end;

end.

