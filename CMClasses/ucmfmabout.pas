unit UCMFMAbout;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, ExtCtrls, UCMForm, UCMCommons;

type

  { TCMFmAbout }

  TCMFmAbout = class(TCMForm)
    btClose: TBitBtn;
    Image1: TImage;
    lbApplicationName: TLabel;
    lbBy: TLabel;
    lbSynopseFrameworName1: TLabel;
    lbSynopseVersion1: TLabel;
    lbVersion: TLabel;
    lbPowered: TLabel;
    lbSynopseFrameworName: TLabel;
    lbSynopseVersion: TLabel;
    memChanges: TMemo;
    memCredits: TMemo;
    pgAbout: TPageControl;
    TabAbout: TTabSheet;
    TabChanges: TTabSheet;
    TabCredits: TTabSheet;
    procedure btCloseClick(Sender: TObject);
  public
    procedure ProcessingCreate(Sender: TObject); override;
    procedure AssignImages; override;
  end;

var
  CMFmAbout: TCMFmAbout;

implementation

uses
  UCMConfigApp, SynCommons;

{$R *.lfm}

{ TCMFmAbout }

procedure TCMFmAbout.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCMFmAbout.ProcessingCreate(Sender: TObject);
begin
  inherited ProcessingCreate(Sender);
  Caption := 'About ' + ConfigApp.AppName;

  lbApplicationName.Caption := ConfigApp.AppName;
  lbVersion.Caption := UTF8ToString(ConfigApp.RunningVersion.VersionRelease);

  lbSynopseFrameworName.Caption:= 'Synopse mORMot Framework';
  lbSynopseVersion.Caption := SYNOPSE_FRAMEWORK_FULLVERSION;

  pgAbout.ActivePage := TabAbout;
end;

procedure TCMFmAbout.AssignImages;
begin
  inherited AssignImages;
  btClose.Glyph := ConfigApp.GetCMImage(TImgCerrar16);
end;

end.

