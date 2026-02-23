(* C2PP
  ***************************************************************************

  DeepL API client library for Delphi
  Copyright (c) 2020-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  DeepL is an online text and document translation tool, also available as
  software and APIs.

  This project is a client library in Pascal for Delphi to use the main
  translation API. Examples of use are also proposed.

  To use the API of DeepL you must have a free or paid account.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://deepl4delphi.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/DeepL4Delphi

  ***************************************************************************
  File last update : 2026-02-23T19:54:23.330+01:00
  Signature : 09841598d48e341bcdb03f213ac99643dc18e26b
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts;

type
  TfrmMain = class(TForm)
    lblAPIURL: TLabel;
    edtURLAPI: TEdit;
    lblTestTranslateFromFrench: TLabel;
    edtTextFR: TEdit;
    zoneTestTranslate: TLayout;
    btnTestTranslateFREN: TButton;
    AniIndicator1: TAniIndicator;
    lblTestTranslateResult: TLabel;
    edtTextEN: TEdit;
    procedure btnTestTranslateFRENClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  OlfSoftware.DeepL.ClientLib;

procedure TfrmMain.btnTestTranslateFRENClick(Sender: TObject);
begin
  if edtTextFR.Text.IsEmpty then
  begin
    edtTextFR.SetFocus;
    raise exception.Create('Source text needed.');
  end;

  if edtURLAPI.Text.IsEmpty then
    DeepLSetAPIURL(CDeepLAPIURL_Free)
  else
    DeepLSetAPIURL(edtURLAPI.Text);

  btnTestTranslateFREN.Visible := false;
  AniIndicator1.Visible := true;
  AniIndicator1.Enabled := true;
  try
    DeepLTranslateTextASync('', 'FR', 'EN', edtTextFR.Text,
      procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
      begin
        edtTextEN.Text := TranslatedText;
        AniIndicator1.Enabled := false;
        AniIndicator1.Visible := false;
        btnTestTranslateFREN.Visible := true;
      end,
      procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
      begin
        AniIndicator1.Enabled := false;
        AniIndicator1.Visible := false;
        btnTestTranslateFREN.Visible := true;
        showmessage('Error during API server call.');
      end);
  except
    AniIndicator1.Enabled := false;
    AniIndicator1.Visible := false;
    btnTestTranslateFREN.Visible := true;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  edtURLAPI.Text := 'http://localhost:8080';
  edtTextFR.Text :=
  'Test du serveur proxy de traduction français vers anglais (ou autres langues).';
end;

end.

