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
  File last update : 2026-02-23T19:54:23.334+01:00
  Signature : ada03e0a71b99a1d83a56bdddb11e03a3a9ca91d
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
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation;

type
  TfrmMain = class(TForm)
    lblPort: TLabel;
    edtServerPort: TEdit;
    lblAPIKey: TLabel;
    edtAPIKey: TEdit;
    lblCommandLine: TLabel;
    edtCommandLine: TEdit;
    lblTestTranslateFromFrench: TLabel;
    edtTextFR: TEdit;
    zoneAppelServeur: TLayout;
    btnLaunchServer: TButton;
    zoneMiseAJourInfos: TLayout;
    btnUpdateFields: TButton;
    lblTestTranslateResult: TLabel;
    edtTextEN: TEdit;
    lblAPIURL: TLabel;
    edtURLAPI: TEdit;
    zoneTestTranslate: TLayout;
    btnTestTranslateFREN: TButton;
    VertScrollBox1: TVertScrollBox;
    AniIndicator1: TAniIndicator;
    procedure btnTestTranslateFRENClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUpdateFieldsClick(Sender: TObject);
    procedure btnLaunchServerClick(Sender: TObject);
  private
    function getCommandLine(APIKeyVisible: boolean = false): string;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  OlfSoftware.DeepL.ClientLib,
  System.IOUtils,
  u_urlOpen;

procedure TfrmMain.btnLaunchServerClick(Sender: TObject);
begin
  if edtCommandLine.Text.Trim.IsEmpty then
    btnUpdateFieldsClick(Sender);

  url_Open_In_Browser(getCommandLine(true));
end;

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
    DeepLTranslateTextASync(edtAPIKey.Text, 'FR', 'EN', edtTextFR.Text,
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

procedure TfrmMain.btnUpdateFieldsClick(Sender: TObject);
begin
  edtURLAPI.Text := 'http://localhost:' + edtServerPort.Text;
  edtCommandLine.Text := getCommandLine(false);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  edtAPIKey.Text := '';

  edtServerPort.Text := '8082';

  if assigned(btnUpdateFields.OnClick) then
    btnUpdateFields.OnClick(Sender);
end;

function TfrmMain.getCommandLine(APIKeyVisible: boolean): string;
begin
  Result := 'DeepLProxyServer';
  if not edtServerPort.Text.IsEmpty then
    Result := Result + ' -port ' + edtServerPort.Text;
  if not edtAPIKey.Text.IsEmpty then
    if APIKeyVisible then
      Result := Result + ' -apikey ' + edtAPIKey.Text
    else
      Result := Result + ' -apikey XXXXXXXXXX';
end;

end.

