/// <summary>
/// ***************************************************************************
///
/// DeepL API client library for Delphi
///
/// Copyright 2020-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// DeepL is an online text and document translation tool, also available as
/// software and APIs.
///
/// This project is a client library in Pascal for Delphi to use the main
/// translation API. Examples of use are also proposed.
///
/// To use the API of DeepL you must have a free or paid account.
///
/// ***************************************************************************
///
/// Author(s) :
///      Patrick PREMARTIN
///
/// Site :
///      https://deepl4delphi.developpeur-pascal.fr
///
/// Project site :
///      https://github.com/DeveloppeurPascal/DeepL4Delphi
///
/// ***************************************************************************
/// File last update : 04/08/2024 07:53:24
/// Signature : e4a5ea877b127d1adf966645e03f534d4e8ea246
/// ***************************************************************************
/// </summary>

unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

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
    { Déclarations privées }
    function getCommandLine(APIKeyVisible: boolean = false): string;
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses OlfSoftware.DeepL.ClientLib, System.IOUtils, u_urlOpen;

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
