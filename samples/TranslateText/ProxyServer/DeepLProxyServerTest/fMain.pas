(* C2PP
  ***************************************************************************

  DeepL API client library for Delphi

  Copyright 2020-2025 Patrick Prémartin under AGPL 3.0 license.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

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
  File last update : 2025-02-09T11:03:31.765+01:00
  Signature : c0683c2b8414c77dffad4ac85a8b05547ffe5075
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

