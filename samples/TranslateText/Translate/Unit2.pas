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
  File last update : 2025-02-09T11:03:31.775+01:00
  Signature : 077c8f848cfd8604b2f2403b71a0ee08a53e4e5d
  ***************************************************************************
*)

unit Unit2;

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
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAPIKeyFileName: string;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  OlfSoftware.DeepL.ClientLib,
  System.IOUtils,
  System.Threading;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Button1.EnableD := false;
  try
    Memo1.Lines.Add('Translation of "' + Edit1.Text + '" submitted.');
    DeepLTranslateTextASync(tfile.ReadAllText(FAPIKeyFileName), 'FR', 'EN',
      Edit1.Text,
      procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
      begin
        Memo1.Lines.Add('Translating "' + OriginalText + '"');
        Memo1.Lines.Add('=> "' + TranslatedText + '"');
        Button1.EnableD := true;
      end,
      procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
      begin
        Memo1.Lines.Add('Translating "' + OriginalText + '"');
        Memo1.Lines.Add('=> "' + ErrorText + '"');
        Button1.EnableD := true;
      end);
    Edit1.SetFocus;
  except
    Button1.EnableD := true;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FAPIKeyFileName := tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.dat');
  if not tfile.Exists(FAPIKeyFileName) then
    FAPIKeyFileName := tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.txt');
  if not tfile.Exists(FAPIKeyFileName) then
    raise exception.Create('File ' + FAPIKeyFileName +
      ' doesn''t exists. Please create it and put there your DeepL API key.');
end;

end.

