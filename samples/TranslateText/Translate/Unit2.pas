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
  File last update : 2026-02-24T17:33:30.000+01:00
  Signature : 6a27756186e5faf84cb5e9e21fe9b37fdff15e8c
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
    TDeepLAPI.TranslateTextASync(tfile.ReadAllText(FAPIKeyFileName), 'FR', 'EN',
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

