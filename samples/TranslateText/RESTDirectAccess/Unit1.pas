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
  File last update : 2026-02-23T19:54:23.346+01:00
  Signature : 234d35d752a8653cfdf0fbec8354d2ba7b355ac7
  ***************************************************************************
*)

unit Unit1;

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
  REST.Types,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  REST.Response.Adapter,
  REST.Client,
  Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    procedure Button1Click(Sender: TObject);
    procedure RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
    procedure FormCreate(Sender: TObject);
  private
    FAPIKeyFileName: string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  System.JSON;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.add('traduction lancée');
  RESTRequest1.Params.ParameterByName('Authorization').Value := 'DeepL-Auth-Key ' + tfile.ReadAllText(FAPIKeyFileName);
  RESTRequest1.Params.ParameterByName('text').Value := Edit1.Text;
  RESTRequest1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAPIKeyFileName := tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.dat');
  if not tfile.Exists(FAPIKeyFileName) then
    FAPIKeyFileName := tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.txt');
  if not tfile.Exists(FAPIKeyFileName) then
    raise exception.Create('File ' + FAPIKeyFileName +
      ' doesn''t exists. Please create it and put there your DeepL API key.');

  // DeepL Free API
  RESTClient1.baseurl := 'https://api-free.deepl.com/v2/translate';

  // DeepL Pro API
  // RESTClient1.baseurl := 'https://api.deepl.com/v2/translate';
end;

procedure TForm1.RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
begin
  showmessage(RESTResponse1.Content);
  if (RESTResponse1.StatusCode = 200) then
  begin
    var
    jso := TJSONObject.ParseJSONValue(RESTResponse1.Content) as TJSONObject;
    try
      var
      jsa := jso.GetValue('translations') as tjsonarray;
      var
      jsitem := jsa[0] as TJSONObject;
      Memo1.lines.add((jsitem.GetValue('text') as tjsonstring).Value);
    finally
      jso.free;
    end;
  end;
end;

end.

