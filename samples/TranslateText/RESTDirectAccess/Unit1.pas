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
/// File last update : 04/08/2024 07:53:21
/// Signature : bc9049b9ca0f6c9439306971e480ee67651ae7c8
/// ***************************************************************************
/// </summary>

unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, REST.Types,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, REST.Response.Adapter, REST.Client, Data.Bind.Components,
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
    { Déclarations privées }
    FAPIKeyFileName: string;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.IOUtils, System.JSON;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.lines.add('traduction lancée');
  RESTRequest1.Params.ParameterByName('auth_key').Value :=
    tfile.ReadAllText(FAPIKeyFileName);
  RESTRequest1.Params.ParameterByName('text').Value := Edit1.Text;
  RESTRequest1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
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
