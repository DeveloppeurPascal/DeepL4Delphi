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
  private
    { Déclarations privées }
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
    tfile.ReadAllText(tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.txt'));
  RESTRequest1.Params.ParameterByName('text').Value := Edit1.Text;
  RESTRequest1.Execute;
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
