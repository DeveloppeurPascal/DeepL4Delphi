unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1APITranslateAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;
  apikey: string;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses OlfSoftware.DeepL.ClientLib, System.json, System.Generics.Collections;

{$R *.dfm}

type
  TListeTraductions = TObjectDictionary<string, tjsonobject>;

var
  ListeTraductions: TListeTraductions;

procedure TWebModule1.WebModule1APITranslateAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  SourceLang, TargetLang, Texte, SplitSentences, PreserveFormatting,
    Formality: string;
  LTK: string;
  TexteTraduit: string;
  jso: tjsonobject;
begin
  // récupérer les paramètres de la requête
  SourceLang := Request.ContentFields.Values['source_lang'];
  TargetLang := Request.ContentFields.Values['target_lang'];
  Texte := Request.ContentFields.Values['text'];
  SplitSentences := Request.ContentFields.Values['split_sentences'];
  PreserveFormatting := Request.ContentFields.Values['preserve_formatting'];
  Formality := Request.ContentFields.Values['formality'];
  // regarder si on a déjà fait cette demande
  LTK := SourceLang + TargetLang + Texte + SplitSentences + PreserveFormatting +
    Formality;
  // si oui, envoyer la réponse de départ
  if ListeTraductions.ContainsKey(LTK) then
  begin
    Response.StatusCode := 200;
    Response.ContentType := 'application/json';
    Response.CustomHeaders.Add('Access-Control-Allow-Origin=*');
    Response.Content := ListeTraductions[LTK].tojson;
  end
  else
  begin
    // si non, faire la demande à DeepL et stocker la réponse
    try
      TexteTraduit := DeepLTranslateTextSync(apikey, SourceLang, TargetLang,
        Texte, SplitSentences, PreserveFormatting, Formality);
      // retourner la réponse trouvée dans le cache ou provanent de DeepL
      Response.StatusCode := 200;
      Response.ContentType := 'application/json';
      Response.CustomHeaders.Add('Access-Control-Allow-Origin=*');
      jso := tjsonobject.create;
      try
        jso.AddPair('translations',
          tjsonarray.create.Add(tjsonobject.create.AddPair
          ('detected_source_language', TargetLang).AddPair('text',
          TexteTraduit)));
        MonitorEnter(ListeTraductions);
        try
          ListeTraductions.Add(LTK, jso);
        finally
          MonitorExit(ListeTraductions);
        end;
        Response.Content := jso.tojson;
      finally
        // jso.free;
        // attached to the ListeTraductions dictionary,
        // using Free will generate access violations when accessing to jso values
      end;
    except
      Response.StatusCode := 500;
    end;
  end;
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content := '<html>' + '<head><title>Proxy DeepL</title></head>' +
    '<body>Proxy DeepL is waiting for your messages.</body>' + '</html>';
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
apikey := '';
ListeTraductions := TListeTraductions.create;

finalization

ListeTraductions.free;

end.
