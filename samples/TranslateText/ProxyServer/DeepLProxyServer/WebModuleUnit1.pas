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
  File last update : 2026-02-23T19:54:23.317+01:00
  Signature : b3beeff69727cf6a26a1a39e5503cd575db271d1
  ***************************************************************************
*)

unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModule1APITranslateAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;
  apikey: string;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses
  OlfSoftware.DeepL.ClientLib,
  System.json,
  System.Generics.Collections;

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
  // writeln(request.Content);

  // for var i := 0 to request.ContentFields.Count-1 do
  // writeln(          request.ContentFields[i]);

  Response.CustomHeaders.Add('Access-Control-Allow-Origin=*');

  // récupérer les paramètres de la requête
  if (Request.ContentFields.IndexOfName('source_lang') < 0) then
  begin
    Response.StatusCode := 404;
    exit;
  end
  else
    SourceLang := Request.ContentFields.Values['source_lang'];
  if (Request.ContentFields.IndexOfName('target_lang') < 0) then
  begin
    Response.StatusCode := 404;
    exit;
  end
  else
    TargetLang := Request.ContentFields.Values['target_lang'];
  if (Request.ContentFields.IndexOfName('text') < 0) then
  begin
    Response.StatusCode := 404;
    exit;
  end
  else
    Texte := Request.ContentFields.Values['text'];
  if (Request.ContentFields.IndexOfName('split_sentences') < 0) then
    SplitSentences := '1'
  else
    SplitSentences := Request.ContentFields.Values['split_sentences'];
  if (Request.ContentFields.IndexOfName('preserve_formatting') < 0) then
    PreserveFormatting := '0'
  else
    PreserveFormatting := Request.ContentFields.Values['preserve_formatting'];
  if (Request.ContentFields.IndexOfName('formality') < 0) then
    Formality := 'default'
  else
    Formality := Request.ContentFields.Values['formality'];
  // regarder si on a déjà fait cette demande
  LTK := SourceLang + TargetLang + Texte + SplitSentences + PreserveFormatting +
  Formality;
  // si oui, envoyer la réponse de départ
  if ListeTraductions.ContainsKey(LTK) then
  begin
    Response.StatusCode := 200;
    Response.ContentType := 'application/json';
    // Response.CustomHeaders.Add('Access-Control-Allow-Origin=*');
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
      // Response.CustomHeaders.Add('Access-Control-Allow-Origin=*');
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

