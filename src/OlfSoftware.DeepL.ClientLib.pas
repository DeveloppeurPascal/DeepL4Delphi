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
  File last update : 2026-02-24T19:48:54.000+01:00
  Signature : ce96e802a04e4aaeae5330c5564da79f6ee6b588
  ***************************************************************************
*)

unit OlfSoftware.DeepL.ClientLib;

interface

uses
  System.JSON;

type
  /// <summary>
  /// Callback procedure when translation is ok
  /// </summary>
  TOnTextTranslatedProc = reference to procedure(OriginalText, TranslatedText,
    SourceLang, TargetLang: string);
  /// <summary>
  /// Callback procedure when translation has an error
  /// </summary>
  TOnTextTranslatedErrorProc = reference to procedure(OriginalText, SourceLang,
    TargetLang, ErrorText: string);

  /// <summary>
  /// Callback method/event when translation is ok
  /// </summary>
  TOnTextTranslatedEvent = procedure(OriginalText, TranslatedText, SourceLang,
    TargetLang: string) of object;
  /// <summary>
  /// Callback method/event when translation has an error
  /// </summary>
  TOnTextTranslatedErrorEvent = procedure(OriginalText, SourceLang, TargetLang,
    ErrorText: string) of object;

  TDeepLAPI = class
  private
    class var FServerURL: string;
    class function POST(URL, auth_key: string; Params: TJSONObject): TJSONObject;
  protected
  public
    const
      ServerURLFree = 'https://api-free.deepl.com';
      ServerURLPro = 'https://api.deepl.com';

      /// <summary>
    /// Call to initialize DeepL API Server URL.
    /// By default the Free API Server is used.
    /// </summary>
/// <param name="AServerURL">
///   URL to the server API. Use TDeepLAPI.ServerURLPro or TDeepLAPI.ServerURLFree for official servers.
/// </param>
    class procedure Init(AServerURL: string);

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (synchrone - current thread is freezed during process)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
/// <param name="OptionalSettings">
///   JSON object contening your optional parameters. They are passed "as it" to DeepL in request body.
/// </param>
    class function TranslateTextSync(auth_key, target_lang, text: string; OptionalSettings: TJSONObject;
      FreeOptionalSettings: boolean; var DetectedSourceLang: string): string; overload;
    class function TranslateTextSync(auth_key, target_lang, text: string; OptionalSettings: TJSONObject = nil;
      FreeOptionalSettings: boolean = true): string; overload;
    class function TranslateTextSync(auth_key, source_lang, target_lang, text: string; OptionalSettings: TJSONObject = nil;
      FreeOptionalSettings: boolean = true): string; overload;

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (asynchrone - don't freeze current thread)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
/// <param name="OptionalSettings">
///   JSON object contening your optional parameters. They are passed "as it" to DeepL in request body.
/// </param>
    class procedure TranslateTextASync(auth_key, target_lang,
      text: string; onTextTranslatedProc: TOnTextTranslatedProc;
      onTextTranslatedErrorProc: TOnTextTranslatedErrorProc = nil; OptionalSettings: TJSONObject = nil; FreeOptionalSettings:
      boolean = true); overload;
    class procedure TranslateTextASync(auth_key, source_lang, target_lang,
      text: string; onTextTranslatedProc: TOnTextTranslatedProc;
      onTextTranslatedErrorProc: TOnTextTranslatedErrorProc = nil; OptionalSettings: TJSONObject = nil; FreeOptionalSettings:
      boolean = true); overload;

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (asynchrone - don't freeze current thread)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
/// <param name="OptionalSettings">
///   JSON object contening your optional parameters. They are passed "as it" to DeepL in request body.
/// </param>
    class procedure TranslateTextASync(auth_key, target_lang, text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
      onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil; OptionalSettings: TJSONObject = nil; FreeOptionalSettings:
      boolean = true); overload;
    class procedure TranslateTextASync(auth_key, source_lang, target_lang,
      text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
      onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil; OptionalSettings: TJSONObject = nil; FreeOptionalSettings:
      boolean = true); overload;
  end;

{$REGION 'deprecated features, use TDeepLAPI class'}
/// <summary>
/// call DeepL API to translate the text from source_lang to target_lang
/// (synchrone - current thread is freezed during process)
/// </summary>
/// <remarks>
/// look at https://developers.deepl.com/api-reference/translate
/// </remarks>
function DeepLTranslateTextSync(auth_key, source_lang, target_lang,
  text: string; split_sentences: string = '1';
  preserve_formatting: string = '0'; formality: string = 'default'): string; deprecated 'use TDeepLAPI.TranslateTextSync';

/// <summary>
/// call DeepL API to translate the text from source_lang to target_lang
/// (asynchrone - don't freeze current thread)
/// </summary>
/// <remarks>
/// look at https://developers.deepl.com/api-reference/translate
/// </remarks>
procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc = nil;
  split_sentences: string = '1'; preserve_formatting: string = '0';
  formality: string = 'default'); overload; deprecated 'use TDeepLAPI.TranslateTextASync';

/// <summary>
/// call DeepL API to translate the text from source_lang to target_lang
/// (asynchrone - don't freeze current thread)
/// </summary>
/// <remarks>
/// look at https://developers.deepl.com/api-reference/translate
/// </remarks>
procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil;
  split_sentences: string = '1'; preserve_formatting: string = '0';
  formality: string = 'default'); overload; deprecated 'use TDeepLAPI.TranslateTextASync';

const
  CDeepLAPIURL_Free = TDeepLAPI.ServerURLFree deprecated 'use TDeepLAPI.ServerURLFree';
  CDeepLAPIURL_Pro = TDeepLAPI.ServerURLPro deprecated 'use TDeepLAPI.ServerURLPro';

/// <summary>
/// Call to initialize DeepL API URL.
/// If you forget to do, you will be on Free API.
/// </summary>
procedure DeepLSetAPIURL(APIURL: string = TDeepLAPI.ServerURLFree); deprecated 'use TDeepLAPI.Init';
{$ENDREGION}

implementation

// TODO : (add global parameter) choose if result text with error is empty or equal original text

uses
  System.Net.HttpClient,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections
{$IF CompilerVersion >= 32.0} // from 10.2 Tokyo
  ,
  System.Threading
{$ENDIF}
  ;

function DeepLTranslateTextSync(auth_key, source_lang, target_lang,
  text: string; split_sentences: string; preserve_formatting: string;
  formality: string): string;
begin
  result := TDeepLAPI.TranslateTextSync(auth_key, source_lang, target_lang, text,
    TJSONObject.Create.AddPair('split_sentences', split_sentences).addpair('preserve_formatting', preserve_formatting =
    '1').addpair('formality', formality));
end;

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc; split_sentences: string;
  preserve_formatting: string; formality: string);
begin
  TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang, text, onTextTranslatedProc, onTextTranslatedErrorProc,
    TJSONObject.Create.AddPair('split_sentences', split_sentences).addpair('preserve_formatting', preserve_formatting =
    '1').addpair('formality', formality));
end;

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent; split_sentences: string;
  preserve_formatting: string; formality: string); overload;
begin
  TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang, text, onTextTranslatedEvent, onTextTranslatedErrorEvent,
    TJSONObject.Create.AddPair('split_sentences', split_sentences).addpair('preserve_formatting', preserve_formatting =
    '1').addpair('formality', formality));
end;

procedure DeepLSetAPIURL(APIURL: string);
begin
  TDeepLAPI.Init(APIURL);
end;

{ TDeepLAPI }

class procedure TDeepLAPI.Init(AServerURL: string);
begin
  if (AServerURL.Trim.IsEmpty) then
    raise exception.Create('Please give the DeepL API URL.');
  FServerURL := AServerURL;
end;

class procedure TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc; OptionalSettings: TJSONObject; FreeOptionalSettings: boolean);
begin
  if source_lang.IsEmpty then
    TranslateTextaSync(auth_key, target_lang, text, onTextTranslatedProc, onTextTranslatedErrorProc, OptionalSettings,
      FreeOptionalSettings)
  else if not assigned(OptionalSettings) then
    TranslateTextaSync(auth_key, target_lang, text, onTextTranslatedProc, onTextTranslatedErrorProc,
      TJSONObject.Create.AddPair('source_lang', source_lang), true)
  else
    TranslateTextaSync(auth_key, target_lang, text, onTextTranslatedProc, onTextTranslatedErrorProc,
      OptionalSettings.AddPair('source_lang', source_lang), FreeOptionalSettings);
end;

class function TDeepLAPI.POST(URL, auth_key: string; Params: TJSONObject): TJSONObject;
var
  APIServer: THTTPClient;
  ParamsStream: TStringStream;
  APIResponse: IHTTPResponse;
  JSONResponse: string;
begin
  APIServer := THTTPClient.Create;
  try
    APIServer.CustomHeaders['Content-Type'] := 'application/json';
    if not auth_key.IsEmpty then
      APIServer.CustomHeaders['Authorization'] := 'DeepL-Auth-Key ' + auth_key;
    ParamsStream := TStringStream.Create(Params.ToJSON);
    try
      ParamsStream.Position := 0;
      APIResponse := APIServer.Post(url, ParamsStream);
    finally
      ParamsStream.Free;
    end;
    if assigned(APIResponse) then
      case (APIResponse.StatusCode) of
        200:
          begin // ok
            JSONResponse := APIResponse.ContentAsString(tencoding.UTF8);
            if JSONResponse.IsEmpty then
              result := nil
            else
              try
                result := TJSONObject.ParseJSONValue(JSONResponse) as TJSONObject;
              except
                result := nil;
              end;
          end;
        429:
          begin // Too many requests. Please wait and resend your request.
            // TODO : retry is suggested by DeepL, see https://developers.deepl.com/docs/best-practices/pre-production-checklist
            raise exception.Create
            ('Too many requests. Please wait and resend your request.');
          end;
        500:
          begin // Internal Server Error. Please wait and resend your request.
            // TODO : retry is suggested by DeepL, see https://developers.deepl.com/docs/best-practices/pre-production-checklist
            raise exception.Create
            ('Internal Server Error. Please wait and resend your request.');
          end;
      else
        // TODO : request error, see status code from https://developers.deepl.com/docs/best-practices/error-handling
        raise exception.Create(APIResponse.StatusCode.ToString + ' ' +
          APIResponse.StatusText);
      end
    else
      // TODO : error APIResponse undefined
      raise Exception.Create('Undefined answer from DeepL server.');
  finally
    APIServer.free;
  end;
end;

class procedure TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent; OptionalSettings: TJSONObject; FreeOptionalSettings: boolean);
begin
  TranslateTextASync(auth_key, source_lang, target_lang, text,
    procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
    begin
      if assigned(onTextTranslatedEvent) then
        onTextTranslatedEvent(OriginalText, TranslatedText, SourceLang,
          TargetLang);
    end,
    procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
    begin
      if assigned(onTextTranslatedErrorEvent) then
        onTextTranslatedErrorEvent(OriginalText, SourceLang, TargetLang,
          ErrorText);
    end, OptionalSettings, FreeOptionalSettings);
end;

class function TDeepLAPI.TranslateTextSync(auth_key, target_lang, text: string;
  OptionalSettings: TJSONObject; FreeOptionalSettings: boolean;
  var DetectedSourceLang: string): string;
var
  Params, Response, FirstItem: TJSONObject;
  Translations: TJSONArray;
  i: integer;
begin
  if text.IsEmpty then
  begin
    result := '';
    DetectedSourceLang := '';
  end
  else
  begin
    // TODO : control the content of the parameters to ensure that they are what the API expects
    Params := TJSONObject.Create;
    try
      Params.AddPair('target_lang', target_lang);
      Params.AddPair('text', TJSONArray.Create.add(text));
      if assigned(OptionalSettings) then
      begin
        for i := OptionalSettings.Count - 1 downto 0 do
          Params.AddPair(OptionalSettings.RemovePair(OptionalSettings.pairs[i].JsonString.Value));
        if FreeOptionalSettings then
          OptionalSettings.Free;
      end;
      response := POST(FServerURL + '/v2/translate', auth_key, Params);
    finally
      Params.free;
    end;
    if assigned(Response) then
      try
        if Response.TryGetValue<TJSONArray>('translations', Translations) and (Translations.Count = 1) then
        begin
          try
{$IF CompilerVersion < 31.0} // before 10.1 Berlin
            // cf https://github.com/DeveloppeurPascal/DeepL4Delphi/issues/6
            FirstItem := Translations.Items[0] as tjsonobject;
{$ELSE}
            FirstItem := Translations[0] as tjsonobject;
{$ENDIF}
          except
            FirstItem := nil;
          end;
          if assigned(FirstItem) then
          begin
            if not FirstItem.TryGetValue<string>('detected_source_language', DetectedSourceLang) then
              DetectedSourceLang := '';
            if not FirstItem.TryGetValue<string>('text', result) then
              result := '';
          end
          else
            raise exception.Create('No or more than 1 answer from DeepL server.');
        end;
      finally
        Response.free;
      end
    else
    begin
      result := '';
      DetectedSourceLang := '';
    end;
  end;
end;

class procedure TDeepLAPI.TranslateTextASync(auth_key, target_lang,
  text: string; onTextTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc;
  OptionalSettings: TJSONObject; FreeOptionalSettings: boolean);
var
  ErrorMessage: string;
begin
{$IF CompilerVersion >= 32.0} // from 10.2 Tokyo
  ttask.run(
    procedure
    var
      Source_Lang,
      result: string;
    begin
      if OptionalSettings.TryGetValue<string>('source_lang', source_lang) then
        source_lang := '';
      try
        result := TranslateTextSync(auth_key, target_lang, text, OptionalSettings, FreeOptionalSettings, Source_Lang);
        if assigned(onTextTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTextTranslatedProc(text, result, source_lang, target_lang);
            end);
      except
        on e: exception do
        begin
          ErrorMessage := e.Message;
          if assigned(onTextTranslatedErrorProc) then
            tthread.queue(nil,
              procedure
              begin
                onTextTranslatedErrorProc(text, source_lang, target_lang, ErrorMessage);
              end);
        end;
      end;
    end);
{$ELSE}
  tthread.CreateAnonymousThread(
    procedure
    var
      source_lang,
      result: string;
    begin
      if OptionalSettings.TryGetValue<string>('source_lang', source_lang) then
        source_lang := '';
      try
        result := TranslateTextSync(auth_key, target_lang, text, OptionalSettings, FreeOptionalSettings);
        if assigned(onTextTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTextTranslatedProc(text, result, source_lang, target_lang);
            end);
      except
        on e: exception do
        begin
          ErrorMessage := e.Message;
          if assigned(onTextTranslatedErrorProc) then
            tthread.queue(nil,
              procedure
              begin
                onTextTranslatedErrorProc(text, source_lang, target_lang,
                  ErrorMessage);
              end);
        end;
      end;
    end).Start;
{$ENDIF}
end;

class procedure TDeepLAPI.TranslateTextASync(auth_key, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTextTranslatedErrorEvent: TOnTextTranslatedErrorEvent;
  OptionalSettings: TJSONObject; FreeOptionalSettings: boolean);
begin
  TranslateTextASync(auth_key, target_lang, text,
    procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
    begin
      if assigned(onTextTranslatedEvent) then
        onTextTranslatedEvent(OriginalText, TranslatedText, SourceLang,
          TargetLang);
    end,
    procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
    begin
      if assigned(onTextTranslatedErrorEvent) then
        onTextTranslatedErrorEvent(OriginalText, SourceLang, TargetLang,
          ErrorText);
    end, OptionalSettings, FreeOptionalSettings);
end;

class function TDeepLAPI.TranslateTextSync(auth_key, target_lang, text: string;
  OptionalSettings: TJSONObject; FreeOptionalSettings: boolean): string;
var
  DetectedSourceLang: string;
begin
  TranslateTextSync(auth_key, target_lang, text, OptionalSettings, FreeOptionalSettings, DetectedSourceLang);
end;

class function TDeepLAPI.TranslateTextSync(auth_key, source_lang, target_lang,
  text: string; OptionalSettings: TJSONObject; FreeOptionalSettings: boolean): string;
var
  DetectedSourceLang: string;
begin
  if source_lang.IsEmpty then
    result := TranslateTextSync(auth_key, target_lang, text, OptionalSettings, FreeOptionalSettings, DetectedSourceLang)
  else if not assigned(OptionalSettings) then
    result := TranslateTextSync(auth_key, target_lang, text, TJSONObject.Create.AddPair('source_lang', source_lang), true,
      DetectedSourceLang)
  else
    result := TranslateTextSync(auth_key, target_lang, text, OptionalSettings.AddPair('source_lang', source_lang),
      FreeOptionalSettings, DetectedSourceLang);
end;

initialization

  TDeepLAPI.Init(TDeeplAPI.ServerURLFree);

end.

