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
  File last update : 2026-02-24T18:18:20.000+01:00
  Signature : 58681f9fa6e926653f1ab8c36bc744a1a90183d1
  ***************************************************************************
*)

unit OlfSoftware.DeepL.ClientLib;

interface

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
  protected
  public
    const
      ServerURLFree = 'https://api-free.deepl.com';
      ServerURLPro = 'https://api.deepl.com';

      /// <summary>
    /// Call to initialize DeepL API Server URL.
    /// By default the Free API Server is used.
    /// </summary>
    class procedure Init(AServerURL: string);

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (synchrone - current thread is freezed during process)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
    class function TranslateTextSync(auth_key, source_lang, target_lang,
      text: string; split_sentences: string = '1';
      preserve_formatting: boolean = false; formality: string = 'default'): string;

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (asynchrone - don't freeze current thread)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
    class procedure TranslateTextASync(auth_key, source_lang, target_lang,
      text: string; onTexTranslatedProc: TOnTextTranslatedProc;
      onTextTranslatedErrorProc: TOnTextTranslatedErrorProc = nil;
      split_sentences: string = '1'; preserve_formatting: boolean = false;
      formality: string = 'default'); overload;

    /// <summary>
    /// call DeepL API to translate the text from source_lang to target_lang
    /// (asynchrone - don't freeze current thread)
    /// </summary>
    /// <remarks>
    /// look at https://developers.deepl.com/api-reference/translate
    /// </remarks>
    class procedure TranslateTextASync(auth_key, source_lang, target_lang,
      text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
      onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil;
      split_sentences: string = '1'; preserve_formatting: boolean = false;
      formality: string = 'default'); overload;
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
  text: string; onTexTranslatedProc: TOnTextTranslatedProc;
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
  onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil;
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
  System.JSON,
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
  result := TDeepLAPI.TranslateTextSync(auth_key, source_lang, target_lang, text, split_sentences, preserve_formatting = '1',
    formality)
end;

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTexTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc; split_sentences: string;
  preserve_formatting: string; formality: string);
begin
  TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang, text, onTexTranslatedProc, onTextTranslatedErrorProc,
    split_sentences, preserve_formatting = '1', formality);
end;

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent; split_sentences: string;
  preserve_formatting: string; formality: string); overload;
begin
  TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang, text, onTextTranslatedEvent, onTexTranslatedErrorEvent,
    split_sentences, preserve_formatting = '1', formality);
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
  text: string; onTexTranslatedProc: TOnTextTranslatedProc;
  onTextTranslatedErrorProc: TOnTextTranslatedErrorProc; split_sentences: string;
  preserve_formatting: boolean; formality: string);
var
  ErrorMessage: string;
begin
{$IF CompilerVersion >= 32.0} // from 10.2 Tokyo
  ttask.run(
    procedure
    var
      result: string;
    begin
      try
        result := TranslateTextSync(auth_key, source_lang, target_lang,
          text, split_sentences, preserve_formatting, formality);
        if assigned(onTexTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTexTranslatedProc(text, result, source_lang, target_lang);
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
    end);
{$ELSE}
  tthread.CreateAnonymousThread(
    procedure
    var
      result: string;
    begin
      try
        result := TranslateTextSync(auth_key, source_lang, target_lang,
          text, split_sentences, preserve_formatting, formality);
        if assigned(onTexTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTexTranslatedProc(text, result, source_lang, target_lang);
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

class procedure TDeepLAPI.TranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTextTranslatedEvent: TOnTextTranslatedEvent;
  onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent; split_sentences: string;
  preserve_formatting: boolean; formality: string);
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
      if assigned(onTexTranslatedErrorEvent) then
        onTexTranslatedErrorEvent(OriginalText, SourceLang, TargetLang,
          ErrorText);
    end, split_sentences, preserve_formatting, formality);
end;

class function TDeepLAPI.TranslateTextSync(auth_key, source_lang, target_lang,
  text, split_sentences: string; preserve_formatting: boolean; formality: string): string;
var
  APIServer: THTTPClient;
  Params: TJSONObject;
  ParamsStream: TStringStream;
  APIResponse: IHTTPResponse;
  JSO, JSO2: TJSONObject;
  JSA: TJSONArray;
  JSONResponse: string;
begin
  if text.IsEmpty then
  begin
    result := '';
    exit;
  end;
  // TODO : control the content of the parameters to ensure that they are what the API expects
  APIServer := thttpclient.Create;
  try
    APIServer.CustomHeaders['Content-Type'] := 'application/json';
    if not auth_key.IsEmpty then
      APIServer.CustomHeaders['Authorization'] := 'DeepL-Auth-Key ' + auth_key;
    Params := TJSONObject.Create;
    try
      Params.AddPair('source_lang', source_lang);
      Params.AddPair('target_lang', target_lang);
      Params.AddPair('text', TJSONArray.Create.add(text));
      Params.AddPair('split_sentences', split_sentences);
      Params.AddPair('preserve_formatting', preserve_formatting);
      Params.AddPair('formality', formality);
      ParamsStream := TStringStream.Create(Params.ToJSON);
      try
        ParamsStream.Position := 0;
        APIResponse := APIServer.Post(FServerURL + '/v2/translate', ParamsStream);
      finally
        ParamsStream.Free;
      end;
    finally
      Params.free;
    end;
    if assigned(APIResponse) then
      case (APIResponse.StatusCode) of
        200:
          begin // ok
            JSONResponse := APIResponse.ContentAsString(tencoding.UTF8);
            if JSONResponse.IsEmpty then
            begin
              // TODO : error
              result := '';
            end
            else
              try
                JSO := tjsonobject.ParseJSONValue(JSONResponse) as tjsonobject;
                if assigned(JSO) then
                  try
                    try
                      JSA := JSO.GetValue('translations') as tjsonarray;
                      if assigned(JSA) then
                        if (JSA.Count = 1) then
                        begin
                          try
{$IF CompilerVersion < 31.0} // before 10.1 Berlin
                            // cf https://github.com/DeveloppeurPascal/DeepL4Delphi/issues/6
                            JSO2 := JSA.Items[0] as tjsonobject;
{$ELSE}
                            JSO2 := JSA[0] as tjsonobject;
{$ENDIF}
                            if assigned(JSO2) then
                              try
                                result := (JSO2.GetValue('text')
                                  as tjsonstring).Value
                              except
                                result := '';
                                // TODO : text undefined
                              end
                            else
                            begin
                              result := '';
                              // TODO : JSO2 (translated item) undefined
                            end;
                          except
                            result := '';
                            // TODO : JSO2 (translated item) undefined
                          end;
                        end
                        else
                        begin
                          result := '';
                          // TODO : add an error log somewhere
                          raise exception.Create('DeepL response with ' +
                            JSA.Count.ToString + ' but attended 1.');
                        end
                      else
                      begin
                        result := '';
                        // TODO : JSA undefined
                      end;
                    except
                      result := '';
                      // TODO : JSA undefined
                    end;
                  finally
                    JSO.free;
                  end
                else
                begin
                  result := '';
                  // TODO : JSO undefined or wrong format
                end;
              except
                result := '';
                // TODO : JSO undefined or wrong format
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
    begin
      result := '';
      // TODO : error APIResponse undefined
    end;
  finally
    APIServer.free;
  end;
end;

initialization

  TDeepLAPI.Init(TDeeplAPI.ServerURLFree);

end.

