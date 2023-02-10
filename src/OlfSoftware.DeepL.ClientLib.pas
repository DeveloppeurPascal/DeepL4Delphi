unit OlfSoftware.DeepL.ClientLib;

// (c) Patrick Prémartin
//
// This file is distributed under AGPL license.
//
// Get the latest version at
// https://github.com/DeveloppeurPascal/DeepL-Delphi-client

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

  /// <summary>
  /// call DeepL API to translate the text from source_lang to target_lang
  /// (synchrone - current thread is freezed during process)
  /// </summary>
  /// <remarks>
  /// look at https://www.deepl.com/docs-api/translating-text/response/
  /// </remarks>
function DeepLTranslateTextSync(auth_key, source_lang, target_lang,
  text: string; split_sentences: string = '1';
  preserve_formatting: string = '0'; formality: string = 'default'): string;

/// <summary>
/// call DeepL API to translate the text from source_lang to target_lang
/// (asynchrone - don't freeze current thread)
/// </summary>
/// <remarks>
/// look at https://www.deepl.com/docs-api/translating-text/response/
/// </remarks>
procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTexTranslatedProc: TOnTextTranslatedProc;
  onTexTranslatedErrorProc: TOnTextTranslatedErrorProc = nil;
  split_sentences: string = '1'; preserve_formatting: string = '0';
  formality: string = 'default'); overload;

/// <summary>
/// call DeepL API to translate the text from source_lang to target_lang
/// (asynchrone - don't freeze current thread)
/// </summary>
/// <remarks>
/// look at https://www.deepl.com/docs-api/translating-text/response/
/// </remarks>
procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTexTranslatedEvent: TOnTextTranslatedEvent;
  onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent = nil;
  split_sentences: string = '1'; preserve_formatting: string = '0';
  formality: string = 'default'); overload;

const
  CDeepLAPIURL_Free = 'https://api-free.deepl.com';
  CDeepLAPIURL_Pro = 'https://api.deepl.com';

  /// <summary>
  /// Call to initialize DeepL API URL.
  /// If you forget to do, you will be on Free API.
  /// </summary>
procedure DeepLSetAPIURL(APIURL: string = CDeepLAPIURL_Free);

implementation

// TODO : (add global parameter) choose if result text with error is empty or equal original text

uses
  System.Net.HttpClient, System.Classes, System.SysUtils, System.JSON,
  System.Generics.Collections
{$IF CompilerVersion >=32.0}
    , System.Threading
{$ENDIF}
    ;

var
  DeepLAPIURL: string;

function DeepLTranslateTextSync(auth_key, source_lang, target_lang,
  text: string; split_sentences: string; preserve_formatting: string;
  formality: string): string;
var
  APIServer: thttpclient;
  Params: tstringlist;
  APIResponse: IHTTPResponse;
  JSO, JSO2: tjsonobject;
  JSA: tjsonarray;
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
    Params := tstringlist.Create;
    try
      Params.AddPair('auth_key', auth_key);
      Params.AddPair('source_lang', source_lang);
      Params.AddPair('target_lang', target_lang);
      Params.AddPair('text', text);
      Params.AddPair('split_sentences', split_sentences);
      Params.AddPair('preserve_formatting', preserve_formatting);
      Params.AddPair('formality', formality);
      APIResponse := APIServer.Post(DeepLAPIURL + '/v2/translate', Params);
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
                            JSO2 := JSA[0] as tjsonobject;
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
            // TODO : change in needed
            raise exception.Create
              ('Too many requests. Please wait and resend your request.');
          end;
      else
        // TODO : request error, see status code from https://www.deepl.com/docs-api/accessing-the-api/error-handling/
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

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTexTranslatedProc: TOnTextTranslatedProc;
  onTexTranslatedErrorProc: TOnTextTranslatedErrorProc; split_sentences: string;
  preserve_formatting: string; formality: string);
begin
{$IF CompilerVersion >=32.0}
  ttask.run(
    procedure
    var
      result: string;
    begin
      try
        result := DeepLTranslateTextSync(auth_key, source_lang, target_lang,
          text, split_sentences, preserve_formatting, formality);
        if assigned(onTexTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTexTranslatedProc(text, result, source_lang, target_lang);
            end);
      except
        on e: exception do
          if assigned(onTexTranslatedErrorProc) then
            tthread.queue(nil,
              procedure
              begin
                onTexTranslatedErrorProc(text, source_lang, target_lang,
                  e.Message);
              end);
      end;
    end);
{$ELSE}
  tthread.CreateAnonymousThread(
    procedure
    var
      result: string;
    begin
      try
        result := DeepLTranslateTextSync(auth_key, source_lang, target_lang,
          text, split_sentences, preserve_formatting, formality);
        if assigned(onTexTranslatedProc) then
          tthread.queue(nil,
            procedure
            begin
              onTexTranslatedProc(text, result, source_lang, target_lang);
            end);
      except
        on e: exception do
          if assigned(onTexTranslatedErrorProc) then
            tthread.queue(nil,
              procedure
              begin
                onTexTranslatedErrorProc(text, source_lang, target_lang,
                  e.Message);
              end);
      end;
    end).Start;
{$ENDIF}
end;

procedure DeepLTranslateTextASync(auth_key, source_lang, target_lang,
  text: string; onTexTranslatedEvent: TOnTextTranslatedEvent;
onTexTranslatedErrorEvent: TOnTextTranslatedErrorEvent; split_sentences: string;
preserve_formatting: string; formality: string); overload;
begin
  DeepLTranslateTextASync(auth_key, source_lang, target_lang, text,
    procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
    begin
      if assigned(onTexTranslatedEvent) then
        onTexTranslatedEvent(OriginalText, TranslatedText, SourceLang,
          TargetLang);
    end,
    procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
    begin
      if assigned(onTexTranslatedErrorEvent) then
        onTexTranslatedErrorEvent(OriginalText, SourceLang, TargetLang,
          ErrorText);
    end, split_sentences, preserve_formatting, formality);
end;

procedure DeepLSetAPIURL(APIURL: string);
begin
  if (APIURL.Trim.IsEmpty) then
    raise exception.Create('Please give the DeepL API URL.');
  DeepLAPIURL := APIURL;
end;

initialization

DeepLSetAPIURL(CDeepLAPIURL_Free);

end.
