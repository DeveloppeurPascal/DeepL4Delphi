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
  File last update : 2025-02-09T11:03:31.770+01:00
  Signature : 3efbe6c896fef1d71fb96b12cbce46fe858e802a
  ***************************************************************************
*)

unit u_urlOpen;

interface

/// <summary>Launch URL website on the default browser</summary>
/// <param name="URL">Absolute address of the website to open in the web browser</param>
procedure url_Open_In_Browser(URL: string);

implementation

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants
{$IF Defined(IOS)}
    , macapi.helpers, iOSapi.Foundation, FMX.helpers.iOS
{$ELSEIF Defined(ANDROID)}
    , Androidapi.JNI.GraphicsContentViewText, Androidapi.helpers
{$ELSEIF Defined(MACOS)}
    , Posix.Stdlib
{$ELSEIF Defined(MSWINDOWS)}
    , Winapi.ShellAPI, Winapi.Windows
{$ENDIF};

procedure url_Open_In_Browser(URL: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  // SharedActivity.startActivity(Intent);
  tandroidhelper.Activity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
  ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
  SharedApplication.OpenURL(StrToNSUrl(URL));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

end.
