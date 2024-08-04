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
/// File last update : 04/08/2024 07:53:24
/// Signature : a2fb8f6357c86c5fb16ca6ed503d4291ea369441
/// ***************************************************************************
/// </summary>

program DeepLProxyServerUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  OlfSoftware.DeepL.ClientLib in '..\..\..\..\src\OlfSoftware.DeepL.ClientLib.pas',
  u_urlOpen in 'u_urlOpen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
