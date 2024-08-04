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
/// File last update : 04/08/2024 07:53:27
/// Signature : 26de1d8806dfbb70616e00740921a4ff3c60dd6e
/// ***************************************************************************
/// </summary>

unit ServerConst1;

interface

resourcestring
  sPortInUse = '- Erreur : Le port %s est déjà utilisé';
  sPortSet = '- Port défini sur %s';
  sServerRunning = '- Le serveur est déjà exécuté';
  sStartingServer = '- Démarrage du serveur HTTP sur le port %d';
  sStoppingServer = '- Arrêt du serveur';
  sServerStopped = '- Serveur arrêté';
  sServerNotRunning = '- Le serveur n'#39'est pas exécuté';
  sInvalidCommand = '- Erreur : Commande non valide';
  sIndyVersion = '- Version Indy : ';
  sActive = '- Actif : ';
  sPort = '- Port : ';
  sSessionID = '- Nom de cookie de l'#39'ID de session : ';
  sCommands = 'Entrez une commande : '#13#10'   - "start" pour démarrer le serveur'#13#10'   - "stop" pour arrêter le serveur'#13#10'   - "set port" pour changer le port par défaut'#13#10'   - "status" pour obtenir l'#39'état du serveur'#13#10'   - "help" pour afficher les commandes'#13#10'   - "exit" pour fe'+
'rmer l'#39'application';

const
  cArrow = '->';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';

implementation

end.
