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
  File last update : 2026-02-23T19:54:23.310+01:00
  Signature : 9c6edbb4111e0d3af35de3ce974f167218243ac9
  ***************************************************************************
*)

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
