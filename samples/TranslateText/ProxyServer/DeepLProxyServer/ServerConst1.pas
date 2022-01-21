unit ServerConst1;

interface

resourcestring
  sPortInUse = '- Erreur : Le port %s est d�j� utilis�';
  sPortSet = '- Port d�fini sur %s';
  sServerRunning = '- Le serveur est d�j� ex�cut�';
  sStartingServer = '- D�marrage du serveur HTTP sur le port %d';
  sStoppingServer = '- Arr�t du serveur';
  sServerStopped = '- Serveur arr�t�';
  sServerNotRunning = '- Le serveur n'#39'est pas ex�cut�';
  sInvalidCommand = '- Erreur : Commande non valide';
  sIndyVersion = '- Version Indy : ';
  sActive = '- Actif�: ';
  sPort = '- Port : ';
  sSessionID = '- Nom de cookie de l'#39'ID de session : ';
  sCommands = 'Entrez une commande : '#13#10'   - "start" pour d�marrer le serveur'#13#10'   - "stop" pour arr�ter le serveur'#13#10'   - "set port" pour changer le port par d�faut'#13#10'   - "status" pour obtenir l'#39'�tat du serveur'#13#10'   - "help" pour afficher les commandes'#13#10'   - "exit" pour fe'+
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
