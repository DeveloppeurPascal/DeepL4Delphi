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
