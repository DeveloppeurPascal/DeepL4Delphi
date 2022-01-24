program DeepLProxyServerTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {frmMain},
  OlfSoftware.DeepL.ClientLib in '..\..\..\..\src\OlfSoftware.DeepL.ClientLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
