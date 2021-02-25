program Translate;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  OlfSoftware.DeepL.ClientLib in '..\..\..\src\OlfSoftware.DeepL.ClientLib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
