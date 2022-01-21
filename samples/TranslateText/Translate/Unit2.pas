unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit;

type
  TForm2 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    FAPIKeyFileName: string;
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses OlfSoftware.DeepL.ClientLib, System.IOUtils, System.Threading;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Button1.EnableD := false;
  try
    Memo1.Lines.Add('Translation of "' + Edit1.Text + '" submitted.');
    DeepLTranslateTextASync(tfile.ReadAllText(FAPIKeyFileName), 'FR', 'EN',
      Edit1.Text,
      procedure(OriginalText, TranslatedText, SourceLang, TargetLang: string)
      begin
        Memo1.Lines.Add('Translating "' + OriginalText + '"');
        Memo1.Lines.Add('=> "' + TranslatedText + '"');
        Button1.EnableD := true;
      end,
      procedure(OriginalText, SourceLang, TargetLang, ErrorText: string)
      begin
        Memo1.Lines.Add('Translating "' + OriginalText + '"');
        Memo1.Lines.Add('=> "' + ErrorText + '"');
        Button1.EnableD := true;
      end);
    Edit1.SetFocus;
  except
    Button1.EnableD := true;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FAPIKeyFileName := tpath.combine(tpath.GetDocumentsPath, 'cle-deepl.txt');
  if not tfile.Exists(FAPIKeyFileName) then
    raise exception.Create('File ' + FAPIKeyFileName +
      ' doesn''t exists. Please create it and put there your DeepL API key.');
end;

end.
