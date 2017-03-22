program prjPickers;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  FMX.Pickers.Helper in 'FMX.Pickers.Helper.pas',
  FMX.Pickers.Android in 'FMX.Pickers.Android.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
