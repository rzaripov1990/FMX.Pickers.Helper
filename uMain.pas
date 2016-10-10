unit uMain;

{
  author: ZuBy

  https://github.com/rzaripov1990/CustomPickersHelper
  rzaripov1990@gmail.com
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects;

type
  TForm1 = class(TForm)
    btList: TButton;
    btDate: TButton;
    btTime: TButton;
    lbListValue: TLabel;
    lbDateValue: TLabel;
    lbTimeValue: TLabel;
    lbDateTimeValue: TLabel;
    procedure btListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btDateClick(Sender: TObject);
    procedure btTimeClick(Sender: TObject);
  private
    { Private declarations }
    procedure PickerListClick(Sender: TObject; const AValueIndex: Integer);
    procedure PickerDateClick(Sender: TObject; const ADateTime: TDateTime);
    procedure PickerTimeClick(Sender: TObject; const ADateTime: TDateTime);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  FPickerListValues: TArray<string>;

implementation

{$R *.fmx}

uses
  System.DateUtils, FMX.Pickers.Helper;

procedure TForm1.btListClick(Sender: TObject);
begin
  Setlength(FPickerListValues, 3);
  FPickerListValues[0] := 'aaa';
  FPickerListValues[1] := 'bbb';
  FPickerListValues[2] := 'ccc';

  TmyPicker.ListFree;
  TmyPicker.ListShow(FPickerListValues, btList, PickerListClick);
end;

procedure TForm1.PickerListClick(Sender: TObject; const AValueIndex: Integer);
begin
  lbListValue.Text := FPickerListValues[AValueIndex];
end;

procedure TForm1.btDateClick(Sender: TObject);
begin
  TmyPicker.DateFree;
  TmyPicker.DateShow(btDate, StrToDateDef(lbDateValue.Text, now), PickerDateClick);
end;

procedure TForm1.PickerDateClick(Sender: TObject; const ADateTime: TDateTime);
begin
  lbDateValue.Text := DateTimeToStr(ADateTime);

  TmyPicker.TimeFree;
  TmyPicker.TimeShow(btTime, now, PickerTimeClick);
end;

procedure TForm1.btTimeClick(Sender: TObject);
var
  aDate: TDateTime;
begin
  aDate := StrToDateDef(lbTimeValue.Text, Date);
  TmyPicker.TimeFree;
  TmyPicker.TimeShow(btTime, aDate, PickerTimeClick);
end;

procedure TForm1.PickerTimeClick(Sender: TObject; const ADateTime: TDateTime);
var
  aCurDate: TDateTime;
begin
  aCurDate := StrToDateDef(lbDateValue.Text, Date);
  lbTimeValue.Text := TimeToStr(ADateTime);

  lbDateTimeValue.Text := DateTimeToStr(aCurDate + TimeOf(ADateTime));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TmyPicker.ListFree;
  TmyPicker.TimeFree;
  TmyPicker.DateFree;
end;

initialization

FormatSettings.LongDateFormat := 'dd.mm.yyyy';
FormatSettings.DateSeparator := '.';

end.
