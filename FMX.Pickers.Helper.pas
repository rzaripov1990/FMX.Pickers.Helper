unit FMX.Pickers.Helper;

{
  author: ZuBy

  https://github.com/rzaripov1990/FMX.Pickers.Helper
  rzaripov1990@gmail.com
}

interface

uses
  System.Types, System.SysUtils,
  FMX.Types, FMX.Forms, FMX.Controls, FMX.Platform,
  FMX.Pickers
{$IFDEF IOS}, FMX.Helpers.IOS{$ENDIF};

type
  TmyPicker = record
  private
    class var FPickerService: IFMXPickerService;
    class var FPickerList: TCustomListPicker;
    class var FPickerDate: TCustomDateTimePicker;
    class var FPickerTime: TCustomDateTimePicker;
  public
    class procedure ListShow(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
      const aIndex: integer = -1); static;
    class procedure ListFree; static;

    class procedure DateShow(const aParent: TControl; const aCurrentDate: TDateTime; aEvent: TOnDateChanged); static;
    class procedure DateFree; static;

    class procedure TimeShow(const aParent: TControl; const aCurrentTime: TDateTime; aEvent: TOnDateChanged); static;
    class procedure TimeFree; static;
  end;

implementation

{ TmyPicker }

uses
  System.UITypes;

function GetParentForm(O: TFmxObject): TForm;
var
  P: TFmxObject;
begin
  Result := nil;
  P := O.Parent;
  while (P <> nil) and (not(P is TForm)) do
    P := P.Parent;
  if P <> nil then
    Result := P as TForm;
end;

class procedure TmyPicker.DateShow(const aParent: TControl; const aCurrentDate: TDateTime; aEvent: TOnDateChanged);
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    FPickerDate := FPickerService.CreateDateTimePicker;
    FPickerDate.OnDateChanged := aEvent;
    FPickerDate.Date := aCurrentDate;
    FPickerDate.MinDate := aCurrentDate;
    FPickerDate.FirstDayOfWeek := TCalDayOfWeek.dowMonday;
    FPickerDate.ShowMode := TDatePickerShowMode.Date;
    FPickerDate.ShowWeekNumbers := true;
    FPickerDate.Show;
  end;
end;

class procedure TmyPicker.DateFree;
begin
  if Assigned(FPickerDate) then
  begin
    FPickerDate.Parent := nil;
    FPickerDate.OnDateChanged := nil;
    FPickerDate.DisposeOf;
  end;
end;

class procedure TmyPicker.ListShow(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
  const aIndex: integer = -1);
var
  i: integer;
{$IFDEF IOS}
  aWidth: Single;
  aForm: TForm;
{$ENDIF}
begin
  if aParent = nil then
    exit;

  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    FPickerList := FPickerService.CreateListPicker;
{$IFDEF IOS}
    if IsPad then
    begin
      aForm := GetParentForm(aParent);
      aWidth := aForm.Width / 3;
      FPickerList.Parent := nil;
      FPickerList.AbsoluteTargetRect := RectF((aWidth - aForm.Width) / 2, 0, aForm.Width, aForm.Height);
    end;
{$ELSE}
    FPickerList.Parent := aParent;
{$ENDIF}
    FPickerList.OnValueChanged := aEvent;
    for i := Low(aValues) to High(aValues) do
      FPickerList.Values.Add(aValues[i]);
    FPickerList.ItemIndex := aIndex;
    FPickerList.Show;
  end;
end;

class procedure TmyPicker.ListFree;
begin
  if Assigned(FPickerList) then
  begin
    FPickerList.Parent := nil;
    FPickerList.OnValueChanged := nil;
    FPickerList.DisposeOf;
  end;
end;

class procedure TmyPicker.TimeShow(const aParent: TControl; const aCurrentTime: TDateTime; aEvent: TOnDateChanged);
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    FPickerTime := FPickerService.CreateDateTimePicker;
    FPickerTime.OnDateChanged := aEvent;
    FPickerTime.Date := aCurrentTime;
    FPickerTime.ShowMode := TDatePickerShowMode.Time;
    FPickerTime.Show;
  end;
end;

class procedure TmyPicker.TimeFree;
begin
  if Assigned(FPickerTime) then
  begin
    FPickerTime.Parent := nil;
    FPickerTime.OnDateChanged := nil;
    FPickerTime.DisposeOf;
  end;
end;

end.
