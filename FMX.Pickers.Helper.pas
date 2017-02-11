unit FMX.Pickers.Helper;

{
  author: ZuBy

  https://github.com/rzaripov1990/FMX.Pickers.Helper
  rzaripov1990@gmail.com
}

interface

uses
  System.Types, FMX.Controls, FMX.Pickers;

type
  TmyPicker = record
  public
    class function ListShow(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
      const aIndex: integer = -1): TCustomListPicker; static;
    class procedure ListFree(aPickerList: TCustomListPicker); static;

    class function DateShow(const aParent: TControl; const aCurrentDate: TDateTime; aEvent: TOnDateChanged)
      : TCustomDateTimePicker; static;
    class procedure DateFree(aPickerDate: TCustomDateTimePicker); static;

    class function TimeShow(const aParent: TControl; const aCurrentTime: TDateTime; aEvent: TOnDateChanged)
      : TCustomDateTimePicker; static;
    class procedure TimeFree(aPickerTime: TCustomDateTimePicker); static;
  end;

implementation

{ TmyPicker }

uses
  System.SysUtils, System.UITypes, FMX.Platform, FMX.Types, FMX.Forms;

type
  PickerException = class(Exception);

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

class function TmyPicker.DateShow(const aParent: TControl; const aCurrentDate: TDateTime; aEvent: TOnDateChanged)
  : TCustomDateTimePicker;
var
  FPickerService: IFMXPickerService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    Result := FPickerService.CreateDateTimePicker;
    Result.OnDateChanged := aEvent;
    Result.Date := aCurrentDate;
    Result.MinDate := aCurrentDate;
    Result.FirstDayOfWeek := TCalDayOfWeek.dowMonday;
    Result.ShowMode := TDatePickerShowMode.Date;
    Result.ShowWeekNumbers := true;
    Result.Show;
  end
  else
    raise PickerException.Create('Platform not support');
end;

class procedure TmyPicker.DateFree(aPickerDate: TCustomDateTimePicker);
begin
  if Assigned(aPickerDate) then
  begin
    aPickerDate.Hide;
    aPickerDate.Parent := nil;
    aPickerDate.OnDateChanged := nil;
{$IF defined(ANDROID) or defined(IOS)}
    aPickerDate.DisposeOf;
    aPickerDate := nil;
{$ELSE}
    FreeAndNil(aPickerDate);
{$ENDIF}
  end;
end;

class function TmyPicker.ListShow(const aValues: TArray<string>; const aParent: TControl; aEvent: TOnValueChanged;
  const aIndex: integer = -1): TCustomListPicker;
var
  FPickerService: IFMXPickerService;
  i: integer;
{$IFDEF IOS}
  aWidth: Single;
  aForm: TForm;
{$ENDIF}
begin
  if aParent = nil then
    raise PickerException.Create('Parent cannot be empty');

  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    Result := FPickerService.CreateListPicker;
{$IFDEF IOS}
    aForm := GetParentForm(aParent);
    aWidth := aForm.Width / 3;
    Result.Parent := nil;
    Result.AbsoluteTargetRect := RectF((aWidth - aForm.Width) / 2, 0, aForm.Width, aForm.Height);
{$ELSE}
    Result.Parent := aParent;
{$ENDIF}
    Result.OnValueChanged := aEvent;
    for i := Low(aValues) to High(aValues) do
      Result.Values.Add(aValues[i]);
    Result.ItemIndex := aIndex;
    Result.Show;
  end
  else
    raise PickerException.Create('Platform not support');
end;

class procedure TmyPicker.ListFree(aPickerList: TCustomListPicker);
begin
  if Assigned(aPickerList) then
  begin
    aPickerList.Hide;
    aPickerList.Parent := nil;
    aPickerList.OnValueChanged := nil;
{$IF defined(ANDROID) or defined(IOS)}
    aPickerList.DisposeOf;
    aPickerList := nil;
{$ELSE}
    FreeAndNil(aPickerList);
{$ENDIF}
  end;
end;

class function TmyPicker.TimeShow(const aParent: TControl; const aCurrentTime: TDateTime; aEvent: TOnDateChanged)
  : TCustomDateTimePicker;
var
  FPickerService: IFMXPickerService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPickerService, FPickerService) then
  begin
    Result := FPickerService.CreateDateTimePicker;
    Result.OnDateChanged := aEvent;
    Result.Date := aCurrentTime;
    Result.ShowMode := TDatePickerShowMode.Time;
    Result.Show;
  end
  else
    raise PickerException.Create('Platform not support');
end;

class procedure TmyPicker.TimeFree(aPickerTime: TCustomDateTimePicker);
begin
  if Assigned(aPickerTime) then
  begin
    aPickerTime.Hide;
    aPickerTime.Parent := nil;
    aPickerTime.OnDateChanged := nil;
{$IF defined(ANDROID) or defined(IOS)}
    aPickerTime.DisposeOf;
    aPickerTime := nil;
{$ELSE}
    FreeAndNil(aPickerTime);
{$ENDIF}
  end;
end;

end.
