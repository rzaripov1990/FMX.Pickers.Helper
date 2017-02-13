# FMX.Pickers.Helper
###### Platforms - Android/IOS/Windows/MACOS


```
var
  // uses FMX.Pickers;
  FPickerList: TCustomListPicker;
  FPickerDate: TCustomDateTimePicker;
  FPickerTime: TCustomDateTimePicker;
```

Native List Picker
```
  Setlength(FPickerListValues, 3);
  FPickerListValues[0] := 'aaa';
  FPickerListValues[1] := 'bbb';
  FPickerListValues[2] := 'ccc';

  TmyPicker.ListFree(FPickerList);
  FPickerList := TmyPicker.ListShow(FPickerListValues, btList, PickerListClick);
```

Native Date Picker
```
  TmyPicker.DateFree(FPickerDate);
  FPickerDate := TmyPicker.DateShow(btDate, StrToDateDef(lbDateValue.Text, now), PickerDateClick);
```

Native Time Picker
```
  TmyPicker.TimeFree(FPickerTime);
  FPickerTime := TmyPicker.TimeShow(btTime, now, PickerTimeClick);
```
