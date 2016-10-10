# CustomPickersHelper
###### Platforms - Android/IOS/Windows/MACOS

Native List Picker
```
  Setlength(FPickerListValues, 3);
  FPickerListValues[0] := 'aaa';
  FPickerListValues[1] := 'bbb';
  FPickerListValues[2] := 'ccc';

  TmyPicker.ListFree;
  TmyPicker.ListShow(FPickerListValues, btList, PickerListClick);
```

Native Date Picker
```
  TmyPicker.DateFree;
  TmyPicker.DateShow(btDate, StrToDateDef(lbDateValue.Text, now), PickerDateClick);
```

Native Time Picker
```
  TmyPicker.TimeFree;
  TmyPicker.TimeShow(btTime, now, PickerTimeClick);
```
