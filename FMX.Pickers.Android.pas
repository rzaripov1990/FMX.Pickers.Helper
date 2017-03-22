{ ******************************************************* }
{ }
{ Delphi FireMonkey Platform }
{ }
{ Copyright(c) 2013-2017 Embarcadero Technologies, Inc. }
{ All rights reserved }
{ }
{ ******************************************************* }

unit FMX.Pickers.Android;

interface

{$IFDEF ANDROID}
{$SCOPEDENUMS ON}
procedure RegisterPickersService;
procedure UnregisterPickersService;

{$ENDIF}

implementation

{$IFDEF ANDROID}

uses
  System.Classes, System.SysUtils, System.DateUtils, System.SyncObjs,
  System.Messaging, Androidapi.JNIBridge,
  Androidapi.JNI.App, Androidapi.JNI.Support, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Embarcadero, Androidapi.Helpers,
  FMX.Platform, FMX.Platform.Android, FMX.Pickers, FMX.Helpers.Android,
  FMX.Controls, FMX.Consts, FMX.Styles, FMX.Types,
  FMX.Forms;

type

  { TAndroidDateTimePicker }

  TDateTimeChangedListener = class;

  TOnDateTimeChangedCallback = procedure(const ADateTime: TDateTime) of object;
  TOnCallback = procedure of object;

  TAndroidDateTimePicker = class(TCustomDateTimePicker)
  strict private
    FNativeDatePicker: JBaseDateTimePicker;
    FNativeTimePicker: JBaseDateTimePicker;
    FDateTimeChangedListener: TDateTimeChangedListener;
  private
    function GetNativeDatePicker: JBaseDateTimePicker;
    function GetNativeTimePicker: JBaseDateTimePicker;
  protected
    procedure SetShowMode(const AValue: TDatePickerShowMode); override;
    procedure DoDateChanged(const ADateTime: TDateTime); override;
    property NativeTimePicker: JBaseDateTimePicker read GetNativeTimePicker;
    property NativeDatePicker: JBaseDateTimePicker read GetNativeDatePicker;
  public
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

  { TDateTimeChangedListener }

  TDateTimeChangedListener = class(TJavaLocal, JOnDateTimeChangedListener)
  strict private
    FChangedCallback: TOnDateTimeChangedCallback;
    FOpenCallback: TOnCallback;
    FCloseCallback: TOnCallback;
    FDateTime: TDateTime;
  protected
    procedure DoDateChanged;
    procedure DoShow;
    procedure DoHide;
  public
    constructor Create(AChangeCallback: TOnDateTimeChangedCallback;
      AOpenCallback, ACloseCallback: TOnCallback); overload;
    { JOnDateTimeChangedListener }
    procedure onDateChanged(date: JDate); cdecl;
    procedure onShow; cdecl;
    procedure onHide; cdecl;
  end;

  { TAndroidListPicker }

  TListChangedListener = class;

  TAndroidListPicker = class(TCustomListPicker)
  strict private
    FNativeListPicker: JBaseListPicker;
    FItemChangedListener: TListChangedListener;
    function GetNativeListPicker: JBaseListPicker;
  protected
    procedure SetValues(AValues: TStrings); override;
    procedure SetValuesToNativePicker(AValues: TStrings); virtual;
    property NativeListPicker: JBaseListPicker read GetNativeListPicker;
  public
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    function IsShown: Boolean; override;
  end;

  TOnItemChangedCallback = procedure(const AItemIndex: Integer) of object;

  { TListChangedListener }

  TListChangedListener = class(TJavaLocal, JOnItemChangedListener)
  strict private
    FChangedCallback: TOnItemChangedCallback;
    FOpenCallback: TOnCallback;
    FCloseCallback: TOnCallback;
    FItemIndex: Integer;
  protected
    procedure DoItemChanged;
    procedure DoShow;
    procedure DoHide;
  public
    constructor Create(AChangeCallback: TOnItemChangedCallback; AOpenCallback, ACloseCallback: TOnCallback);
    { JOnItemChangedListener }
    procedure onItemChanged(itemIndex: Integer); cdecl;
    procedure onShow; cdecl;
    procedure onHide; cdecl;
  end;

  { Picker Factory Service }

  TAndroidPickerService = class(TPickerFactoryService)
  private
    procedure FormShownHandler(const Sender: TObject; const M: TMessage);
  protected
    function DoCreateDateTimePicker: TCustomDateTimePicker; override;
    function DoCreateListPicker: TCustomListPicker; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  PickerService: TPickerFactoryService;

procedure RegisterPickersService;
begin
  PickerService := TAndroidPickerService.Create;
  TPlatformServices.Current.AddPlatformService(IFMXPickerService, PickerService);
end;

procedure UnregisterPickersService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXPickerService);
  PickerService := nil;
end;

function GetPickersFactory: JBasePickersFactory;
begin
  Result := TJBasePickersFactory.JavaClass.getFactory;
end;

{ TAndroidDateTimePicker }

destructor TAndroidDateTimePicker.Destroy;
begin
  FDateTimeChangedListener.DisposeOf;
  FNativeDatePicker := nil;
  FNativeTimePicker := nil;
  inherited;
end;

procedure TAndroidDateTimePicker.DoDateChanged(const ADateTime: TDateTime);
var
  DateTimeTmp: TDateTime;
begin
  // We need truncate Date or time part
  case ShowMode of
    TDatePickerShowMode.date:
      DateTimeTmp := DateOf(ADateTime);
    TDatePickerShowMode.Time:
      DateTimeTmp := TimeOf(ADateTime);
  else
    DateTimeTmp := ADateTime;
  end;

  inherited DoDateChanged(DateTimeTmp);
end;

function TAndroidDateTimePicker.GetNativeDatePicker: JBaseDateTimePicker;
var
  NativePickersFactory: JBasePickersFactory;
begin
  if FNativeDatePicker = nil then
  begin
    NativePickersFactory := GetPickersFactory;
    if NativePickersFactory <> nil then
    begin
      if FDateTimeChangedListener = nil then
        FDateTimeChangedListener := TDateTimeChangedListener.Create(DoDateChanged, DoShow, DoHide);
      FNativeDatePicker := NativePickersFactory.createDatePicker(MainActivity);
    end
    else
      raise EFeatureError.Create(SListPickerIsNotFound);
    FNativeDatePicker.setListener(FDateTimeChangedListener);
  end;
  Result := FNativeDatePicker;
end;

function TAndroidDateTimePicker.GetNativeTimePicker: JBaseDateTimePicker;
var
  NativePickersFactory: JBasePickersFactory;
begin
  if FNativeTimePicker = nil then
  begin
    NativePickersFactory := GetPickersFactory;
    if NativePickersFactory <> nil then
    begin
      if FDateTimeChangedListener = nil then
        FDateTimeChangedListener := TDateTimeChangedListener.Create(DoDateChanged, DoShow, DoHide);
      FNativeTimePicker := NativePickersFactory.createTimePicker(MainActivity);
    end
    else
      raise EFeatureError.Create(SDateTimePickerIsNotFound);
    FNativeTimePicker.setListener(FDateTimeChangedListener);
  end;
  Result := FNativeTimePicker;
end;

procedure TAndroidDateTimePicker.Hide;
begin
  // We don't check state of dialog directly by NativeDatePicker.isShown. Because if Picker doesn't have a native dialog,
  // NativeDatePicker will create native dialog. But when we try hide dialog, we don't need to create native dialog
  if (FNativeDatePicker <> nil) and NativeDatePicker.IsShown then
    NativeDatePicker.Hide;
  if (FNativeDatePicker <> nil) and NativeTimePicker.IsShown then
    NativeTimePicker.Hide;
end;

function TAndroidDateTimePicker.IsShown: Boolean;
begin
  case ShowMode of
    TDatePickerShowMode.date:
      Result := (FNativeDatePicker <> nil) and NativeDatePicker.IsShown;
    TDatePickerShowMode.Time:
      Result := (FNativeTimePicker <> nil) and NativeTimePicker.IsShown;
  else
    Result := False;
  end;
end;

procedure TAndroidDateTimePicker.SetShowMode(const AValue: TDatePickerShowMode);
begin
  if AValue = TDatePickerShowMode.DateTime then
    raise EFeatureError.Create(SDateTimePickerShowModeNotSupported)
  else
    inherited;
end;

procedure TAndroidDateTimePicker.Show;
begin
  inherited;
  case ShowMode of
    TDatePickerShowMode.date:
      begin
        // NativeDatePicker.setTheme(GetNativeTheme(Parent));     // ZuBy
        NativeDatePicker.setTheme(TJAlertDialog.JavaClass.THEME_DEVICE_DEFAULT_LIGHT); // ZuBy
        NativeDatePicker.setDate(DateTimeToUnix(Self.date) * 1000);
        NativeDatePicker.Show;
      end;
    TDatePickerShowMode.Time:
      begin
        // NativeTimePicker.setTheme(GetNativeTheme(Parent));          // ZuBy
        NativeTimePicker.setTheme(TJAlertDialog.JavaClass.THEME_DEVICE_DEFAULT_LIGHT); // ZuBy
        NativeTimePicker.setDate(DateTimeToUnix(Self.date) * 1000);
        NativeTimePicker.Show;
      end
  else
    raise EFeatureError.Create(SDateTimePickerShowModeNotSupported);
  end;
end;

{ TDateTimeChangedListener }

constructor TDateTimeChangedListener.Create(AChangeCallback: TOnDateTimeChangedCallback;
  AOpenCallback, ACloseCallback: TOnCallback);
begin
  inherited Create;
  FChangedCallback := AChangeCallback;
  FOpenCallback := AOpenCallback;
  FCloseCallback := ACloseCallback;
end;

procedure TDateTimeChangedListener.onHide;
begin
  // We got to this method in Java thread. So We need synchronize event handler in
  // another thread, Because ShowMessage and other thread dependend methods do
  // dead lock for application
  if FDateTime <> 0 then
    TThread.Queue(nil, DoDateChanged);
  TThread.Queue(nil, DoHide);
end;

procedure TDateTimeChangedListener.onShow;
begin
  FDateTime := 0;
  // ! See comments for TDateTimeChangedListener.onHide
  TThread.Queue(nil, DoShow);
end;

procedure TDateTimeChangedListener.DoDateChanged;
begin
  if Assigned(FChangedCallback) then
    FChangedCallback(FDateTime);
end;

procedure TDateTimeChangedListener.DoHide;
begin
  if Assigned(FCloseCallback) then
    FCloseCallback;
end;

procedure TDateTimeChangedListener.DoShow;
begin
  if Assigned(FOpenCallback) then
    FOpenCallback;
end;

procedure TDateTimeChangedListener.onDateChanged(date: JDate);
begin
  FDateTime := UnixToDateTime(Trunc(date.getTime / 1000));
end;

{ TAndroidListPicker }

destructor TAndroidListPicker.Destroy;
begin
  FNativeListPicker := nil;
  FItemChangedListener.DisposeOf;
  inherited;
end;

function TAndroidListPicker.GetNativeListPicker: JBaseListPicker;
var
  NativePickersFactory: JBasePickersFactory;
begin
  if FNativeListPicker = nil then
  begin
    NativePickersFactory := GetPickersFactory;
    if NativePickersFactory <> nil then
    begin
      FNativeListPicker := NativePickersFactory.createListPicker(MainActivity);
      FItemChangedListener := TListChangedListener.Create(DoItemChanged, DoShow, DoHide);
      FNativeListPicker.setListener(FItemChangedListener);
    end
    else
      raise EFeatureError.Create(SListPickerIsNotFound);
  end;
  Result := FNativeListPicker
end;

procedure TAndroidListPicker.Hide;
begin
  if IsShown then
    NativeListPicker.Hide;
end;

function TAndroidListPicker.IsShown: Boolean;
begin
  // We don't check state of dialog directly by NativeDatePicker.isShown. Because if Picker doesn't have a native dialog,
  // NativeDatePicker will create native dialog. But when we try hide dialog, we don't need to create native dialog
  Result := (FNativeListPicker <> nil) and NativeListPicker.IsShown;
end;

procedure TAndroidListPicker.SetValues(AValues: TStrings);
begin
  inherited;
  SetValuesToNativePicker(AValues);
end;

procedure TAndroidListPicker.SetValuesToNativePicker(AValues: TStrings);
var
  I: Integer;
  NativeArray: TJavaObjectArray<JCharSequence>;
begin
  NativeArray := TJavaObjectArray<JCharSequence>.Create(AValues.Count);
  for I := 0 to AValues.Count - 1 do
    NativeArray.SetRawItem(I, (StrToJCharSequence(AValues[I]) as ILocalObject).GetObjectID);

  NativeListPicker.setItems(NativeArray);
end;

procedure TAndroidListPicker.Show;
begin
  inherited;
  if Values.Count > 0 then
  begin
    SetValuesToNativePicker(Values);
    // NativeListPicker.setTheme(GetNativeTheme(Parent));   // ZuBy
    NativeListPicker.setTheme(TJAlertDialog.JavaClass.THEME_DEVICE_DEFAULT_LIGHT); // ZuBy
    NativeListPicker.setItemIndex(itemIndex);
    NativeListPicker.Show;
  end;
end;

{ TListChangedListener }

constructor TListChangedListener.Create(AChangeCallback: TOnItemChangedCallback;
  AOpenCallback, ACloseCallback: TOnCallback);
begin
  inherited Create;
  FChangedCallback := AChangeCallback;
  FOpenCallback := AOpenCallback;
  FCloseCallback := ACloseCallback;
end;

procedure TListChangedListener.DoItemChanged;
begin
  if Assigned(FChangedCallback) then
    FChangedCallback(FItemIndex);
end;

procedure TListChangedListener.DoShow;
begin
  if Assigned(FOpenCallback) then
    FOpenCallback;
end;

procedure TListChangedListener.DoHide;
begin
  if Assigned(FCloseCallback) then
    FCloseCallback;
end;

procedure TListChangedListener.onItemChanged(itemIndex: Integer);
begin
  FItemIndex := itemIndex;
end;

procedure TListChangedListener.onHide;
begin
  // We got to this method in Java thread. So We need synchronize event handler in
  // another thread, Because ShowMessage and other thread dependend methods do
  // dead lock for application
  if FItemIndex <> -1 then
    TThread.Queue(nil, DoItemChanged);
  TThread.Queue(nil, DoHide);
end;

procedure TListChangedListener.onShow;
begin
  // ! See comments for TListChangedListener.onHide
  FItemIndex := -1;
  TThread.Queue(nil, DoShow);
end;

{ TAndroidPickerService }

constructor TAndroidPickerService.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TFormBeforeShownMessage, FormShownHandler);
end;

destructor TAndroidPickerService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, FormShownHandler);
  inherited;
end;

function TAndroidPickerService.DoCreateDateTimePicker: TCustomDateTimePicker;
begin
  Result := TAndroidDateTimePicker.Create(Self);
end;

function TAndroidPickerService.DoCreateListPicker: TCustomListPicker;
begin
  Result := TAndroidListPicker.Create(Self);
end;

procedure TAndroidPickerService.FormShownHandler(const Sender: TObject; const M: TMessage);
begin
  CloseAllPickers;
end;

{$ENDIF}

end.
