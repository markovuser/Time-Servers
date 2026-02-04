Unit Unit_Base;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, ShellApi, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, WindowsDarkMode, IniFiles, Registry,
  Vcl.Samples.Spin, Vcl.Grids, Vcl.Buttons, Vcl.Menus, StrUtils, Translation,
  Vcl.Imaging.pngimage, IdComponent, IdBaseComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.JSON, IdSSLOpenSSL, Math, WinSvc, ShlObj,
  FileInfoUtils;

Type
  TimeThread = Class(TThread)
  Private
  Protected
    Procedure Execute; Override;
  End;

Type
  TForm1 = Class(TForm)
    TabControlTitle: TTabControl;
    TabControlBody: TTabControl;
    GroupBoxTimeServer: TGroupBox;
    ComboBoxTimeServer: TComboBox;
    ButtonUpdateTime: TButton;
    StatusBarTmeServer: TStatusBar;
    GroupBoxServerList: TGroupBox;
    MemoSereverList: TMemo;
    SpeedButton_GeneralMenu: TSpeedButton;
    PopupMenu_General: TPopupMenu;
    ThemeMenu: TMenuItem;
    ThemeAuto: TMenuItem;
    ThemeLight: TMenuItem;
    ThemeDark: TMenuItem;
    N3: TMenuItem;
    CheckUpdateMenu: TMenuItem;
    N1: TMenuItem;
    AboutMenu: TMenuItem;
    GroupBox1: TGroupBox;
    ButtonServerToRegistry: TButton;
    ButtonServerDelete: TButton;
    ButtonServerReset: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    IdHTTP2: TIdHTTP;
    PopupMenuLanguage: TPopupMenu;
    LangNoUpdate: TMenuItem;
    LangUpdateAvailable: TMenuItem;
    LangOpenWebSite: TMenuItem;
    LangOnlyWindows: TMenuItem;
    LangError: TMenuItem;
    GroupBoxSynchronization: TGroupBox;
    CheckBoxAutoSynchronization: TCheckBox;
    LanguageMenu: TMenuItem;
    ProgressBar1: TProgressBar;
    Procedure FormCreate(Sender: TObject);
    Procedure LoadNastr;
    Procedure SaveNastr;
    Procedure UnCheckTheme;
    Procedure Globload;
    Procedure CleanTranslationsLikeGlobload;
    Procedure LoadLanguage;
    Procedure SaveTimeServersToRegistry(MemoTimeServer: TMemo);
    Procedure LoadTimeServersFromRegistry(ComboBoxTimeServer: TComboBox);
    Procedure ClearTimeServersFromRegistry;
    Function TicksToDateTime(Const Ticks: Int64): TDateTime;
    Procedure LastTimeUpdate;
    Procedure ResetTimeService;
    Function IsWindows10Or11: Boolean;
    Function PortablePath: String;
    Function PortablePathServer: String;
    Function ReadCurrentTimeSyncState: Boolean;
    Procedure UpdateTimeSyncSettings(Enabled: Boolean);
    Function RunAsAdmin(Const FileName, Parameters: String): Boolean;
    Function ExecuteCommand(Const Command: String): Boolean;
    Procedure SetTimeServerViaRegistry(Const Server: String);
    Function GetCurrentTimeServer: String;
    Procedure ListFileDir(Path, Ext: String; aCombobox: TComboBox);
    Procedure LabelLastUpdateTimeClick(Sender: TObject);
    Procedure LabelCurrentServerClick(Sender: TObject);
    Procedure ButtonUpdateTimeClick(Sender: TObject);
    Procedure ButtonServerToRegistryClick(Sender: TObject);
    Procedure ButtonServerDeleteClick(Sender: TObject);
    Procedure ButtonServerResetClick(Sender: TObject);
    Procedure A2Show(Sender: TObject);
    Procedure SpeedButton_GeneralMenuClick(Sender: TObject);
    Procedure ThemeAutoClick(Sender: TObject);
    Procedure ThemeLightClick(Sender: TObject);
    Procedure ThemeDarkClick(Sender: TObject);
    Procedure CheckUpdateMenuClick(Sender: TObject);
    Procedure AboutMenuClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Timer2Timer(Sender: TObject);
    Procedure ImageTitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure MemoSereverListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure GroupBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure GroupBoxTimeServerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure CheckBoxAutoSynchronizationClick(Sender: TObject);
    Procedure RestartTimeService;
  Private
    { Private declarations }
    Procedure WMSettingChange(Var Message: TWMSettingChange); Message WM_SETTINGCHANGE;
    Procedure LanguageMenuItemClick(Sender: TObject);
  Public
    { Public declarations }
  Protected
  End;

Var
  Form1: TForm1;
  i: Int64;
  LastSync: TDateTime;
  version: Integer;
  portable: Boolean;
  Thread: TimeThread;
  LangCode: String;
  LangLocal: String;

Const
  ServerName = 'Time-Servers';
  ApiGithub = 'https://api.github.com/repos/markovuser/' + ServerName + '/releases/latest';

Implementation

{$R *.dfm}

Uses
  Unit_Update, Unit_About;

Function TForm1.IsWindows10Or11: Boolean;
Begin
  Result := (TOSVersion.Major = 10) And (TOSVersion.Build >= 10240);
End;

Procedure TForm1.WMSettingChange(Var Message: TWMSettingChange);
Begin

  If ThemeAuto.Checked = true Then
  Begin
    Try
      If DarkModeIsEnabled = true Then
      Begin
        SetSpecificThemeMode(true, 'Windows11 Modern Dark', 'Windows11 Modern Light');
      End;

      If DarkModeIsEnabled = False Then
      Begin
        SetSpecificThemeMode(true, 'Windows11 Modern Light', 'Windows11 Modern Dark');
      End;
    Except
    End;
    Application.ProcessMessages;
  End;
  Application.ProcessMessages;
End;

Function ExtractOnlyFileName(Const FileName: String): String;
Begin
  Try
    Result := StringReplace(ExtractFileName(FileName), ExtractFileExt(FileName), '', []);
  Except
  End;
End;

Procedure TForm1.ListFileDir(Path, Ext: String; aCombobox: TComboBox);
Var
  SR: TSearchRec;
  Ini: TMemIniFile;
  LangName: String;
Begin
  If FindFirst(Path + Ext, faAnyFile, SR) = 0 Then
  Begin
    Repeat
      If (SR.Attr <> faDirectory) Then
      Begin
        Ini := TMemIniFile.Create(Path + SR.Name);
        LangName := Ini.ReadString('Language information', 'LANGNAME', '');
        // FormattedText :=Format('%-35s%s', [ '[ '+LangName+ ' ]',ExtractOnlyFileName(SR.Name)]) ;
        // aCombobox.Items.Add(FormattedText);
        aCombobox.Items.Add(ExtractOnlyFileName(SR.Name) + ' (' + LangName + ')');
        Application.ProcessMessages;
        Ini.Free;
      End;
    Until FindNext(SR) <> 0;
    FindClose(SR);
    Application.ProcessMessages;

  End;
End;

Procedure TForm1.A2Show(Sender: TObject);
Begin
  LoadTimeServersFromRegistry(ComboBoxTimeServer);
End;

Procedure TForm1.SetTimeServerViaRegistry(Const Server: String);
Var
  Reg: TRegistry;
  i, MaxIndex: Integer;
  ServerList: TStringList;
  ServerExists: Boolean;
Begin
  If Server = '' Then
    Exit;

  Reg := TRegistry.Create(KEY_READ Or KEY_WRITE Or KEY_WOW64_64KEY);
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    If Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime\Servers', true) Then
    Begin
      ServerList := TStringList.Create;
      Try
        Reg.GetValueNames(ServerList);
        MaxIndex := 0;
        ServerExists := False;

        // Проверяем, существует ли сервер в списке и находим максимальный индекс
        For i := 0 To ServerList.Count - 1 Do
        Begin
          If (ServerList[i] <> '') Then
          Begin
            // Проверяем, совпадает ли значение с нашим сервером
            If Reg.ReadString(ServerList[i]) = Server Then
              ServerExists := true;

            // Находим максимальный индекс
            If StrToIntDef(ServerList[i], -1) > MaxIndex Then
              MaxIndex := StrToIntDef(ServerList[i], -1);
          End;
        End;

        // Добавляем сервер только если его нет в списке
        If Not ServerExists Then
        Begin
          Reg.WriteString(IntToStr(MaxIndex + 1), Server);
          Reg.WriteString('', IntToStr(MaxIndex + 1));
        End
        Else
        Begin
          // Если сервер уже есть, устанавливаем его как текущий
          For i := 0 To ServerList.Count - 1 Do
          Begin
            If (ServerList[i] <> '') And (Reg.ReadString(ServerList[i]) = Server) Then
            Begin
              Reg.WriteString('', ServerList[i]);
              Break;
            End;
          End;
        End;

      Finally
        ServerList.Free;
      End;
      Reg.CloseKey;
    End;

    // Остальная часть кода остается без изменений

    If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Parameters', False) Then
    Begin
      Reg.WriteString('NtpServer', Server + ',0x9');
      Reg.CloseKey;
    End;

    If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Config', False) Then
    Begin
      Reg.WriteInteger('AnnounceFlags', 5);
      Reg.CloseKey;
    End;

  Finally
    Reg.Free;
  End;
End;

Procedure TForm1.AboutMenuClick(Sender: TObject);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
  Form8.ShowModal;
End;

Procedure TForm1.ButtonServerDeleteClick(Sender: TObject);
Begin
  ClearTimeServersFromRegistry;
  LoadTimeServersFromRegistry(ComboBoxTimeServer);
End;

Procedure TForm1.ButtonServerResetClick(Sender: TObject);
Begin
  MemoSereverList.Lines.BeginUpdate;
  Try
    MemoSereverList.Lines.Clear;
    MemoSereverList.Lines.Add('timeserver.ru');
    MemoSereverList.Lines.Add('ntp1.stratum2.ru');
    MemoSereverList.Lines.Add('ntp2.stratum2.ru');
    MemoSereverList.Lines.Add('ntp3.stratum2.ru');
    MemoSereverList.Lines.Add('time.windows.com');
    MemoSereverList.Lines.Add('time.nist.gov');
    MemoSereverList.Lines.Add('europe.pool.ntp.org');
    MemoSereverList.Lines.Add('pool.ntp.org');
    MemoSereverList.Lines.Add('time.google.com');
    MemoSereverList.Lines.Add('time.apple.com');
    MemoSereverList.Lines.Add('time.cloudflare.com');
    MemoSereverList.Lines.Add('ntp.ubuntu.com');
    MemoSereverList.Lines.Add('de.pool.ntp.org');
    MemoSereverList.Lines.Add('us.pool.ntp.org');
    MemoSereverList.Lines.Add('asia.pool.ntp.org');
  Finally
    MemoSereverList.Lines.EndUpdate;
  End;
End;

Procedure TForm1.ButtonServerToRegistryClick(Sender: TObject);
Begin
  ClearTimeServersFromRegistry;
  SaveTimeServersToRegistry(MemoSereverList);
  LoadTimeServersFromRegistry(ComboBoxTimeServer);
End;

Procedure TForm1.SaveTimeServersToRegistry(MemoTimeServer: TMemo);
Var
  Reg: TRegistry;
  i: Integer;
Begin
  If Not Assigned(MemoTimeServer) Or (MemoTimeServer.Lines.Count = 0) Then
    Exit;

  // Универсальный доступ для 32/64 бит
  Reg := TRegistry.Create(KEY_WRITE Or KEY_WOW64_64KEY);
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    If Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime\Servers', True) Then
    Begin
      Try
        // Записываем количество серверов
        Reg.WriteInteger('', MemoTimeServer.Lines.Count);

        // Записываем серверы
        For i := 0 To MemoTimeServer.Lines.Count - 1 Do
        Begin
          Reg.WriteString(IntToStr(i), Trim(MemoTimeServer.Lines[i]));
        End;

      Except
        On E: Exception Do
        Begin
          // Логирование ошибки
          OutputDebugString(PChar('SaveTimeServers Error: ' + E.Message));
        End;
      End;
      Reg.CloseKey;
    End;

  Finally
    Reg.Free;
  End;
End;

Function TForm1.RunAsAdmin(Const FileName, Parameters: String): Boolean;
Var
  Info: TShellExecuteInfo;
Begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  Info.fMask := SEE_MASK_NOCLOSEPROCESS;
  Info.lpFile := pchar(FileName);
  Info.lpParameters := pchar(Parameters);
  Info.nShow := SW_HIDE;
  Info.lpVerb := 'runas'; // Запуск с правами администратора

  Result := ShellExecuteEx(@Info);
  If Result Then
  Begin
    WaitForSingleObject(Info.hProcess, INFINITE);
    CloseHandle(Info.hProcess);
  End;
End;

Procedure TForm1.ResetTimeService;
Var
  NtpServers: String;
Begin
  TabControlBody.Enabled := false;
  TabControlTitle.Enabled := false;
  // Подготавливаем прогресс-бар
  ProgressBar1.Min := 0;
  ProgressBar1.Max := 7; // 8 шагов, но отсчет с 0
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;
  Application.ProcessMessages;

  // Определяем серверы
  If ComboBoxTimeServer.Text <> '' Then
    NtpServers := ComboBoxTimeServer.Text
  Else
    NtpServers := 'time.windows.com,0x1';

  Try
    // Шаг 1: Остановка службы
    RunAsAdmin('net.exe', 'stop w32time');
    ProgressBar1.Position := 1;
    Application.ProcessMessages;
    Sleep(2000);

    // Шаг 2: Отмена регистрации
    RunAsAdmin('w32tm.exe', '/unregister');
    ProgressBar1.Position := 2;
    Application.ProcessMessages;
    Sleep(1000);

    // Шаг 3: Регистрация
    RunAsAdmin('w32tm.exe', '/register');
    ProgressBar1.Position := 3;
    Application.ProcessMessages;
    Sleep(1000);

    // Шаг 4: Запуск службы
    RunAsAdmin('net.exe', 'start w32time');
    ProgressBar1.Position := 4;
    Application.ProcessMessages;
    Sleep(3000);

    // Шаг 5: Настройка серверов
    RunAsAdmin('w32tm.exe', '/config /syncfromflags:manual /manualpeerlist:"' + NtpServers + '"');
    ProgressBar1.Position := 5;
    Application.ProcessMessages;
    Sleep(2000);

    // Шаг 6: Обновление конфигурации
    RunAsAdmin('w32tm.exe', '/config /update');
    ProgressBar1.Position := 6;
    Application.ProcessMessages;
    Sleep(1000);

    // Шаг 7: Синхронизация
    RunAsAdmin('w32tm.exe', '/resync');
    ProgressBar1.Position := 7;
    Application.ProcessMessages;
    Sleep(1000);

    // Шаг 8: Проверка статуса
    RunAsAdmin('w32tm.exe', '/query /status');
    ProgressBar1.Position := 8;
    Application.ProcessMessages;

  Finally
    // Возвращаемся к исходному состоянию
    Sleep(1000);
  End;
  TabControlBody.Enabled := true;
  TabControlTitle.Enabled := true;
End;

Procedure TimeThread.Execute;
Begin
  Thread.FreeOnTerminate := true;
  Form1.ResetTimeService;
End;

Procedure TForm1.ButtonUpdateTimeClick(Sender: TObject);
Begin
  If ComboBoxTimeServer.Text <> '' Then
    SetTimeServerViaRegistry(ComboBoxTimeServer.Text);
  StatusBarTmeServer.Panels[0].Text := GetCurrentTimeServer;

  Thread := TimeThread.Create(False);
  Thread.FreeOnTerminate := true;
  Thread.Priority := tpNormal;
  // Thread.Start;
  Application.ProcessMessages;
  LastTimeUpdate;
  Application.ProcessMessages;
End;

Procedure TForm1.LoadTimeServersFromRegistry(ComboBoxTimeServer: TComboBox);
Var
  Reg: TRegistry;
  i: Integer;
  ServerName: String;
  DefaultIndex: Integer;
Begin
  If Not Assigned(ComboBoxTimeServer) Then
    Exit;

  ComboBoxTimeServer.Items.Clear;

  Reg := TRegistry.Create(KEY_READ Or KEY_WOW64_64KEY);
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    If Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime\Servers', False) Then
    Begin
      Try
        Try
          DefaultIndex := StrToInt(Reg.ReadString(''));
        Except
          DefaultIndex := 0;
        End;
        i := 0;
        While Reg.ValueExists(IntToStr(i)) Do
        Begin
          ServerName := Reg.ReadString(IntToStr(i));
          If ServerName <> '' Then
            ComboBoxTimeServer.Items.Add(ServerName);
          Inc(i);
        End;
        If (DefaultIndex >= 0) And (DefaultIndex < ComboBoxTimeServer.Items.Count) Then
          ComboBoxTimeServer.ItemIndex := DefaultIndex
        Else If ComboBoxTimeServer.Items.Count > 0 Then
          ComboBoxTimeServer.ItemIndex := 0;
      Except
      End;
      Reg.CloseKey;
    End
  Finally
    Reg.Free;
  End;
End;

Procedure TForm1.MemoSereverListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
End;

Procedure TForm1.CheckUpdateMenuClick(Sender: TObject);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
  Form10.ShowModal;
End;

Procedure TForm1.ClearTimeServersFromRegistry;
Var
  Reg: TRegistry;
  AccessFlags: Cardinal;
Begin
  // Определяем флаги доступа в зависимости от архитектуры
  {$IFDEF WIN64}
  AccessFlags := KEY_WRITE;
  {$ELSE}
  AccessFlags := KEY_WRITE Or KEY_WOW64_64KEY;
  {$ENDIF}

  Reg := TRegistry.Create(AccessFlags);
  Try
    Try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      // 1. Удаляем раздел с серверами времени
      If Reg.KeyExists('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime\Servers') Then
      Begin
        // Вместо DeleteKey можно использовать DeleteKeyIncludingSubkeys для полного удаления
        Reg.DeleteKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime\Servers');
      End;

      // 2. Также очищаем NTP серверы в разделе службы времени
      If Reg.KeyExists('SYSTEM\CurrentControlSet\Services\W32Time\Parameters') Then
      Begin
        If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Parameters', True) Then
        Begin
          // Удаляем параметры NTP серверов если они существуют
          If Reg.ValueExists('NtpServer') Then
            Reg.DeleteValue('NtpServer');

          If Reg.ValueExists('Type') Then
            Reg.WriteString('Type', 'NoSync'); // Устанавливаем тип "без синхронизации"

          Reg.CloseKey;
        End;
      End;

      // 3. Очищаем настройки клиента NTP
      If Reg.KeyExists('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient') Then
      Begin
        If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient', True) Then
        Begin
          // Устанавливаем стандартный NTP сервер Windows (можно изменить)
          Reg.WriteString('NtpServer', 'time.windows.com,0x1');
          Reg.CloseKey;
        End;
      End;

      // 4. После изменений перезапускаем службу времени
      // Это можно сделать через отдельную процедуру
      RestartTimeService;

    Except
    End;

  Finally
    Reg.Free;
  End;
End;

Function TForm1.ExecuteCommand(Const Command: String): Boolean;
Var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
Begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  Result := CreateProcess(Nil, pchar('cmd.exe /C ' + Command), Nil, Nil, False, CREATE_NEW_CONSOLE Or NORMAL_PRIORITY_CLASS, Nil, Nil, StartupInfo, ProcessInfo);

  If Result Then
  Begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  End;
End;

Procedure TForm1.LoadNastr;
Var
  Ini: TMemIniFile;
  i: Integer;
Begin

  Ini := TMemIniFile.Create(Form1.PortablePath);
  For i := 0 To ThemeMenu.Count - 1 Do
  Begin
    ThemeMenu.Items[i].Checked := Ini.ReadBool('Option', ThemeMenu.Items[i].Name, False);
    If ThemeMenu.Items[i].Checked = true Then
    Begin
      ThemeMenu.Items[i].Click;
    End;
  End;
  Ini.Free;
End;

Function GetAppDataRoamingPath: String;
Var
  Path: Array[0..MAX_PATH] Of Char;
Begin
  If SUCCEEDED(SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @Path[0])) Then
    Result := IncludeTrailingPathDelimiter(Path)
  Else
    Result := '';
End;

Function TForm1.PortablePath: String;
Begin
  If portable Then
    Result := ExtractFilePath(ParamStr(0)) + 'Config\Option.ini'
  Else
    Result := IncludeTrailingPathDelimiter(GetAppDataRoamingPath) + IncludeTrailingPathDelimiter(getCompanyName(ParamStr(0))) + Application.Title + '\Config\Option.ini';
    //Result := ExtractFilePath(ParamStr(0)) + 'Config\Option.ini';
  // Создаем папку для конфига
  ForceDirectories(ExtractFilePath(Result));
End;

Function TForm1.PortablePathServer: String;
Begin
  If portable Then
    Result := ExtractFilePath(ParamStr(0)) + 'Config\Time Servers.ini'
  Else
    Result := IncludeTrailingPathDelimiter(GetAppDataRoamingPath) + IncludeTrailingPathDelimiter(getCompanyName(ParamStr(0))) + Application.Title + '\Config\Time Servers.ini';
    //Result := ExtractFilePath(ParamStr(0)) + 'Config\Option.ini';
  // Создаем папку для конфига
  ForceDirectories(ExtractFilePath(Result));
End;

Procedure TForm1.SaveNastr;
Var
  Ini: TMemIniFile;
  i: Integer;
Begin
  Ini := TMemIniFile.Create(Form1.PortablePath);

  Ini.WriteBool('Option', Form10.CheckBoxQuickUpdate.Name, Form10.CheckBoxQuickUpdate.Checked);

  For i := 0 To ThemeMenu.Count - 1 Do
  Begin
    Ini.WriteBool('Option', ThemeMenu.Items[i].Name, ThemeMenu.Items[i].Checked);
  End;
  Ini.UpdateFile;
  Ini.Free;
End;

Procedure TForm1.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  Form1.SaveNastr;
  Try
    Form1.MemoSereverList.Lines.SaveToFile(PortablePathServer, TEncoding.Unicode);
    Application.ProcessMessages;
  Except
  End;
End;

Function TForm1.ReadCurrentTimeSyncState: Boolean;
Var
  Reg: TRegistry;
  AutoSetTime, NtpClientEnabled: Integer;
  TypeStr: String;
Begin
  // Инициализация переменных по умолчанию
  AutoSetTime := 0;
  NtpClientEnabled := 0;
  TypeStr := '';

  Reg := TRegistry.Create;
  Try
    // Устанавливаем доступ к 64-битному реестру
    // Это работает как для 32-битных, так и для 64-битных программ
    Reg.Access := KEY_READ Or KEY_WOW64_64KEY;

    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // Проверяем основную настройку автоматической синхронизации
    If Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime') Then
    Begin
      If Reg.ValueExists('AutoSetTime') Then
        AutoSetTime := Reg.ReadInteger('AutoSetTime')
      Else
        AutoSetTime := 0;
      Reg.CloseKey;
    End;

    // Проверяем тип синхронизации
    If Reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\W32Time\Parameters') Then
    Begin
      If Reg.ValueExists('Type') Then
        TypeStr := Reg.ReadString('Type')
      Else
        TypeStr := '';
      Reg.CloseKey;
    End;

    // Проверяем состояние NTP клиента
    If Reg.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient') Then
    Begin
      If Reg.ValueExists('Enabled') Then
        NtpClientEnabled := Reg.ReadInteger('Enabled')
      Else
        NtpClientEnabled := 0;
      Reg.CloseKey;
    End;

    // Считаем, что синхронизация включена если основные параметры установлены
    Result := (AutoSetTime = 1) And (TypeStr = 'NTP') And (NtpClientEnabled = 1);

  Except
    On E: Exception Do
    Begin
      // Логирование ошибки
      OutputDebugString(PChar('Ошибка чтения реестра: ' + E.Message));
      Result := False;
    End;
  End;

  Reg.Free;
End;

Procedure TForm1.RestartTimeService;
Var
  ShellExecuteInfo: TShellExecuteInfo;
Begin
  // Для применения изменений перезапускаем службу времени
  FillChar(ShellExecuteInfo, SizeOf(ShellExecuteInfo), 0);
  ShellExecuteInfo.cbSize := SizeOf(ShellExecuteInfo);
  ShellExecuteInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  ShellExecuteInfo.lpVerb := 'runas'; // Запуск с правами администратора
  ShellExecuteInfo.lpFile := 'net';
  ShellExecuteInfo.lpParameters := 'stop w32time && net start w32time';
  ShellExecuteInfo.nShow := SW_HIDE;

  If ShellExecuteEx(@ShellExecuteInfo) Then
    WaitForSingleObject(ShellExecuteInfo.hProcess, INFINITE);
End;

Procedure TForm1.UpdateTimeSyncSettings(Enabled: Boolean);
Var
  Reg: TRegistry;
Begin
  Reg := TRegistry.Create;
  Try
    // Устанавливаем доступ к 64-битному реестру
    // Это работает как для 32-битных, так и для 64-битных программ
    Reg.Access := KEY_WRITE Or KEY_WOW64_64KEY;

    Reg.RootKey := HKEY_LOCAL_MACHINE;

    If Enabled Then
    Begin
      // ВКЛЮЧИТЬ все настройки синхронизации

      // Основная настройка автоматической синхронизации
      If Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime', true) Then
      Begin
        Reg.WriteInteger('AutoSetTime', 1); // 1 = включено
        Reg.CloseKey;
      End;

      // Тип синхронизации - NTP
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Parameters', true) Then
      Begin
        Reg.WriteString('Type', 'NTP');
        Reg.CloseKey;
      End;

      // Включить NTP клиент
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient', true) Then
      Begin
        Reg.WriteInteger('Enabled', 1);
        Reg.CloseKey;
      End;

      // Включить NTP сервер (опционально)
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpServer', true) Then
      Begin
        Reg.WriteInteger('Enabled', 1);
        Reg.CloseKey;
      End;

    End
    Else
    Begin
      // ВЫКЛЮЧИТЬ все настройки синхронизации

      // Основная настройка автоматической синхронизации
      If Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\DateTime', true) Then
      Begin
        Reg.WriteInteger('AutoSetTime', 0); // 0 = выключено
        Reg.CloseKey;
      End;

      // Тип синхронизации - NoSync (без синхронизации)
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Parameters', true) Then
      Begin
        Reg.WriteString('Type', 'NoSync');
        Reg.CloseKey;
      End;

      // Выключить NTP клиент
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpClient', true) Then
      Begin
        Reg.WriteInteger('Enabled', 0);
        Reg.CloseKey;
      End;

      // Выключить NTP сервер
      If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\TimeProviders\NtpServer', true) Then
      Begin
        Reg.WriteInteger('Enabled', 0);
        Reg.CloseKey;
      End;

    End;

    // После изменения настроек реестра, перезапускаем службу времени
    RestartTimeService;

  Except
  End;

  Reg.Free;
End;

Function SetTimeServiceAutoStart: Boolean;
Const
  SERVICE_NAME = 'W32Time'; // Имя службы времени Windows
Var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  ServiceStatus: SERVICE_STATUS;
  dwStartType: DWORD;
Begin
  Result := False;
  SCManager := OpenSCManager(Nil, Nil, SC_MANAGER_ALL_ACCESS);
  If SCManager = 0 Then
    Exit;

  Try
    Service := OpenService(SCManager, pchar(SERVICE_NAME), SERVICE_ALL_ACCESS);
    If Service = 0 Then
      Exit;

    Try
      // Получаем текущий статус службы
      If Not QueryServiceStatus(Service, ServiceStatus) Then
        Exit;

      // Устанавливаем автостарт
      dwStartType := SERVICE_AUTO_START;
      If Not ChangeServiceConfig(Service, SERVICE_NO_CHANGE,
        // Тип службы не меняем
        dwStartType, // Тип запуска
        SERVICE_NO_CHANGE, // Тип управления ошибками не меняем
        Nil, // Путь к бинарному файлу не меняем
        Nil, // Группа загрузки не меняем
        Nil, // Tag ID не меняем
        Nil, // Зависимости не меняем
        Nil, // Имя учетной записи не меняем
        Nil, // Пароль не меняем
        Nil // Имя дисплея не меняем
      ) Then
        Exit;

      // Запускаем службу, если она не работает
      If ServiceStatus.dwCurrentState <> SERVICE_RUNNING Then
      Begin
        Var Args: pchar := Nil;
        If Not StartService(Service, 0, Args) Then
        Begin
          If GetLastError() <> ERROR_SERVICE_ALREADY_RUNNING Then
            Exit;
        End;
      End;

      Result := true;
    Finally
      CloseServiceHandle(Service);
    End;
  Finally
    CloseServiceHandle(SCManager);
  End;
End;

Function DisableTimeService: Boolean;
Const
  SERVICE_NAME = 'W32Time'; // Имя службы времени Windows
Var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  ServiceStatus: TServiceStatus;
  dwStartType: DWORD;
Begin
  Result := False;
  SCManager := OpenSCManager(Nil, Nil, SC_MANAGER_ALL_ACCESS);
  If SCManager = 0 Then
    Exit;

  Try
    Service := OpenService(SCManager, pchar(SERVICE_NAME), SERVICE_ALL_ACCESS);
    If Service = 0 Then
      Exit;

    Try
      // Получаем текущий статус службы
      If Not QueryServiceStatus(Service, ServiceStatus) Then
        Exit;

      // Останавливаем службу, если она запущена
      If ServiceStatus.dwCurrentState = SERVICE_RUNNING Then
      Begin
        If Not ControlService(Service, SERVICE_CONTROL_STOP, ServiceStatus) Then
          Exit;

        // Ждем завершения остановки (опционально)
        Sleep(1000);
        While QueryServiceStatus(Service, ServiceStatus) And (ServiceStatus.dwCurrentState = SERVICE_STOP_PENDING) Do
        Begin
          Sleep(100);
        End;
      End;

      // Устанавливаем тип запуска "Отключено"
      dwStartType := SERVICE_DEMAND_START;
      If Not ChangeServiceConfig(Service, SERVICE_NO_CHANGE, dwStartType, SERVICE_NO_CHANGE, Nil, Nil, Nil, Nil, Nil, Nil, Nil) Then
        Exit;

      Result := true;
    Finally
      CloseServiceHandle(Service);
    End;
  Finally
    CloseServiceHandle(SCManager);
  End;
End;

Procedure TForm1.CheckBoxAutoSynchronizationClick(Sender: TObject);
Begin
  UpdateTimeSyncSettings(CheckBoxAutoSynchronization.Checked);
  If CheckBoxAutoSynchronization.Checked Then
  Begin
    SetTimeServiceAutoStart;
  End;
  If CheckBoxAutoSynchronization.Checked = False Then
  Begin
    DisableTimeService;
  End;

End;

Procedure TForm1.LoadLanguage;
Var
  Ini: TMemIniFile;
  LangFiles: TStringList;
  i: Integer;
  MenuItem: TMenuItem;
  FileName, DisplayName, MenuCaption: String;
  SearchRec: TSearchRec;
Begin
  Ini := TMemIniFile.Create(PortablePath);
  While LanguageMenu.Count > 0 Do
    LanguageMenu.Items[0].Free;
  LangLocal := Ini.ReadString('Language', 'Language', '');
  Ini.Free;

  LangFiles := TStringList.Create;
  Try
    If FindFirst(ExtractFilePath(ParamStr(0)) + 'Language\*.ini', faAnyFile, SearchRec) = 0 Then
    Begin
      Repeat
        If (SearchRec.Name <> '.') And (SearchRec.Name <> '..') Then
          LangFiles.Add(SearchRec.Name);
      Until FindNext(SearchRec) <> 0;
      FindClose(SearchRec);
    End;
    LangFiles.Sort;
    For i := 0 To LangFiles.Count - 1 Do
    Begin
      FileName := LangFiles[i];
      LangCode := ChangeFileExt(FileName, '');
      DisplayName := LangCode;
      Try
        Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Language\' + FileName);
        Try
          DisplayName := Ini.ReadString('Language information', 'LANGNAME', LangCode);
        Finally
          Ini.Free;
        End;
      Except
      End;
      MenuCaption := LangCode + #9 + #9 + DisplayName;
      MenuItem := TMenuItem.Create(LanguageMenu);
      MenuItem.RadioItem := true;
      MenuItem.Caption := MenuCaption;
      MenuItem.AutoHotkeys := maManual;
      MenuItem.AutoCheck := True;
      If (LangCode = LangLocal) Or (SameText(LangCode, LangLocal)) Or (LangCode + '.ini' = LangLocal) Then
        MenuItem.Checked := True;
      MenuItem.OnClick := LanguageMenuItemClick;
      LanguageMenu.Add(MenuItem);
    End;

  Finally
    LangFiles.Free;
  End;
End;

Procedure TForm1.LanguageMenuItemClick(Sender: TObject);
Var
  MenuItem: TMenuItem;
  Ini: TMemIniFile;
  i: Integer;
Begin
  If Sender Is TMenuItem Then
  Begin
    MenuItem := TMenuItem(Sender);
    LangCode := Copy(MenuItem.Caption, 1, Pos(#9, MenuItem.Caption) - 1);
    LangLocal := LangCode;
    For i := 0 To LanguageMenu.Count - 1 Do
      LanguageMenu.Items[i].Checked := False;
    MenuItem.Checked := True;
    Ini := TMemIniFile.Create(PortablePath);
    Try
      Ini.WriteString('Language', 'Language', LangLocal);
      Ini.UpdateFile;
    Finally
      Ini.Free;
    End;
    LoadLanguage;
    Form1.Globload;

  End;
End;

Procedure TForm1.CleanTranslationsLikeGlobload;
Var
  i, j, k, m: Integer;
  Ini: TMemIniFile;
  Sections, Keys: TStringList;
  SearchRec: TSearchRec;
  FindResult: Integer;
  CompPath, FormName, CompName, PropName: String;
  FirstDot, SecondDot: Integer;
  FormExists, CompExists: Boolean;
  CurrentForm: TForm;
  CurrentComponent: TComponent;
  Modified: Boolean;
  IsDuplicate: Boolean;
  n: Integer;
  CompareKey, CompareFormName: String;
  CompareDotPos: Integer;
Begin
  // Создаем все формы проекта (если нужно)
  // CreateAllProjectForms;

  FindResult := FindFirst(ExtractFilePath(ParamStr(0)) + 'Language\*.ini', faAnyFile, SearchRec);
  If FindResult = 0 Then
  Begin
    Repeat
      If (SearchRec.Name <> '.') And (SearchRec.Name <> '..') Then
      Begin
        Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Language\' + SearchRec.Name);
        Sections := TStringList.Create;
        Keys := TStringList.Create;
        Modified := False;

        Try
          Ini.ReadSections(Sections);

          For i := 0 To Sections.Count - 1 Do
          Begin
            // ========== ИСКЛЮЧАЕМ ЭТИ СЕКЦИИ ИЗ ОБРАБОТКИ ==========
            If SameText(Sections[i], 'Language information') Or SameText(Sections[i], 'DestDir') Then
              Continue; // Пропускаем эти секции полностью

            Ini.ReadSection(Sections[i], Keys);

            // Проходим по всем ключам в обратном порядке
            For j := Keys.Count - 1 Downto 0 Do
            Begin
              CompPath := Keys[j];
              FirstDot := Pos('.', CompPath);

              If FirstDot > 0 Then
              Begin
                FormName := Copy(CompPath, 1, FirstDot - 1);
                FormExists := False;
                CompExists := False;

                // ==================== ПРОВЕРКА СУЩЕСТВОВАНИЯ КОМПОНЕНТА ====================
                // Проверяем ВСЕ формы в Screen
                For k := 0 To Screen.FormCount - 1 Do
                Begin
                  If SameText(Screen.Forms[k].Name, FormName) Then
                  Begin
                    FormExists := True;
                    CurrentForm := Screen.Forms[k];

                    // Извлекаем остаток пути после имени формы
                    CompName := Copy(CompPath, FirstDot + 1, Length(CompPath));
                    SecondDot := Pos('.', CompName);

                    If SecondDot > 0 Then
                    Begin
                      // Есть вложенный компонент: Form1.TrayIcon1.Hint
                      PropName := Copy(CompName, SecondDot + 1, Length(CompName));
                      CompName := Copy(CompName, 1, SecondDot - 1);

                      // Ищем компонент на форме
                      CurrentComponent := CurrentForm.FindComponent(CompName);

                      // Если не нашли через FindComponent, ищем вручную
                      If CurrentComponent = Nil Then
                      Begin
                        For m := 0 To CurrentForm.ComponentCount - 1 Do
                        Begin
                          If SameText(CurrentForm.Components[m].Name, CompName) Then
                          Begin
                            CurrentComponent := CurrentForm.Components[m];
                            Break;
                          End;
                        End;
                      End;

                      CompExists := (CurrentComponent <> Nil);
                    End
                    Else
                    Begin
                      // Нет второй точки - это свойство формы (Form1.Caption)
                      CompExists := True;
                    End;

                    Break; // Форма найдена, выходим из цикла
                  End;
                End;

                // ==================== ПРОВЕРКА ДУБЛИКАТОВ ====================
                IsDuplicate := False;
                // Проверяем предыдущие ключи на дубликаты (только внутри той же формы)
                For n := 0 To j - 1 Do
                Begin
                  CompareKey := Keys[n];
                  CompareDotPos := Pos('.', CompareKey);

                  If CompareDotPos > 0 Then
                  Begin
                    CompareFormName := Copy(CompareKey, 1, CompareDotPos - 1);

                    // Дубликатом считаем только если:
                    // 1. Имя формы совпадает
                    // 2. Полный путь совпадает (регистронезависимо)
                    If (SameText(FormName, CompareFormName)) And (SameText(CompPath, CompareKey)) Then
                    Begin
                      IsDuplicate := True;
                      Break;
                    End;
                  End;
                End;

                // ==================== УДАЛЕНИЕ КЛЮЧА ====================
                // Удаляем если:
                // 1. Форма или компонент не существуют ИЛИ
                // 2. Найден дубликат в той же форме
                If (Not (FormExists And CompExists)) Or IsDuplicate Then
                Begin
                  Ini.DeleteKey(Sections[i], Keys[j]);
                  Modified := True;
                End;
              End
              Else
              Begin
                // Некорректный формат - удаляем
                Ini.DeleteKey(Sections[i], Keys[j]);
                Modified := True;
              End;
            End;

            // Проверяем, не пустая ли секция после удаления
            // (кроме исключенных секций)
            If Not (SameText(Sections[i], 'Language information') Or SameText(Sections[i], 'DestDir')) Then
            Begin
              Ini.ReadSection(Sections[i], Keys);
              If Keys.Count = 0 Then
              Begin
                Ini.EraseSection(Sections[i]);
                Modified := True;
              End;
            End;
          End;

          If Modified Then
            Ini.UpdateFile;

        Finally
          Keys.Free;
          Sections.Free;
          Ini.Free;
        End;
      End;
    Until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  End;
End;

Function GetApplicationBitness: String;
Begin
  {$IFDEF WIN64}
  Result := '(64-bit)';
  {$ELSE}
  Result := '(32-bit)';
  {$ENDIF}
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  portable := fileExists(ExtractFilePath(ParamStr(0)) + 'portable.ini');
  Form1.Caption := Form1.Caption + ' ' + GetFileVersion(ParamStr(0)) + ' ' + GetApplicationBitness;
  If portable Then
    Form1.Caption := Form1.Caption + ' Portable';

  Form1.LoadNastr;
  CheckBoxAutoSynchronization.Checked := ReadCurrentTimeSyncState;
  LoadTimeServersFromRegistry(ComboBoxTimeServer);
  StatusBarTmeServer.Panels[0].Text := GetCurrentTimeServer;
  LastTimeUpdate;
  If fileExists(PortablePathServer) Then
  Begin
    Form1.MemoSereverList.Lines.LoadFromFile(PortablePathServer, TEncoding.Unicode);
  End;

End;

Procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
End;

Function TForm1.GetCurrentTimeServer: String;
Var
  Reg: TRegistry;
Begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    If Reg.OpenKey('SYSTEM\CurrentControlSet\Services\W32Time\Parameters', False) Then
    Begin
      If Reg.ValueExists('NtpServer') Then
        Result := Reg.ReadString('NtpServer');
      Reg.CloseKey;
    End;
  Finally
    Reg.Free;
  End;
End;

Function FileTimeToDateTime(Const FileTime: TFileTime): TDateTime;
Var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
Begin
  // Преобразуем FILETIME в локальное время
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  // Преобразуем в SYSTEMTIME
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  // Преобразуем в TDateTime
  Result := SystemTimeToDateTime(SystemTime);
End;

Procedure TForm1.UnCheckTheme;
Var
  i: Integer;
Begin
  For i := 0 To ThemeMenu.Count - 1 Do
  Begin
    ThemeMenu.Items[i].Checked := False;
  End;
End;

Procedure TForm1.ThemeAutoClick(Sender: TObject);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
  UnCheckTheme;
  ThemeAuto.Checked := true;
  If DarkModeIsEnabled = true Then
  Begin
    SetSpecificThemeMode(true, 'Windows11 Modern Dark', 'Windows11 Modern Light');
    Application.ProcessMessages;
  End;

  If DarkModeIsEnabled = False Then
  Begin
    SetSpecificThemeMode(true, 'Windows11 Modern Light', 'Windows11 Modern Dark');
    Application.ProcessMessages;
  End;
End;

Procedure TForm1.ThemeDarkClick(Sender: TObject);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
  UnCheckTheme;
  ThemeDark.Checked := true;
  SetSpecificThemeMode(true, 'Windows11 Modern Dark', 'Windows11 Modern Light');
  Application.ProcessMessages;
End;

Procedure TForm1.ThemeLightClick(Sender: TObject);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
  UnCheckTheme;
  ThemeLight.Checked := true;
  SetSpecificThemeMode(true, 'Windows11 Modern Light', 'Windows11 Modern Dark');
  Application.ProcessMessages;
End;

Function TForm1.TicksToDateTime(Const Ticks: Int64): TDateTime;
Var
  FileTime: TFileTime;
Begin
  // Преобразуем Int64 в структуру FILETIME
  FileTime.dwLowDateTime := DWORD(Ticks And $FFFFFFFF);
  FileTime.dwHighDateTime := DWORD(Ticks Shr 32);

  // Преобразуем FILETIME в TDateTime
  Result := FileTimeToDateTime(FileTime);
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  LastTimeUpdate;
End;

Procedure TForm1.Timer2Timer(Sender: TObject);
Begin
  Application.ActivateHint(Mouse.CursorPos);
  Application.ProcessMessages;
End;

Function ExtractTimestampFromRegistry: String;
Var
  Reg: TRegistry;
  ValueStr: String;
  SeparatorPos: Integer;
Begin
  Result := '';
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    If Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\W32Time\Config\Status') Then
    Begin
      ValueStr := Reg.ReadString('LastGoodSampleInfo');

      // Ищем позицию разделителя ';'
      SeparatorPos := Pos(';', ValueStr);
      If SeparatorPos > 0 Then
        // Извлекаем часть строки до разделителя
        Result := Copy(ValueStr, 1, SeparatorPos - 1)
      Else
        Result := ValueStr; // Если разделителя нет, возвращаем всю строку

      Reg.CloseKey;
    End;
  Finally
    Reg.Free;
  End;
End;

Function ExtractTimestampAsInt64: Int64;
Var
  TimestampStr: String;
Begin
  TimestampStr := ExtractTimestampFromRegistry;
  If TimestampStr <> '' Then
    Result := StrToInt64Def(TimestampStr, 0)
  Else
    Result := 0;
End;

Procedure TForm1.LabelCurrentServerClick(Sender: TObject);
Begin
  StatusBarTmeServer.Panels[0].Text := '';
  StatusBarTmeServer.Panels[0].Text := GetCurrentTimeServer;
  Application.ProcessMessages;
End;

Procedure TForm1.LabelLastUpdateTimeClick(Sender: TObject);
Begin
  StatusBarTmeServer.Panels[1].Text := '';
  LastTimeUpdate;
  Application.ProcessMessages;
End;

Procedure TForm1.LastTimeUpdate;
Var
  Ticks: Int64;
  DateTime: TDateTime;
Begin
  Ticks := ExtractTimestampAsInt64;
  DateTime := TicksToDateTime(Ticks);
  StatusBarTmeServer.Panels[1].Text := FormatDateTime('dd.mm.yyyy hh:nn:ss', DateTime);
  Application.ProcessMessages;
End;

Procedure TForm1.SpeedButton_GeneralMenuClick(Sender: TObject);
Var
  ButtonRight: TPoint;
Begin
  If (SpeedButton_GeneralMenu.AllowAllUp) Then
  Begin
    SpeedButton_GeneralMenu.AllowAllUp := False;
    SpeedButton_GeneralMenu.Down := true;
    Application.ProcessMessages;
    If Sender Is TControl Then
    Begin
      ButtonRight.X := SpeedButton_GeneralMenu.Left;
      ButtonRight.Y := SpeedButton_GeneralMenu.Top;
      ButtonRight := ClientToScreen(ButtonRight);
      PopupMenu_General.Popup(ButtonRight.X + SpeedButton_GeneralMenu.Width, ButtonRight.Y + SpeedButton_GeneralMenu.Height);
    End;
  End
  Else
  Begin
    SpeedButton_GeneralMenu.AllowAllUp := true;
    SpeedButton_GeneralMenu.Down := False;
    PopupMenu_General.CloseMenu;
    Application.ProcessMessages;
  End;
End;

Function RemoveAfterChar(Const S: String; Ch: Char): String;
Var
  Index: Integer;
Begin
  Index := Pos(Ch, S);
  If Index = 0 Then
    Result := S
  Else
    Result := TrimRight(Copy(S, 1, Index - 1));
End;

Procedure TForm1.Globload;
Var
  i: Integer;
  Internat: TTranslation;
  Ini: TMemIniFile;
  lang, lang_file: String;
Begin
  For i := 0 To Screen.FormCount - 1 Do
  Begin
    Ini := TMemIniFile.Create(Form1.PortablePath);
    lang := Ini.ReadString('Language', 'Language', '');
    lang_file := ExtractFilePath(ParamStr(0)) + 'Language\' + lang + '.ini';
    Ini.Free;
    Ini := TMemIniFile.Create(lang_file);
    If Ini.SectionExists(Application.Title) Then // Используем конкретную секцию
    Begin
      Internat.Execute(Screen.Forms[i], Application.Title);
      // Передаем имя секции
      Application.ProcessMessages;
    End;
    Ini.Free;
  End;
End;

Procedure TForm1.GroupBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
End;

Procedure TForm1.GroupBoxTimeServerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
End;

Procedure TForm1.ImageTitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  SpeedButton_GeneralMenu.AllowAllUp := true;
  SpeedButton_GeneralMenu.Down := False;
End;

End.

