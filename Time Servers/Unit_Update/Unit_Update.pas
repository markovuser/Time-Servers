Unit Unit_Update;

Interface

Uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Menus, Math, IdComponent, IdBaseComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, System.Win.TaskbarCore, Vcl.Taskbar, IdSSLOpenSSL, ShellApi,
  IniFiles, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, registry, System.JSON,
  System.Generics.Collections, FileInfoUtils;

Type
  CheckThread = Class(TThread)
  Private
  Protected
    Procedure Execute; Override;
  End;

Type
  TForm10 = Class(TForm)
    MemoUpdateLog: TMemo;
    TabControlButtons: TTabControl;
    ButtonDownload: TButton;
    PopupMenuLanguage: TPopupMenu;
    LangUpdateCancel: TMenuItem;
    LangNoDownload: TMenuItem;
    LangDownloadComplet: TMenuItem;
    LangNoUpdate: TMenuItem;
    LangServerVersion: TMenuItem;
    LangVersionComputer: TMenuItem;
    LangUpdateAvailable: TMenuItem;
    LangSize: TMenuItem;
    LangLaunchApp: TMenuItem;
    LangCloseApp: TMenuItem;
    LangDownload: TMenuItem;
    LangCancel: TMenuItem;
    IdHTTP1: TIdHTTP;
    IdHTTP2: TIdHTTP;
    TaskbarUpdate: TTaskbar;
    ProgressBarUpdate: TProgressBar;
    LangServerError: TMenuItem;
    LangVersionChanges: TMenuItem;
    ButtonClose: TButton;
    GroupBoxOption: TGroupBox;
    CheckBoxQuickUpdate: TCheckBox;
    CheckBoxForceUpdate: TCheckBox;
    Procedure UpdateApp(Server: String);
    Procedure Check(ServerVersion, LocalVersion: String);
    Procedure log(Const AText: String);
    Procedure GetLatestReleaseBody;
    Function FormatGitHubDate(Const GitHubDate: String): String;
    Function GetSystemProxy(Out ProxyServer: String; Out ProxyPort: Integer; Out Enabled: Boolean): Boolean;
    Procedure FormCreate(Sender: TObject);
    Procedure ButtonDownloadClick(Sender: TObject);
    Procedure IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    Procedure IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    Procedure IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    Procedure FormShow(Sender: TObject);
    Procedure ButtonCloseClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure CheckBoxForceUpdateClick(Sender: TObject);
  Private
    { Private declarations }
    AbortUpdate: Boolean;
  Public
  Protected
    Procedure CreateParams(Var Params: TCreateParams); Override;
  End;

Var
  Form10: TForm10;
  i: Int64;
  Thread: CheckThread;
  version: Integer;
  FileDownloadUrl: String;
  DownloadVersion: String;
  ProxyServer: String;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;

Implementation

Uses
  Unit_Base;
{$R *.dfm}

Procedure TForm10.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle Or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
End;

Function ExtractFileNameFromURL(Const URL: String): String;
Var
  i: Integer;
Begin
  Result := '';
  // Ищем последний слэш в URL
  For i := Length(URL) Downto 1 Do
  Begin
    If URL[i] = '/' Then
    Begin
      Result := Copy(URL, i + 1, Length(URL) - i);
      Break;
    End;
  End;

  // Если слэш не найден, возвращаем исходную строку
  If Result = '' Then
    Result := URL;
End;

Procedure TForm10.log(Const AText: String);
Begin
  MemoUpdateLog.Lines.Add('    ' + AText);
  // Автопрокрутка
  MemoUpdateLog.SelStart := Length(MemoUpdateLog.Text);
  SendMessage(MemoUpdateLog.Handle, EM_SCROLLCARET, 0, 0);
End;

Function GetTempDirectory: String;
Var
  Buffer: Array[0..MAX_PATH] Of Char;
Begin
  GetTempPath(MAX_PATH, Buffer);
  Result := IncludeTrailingPathDelimiter(StrPas(Buffer));
End;

Function CompareVersions(Const Ver1, Ver2: String): Integer;
Var
  Parts1, Parts2: TStringList;
  i, Num1, Num2: Integer;
Begin
  Result := 0;
  Parts1 := TStringList.Create;
  Parts2 := TStringList.Create;
  Try
    Parts1.Delimiter := '.';
    Parts1.DelimitedText := Ver1;
    Parts2.Delimiter := '.';
    Parts2.DelimitedText := Ver2;
    For i := 0 To Max(Parts1.Count - 1, Parts2.Count - 1) Do
    Begin
      If i < Parts1.Count Then
        Num1 := StrToIntDef(Parts1[i], 0)
      Else
        Num1 := 0;
      If i < Parts2.Count Then
        Num2 := StrToIntDef(Parts2[i], 0)
      Else
        Num2 := 0;
      If Num1 > Num2 Then
        Exit(1) // Ver1 > Ver2
      Else If Num1 < Num2 Then
        Exit(-1); // Ver1 < Ver2
    End;
  Finally
    Parts1.Free;
    Parts2.Free;
  End;
End;

Procedure TForm10.ButtonCloseClick(Sender: TObject);
Begin
  Try
    Close;
  Except
  End;
End;

Procedure TForm10.ButtonDownloadClick(Sender: TObject);
Var
  Params: String;
Begin
  Try
    Case ButtonDownload.Tag Of
      0:
        Begin
          AbortUpdate := false;
          ButtonDownload.Tag := 1;
          ButtonDownload.Caption := LangCancel.Caption;
          ButtonClose.Enabled := false;
          ProgressBarUpdate.Visible := TRUE;
          ProgressBarUpdate.Position := 0;
          GroupBoxOption.Enabled := false;
          UpdateApp(FileDownloadUrl);
          log(LangDownloadComplet.Caption);

          If portable = false Then
          Begin
            If CheckBoxQuickUpdate.Checked = TRUE Then
            Begin
              Params := '/SILENT /CLOSEAPPLICATIONS /SUPPRESSMSGBOXES /LANG=' + LangLocal + ' /MERGETASKS="runcmd1"';
              ShellExecute(0, 'open', pchar(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl)), pchar(Params), Nil, SW_SHOWNORMAL);
              Application.Terminate;
              Application.ProcessMessages;
            End;

            If CheckBoxQuickUpdate.Checked = false Then
            Begin
              ShellExecute(0, 'open', pchar(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl)), Nil, Nil, SW_SHOWNORMAL);
              // Application.Terminate;
              Application.ProcessMessages;
            End;
          End;

          ButtonDownload.Tag := 0;
          ButtonDownload.Caption := LangDownload.Caption;
          ButtonClose.Enabled := TRUE;
          ProgressBarUpdate.Visible := false;
          GroupBoxOption.Enabled := TRUE;
          Application.ProcessMessages;
        End;
      1:
        Begin
          ButtonDownload.Tag := 0;
          ButtonDownload.Caption := LangDownload.Caption;
          ProgressBarUpdate.Position := 0;
          ProgressBarUpdate.Visible := false;
          ButtonClose.Enabled := TRUE;
          GroupBoxOption.Enabled := TRUE;
          Try
            AbortUpdate := TRUE;
            If Assigned(IdHTTP1) Then
              IdHTTP1.Disconnect;
            ProgressBarUpdate.Position := 0;
            log(LangUpdateCancel.Caption);
            log(LangNoDownload.Caption);
            DeleteFile(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl));
          Except
          End;
        End;
    End;
  Except
  End;
End;

Function ShortenPathForLog(Const Path: String): String;
Const
  MAX_PATH_LENGTH = 60;
Var
  Drive, Dir, FileName: String;
  AvailableLength: Integer;
Begin
  If Length(Path) <= MAX_PATH_LENGTH Then
  Begin
    Result := Path;
    Exit;
  End;

  Drive := ExtractFileDrive(Path);
  FileName := ExtractFileName(Path);
  AvailableLength := MAX_PATH_LENGTH - Length(Drive) - Length(FileName) - 4;
  If AvailableLength > 10 Then
  Begin
    Dir := ExtractFilePath(Path);
    Delete(Dir, 1, Length(Drive));
    While (Dir <> '') And (Length(Dir) > AvailableLength) Do
    Begin
      If Pos('\', Dir) > 0 Then
        Delete(Dir, 1, Pos('\', Dir))
      Else
        Break;
    End;
    Result := Drive + '\...\' + Dir + FileName;
  End
  Else
  Begin
    Result := Drive + '\...\' + FileName;
    If Length(Result) > MAX_PATH_LENGTH Then
    Begin
      FileName := Copy(ExtractFileName(Path), 1, 15) + '...' + ExtractFileExt(Path);
      Result := Drive + '\...\' + FileName;
    End;
  End;
  If Length(Result) > MAX_PATH_LENGTH Then
    Result := Copy(Path, 1, MAX_PATH_LENGTH - 3) + '...';
End;

Procedure TForm10.UpdateApp(Server: String);
Var
  LoadStream: TMemoryStream;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
Begin
  SSLIOHandler := Nil;
  Try
    LoadStream := TMemoryStream.Create;
    SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    IdHTTP1.IOHandler := SSLIOHandler;
    If GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) And ProxyEnabled Then
    Begin
      IdHTTP1.ProxyParams.ProxyServer := ProxyServer;
      IdHTTP1.ProxyParams.ProxyPort := ProxyPort;
    End;
    IdHTTP1.HandleRedirects := TRUE;
    IdHTTP1.Request.BasicAuthentication := TRUE;
    IdHTTP1.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    // portable false
    If portable = false Then
    Begin
      log(ShortenPathForLog(GetTempDirectory + ExtractFileNameFromURL(FileDownloadUrl)));
      IdHTTP1.Get(IdHTTP1.URL.PathEncode(FileDownloadUrl), LoadStream);
      LoadStream.SaveToFile(GetTempDirectory + ExtractFileNameFromURL(FileDownloadUrl));
      Application.ProcessMessages;
    End;
    // portable true
    If portable = TRUE Then
    Begin
      CheckBoxQuickUpdate.Checked := false;
      log(ShortenPathForLog(ExtractFilePath(ParamStr(0)) + ExtractFileNameFromURL(FileDownloadUrl)));
      IdHTTP1.Get(IdHTTP1.URL.PathEncode(FileDownloadUrl), LoadStream);
      LoadStream.SaveToFile(ExtractFilePath(ParamStr(0)) + ExtractFileNameFromURL(FileDownloadUrl));
      Application.ProcessMessages;
    End;
    LoadStream.Free;
  Finally
    IdHTTP1.IOHandler := Nil;
    SSLIOHandler.Free;
  End;
End;

Function TForm10.GetSystemProxy(Out ProxyServer: String; Out ProxyPort: Integer; Out Enabled: Boolean): Boolean;
Var
  Reg: TRegistry;
  ProxyStr: String;
  PosColon: Integer;
Begin
  Result := false;
  ProxyServer := '';
  ProxyPort := 0;
  Enabled := false;
  Reg := TRegistry.Create;
  Try
    Reg.RootKey := HKEY_CURRENT_USER;
    If Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', false) Then
    Begin
      Enabled := Reg.ReadInteger('ProxyEnable') = 1;
      If Enabled Then
      Begin
        ProxyStr := Reg.ReadString('ProxyServer');
        If ProxyStr <> '' Then
        Begin
          PosColon := Pos(':', ProxyStr);
          If PosColon > 0 Then
          Begin
            ProxyServer := Copy(ProxyStr, 1, PosColon - 1);
            ProxyPort := StrToIntDef(Copy(ProxyStr, PosColon + 1, Length(ProxyStr)), 8080);
            Result := TRUE;
          End;
        End;
      End;
      Reg.CloseKey;
    End;
  Finally
    Reg.Free;
  End;
End;

Procedure TForm10.GetLatestReleaseBody;
Var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  JSON: TJSONObject;
  ProxyServer: String;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;
  BodyText: String;
  Lines: TArray<String>;
  I: Integer;
Begin
  HTTP := TIdHTTP.Create(Nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
  JSON := Nil;
  Try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSLHandler;
    HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    HTTP.ConnectTimeout := 3000;
    HTTP.ReadTimeout := 3000;

    If GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) And ProxyEnabled Then
    Begin
      HTTP.ProxyParams.ProxyServer := ProxyServer;
      HTTP.ProxyParams.ProxyPort := ProxyPort;
    End;

    Try
      JSON := TJSONObject.ParseJSONValue(HTTP.Get(ApiGithub)) As TJSONObject;
      If Assigned(JSON) Then
      Begin
        BodyText := JSON.GetValue('body').Value;
        Lines := BodyText.Split([#13#10, #13, #10], TStringSplitOptions.None);
        log('====================');
        log(LangVersionChanges.Caption);
        log('');
        For I := 0 To High(Lines) Do
        Begin
          log(Lines[I]);
        End;
        log('====================');
        Application.ProcessMessages;
      End;
    Except
    End;
  Finally
    HTTP.Free;
    SSLHandler.Free;
    If Assigned(JSON) Then
      JSON.Free;
  End;
End;

Function TForm10.FormatGitHubDate(Const GitHubDate: String): String;
Var
  Year, Month, Day, Hour, Min, Sec: Integer;
  DateTime: TDateTime;
  CleanDate: String;
Begin
  Try
    CleanDate := StringReplace(GitHubDate, 'Z', '', [rfReplaceAll]);
    CleanDate := StringReplace(CleanDate, 'T', ' ', [rfReplaceAll]);
    Year := StrToInt(Copy(CleanDate, 1, 4));
    Month := StrToInt(Copy(CleanDate, 6, 2));
    Day := StrToInt(Copy(CleanDate, 9, 2));
    Hour := StrToInt(Copy(CleanDate, 12, 2));
    Min := StrToInt(Copy(CleanDate, 15, 2));
    Sec := StrToInt(Copy(CleanDate, 18, 2));
    DateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, 0);
    Result := FormatDateTime('dd.mm.yyyy hh:nn:ss', DateTime);
  Except
    On E: Exception Do
    Begin
      Result := 'Date error: ' + GitHubDate;
    End;
  End;
End;

Procedure TForm10.Check(ServerVersion, LocalVersion: String);
Var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  ProxyServer: String;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;
  JSON: TJSONObject;
  Response: String;
  JSONArray: TJSONArray;
  DownloadUrl: String;
  Asset: TJSONObject;
  AssetName: String;
  FileSize, FileSizePort: Int64;
  UploadDate: String;
  i: Integer;
Begin
  SSLIOHandler := Nil;
  FileSizePort := 0;
  FileSize := 0;
  Try
    SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    IdHTTP2.IOHandler := SSLIOHandler;
    If GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) And ProxyEnabled Then
    Begin
      IdHTTP2.ProxyParams.ProxyServer := ProxyServer;
      IdHTTP2.ProxyParams.ProxyPort := ProxyPort;
    End;
    IdHTTP2.HandleRedirects := TRUE;
    IdHTTP2.Request.BasicAuthentication := TRUE;
    IdHTTP2.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    IdHTTP2.ConnectTimeout := 3000;
    IdHTTP2.ReadTimeout := 3000;

    Response := IdHTTP2.Get(ApiGithub);
    JSON := TJSONObject.ParseJSONValue(Response) As TJSONObject;
    Try
      ServerVersion := JSON.GetValue('name').Value;
      DownloadVersion := ServerVersion;

      If Not portable Then
      Begin
        JSONArray := JSON.GetValue('assets') As TJSONArray;
        If JSONArray.Count > 0 Then
        Begin
          Asset := JSONArray.Items[0] As TJSONObject;
          DownloadUrl := Asset.GetValue('browser_download_url').Value;
          FileDownloadUrl := DownloadUrl;
          FileSize := Asset.GetValue<Int64>('size');
          UploadDate := Asset.GetValue<String>('created_at');
        End;
      End;

      If portable Then
      Begin
        JSONArray := JSON.GetValue('assets') As TJSONArray;
        If Assigned(JSONArray) And (JSONArray.Count > 0) Then
        Begin
          For i := 0 To JSONArray.Count - 1 Do
          Begin
            Asset := JSONArray.Items[i] As TJSONObject;
            AssetName := Asset.GetValue<String>('name');
            DownloadUrl := Asset.GetValue<String>('browser_download_url');
            If (LowerCase(ExtractFileExt(AssetName)) = '.zip') Or (Pos('.zip', LowerCase(AssetName)) > 0) Then
            Begin
              FileDownloadUrl := DownloadUrl;
              FileSizePort := Asset.GetValue<Int64>('size');
              UploadDate := Asset.GetValue<String>('created_at');
              Break;
            End;
          End;
        End;
      End;

    Finally
      JSON.Free;
    End;

    version := CompareVersions(ServerVersion, LocalVersion);
    If (version = 0) Or (version = -1) Then
    Begin
      log(LangNoUpdate.Caption);
      log('====================');
      log(LangVersionComputer.Caption + ' ' + LocalVersion);
      log(LangServerVersion.Caption + ' ' + ServerVersion);
      log('====================');
      If Not portable Then
      Begin
        log('[Setup]');
        log('====================');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSize) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      End;
      If portable Then
      Begin
        log('[Portable]');
        log('====================');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSizePort) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      End;
      GetLatestReleaseBody;
    End;
    If version = 1 Then
    Begin
      log(LangUpdateAvailable.Caption + ' ' + ServerVersion);
      log('====================');
      log(LocalVersion + ' ---> ' + ServerVersion);
      If Not portable Then
      Begin
        log('====================');
        log('[Setup]');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSize) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      End;
      If portable Then
      Begin
        log('====================');
        log('[Portable]');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSizePort) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      End;
      GetLatestReleaseBody;
    End;

  Finally
    IdHTTP2.IOHandler := Nil;
    SSLIOHandler.Free;
  End;
End;

Procedure TForm10.CheckBoxForceUpdateClick(Sender: TObject);
Begin
  If CheckBoxForceUpdate.Checked Then
  Begin
    Form10.ButtonDownload.Tag := 0;
    Form10.ButtonDownload.Enabled := TRUE;
  End;
  If CheckBoxForceUpdate.Checked = false Then
  Begin
    Form10.ButtonDownload.Tag := 0;
    Form10.ButtonDownload.Enabled := false;
  End;
End;

Procedure CheckThread.Execute;
Begin
  Thread.FreeOnTerminate := TRUE;
  Form10.GroupBoxOption.Enabled := false;
  Form10.ButtonDownload.Enabled := false;
  // Form10.Check(DownloadVersion, '1');
  Form10.Check(DownloadVersion, GetFileVersion(ParamStr(0)));
  If Form10.CheckBoxForceUpdate.Checked = false Then
  Begin
    If (version = 0) Or (version = -1) Then
    Begin
      Form10.ButtonDownload.Tag := 0;
      Form10.ButtonDownload.Enabled := false;
      Form10.GroupBoxOption.Enabled := true;
    End;
    If version = 1 Then
    Begin
      Form10.ButtonDownload.Tag := 0;
      Form10.ButtonDownload.Enabled := TRUE;
      Form10.GroupBoxOption.Enabled := true;
    End;
  End;

End;

Procedure TForm10.FormClose(Sender: TObject; Var Action: TCloseAction);
Begin
  MemoUpdateLog.Clear;
End;

Procedure TForm10.FormCreate(Sender: TObject);
Begin
  CheckBoxQuickUpdate.Checked := Not portable;
  CheckBoxQuickUpdate.Enabled := Not portable;
End;

Procedure TForm10.FormShow(Sender: TObject);
Begin
  MemoUpdateLog.Clear;
  CheckBoxForceUpdate.Checked := False;
  Thread := CheckThread.Create(false);
  Thread.FreeOnTerminate := TRUE;
  Thread.Priority := tpNormal;
End;

Procedure TForm10.IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
Begin
  Try
    If AbortUpdate = TRUE Then
    Begin
      IdHTTP1.Disconnect;
    End;
    TaskbarUpdate.ProgressState := TTaskBarProgressState.Normal;
    ProgressBarUpdate.Position := Round(AWorkCount / i);
    TaskbarUpdate.ProgressValue := ProgressBarUpdate.Position;
    Application.ProcessMessages;
  Except
  End;
End;

Procedure TForm10.IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
Begin
  Try
    ProgressBarUpdate.Max := 100;
    TaskbarUpdate.ProgressMaxValue := ProgressBarUpdate.Max;
    i := Round(AWorkCountMax / ProgressBarUpdate.Max);
    Application.ProcessMessages;
  Except
  End;
End;

Procedure TForm10.IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
Begin
  TaskbarUpdate.ProgressState := TTaskBarProgressState.None;
End;

End.

