unit Unit_Update;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Menus, Math, IdComponent,
  IdBaseComponent, IdTCPConnection, IdTCPClient, IdHTTP, System.Win.TaskbarCore,
  Vcl.Taskbar, IdSSLOpenSSL, ShellApi, IniFiles, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, registry, System.JSON, System.Generics.Collections,
  FileInfoUtils;

type
  CheckThread = class(TThread)
  private
  protected
    procedure Execute; override;
  end;

type
  TForm10 = class(TForm)
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
    procedure UpdateApp(Server: string);
    procedure Check(ServerVersion, LocalVersion: string);
    procedure log(const AText: string);
    procedure GetLatestReleaseBody;
    function FormatGitHubDate(const GitHubDate: string): string;
    function GetSystemProxy(out ProxyServer: string; out ProxyPort: Integer; out Enabled: Boolean): Boolean;
    procedure ButtonDownloadClick(Sender: TObject);
    procedure IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure FormShow(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBoxForceUpdateClick(Sender: TObject);

    procedure SimplePortableUpdate(const ZipInAppDir, CurrentExePath: string);
  private
    { Private declarations }
    AbortUpdate: Boolean;
  public
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  Form10: TForm10;
  i: Int64;
  Thread: CheckThread;
  version: Integer;
  FileDownloadUrl: string;
  DownloadVersion: string;
  ProxyServer: string;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;

implementation

uses
  Unit_Base;
{$R *.dfm}

procedure TForm10.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;

function ExtractFileNameFromURL(const URL: string): string;
var
  i: Integer;
begin
  Result := '';
  // Ищем последний слэш в URL
  for i := Length(URL) downto 1 do
  begin
    if URL[i] = '/' then
    begin
      Result := Copy(URL, i + 1, Length(URL) - i);
      Break;
    end;
  end;

  // Если слэш не найден, возвращаем исходную строку
  if Result = '' then
    Result := URL;
end;

procedure TForm10.log(const AText: string);
begin
  MemoUpdateLog.Lines.Add('    ' + AText);
  // Автопрокрутка
  MemoUpdateLog.SelStart := Length(MemoUpdateLog.Text);
  SendMessage(MemoUpdateLog.Handle, EM_SCROLLCARET, 0, 0);
end;

function GetTempDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buffer);
  Result := IncludeTrailingPathDelimiter(StrPas(Buffer));
end;

function CompareVersions(const Ver1, Ver2: string): Integer;
var
  Parts1, Parts2: TStringList;
  i, Num1, Num2: Integer;
begin
  Result := 0;
  Parts1 := TStringList.Create;
  Parts2 := TStringList.Create;
  try
    Parts1.Delimiter := '.';
    Parts1.DelimitedText := Ver1;
    Parts2.Delimiter := '.';
    Parts2.DelimitedText := Ver2;
    for i := 0 to Max(Parts1.Count - 1, Parts2.Count - 1) do
    begin
      if i < Parts1.Count then
        Num1 := StrToIntDef(Parts1[i], 0)
      else
        Num1 := 0;
      if i < Parts2.Count then
        Num2 := StrToIntDef(Parts2[i], 0)
      else
        Num2 := 0;
      if Num1 > Num2 then
        Exit(1) // Ver1 > Ver2
      else if Num1 < Num2 then
        Exit(-1); // Ver1 < Ver2
    end;
  finally
    Parts1.Free;
    Parts2.Free;
  end;
end;

procedure TForm10.ButtonCloseClick(Sender: TObject);
begin
  try
    Close;
  except
  end;
end;

procedure TForm10.SimplePortableUpdate(const ZipInAppDir, CurrentExePath: string);
var
  BatFile: TextFile;
  BatName, AppName, TempDir: string;
begin
  AppName := ExtractFileName(CurrentExePath);
  TempDir := GetTempDirectory;
  BatName := TempDir + '\update_' + FormatDateTime('hhnnss', Now) + '.bat';

  AssignFile(BatFile, BatName);
  try
    Rewrite(BatFile);

    // ТОЧНО ТОТ ЖЕ BAT
    Writeln(BatFile, '@echo off');
    Writeln(BatFile, 'cd /d "' + ExtractFilePath(CurrentExePath) + '"');
    Writeln(BatFile, '');
    Writeln(BatFile, 'echo Killing app...');
    Writeln(BatFile, 'taskkill /F /IM "' + AppName + '" >nul 2>&1');
    Writeln(BatFile, 'timeout /t 2 >nul');
    Writeln(BatFile, '');
    Writeln(BatFile, 'echo Extracting...');
    Writeln(BatFile, 'powershell -Command "Add-Type -AssemblyName System.IO.Compression.FileSystem; $zip = [System.IO.Compression.ZipFile]::OpenRead(''' + ExtractFileName(ZipInAppDir) + '''); foreach ($entry in $zip.Entries) { $target = $entry.FullName; if (-not $entry.Name) { [System.IO.Directory]::CreateDirectory($target) | Out-Null; } else { try { [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $target, $true) } catch { Write-Host (''Skipping: '' + $entry.FullName) } } }; $zip.Dispose(); Write-Host ''Extraction complete''"');
    Writeln(BatFile, '');
    Writeln(BatFile, 'echo Deleting ZIP...');
    Writeln(BatFile, 'del "' + ExtractFileName(ZipInAppDir) + '"');
    Writeln(BatFile, '');
    Writeln(BatFile, 'echo Starting app...');
    Writeln(BatFile, 'start "" "' + AppName + '"');
    Writeln(BatFile, '');
    Writeln(BatFile, 'echo Done');
    Writeln(BatFile, 'timeout /t 2 >nul');
    Writeln(BatFile, 'del "%~f0"');

  finally
    CloseFile(BatFile);
  end;

  // Проверяем создался ли файл
  if FileExists(BatName) then
  begin
    // Запускаем
    ShellExecute(0, 'open', PChar(BatName), nil, nil, SW_HIDE);
    Halt;
  end
  else
  begin
    ShowMessage('Failed to create update script!');
  end;
end;

procedure TForm10.ButtonDownloadClick(Sender: TObject);
var
  Params: string;
begin
  try
    case ButtonDownload.Tag of
      0:
        begin
          AbortUpdate := false;
          ButtonDownload.Tag := 1;
          ButtonDownload.Caption := LangCancel.Caption;
          ButtonClose.Enabled := false;
          ProgressBarUpdate.Visible := TRUE;
          ProgressBarUpdate.Position := 0;
          GroupBoxOption.Enabled := false;
          UpdateApp(FileDownloadUrl);
          log(LangDownloadComplet.Caption);

          if portable = false then
          begin
            if CheckBoxQuickUpdate.Checked = TRUE then
            begin
              Params := '/SILENT /CLOSEAPPLICATIONS /SUPPRESSMSGBOXES /LANG=' + LangLocal + ' /MERGETASKS="runcmd1"';
              ShellExecute(0, 'open', pchar(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl)), pchar(Params), nil, SW_SHOWNORMAL);
              Application.Terminate;
              Application.ProcessMessages;
            end
            else
            begin
              ShellExecute(0, 'open', pchar(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl)), nil, nil, SW_SHOWNORMAL);
              Application.ProcessMessages;
            end;
          end
          else
          begin
            if CheckBoxQuickUpdate.Checked = TRUE then
            begin
              SimplePortableUpdate(ExtractFilePath(ParamStr(0)) + ExtractFileNameFromURL(FileDownloadUrl), Application.ExeName);
            end;
          end;

          ButtonDownload.Tag := 0;
          ButtonDownload.Caption := LangDownload.Caption;
          ButtonClose.Enabled := TRUE;
          ProgressBarUpdate.Visible := false;
          GroupBoxOption.Enabled := TRUE;
          Application.ProcessMessages;
        end;
      1:
        begin
          ButtonDownload.Tag := 0;
          ButtonDownload.Caption := LangDownload.Caption;
          ProgressBarUpdate.Position := 0;
          ProgressBarUpdate.Visible := false;
          ButtonClose.Enabled := TRUE;
          GroupBoxOption.Enabled := TRUE;
          try
            AbortUpdate := TRUE;
            if Assigned(IdHTTP1) then
              IdHTTP1.Disconnect;
            ProgressBarUpdate.Position := 0;
            log(LangUpdateCancel.Caption);
            log(LangNoDownload.Caption);
            DeleteFile(GetTempDirectory + '\' + ExtractFileNameFromURL(FileDownloadUrl));
          except
          end;
        end;
    end;
  except
  end;
end;

function ShortenPathForLog(const Path: string): string;
const
  MAX_PATH_LENGTH = 60;
var
  Drive, Dir, FileName: string;
  AvailableLength: Integer;
begin
  if Length(Path) <= MAX_PATH_LENGTH then
  begin
    Result := Path;
    Exit;
  end;

  Drive := ExtractFileDrive(Path);
  FileName := ExtractFileName(Path);
  AvailableLength := MAX_PATH_LENGTH - Length(Drive) - Length(FileName) - 4;
  if AvailableLength > 10 then
  begin
    Dir := ExtractFilePath(Path);
    Delete(Dir, 1, Length(Drive));
    while (Dir <> '') and (Length(Dir) > AvailableLength) do
    begin
      if Pos('\', Dir) > 0 then
        Delete(Dir, 1, Pos('\', Dir))
      else
        Break;
    end;
    Result := Drive + '\...\' + Dir + FileName;
  end
  else
  begin
    Result := Drive + '\...\' + FileName;
    if Length(Result) > MAX_PATH_LENGTH then
    begin
      FileName := Copy(ExtractFileName(Path), 1, 15) + '...' + ExtractFileExt(Path);
      Result := Drive + '\...\' + FileName;
    end;
  end;
  if Length(Result) > MAX_PATH_LENGTH then
    Result := Copy(Path, 1, MAX_PATH_LENGTH - 3) + '...';
end;

procedure TForm10.UpdateApp(Server: string);
var
  LoadStream: TMemoryStream;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLIOHandler := nil;
  try
    LoadStream := TMemoryStream.Create;
    SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    IdHTTP1.IOHandler := SSLIOHandler;
    if GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) and ProxyEnabled then
    begin
      IdHTTP1.ProxyParams.ProxyServer := ProxyServer;
      IdHTTP1.ProxyParams.ProxyPort := ProxyPort;
    end;
    IdHTTP1.HandleRedirects := TRUE;
    IdHTTP1.Request.BasicAuthentication := TRUE;
    IdHTTP1.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    // portable false
    if portable = false then
    begin
      log(ShortenPathForLog(GetTempDirectory + ExtractFileNameFromURL(FileDownloadUrl)));
      IdHTTP1.Get(IdHTTP1.URL.PathEncode(FileDownloadUrl), LoadStream);
      LoadStream.SaveToFile(GetTempDirectory + ExtractFileNameFromURL(FileDownloadUrl));
      Application.ProcessMessages;
    end;
    // portable true
    if portable = TRUE then
    begin
      //CheckBoxQuickUpdate.Checked := false;
      log(ShortenPathForLog(ExtractFilePath(ParamStr(0)) + ExtractFileNameFromURL(FileDownloadUrl)));
      IdHTTP1.Get(IdHTTP1.URL.PathEncode(FileDownloadUrl), LoadStream);
      LoadStream.SaveToFile(ExtractFilePath(ParamStr(0)) + ExtractFileNameFromURL(FileDownloadUrl));
      Application.ProcessMessages;
    end;
    LoadStream.Free;
  finally
    IdHTTP1.IOHandler := nil;
    SSLIOHandler.Free;
  end;
end;

function TForm10.GetSystemProxy(out ProxyServer: string; out ProxyPort: Integer; out Enabled: Boolean): Boolean;
var
  Reg: TRegistry;
  ProxyStr: string;
  PosColon: Integer;
begin
  Result := false;
  ProxyServer := '';
  ProxyPort := 0;
  Enabled := false;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings', false) then
    begin
      Enabled := Reg.ReadInteger('ProxyEnable') = 1;
      if Enabled then
      begin
        ProxyStr := Reg.ReadString('ProxyServer');
        if ProxyStr <> '' then
        begin
          PosColon := Pos(':', ProxyStr);
          if PosColon > 0 then
          begin
            ProxyServer := Copy(ProxyStr, 1, PosColon - 1);
            ProxyPort := StrToIntDef(Copy(ProxyStr, PosColon + 1, Length(ProxyStr)), 8080);
            Result := TRUE;
          end;
        end;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TForm10.GetLatestReleaseBody;
var
  HTTP: TIdHTTP;
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
  JSON: TJSONObject;
  ProxyServer: string;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;
  BodyText: string;
  Lines: TArray<string>;
  I: Integer;
begin
  HTTP := TIdHTTP.Create(nil);
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  JSON := nil;
  try
    SSLHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSLHandler;
    HTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    HTTP.ConnectTimeout := 3000;
    HTTP.ReadTimeout := 3000;

    if GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) and ProxyEnabled then
    begin
      HTTP.ProxyParams.ProxyServer := ProxyServer;
      HTTP.ProxyParams.ProxyPort := ProxyPort;
    end;

    try
      JSON := TJSONObject.ParseJSONValue(HTTP.Get(ApiGithub)) as TJSONObject;
      if Assigned(JSON) then
      begin
        BodyText := JSON.GetValue('body').Value;
        Lines := BodyText.Split([#13#10, #13, #10], TStringSplitOptions.None);
        log('====================');
        log(LangVersionChanges.Caption);
        log('');
        for I := 0 to High(Lines) do
        begin
          log(Lines[I]);
        end;
        log('====================');
        Application.ProcessMessages;
      end;
    except
    end;
  finally
    HTTP.Free;
    SSLHandler.Free;
    if Assigned(JSON) then
      JSON.Free;
  end;
end;

function TForm10.FormatGitHubDate(const GitHubDate: string): string;
var
  Year, Month, Day, Hour, Min, Sec: Integer;
  DateTime: TDateTime;
  CleanDate: string;
begin
  try
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
  except
    on E: Exception do
    begin
      Result := 'Date error: ' + GitHubDate;
    end;
  end;
end;

procedure TForm10.Check(ServerVersion, LocalVersion: string);
var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  ProxyServer: string;
  ProxyPort: Integer;
  ProxyEnabled: Boolean;
  JSON: TJSONObject;
  Response: string;
  JSONArray: TJSONArray;
  DownloadUrl: string;
  Asset: TJSONObject;
  AssetName: string;
  FileSize, FileSizePort: Int64;
  UploadDate: string;
  i: Integer;
begin
  SSLIOHandler := nil;
  FileSizePort := 0;
  FileSize := 0;
  try
    SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
    IdHTTP2.IOHandler := SSLIOHandler;
    if GetSystemProxy(ProxyServer, ProxyPort, ProxyEnabled) and ProxyEnabled then
    begin
      IdHTTP2.ProxyParams.ProxyServer := ProxyServer;
      IdHTTP2.ProxyParams.ProxyPort := ProxyPort;
    end;
    IdHTTP2.HandleRedirects := TRUE;
    IdHTTP2.Request.BasicAuthentication := TRUE;
    IdHTTP2.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64;rv:12.0) Gecko/20100101 Firefox/12.0';
    IdHTTP2.ConnectTimeout := 3000;
    IdHTTP2.ReadTimeout := 3000;

    Response := IdHTTP2.Get(ApiGithub);
    JSON := TJSONObject.ParseJSONValue(Response) as TJSONObject;
    try
      ServerVersion := JSON.GetValue('name').Value;
      DownloadVersion := ServerVersion;

      if not portable then
      begin
        JSONArray := JSON.GetValue('assets') as TJSONArray;
        if JSONArray.Count > 0 then
        begin
          Asset := JSONArray.Items[0] as TJSONObject;
          DownloadUrl := Asset.GetValue('browser_download_url').Value;
          FileDownloadUrl := DownloadUrl;
          FileSize := Asset.GetValue<Int64>('size');
          UploadDate := Asset.GetValue<string>('created_at');
        end;
      end;

      if portable then
      begin
        JSONArray := JSON.GetValue('assets') as TJSONArray;
        if Assigned(JSONArray) and (JSONArray.Count > 0) then
        begin
          for i := 0 to JSONArray.Count - 1 do
          begin
            Asset := JSONArray.Items[i] as TJSONObject;
            AssetName := Asset.GetValue<string>('name');
            DownloadUrl := Asset.GetValue<string>('browser_download_url');
            if (LowerCase(ExtractFileExt(AssetName)) = '.zip') or (Pos('.zip', LowerCase(AssetName)) > 0) then
            begin
              FileDownloadUrl := DownloadUrl;
              FileSizePort := Asset.GetValue<Int64>('size');
              UploadDate := Asset.GetValue<string>('created_at');
              Break;
            end;
          end;
        end;
      end;

    finally
      JSON.Free;
    end;

    version := CompareVersions(ServerVersion, LocalVersion);
    if (version = 0) or (version = -1) then
    begin
      log(LangNoUpdate.Caption);
      log('====================');
      log(LangVersionComputer.Caption + ' ' + LocalVersion);
      log(LangServerVersion.Caption + ' ' + ServerVersion);
      log('====================');
      if not portable then
      begin
        log('[Setup]');
        log('====================');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSize) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      end;
      if portable then
      begin
        log('[Portable]');
        log('====================');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSizePort) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      end;
      GetLatestReleaseBody;
    end;
    if version = 1 then
    begin
      log(LangUpdateAvailable.Caption + ' ' + ServerVersion);
      log('====================');
      log(LocalVersion + ' ---> ' + ServerVersion);
      if not portable then
      begin
        log('====================');
        log('[Setup]');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSize) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      end;
      if portable then
      begin
        log('====================');
        log('[Portable]');
        log(ExtractFileNameFromURL(FileDownloadUrl) + ' [' + GetNormalSize(FileSizePort) + ']' + ' - ' + FormatGitHubDate(UploadDate));
      end;
      GetLatestReleaseBody;
    end;

  finally
    IdHTTP2.IOHandler := nil;
    SSLIOHandler.Free;
  end;
end;

procedure TForm10.CheckBoxForceUpdateClick(Sender: TObject);
begin
  if CheckBoxForceUpdate.Checked then
  begin
    Form10.ButtonDownload.Tag := 0;
    Form10.ButtonDownload.Enabled := TRUE;
  end;
  if CheckBoxForceUpdate.Checked = false then
  begin
    Form10.ButtonDownload.Tag := 0;
    Form10.ButtonDownload.Enabled := false;
  end;
end;

procedure CheckThread.Execute;
begin
  Thread.FreeOnTerminate := TRUE;
  Form10.GroupBoxOption.Enabled := false;
  Form10.ButtonDownload.Enabled := false;
  // Form10.Check(DownloadVersion, '1');
  Form10.Check(DownloadVersion, GetFileVersion(ParamStr(0)));
  if Form10.CheckBoxForceUpdate.Checked = false then
  begin
    if (version = 0) or (version = -1) then
    begin
      Form10.ButtonDownload.Tag := 0;
      Form10.ButtonDownload.Enabled := false;
      Form10.GroupBoxOption.Enabled := true;
    end;
    if version = 1 then
    begin
      Form10.ButtonDownload.Tag := 0;
      Form10.ButtonDownload.Enabled := TRUE;
      Form10.GroupBoxOption.Enabled := true;
    end;
  end;

end;

procedure TForm10.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MemoUpdateLog.Clear;
end;

procedure TForm10.FormShow(Sender: TObject);
begin
  MemoUpdateLog.Clear;
  CheckBoxForceUpdate.Checked := False;
  Thread := CheckThread.Create(false);
  Thread.FreeOnTerminate := TRUE;
  Thread.Priority := tpNormal;
end;

procedure TForm10.IdHTTP1Work(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  try
    if AbortUpdate = TRUE then
    begin
      IdHTTP1.Disconnect;
    end;
    TaskbarUpdate.ProgressState := TTaskBarProgressState.Normal;
    ProgressBarUpdate.Position := Round(AWorkCount / i);
    TaskbarUpdate.ProgressValue := ProgressBarUpdate.Position;
    Application.ProcessMessages;
  except
  end;
end;

procedure TForm10.IdHTTP1WorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  try
    ProgressBarUpdate.Max := 100;
    TaskbarUpdate.ProgressMaxValue := ProgressBarUpdate.Max;
    i := Round(AWorkCountMax / ProgressBarUpdate.Max);
    Application.ProcessMessages;
  except
  end;
end;

procedure TForm10.IdHTTP1WorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  TaskbarUpdate.ProgressState := TTaskBarProgressState.None;
end;

end.

