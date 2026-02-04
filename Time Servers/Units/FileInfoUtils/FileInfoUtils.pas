Unit FileInfoUtils;

Interface

Uses
  Windows, SysUtils, ShellAPI, ShlObj, ActiveX, ComObj, Classes, Controls, Forms,
  StdCtrls, CommCtrl, Graphics, ImgList, SHDocVw, Winapi.Messages, ComCtrls,
  Registry;

Type
  TLangChrSet = Array[0..1] Of Word;

  PLangChrSet = ^TLangChrSet;

// Функция извлекает чистое имя файла без расширения
Function ExtractOnlyFileName(Const FileName: String): String;

// Возвращает строку с копирайтом файла
Function GetFileCopyright(Const FName: TFileName): String;

// Получение комментария к файлу
Function GetFileComment(Const FName: TFileName): String;

// Получение описания файла
Function GetFileDescription(Const FName: TFileName): String;

// Получение версии файла
Function GetFileVersion(Const FName: TFileName): String;

// Получение имени производителя файла
Function GetCompanyName(Const FName: TFileName): String;

// Определение архитектуры исполняемого файла (32/64 бит)
Function GetBinaryPlatform(aFileName: String): String;

// Преобразование размера файла в удобочитаемый вид
Function GetNormalSize(Size: Int64): String;

// Открытие проводника с выделением конкретного файла
Function OpenExplorerAndSelectFile(Const Path: String): Boolean;

// Извлекает размер файла
Function GetMyFileSize(Const namefile: String): Int64;

// Удаление в корзину
Function DeleteFileWithUndo(Const sFileName: String): boolean;

// Свойства файла и папки
Function ShowPropertiesDialog(Const FName: String): Boolean;

// Процедура подключения значков (БЕЗОПАСНАЯ ВЕРСИЯ)
Procedure CreateIcon(ListView: TListView);

// Установка системных иконок для TListView (большие/маленькие)
Procedure SetListViewSystemIcons(ListView: TListView; UseLargeIcons: Boolean);

// Открывает Regedit и переходит к указанному ключу реестра
Function OpenRegeditToKey(KeyPath: String): Boolean;

// Альтернативная версия с параметрами для раздельного указания корневого ключа и пути
Function OpenRegeditToKeyEx(RootKey, SubKey: String): Boolean;


// Полное удаление содержимого папки (файлов и подпапок), но не самой папки
Function FullRemoveDirContents(Const Dir: String): Boolean;

Implementation

// Полное удаление содержимого папки (файлов и подпапок), но не самой папки

Function FullRemoveDirContents(Const Dir: String): Boolean;

  Function DeleteDirectoryContents(Const Directory: String): Boolean;
  Var
    SearchRec: TSearchRec;
    FindResult: Integer;
    FilePath: String;
    Attr: Integer;
  Begin
    Result := True;
    FindResult := FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, SearchRec);
    If FindResult = 0 Then
    Begin
      Try
        Repeat
          If (SearchRec.Name = '.') Or (SearchRec.Name = '..') Then
            Continue;

          FilePath := IncludeTrailingPathDelimiter(Directory) + SearchRec.Name;
          Attr := SearchRec.Attr;
          SetFileAttributes(PChar(FilePath), FILE_ATTRIBUTE_NORMAL);
          If (Attr And faDirectory) = faDirectory Then
          Begin
            If Not DeleteDirectoryContents(FilePath) Then
              Result := False;
            RemoveDir(FilePath);
          End
          Else
          Begin
            If Not DeleteFile(FilePath) Then
              Result := False;
          End;
        Until FindNext(SearchRec) <> 0;
      Finally
        FindClose(SearchRec);
      End;
    End;
  End;

Begin
  Result := False;
  If (Trim(Dir) = '') Or (Not DirectoryExists(Dir)) Then
    Exit;

  SetFileAttributes(PChar(Dir), FILE_ATTRIBUTE_NORMAL);
  Result := DeleteDirectoryContents(Dir);
End;

Function OpenRegeditToKey(KeyPath: String): Boolean;
Var
  Reg: TRegistry;
  RegeditPath: String;
Begin
  Result := False;
  Reg := TRegistry.Create(KEY_WRITE);
  Try
    Try
      Reg.RootKey := HKEY_CURRENT_USER;
      If Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Applets\Regedit', True) Then
      Begin
        Reg.WriteString('LastKey', KeyPath);
        Reg.CloseKey;
        RegeditPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('SystemRoot')) + 'SysNative\regedt32.exe';
        If Not FileExists(RegeditPath) Then
          RegeditPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('SystemRoot')) + 'regedit.exe';
        Result := ShellExecute(0, 'open', PChar(RegeditPath), Nil, Nil, SW_SHOWNORMAL) > 32;
      End;
    Except
      On E: Exception Do
      Begin
        Result := False;
      End;
    End;
  Finally
    Reg.Free;
  End;
End;

// Альтернативная версия с параметрами для раздельного указания корневого ключа и пути
Function OpenRegeditToKeyEx(RootKey, SubKey: String): Boolean;
Var
  FullKeyPath: String;
Begin
  If (RootKey <> '') And (SubKey <> '') Then
  Begin
    FullKeyPath := StringReplace(RootKey + '\' + SubKey, '\\', '\', [rfReplaceAll]);
    Result := OpenRegeditToKey(FullKeyPath);
  End
  Else If RootKey <> '' Then
  Begin
    Result := OpenRegeditToKey(RootKey);
  End
  Else
  Begin
    Result := False;
  End;
End;

// Вспомогательная функция для получения информации о версии файла
Function GetFileVersionInfoStr(Const FName: TFileName; Const InfoName: String): String;
Var
  S: String;
  n, Len: Cardinal;
  Buf, Value: PChar;
  LangChrSet: PLangChrSet;
Begin
  Result := '';
  n := GetFileVersionInfoSize(PChar(FName), n);
  If n = 0 Then
    Exit;

  Buf := AllocMem(n);
  Try
    If Not GetFileVersionInfo(PChar(FName), 0, n, Buf) Then
      Exit;

    If VerQueryValue(Buf, pchar('VarFileInfo\Translation'), pointer(LangChrSet), Len) Then
    Begin
      If (LangChrSet = Nil) Or (Len < SizeOf(TLangChrSet)) Then
        S := '040904E4'
      Else
        S := Format('%.4x%.4x', [LangChrSet^[0], LangChrSet^[1]]);
      If (S = '00000000') Then
        S := '040904E4';
    End
    Else
    Begin
      S := '040904E4';
    End;
    If VerQueryValue(Buf, pchar('StringFileInfo\' + S + '\' + InfoName), pointer(Value), Len) Then
    Begin
      If Len > 0 Then
        Result := Value;
    End;
  Finally
    FreeMem(Buf, n);
  End;
End;

// Извлечение чистого имени файла без расширения
Function ExtractOnlyFileName(Const FileName: String): String;
Begin
  Try
    Result := ChangeFileExt(ExtractFileName(FileName), '');
  Except
    Result := '';
  End;
End;

Function GetFileVersion(Const FName: TFileName): String;
Var
  S: String;
  n, Len: Cardinal;
  Buf, Value: PChar;
  LangChrSet: PLangChrSet;
  PVerValue: PVSFixedFileInfo;
  i: Integer;
  Ch: Char;
  VersionStr: String;
Begin
  Result := '';
  n := GetFileVersionInfoSize(PChar(FName), n);
  If n = 0 Then
    Exit;
  Buf := AllocMem(n);
  Try
    If Not GetFileVersionInfo(PChar(FName), 0, n, Buf) Then
      Exit;
    If Not VerQueryValue(Buf, PChar('VarFileInfo\Translation'), Pointer(LangChrSet), Len) Then
      S := '040904E4'
    Else
    Begin
      If (LangChrSet = Nil) Or (Len < SizeOf(TLangChrSet)) Then
        S := '040904E4'
      Else
        S := Format('%.4x%.4x', [LangChrSet^[0], LangChrSet^[1]]);
      If S = '00000000' Then
        S := '040904E4';
    End;
    If VerQueryValue(Buf, PChar('StringFileInfo\' + S + '\FileVersion'), Pointer(Value), Len) Then
    Begin
      If Len > 0 Then
      Begin
        SetString(VersionStr, Value, Len);
        Result := '';
        For i := 1 To Length(VersionStr) Do
        Begin
          Ch := VersionStr[i];
          If CharInSet(Ch, ['0'..'9']) Then
            Result := Result + Ch
          Else If (Ch = '.') Or (Ch = ',') Then
          Begin
            If (Result <> '') And ((Length(Result) = 0) Or (Result[Length(Result)] <> '.')) Then
              Result := Result + '.';
          End
          Else If Ch = ' ' Then
            Continue
          Else
            Break;
        End;
        If (Result <> '') And (Result[Length(Result)] = '.') Then
          Delete(Result, Length(Result), 1);
        If Result <> '' Then
          Exit;
      End;
    End;
    If VerQueryValue(Buf, '\', Pointer(PVerValue), Len) Then
    Begin
      With PVerValue^ Do
      Begin
        Result := Format('%d.%d.%d.%d', [HiWord(dwFileVersionMS), LoWord(dwFileVersionMS), HiWord(dwFileVersionLS), LoWord(dwFileVersionLS)]);
      End;
    End;
  Finally
    FreeMem(Buf, n);
  End;
End;

Function GetFileDescription(Const FName: TFileName): String;
Begin
  Result := GetFileVersionInfoStr(FName, 'FileDescription');
End;

Function GetFileCopyright(Const FName: TFileName): String;
Begin
  Result := GetFileVersionInfoStr(FName, 'LegalCopyright');
End;

Function GetCompanyName(Const FName: TFileName): String;
Begin
  Result := GetFileVersionInfoStr(FName, 'CompanyName');
End;

Function GetFileComment(Const FName: TFileName): String;
Begin
  Result := GetFileVersionInfoStr(FName, 'Comments');
End;

// Определение архитектуры исполняемого файла (32/64 бит)

Function GetBinaryPlatform(aFileName: String): String;
Var
  BinaryType: DWORD;
  FileHandle: THandle;
  DosHeader: Array[0..63] Of Byte;
  BytesRead: DWORD;
  NtHeaders: DWORD;
  MachineType: Word;

  Function GetExtensionWithoutDot(Const FileName: String): String;
  Var
    ExtWithDot: String;
  Begin
    ExtWithDot := UpperCase(ExtractFileExt(FileName));
    If ExtWithDot <> '' Then
      Result := Copy(ExtWithDot, 2, MaxInt)  // Убираем точку
    Else
      Result := '';
  End;

Begin
  If Not FileExists(aFileName) Then
  Begin
    Result := '';
    Exit;
  End;

  // Сначала пробуем стандартный способ
  If GetBinaryType(PChar(aFileName), BinaryType) Then
  Begin
    If BinaryType = SCS_32BIT_BINARY Then
      Result := '32-bit'
    Else If BinaryType = SCS_64BIT_BINARY Then
      Result := '64-bit'
    Else If BinaryType = SCS_DOS_BINARY Then
      Result := 'DOS'
    Else If BinaryType = SCS_WOW_BINARY Then
      Result := '16-bit Win'
    Else If BinaryType = SCS_PIF_BINARY Then
      Result := 'PIF'
    Else If BinaryType = SCS_POSIX_BINARY Then
      Result := 'POSIX'
    Else If BinaryType = SCS_OS216_BINARY Then
      Result := '16-bit OS/2'
    Else
    Begin
      // Неизвестный тип - показываем расширение без точки
      Result := GetExtensionWithoutDot(aFileName);
      If Result = '' Then
        Result := ' (' + IntToStr(BinaryType) + ')';
    End;
    Exit;
  End;

  // Если GetBinaryType не сработал, анализируем PE-заголовок вручную
  FileHandle := CreateFile(PChar(aFileName), GENERIC_READ, FILE_SHARE_READ, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  If FileHandle = INVALID_HANDLE_VALUE Then
  Begin
    Result := GetExtensionWithoutDot(aFileName);
    Exit;
  End;

  Try
    // Читаем DOS заголовок
    If Not ReadFile(FileHandle, DosHeader[0], 64, BytesRead, Nil) Then
    Begin
      Result := GetExtensionWithoutDot(aFileName);
      Exit;
    End;

    // Проверяем сигнатуру MZ
    If (DosHeader[0] = $4D) And (DosHeader[1] = $5A) Then // 'MZ'
    Begin
      // Получаем смещение к PE заголовку (e_lfanew)
      Move(DosHeader[$3C], NtHeaders, 4);

      // Перемещаемся к PE заголовку
      SetFilePointer(FileHandle, NtHeaders, Nil, FILE_BEGIN);

      // Читаем PE сигнатуру
      If Not ReadFile(FileHandle, NtHeaders, 4, BytesRead, Nil) Then
      Begin
        Result := 'PE file';
        Exit;
      End;

      // Проверяем PE сигнатуру 'PE\0\0'
      If NtHeaders = $00004550 Then
      Begin
        // Читаем Machine Type
        If Not ReadFile(FileHandle, MachineType, 2, BytesRead, Nil) Then
        Begin
          Result := 'PE file';
          Exit;
        End;

        // Определяем архитектуру
        Case MachineType Of
          $014C:
            Result := '32-bit';    // IMAGE_FILE_MACHINE_I386
          $8664:
            Result := '64-bit';    // IMAGE_FILE_MACHINE_AMD64
          $0200:
            Result := 'Itanium';   // IMAGE_FILE_MACHINE_IA64
          $01C4:
            Result := 'ARM';       // IMAGE_FILE_MACHINE_ARMNT
          $AA64:
            Result := 'ARM64';     // IMAGE_FILE_MACHINE_ARM64
        Else
          Result := 'PE file';
        End;
      End
      Else
      Begin
        Result := 'DOS/16-bit';
      End;
    End
    Else
    Begin
      // Не PE файл - показываем расширение без точки
      Result := GetExtensionWithoutDot(aFileName);
    End;
  Finally
    CloseHandle(FileHandle);
  End;
End;

// Преобразует байтовый размер файла в читабельный формат
Function GetNormalSize(Size: Int64): String;
Const
  UNITS: Array[0..3] Of String = (' B', ' kB', ' MB', ' GB');
Var
  Value: real;
  UnitIndex: integer;
Begin
  Value := Size;
  UnitIndex := 0;

  While (Value >= 700) And (UnitIndex < high(UNITS)) Do
  Begin
    Value := Value / 1024;
    inc(UnitIndex);
  End;

  If UnitIndex > 0 Then
    Result := FloatToStr(round(Value)) + UNITS[UnitIndex]
  Else
    Result := FloatToStr(Value) + UNITS[UnitIndex];
End;

// Метод для добавления стандартных иконок к ListView (БЕЗОПАСНАЯ)
Procedure CreateIcon(ListView: TListView);
Var
  {$IFDEF WIN64}
  SysImageList: NativeUInt;
  {$ELSE}
  SysImageList: Cardinal;
  {$ENDIF}
  SFI: TSHFileInfo;
Begin
  If Not Assigned(ListView) Then
    Exit;
  {$IFDEF WIN64}
  SysImageList := NativeUInt(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX Or SHGFI_LARGEICON));
  {$ELSE}
  SysImageList := Cardinal(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX Or SHGFI_LARGEICON));
  {$ENDIF}

  If SysImageList <> 0 Then
  Begin
    If Not Assigned(ListView.LargeImages) Then
      ListView.LargeImages := TImageList.Create(Nil);
    ListView.LargeImages.Handle := SysImageList;
    ListView.LargeImages.ShareImages := True;
  End;
  {$IFDEF WIN64}
  SysImageList := NativeUInt(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX Or SHGFI_SMALLICON));
  {$ELSE}
  SysImageList := Cardinal(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX Or SHGFI_SMALLICON));
  {$ENDIF}

  If SysImageList <> 0 Then
  Begin
    If Not Assigned(ListView.SmallImages) Then
      ListView.SmallImages := TImageList.Create(Nil);
    ListView.SmallImages.Handle := SysImageList;
    ListView.SmallImages.ShareImages := True;
  End;
End;

// Установка системных иконок для TListView
Procedure SetListViewSystemIcons(ListView: TListView; UseLargeIcons: Boolean);
Var
  {$IFDEF WIN64}
  SysImageList: NativeUInt;
  {$ELSE}
  SysImageList: Cardinal;
  {$ENDIF}
  SFI: TSHFileInfo;
  Flags: DWORD;
Begin
  If Not Assigned(ListView) Then
    Exit;

  If UseLargeIcons Then
    Flags := SHGFI_SYSICONINDEX Or SHGFI_LARGEICON
  Else
    Flags := SHGFI_SYSICONINDEX Or SHGFI_SMALLICON;
  {$IFDEF WIN64}
  SysImageList := NativeUInt(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags));
  {$ELSE}
  SysImageList := Cardinal(SHGetFileInfo('', 0, SFI, SizeOf(TSHFileInfo), Flags));
  {$ENDIF}

  If SysImageList <> 0 Then
  Begin
    If Not Assigned(ListView.LargeImages) Then
      ListView.LargeImages := TImageList.Create(Nil);
    If Not Assigned(ListView.SmallImages) Then
      ListView.SmallImages := TImageList.Create(Nil);
    ListView.LargeImages.Handle := SysImageList;
    ListView.LargeImages.ShareImages := True;
    ListView.SmallImages.Handle := SysImageList;
    ListView.SmallImages.ShareImages := True;
  End;
End;

// Открывает окно Проводника с выделенным заданным файлом
Function OpenExplorerAndSelectFile(Const Path: String): Boolean;
Begin
  Result := FileExists(Path);
  If Not Result Then
    Exit;

  ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Path + '"'), PChar(ExtractFilePath(Path)), SW_SHOWNORMAL);
End;

// Возвращает размер файла в байтах
Function GetMyFileSize(Const namefile: String): Int64;
Var
  InfoFile: TSearchRec;
Begin
  Result := -1;
  If FindFirst(namefile, faAnyFile, InfoFile) = 0 Then
  Begin
    Result := InfoFile.Size;
    FindClose(InfoFile);
  End;
End;

// Удаление в корзину
Function DeleteFileWithUndo(Const sFileName: String): boolean;
Var
  fos: TSHFileOpStruct;
  Buffer: Array[0..MAX_PATH] Of Char;
Begin
  Result := False;
  If Not FileExists(sFileName) Then
    Exit;

  StrPCopy(Buffer, sFileName);
  Buffer[Length(sFileName) + 1] := #0;

  FillChar(fos, SizeOf(fos), 0);
  With fos Do
  Begin
    wFunc := FO_DELETE;
    pFrom := Buffer;
    fFlags := FOF_ALLOWUNDO Or FOF_NOCONFIRMATION Or FOF_SILENT;
  End;

  Result := (ShFileOperation(fos) = 0);
End;

// Свойства файла и папки
Function ShowPropertiesDialog(Const FName: String): Boolean;
Var
  ExInfo: TShellExecuteInfo;
Begin
  FillChar(ExInfo, SizeOf(ExInfo), 0);
  ExInfo.cbSize := SizeOf(ExInfo);
  ExInfo.Wnd := Application.Handle;
  ExInfo.lpVerb := 'properties';
  ExInfo.lpFile := PChar(FName);
  ExInfo.nShow := SW_SHOWDEFAULT;
  ExInfo.fMask := SEE_MASK_INVOKEIDLIST;

  Result := ShellExecuteEx(@ExInfo);
End;

End.

