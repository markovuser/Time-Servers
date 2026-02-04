Unit Translation;

Interface

Uses
  Classes, ActnMan, ComCtrls, DBCtrls, ExtCtrls, Forms, SysUtils, TypInfo,
  StdCtrls, Buttons, Dialogs, Controls, IniFiles, Windows, checklst, fileCtrl;

Type
  TTranslation = Object
  Private
    FForm: TForm;
    Translated: TStringList;
    NotTranslated: TStringList;
    FSectionName: String; // Добавлено поле для хранения имени секции
  Public
    Procedure Execute(Form: TForm; Const SectionName: String);
    // Добавлен параметр SectionName
    Procedure CheckAndSetComponentPropertyString(Var s: String; Const PropName: String); Overload;
    Procedure CheckAndSetComponentPropertyString(Const prop: String; Var cm: TObject); Overload;
    Procedure CheckAndSetComponentPropertyStrings(prop: TStrings; Const PropName: String);
    Function GetString(Const CompName, Value: String): String;
  End;

Implementation

Uses
  Unit_Base, Unit_About;

Procedure TTranslation.CheckAndSetComponentPropertyString(Var s: String; Const PropName: String);
Var
  TempInt: integer;
  strValue: String;
Begin
  If (s <> '') And (s <> '-') And Not TryStrToInt(s, TempInt) Then
  Begin
    strValue := GetString(PropName, s);
    Application.ProcessMessages;
    If strValue <> '' Then
      s := strValue;
    Application.ProcessMessages;
  End;
End;

Procedure TTranslation.CheckAndSetComponentPropertyString(Const prop: String; Var cm: TObject);
Var
  TempInt: integer;
  strValue: String;
Begin
  If assigned(GetPropInfo(cm, prop)) Then
  Begin
    strValue := GetPropValue(cm, prop);
    Application.ProcessMessages;
    If (strValue <> '') And (strValue <> '-') And Not TryStrToInt(strValue, TempInt) Then
    Begin
      strValue := GetString(FForm.Name + '.' + (cm As TComponent).Name + '.' + prop, strValue);
      Application.ProcessMessages;
      If strValue <> '' Then
        SetPropValue(cm, prop, strValue);
      Application.ProcessMessages;
    End;
  End;
End;

Procedure TTranslation.CheckAndSetComponentPropertyStrings(prop: TStrings; Const PropName: String);
Var
  i: integer;
  TempInt: integer;
  Strings: TStringList;
  strValue: String;
Begin
  Strings := TStringList.Create;
  Application.ProcessMessages;
  Try
    Strings.Assign(prop);
    Application.ProcessMessages;
    For i := 0 To Strings.Count - 1 Do
    Begin
      strValue := Strings[i];
      Application.ProcessMessages;
      If (strValue <> '') And (strValue <> '-') And Not TryStrToInt(strValue, TempInt) Then
      Begin
        strValue := GetString(Format('%s[%d]', [PropName, i]), strValue);
        If strValue <> '' Then
          prop[i] := strValue;
        Application.ProcessMessages;
      End;
    End;
  Finally
    Strings.Free;
  End;
End;

Procedure TTranslation.Execute(Form: TForm; Const SectionName: String);
// Измененная сигнатура
Var
  lang_file, log_File: String;
  i, j, k, Tree: integer;
  cm: TObject;
  strValue: String;
  s, S_Tree, lang, Path: String;
  Ini: TMemIniFile;
Begin
  Try
    FSectionName := SectionName; // Сохраняем имя секции
    Ini := TMemIniFile.Create(Form1.PortablePath);
    lang := Ini.ReadString('Language', 'Language', '');
    lang_file := ExtractFilePath(ParamStr(0)) + 'Language\' + lang + '.ini';
    Path := ExtractFilePath(ParamStr(0));
    Path := ExtractFilePath(ExcludeTrailingPathDelimiter(Path));
    log_File := Path + Application.Title + '.log';
    If Not FileExists(lang_file) Then
      Exit;
    Translated := TStringList.Create;
    NotTranslated := TStringList.Create;

    // Загружаем только указанную секцию
    Ini.Free;
    Ini := TMemIniFile.Create(lang_file);
    Ini.CaseSensitive := False;
    Ini.ReadSectionValues(FSectionName, Translated);
    // Читаем только нужную секцию

    If FileExists(log_File) Then
    Begin
      NotTranslated.LoadFromFile(log_File);
    End;
    FForm := Form;
    If FForm <> Form1 Then
      Form.Caption := GetString(FForm.Name + '.Caption', Form.Caption);
    For i := 0 To Form.ComponentCount - 1 Do
    Begin
      cm := Form.Components[i];
      If cm Is TComboBox Then
        continue;
      If cm Is TStatusBar Then
        continue;
      If cm Is TTabSheet Then
        continue;
      If cm = Form8.LabelCopyright Then
        continue;
      CheckAndSetComponentPropertyString('Caption', cm);
      CheckAndSetComponentPropertyString('Hint', cm);
      CheckAndSetComponentPropertyString('Title', cm);
      CheckAndSetComponentPropertyString('Text', cm);
      CheckAndSetComponentPropertyString('TextHint', cm);
      Application.ProcessMessages;
      // TLabeledEdit
      If cm Is TLabeledEdit Then
      Begin
        strValue := (cm As TLabeledEdit).EditLabel.Caption;
        If (strValue <> '') And (strValue <> '-') Then
        Begin
          strValue := GetString(FForm.Name + '.' + (cm As TComponent).Name + '.Caption', strValue);
          Application.ProcessMessages;
          If strValue <> '' Then
          Begin
            (cm As TLabeledEdit).EditLabel.Caption := strValue
          End;
        End;
      End;
      // TActionManager
      If cm Is TActionManager Then
      Begin
        For j := 0 To TActionManager(cm).ActionBars.Count - 1 Do
          For k := 0 To TActionManager(cm).ActionBars[j].Items.Count - 1 Do
          Begin
            cm := TActionManager(cm).ActionBars[j].Items[k];
            Application.ProcessMessages;
          End;
      End;
      // TOpenDialog
      If cm Is TOpenDialog Then
      Begin
        CheckAndSetComponentPropertyString('Filter', cm);
        Application.ProcessMessages;
      End;
      // TSaveDialog
      If cm Is TSaveDialog Then
      Begin
        CheckAndSetComponentPropertyString('Filter', cm);
        Application.ProcessMessages;
      End;
      // TLabeledEdit
      If cm Is TLabeledEdit Then
      Begin
        cm := TLabeledEdit(cm).EditLabel;
        CheckAndSetComponentPropertyString('Caption', cm);
        Application.ProcessMessages;
      End;
      // TTreeView
      If cm Is TTreeView Then
      Begin
        For Tree := 0 To TTreeView(cm).Items.Count - 1 Do
        Begin
          S_Tree := TTreeView(cm).Items.Item[Tree].Text;
          CheckAndSetComponentPropertyString(S_Tree, FForm.Name + '.' + (cm As TComponent).Name + '.Items.Item[' + inttostr(Tree) + '].Text');
          TTreeView(cm).Items.Item[Tree].Text := S_Tree;
          Application.ProcessMessages;
        End;
      End;

      // TListView  Columns
      If cm Is TListView Then
      Begin
        For j := 0 To TListView(cm).Columns.Count - 1 Do
        Begin
          s := TListView(cm).Columns[j].Caption;
          CheckAndSetComponentPropertyString(s, FForm.Name + '.' + (cm As TComponent).Name + '.Columns[' + inttostr(j) + '].Caption');
          TListView(cm).Columns[j].Caption := s;
          Application.ProcessMessages;
        End;
      End;
      // TListView  Groups
      { if cm is TListView then
        begin
        for j := 0 to TListView(cm).Groups.Count - 1 do
        begin
        s := TListView(cm).Groups[j].Header;
        CheckAndSetComponentPropertyString(s,
        FForm.Name + '.' + (cm as TComponent).Name + '.Groups[' +
        inttostr(j) + '].Header');
        TListView(cm).Groups[j].Header := s;
        Application.ProcessMessages;
        end;
        end; }
      // TListView  items
      { if cm is TListView then
        begin
        for j := 0 to TListView(cm).Items.Count - 1 do
        begin
        s := TListView(cm).Items[j].Caption;
        CheckAndSetComponentPropertyString(s,
        FForm.Name + '.' + (cm as TComponent).Name + '.items[' + inttostr(j)
        + '].Caption');
        TListView(cm).Items[j].Caption := s;
        Application.ProcessMessages;
        end;
        end; }
      // TFilterComboBox
      If cm Is TFilterComboBox Then
      Begin
        CheckAndSetComponentPropertyString('Filter', cm);
        Application.ProcessMessages;
      End;
      // TTabControl
      If cm Is TTabControl Then
      Begin
        CheckAndSetComponentPropertyStrings((cm As TTabControl).Tabs, FForm.Name + '.' + (cm As TComponent).Name + '.Tabs ');
        Application.ProcessMessages;
      End;
      // TCheckListBox
      If cm Is TCheckListBox Then
      Begin
        CheckAndSetComponentPropertyStrings((cm As TCheckListBox).Items, FForm.Name + '.' + (cm As TComponent).Name + '.Items ');
        Application.ProcessMessages;
      End;
      // THeaderControl
      If cm Is THeaderControl Then
      Begin
        For j := 0 To THeaderControl(cm).sections.Count - 1 Do
        Begin
          s := THeaderControl(cm).sections[j].Text;
          CheckAndSetComponentPropertyString(s, FForm.Name + '.' + (cm As TComponent).Name + '.Text');
          THeaderControl(cm).sections[j].Text := s;
          Application.ProcessMessages;
        End;
      End;
    End;

    Translated.Free;
    Application.ProcessMessages;
    If FileExists(log_File) = true Then
    Begin
      If NotTranslated.Count > 0 Then
      Begin
        NotTranslated.SaveToFile(log_File, TEncoding.Unicode);
      End;
    End;
    NotTranslated.Free;
    Application.ProcessMessages;
  Except
  End;
End;

Function TTranslation.GetString(Const CompName, Value: String): String;
Var
  i: integer;

  Function IndexOf(Const Value: String): integer;
  Var
    i: integer;
  Begin
    Result := -1;
    For i := 0 To Translated.Count - 1 Do
      If AnsiSameText(Copy(Translated[i], 1, Pos('=', Translated[i]) - 1), Value) Then
      Begin
        Result := i;
        Break;
      End;
  End;

Begin
  i := IndexOf(CompName);
  If i = -1 Then
  Begin
    NotTranslated.Add(CompName + '=' + Value);
    Application.ProcessMessages;
    Result := Value;
  End
  Else
  Begin
    Result := Copy(Translated[i], Length(CompName) + 2, MaxInt);
    Translated.Delete(i);
    Application.ProcessMessages;
  End;
End;

End.

