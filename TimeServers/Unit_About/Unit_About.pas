Unit Unit_About;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, ExtCtrls, shellapi, ComCtrls, Vcl.Imaging.pngimage, FileInfoUtils;

Type
  TForm8 = Class(TForm)
    TabControlBody: TTabControl;
    LabelYaMail: TLabel;
    TabControlButtons: TTabControl;
    Button1: TButton;
    LabelVersion: TLabel;
    LabelCopyright: TLabel;
    LabelMainWebSite: TLabel;
    LabelMirrorWebSite: TLabel;
    LabelMirrorWebSite1: TLabel;
    LabelGMail: TLabel;
    LabelTelegram: TLabel;
    Image1: TImage;
    Procedure LabelYaMailClick(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure LabelMainWebSiteClick(Sender: TObject);
    Procedure LabelMirrorWebSiteClick(Sender: TObject);
    Procedure LabelMirrorWebSite1Click(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure LabelGMailClick(Sender: TObject);
    Procedure LabelTelegramClick(Sender: TObject);

  Private
  Public
    { Public declarations }
  Protected
    Procedure CreateParams(Var Params: TCreateParams); Override;
  Protected
  End;

Var
  Form8: TForm8;
  X, Y: Integer;

Implementation

Uses
  Unit_Base;
{$R *.dfm}

Procedure TForm8.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle Or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
End;

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  Image1.Picture.Icon.Handle := LoadIcon(hInstance, 'MAINICON');
  LabelCopyright.Caption := GetFileCopyright(ParamStr(0));
End;

Procedure TForm8.FormShow(Sender: TObject);
Begin
  LabelVersion.Caption := Form1.Caption;
End;

Procedure TForm8.LabelGMailClick(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'mailto:markov.user@gmail.com', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.LabelMainWebSiteClick(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'https://sites.google.com/view/simpleutilities/index', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.LabelMirrorWebSite1Click(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'https://github.com/markovuser?tab=repositories', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.LabelMirrorWebSiteClick(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'v95240cd.beget.tech/index.html', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.LabelTelegramClick(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'https://t.me/markovuser', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.LabelYaMailClick(Sender: TObject);
Begin
  ShellExecute(Handle, 'open', 'mailto:markov.user@yandex.ru', Nil, Nil, 5);
  application.ProcessMessages;
End;

Procedure TForm8.Button1Click(Sender: TObject);
Begin
  Form8.Close;
End;

End.

