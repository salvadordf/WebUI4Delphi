unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  uWebUIWindow;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    FWindow : TWebUIWindow;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uWebUI;

procedure TForm1.Button1Click(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  if WebUI.Initialize then
    showmessage('success')
   else
    showmessage('failure');

  FWindow := TWebUIWindow.Create;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FreeAndNil(FWindow);
  DestroyWebUI;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  //FWindow.Show('<html><head><script src="webui.js"></script></head> Hello World ! </html>');
  FWindow.Show('https://www.bing.com');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Edit1.Text := FWindow.Url;
end;

end.
