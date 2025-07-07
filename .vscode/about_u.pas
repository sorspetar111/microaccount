unit about_u;

{*
Program: DFMEdit
Unit: about_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
simple About-Box

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, shellapi;

type
  TForm_DFMAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Lbl_ProjectPage: TLabel;
    Lbl_EMail: TLabel;
    Lbl_Homepage: TLabel;
    Lbl_TranslationBy: TLabel;
    Btn_OK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LinkLabelClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form_DFMAbout: TForm_DFMAbout;

implementation

{$R *.DFM}

uses Version_u,classes_u;

procedure TForm_DFMAbout.FormCreate(Sender: TObject);
var vi:TVersionInfo;
begin
  image1.Picture.Icon.Assign(application.icon);
  vi:=GetFileVersion(paramstr(0));
  caption:='DFM-Editor V'+vi.FVersion;
end;

procedure TForm_DFMAbout.LinkLabelClick(Sender: TObject);
begin
  shellexecute(0,'open',PCHAR((sender as TLabel).hint),'','',sw_show);
end;

end.
