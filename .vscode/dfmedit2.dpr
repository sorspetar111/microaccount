program dfmedit2;

{*
Program: DFMEdit
Unit: dfmedit.dpr
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
DFMEdit-Projectfile

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

uses
  FastMM4 in 'FastMM4.pas',
  Forms,
  XPMan,
  main_u in 'main_u.pas' {Form_DFMMain},
  SizeControl in 'components\sizecontrol\SizeControl.pas',
  preview_u in 'preview_u.pas' {Form_DFMPreview},
  addproperty_u in 'addproperty_u.pas' {Form_DFMAdd},
  dfmparse_u in 'dfmparse_u.pas',
  Classes_u in 'Classes_u.pas',
  Version_u in 'Version_u.pas',
  localize_u in 'localize_u.pas',
  pref_u in 'pref_u.pas',
  about_u in 'about_u.pas' {Form_DFMAbout},
  chooseRes_u in 'chooseRes_u.pas' {Form_DFMChooseRes},
  functions_u in 'functions_u.pas',
  unit_u in 'unit_u.pas',
  inspector_u in 'inspector_u.pas' {Form_DFMInspector},
  rttifunctions_u in 'rttifunctions_u.pas',
  options_u in 'options_u.pas' {Form_DFMOptions},
  MenuEdit_u in 'MenuEdit_u.pas',
  palette_u in 'palette_u.pas' {Form_DFMPalette},
  dialogs_u in 'dialogs_u.pas',
  convert_u in 'convert_u.pas';

{$R *.RES}
{$R versioninfo.res}

begin
  Application.Initialize;
  Recreate:=false;
  Application.CreateForm(TForm_DFMMain, Form_DFMMain);
  Application.CreateForm(TForm_DFMAbout, Form_DFMAbout);
  Application.CreateForm(TForm_DFMChooseRes, Form_DFMChooseRes);
  Application.CreateForm(TForm_DFMInspector, Form_DFMInspector);
  Application.CreateForm(TForm_DFMOptions, Form_DFMOptions);
  Application.CreateForm(TForm_DFMPalette, Form_DFMPalette);
  loading:=true;
  Application.CreateForm(TForm_DFMPreview, Form_DFMPreview);
  Application.CreateForm(TForm_DFMAdd, Form_DFMAdd);
  readconfig;
  loading:=false;
  Application.Run;
end.
