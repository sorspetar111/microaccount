unit pref_u;

{*
Program: DFMEdit
Unit: pref_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Functions for loading main-config

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

procedure ReadConfig;

implementation

uses
  forms,sysutils,dialogs,classes,controls,inifiles,classes_u,functions_u,main_u,menus,localize_u,
  options_u,preview_u,palette_u;

procedure ReadConfig;
var ini:Tinifile;
    s:string;
    i,l,t,h,w:integer;
    sl:TStringlist;
    c:TComponent;
begin
  ini:=Tinifile.Create(conffile);
  Form_DFMPalette.loadSettings(Form_DFMPalette.imagelist1,Form_DFMPalette.pagecontrol1);
  cfg.language:=ini.ReadString('main','language','de');
  cfg.BinForm:=ini.ReadBool('main','BinForm',Form_DFMMain.TB_BinForm.down);
  cfg.ShowHidden:=ini.ReadBool('main','ShowHidden',false);
  cfg.EnableDisabled:=ini.ReadBool('main','EnableDisabled',false);
  cfg.ShowToolbar:=ini.ReadBool('main','ShowToolbar',true);
  cfg.ShowMainMenu:=ini.ReadBool('main','ShowMainMenu',false);
  //prevent displaying no Menu
  if not cfg.ShowToolbar and not cfg.ShowMainMenu then cfg.ShowToolbar:=true;

  Form_DFMMain.MI_BinForm.checked:=cfg.BinForm;
  Form_DFMMain.MI_ShowHidden.checked:=cfg.ShowHidden;
  Form_DFMMain.MI_EnableDisabled.checked:=cfg.EnableDisabled;
  Form_DFMMain.MI_BinFormClick(nil);
  if not cfg.ShowMainMenu then Form_DFMMain.Menu:=nil;
  Form_DFMMain.Toolbar1.Visible:=cfg.ShowToolbar;

  cfg.OpenCreatedPas:=ini.ReadBool('main','OpenCreatedUnits',false);
  cfg.OpenModifiedPas:=ini.ReadBool('main','OpenModifiedUnits',false);
  cfg.AskForOpen:=ini.ReadBool('main','AskForOpen',false);
  cfg.ExtEditor:=ini.ReadString('main','External','');


  cfg.GridSize:=ini.ReadInteger('main','GridSize',4);
  cfg.DrawGrid:=(cfg.gridsize>1) and ini.readBool('main','DrawGrid',false);
  cfg.SnapToGrid:=ini.ReadBool('main','SnapToGrid',false);
  
  cfg.ChangeFocus:=ini.ReadBool('main','ChangeFocus',true);
  cfg.IgnoreFormMove:=ini.ReadBool('main','IgnoreFormMove',false);
  cfg.UseDllForSaving:=ini.readBool('main','UseDllForSaving',false);
  Form_DFMMain.MI_UseDllForSaving.checked:=cfg.UseDllForSaving;
  Form_DFMMain.MI_UseDllForSaving2.checked:=cfg.UseDllForSaving;

  cfg.BackupCount:=ini.ReadInteger('main','BackupCount',1);
  cfg.BackupDir:=ini.ReadString('main','BackupDir','_bak_');

  cfg.lazres:=ini.ReadString('main','lazres','');
  if cfg.lazres='' then cfg.lazres:='lazres.exe';
  cfg.lazres:=getFullPath(cfg.lazres);
  cfg.lazunitext:=ini.ReadString('main','LazUnitExt','pas');
  {$IFNDEF NOPACKAGES}
  for i:=0 to 9 do
  begin
    s:=GetFullPath(ini.readString('packages',inttostr(i),''));
    cfg.packages[i].handle:=0;
    cfg.packages[i].filename:='';
    if s<>'' then
    begin
      cfg.packages[i].filename:=s;
      if fileexists(s) then
      begin
        cfg.packages[i].handle:=loadpackage(s);
        if cfg.packages[i].handle=0 then
          showmessage(format(localizeString('Msg_ErrorLoadingPackage'),[s]));
      end else
        Showmessage(format(localizeString('Msg_PackageNotFound'),[s]));
    end;
  end;
  {$ENDIF}
  //Position and Size
  for i:=0 to screen.FormCount-1 do
  begin
    s:=copy(screen.forms[i].name,9,length(screen.forms[i].name)-8);
    if (screen.forms[i]<>Form_DFMPreview) and (screen.forms[i].tag<>0) and
      (screen.forms[i].tag<Form_DFMOptions.StringGrid1.RowCount) then
    begin
      cfg.FormSizes[1,screen.forms[i].tag]:=-1;
      cfg.FormSizes[2,screen.forms[i].tag]:=-1;
      cfg.FormSizes[3,screen.forms[i].tag]:=-1;
      cfg.FormSizes[4,screen.forms[i].tag]:=-1;
      l:=ini.ReadInteger('Position',s+'_Left',-1);
      t:=ini.ReadInteger('Position',s+'_Top',-1);
      if l>-1 then
      begin
        cfg.FormSizes[1,screen.forms[i].tag]:=l;
        screen.forms[i].left:=l;
      end;
      if t>-1 then
      begin
        cfg.FormSizes[2,screen.forms[i].tag]:=t;
        screen.forms[i].top:=t;
      end;
      if (screen.forms[i].borderStyle in [bsSizeable,bsSizeToolWin]) then
      begin
        h:=ini.ReadInteger('Position',s+'_Height',-1);
        w:=ini.ReadInteger('Position',s+'_Width',-1);
        if h>-1 then
        begin
          cfg.FormSizes[3,screen.forms[i].tag]:=h;
          screen.forms[i].height:=h;
        end;
        if w>-1 then
        begin
          cfg.FormSizes[4,screen.forms[i].tag]:=w;
          screen.forms[i].width:=w;
        end;
      end;
    end;
  end;
  //loading Plugins here

  //change visibility of toolbar-buttons and MainMenu-items
  sl:=TStringlist.create;
  ini.ReadSection('Hidden',sl);
  for i:=0 to sl.Count-1 do
  begin
    c:=Form_DFMMain.FindComponent(sl.strings[i]);
    if assigned(c) then
    begin
      if c is TMenuItem then
        (c as TMenuItem).visible:=not ini.ReadBool('Hidden',sl.strings[i],true)
      else if c is TControl then
        (c as TControl).visible:=not ini.ReadBool('Hidden',sl.strings[i],true)
    end;
  end;
  sl.free;
  ini.free;
  cfg.FileHistory.Load;
  Form_DFMMain.UpdateToolbar;
  setLanguage(cfg.Language);
end;


end.
