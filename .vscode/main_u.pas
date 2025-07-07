unit main_u;

{*
Program: DFMEdit
Unit: main_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Main-Form/Functions

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ExtCtrls, checklst, Spin, Menus, typinfo, shellapi,
  ToolWin, commctrl{$IFDEF DELPHI4_UP}, ImgList{$ENDIF}, ExtDlgs,

  Classes_u, Version_u, localize_u, dialogs_u;

type
  TForm_DFMMain = class(TForm)
    //toolbar
    MainMenu1: TMainMenu;
      MI_File: TMenuItem;
        MI_NewForm: TMenuItem;
        MI_Open: TMenuItem;
        MI_LastFiles: TMenuItem;
        N1: TMenuItem;
        MI_UseDllForSaving2: TMenuItem;
        N10: TMenuItem;
        MI_Save2: TMenuItem;
        MI_SaveAs2: TMenuItem;
        MI_SaveOutput2: TMenuItem;
        MI_SaveOutputAs2: TMenuItem;
        MI_SaveOutputToBinary2: TMenuItem;
        N2: TMenuItem;
        MI_LoadResource: TMenuItem;
        MI_SaveToBinary2: TMenuItem;
        MI_DFMConvert: TMenuItem;
        N3: TMenuItem;
        MI_Exit: TMenuItem;
      MI_Edit2: TMenuItem;
        MI_AddObject2: TMenuItem;
        MI_AddProperty2: TMenuItem;
        MI_AddEntry2: TMenuItem;
        MI_Edit3: TMenuItem;
        MI_Delete2: TMenuItem;
        N8: TMenuItem;
        MI_MoveUp: TMenuItem;
        MI_MoveDown: TMenuItem;
        MI_MoveTop: TMenuItem;
        MI_MoveBottom: TMenuItem;
      MI_Options: TMenuItem;
        MI_BinForm: TMenuItem;
        MI_ShowHidden: TMenuItem;
        MI_EnableDisabled: TMenuItem;
        N6: TMenuItem;
        MI_Options2: TMenuItem;
      MI_Tools: TMenuItem;
        MI_GenPreview: TMenuItem;
        MI_Preview: TMenuItem;
        N7: TMenuItem;
        MI_CreatePas: TMenuItem;
        MI_GenRTC: TMenuItem;
        MI_MenuEditor2: TMenuItem;
        MI_RemoveUnwanted: TMenuItem;
      MI_Question: TMenuItem;
        MI_Help3: TMenuItem;
        MI_About2: TMenuItem;
        N4: TMenuItem;
        MI_CheckVersion: TMenuItem;
          MI_LastStable2: TMenuItem;
          MI_LastBeta2: TMenuItem;
        N5: TMenuItem;
        MI_Request2: TMenuItem;
    ToolBar1: TToolBar;
      TS_1: TToolButton;
      TB_NewForm: TToolButton;
      TB_Open: TToolButton;
      TB_DFMConvert: TToolButton;
      TB_LoadResource: TToolButton;
      TS_2: TToolButton;
      TB_BinForm: TToolButton;
      TB_Save: TToolButton;
      TB_CreatePas: TToolButton;
      TB_GenRTC: TToolButton;
      TS_3: TToolButton;
      TB_GenPreview: TToolButton;
      TB_Preview: TToolButton;
      TB_ShowHidden: TToolButton;
      TB_EnableDisabled: TToolButton;
      TB_AddObject: TToolButton;
      TB_AddProperty: TToolButton;
      TB_AddEntry: TToolButton;
      TB_Edit: TToolButton;
      TB_Delete: TToolButton;
      TS_4: TToolButton;
      TB_MoveUp: TToolButton;
      TB_MoveDown: TToolButton;
      TB_MoveTop: TToolButton;
      TB_MoveBottom: TToolButton;
      TS_5: TToolButton;
      TB_MenuEditor: TToolButton;
      TB_RemoveUnwanted: TToolButton;
      TS_6: TToolButton;
      TB_CheckVersion: TToolButton;
      TB_About: TToolButton;
      TS_7: TToolButton;
      TB_Options: TToolButton;

    PageControl1: TPageControl;
      Tab_Source: TTabSheet;
        Pnl_Source: TPanel;
          Btn_ImportDFMSrc: TButton;
        Memo_Source: TMemo;
      Tab_Tree: TTabSheet;
        Pnl_Treeview: TPanel;
          CB_Objects: TComboBox;
        TreeView1: TTreeView;
      Tab_Output: TTabSheet;
        Memo_Output: TMemo;

    StatusBar1: TStatusBar;
      ProgressBar1: TProgressBar;

    Pnl_Right: TPanel;
      GrpBox_Object: TGroupBox;
        Pnl_Object: TPanel;
          Lbl_ObjectKind: TLabel;
          Lbl_ObjectName: TLabel;
          Lbl_ObjectType: TLabel;
          CB_ObjectType: TComboBox;
          Edit_Name: TEdit;
          Edit_Type: TEdit;
        GrpBox_Property: TGroupBox;
          Pnl_Property: TPanel;
            Lbl_PropName: TLabel;
            Lbl_PropValue: TLabel;
            Edit_PropName: TEdit;
            Edit_Value: TEdit;
          GrpBox_Special: TGroupBox;
            Memo_Special: TMemo;
        Pnl_Apply: TPanel;
          Btn_Apply: TButton;
      PageControl2: TPageControl;
        Tab_Search: TTabSheet;
          Edit_SearchText: TEdit;
          Btn_SearchText: TButton;
          SpinEdit1: TEdit;
          UpDown1: TUpDown;
          Btn_GotoLine: TButton;
          LineLabel: TLabel;
        Tab_Replace: TTabSheet;
          Lbl_Search: TLabel;
          Lbl_Replace: TLabel;
          Edit_Search: TEdit;
          Edit_Replace: TEdit;
          Btn_ReplaceAll: TButton;
        Tab_Search2: TTabSheet;
          Edit_FindNode: TEdit;
          Btn_SearchText2: TButton;
    Popup_Treeview: TPopupMenu;
      MI_AddObject: TMenuItem;
      MI_AddProperty: TMenuItem;
      MI_AddEntry: TMenuItem;
      MI_Edit: TMenuItem;
      MI_Delete: TMenuItem;
      MI_MenuEditor: TMenuItem;
    Popup_Toolbar: TPopupMenu;
      MI_LastStable: TMenuItem;
      MI_LastBeta: TMenuItem;
      MI_About: TMenuItem;
      MI_Help: TMenuItem;
      MI_Request: TMenuItem;
      MI_UseDllForSaving: TMenuItem;
      N9: TMenuItem;
      MI_Save: TMenuItem;
      MI_SaveAs: TMenuItem;
      MI_SaveOutput: TMenuItem;
      MI_SaveOutputAs: TMenuItem;
      MI_SaveToBinary: TMenuItem;
      MI_SaveOutputToBinary: TMenuItem;
    Popup_History: TPopupMenu;
    IL_Toolbar: TImageList;
    IL_TreeView: TImageList;

    Pnl_Debug: TPanel;

    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose:boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    //MainMenu
    procedure MI_LastFileClick(Sender: TObject);
    procedure MI_ExitClick(Sender: TObject);
    procedure MI_UseDllForSavingClick(Sender: TObject);
    procedure MI_BinFormClick(Sender: TObject);
    //toolbar
    procedure TB_NewFormClick(Sender: TObject);
    procedure TB_OpenClick(Sender: TObject);
    procedure TB_DFMConvertClick(Sender: TObject);
    procedure TB_LoadResourceClick(Sender: TObject);
    procedure TB_BinFormClick(Sender: TObject);
    procedure TB_SaveClick(Sender: TObject);
    procedure MI_SaveToBinaryClick(Sender: TObject);
    procedure TB_CreatePasClick(Sender: TObject);
    procedure TB_GenRTCClick(Sender: TObject);
    procedure TB_GenPreviewClick(Sender: TObject);
    procedure TB_AddObjectClick(Sender: TObject);
    procedure TB_AddPropertyClick(Sender: TObject);
    procedure TB_AddEntryClick(Sender: TObject);
    procedure TB_EditClick(Sender: TObject);
    procedure TB_DeleteClick(Sender: TObject);
    procedure TB_MoveClick(Sender: TObject);
    procedure TB_MenuEditorClick(Sender: TObject);
    procedure TB_RemoveUnwantedClick(Sender: TObject);
    procedure TB_CheckVersionClick(Sender: TObject);
    procedure TB_AboutClick(Sender: TObject);
    procedure MI_RequestClick(Sender: TObject);
    procedure TB_OptionsClick(Sender: TObject);
    //main-PageControl
    procedure PageControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure Pnl_SourceResize(Sender: TObject);
    procedure Btn_ImportDFMSrcClick(Sender: TObject);
    procedure MemoClick(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure MemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Pnl_TreeviewResize(Sender: TObject);
    procedure CB_ObjectsChange(Sender: TObject);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1DblClick(Sender: TObject);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo_OutputChange(Sender: TObject);
    //GrpBox_Object
    procedure Edit_NameChange(Sender: TObject);
    procedure Btn_ApplyClick(Sender: TObject);
    //Search-PageControl
    procedure Btn_GotoLineClick(Sender: TObject);
    procedure Btn_SearchTextClick(Sender: TObject);
    procedure SearchBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Btn_ReplaceAllClick(Sender: TObject);
    procedure Btn_SearchText2Click(Sender: TObject);

    procedure StatusBar1Resize(Sender: TObject);

    procedure Popup_TreeviewPopup(Sender: TObject);
    procedure Popup_ToolbarPopup(Sender: TObject);
    //pnl_debug
    procedure Pnl_DebugDblClick(Sender: TObject);
    procedure Pnl_DebugMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
    selNode:TTreenode;
    DropButton: Integer;//index of toolbutton for dropdown-menu (>D3)
    FileToLoad:string;
    function GetCurrentMemo:TMemo;
    procedure AppException(Sender: TObject; E: Exception);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DFMCallback(percent:integer;Text:String);
    procedure disableClose(disable:Boolean);
    procedure Generate(usage:byte);
    procedure LoadDFM;
    procedure LoadDFMFile(filename:string);
    procedure LoadResourceForm(filename:string);
    procedure ShowCurrentLine(memo:Tmemo);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMGetMinMaxInfo(var Msg: TMessage); Message WM_GETMINMAXINFO;
    Procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    procedure WMRecreateForms(var Msg: TMessage); message WM_RecreateForms;
  public
    { Public-Deklarationen }
    NodeChanged:Boolean;
    procedure UpdateToolbar;
  end;

var
  Form_DFMMain: TForm_DFMMain;

implementation

uses dfmparse_u,preview_u,addproperty_u,pref_u, rttifunctions_u, functions_u, about_u, chooseRes_u, unit_u,
  inspector_u, options_u, palette_u;

{$R *.DFM}
//help-functions
procedure renameNode(node:TTreenode);
var p:integer;
begin
  //nur bei temporärem name (nach nodecopy) !!!
  if isObject(node) then
  begin
    ChangeObjectName(node,GetNextObjectNumber(GetObjectName(node)));
    p:=pos(' ',node.Text);
    form_DFMMain.CB_Objects.Items.Add(copy(node.text,p+1,length(node.text)-p));
  end;
end;

//private
function TForm_DFMMain.GetCurrentMemo:TMemo;
begin
  if PageControl1.ActivePage=Tab_Source then
    result:=Memo_Source
  else
    result:=Memo_Output;
end;

procedure TForm_DFMMain.AppException(Sender: TObject; E: Exception);
var s:string;
    i:integer;
    vi:TVersioninfo;
begin
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  if e is eParserError then
  begin
    //code for getting linenumber from errormessage
    pagecontrol1.ActivePage:=Tab_Output;
    pagecontrol1change(self);
    s:=e.Message;
    i:=length(s);
    while s[i] in ['0'..'9'] do dec(i);
    spinedit1.text:=(copy(s,i+1,length(s)-i));
    Btn_GotoLineClick(self);
  end;
  if MessageBox(handle,PCHAR(e.Message+' ('+e.ClassName+')'+#13#13+LocalizeString('Ask_ReportBug')),PCHAR(LocalizeString('Cap_Error')),MB_IconError or MB_YesNO)=mrYes then
  begin
    vi:=GetFileVersion(paramstr(0));
    shellexecute(hinstance,'open',PChar(format(page_request+'&caption='+e.Message+' ('+e.ClassName+')',[VersionToStr(vi),cfg.language])),'','',sw_shownormal);
    //'http://www.fw-web.de/scripts/support.php?app=dfmedit&version='+VersionToStr(vi)+'&lang='+cfg.language+'&caption='+e.Message+' ('+e.ClassName+')'),'','',sw_shownormal);
  end;
end;

procedure TForm_DFMMain.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.message=WM_MOUSEWHEEL then
  begin
    if screen.ActiveForm=Form_DFMInspector then
    begin
      Sendmessage(Form_DFMInspector.Handle,msg.Message,msg.wParam,msg.lParam);
      Handled:=true;
    end;
  end;
end;

procedure TForm_DFMMain.DFMCallback(percent:integer;Text:String);
begin
  ProgressBar1.Position:=percent;
  statusbar1.panels[1].text:=inttostr(percent)+' %';
  application.ProcessMessages;
end;

procedure TForm_DFMMain.disableClose(disable:Boolean);
var
  m,m2: HMENU;
  value:integer;
begin
  if disable then value:=(MF_DISABLED or MF_GRAYED) else value:=0;
  m := GetSystemMenu(Handle, False); //menu for window
  m2:= GetSystemMenu(application.Handle, False); //menu for taskbar
  if (m <> 0) then begin
    if (Integer(EnableMenuItem(m, SC_CLOSE,
                MF_BYCOMMAND or value)) >= 0) then begin
    end;
  end;
  if (m2 <> 0) then begin
    if (Integer(EnableMenuItem(m2, SC_CLOSE,
                MF_BYCOMMAND or value)) >= 0) then begin
    end;
  end;
  //deactivate alt+f4
  if disable then
    SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) or CS_NOCLOSE)
  else
    SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not CS_NOCLOSE);

  drawMenuBar(handle);
  drawMenuBar(application.handle);
end;

procedure TForm_DFMMain.Generate(usage:byte);
var time:integer;
begin
  startPerfCounter(time);
  //Memo_Output.Lines.BeginUpdate;
  progressbar1.Visible:=true;
  Pnl_Right.Enabled:=false;
  Pagecontrol1.Enabled:=false;
  Toolbar1.Enabled:=false;
  DisableClose(true);
  screen.cursor:=crHourglass;
  memo_output.lines.text:=dfmparse_u.Generate(treeview1,usage,TB_ShowHidden.Down,TB_EnableDisabled.Down,dfmcallback);
  screen.cursor:=crDefault;
  DisableClose(False);
  Pnl_Right.Enabled:=True;
  Toolbar1.Enabled:=True;
  Pagecontrol1.Enabled:=true;
  progressbar1.Visible:=false;
  //Memo_Output.Lines.EndUpdate;
  time:=stopPerfCounter(time);
  statusbar1.panels[1].text:=statusbar1.panels[1].text+' - '+formatfloat('0.00', time / 1000) + ' sek';
end;

procedure TForm_DFMMain.LoadDFM;
var time:integer;
begin
  if memo_source.Lines.Count>0 then
  begin
    try
      startPerfCounter(time);
      progressbar1.Visible:=true;
      screen.cursor:=crHourglass;
      dfmparse_u.LoadDfm(treeview1,memo_source.lines,CB_Objects.Items,dfmcallback);
      screen.cursor:=crDefault;
      progressbar1.Visible:=false;
      time:=stopPerfCounter(time);
      statusbar1.panels[1].text:=statusbar1.panels[1].text+' - '+formatfloat('0.00', time / 1000) + ' sek';
    finally
      Memo_output.lines.Clear;
      UpdateToolbar;
      filechanged:=false;
    end;
  end;
end;

procedure TForm_DFMMain.LoadDFMFile(filename:string);
begin
  if pos('\',filename)=0 then
    filename:=GetCurrentDir+'\'+filename;
  TB_BinForm.Down:=dfmparse_u.LoadDFMFile(filename,memo_source.lines);
  if memo_source.lines.Text<>'' then //on file-access-error these lines should be empty
  begin
    cfg.FileHistory.addFile(filename);
    LoadedFile:=filename;
    Statusbar1.Panels[0].Text:=LoadedFile;
    Statusbar1.Hint:=LoadedFile;
    dt:=dtFile;
    LoadDfm;
  end;
end;

procedure TForm_DFMMain.LoadResourceForm(filename:string);
begin
  memo_Source.lines.clear;
  Form_DFMChooseRes.ChooseForm(filename,Memo_Source.Lines);
  if memo_Source.lines.count>0 then
  begin
    dt:=dtResource;
    LoadDfm;
  end;
end;

procedure TForm_DFMMain.ShowCurrentLine(memo:Tmemo);
begin
  linelabel.caption:=LocalizeString('Cap_Line')+':'+inttostr(getcurrentLine(memo))+' ('+inttostr(getcurrentCol(memo))+')';
end;

procedure TForm_DFMMain.UpdateToolbar;
var r:TDatarec;
begin
  selNode:=Treeview1.Selected;
  r:=nil;
  if assigned(selnode) and assigned(selNode.Data) then
    r:=TDataRec(selnode.data);
  TB_CreatePas.enabled:=(treeview1.Items.Count>0) and (dt=dtFile);
  TB_GenPreview.Enabled:=treeview1.Items.Count>0;
  TB_Preview.Enabled:=(treeview1.Items.Count>0) and (Memo_output.text<>'');
  TB_AddProperty.enabled:=assigned(selNode) and (isObject(selNode) or isItem(selNode));
  TB_AddObject.enabled:=(assigned(selNode) and isObject(selNode));
  TB_AddEntry.enabled:=assigned(selNode) and assigned(r) and (r.Kind=dkCollection);//(copy(selNode.Text,length(selNode.Text),1)='>');
  TB_Delete.Enabled:=assigned(selNode);
  TB_Edit.Enabled:=assigned(selNode) and not isItem(SelNode);
  TB_Preview.Enabled:=Memo_output.Text<>'';
  TB_MoveUp.enabled:=assigned(selNode);
  TB_MoveDown.enabled:=assigned(selNode);
  TB_MoveTop.enabled:=assigned(selNode);
  TB_MoveBottom.enabled:=assigned(selNode);

  TB_GenRTC.Enabled:=assigned(selNode) and isObject(selNode);
  TB_RemoveUnwanted.enabled:=Treeview1.items.count>0;
  TB_MenuEditor.enabled:=isObject(selNode) and (lowercase(copy(selNode.Text,length(selNode.Text)-3,4))='menu');

  TB_Save.Enabled:=(treeview1.Items.Count>0) or (Memo_output.Text<>'');
  MI_SaveToBinary.Enabled:=(dt=dtResource);
  MI_SaveOutputToBinary.Enabled:=(dt=dtResource) and (Memo_output.Text<>'');
  MI_Save.enabled:=treeview1.Items.Count>0;
  MI_SaveAs.enabled:=treeview1.Items.Count>0;
  MI_SaveOutput.enabled:=Memo_output.Text<>'';
  MI_SaveOutputAs.enabled:=Memo_output.Text<>'';

  //mainMenu
  MI_CreatePas.enabled:=TB_CreatePas.enabled;
  MI_GenPreview.Enabled:=TB_GenPreview.Enabled;
  MI_Preview.Enabled:=TB_Preview.Enabled;
  MI_AddProperty2.enabled:=TB_AddProperty.enabled;
  MI_AddObject2.enabled:=TB_AddObject.enabled;
  MI_AddEntry2.enabled:=TB_AddEntry.enabled;
  MI_Delete2.Enabled:=TB_Delete.Enabled;
  MI_Edit3.Enabled:=TB_Edit.Enabled;
  MI_Preview.Enabled:=TB_Preview.Enabled;
  MI_MoveUp.enabled:=TB_MoveUp.enabled;
  MI_MoveDown.enabled:=TB_MoveDown.enabled;
  MI_MoveTop.enabled:=TB_MoveTop.enabled;
  MI_MoveBottom.enabled:=TB_MoveBottom.enabled;

  MI_GenRTC.Enabled:=TB_GenRTC.Enabled;
  MI_MenuEditor2.enabled:=TB_MenuEditor.enabled;
  MI_RemoveUnwanted.enabled:=TB_RemoveUnwanted.enabled;

  MI_SaveToBinary2.Enabled:=MI_SaveToBinary.Enabled;
  MI_SaveOutputToBinary2.Enabled:=MI_SaveOutputToBinary.Enabled;
  MI_Save2.enabled:=MI_Save.enabled;
  MI_SaveAs2.enabled:=MI_SaveAs.enabled;
  MI_SaveOutput2.enabled:=MI_SaveOutput.enabled;
  MI_SaveOutputAs2.enabled:=MI_SaveOutputAs.enabled;
  Form_DFMMain.MI_LastFiles.Enabled:=Form_DFMMain.MI_LastFiles.Count>0;
  if cfg.FileHistory.Count>0 then
    Form_DFMMain.TB_Open.Style:=tbsDropDown
  else
    Form_DFMMain.TB_Open.Style:=tbsButton;
end;

procedure TForm_DFMMain.WMDropFiles(var msg : TWMDropFiles);
var
   PFileName: Array [0 .. 255] of char;
   ext:string;
   count:integer;
begin
  Count := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  if count>0 then
  begin
    DragQueryFile(Msg.Drop, 0, PFileName, 255);
    ext:=uppercase(extractfileext(pFilename));
    if (ext='.EXE') or (ext='.DLL') then
      LoadResourceForm(pfilename)
    else
      LoadDFMFile(PFileName);
  end;
  DragFinish(Msg.Drop);
  Msg.Result := 0;
end;

Procedure TForm_DFMMain.WMGetMinMaxInfo(Var Msg: TMessage);
Begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize:= Point(Main_MinWidth, Main_MinHeight);
End;

Procedure TForm_DFMMain.WMNotify(Var Msg: TWMNotify);
Begin
  If Msg.NMHdr^.code=TBN_DROPDOWN Then
    DropButton := PNMTOOLBAR(Msg.NMHdr)^.iItem;
  inherited;
End;

//component events
procedure TForm_DFMMain.FormActivate(Sender: TObject);
begin
  if FileToLoad<>'' then
  begin
    application.ProcessMessages;
    LoadDFMFile(FileToLoad);
    FileToLoad:='';
  end;
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  BringtoFront;
end;

procedure TForm_DFMMain.FormCreate(Sender: TObject);
var vi:TVersionInfo;
  capExt:string;
  MA:TMenuArray;
begin
  Application.OnException := AppException;
  Application.OnMessage:=AppMessage;
  ConfFile:=extractfilepath(paramstr(0))+_ConfFile;
  CompFile:=extractfilepath(paramstr(0))+_CompFile;
  PropFile:=extractfilepath(paramstr(0))+_PropFile;
  UnwantedFile:=extractfilepath(paramstr(0))+_UnwantedFile;
  progressbar1.Width:=Statusbar1.width-Statusbar1.Panels[0].width-60;
  cfg.FileHistory:=TFileHistory.Create;
  ma[0]:=MI_LastFiles;
  ma[1]:=Popup_History.Items;
  cfg.FileHistory.setmenus(ma,MI_LastFileClick);
  vi:=GetFileVersion(paramstr(0));
  capExt:='';
  {$IfDef TestBuild}
  capExt:=' beta';
  {$EndIf}
  DragAcceptfiles(Form_DFMMain.handle,true);
  progressbar1.Parent:=Statusbar1;
  progressbar1.top:=4;
  PageControl1Change(self);
  cfg.DebugMode:=(lowercase(ParamStr(1))='/debug');
  if cfg.DebugMode and (pnl_debug.ControlCount>0) then
  begin
    Pnl_Debug.Visible:=true;
    capExt:=capExt+' [debugmode]';
  end;
  if not recreate then
  begin
    dt:=dtNew;
    LoadedFile:='';
    Statusbar1.Hint:='';
    if (paramCount>0) then
    begin
      if fileExists(ParamStr(paramCount)) then
        FileToLoad:=ParamStr(paramCount);
    end;
  end else PostMessage(self.handle,WM_RecreateForms,0,0);//timer1.enabled:=true;
  Caption:=Caption+' (V'+VersionToStr(vi)+capExt+')';
end;

procedure TForm_DFMMain.FormCloseQuery(Sender: TObject; var CanClose:boolean);
var res:Word;
begin
  res:=mrYes;
  if filechanged then
    res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_SureQuit')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
   CanClose:=(res=mrYes);
   if canClose and Form_DFMPreview.Visible then
     Form_DFMPreview.Close;
end;

procedure TForm_DFMMain.FormDestroy(Sender: TObject);
begin
  DragAcceptfiles(Form_DFMMain.handle,false);
  {$IFNDEF NOPACKAGES}
  unloadPackages;
  {$ENDIF}
  cfg.FileHistory.free;
end;

procedure TForm_DFMMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    ord('N'):
      if (Shift = [ssCtrl]) then
        TB_NewFormClick(Self);
    ord('O'):
      if (Shift = [ssCtrl]) then
        TB_OpenClick(Self)
      else if (Shift = [ssCtrl,ssAlt]) then
        TB_OptionsClick(Self);
    ord('C'):
      if (Shift = [ssCtrl,ssShift]) then
        TB_DFMConvertClick(Self);
    ord('R'):
      if (Shift = [ssCtrl]) then
        TB_LoadResourceClick(Self)
      else if (Shift = [ssCtrl,ssShift]) and (dt=dtFile) then
        loaddfmFile(LoadedFile);
    ord('B'):
      if (Shift = [ssCtrl]) then
        TB_BinForm.Down:=not TB_BinForm.Down;
    ord('G'):
      if (Shift = [ssCtrl]) and (TB_GenRTC.Enabled) then
        TB_GenRTCClick(Self);
    ord('S'):
    begin
      if (Shift = [ssCtrl]) and (Treeview1.Items.Count>0) then
        TB_SaveClick(TB_Save);
      if (Shift = [ssCtrl,ssAlt]) and (Memo_Output.Text<>'') then
        TB_SaveClick(MI_SaveOutput);
      if (Shift = [ssCtrl,ssShift]) and (MI_SaveToBinary.Enabled) then
        MI_SaveToBinaryClick(MI_SaveToBinary);
    end;
    ord('U'):
    begin
      if (Shift = [ssCtrl]) and (TB_CreatePas.Enabled) then
        TB_CreatePasClick(self);
      if (Shift = [ssCtrl,ssShift]) and (TB_RemoveUnwanted.Enabled) then
        TB_RemoveUnwantedClick(self);
    end;
    ord('M'):
      if (Shift = [ssCtrl]) and (TB_MenuEditor.Enabled) then
        TB_MenuEditorClick(Self);
    ord('P'):
    begin
      if (Shift = [ssCtrl]) and (TB_GenPreview.Enabled) then
        TB_GenPreviewClick(TB_GenPreview);
      if ([ssCtrl,ssShift] = Shift) and (TB_Preview.Enabled) then
        TB_GenPreviewClick(TB_Preview);
    end;
    ord('H'):
      if (Shift = [ssCtrl]) then
        TB_ShowHidden.Down:=not TB_ShowHidden.Down;
    ord('E'):
      if (Shift = [ssCtrl]) then
        TB_EnableDisabled.Down:=not TB_EnableDisabled.Down;
    vk_insert:
    begin
      if (Shift = []) and (TB_AddObject.Enabled) then
        TB_AddObjectClick(Self);
      if (Shift = [ssCtrl]) and (TB_AddProperty.Enabled) then
        TB_AddPropertyClick(Self);
      if (Shift = [ssShift]) and (TB_AddEntry.Enabled) then
        TB_AddEntryClick(Self);
    end;
    vk_return:
      if (activeControl=Treeview1) and TB_Edit.Enabled then
        TB_EditClick(Self)
      else if (ActiveControl=edit_name) or
              (ActiveControl=edit_type) or
              (ActiveControl=edit_Propname) or
              (ActiveControl=edit_Value) then
        Btn_ApplyClick(self);
    vk_delete:
      if (ActiveControl=Treeview1) and (TB_Delete.Enabled) then
        TB_DeleteClick(Self);
    vk_up:
      begin
        if (Shift = [ssCtrl]) then
          TB_MoveClick(TB_MoveUp);
        if (Shift = [ssCtrl,ssShift]) then
          TB_MoveClick(TB_MoveTop);
      end;
    vk_down:
      begin
        if (Shift = [ssCtrl]) then
          TB_MoveClick(TB_MoveDown);
        if (Shift = [ssCtrl,ssShift]) then
          TB_MoveClick(TB_MoveBottom);
      end;
    ord('I'):
      if (Shift = [ssCtrl]) then
        TB_CheckVersionClick(self);
    vk_F1:
      if (Shift = [ssShift]) then
        TB_AboutClick(self)
      else
        TB_AboutClick(MI_Help);
    ord('A'):
      if (Shift = [ssCtrl]) and (PageControl1.ActivePage<>Tab_Tree) then
      begin
        with GetCurrentMemo do
        begin
          selstart:=0;
          sellength:=length(text);
        end;
      end;
  end;
end;

//MainMenu
procedure TForm_DFMMain.MI_LastFileClick(Sender: TObject);
var
  res:word;
  filename:string;
begin
  res:=mrYes;
  if filechanged and (Treeview1.Items.Count>0) then
  begin
    res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_LoadCreate')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
  end;
  if res=mrYes then
  begin
    filename:=cfg.FileHistory.GetFileName((Sender as TMenuItem).MenuIndex);
    if fileexists(filename) then
    begin
      dt:=dtFile;
      LoadDfmFile(filename);
    end else
    begin
      ShowMessage(LocalizeString('Msg_FailedToOpen'));
      cfg.FileHistory.Delete((Sender as TMenuItem).MenuIndex);
    end;
  end;
end;

procedure TForm_DFMMain.MI_ExitClick(Sender: TObject);
begin
  close;
end;

procedure TForm_DFMMain.MI_UseDllForSavingClick(Sender: TObject);
begin
  MI_UseDllForSaving.checked:=not MI_UseDllForSaving.checked;
  MI_UseDllForSaving2.checked:=MI_UseDllForSaving.checked;
  cfg.UseDllForSaving:=MI_UseDllForSaving.checked;
end;

procedure TForm_DFMMain.MI_BinFormClick(Sender: TObject);
begin
  if assigned(sender) then
    (sender as TMenuItem).Checked:=not(sender as TMenuItem).Checked;
  TB_BinForm.Down:=MI_BinForm.Checked;
  TB_ShowHidden.Down:=MI_ShowHidden.Checked;
  TB_EnableDisabled.Down:=MI_EnableDisabled.Checked;
end;

//toolbar-buttons
procedure TForm_DFMMain.TB_NewFormClick(Sender: TObject);
var tn:TTreenode;
    res:word;
    i:integer;
    sl:TStringlist;
begin
  res:=mrYes;
  if filechanged and (Treeview1.Items.Count>0) then
  begin
    res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_LoadCreate')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
  end;
  if res=mrYes then
  begin
    Treeview1.Items.Clear;
    CB_Objects.items.clear;
    Memo_Source.lines.Clear;
    Memo_output.lines.Clear;
    tn:=treeview1.items.Add(nil,'object Form1: TForm1');
    tn.ImageIndex:=iiObject;
    tn.SelectedIndex:=iiObjectSel;
    CB_Objects.Items.Add(copy(tn.text,8,length(tn.text)-7));
    tn.ImageIndex:=iiObject;
    tn.SelectedIndex:=iiObjectSel;
    sl:=TStringlist.create;
    getProperties('TForm',sl);
    for i:=0 to sl.count-1 do
      treeview1.Items.AddChild(tn,sl.strings[i]);
    sl.free;
    for i:=0 to tn.Count-1 do
    begin
      tn.Item[i].ImageIndex:=iiProperty;
      tn.Item[i].SelectedIndex:=iiPropertySel;
    end;
    tn.Expand(false);
    tn.Selected:=true;
    pagecontrol1.ActivePage:=Tab_tree;
    activeControl:=Treeview1;
    dt:=dtNew;
    LoadedFile:='';
    Statusbar1.Panels[0].text:='';
    Statusbar1.Panels[1].text:='';
    Statusbar1.Hint:='';
    filechanged:=false;
    UpdateToolbar;
  end;
end;

procedure TForm_DFMMain.TB_OpenClick(Sender: TObject);
var
  res:word;
  fName:string;
begin
  res:=mrYes;
  if filechanged and (Treeview1.Items.Count>0) then
  begin
    res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_LoadCreate')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
  end;
  if res=mrYes then
  begin
    if OpenSaveFileDialog(self.handle,
      '',
      'Delphi-Form|*.dfm|Lazarus-Form|*.lfm',
      extractFilepath(paramstr(0)),
      TB_Open.Hint,
      fName,
      true,0) then
    begin
      dt:=dtFile;
      loaddfmFile(fName);
    end;
  end;
end;

procedure TForm_DFMMain.TB_DFMConvertClick(Sender: TObject);
var i:integer;
    fn:string;
    sl,fl:TStringlist;
    bin:boolean;
begin
  fl:=TStringList.create;
  if OpenSaveFileDialog(self.handle,'','Delphi-Form|*.dfm',extractFilePath(paramStr(0)),TB_DFMConvert.Hint,fn,true,OFN_ALLOWMULTISELECT) then
  begin
    fl.Text:=trim(fn);
    sl:=TStringlist.create;
    for i:=0 to fl.count-1 do
    begin
      fn:=fl.strings[i];
      createbackup(fn);
      sl.Clear;
      bin:=dfmparse_u.loaddfmFile(fn,sl);
      dfmparse_u.SaveDFMFile(fn,sl,not bin);
    end;
    sl.free;
  end;
  fl.free;
end;

procedure TForm_DFMMain.TB_LoadResourceClick(Sender: TObject);
var fName:string;
begin
  if OpenSaveFileDialog(self.handle,
    '',
    '*.exe|*.exe|*.dll|*.dll',
    extractFilepath(paramstr(0)),
    TB_LoadResource.Hint,
    fName,
    true,0) then
      loadResourceform(fName);
end;

procedure TForm_DFMMain.TB_BinFormClick(Sender: TObject);
begin
  MI_BinForm.Checked:=TB_BinForm.Down;
  MI_ShowHidden.Checked:=TB_ShowHidden.Down;
  MI_EnableDisabled.Checked:=TB_EnableDisabled.Down;
end;

procedure TForm_DFMMain.TB_SaveClick(Sender: TObject);
var fname,bname,cmd,ext:string;
begin
  if (Treeview1.Items.Count>0) then
  begin
    if (sender=TB_Save) or
       (sender=MI_Save) or (sender=MI_SaveAs) or //Toolbar-popup
       (sender=MI_Save2) or (sender=MI_SaveAs2) then //MainMenu-popup
    begin
      generate(usg_save);
    end;
  end;
  if memo_output.Text<>'' then
  begin
    if (dt=dtFile) and not
       ((sender=MI_SaveAs) or (sender=MI_SaveOutputAs) or
       (sender=MI_SaveAs2) or (sender=MI_SaveOutputAs2)) then
       fName:=LoadedFile
    else
    begin
      if LoadedFile<>'' then
        FName:=LoadedFile;

      OpenSaveFileDialog(self.handle,
      '.dfm',
      'Delphi-Form|*.dfm|Lazarus-Form|*.lfm',
      extractFilepath(paramstr(0)),
      TB_Save.Hint,
      fName,
      false,0);
    end;
    if FName<>'' then
    begin
      bname:=extractBaseName(extractFilename(FName));
      if checkid(bname) then
      begin
        try
          ext:=lowercase(extractFileExt(FName));
          dfmparse_u.SaveDFMFile(FName,memo_output.lines,TB_BinForm.Down and (ext<>'.lfm'));
          if dt=dtNew then dt:=dtFile;
          LoadedFile:=FName;
          cfg.FileHistory.AddFile(FName);
        except
          showmessage(LocalizeString('Msg_ErrorWritingFile'));
        end;
        filechanged:=false;
        if (ext='.lfm') then
        begin
          if fileexists(cfg.lazres) then
          begin
            cmd:='"'+cfg.lazres+'" "'+bname+'.lrs" "'+fname+'"';
            Winexec(PCHAR({'cmd /c '+}cmd{+'>lazres.log'}),sw_Show);
          end else ShowMessage(format(LocalizeString('Msg_LazresNotFound'),[cfg.lazres]));
        end;
        statusbar1.panels[0].Text:=LoadedFile;
        Statusbar1.Hint:=LoadedFile;
      end;
    end;
  end else showmessage(LocalizeString('Msg_NoOutput'));
  updateToolbar;
end;

procedure TForm_DFMMain.MI_SaveToBinaryClick(Sender: TObject);
begin
  if (sender<>MI_SaveOutputToBinary) and (sender<>MI_SaveOutputToBinary2) then
    generate(usg_save);
  try
    Form_DFMChooseRes.replaceRes(memo_output.lines,'');
  except
    showmessage(LocalizeString('Msg_ErrorWritingFile'));
  end;
  filechanged:=false;
end;

procedure TForm_DFMMain.TB_CreatePasClick(Sender: TObject);
var pasname:string;
    laz:boolean;
    res:Word;
begin
  laz:=(lowercase(extractfileext(LoadedFile))='.lfm');
  if laz then
    pasname:=ExtractBaseName(LoadedFile)+'.'+cfg.lazunitext
  else
    pasname:=ExtractBaseName(LoadedFile)+'.pas';
  try
    res:=mrCancel;
    if fileexists(pasname) then
    begin
      ModifyPas(pasname,cb_Objects.items,treeview1);
      if Form_DFMOptions.Chk_OpenModifiedPas.checked then res:=mrYes;
    end else
    begin
      CreatePas(pasname,laz,cb_Objects.items,treeview1);
      if Form_DFMOptions.Chk_OpenCreatedPas.checked then res:=mrYes;
    end;
    if (Form_DFMOptions.Chk_AskForOpen.checked) and (res=mrYes) then
      res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_Open')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
    if res=mrYes then
    begin
      if Form_DFMOptions.Edit_ExtEditor.Text='' then
        ShellExecute(self.handle,'open',PChar('"'+pasname+'"'),'','',sw_show)
      else
        WinExec(PChar(Form_DFMOptions.Edit_ExtEditor.Text+' "'+pasname+'"'),sw_show);
    end;
  except
    showmessage(LocalizeString('Msg_ErrorWritingFile'));
  end;
end;

procedure TForm_DFMMain.TB_GenRTCClick(Sender: TObject);
var sl:TStringlist;
    i,p:integer;
    s:string;
begin
  sl:=TStringlist.create;
  Memo_Output.Lines.Clear;
  generateRTC(getFormName,'',Treeview1.selected,Memo_Output.Lines,sl);
  indentlines(1,Memo_Output.Lines);
  //break long lines
  i:=0;
  while i < sl.count do
  begin
    if length(sl.Strings[i])>60 then
    begin
      s:=sl.Strings[i];
      sl.Delete(i);
      while (length(s) >60) do
      begin
        p:=60;
        while (p<=length(s)) and (s[p]<>',') do
          inc(p);
        sl.insert(i,copy(s,1,p));
        delete(s,1,p);
        if s<>'' then s:=' '+s;
        inc(i);
      end;
      if s<>'' then sl.Insert(i,s);
    end;
    inc(i);
  end;
  //end break long lines
  indentlines(1,sl);
  sl.Insert(0,'var');
  sl.Add('begin');
  sl.AddStrings(Memo_Output.Lines);
  sl.Add('end;');
  Memo_Output.Lines.Assign(sl);
  sl.free;
  Pagecontrol1.ActivePage:=Tab_Output;
  pagecontrol1Change(Pagecontrol1);
end;

procedure TForm_DFMMain.TB_GenPreviewClick(Sender: TObject);
var s:string;
    p:integer;
begin
  if (sender=TB_GenPreview) or (sender=MI_GenPreview) then
  begin
    generate(usg_Preview);
  end;
  if memo_output.Text<>'' then
  begin
    //first check if output valid
    loading:=true;
    form_DFMPreview.close;
    s:=memo_output.lines.Strings[0];
    p:=pos(':',s);
    s:=lowercase(trim(copy(s,p+1,length(s)-p)));
    if s='tform_dfmpreview' then
      AssignDFMToForm(memo_output.lines);
    loading:=false;
  end else showmessage(LocalizeString('Msg_NoOutput'));
end;

procedure TForm_DFMMain.TB_AddObjectClick(Sender: TObject);
begin
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  if assigned(selnode) then
    Treeview1.Selected:=selNode;
  selnode:=nil;
  Form_DFMAdd.EditDFM(eaAddObject,treeview1.selected);
end;

procedure TForm_DFMMain.TB_AddPropertyClick(Sender: TObject);
begin
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  if assigned(selnode) then
    Treeview1.Selected:=selNode;
  selnode:=nil;
  Form_DFMAdd.EditDFM(eaAddProperty,treeview1.selected);
end;

procedure TForm_DFMMain.TB_AddEntryClick(Sender: TObject);
var tn:TTreenode;
begin
  if assigned(selNode) and (copy(selnode.Text,length(selnode.Text),1)='>') then
  begin
    //funktion nur für TCollections
    Treeview1.Selected:=selNode;
    tn:=Treeview1.Items.AddChild(selnode,'item');
    tn.ImageIndex:=iiItem;
    tn.SelectedIndex:=iiItem;
    tn.Selected:=true;
  end;
end;

procedure TForm_DFMMain.TB_EditClick(Sender: TObject);
begin
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  if assigned(selnode) then
    Treeview1.Selected:=selNode;
  selnode:=nil;
  if sender=Form_DFMPalette.MI_Edit then
    Form_DFMAdd.CB_Value.enabled:=false;
  if isObject(treeview1.selected) then
    Form_DFMAdd.EditDFM(eaEditObject,treeview1.selected)
  else
    Form_DFMAdd.EditDFM(eaEditProperty,treeview1.selected);
  if Sender=Form_DFMPalette.MI_Edit then
  begin
    Form_DFMPreview.EditControl.Name:=GetObjectName(treeview1.selected);
  end;
  Form_DFMAdd.CB_Value.enabled:=true;
end;

procedure TForm_DFMMain.TB_DeleteClick(Sender: TObject);
var q:string;
begin
  SetWindowPos(self.handle,hwnd_top,0,0,0,0,swp_noMove or swp_noSize);
  if assigned(Treeview1.Selected) then
  begin
    if isObject(Treeview1.Selected) then
      q:=format(localizeString('Ask_DeleteObject'),[getObjectName(Treeview1.selected)])
    else
      q:=format(localizeString('Ask_DeleteProperty'),[getObjectName(GetObjectFromNode(Treeview1.selected))+'.'+GetPropertyName(Treeview1.selected)]);
    if MessageBox(self.Handle,PCHAR(q), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO)=mrYes then
    begin
      //Eintrag aus Combobox löschen
      treeview1.Items.Delete(treeview1.selected);
      UpdateToolbar;
      filechanged:=true;
    end;
  end else showmessage(localizeString('Msg_NothingSelected'));
end;

procedure TForm_DFMMain.TB_MoveClick(Sender: TObject);
begin
  if (sender=TB_MoveUp) or (Sender=MI_MoveUp) then moveNodeUp(treeview1.selected) else
  if (sender=TB_MoveDown) or (Sender=MI_MoveDown) then moveNodeDown(treeview1.selected) else
  if (sender=TB_MoveTop) or (Sender=MI_MoveTop) then MoveNodeTop(treeview1.selected) else
  if (sender=TB_MoveBottom) or (Sender=MI_MoveBottom) then MoveNodeBottom(treeview1.selected);
  filechanged:=true;
end;

procedure TForm_DFMMain.TB_MenuEditorClick(Sender: TObject);
begin
  Form_DFMPreview.Free;
  loading:=true;
  Form_DFMPreview:=TForm_DFMPreview.create(Application);
  LocalizeControls(Form_DFMPreview,cfg.language);
  form_DFMPreview.CreateMenuEditor(treeview1.Selected);
  form_dfmpreview.Caption:=TB_MenuEditor.Hint;
  form_dfmpreview.Show;//+hide palette
  loading:=false;
  enabled:=false;
end;

procedure TForm_DFMMain.TB_RemoveUnwantedClick(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:=format(localizeString('Cap_EntriesDeleted'),[RemoveUnwanted(treeview1)]);
end;

procedure TForm_DFMMain.TB_CheckVersionClick(Sender: TObject);
var vi_new,vi_old:TVersionInfo;
begin
  vi_new:=GetNewVersion((sender=MI_LastBeta) or (sender=MI_LastBeta2));
  vi_old:=GetFileVersion(ParamStr(0));
  if compareVersions(vi_old,vi_new) then
  begin
    if MessageBox(self.Handle,PCHAR(format(localizeString('Ask_VisitWebpage'),[VersionToStr(vi_new)])),
      PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO)=mrYes then
    begin
      if (sender=MI_LastBeta) or (sender=MI_LastBeta2) then
      begin
        if vi_new.BetaDownload<>'' then
          shellexecute(hInstance,'open',PCHAR(vi_new.BetaDownload),'','',sw_show)
        else
          shellexecute(hInstance,'open',page_beta,'','',sw_show);
      end else
        shellexecute(hInstance,'open',page_stable,'','',sw_show);
    end;
  end else
    showmessage(format(localizeString('Msg_NoNewVersion'),[VersionToStr(vi_new)]));
end;

procedure TForm_DFMMain.TB_AboutClick(Sender: TObject);
var helpdir,helpfile:string;
begin
  if (sender=MI_Help) then
  begin
    Helpdir:=extractfilepath(paramstr(0))+'help\';
    Helpfile:='index_'+cfg.Language+'.htm';
    if not fileexists(helpdir+helpfile) then
      helpfile:='index_en.htm';
    ShellExecute(hinstance,'open',PCHAR(helpfile),'',PCHAR(helpdir),sw_show);
  end else
    Form_DFMAbout.showModal;
end;

procedure TForm_DFMMain.MI_RequestClick(Sender: TObject);
var vi:TVersionInfo;
begin
  vi:=GetFileVersion(paramstr(0));
  shellexecute(hinstance,'open',PChar(format(page_request,[VersionToStr(vi),cfg.language])),'','',sw_shownormal);
end;

procedure TForm_DFMMain.TB_OptionsClick(Sender: TObject);
begin
  Form_DFMOptions.show;
end;

//main-pagecontrol
procedure TForm_DFMMain.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  TreeView1Changing(treeview1,treeview1.selected,AllowChange);
end;

procedure TForm_DFMMain.PageControl1Change(Sender: TObject);
begin
  if (Pagecontrol1.ActivePage=Tab_Tree) then
    TreeView1Change(treeview1,Treeview1.selected);
  Tab_Search2.TabVisible:=not (Pagecontrol1.ActivePage<>Tab_Tree);
  Tab_Search.TabVisible:=(Pagecontrol1.ActivePage<>Tab_Tree);
  Tab_Replace.TabVisible:=(Pagecontrol1.ActivePage<>Tab_Tree);
  if Tab_Search2.TabVisible then
  begin
    PageControl2.ActivePage:=Tab_Search2;
  end else
  begin
    PageControl2.ActivePage:=Tab_Search;
  end;
  Pnl_object.visible:=(PageControl1.ActivePage=Tab_Tree) and (assigned(Treeview1.selected));
  pnl_Apply.visible:=Pnl_object.visible;
  GrpBox_Property.visible:=Pnl_object.visible and (not isObject(Treeview1.selected));
end;

procedure TForm_DFMMain.Pnl_SourceResize(Sender: TObject);
begin
  Btn_ImportDFMSrc.width:=Pnl_Source.width-8;
end;

procedure TForm_DFMMain.Btn_ImportDFMSrcClick(Sender: TObject);
var res:word;
begin
  res:=mrYes;
  if filechanged and (Treeview1.Items.Count>0) then
  begin
    res:=MessageBox(self.Handle,PCHAR(LocalizeString('Ask_LoadCreate')), PCHAR(localizeString('Cap_Question')),MB_ICONQUESTION or MB_YESNO);
  end;
  if res=mrYes then
  begin
    loaddfm;
    PageControl1.ActivePage:=Tab_Tree;
    Pagecontrol1Change(Pagecontrol1);
  end;
end;

procedure TForm_DFMMain.MemoClick(Sender: TObject);
begin
  showCurrentLine(sender as TMemo);
end;

procedure TForm_DFMMain.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_f3 then
  begin
    if (PageControl2.ActivePage=Tab_Search) and
       (Edit_SearchText.Text<>'') then
      Btn_SearchTextClick(self) else
    if (PageControl2.ActivePage=Tab_Search2) and
       (Edit_FindNode.Text<>'') then
      Btn_SearchText2Click(self);
  end;
end;

procedure TForm_DFMMain.MemoKeyPress(Sender: TObject;
  var Key: Char);
var l,l2,i,ss,sl:integer;
    s:string;
    m:TMemo;
    r:boolean;
const
  indent=2;
begin
  m:=(Sender as TMemo);
  l:=getcurrentline(m);
  if key=#13 then
  begin
    key:=#0;
    m.seltext:=#13#10;
    if l>0 then
    begin
      i:=1;
      s:=m.Lines.Strings[l-1];   //index of current line
      if s<>'' then
      begin
        while s[i]=' ' do inc(i);
        s:=trim(s);
        s:=lowercase(copy(s,1,pos(' ',s)-1));
        if (s='object') or (s='inherited') or (s='inline') then
        begin
          m.seltext:=#13#10+stringofchar(' ',i-1)+'end';
          m.SelStart:=m.selstart-i-4;
          m.seltext:=stringofchar(' ',i+1);
        end else
          m.seltext:=stringofchar(' ',i-1);//copy(s,1,i-1);
      end;
    end;
  end else if key=#9 then
  begin
    key:=#0;
    l2:=SendMessage(m.Handle, EM_LINEFROMCHAR, m.SelStart+m.sellength, 0)+1;
    if (getasynckeystate(vk_shift) and $8000 <> 0) then
    begin
      r:=true;
      for i:=l to l2 do
        if copy(m.lines.strings[i-1],1,indent)<>StringOfChar(' ',indent) then
          r:=false;
      if r then
      begin
        ss:=m.SelStart;
        sl:=0;
        if m.sellength>0 then
          sl:=m.SelLength-((l2-l+1)*indent)
        else
          ss:=ss-indent;
        for i:=l to l2 do
        begin
          m.Lines.Strings[i-1]:=copy(m.Lines.Strings[i-1],3,length(m.Lines.Strings[i-1])-2);
        end;
        m.SelStart:=ss;
        if sl>(0-indent) then
          m.SelLength:=sl;
      end;
    end else
    begin
      ss:=m.SelStart;
      sl:=0;
      if m.sellength>0 then
        sl:=m.SelLength+((l2-l+1)*indent)
      else
        ss:=ss+indent;
      for i:=l to l2 do
      begin
        m.Lines.Strings[i-1]:=stringOfChar(' ',indent)+m.Lines.Strings[i-1];
      end;
      m.SelStart:=ss;
      if sl>0 then
        m.SelLength:=sl;
    end;
  end;
end;

procedure TForm_DFMMain.MemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=VK_UP) or (key=VK_DOWN) or (key=VK_Left) or (key=VK_Right) or (key=vk_return) then
    showCurrentLine(sender as Tmemo);
end;

procedure TForm_DFMMain.Pnl_TreeviewResize(Sender: TObject);
begin
  CB_Objects.Width:=Pnl_Treeview.width-8;
end;

procedure TForm_DFMMain.CB_ObjectsChange(Sender: TObject);
var tn:TTreenode;
    p :integer;
    n:string;
    c:TComponent;
begin
  p:=pos(':',CB_Objects.text);
  n:=copy(CB_Objects.text,1,p-1);
  tn:=getObjectNode(n);
  if assigned(tn) then tn.Selected:=true;
  if assigned(form_dfmpreview) and (form_dfmpreview.Visible) then
  begin
    c:=form_dfmpreview.findcomponent(n);
    if (c is Tcontrol) and (c<>form_dfmpreview) and ((c as TControl).visible) then
      form_dfmpreview.setcontrol(c as TControl);
  end;
end;

procedure TForm_DFMMain.TreeView1Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if NodeChanged then
    case Messagebox(self.handle,PCHAR(LocalizeString('Ask_SaveChanges')),PCHAR(LocalizeString('Cap_Question')),MB_YESNOCANCEL) of
      IDYes:begin Btn_ApplyClick(self);AllowChange:=not NodeChanged; end;
      IDCancel:AllowChange:=False;
      IDNo:NodeChanged:=false;
    end;
end;

procedure TForm_DFMMain.TreeView1Change(Sender: TObject; Node: TTreeNode);
var tn,tn_sel:TTreenode;
    s:string;
    p1,p2:integer;
    r:TDataRec;
begin
  loading:=true;
  UpdateToolbar;
  tn_sel:=Treeview1.Selected;
  Pnl_Object.Visible:=assigned(tn_sel);
  Pnl_Apply.Visible:=assigned(tn_sel);
  if assigned(tn_sel) then
  begin
    //object holen
    if not ((tn_sel.HasChildren) and isObject(tn_sel)) then
      tn:=getObjectFromNode(tn_sel)
    else
      tn:=tn_sel;
    GrpBox_Property.Visible:=(not (tn=tn_sel)) and (tn_sel.ImageIndex<>iiItem);
    s:=tn_sel.Text;
    p1:=pos('=',s);
    if p1>0 then
    begin
      Edit_PropName.Text:=trim(copy(s,1,p1-1));
      Edit_Value.Text:=trim(copy(s,p1+1,length(s)-p1));
    end;
    Edit_Value.enabled:=not assigned(tn_sel.data);
    //name des controls holen
    s:=tn.Text;
    p1:=pos(' ',s)+1;
    p2:=pos(':',s);
    CB_ObjectType.Text:=lowercase(copy(s,1,p1-2));
    edit_Name.Text:=trim(copy(s,p1,p2-p1));
    edit_Type.Text:=trim(copy(s,p2+1,length(s)-p2));
    r:=nil;
    if assigned(tn_sel.Data) then
    begin
      r:=TDataRec(tn_sel.Data);
      memo_special.Lines.text:=r.value;
    end else
    begin
      Memo_Special.lines.text:='';
    end;
    GrpBox_Special.visible:=assigned(r) and (r.kind<>dkCollection);
    loading:=false;
  end;
  NodeChanged:=false;
end;

procedure TForm_DFMMain.TreeView1DblClick(Sender: TObject);
begin
  if Assigned(treeview1.selected) and (treeview1.selected.ImageIndex=iiProperty) then
    TB_EditClick(self);
end;

procedure TForm_DFMMain.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
var p:integer;
    s:string;
begin
  if assigned(node.Data) and not Recreate then
  begin
    //OutputDebugString(Pchar(node.text+' will be freed'));
    TDataRec(node.Data).Free;
  end;
  if isObject(node) then
  begin
    p:=pos(' ',node.text);
    s:=copy(node.text,p+1,length(node.text)-p);
    p:=CB_Objects.Items.IndexOf(s);
    if p>-1 then CB_Objects.Items.Delete(p);
  end;
end;

procedure TForm_DFMMain.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var TN,tn2: TTreeNode;
    expanded:boolean;
begin
  expanded:=treeview1.Selected.Expanded;
  TN:=Treeview1.GetNodeAt(X,Y); // the node over which SN
  if GetKeyState(VK_CONTROL)>=0 then
    treeview1.selected.MoveTo(TN,naAddChild)
  else
  begin
    tn:=NodeCopy(tn,treeview1.selected);
    renameNode(tn);
    tn2:=tn.GetFirstChild;
    while assigned(tn2) and (tn2.level>tn.level) do
    begin
      renameNode(tn2);
      tn2:=tn2.GetNext;
    end;
    tn.Selected:=true;
  end;
  treeview1.Selected.Expanded:=expanded;
end;

procedure TForm_DFMMain.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var SN, TN: TTreeNode;
begin
  SN:=treeview1.Selected;  // the node beeing dragged
  TN:=Treeview1.GetNodeAt(X,Y); // the node over which SN
  accept:=false;
  if GetKeyState(VK_CONTROL)<0 then
    treeview1.DragCursor:=crMultiDrag
  else
    treeview1.DragCursor:=crDrag;
  if (assigned(tn) and assigned(sn) and (sn<>tn)) then
  begin
    //parent ändern
     accept:=(isObject(sn) and isObject(tn) and (not isChild(tn,sn)));
  end;
  // scrolling, if needed
  if y<20 then
    SendMessage(Treeview1.Handle,WM_VSCROLL,SB_LINEUP,0);
  if y>treeview1.Height-20 then
    SendMessage(Treeview1.Handle,WM_VSCROLL,SB_LINEDOWN,0);
end;

procedure TForm_DFMMain.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tn:TTreenode;
begin
  if Button=mbright then
  begin
    tn:=treeview1.GetNodeAt(x,y);
    if assigned(tn) then tn.selected:=true;
  end;
end;

procedure TForm_DFMMain.Memo_OutputChange(Sender: TObject);
begin
  UpdateToolbar;
end;

procedure TForm_DFMMain.Edit_NameChange(Sender: TObject);
begin
  NodeChanged:=true;
end;

procedure TForm_DFMMain.Btn_ApplyClick(Sender: TObject);
var tn_sel,tn,tn2:TTreenode;
    p,r:integer;
    s:string;
    fullchanged:boolean;
begin
    tn_sel:=treeview1.selected;
    //Object Holen
    if not isObject(tn_sel) then
      tn:=GetObjectFromNode(tn_sel)
    else
      tn:=tn_sel;
    fullchanged:=true;
    //Object-Name/Type
    if (CB_ObjectType.text<>'') and (edit_name.text<>'') and (edit_type.text<>'') then
    begin
      if (not ObjectExists(edit_name.Text)) or (GetObjectNode(edit_Name.text)=tn) then
      begin
        p:=pos(' ',tn.text);
        s:=trim(copy(tn.text,p+1,length(tn.text)-p));
        p:=CB_Objects.Items.IndexOf(s);
        CB_Objects.Items.strings[p]:=edit_name.text+': '+edit_type.text;
        tn.Text:=CB_ObjectType.Text+' '+edit_name.text+': '+edit_type.text;
        if (dt=dtResource) and (tn_sel.Level=0) then
        begin
          dt:=dtNew;
          UpdateToolbar;
        end;
      end else
      begin
        showmessage(localizeString('Msg_ObjectExists'));
        fullchanged:=false;
      end;
    end else
    begin
      showMessage(format(localizeString('Msg_EmptyFields'),[removeColon(Lbl_ObjectName.Caption)+','+removeColon(Lbl_ObjectType.Caption)]));
      fullchanged:=false;
    end;
    //property
    if GrpBox_Property.visible then
    begin
      if (edit_PropName.text<>'') and (edit_Value.text<>'') then
      begin
        tn2:=GetPropertyNode(tn,edit_PropName.text);
        r:=mryes;
        if (assigned(tn2)) and (tn2<>tn_sel) then
        begin
          r:=messageBox(0,PChar(localizeString('Ask_ReplaceProperty')),PCHAR(localizeString('Cap_Question')),MB_YESNO or MB_ICONQuestion);
          if r=mrYes then tn_sel:=tn2;
        end;
        if (r=mryes) then
        begin
          tn_sel.Text:=edit_propname.Text+' = '+edit_value.text;
        end else fullchanged:=false;
      end else
      begin
        showMessage(format(localizeString('Msg_EmptyFields'),[removeColon(Lbl_PropName.Caption)+','+removeColon(Lbl_PropValue.Caption)]));
        fullchanged:=false;
      end;
    end;
    //special-Data
    if GrpBox_Special.visible then
    begin
      if assigned(tn_sel.data) then
      begin
        TDataRec(tn_sel.Data).Value:=Memo_Special.Text;
      end;
    end;
    if NodeChanged and fullchanged then
      filechanged:=true;
    if fullchanged then
      NodeChanged:=false;
end;

// search-box
procedure TForm_DFMMain.Btn_GotoLineClick(Sender: TObject);
var memo:TMemo;
begin
  memo:=GetCurrentMemo;
  With memo Do Begin
    SelStart := Perform( EM_LINEINDEX, StrToInt(spinedit1.text)-1, 0 );
    Perform( EM_SCROLLCARET, 0, 0 );
    SetFocus;
  End;
  ShowCurrentLine(memo);
end;

procedure TForm_DFMMain.Btn_SearchTextClick(Sender: TObject);
var p,s,l:integer;
    s_temp:string;
    memo:TMemo;
begin
  if Edit_SearchText.Text<>'' then
  begin
    memo:=GetCurrentMemo;
    if Memo.SelStart=length(Memo.text) then
      Memo.SelStart:=0;
    if Memo.SelLength>0 then
      l:=Memo.SelLength
    else
      l:=length(Memo.text)-Memo.SelStart;
    s:=Memo.SelStart+1;
    s_temp:=lowercase(copy(memo.text,s+1,l));
    p:=pos(lowercase(Edit_SearchText.Text),s_temp);
    if p>0 then
    begin
      memo.SelStart:=s+p-1;
      memo.SetFocus;
      memo.Perform(EM_SCROLLCARET, 0, 0);
      ShowCurrentLine(memo);
    end else
      showmessage(localizeString('Msg_TextNotFound'));
  end else showmessage(localizeString('Msg_NoSearchText'));
end;

procedure TForm_DFMMain.SearchBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_return then
  begin
    if Sender=Spinedit1 then
      Btn_GotoLineClick(self) else
    if Sender=Edit_SearchText then
      Btn_SearchTextClick(self) else
    if (Sender=Edit_Search) or (Sender=Edit_Replace) then
      Btn_ReplaceAllClick(self) else
    if (Sender=Edit_FindNode) then
      Btn_SearchText2Click(self);
  end;
end;

procedure TForm_DFMMain.Btn_ReplaceAllClick(Sender: TObject);
var m:TMemo;
    r:string;
    p:integer;
begin
  if (Edit_Search.Text<>'') then
  begin
    m:=GetCurrentMemo;
    r:=Edit_Replace.Text;
    p:=pos('*',r);  //Replace * with SearchText
    if p>0 then
    begin
      delete(r,p,1);
      insert(Edit_Search.Text,r,p);
    end;
    m.Text:=ReplaceAll(m.Text,Edit_Search.Text,r);
  end else showmessage(localizeString('Msg_NoSearchText'));
end;

procedure TForm_DFMMain.Btn_SearchText2Click(Sender: TObject);
var tn:TTreenode;
begin
  if Edit_FindNode.Text<>'' then
  begin
    tn:=Treeview1.Items.GetFirstNode;
    if assigned(Treeview1.Selected) then
    begin
      if not (tn=Treeview1.Selected) then
        tn:=Treeview1.selected.GetNext;
    end;
    if assigned(tn) then
      tn:=findnode(tn,Edit_FindNode.text);
    if assigned(tn) then
      tn.Selected:=true
    else
      showmessage(localizeString('Msg_TextNotFound'));
  end else showmessage(localizeString('Msg_NoSearchText'));
end;

procedure TForm_DFMMain.StatusBar1Resize(Sender: TObject);
begin
  Statusbar1.Panels[0].width:=Statusbar1.Width-160;
  progressbar1.Left:=Statusbar1.Panels[0].width+40;
end;

procedure TForm_DFMMain.Popup_TreeviewPopup(Sender: TObject);
begin
  UpdateToolbar;
  MI_AddProperty.enabled:=TB_AddProperty.enabled;
  MI_AddObject.enabled:=TB_AddObject.enabled;
  MI_Delete.enabled:=TB_Delete.enabled;
  MI_AddEntry.enabled:=TB_AddEntry.enabled;
  MI_Edit.Enabled:=TB_Edit.Enabled;
  MI_MenuEditor.enabled:=TB_MenuEditor.enabled;
end;

procedure TForm_DFMMain.Popup_ToolbarPopup(Sender: TObject);
begin
  Popup_Toolbar.PopupComponent:=ToolBar1.Buttons[DropButton]; //if popupcomponent always contains Toolbar1
  MI_LastStable.Visible:=(Popup_Toolbar.PopupComponent=TB_CheckVersion);
  MI_LastBeta.Visible:=(Popup_Toolbar.PopupComponent=TB_CheckVersion);
  MI_About.Visible:=(Popup_Toolbar.PopupComponent=TB_About);
  MI_Help.Visible:=(Popup_Toolbar.PopupComponent=TB_About);
  MI_Request.Visible:=(Popup_Toolbar.PopupComponent=TB_About);
  //
  MI_Save.visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_SaveAs.visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_SaveOutput.visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_SaveOutputAs.visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_SaveToBinary.Visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_SaveOutputToBinary.visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  MI_UseDllForSaving.Visible:=(Popup_Toolbar.PopupComponent=TB_Save);
  N9.Visible:=(Popup_Toolbar.PopupComponent=TB_Save);
end;

procedure TForm_DFMMain.WMRecreateForms(var Msg: TMessage);
begin
  if recreate then
  begin
    recreateforms;
    readconfig;
    recreate:=false;
  end;
end;

procedure TForm_DFMMain.Pnl_DebugDblClick(Sender: TObject);
begin
  pnl_debug.Visible:=false;
end;

procedure TForm_DFMMain.Pnl_DebugMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  pnl_debug.Perform(WM_NCLBUTTONDOWN,HTcaption,0);
end;

end.
