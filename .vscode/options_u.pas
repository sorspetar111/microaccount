unit options_u;

{*
Program: DFMEdit
Unit: options_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Preferences-Dialog

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,shlobj,
  StdCtrls, ComCtrls,inifiles, ExtCtrls, Grids, Spin, Buttons, checklst,menus;

type
  TForm_DFMOptions = class(TForm)
    Pnl_ChangeOptions: TPanel;
      Btn_OK: TButton;
      Btn_Cancel: TButton;
    PageControl1: TPageControl;
      Tab_General: TTabSheet;
        Chk_BinForm: TCheckBox;
        Chk_ShowHidden: TCheckBox;
        Chk_EnableDisabled: TCheckBox;
        Chk_OpenCreatedPas: TCheckBox;
        Chk_OpenModifiedPas: TCheckBox;
        Chk_AskForOpen: TCheckBox;
        Chk_ShowToolbar: TCheckBox;
        Chk_ShowMainMenu: TCheckBox;
        Chk_DrawGrid: TCheckBox;
        Chk_SnapToGrid: TCheckBox;
        Chk_ChangeFocusToNew: TCheckBox;
        Chk_IgnorePreviewMove: TCheckBox;

        Lbl_Language: TLabel;
        CB_Language: TComboBox;
        Lbl_HistoryCount: TLabel;
        Spin_HistoryCount: TEdit;
        UD_HistoryCount: TUpDown;
        SB_DelHistory: TSpeedButton;
        Lbl_GridSize: TLabel;
        Spin_GridSize: TEdit;
        UD_GridSize: TUpDown;
        Lbl_LazUnitExt: TLabel;
        CB_LazUnitExt: TComboBox;
        Lbl_LazarusPath: TLabel;
        Edit_Lazres: TEdit;
        Btn_FindLazres: TButton;
        Lbl_ExternalEditor: TLabel;
        Edit_ExtEditor: TEdit;
        Btn_ChooseEditor: TButton;
        Lbl_BackupCount: TLabel;
        Spin_BackupCount: TEdit;
        UD_BackupCount: TUpDown;
        Lbl_Backupdir: TLabel;
        Edit_Backupdir: TEdit;
        Btn_ChooseFolder: TButton;

      Tab_Packages: TTabSheet;
        ListBox1: TListBox;
        Pnl_ChangePackages: TPanel;
          Btn_Add: TButton;
          Btn_Delete: TButton;
      Tab_EditDlg: TTabSheet;
        TreeView1: TTreeView;
        Pnl_ChangeProps: TPanel;
          Lbl_Component: TLabel;
          Edit_Comp: TEdit;
          Lbl_Property: TLabel;
          CB_Property: TComboBox;
          Lbl_Type: TLabel;
          CB_Type: TComboBox;
          Lbl_OptValue: TLabel;
          Cb_Value: TComboBox;
          Lbl_OptRule: TLabel;
          Edit_Unwanted: TEdit;
          Pnl_ImgIdx: TPanel;
            Edit_ImgIdx: TEdit;
            UD_ImgIdx: TUpDown;
            Image1: TImage;
            SB_SetImgIdx: TSpeedButton;
          Btn_Add2: TButton;
          Btn_Delete2: TButton;
      Tab_Position: TTabSheet;
        StringGrid1: TStringGrid;
      Tab_Customize: TTabSheet;
        GB_Toolbar: TGroupBox;
          CLB_ToolButtons: TCheckListBox;
          Lbl_Toolbutton: TLabel;
        Splitter1: TSplitter;
        GB_MainMenu: TGroupBox;
          CLB_MenuItems: TCheckListBox;
          Lbl_MenuItem: TLabel;

    OpenDialog1: TOpenDialog;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    
    procedure Chk_ShowToolbarClick(Sender: TObject);
    procedure CB_LanguageDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SB_DelHistoryClick(Sender: TObject);
    procedure Spin_GridSizeChange(Sender: TObject);
    procedure Btn_FindLazresClick(Sender: TObject);
    procedure Btn_ChooseEditorClick(Sender: TObject);
    procedure Btn_ChooseFolderClick(Sender: TObject);
    
    procedure Btn_AddClick(Sender: TObject);
    procedure Btn_DeleteClick(Sender: TObject);
    
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure CB_PropertyChange(Sender: TObject);
    procedure Edit_ImgIdxChange(Sender: TObject);
    procedure SB_SetImgIdxClick(Sender: TObject);
    procedure Btn_Add2Click(Sender: TObject);
    procedure Btn_Delete2Click(Sender: TObject);
    
    procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid1SelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
      
    procedure CLB_ToolbuttonsClickCheck(Sender: TObject);
    procedure CLB_MenuItemsClickCheck(Sender: TObject);
    procedure CLB_ToolButtonsClick(Sender: TObject);
    procedure CLB_MenuItemsClick(Sender: TObject);

    procedure Btn_OKClick(Sender: TObject);
    procedure Btn_CancelClick(Sender: TObject);
  private
    { Private-Deklarationen }
    curCell:TPoint;
    RestartNeeded:boolean;
    function GetImageIndex(eName:string):integer;
    procedure SetCell(aCol,aRow:Integer;aValue:String);
  public
    { Public-Deklarationen }
    procedure Load;
  end;

var
  Form_DFMOptions: TForm_DFMOptions;

implementation

{$R *.DFM}
uses classes_u, addproperty_u, about_u, chooseRes_u, inspector_u,localize_u,
  main_u,functions_u,pref_u, preview_u, palette_u,dialogs_u;

//private
function TForm_DFMOptions.GetImageIndex(eName:string):integer;
begin
  result:=iiProperty;
  case CB_Property.Items.IndexOf(eName) of
    0:result:=iiControl;  //<isControl>
    1:result:=iiReplace;  //<replace>
    2:result:=iiHide;  //<hide>
    3:result:=iiMenu;  //<Menu>
    4:result:=iiMenuPos;  //<Menu_Pos>
    5:result:=iiMenuImg;  //<Menu_ImgIdx>
    6:result:=iiIgnore;  //<ignore>
    7:result:=iiReplaceProp;  //<ReplaceProperties>
  end;
end;

procedure TForm_DFMOptions.SetCell(aCol,aRow:Integer;aValue:String);
var minval:integer;
begin
  if avalue<>'' then
  begin
    minval:=0;
    case aCol of
      3:
      begin
        case aRow of
          2:minval:=Add_MinHeight;
          3:minval:=ChooseRes_MinHeight;
          4:minval:=Inspector_MinHeight;
          5:minval:=Main_MinHeight;
          7:minval:=Palette_MinHeight;
        end;
      end;
      4:
      begin
        case aRow of
          2:minval:=Add_MinWidth;
          3:minval:=ChooseRes_MinWidth;
          4:minval:=Inspector_MinWidth;
          5:minval:=Main_MinWidth;
          7:minval:=Palette_MinWidth;
        end;
      end;
    end;
    if isnumeric(aValue) and (StrToInt(aValue)>=minval) then
      Stringgrid1.Cells[aCol,aRow]:=aValue
    else
    begin
      //Stringgrid1.Cells[aCol,aRow]:='';
      showmessage(format(LocalizeString('Msg_ValueMust'),[Stringgrid1.Cells[aCol,0],Stringgrid1.Cells[0,aRow],minval]));
      Stringgrid1.Cells[aCol,aRow]:=IntToStr(minval);
    end;
  end;
end;

procedure enumMenu(MenuItem:TMenuItem;clb:TChecklistbox);
var i:integer;
begin
  clb.Items.Add(Menuitem.Name);
  clb.Checked[clb.items.count-1]:=Menuitem.visible;
  for i:=0 to MenuItem.count-1 do
  begin
    if Menuitem.Items[i].Name<>'' then //hide entries of Last Files
    begin
      enumMenu(Menuitem.Items[i],clb);
    end;
  end;
end;

procedure TForm_DFMOptions.Load;
var i,j:integer;
    ini_comp,ini_prop,ini_unwanted:Tinifile;
    sl,sl2,sl3,sl4:TStringlist;
    tn_comp,tn_prop:TTreenode;
    s:string;
begin
  RestartNeeded:=false;
  chk_BinForm.Checked:=cfg.BinForm;
  chk_ShowHidden.checked:=cfg.ShowHidden;
  chk_EnableDisabled.checked:=cfg.EnableDisabled;
  Chk_OpenCreatedPas.checked:=cfg.OpenCreatedPas;
  Chk_OpenModifiedPas.checked:=cfg.OpenModifiedPas;
  Chk_AskForOpen.checked:=cfg.AskForOpen;
  Chk_ShowToolbar.Checked:=cfg.ShowToolbar;
  Chk_ShowMainMenu.checked:=cfg.ShowMainMenu;
  Chk_DrawGrid.checked:=cfg.DrawGrid;
  Chk_SnapToGrid.checked:=cfg.SnapToGrid;
  
  Edit_ExtEditor.Text:=cfg.ExtEditor;
  UD_GridSize.Position:=cfg.GridSize;
  edit_lazres.text:=cfg.lazres;
  CB_LazUnitExt.Text:=cfg.lazUnitExt;
  UD_HistoryCount.Position:=cfg.FileHistory.Max;
  UD_BackupCount.Position:=cfg.BackupCount;
  Edit_BackupDir.Text:=cfg.BackupDir;

  //load packages
  {$IFNDEF NOPACKAGES}
  Listbox1.Items.clear;
  for i:=0 to 9 do
  begin
    s:=cfg.packages[i].filename;
    if s<>'' then
    begin
      Form_DFMOptions.Listbox1.Items.Add(s);
    end;
  end;
  {$ELSE}
  Form_DFMOptions.Tab_Packages.tabvisible:=false;
  {$ENDIF}

  //load Treeview
  Treeview1.items.clear;
  ini_comp:=Tinifile.create(compfile);
  ini_prop:=Tinifile.create(propfile);
  ini_unwanted:=Tinifile.create(unwantedfile);
  sl:=TStringlist.create;
  sl2:=TStringlist.create; //comp-props
  sl3:=TStringlist.create;//prop-props
  sl4:=TStringlist.create; //unwanted - props
  sl2.Sorted:=true;
  sl2.Duplicates:=dupIgnore;
  ini_comp.ReadSections(sl);
  sl.Sort;
  for i:=0 to sl.count-1 do
  begin
    tn_comp:=treeview1.Items.Add(nil,sl[i]);
    ini_comp.ReadSection(sl[i],sl2);
    ini_prop.ReadSection(sl[i],sl3);
    ini_unwanted.ReadSection(sl[i],sl4);
    sl2.AddStrings(sl3);
    sl2.AddStrings(sl4);
    for j:=0 to sl2.count-1 do
    begin
      tn_prop:=treeview1.Items.AddChild(tn_comp,sl2[j]);
      tn_prop.imageindex:=iiProperty;
      if isFirstChar(Pchar(sl2[j]),'<') then
      begin
        tn_prop.imageindex:=GetImageIndex(sl2[j]);
      end;
      tn_prop.selectedIndex:=tn_prop.imageindex;
      s:=ini_prop.ReadString(sl[i],sl2[j],'');
      if s<>'' then
      begin
        with treeview1.Items.AddChild(tn_prop,s) do
        begin
          if isFirstChar(Pchar(sl2[j]),'<') then
            imageindex:=iiValue
          else
            imageindex:=iiType;
          selectedIndex:=imageindex;
        end;
      end;
      s:=ini_comp.ReadString(sl[i],sl2[j],'');
      if s<>'' then
        with treeview1.Items.AddChild(tn_prop,s) do
        begin
          imageindex:=iiValue;
          selectedIndex:=imageindex;
        end;
      s:=ini_unwanted.ReadString(sl[i],sl2[j],'');
      if s<>'' then
        with treeview1.Items.AddChild(tn_prop,s) do
        begin
          imageindex:=iiUnwanted;
          selectedIndex:=imageindex;
        end;
    end;
  end;
  sl4.free;
  sl3.free;
  sl2.free;
  sl.free;
  ini_unwanted.free;
  ini_comp.free;
  ini_prop.free;

  //load form-size and position
  for i:=1 to 7 do
  begin
    for j:=1 to 4 do
    begin
      if SizeEditable[i,j] and (cfg.FormSizes[j,i]>-1) then
      begin
        Stringgrid1.Cells[j,i]:=IntToStr(cfg.FormSizes[j,i]);
      end;
    end;
  end;
  CLB_ToolButtons.items.clear;
  for i:=0 to Form_DFMMain.ToolBar1.ButtonCount-1 do
  begin
    CLB_ToolButtons.Items.Add(Form_DFMMain.ToolBar1.Buttons[i].Name);
    CLB_ToolButtons.Checked[i]:=Form_DFMMain.ToolBar1.Buttons[i].visible;
  end;
  CLB_MenuItems.items.clear;
  for i:=0 to Form_DFMMain.MainMenu1.Items.Count-1 do
  begin
    EnumMenu(Form_DFMMain.MainMenu1.Items[i],CLB_MenuItems);
  end;
  CB_Language.ItemIndex:=CB_Language.Items.IndexOf(cfg.language);
end;

//component-events
procedure TForm_DFMOptions.FormCreate(Sender: TObject);
var sr:TSearchRec;
    b:TBitmap;
    lng,bname:string;
    i:integer;
begin
  //load languages
  findfirst(extractfilepath(paramstr(0))+lng_prefix+'*.lng',faAnyFile,sr);
  repeat
    if sr.name<>'' then
    begin
      lng:=copy(sr.name,length(lng_prefix)+1,length(sr.name)-length(lng_prefix)-4);
      if lng<>'de' then
        Form_DFMOptions.CB_Language.Items.add(lng);
    end;
  until findnext(sr)<>0;
  for i:=0 to Form_DFMOptions.CB_Language.Items.count-1 do
  begin
    bname:= ExtractFilepath(paramstr(0))+lng_prefix+CB_Language.Items.Strings[i]+'.bmp';
    if FileExists(bname) then
    begin
      b:=TBitmap.create;
      b.loadfromfile(bname);
    end else b:=nil;
    CB_Language.Items.Objects[i]:=b;
  end;
end;

procedure TForm_DFMOptions.FormDestroy(Sender: TObject);
var i:integer;
begin
  for i:=0 to cb_Language.Items.Count-1 do
  begin
    if assigned(cb_Language.Items.Objects[i]) then
      cb_Language.Items.Objects[i].free;
  end;
end;

procedure TForm_DFMOptions.FormShow(Sender: TObject);
begin
  activeControl:=PageControl1;
  PageControl1.ActivePage:=Tab_General;
  load;
end;

procedure TForm_DFMOptions.Chk_ShowToolbarClick(Sender: TObject);
begin
  if not Chk_ShowToolbar.Checked and not Chk_ShowMainMenu.Checked then
    Chk_ShowToolbar.Checked:=true;
  RestartNeeded:=true;
end;

procedure TForm_DFMOptions.CB_LanguageDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  CB_Language.Canvas.fillrect(rect);
  if assigned(CB_Language.Items.Objects[index]) then
  begin
    CB_Language.Canvas.Draw(rect.left,rect.top,TBitmap(CB_Language.Items.Objects[index]));
  end;
  CB_Language.Canvas.TextOut(rect.left+20,rect.top,CB_Language.Items.Strings[index]);
end;

procedure TForm_DFMOptions.SB_DelHistoryClick(Sender: TObject);
begin
  cfg.FileHistory.Clear;
  cfg.FileHistory.Save;
end;

procedure TForm_DFMOptions.Spin_GridSizeChange(Sender: TObject);
begin
  Chk_DrawGrid.Enabled:=UD_GridSize.Position>0;
end;

procedure TForm_DFMOptions.Btn_FindLazresClick(Sender: TObject);
begin
  opendialog1.filter:='lazres.exe|lazres.exe';
  opendialog1.InitialDir:=extractfilepath(edit_lazres.text);
  if opendialog1.execute then
    edit_lazres.text:=opendialog1.FileName;
end;

procedure TForm_DFMOptions.Btn_ChooseEditorClick(Sender: TObject);
begin
  opendialog1.filter:='*.exe|*.exe';
  opendialog1.InitialDir:=extractfilepath(Edit_ExtEditor.text);
  if opendialog1.execute then
    Edit_ExtEditor.text:=opendialog1.FileName;
end;

procedure TForm_DFMOptions.Btn_ChooseFolderClick(Sender: TObject);
var s:string;
begin
  if openfolder(CSIDL_DESKTOP,LocalizeString('Cap_ChooseFolder'),s) then
    edit_BackUpDir.Text:=s;
end;

procedure TForm_DFMOptions.Btn_AddClick(Sender: TObject);
begin  {$IFNDEF NOPACKAGES}
  if listbox1.Items.Count<10 then
  begin
    opendialog1.filter:='*.dpl|*.dpl|*.bpl|*.bpl';
    if opendialog1.execute then
      listbox1.Items.Add(opendialog1.FileName);
  end else showmessage(LocalizeString('Msg_Only10Packages'));  {$ENDIF}
end;

procedure TForm_DFMOptions.Btn_DeleteClick(Sender: TObject);
begin    {$IFNDEF NOPACKAGES}
  if Listbox1.ItemIndex>-1 then
    Listbox1.Items.Delete(Listbox1.ItemIndex)
  else
    showmessage(LocalizeString('Msg_NothingSelected'));  {$ENDIF}
end;

procedure TForm_DFMOptions.TreeView1Change(Sender: TObject;
  Node: TTreeNode);
var tn:TTreenode;
begin
  cb_Property.Text:='';
  CB_Type.Text:='';
  cb_Value.Text:='';
  Edit_Unwanted.Text:='';
  tn:=treeview1.selected;
  edit_Comp.text:=getObjectFromNode(tn).Text;
  if tn.Level>0 then
  begin
    tn:=GetPropertyFromNode(tn);
    cb_Property.Text:=tn.Text;
    tn:=tn.GetFirstChild;
    while assigned(tn) do
    begin
      if (tn.Imageindex in [iiType,iiValue,iiUnwanted]) then
      begin
        if tn.ImageIndex=iiType then
          CB_Type.Text:=tn.text
        else if tn.ImageIndex=iiValue then
          cb_Value.Text:=tn.Text
        else
          Edit_Unwanted.Text:=tn.Text;
        tn:=tn.getnextSibling;
      end else tn:=nil;
    end;
    cb_PropertyChange(self);
  end;
end;

procedure TForm_DFMOptions.TreeView1Expanded(Sender: TObject;
  Node: TTreeNode);
var n:TTreenode;
begin
  if (GetAsyncKeyState(vk_Shift) and $8000)<0 then
  begin
    n:=node.getfirstchild;
    while assigned(n) do
    begin
      n.Expand(false);
      n:=n.GetNextSibling;
    end;
  end;
end;

procedure TForm_DFMOptions.CB_PropertyChange(Sender: TObject);
var s:string;
    i:integer;
begin
  Cb_Value.Items.Clear;
  if isFirstChar(PChar(CB_Property.Text),'<') then
  begin
    CB_Type.Enabled:=False;
    if (isCharAtPos(PChar(CB_Property.Text),'i',2) and
       isCharAtPos(PChar(CB_Property.Text),'s',3)) or (copy(CB_Property.Text,2,4)='hide') then
      cb_Value.Items.Text:='0'#13#10'1'
    else
    begin
      s:=lowercase(copy(CB_Property.Text,2,pos('>', CB_Property.Text)-2));
      if s='replace' then
      begin
        getComponents(cb_Value.Items);
        if edit_Comp.Text<>'' then
        begin
          i:=cb_Value.Items.IndexOf(edit_Comp.Text);
          if i>-1 then cb_Value.Items.Delete(i);
        end;
      end;
    end;
  end else CB_Type.Enabled:=True;
  Edit_unwanted.Enabled:=CB_Type.Enabled;
  Pnl_ImgIdx.Visible:=ansiCompareText(CB_Property.Text,'<Menu_ImgIdx>')=0;
  if Pnl_ImgIdx.Visible and (CB_Value.Text<>'') then
    Edit_ImgIdx.Text:=CB_Value.Text;
  if cb_Value.Items.Count>0 then
  begin
    cb_Value.Style:=csDropDown;
  end else
    cb_Value.Style:=csSimple;
end;

procedure TForm_DFMOptions.Edit_ImgIdxChange(Sender: TObject);
begin
  if isNumeric(Edit_ImgIdx.Text) then
  begin
    image1.picture:=nil;
    form_DFMPalette.imagelist1.GetBitmap(StrToInt(Edit_ImgIdx.Text),image1.picture.bitmap);
    image1.Repaint;
  end;
end;

procedure TForm_DFMOptions.SB_SetImgIdxClick(Sender: TObject);
begin
  cb_Value.Text:=Edit_ImgIdx.Text;
end;

procedure TForm_DFMOptions.Btn_Add2Click(Sender: TObject);
var tn_obj,tn_Prop,tn_Value:TTreenode;
begin
  if Edit_Comp.Text<>'' then
  begin
    tn_obj:=GetCompNode(edit_comp.text);
    if not assigned(tn_Obj) then
      tn_Obj:=Treeview1.Items.Add(nil,edit_comp.text);
    if (cb_Property.Text<>'') then
    begin
      if ((CB_Type.Text<>'') or (cb_Value.Text<>'')) then
      begin
        tn_Prop:=GetPropNode(tn_Obj,cb_Property.Text);
        if not assigned(tn_Prop) then
          tn_Prop:=Treeview1.Items.AddChild(tn_obj,cb_Property.Text);
        if (CB_Type.Text<>'') then
        begin
          tn_Value:=GetTypeNode(tn_Prop);
          if not assigned(tn_Value) then
            tn_Value:=Treeview1.Items.AddChildFirst(tn_prop,CB_Type.Text)
          else
            tn_Value.Text:=CB_Type.Text;
          tn_Value.ImageIndex:=iiType;
          tn_Value.SelectedIndex:=tn_Value.ImageIndex;
        end;
        if (cb_Value.Text<>'') then
        begin
          tn_Value:=GetValueNode(tn_Prop);
          if not assigned(tn_Value) then
            tn_Value:=Treeview1.Items.AddChild(tn_prop,cb_Value.Text)
          else
            tn_Value.Text:=cb_Value.Text;
          tn_Value.ImageIndex:=iiValue;
          if isFirstChar(PChar(cb_Value.Text),'<') then
            tn_Value.ImageIndex:=GetImageIndex(cb_Value.Text);
          tn_Value.SelectedIndex:=tn_Value.ImageIndex;
        end;
        if (Edit_Unwanted.Text<>'') then
        begin
          tn_Value:=GetRuleNode(tn_Prop);
          if not assigned(tn_Value) then
            tn_Value:=Treeview1.Items.AddChild(tn_prop,cb_Value.Text)
          else
            tn_Value.Text:=cb_Value.Text;
          tn_Value.ImageIndex:=iiUnwanted;
          tn_Value.SelectedIndex:=tn_Value.ImageIndex;
        end;
        tn_Prop.ImageIndex:=iiProperty;
        if isFirstChar(Pchar(CB_Property.Text),'<') then
          tn_prop.ImageIndex:=GetImageindex(CB_Property.text);
        tn_Prop.SelectedIndex:=tn_Prop.ImageIndex;
      end else ShowMessage(LocalizeString('Msg_TypeOrValue'));
    end;
  end;
end;

procedure TForm_DFMOptions.Btn_Delete2Click(Sender: TObject);
var parent:TTreenode;
begin
  if (Treeview1.Selected<>nil) then
  begin
    if(MessageBox(self.handle,PCHAR(LocalizeString('Ask_Delete')),PCHAR(LocalizeString('Cap_Question')),MB_IconQuestion or MB_YesNo)=mrYes) then
    begin
      parent:=Treeview1.Selected.Parent;
      Treeview1.Selected.Delete;
      if assigned(parent) and (Parent.ImageIndex=iiProperty) and (not parent.HasChildren) then
        parent.Delete;
    end;
  end else Showmessage(LocalizeString('Msg_NothingSelected'));
end;

procedure TForm_DFMOptions.StringGrid1KeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then
    setCell(curcell.x,curcell.y,Stringgrid1.Cells[curcell.x,curcell.y]);
end;

procedure TForm_DFMOptions.StringGrid1SelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  setCell(curcell.x,curcell.y,Stringgrid1.Cells[curcell.x,curcell.y]);
  curCell:=Point(col,row);
  if SizeEditable[row,col] then
    Stringgrid1.Options:=Stringgrid1.Options+[goEditing]
  else
    Stringgrid1.Options:=Stringgrid1.Options-[goEditing];
end;

procedure TForm_DFMOptions.CLB_ToolbuttonsClickCheck(Sender: TObject);
begin
  RestartNeeded:=true;
end;

procedure TForm_DFMOptions.CLB_MenuItemsClickCheck(Sender: TObject);
var c:TMenuItem;
begin
  c:=form_dfmMain.FindComponent(CLB_MenuItems.Items.Strings[CLB_MenuItems.ItemIndex]) as TMenuItem;
  if assigned(c) then
    c.visible:= CLB_MenuItems.Checked[CLB_MenuItems.ItemIndex];
end;

procedure TForm_DFMOptions.CLB_ToolButtonsClick(Sender: TObject);
var c:TComponent;
begin
  c:=Form_DFMMain.findComponent(CLB_ToolButtons.Items.Strings[CLB_ToolButtons.ItemIndex]);
  if assigned(c) and (c is TToolbutton) then
    Lbl_Toolbutton.Caption:=(c as TToolbutton).Hint
  else
    Lbl_Toolbutton.Caption:='';
end;

procedure TForm_DFMOptions.CLB_MenuItemsClick(Sender: TObject);
var c:TComponent;
begin
  c:=Form_DFMMain.findComponent(CLB_MenuItems.Items.Strings[CLB_MenuItems.ItemIndex]);
  if assigned(c) and (c is TMenuItem) then
  begin
    Lbl_MenuItem.Caption:=(c as TMenuItem).Caption;
    if (pos(#9,Lbl_MenuItem.Caption)>0) then  //remove tab and the following chars (shortcut)
      Lbl_MenuItem.Caption:=copy(Lbl_MenuItem.Caption,1,pos(#9,Lbl_MenuItem.Caption)-1);
  end else
    Lbl_MenuItem.Caption:='';
end;

procedure TForm_DFMOptions.Btn_OKClick(Sender: TObject);
var ini:Tinifile;
    tn:TTreenode;
    sl_comp,sl_prop,sl_unwanted:TStringlist;
    prop,s:string;
    i:integer;
begin
  {$IFNDEF NOPACKAGES}
  unloadPackages;
  {$ENDIF}
  createbackup(ConfFile);
  createbackup(CompFile);
  createbackup(PropFile);
  createbackup(UnwantedFile);
  ini:=TInifile.create(ConfFile);
  ini.WriteString('main','language',cb_Language.text);
  ini.WriteBool('main','BinForm',Chk_BinForm.checked);
  ini.WriteBool('main','ShowHidden',Chk_ShowHidden.checked);
  ini.WriteBool('main','EnableDisabled',Chk_EnableDisabled.checked);
  ini.WriteBool('main','OpenCreatedUnits',Chk_OpenCreatedPas.checked);
  ini.WriteBool('main','OpenModifiedUnits',Chk_OpenModifiedPas.checked);
  ini.WriteBool('main','AskForOpen',Chk_AskForOpen.checked);
  ini.WriteBool('main','ShowToolbar',Chk_ShowToolbar.Checked);
  ini.WriteBool('main','ShowMainMenu',Chk_ShowMainMenu.checked);
  ini.WriteBool('main','DrawGrid',Chk_DrawGrid.checked);
  ini.WriteBool('main','SnapToGrid',Chk_SnapToGrid.checked);
  ini.WriteString('main','External',Edit_ExtEditor.Text);
  ini.WriteInteger('main','GridSize',StrToInt(Spin_GridSize.Text));
  ini.WriteString('main','lazres',getRelPath(Edit_Lazres.Text));
  ini.WriteString('main','LazUnitExt',CB_LazUnitExt.Text);
  ini.WriteInteger('main','BackupCount',StrToInt(Spin_BackupCount.Text));
  ini.WriteString('main','BackupDir',Edit_BackupDir.Text);
  {$IFNDEF NOPACKAGES}
  ini.EraseSection('packages');
  for i:=0 to listbox1.items.count-1 do
  begin
    ini.WriteString('packages',IntToStr(i),getRelPath(ListBox1.items.strings[i]));
  end;
  {$ENDIF}
  ini.EraseSection('Position');
  for i:=0 to screen.FormCount-1 do
  begin
    s:=copy(screen.forms[i].name,9,length(screen.forms[i].name)-8);
    if (screen.forms[i]<>Form_DFMPreview) and (screen.forms[i].tag<>0) and
       (screen.forms[i].tag<StringGrid1.RowCount) then
    begin
      if stringgrid1.Cells[1,screen.forms[i].tag]<>'' then
        ini.WriteInteger('Position',s+'_Left',StrToInt(stringgrid1.Cells[1,screen.forms[i].tag]));
      if stringgrid1.Cells[2,screen.forms[i].tag]<>'' then
        ini.WriteInteger('Position',s+'_Top',StrToInt(stringgrid1.Cells[2,screen.forms[i].tag]));
      if stringgrid1.Cells[3,screen.forms[i].tag]<>'' then
        ini.WriteInteger('Position',s+'_Height',StrToInt(stringgrid1.Cells[3,screen.forms[i].tag]));
      if stringgrid1.Cells[4,screen.forms[i].tag]<>'' then
        ini.WriteInteger('Position',s+'_Width',StrToInt(stringgrid1.Cells[4,screen.forms[i].tag]));
    end;
  end;
  ini.EraseSection('Hidden');
  //toolbuttons
  for i:=0 to CLB_Toolbuttons.Items.Count-1 do
    if not CLB_ToolButtons.Checked[i] then ini.WriteBool('Hidden',CLB_ToolButtons.Items.Strings[i],true);
  //MenuItems
  for i:=0 to CLB_MenuItems.Items.Count-1 do
    if not CLB_MenuItems.Checked[i] then ini.WriteBool('Hidden',CLB_MenuItems.Items.Strings[i],true);

  ini.free;
  //object/property-lists
  sl_comp:=TStringlist.create;
  sl_prop:=TStringlist.create;
  sl_unwanted:=TStringlist.create;
  sl_comp.add(';note that stringproperties must be defined by 2x2 ''');
  sl_comp.add(';e.g. ''''Test''''');
  sl_prop.add(';setting type of unknown properties');
  sl_prop.add(';possible values for specialdata: <Binary>,<TStrings>,<IntList>,<TCollection>');
  sl_prop.add(';else all known types are supported e.g. TFont (classes), integer, string, boolean, TOpenOptions (sets), TWindowState (enum)');

  sl_unwanted.Add(';prop = * => always delete prop');
  sl_unwanted.Add(';prop = test => delete prop if value is test');
  sl_unwanted.Add(';prop = otherprop = test => delete prop if value of otherprop is test');
  sl_unwanted.Add(';prop = [test,test2] => delete prop if value is test or test2');

  tn:=treeview1.Items.GetFirstNode;
  while assigned(tn) do
  begin
    case (tn.imageindex) of
      iiObject:
      begin
        if (sl_comp.count>0) and (sl_comp.strings[sl_comp.Count-1]<>'') then sl_comp.add('');
        if (sl_prop.count>0) and (sl_prop.strings[sl_prop.Count-1]<>'') then sl_prop.add('');
        if (sl_unwanted.count>0) and (sl_unwanted.strings[sl_unwanted.Count-1]<>'') then sl_unwanted.add('');
        sl_unwanted.Add('['+tn.Text+']');
        sl_prop.Add('['+tn.Text+']');
        sl_comp.Add('['+tn.Text+']');
      end;
      iiProperty,iiControl..iiReplaceProp: prop:=tn.text;
      iiType: sl_prop.add('  '+prop+'='+tn.text);
      iiValue:
      begin
        case tn.Parent.ImageIndex of
          iiProperty,iiControl..iiMenuImg: sl_comp.add('  '+prop+'='+duplicateQuotes(tn.text));
          iiignore..iiReplaceProp:sl_prop.add('  '+prop+'='+tn.text);
        end;
      end;
      iiUnwanted:sl_unwanted.add('  '+prop+'='+tn.text);
    end;
    tn:=tn.getnext;
  end;
  sl_comp.SaveToFile(CompFile);
  sl_prop.SaveToFile(PropFile);
  sl_unwanted.SaveToFile(UnwantedFile);
  sl_unwanted.Free;
  sl_prop.free;
  sl_comp.free;
  cfg.FileHistory.Max:=StrToInt(Spin_HistoryCount.Text);
  cfg.FileHistory.Save;
  close;
  if ((cb_language.ItemIndex=0) and (cfg.language<>cb_Language.Text)) or RestartNeeded then
  begin
    Recreate:=true;
    RecreateMainform;
  end else ReadConfig;
end;

procedure TForm_DFMOptions.Btn_CancelClick(Sender: TObject);
begin
  close;
end;

end.
