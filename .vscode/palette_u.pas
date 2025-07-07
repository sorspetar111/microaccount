unit palette_u;

{*
Program: DFMEdit
Unit: Palette_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Component-Palette

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Menus,inifiles,commctrl;

type
  TForm_DFMPalette = class(TForm)
    Panel1: TPanel;
      Btn_Actions: TButton;
    PageControl1: TPageControl;
    Popup_EditDFM: TPopupMenu;
      MI_ChangeFocus: TMenuItem;
      MI_IgnoreFormMove: TMenuItem;
    N1: TMenuItem;
      MI_KeepZOrder: TMenuItem;
      MI_MoveTop: TMenuItem;
      MI_MoveBottom: TMenuItem;
    N2: TMenuItem;
      MI_Edit: TMenuItem;
      MI_Delete: TMenuItem;
      MI_AddObject: TMenuItem;
    N3: TMenuItem;
      MI_Copy: TMenuItem;
      MI_Cut: TMenuItem;
      MI_Paste: TMenuItem;
    N4: TMenuItem;
      MI_Close: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Btn_ActionsClick(Sender: TObject);
    procedure Popup_EditDFMPopup(Sender: TObject);
    procedure CheckMenuItem(Sender: TObject);
    procedure MI_MoveTopClick(Sender: TObject);
    procedure MI_MoveBottomClick(Sender: TObject);
    procedure MI_EditClick(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure MI_AddObjectClick(Sender: TObject);
    procedure MI_CopyClick(Sender: TObject);
    procedure MI_CutClick(Sender: TObject);
    procedure MI_PasteClick(Sender: TObject);
    procedure MI_CloseClick(Sender: TObject);
    procedure AddCompClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure WMGetMinMaxInfo(Var Msg: TMessage); Message WM_GETMINMAXINFO;
    //procedure FormSized(var Msg: TWMSize);message WM_SIZE;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
  public
    { Public-Deklarationen }
    procedure LoadSettings(imagelist:TImagelist;PageControl:TPageControl);
    procedure UpdatePopup;
  end;

var
  Form_DFMPalette: TForm_DFMPalette;

implementation

uses functions_u,rttifunctions_u,classes_u,preview_u, main_u, addproperty_u, inspector_u,localize_u;

{$R *.DFM}
Procedure AddComp(aType:string);
var aParent:TWinControl;
    aName:string;
    pNode:TTreenode;
    comp:TComponent;
begin
  comp:=nil;
  if (form_dfmpreview.editcontrol is TWinControl) and (csAcceptsControls in form_dfmpreview.editcontrol.ControlStyle) then
    aparent:=form_dfmpreview.editcontrol as TWincontrol
  else
    aparent:=form_dfmpreview.editcontrol.parent;
  aName:=GetNameFromClass(aType);
  pNode:=GetNode(aParent);
  pNode:=createObject(form_DFMMain.Treeview1,pNode,aName,aType,true);
  if assigned(GetClass(aType)) then
    comp:=CreateComponent(aName,aType,aParent,form_dfmPreview)
  else if assigned(getPropertyNode(pNode,'Width')) then
  begin
    comp:=CreateComponent(aName,'TUnknownControl',aParent,form_dfmPreview);
    (comp as TUnknownControl).Text:=LocalizeString('Cap_Unknown')+':'+aType;
  end;
  if assigned(comp) then
  begin
    AssignPropsFromNode(pNode,comp);
    if comp is TControl then
    begin
      Form_DFMPreview.Sizecontrol1.RegisterControl(comp as TControl);
      if Form_DFMPalette.MI_ChangeFocus.checked then
        Form_DFMPreview.SetControl(comp as TControl);
    end;
  end;
end;

//private
Procedure TForm_DFMPalette.WMGetMinMaxInfo(Var Msg: TMessage);
Begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize:= Point(Palette_MinWidth, Palette_MinHeight);
End;

procedure TForm_DFMPalette.WMExitSizeMove(var Msg: TMessage);
begin
  msg.result:=0;
  if not isZoomed(form_DFMPreview.handle) then
    form_DFMPreview.SetFormPositions;
end;

//public
procedure TForm_DFMPalette.LoadSettings(imagelist:TImagelist;PageControl:TPageControl);
var ini:Tinifile;
    sl:TStringlist;
    i,j,menu:integer;
    tb:TToolbar;
    c:TControl;
    iFName:string;
begin
  //tb:=nil;
  imagelist1.clear;
  for i:=PageControl1.PageCount-1 downto 0 do
    PageControl1.Pages[i].free;
  iFName:=extractfilepath(paramstr(0))+'images.bmp';
  if FileExists(iFName) then
    loadimages(imagelist,iFName);
  ini:=Tinifile.create(extractfilepath(paramstr(0))+'components.obj');
  try
    sl:=TStringlist.create;
    try
      ini.readsections(sl);
      for i:=0 to sl.count-1 do
      begin
        menu:=ini.ReadInteger(sl.strings[i],'<menu>',-1);
        if menu>-1 then
        begin
          if menu>Pagecontrol1.PageCount-1 then
          begin
            for j:=Pagecontrol1.PageCount to menu do
            begin
              CreateTab(PageControl1,imagelist,ScrollbarChange);
            end;
          end;
          Pagecontrol1.Pages[menu].TabVisible:=true;
          c:=Pagecontrol1.Pages[menu].Controls[0];
          tb:=(c as TToolbar);
          createToolButton(tb,
            ini.ReadInteger(sl.strings[i],'<menu_pos>',-1),
            ini.ReadInteger(sl.strings[i],'<menu_imgidx>',99),
            sl.strings[i],AddCompClick);
          if imagelist.Count=0 then
          begin
            tb.ShowCaptions:=true;
            tb.Images:=nil;
          end;
        end;
      end;
    finally
      sl.free;
    end;
  finally
    ini.free;
  end;

  for i:=0 to pagecontrol1.PageCount-1 do
  begin
    tb:=(Pagecontrol1.Pages[i].Controls[0] as TToolbar);
    SortButtons(tb);
    tb.Width:=tb.ButtonCount * tb.ButtonWidth+4; //fix autosizing-bug
  end;
  //select first tab
  if pagecontrol1.pagecount>0 then
    pagecontrol1.ActivePage:=pagecontrol1.pages[0];
end;

//form-methodes
procedure TForm_DFMPalette.FormCreate(Sender: TObject);
begin
  MI_ChangeFocus.checked:=cfg.ChangeFocus;
  MI_IgnoreFormMove.checked:=cfg.IgnoreFormMove;
end;

procedure TForm_DFMPalette.FormResize(Sender: TObject);
var i:integer;
    tb:TToolbar;
begin
  for i:=0 to Pagecontrol1.PageCount-1 do
  begin
    tb:=(Pagecontrol1.Pages[i].Controls[0] as TToolbar);
    if tb.buttonwidth*tb.buttoncount > Pagecontrol1.Width-10 then
    begin
      (Pagecontrol1.Pages[i].Controls[1] as TScrollbar).max:=(tb.width - Pagecontrol1.Width+15) div 10;
      (Pagecontrol1.Pages[i].Controls[1] as TScrollbar).visible:=true;
    end else
    begin
      (Pagecontrol1.Pages[i].Controls[1] as TScrollbar).Position:=0;
      (Pagecontrol1.Pages[i].Controls[1] as TScrollbar).visible:=false;
    end;
  end;
end;

procedure TForm_DFMPalette.Btn_ActionsClick(Sender: TObject);
var p:TPoint;
begin
  p:=panel1.clienttoscreen(point(Btn_Actions.left,Btn_Actions.top+Btn_Actions.height));
  Popup_EditDFM.Popup(p.x,p.y);
end;

procedure TForm_DFMPalette.Popup_EditDFMPopup(Sender: TObject);
begin
  UpdatePopup;
  selectControl(form_DFMPreview.editcontrol);
end;

procedure TForm_DFMPalette.CheckMenuItem(Sender: TObject);
begin
  (Sender as TMenuItem).Checked:=not (Sender as TMenuItem).Checked;
end;

procedure TForm_DFMPalette.MI_MoveTopClick(Sender: TObject);
var ec:TControl;
begin
  if not MI_KeepZOrder.Checked then
    Form_DFMMain.TB_MoveClick(Form_DFMMain.TB_MoveBottom);
  Form_DFMPreview.editcontrol.bringtoFront;
  ec:=Form_DFMPreview.Editcontrol;
  Form_DFMPreview.ReRegisterComponents;
  Form_DFMPreview.SizeControl1.AddTarget(ec);
end;

procedure TForm_DFMPalette.MI_MoveBottomClick(Sender: TObject);
var ec:TControl;
begin
  if not MI_KeepZOrder.Checked then
    Form_DFMMain.TB_MoveClick(Form_DFMMain.TB_MoveTop);
  ec:=Form_DFMPreview.Editcontrol;
  ec.SendToBack;
  Form_DFMPreview.ReRegisterComponents;
  Form_DFMPreview.SizeControl1.AddTarget(ec);
end;

procedure TForm_DFMPalette.MI_EditClick(Sender: TObject);
begin
  form_DFMMain.TB_EditClick(sender);
end;

procedure TForm_DFMPalette.MI_DeleteClick(Sender: TObject);
  function AllowDelete(Ctrl: TControl): Boolean;
  var
    tn:TTreenode;
  begin
    Result := False;
    tn:=GetNode(Ctrl);
    if not assigned(tn) then
      Exit;
    tn.Selected:=true;
    form_DFMMain.TB_DeleteClick(self);
    Result := not assigned(GetNode(ctrl));
  end;

var
  i:integer;

  procedure UnregisterChilds(Ctrl: TControl);
  var
    x: Integer;
    Ctrl_Sub: TControl;
  begin
    x := Form_DFMPreview.sizecontrol1.RegisteredIndex(Ctrl);
    if x > -1 then //target registered
    begin
      //If x>i --> i stays the same as the control was processed already ...
      //   x=i --> same control (prevented by precondition)
      //   x<i --> the index of c is lowered by deleting c as c moves towards index 0
      if x < i Then
        Dec(i);
    end;
    Form_DFMPreview.Sizecontrol1.UnRegisterControl(Ctrl);
    //Look for SubControls that might be registered ...
    if Ctrl is TWinControl then
    begin
      for x := TWinControl(Ctrl).ControlCount-1 downto 0 do
      begin
        Ctrl_Sub := TWinControl(Ctrl).Controls[x];
        UnregisterChilds(Ctrl_Sub);
        Ctrl_Sub.Free;
      end;
    end;
  end;

  function IsChildOf(Parent: TControl; ProbableChild: TControl): Boolean;
  var child:TControl;
  begin
    child:=ProbableChild;
    while Assigned(Child) Do
    begin
      if Child = Parent Then
      begin
        //The given ProbableChild object is a true child of the assumed parent
        Result := True;
        Exit;
      end;
      Child := Child.Parent;
    end;
    Result := False;
    Exit;
  end;

var
  c:TControl;
begin
  with Form_DFMPreview do
  begin
    i := SizeControl1.TargetCount - 1;
    while i >= 0 do
    begin
      c:=SizeControl1.Targets[i];
      if AllowDelete(c) then //Shorthand for asking the user for confirmation AND if the TN really exists ...
      begin
        UnregisterChilds(c);//unregister target and Childs
        c.Free;
        editcontrol:=Form_DFMPreview.getControl;
      end;
      dec(i);
      if i>=SizeControl1.TargetCount then
        i:=SizeControl1.TargetCount-1;
    end;
    editcontrol:=getControl;
  end;
end;

procedure TForm_DFMPalette.MI_AddObjectClick(Sender: TObject);
var aName,aType,aValue:string;
    aParent:TWinControl;
    comp:TComponent;
    tn:TTreenode;
begin
  if form_dfmpreview.editcontrol is TWinControl then
    aparent:=form_dfmpreview.editcontrol as TWincontrol
  else
    aparent:=form_dfmpreview.editcontrol.parent;
  form_DFMMain.TB_AddObjectClick(form_DFMAdd);
  aName:=getObjectName(form_DFMMain.Treeview1.selected);
  aType:=getObjectClass(form_DFMMain.Treeview1.selected);
  if (form_dfmpreview.editcontrol.name<>aName) and (form_dfmPreview.FindComponent(aName)=nil) then
  begin
    comp:=CreateComponent(aName,aType,aParent,form_dfmPreview);
    tn:=form_DFMMain.treeview1.Selected.GetFirstChild;
    while (assigned(tn)) and (not isObject(tn)) do
    begin
      if not isItem(tn) then
      begin
        aName:=getPropertyName(tn);
        aType:=getPropertyValue(tn);
        aValue:=RemoveSingleQuotes(aType);
        setProperty(comp,aName,aValue);
      end;
      tn:=tn.GetNextSibling;
    end;
    if comp is TControl then
    begin
      Form_DFMPreview.Sizecontrol1.RegisterControl(comp as TControl);
      Form_DFMPreview.SetControl(comp as TControl);
    end;
  end;
end;

procedure TForm_DFMPalette.MI_CopyClick(Sender: TObject);
begin
  Form_DFMPreview.setCopyAction(caCopy);
end;

procedure TForm_DFMPalette.MI_CutClick(Sender: TObject);
begin
  Form_DFMPreview.setCopyAction(caCut);
end;

procedure TForm_DFMPalette.MI_PasteClick(Sender: TObject);
begin
  Form_DFMPreview.PasteControl;
end;

procedure TForm_DFMPalette.MI_CloseClick(Sender: TObject);
begin
  Form_DFMPreview.Close;
end;

procedure TForm_DFMPalette.AddCompClick(Sender: TObject);
begin
  if sender is TMenuItem then
    AddComp(RemoveChar((sender as TMenuItem).Caption,'&'))
  else
    AddComp(RemoveChar((sender as TToolbutton).Caption,'&'))
end;

procedure TForm_DFMPalette.ScrollBarChange(Sender: TObject);
var ts:TTabSheet;
    tb:TToolbar;
begin
  ts:=((sender as TScrollbar).owner as TTabsheet);
  tb:=ts.controls[0] as TToolbar;
  tb.left:=-10*(sender as TScrollbar).position;
end;

//Action-Button / Popup
procedure TForm_DFMPalette.UpdatePopup;
begin
  MI_Copy.Enabled:=Form_DFMPreview.SizeControl1.TargetCount>0;
  MI_Cut.Enabled:=Form_DFMPreview.SizeControl1.TargetCount>0;
  MI_Paste.Enabled:=Form_DFMPreview.copyAction<>caNone; 

  MI_MoveTop.Enabled:=Form_DFMPreview.SizeControl1.TargetCount=1;
  MI_MoveBottom.Enabled:=Form_DFMPreview.SizeControl1.TargetCount=1;
  MI_Edit.Enabled:=Form_DFMPreview.SizeControl1.TargetCount<2; //0 (form) and 1
  MI_AddObject.Enabled:=Form_DFMPreview.SizeControl1.TargetCount<2;

  MI_Delete.Enabled:=Form_DFMPreview.SizeControl1.TargetCount>=1; //at least 1 component
end;

end.
