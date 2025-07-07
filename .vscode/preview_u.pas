unit preview_u;

{*
Program: DFMEdit
Unit: preview_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Form/Functions for DFM-preview

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,typinfo,SizeControl,comctrls,
  Classes_u, Menus,MenuEdit_u;

type
  TForm_DFMPreview = class(TForm)
    Popup_MenuEditor: TPopupMenu;
    MI_New: TMenuItem;
    MI_NewSub: TMenuItem;
    MI_Delete: TMenuItem;
    MI_DeleteSub: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SizeCtrl1StartSizeMove(Sender: TObject; State: TSCState);
    procedure SizeControl1EndSizeMove(Sender: TObject; State: TSCState);
    procedure SizeControl1MouseDown(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeControl1SetCursor(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeControl1TargetChange(Sender: TObject);
    procedure MI_NewClick(Sender: TObject);
    procedure MI_NewSubClick(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure MI_DeleteSubClick(Sender: TObject);
  private
    { Private-Deklarationen }
    copyControl:TControl;
    fcopylist:TList;
    procedure ControlMoved(Control:TControl;NewRect:TRect;SelectNode:Boolean);
    procedure FormMoved(var Msg: TWMMove);message WM_Move;
    procedure FormSized(var Msg: TWMSize);message WM_SIZE;
    function SelSameType:boolean;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMDrawAlignGrid(var Msg:TMessage);message WM_DrawAlignGrid;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  public
    { Public-Deklarationen }
    SizeControl1:TSizeCtrl;
    copyAction:TCopyAction;
    EditControl:TControl;
    CV:TCanvas;
    me:TMenuEditor;
    tn_root:TTreenode;
    procedure CreateMenuEditor(rootnode:TTreenode);
    procedure MenuMove(Sender,Source:TObject;FromIndex,ToIndex:integer);
    procedure MenuAdd(Sender:TObject;Index:integer);
    procedure MenuDelete(Sender:TObject;Index:integer);
    procedure MEChange(Sender: TObject;Index:Integer;Propname:string);
    procedure MEClick(Sender: TObject);
    function GetControl:TControl;
    procedure CreateSizeControl;
    procedure FreeSizeControl;
    procedure PasteControl;
    procedure ReRegisterComponents;
    procedure SetControl(c:Tcontrol);
    procedure setControlPos(control:TControl);
    Procedure SetCopyAction(CA:TCopyAction);
    procedure SetFormPositions;
    procedure DrawHelpLines;
  end;

var
  Form_DFMPreview: TForm_DFMPreview;

function getNode(Control:TControl):TTreenode;
function selectControl(Control:TControl):TTreenode;

implementation

uses main_u,rttifunctions_u, functions_u, inspector_u,palette_u;

{$R *.DFM}

procedure RegComponents(aParent: TWinControl; SizeCtrl: TSizeCtrl);
var
  i: integer;
begin
  for i := 0 to aParent.ControlCount -1 do
  begin
    SizeCtrl.RegisterControl(aParent.Controls[i]);
    if aParent.Controls[i] is TWinControl then
      RegComponents(TWinControl(aParent.Controls[i]), SizeCtrl);
  end;
end;

//private
function getNode(Control:TControl):TTreenode;
begin
  if Control=Form_DFMPreview then
    result:=Form_DFMMain.treeview1.items.GetFirstNode
  else
    result:=GetObjectNode(control.name);
end;

function selectControl(Control:TControl):TTreenode;
begin
  result:=getNode(Control);
  if assigned(result) then
  begin
    result.selected:=true;
    result.Expand(false);
  end;
end;

//form-private
procedure TForm_DFMPreview.ControlMoved(Control:TControl;NewRect:TRect;SelectNode:Boolean);
var tn:TTreenode;
begin
  if not assigned(me) then
  begin
    //left, top, width und height ändern
    if Assigned(SizeControl1) then
    begin
      SizeControl1.BtnColor:=clBlue;
      SizeControl1.Update;
    end;
    if SelectNode then
      tn:=SelectControl(Control)
    else
      tn:=getNode(Control);
    if assigned(tn) then
    begin
      AddPropertyNode(tn,'Left',IntToStr(NewRect.Left));
      AddPropertyNode(tn,'Top',IntToStr(NewRect.Top));
      AddPropertyNode(tn,'Width',IntToStr(NewRect.Right-NewRect.Left));
      AddPropertyNode(tn,'Height',IntToStr(NewRect.Bottom-NewRect.Top));
    end;
    form_DFMInspector.GetSizeProps(control);
    filechanged:=true;
  end;
end;

procedure TForm_DFMPreview.FormMoved(var Msg: TWMMove);
begin
  if assigned(self) and visible and not loading then
  begin
    if not Form_DFMPalette.MI_IgnoreFormMove.Checked then
      ControlMoved(Self,Rect(left,top,left+width,top+height),false);
    SetFormPositions;
  end;
  msg.result:=0;
end;

procedure TForm_DFMPreview.FormSized(var Msg: TWMSize);
begin
  if assigned(self) and visible and not loading then
  begin
    ControlMoved(Self,Rect(left,top,left+width,top+height),false);
    Realign;
    SetFormPositions;
  end;
  msg.result:=0;
end;

function TForm_DFMPreview.SelSameType:boolean;
var i:integer;
    s:string;
begin
  result:=true;
  if Sizecontrol1.targetCount>1 then
  begin
    s:=sizecontrol1.targets[0].classname;
    for i:=1 to Sizecontrol1.targetCount-1 do
    begin
      if sizecontrol1.targets[i].classname<>s then result:=false;
    end;
  end;
end;

procedure TForm_DFMPreview.WMExitSizeMove(var Msg: TMessage);
begin
  msg.result:=0;
end;

procedure TForm_DFMPreview.WMDrawAlignGrid(var Msg:TMessage);
begin
  DrawHelpLines;
end;

procedure TForm_DFMPreview.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  SetFormPositions;
end;

//form-public
function TForm_DFMPreview.GetControl:TControl;
begin
  if SizeControl1.targetCount>0 then
    result:=SizeControl1.Targets[0]
  else
    result:=self;//nil;
end;

procedure TForm_DFMPreview.CreateSizeControl;
begin
  FreeSizeControl;
  SizeControl1:=TSizeCtrl.Create(self);
  sizecontrol1.OnTargetChange:=SizeControl1TargetChange;
  sizecontrol1.OnSetCursor:=SizeControl1SetCursor;
  sizecontrol1.OnMouseDown:=SizeControl1MouseDown;
  RegComponents(self,SizeControl1);
  SizeControl1.Enabled := true;
  Sizecontrol1.GridSize:=cfg.GridSize;
  SizeControl1.SnapToGrid:=cfg.SnapToGrid;
  SizeControl1.OnKeyDown:=FormKeyDown;
  SizeControl1.PopupMenu:=form_DFMPalette.Popup_editDFM;
  SizeControl1.OnStartSizeMove:=SizeCtrl1StartSizeMove;
  SizeControl1.OnEndSizeMove:=SizeControl1EndSizeMove;
end;

procedure TForm_DFMPreview.FreeSizeControl;
begin
  if assigned(SizeControl1) then
  begin
    SizeControl1.Enabled:=false;
    SizeControl1.OnTargetChange:=nil;
    FreeAndNil(SizeControl1);
  end;
end;

procedure TForm_DFMPreview.PasteControl;
var tn,tn2:TTreenode;
    comp:TComponent;
    ec:Tcontrol;
    i:integer;
begin
  ec:=Editcontrol;
  if fcopylist.indexof(editcontrol)>-1 then exit;
  for i:=0 to fcopylist.count-1 do
  begin
    copycontrol:=Tcontrol(fcopylist.items[i]);

  tn:=GetNode(copycontrol);
  tn2:=GetNode(ec);
  if (Ec is TWinControl) then
  begin
    if assigned(tn) and assigned(tn2) then
    begin
      comp:=copycontrol;
      case CopyAction of
        caCut:
        begin
          CopyControl.Parent:=TWinControl(Ec);
          tn.MoveTo(tn2,naAddChild);
          setControlPos(copycontrol);
          Sizecontrol1.Update;
        end;
        caCopy:
        begin
          tn:=NodeCopy(tn2,tn);
          comp:=cloneobject(tn,copycontrol,self,ec);
          (comp as TControl).BringtoFront;
          setControlPos(comp as TControl);
          sizecontrol1.RegisterControl(comp as TControl);
        end;
      end;
      sizecontrol1.AddTarget(comp as TControl);
    end;
  end;
  sizecontrol1.DeleteTarget(ec);
  end;
  setCopyAction(caNone);
end;

procedure TForm_DFMPreview.ReRegisterComponents;
begin
  SizeControl1.UnRegisterAll;
  RegComponents(self,SizeControl1);
end;

procedure TForm_DFMPreview.SetControl(c:TControl);
begin
  Sizecontrol1.ClearTargets;
  if assigned(c) then
    SizeControl1.AddTarget(c);
end;

procedure TForm_DFMPreview.setControlPos(control:TControl);
begin
  if (control).left>(control).parent.width-(Control).width-10 then
    (Control).left:=(Control).parent.width-(Control).width
  else
    (Control).left:=Control.left+10;
  if (control).top>(Control).parent.height-(Control).height-10 then
    (Control).top:=(Control).parent.height-(Control).height
  else
    (Control).top:=Control.top+10;
  ControlMoved(control,rect(control.left,control.top,control.left+control.width,control.top+control.height),false);
end;

Procedure TForm_DFMPreview.SetCopyAction(CA:TCopyAction);
var c1,c2:TControl;
    i:integer;
begin
  if assigned(Sizecontrol1) then
  begin
    if (getControl is TControl) or (ca=caNone) then
    begin
      case ca of
        caNone: SizeControl1.Btncolor:=clBlue;
        caCopy: SizeControl1.Btncolor:=clGreen;
        caCut: SizeControl1.Btncolor:=clRed;
      end;
      copyAction:=CA;
      if ca<>caNone then
      begin
        fcopylist.clear;
        //create a list with only the top selected controls
        for i:=0 to sizecontrol1.targetcount-1 do
        begin
          c1:=sizecontrol1.targets[i];
          c2:=c1;
          while assigned(c2) do
          begin
            c2:=c2.parent;
            if sizecontrol1.TargetIndex(c2)>-1 then c1:=c2;
          end;
          if fcopylist.IndexOf(c1)=-1 then //if selected control isn't in list add it
            fcopylist.add(c1);
        end;
      end;
    end;
  end;
end;

procedure TForm_DFMPreview.SetFormPositions;
begin
    if assigned(form_DFMInspector) and assigned(Form_DFMPalette) then
    begin
      if top>=Form_DFMPalette.Height then
        Form_DFMPalette.top:=top-form_DFMPalette.Height
      else if top+Height+Form_DFMPalette.Height<=screen.Height then
      begin
        if height<form_DFMInspector.Height then
          Form_DFMPalette.top:=top+form_DFMInspector.Height
        else
          Form_DFMPalette.top:=top+Height;
      end else
        form_DFMPalette.Top:=0;

      if (form_DFMPalette.Top=0)then
        form_DFMInspector.top:=form_DFMPalette.Height
      else
        form_DFMInspector.top:=Top;
      if left>=form_DFMInspector.Width then
      begin
        form_DFMInspector.left:=left-form_DFMInspector.Width;
        form_DFMPalette.left:=form_DFMInspector.left;
      end else
      begin
        if left+width+form_DFMInspector.width<=screen.width then
          form_DFMInspector.left:=left+Width
        else
          form_DFMInspector.left:=0;
        form_DFMPalette.left:=Left;
      end;
      if (form_DFMPalette.left=0) or (form_DFMPalette.top=0) or
         (form_DFMInspector.left=0) or (form_DFMInspector.top=0) then
    end;
end;

//events
procedure TForm_DFMPreview.FormActivate(Sender: TObject);
begin
  Form_DFMInspector.HideInplaceEditor;
  setWindowPos(form_DFMInspector.Handle,HWND_TOP,0,0,0,0,SWP_NoMove or SWP_NoSize or SWP_NoActivate);
  setWindowPos(form_DFMPalette.Handle,HWND_TOP,0,0,0,0,SWP_NoMove or SWP_NoSize or SWP_NoActivate);
end;

procedure TForm_DFMPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(SizeControl1) then FreeSizeControl;
  if assigned(me) then
  begin
    me.OnAdd:=nil;
    me.OnMove:=nil;
    me.OnDelete:=nil;
  end;
  form_DFMInspector.close;
  form_DFMPalette.close;
  Form_DFMMain.enabled:=true; //is disabled when menueditor is shown
  //there maybe problems when moving treenodes with menueditor if item/propertycount is changed in treeview 
end;

procedure TForm_DFMPreview.FormCreate(Sender: TObject);
begin
  fcopylist:=TList.create;
  me:=nil;
  CV:=TCanvas.create;
  CV.Handle := GetWindowDC( GetDesktopWindow ); 
end;

procedure TForm_DFMPreview.FormDestroy(Sender: TObject);
begin
  fcopylist.free;
  if assigned(me) then me.free;
  CV.free;
end;

procedure TForm_DFMPreview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if SizeControl1.Targetcount<2 then //allow most function only when 1 control is selected
  begin
    if key=vk_Return then
    begin
      Form_DFMMain.TB_EditClick(self);
      Key:=0;
    end else if key=vk_Insert then
    begin
      Form_DFMPalette.MI_AddObjectClick(self);
    end;
  end;
  if key=vk_delete then
  begin
    Form_DFMPalette.MI_DeleteClick(self);
  end else if Key=ord('X') then
  begin
    if (Shift=[ssCtrl]) then
    begin
      Form_DFMPalette.MI_CutClick(self);
    end;
  end else if Key=ord('C') then
  begin
    if (Shift=[ssCtrl]) then
    begin
      Form_DFMPalette.MI_CopyClick(self);
    end;
  end else if Key=ord('V') then
  begin
    if (Shift=[ssCtrl]) and (copyAction<>caNone) then
    begin
      Form_DFMPalette.MI_PasteClick(self);
    end;
  end;
end;

procedure TForm_DFMPreview.FormPaint(Sender: TObject);
var i,j,ic,jc:integer;
begin
  if assigned(sizecontrol1) and (sizecontrol1.Gridsize>1) then
  begin
    ic:=self.height div sizecontrol1.Gridsize;
    jc:=self.width div sizecontrol1.Gridsize;
    for i:=0 to ic do
    begin
      for j:=0 to jc do
      begin
        canvas.Pixels[j*sizecontrol1.Gridsize,i*sizecontrol1.Gridsize]:=clBlack;
      end;
    end;
  end;
end;

procedure TForm_DFMPreview.FormShow(Sender: TObject);
begin
  if not assigned(me) then
  begin                                                           
    editcontrol:=self;
    Popup_MenuEditor.Free;
    if not Assigned(SizeControl1) then
    begin
      createSizeControl;
      setCopyAction(caNone);
    end;
    form_DFMInspector.GetPropValues(editcontrol);
    form_DFMPalette.show;
    form_DFMInspector.show;
  end;
  SetFormPositions;
end;

procedure TForm_DFMPreview.SizeCtrl1StartSizeMove(Sender: TObject;
  State: TSCState);
begin
  if (cfg.DrawGrid) then
    postmessage(self.handle,WM_DrawAlignGrid,0,0);
end;

procedure TForm_DFMPreview.SizeControl1EndSizeMove(Sender: TObject; State: TSCState);
var i:integer;
    c:TControl;
const
  FLAGS = RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN;
begin
  for i:=0 to SizeControl1.TargetCount-1 do
  begin
    c:=SizeControl1.Targets[i];
    ControlMoved(c,rect(c.left,c.top,c.left+c.width,c.top+c.height),false);
  end;
  if cfg.DrawGrid then
  begin
    //invalidateControl(self);
    RedrawWindow(self.Handle, nil, 0, FLAGS);
  end;
end;

procedure TForm_DFMPreview.SizeControl1MouseDown(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  //if clicking the PageControl, it's kind of nice to be able to change pages.
  //So, let's see if a new page needs to be displayed ...
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      with TargetPt do i := MyIndexOfTabAt(TPageControl(Target), X, Y);
      if (i >= 0) and ( ActivePage.PageIndex <> i) then
      begin
        //OK, we'll manage things from here ...
        handled := true;
        //yes, we do need to show a different page ...
        ActivePage := Pages[i];
        //now, make sure all the right controls are registered with SizeCtrl ...
        SizeControl1.UnRegisterAll;
        RegComponents(self, SizeControl1);
        //finally, reset the target ...
        SizeControl1.AddTarget(Target);
      end;
    end;
end;

procedure TForm_DFMPreview.SizeControl1SetCursor(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  //if clicking the PageControl, it's kind of nice to show an appropriate
  //cursor if clicking a new page ...
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      with TargetPt do i := MyIndexOfTabAt(TPageControl(Target), X, Y);
      if (i >= 0) and (ActivePage.PageIndex {ActivePageIndex} <> i) then
      begin
        //OK, we'll manage things from here ...
        handled := true;
        //assign the cursor directly ...
        windows.SetCursor(screen.Cursors[crDefault]);
      end;
    end;
end;

procedure TForm_DFMPreview.SizeControl1TargetChange(Sender: TObject);
var Newtarget:TControl;
    i:integer;
begin
  SizeControl1.Btncolor:=clBlue;
  Form_DFMInspector.HideInPlaceEditor;
  if (SizeControl1.Targetcount<2) then
  begin
    newtarget:=nil;
    if SizeControl1.targetcount=1 then
      newtarget:=GetControl;
    if not assigned(NewTarget) then
    begin
      NewTarget:=Self;
    end;
    selectControl(newtarget);
    EditControl:=newtarget;
    form_DFMInspector.GetPropValues(editcontrol);
    Form_DFMInspector.Show;
  end else if SelSameType then
  begin
    //prevent change of name in multiselect
    i:=Form_DFMInspector.GetSGIndex('Name');
    if i>-1 then Form_DFMInspector.StringGrid1.Cells[1,i]:='N/A';
    form_DFMInspector.GetPropValues(SizeControl1.Targets[SizeControl1.Targetcount-1]);
  end else
  begin
    Form_DFMInspector.SetOIOnlySize;
    form_DFMInspector.GetSizeProps(GetControl);
  end;
  setfocus;
end;

procedure TForm_DFMPreview.MenuMove(Sender,Source:TObject;FromIndex,ToIndex:integer);
var tn,tn2:TTreenode;
begin

  tn:=TTreenode((source as TMenuEditor).Entries[fromindex].data);
  tn2:=TTreenode((Sender as TMenuEditor).data);
  if (sender = source) then
  begin
    if (fromindex<=toindex) then
    begin
      if toindex<(sender as TMenuEditor).count then
      begin
        tn.MoveTo(tn.parent.Item[toindex+(tn.parent.Count-(sender as TMenuEditor).count)],naInsert)
      end else
        tn.MoveTo(tn.parent.Item[toindex],naAdd);
    end else
      tn.MoveTo(tn.parent.Item[toindex+(tn.parent.Count-(sender as TMenuEditor).count)],naInsert);
  end else
  begin
    if toindex<(sender as TMenuEditor).count then
      tn.MoveTo(tn2.Item[toindex+tn2.count-(sender as TMenuEditor).count],naInsert)
    else
      tn.MoveTo(tn2.Item[toindex],naAdd);
  end;
  filechanged:=true;
end;

procedure TForm_DFMPreview.MenuAdd(Sender:TObject;Index:integer);
var tn:TTreenode;
begin
  tn:=TTreenode((sender as TMenuEditor).data);
  tn:=(tn.owner).AddChild(tn,'object '+(sender as TMenuEditor).Entries[index].GetPropertyValue('Name')+': TMenuItem');
  with (tn.owner).AddChildFirst(tn,'Caption = '''+(sender as TMenuEditor).Entries[index].GetpropertyValue('caption')+'''') do
  begin
    imageIndex:=1;
    SelectedIndex:=1;
  end;
  (sender as TMenuEditor).Entries[index].Data:=tn;
  filechanged:=true;
end;

procedure TForm_DFMPreview.MenuDelete(Sender:TObject;Index:integer);
var tn:TTreenode;
begin
  tn:=TTreenode((sender as TMenuEditor).Entries[index].data);
  tn.free;
  filechanged:=true;
end;

procedure TForm_DFMPreview.MEChange(Sender: TObject;Index:Integer;Propname:string);
var PropValue:string;
    tn:TTreenode;
begin
  tn:=TTreenode((sender as TMenuEditor).Entries[index].data);
  PropValue:=(sender as TMenuEditor).Entries[index].GetPropertyValue(PropName);
  if ansiCompareText(propName,'name')=0 then
  begin
    //renameControl, should be handled by OI
  end else
  begin
    tn:=GetPropertyNode(tn,PropName);
    if assigned(tn) then
    begin
      if PropValue='' then
        tn.free
      else
        tn.Text:=PropName+' = '+PropValue;
    end;
  end;
end;

procedure TForm_DFMPreview.MEClick(Sender: TObject);
var editor:TMenuEditor;
begin
  editor:=(sender as TMenueditor);
  form_DFMInspector.show;
  Form_DfmInspector.GetPropValues(editor.Entries[editor.itemindex]);
end;

procedure TForm_DFMPreview.CreateMenuEditor(rootnode:TTreenode);
begin
  me:=TMenuEditor.create(self);
  me.parent:=self;
  tn_root:=rootnode;
  me.OnClick:=MeClick;
  me.OnMove:=MenuMove;
  me.PopupMenu:=Popup_MenuEditor;
  BuildMenu(me,rootnode);
  me.OnAdd:=MenuAdd;
  me.OnDelete:=MenuDelete;
end;

procedure TForm_DFMPreview.MI_NewClick(Sender: TObject);
var me:TMenuEditor;
    mEntry:TMenuEntry;
    oName:string;
begin
  if sender is TMenuEditor then
    me:=Sender as TMenuEditor
  else
    me:=(Popup_MenuEditor.PopupComponent as TMenuEditor);
  mentry:=me.AddEntry('New_entry');
  mentry.SetPropertyValue('Name',GetNameFromClass('TMenuItem'));
  oName:=mEntry.GetPropertyValue('Name')+': TMenuItem';
  TTreenode(mentry.data).Text:='object '+oName;
  Form_DFMMain.CB_Objects.Items.Add(oName);
end;

procedure TForm_DFMPreview.MI_NewSubClick(Sender: TObject);
var me:TMenuEditor;
    mEntry:TMenuEntry;
begin
  me:=(Popup_MenuEditor.PopupComponent as TMenuEditor);
  mEntry:=me.Entries[me.Itemindex];
  me:=me.AddSubMenu(me.ItemIndex,true);
  me.Data:=mEntry.data;
  MI_NewClick(me);
end;

procedure TForm_DFMPreview.MI_DeleteClick(Sender: TObject);
var me:TMenuEditor;
begin
  me:=(Popup_MenuEditor.PopupComponent as TMenuEditor);
  me.DeleteEntry(me.ItemIndex);
end;

procedure TForm_DFMPreview.MI_DeleteSubClick(Sender: TObject);
var me:TMenuEditor;
begin
  me:=(Popup_MenuEditor.PopupComponent as TMenuEditor);
  me.DeleteSubMenu(me.itemindex);
end;

procedure TForm_DFMPreview.DrawHelpLines;
var sz,xc,yc,ix,iy:integer;
    p:TPoint;
begin
  sz:=cfg.GridSize*5;
  xc:=clientwidth div sz;
  yc:=clientheight div sz;
  p:=ClientToScreen(Point(0,0));
  CV.Pen.Color:=clGreen;
  for iy:=0 to yc do
  begin
    for ix:=0 to xc do
    begin
      CV.MoveTo(p.x+ix*sz,p.y);
      CV.LineTo(p.x+ix*sz,p.y+ClientHeight);
    end;
    CV.MoveTo(p.x,p.y+iy*sz);
    CV.LineTo(p.x+ClientWidth,p.y+iy*sz);
  end;
end;

end.
