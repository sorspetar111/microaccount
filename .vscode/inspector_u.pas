unit inspector_u;

{*
Program: DFMEdit
Unit: inspector_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Form/Functions for ObjectInspector

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls,typinfo, ExtCtrls,functions_u, rttifunctions_u,
  SizeControl,
  Buttons,Classes_u,comctrls, Menus, checklst;

type
  TForm_DFMInspector = class(TForm)
    HeaderControl1: THeaderControl;
    StringGrid1: TStringGrid;
    Panel1: TPanel;
      CompLabel: TLabel;
    
    Edit1: TEdit;
    UpDown1: TUpDown;
    ComboBox1: TComboBox;
    CheckListBox1: TCheckListBox;
    HotKey1: THotKey;
    DateTimePicker1: TDateTimePicker;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    //HeaderControl
    procedure HeaderControl1SectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure HeaderControl1SectionTrack(HeaderControl: THeaderControl;
      Section: THeaderSection; Width: Integer; State: TSectionTrackState);
    //Stringgrid
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; Col, Row: Integer;
      R: TRect; State: TGridDrawState);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1TopLeftChanged(Sender: TObject);
    //InPlace-Editors
    procedure Edit1Exit(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure UpDown1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure ComboBox1Exit(Sender: TObject);
    procedure ComboBox1DblClick(Sender: TObject);
    procedure CheckListBox1Exit(Sender: TObject);
    procedure HotKey1Exit(Sender: TObject);
    procedure DateTimePicker1Exit(Sender: TObject);
  private
    { Private-Deklarationen }
    oldrow,selrow,lastline:integer;
    oldvalue:string;
    showUpdown:Boolean;
    editcontrol:TComponent;//Control;
    sgwndproc:TWndMethod;
    FWCombo:TFWCombobox;
    inPlaceEditor:TWinControl;
    procedure WMGetMinMaxInfo(Var Msg: TMessage); Message WM_GETMINMAXINFO;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
    procedure WMMouseWheel(var msg:TMessage); message WM_MOUSEWHEEL;
    procedure SGWindowProc(var Msg: TMessage);
    procedure FreeObjects;
    procedure AlignInPlaceEditor(col,row:integer);
    procedure SetInPlaceEditorValue(value:string);
    function updatecontrol(control:TControl;prop,value:string):boolean;
    function changeproperty:boolean;
  public
    { Public-Deklarationen }
    procedure GetPropValues(component:TComponent);
    function GetSGIndex(CellCaption: string):integer;
    procedure GetSizeProps(Control:TControl);
    procedure HideInPlaceEditor;
    procedure SetOIOnlySize;
    function updatecontrols(prop:string;value:string):boolean;
  published
    property Editor:TWinControl read inPlaceEditor;
  end;

var
  Form_DFMInspector: TForm_DFMInspector;

implementation

uses preview_u, main_u, addproperty_u, dfmparse_u,localize_u,MenuEdit_u,
  palette_u;

{$R *.DFM}
//********************************************  private  **********************************************************
//message-methods first
Procedure TForm_DFMInspector.WMGetMinMaxInfo(Var Msg: TMessage);
Begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize:= Point(Inspector_MinWidth, Inspector_MinHeight);
End;

procedure TForm_DFMInspector.WMExitSizeMove(var Msg: TMessage);
begin
  msg.result:=0;
  if not isZoomed(form_DFMPreview.handle) then
    form_DFMPreview.SetFormPositions;
end;

procedure TForm_DFMInspector.CMDialogKey(var msg: TCMDialogKey);
begin
  if msg.CharCode = VK_TAB then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
    begin
      if (stringgrid1.Row>0) then
      begin
        dec(selrow);
        stringgrid1.Row:=stringgrid1.Row-1
      end;
    end else
    begin
      if (stringgrid1.Row<stringgrid1.RowCount-1) then
      begin
        inc(selrow);
        stringgrid1.Row:=stringgrid1.Row+1;
      end;
    end;
    msg.Result := 1;
    exit;
  end else if assigned(inPlaceEditor) then
  begin
    if msg.CharCode=vk_return then
    begin
      THackedWinCtrl(inplaceEditor).onExit(inplaceEditor); //set last changed property
      updatecontrols(Stringgrid1.cells[0,stringgrid1.row],Stringgrid1.cells[1,stringgrid1.row]);
      oldValue:=Stringgrid1.cells[1,stringgrid1.row];
    end else if msg.CharCode=vk_escape then
    begin
      SetInPlaceEditorValue(oldvalue);
    end;
  end else inherited;
end;

procedure TForm_DFMInspector.WMMouseWheel(var Msg: TMessage);
begin
  if Msg.wParam > 0 then
    Stringgrid1.Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
    Stringgrid1.Perform(WM_VSCROLL, SB_LINEDOWN, 0);
  Msg.Result := 0;
end;

procedure TForm_DFMInspector.SGWindowProc(var Msg: TMessage);
begin
  if msg.Msg=WM_EraseBkgnd then
    //
  else
    sgwndproc(msg);
end;

//other private methods
procedure TForm_DFMInspector.AlignInPlaceEditor(col,row:integer);
var r:TRect;
begin
  Updown1.Visible:=showUpDown;
  if assigned(inPlaceEditor) then
  begin
    if row in [stringgrid1.TopRow..stringgrid1.TopRow+Stringgrid1.VisibleRowCount-1] then
    begin
      r:=Stringgrid1.CellRect(col,row);
      r:=Rect(r.left+stringgrid1.left+2,r.top+stringgrid1.top+2,r.right+stringgrid1.left+2,r.bottom+stringgrid1.top+2);
      if showUpdown then
      begin
        r.Right:=r.right-updown1.width;
        updown1.top:=r.top;
        updown1.left:=r.right;
      end;
      inPlaceEditor.SetBounds(r.left,r.top,r.right-r.left,r.bottom-r.top);
      inPlaceEditor.Visible:=true;
      inPlaceEditor.SetFocus;
    end else
    begin
      inPlaceEditor.Visible:=false;
      Updown1.Visible:=false;
    end;
  end;
end;

function TForm_DFMInspector.changeproperty:boolean;
begin
  result:=updatecontrols(Stringgrid1.cells[0,oldrow],Stringgrid1.cells[1,oldrow]);
end;

procedure TForm_DFMInspector.FreeObjects;
var i:integer;
begin
  for i:=0 to Stringgrid1.rowcount-1 do  //was 0
    if assigned(stringgrid1.objects[0,i]) then
    begin
      stringgrid1.objects[0,i].free;
      stringgrid1.objects[0,i]:=nil;
    end;
end;

procedure TForm_DFMInspector.SetInPlaceEditorValue(value:string);
var i:integer;
begin
  if inPlaceEditor=Edit1 then
  begin
    edit1.Text:=value;
    edit1.SelLength:=length(edit1.text);
  end else if inplaceEditor=combobox1 then
  begin
    combobox1.Text:=Value;
    combobox1.SelLength:=length(combobox1.text);
  end else if inplaceEditor=hotkey1 then
  begin
    hotkey1.Hotkey:=TextToShortCut(Value);
  end else if inPlaceEditor=checklistbox1 then
  begin
    for i:=0 to checklistbox1.items.count-1 do
      checklistbox1.Checked[i]:=(pos(checklistbox1.Items.strings[i],value)>0);
  end else if inPlaceEditor=datetimePicker1 then
  begin
    try
      datetimePicker1.Date:=StrToDate(value);
    except
      try
        datetimePicker1.Time:=StrToTime(value);
      except
        abort;
      end;
    end;
  end;
end;

function TForm_DFMInspector.updatecontrol(control:TControl;prop,value:string):boolean;
var s,oType:string;
    tn:TTreenode;
    sl:TStringlist;
    i:integer;
begin
  sl:=TStringlist.create;
  value:=RemoveSingleQuotes(value);(*
  result:=false;
  case setproperty(control,prop,value) of
    sprNotSupported:showmessage(format(LocalizeString('Msg_PropertyNotSupported'),[prop]));
    sprPropNotExists: if Value<>'N/A' then showmessage(format(LocalizeString('Msg_UnknownProperty'),[prop]));
    sprError: showmessage(format(LocalizeString('Msg_ErrorSettingProperty'),[prop]));
    sprOK:result:=true;
  end;            *)
  result:=setproperty(control,prop,value)=sprOK;
  if result then
  begin
    if Prop='Name' then
    begin
      renamecontrol(oldvalue,RemoveSingleQuotes(value),'');
    end else
    begin
      if control=form_dfmpreview then
      begin
        //addPropertyNodeTxt(GetFormName,prop,value)
        tn:=GetObjectNode(GetFormName);
        oType:='TForm';
      end else
      begin
        tn:=GetObjectNode(control.name);
        oType:=GetObjectClass(tn);
      end;
      GetReplacementProperties(oType,sl);
      for i:=0 to sl.Count-1 do //change name and value
      begin
        sl.Strings[i]:=sl.Values[sl.Names[i]]+'='+sl.Names[i]
      end;
      s:=sl.Values[prop];
      if s<>'' then prop:=s;
      AddPropertyNode(tn,prop,value);
    end;
    filechanged:=true;
  end else Stringgrid1.cells[1,oldrow]:=oldValue;
  sl.free;
end;

//************************************************  public  ********************************************************
procedure TForm_DFMInspector.GetPropValues(component:TComponent);
var sL:TStringlist;
    cName,pValue:string;
    i:integer;
    co:TCellOptions;
    tn,tn_prop:TTreenode;
    mi:TMenuItem;
    ti:pTypeInfo;
    td:pTypeData;
const gr:TGridRect=(Left:0;Top:0;right:1;Bottom:0);
begin
  tn_Prop:=nil;
  mi:=TMenuItem.create(nil);
  combobox1.Text:='';
  fwcombo.Text:='';
    editcontrol:=(Component);
  if editcontrol=Form_DFMPreview then
  begin
    cName:='TForm';
    tn:=Form_DFMMain.treeview1.Items.GetFirstNode;
  end else
  begin
    td:=nil;
    ti:=TypeInfo(TMenuEntry);
    if assigned(ti) then
      td:=GetTypeData(ti);
    if assigned(td) and (component.ClassType=td.ClassType) then //funktioniert nicht
    begin
      cName:='TMenuItem';
    end else
      cName:=component.classname;
    tn:=GetObjectNode(component.name);
  end;
  sL:=TStringlist.create;
  freeObjects;
  oldrow:=0;
  stringgrid1.Selection:=gr;
  getComponentProps(cName,sl,true);
  stringgrid1.RowCount:=sl.count;
  for i:=0 to sl.count-1 do
  begin
    stringgrid1.Cells[0,i]:=sl.strings[i];
    stringgrid1.Cells[1,i]:='';
    co:=TCellOptions.create;
    co.ChangeEnabled:=not (TTypeObject(sl.Objects[i]).PropKind=tkClass);
    co.typeobject.Assign(TTypeObject(sl.Objects[i]));
    stringgrid1.Objects[0,i]:=co;
  end;
  freeSLObjects(sl);
  sl.free;

  for i:=0 to stringgrid1.RowCount-1 do
  begin
    pValue:='';
    if (lowercase(copy(stringgrid1.Cells[0,i],1,2))='on') then
    begin //get events from treeview
      if assigned(tn) then
        tn_prop:=getPropertyNode(tn,stringgrid1.Cells[0,i]);
      if assigned(tn_prop) then
        stringgrid1.Cells[1,i]:=RemoveSingleQuotes(GetPropertyValue(tn_prop));
    end else
    begin
      if (component.ClassName='TMenuEntry') then
      begin
        pValue:=(component as TMenuEntry).getPropertyValue(stringgrid1.Cells[0,i]);//GetPropertyValue(tn_prop);
        if pValue='' then
          pValue:=GetValue(mi,stringgrid1.Cells[0,i]);
      end else
        pValue:=GetValue(editcontrol,stringgrid1.Cells[0,i]);
        stringgrid1.Cells[1,i]:=pValue;
    end;
  end;
  if component<>form_DFMPreview then
    CompLabel.Caption:=editcontrol.Name+':'+cName//editcontrol.ClassName
  else
    CompLabel.Caption:=GetFormName+':'+GetFormClass;
  oldvalue:=stringgrid1.Cells[1,oldrow];
  stringgrid1.Row:=0;
  if assigned(mi) then FreeAndNil(mi);
  SetInPlaceEditorValue(Stringgrid1.Cells[Stringgrid1.Col,Stringgrid1.Row]);
end;

function TForm_DFMInspector.GetSGIndex(CellCaption: string):integer;
var i:integer;
begin
  result:=-1;
  i:=0;
  while (result=-1) and (i<Stringgrid1.rowCount) do
    if Stringgrid1.cells[0,i]=CellCaption then
      result:=i
    else
      inc(i);
end;

procedure TForm_DFMInspector.GetSizeProps(Control:TControl);
var idx:integer;
begin
  idx:=GetSGIndex('Height');
  if idx>-1 then stringgrid1.Cells[1,idx]:=GetValue(control,stringgrid1.Cells[0,idx]);
  idx:=GetSGIndex('Left');
  if idx>-1 then stringgrid1.Cells[1,idx]:=GetValue(control,stringgrid1.Cells[0,idx]);
  idx:=GetSGIndex('Top');
  if idx>-1 then stringgrid1.Cells[1,idx]:=GetValue(control,stringgrid1.Cells[0,idx]);
  idx:=GetSGIndex('Width');
  if idx>-1 then stringgrid1.Cells[1,idx]:=GetValue(control,stringgrid1.Cells[0,idx]);
  oldvalue:=stringgrid1.Cells[1,oldrow];
end;

procedure TForm_DFMInspector.HideInPlaceEditor;
begin
  if assigned(inPlaceEditor) then
  begin
    Stringgrid1.SetFocus;
    stringgrid1.rowHeights[stringgrid1.row]:=stringgrid1.defaultRowHeight;
    inPlaceEditor.Visible:=false;
    inplaceEditor:=nil;
  end;
end;

procedure TForm_DFMInspector.SetOIOnlySize;
var i:integer;
    co:TCellOptions;
begin
  freeObjects;
  Stringgrid1.RowCount:=4;
  Stringgrid1.Cells[0,0]:='Height';
  Stringgrid1.Cells[0,1]:='Left';
  Stringgrid1.Cells[0,2]:='Top';
  Stringgrid1.Cells[0,3]:='Width';
  for i:=1 to StringGrid1.RowCount-1 do
  begin
    co:=TCellOptions.create;
    Stringgrid1.objects[0,i]:=co;
  end;
  StringGrid1.row:=0;
  Stringgrid1.Col:=1;
end;

function TForm_DFMInspector.updatecontrols(prop:string;value:string):boolean;
var comp:TControl;
    I:integer;
begin
  result:=true;
  if oldValue<>value then
  begin
    if assigned(Form_DFMPreview.me) then
    begin
      if ansiCompareText(prop,'Shortcut')=0 then value:=IntToStr(TextToShortCut(Value));
      (editcontrol as TMenuEntry).SetPropertyValue(prop,value);
      if prop='Name' then
        renamecontrol(oldvalue,RemoveSingleQuotes(value),'')
      else if assigned((editcontrol as TMenuEntry).data) then
      begin
        if TCellOptions(Stringgrid1.Objects[0,stringgrid1.row]).typeobject.PropKind in [tkString, tkLString, tkWString] then
          value:=''''+value+'''';
        AddPropertyNode(((editcontrol as TMenuEntry).data as TTreenode),prop,value); //auf string prüfen
      end;
    end else 
    begin
      if form_dfmpreview.SizeControl1.TargetCount>0 then
      begin
        for i:=0 to form_dfmpreview.SizeControl1.TargetCount-1 do
        begin
          comp:=form_dfmpreview.SizeControl1.Targets[i];
          result:=result and UpdateControl(comp,prop,value);
        end;
      end else
      begin
        UpdateControl(form_dfmpreview,prop,value);
      end;
      form_dfmpreview.SizeControl1.update;
    end;
  end;
end;

//**************************************************  component events  ***********************************************
procedure TForm_DFMInspector.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if assigned(inPlaceEditor) then
  begin
    inPlaceEditor.Visible:=false;
    inPlaceEditor:=nil;
  end;
end;

procedure TForm_DFMInspector.FormCreate(Sender: TObject);
begin
  oldrow:=0;
  lastline:=-1;
  FWCombo:=TFWCombobox.create(self);
  FWCombo.Visible:=false;
  FWCombo.OnExit:=Combobox1Exit;
  FWCombo.OnDblClick:=ComboBox1DblClick;
  //FWCombo.OnKeyDown:=StringGrid1KeyDown;
  sgwndproc:=Stringgrid1.WindowProc;
  Stringgrid1.WindowProc:=SGWindowProc;
end;

procedure TForm_DFMInspector.FormDestroy(Sender: TObject);
begin
  Stringgrid1.WindowProc:=sgwndproc;
  FWCombo.free;
  freeObjects;
//  repProps.free;
end;

procedure TForm_DFMInspector.FormResize(Sender: TObject);
begin
  HeaderControl1.Sections[1].MaxWidth:=HeaderControl1.Width-50;
  HeaderControl1SectionResize(nil,nil);
  AlignInPlaceEditor(1,stringgrid1.row);
end;

//HeaderControl
procedure TForm_DFMInspector.HeaderControl1SectionResize(
  HeaderControl: THeaderControl; Section: THeaderSection);
var sw,w:integer;
begin
  Stringgrid1.canvas.Pen.Mode:=pmXor;
  if lastline>-1 then
  begin
    Stringgrid1.Canvas.MoveTo(lastline,0);
    Stringgrid1.Canvas.LineTo(lastline,Stringgrid1.height);
  end;
  Stringgrid1.canvas.Pen.Mode:=pmCopy;
  sw:=GetSystemMetrics(SM_CXVSCROLL)+4;
  stringgrid1.colwidths[0]:=HeaderControl1.Sections[1].width;
  w:=Headercontrol1.width-HeaderControl1.Sections[1].width-4-sw;
  HeaderControl1.Sections[2].maxWidth:=w;
  HeaderControl1.Sections[2].minWidth:=w;
  HeaderControl1.Sections[2].Width:=w;
  stringgrid1.colwidths[1]:=w;
  AlignInPlaceEditor(1,stringgrid1.row);
end;

procedure TForm_DFMInspector.HeaderControl1SectionTrack(
  HeaderControl: THeaderControl; Section: THeaderSection; Width: Integer;
  State: TSectionTrackState);
begin
  if Section.Index=1 then
  begin
    Stringgrid1.canvas.Pen.Mode:=pmXor;
    if lastline>-1 then
    begin
      Stringgrid1.Canvas.MoveTo(lastline,0);
      Stringgrid1.Canvas.LineTo(lastline,Stringgrid1.height);
    end;
    lastline:=width;
    Stringgrid1.Canvas.MoveTo(lastline,0);
    Stringgrid1.Canvas.LineTo(lastline,Stringgrid1.height);
    Stringgrid1.canvas.Pen.Mode:=pmCopy;
  end;
end;

//Stringgrid-methodes
procedure TForm_DFMInspector.StringGrid1DblClick(Sender: TObject);
var tn_obj,tn_Prop:TTreenode;
begin
  if editcontrol=form_DFMPreview then
    tn_Obj:=getformnode
  else
    tn_Obj:=GetObjectNode(Editcontrol.name);
  tn_Prop:=GetPropertyNode(tn_obj,Stringgrid1.cells[0,Stringgrid1.Row]);
  if not assigned(tn_Prop) then
    tn_Prop:=AddPropertyNode(tn_obj,Stringgrid1.cells[0,Stringgrid1.Row],Stringgrid1.cells[1,Stringgrid1.Row]);

  Form_DFMadd.EditDfm(eaEditProperty,tn_prop);
  Stringgrid1.cells[1,Stringgrid1.Row]:=getPropertyValue(tn_prop); //maybe problems with TShortcut and TDate/TTime that are displayed decoded in OI
  updatecontrols(Stringgrid1.cells[0,stringgrid1.row],Stringgrid1.cells[1,stringgrid1.row]);
  oldValue:=Stringgrid1.cells[1,Stringgrid1.Row];
  SetInPlaceEditorValue(oldValue);
end;

procedure TForm_DFMInspector.StringGrid1DrawCell(Sender: TObject; Col,
  Row: Integer; R: TRect; State: TGridDrawState);
var FText:string;
    ti:pTypeInfo;
    r2:Trect;
begin
  Stringgrid1.canvas.Pen.Mode:=pmCopy;
  Stringgrid1.Canvas.Font.Color:=clWindowText;
  r2:=ResizeRect(r,-2);
  if Col=0 then
  begin
    Stringgrid1.Canvas.Brush.Color:=Stringgrid1.FixedColor;
    Stringgrid1.Canvas.fillrect(r);
    //Stringgrid1.Canvas.DrawFocusRect(rect);
    if row=Stringgrid1.row then
      Frame3D(Stringgrid1.Canvas, R,clBtnShadow,clBtnHighlight,2)
    else
      Frame3D(Stringgrid1.Canvas, R,clBtnHighlight,clBtnShadow,1);
    Stringgrid1.Canvas.TextRect(r2,r2.left,r2.top,Stringgrid1.Cells[0,Row]);
  end else
  begin
    FText:=Stringgrid1.Cells[Col,Row];
    ti:=TCellOptions(Stringgrid1.Objects[0,Row]).TypeObject.CompType;

    if FText<>'' then
    begin
      if (ti)=TypeInfo(TDate) then
        FText:=DateToStr(StrToFloat(FText))
      else if (ti)=TypeInfo(TTime) then
        FText:=TimeToStr(StrToFloat(FText))
      else if (ti)=TypeInfo(TShortCut) then
        FText:=ShortCutToText(StrToInt(FText))
      else
        FText:=RemoveSingleQuotes(FText);
    end;
    if gdSelected in State then
    begin
      Stringgrid1.Canvas.Brush.Color:=clHighlight;
      Stringgrid1.Canvas.Font.Color:=clHighlightText;
    end else
    begin
      Stringgrid1.Canvas.Brush.Color:=Stringgrid1.Color;
    end;
    Stringgrid1.Canvas.fillrect(r);
    Stringgrid1.Canvas.TextRect(r2,r2.left,r2.top,FText);

  end;
end;

procedure TForm_DFMInspector.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var c:integer;
begin
  stringgrid1.MouseToCell(x,y,c,selrow);
end;

procedure TForm_DFMInspector.StringGrid1SelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
var ti:PTypeInfo;
begin
  //updown1.Visible
  showUpdown:=false;
  canselect:=(row=selrow);
  if not canSelect then exit;
  if assigned(inPlaceEditor) then
  begin
    if inplaceeditor=checklistbox1 then
    begin
      stringgrid1.RowHeights[stringgrid1.row]:=stringgrid1.DefaultRowHeight;
    end;
    Combobox1.Items.clear;
    inPlaceEditor.Visible:=false;
    inplaceeditor:=nil;
  end;
  if Stringgrid1.Cells[1,row]='N/A' then CanSelect:=False;
  if not canSelect then exit;
  oldrow:=row;
  oldValue:=Stringgrid1.cells[1,oldrow];
  if assigned(Stringgrid1.Objects[0,row]) then
  begin
    case TCellOptions(Stringgrid1.Objects[0,row]).TypeObject.PropKind of
      tkEnumeration:
      begin
        inPlaceEditor:=combobox1;
        GetEnumNames(TCellOptions(Stringgrid1.Objects[0,row]).TypeObject.CompType,Combobox1.Items);
        Combobox1.Text:=oldvalue;
      end;
      tkSet:
      begin
        inPlaceEditor:=checklistbox1;
        GetEnumNames(TCellOptions(Stringgrid1.Objects[0,row]).TypeObject.CompType,CheckListbox1.Items);
        SetInPlaceEditorValue(oldvalue);
        Stringgrid1.RowHeights[row]:=stringgrid1.DefaultRowHeight*3;
      end;
      tkClass,tkMethod:inPlaceEditor:=nil;
      else
      begin
        begin
          inPlaceEditor:=edit1;
          showUpdown:=(TCellOptions(Stringgrid1.Objects[0,row]).TypeObject.PropKind in [tkInteger]) and isnumeric(oldvalue);
          Edit1.Text:=oldvalue;
        end;
        ti:=TCellOptions(Stringgrid1.Objects[0,row]).TypeObject.CompType;
        if (ti = TypeInfo(TShortCut)) then
        begin
          inPlaceEditor:=Hotkey1;
          showUpdown:=false;
          if (Stringgrid1.cells[1,row]<>'') and (Stringgrid1.cells[1,row]<>'0') then
            Hotkey1.HotKey:=StrToInt(Stringgrid1.Cells[1,row])
          else
            Hotkey1.HotKey:=0;
        end else
        if (ti = TypeInfo(TColor)) then
        begin
          inPlaceEditor:=FWCombo;
          FWCombo.ComboType:=ctColor;
          FWCombo.Text:=Stringgrid1.Cells[1,row];
        end else
        if (ti = TypeInfo(TCursor)) then
        begin
          inPlaceEditor:=FWCombo;
          FWCombo.ComboType:=ctCursor;
          FWCombo.Text:=Stringgrid1.Cells[1,row];
        end else
        if (ti = TypeInfo(TFontCharset)) then
        begin
          inPlaceEditor:=Combobox1;
          getCharsetValues(Combobox1.Items.append);
          Combobox1.Text:=Stringgrid1.Cells[1,row];
        end else if (ti = TypeInfo(TDate)) or (ti = TypeInfo(TTime)) then
        begin
          inPlaceEditor:=DateTimePicker1;
          if ti = TypeInfo(TDate) then
          begin
            Datetimepicker1.Kind:=dtkDate;
            Datetimepicker1.Date:=StrToFloat(Stringgrid1.Cells[1,row]);
          end else if ti = TypeInfo(TTime) then
          begin
            Datetimepicker1.Kind:=dtkTime;
            Datetimepicker1.Time:=StrToFloat(Stringgrid1.Cells[1,row]);
          end;
        end;
      end;
    end;
  end;
  AlignInPlaceEditor(1,row);
end;

procedure TForm_DFMInspector.StringGrid1TopLeftChanged(Sender: TObject);
begin
  AlignInPlaceEditor(1,stringgrid1.row);
end;

//inPlaceEditor
procedure TForm_DFMInspector.Edit1Change(Sender: TObject);
begin
  if showUpdown then
    updown1.Position:=StrToInt(Edit1.Text);
end;

procedure TForm_DFMInspector.Edit1Exit(Sender: TObject);
begin
  if edit1.text='' then
  begin
    case TCellOptions(stringgrid1.Objects[0,Stringgrid1.row]).typeObject.PropKind of
      tkInteger,tkFloat:Edit1.text:='0';
    end;
  end;
  stringgrid1.cells[1,Stringgrid1.row]:=Edit1.Text;
  if not changeproperty and edit1.Visible then
    Edit1.SetFocus;
end;

procedure TForm_DFMInspector.UpDown1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  edit1.Text:=IntToStr(UpDown1.position);
end;

procedure TForm_DFMInspector.ComboBox1DblClick(Sender: TObject);
var p:integer;
begin
  p:=(sender as TCombobox).Items.indexOf((sender as TCombobox).Text);
  if p<(sender as TCombobox).Items.count-1 then inc(p) else p:=0;
  (sender as TCombobox).text:=(sender as TCombobox).Items.Strings[p];
end;

procedure TForm_DFMInspector.ComboBox1Exit(Sender: TObject);
begin
  stringgrid1.cells[1,Stringgrid1.row]:=(sender as TCombobox).Text;
  if not changeproperty and (sender as TCombobox).visible then
    (sender as TCombobox).SetFocus
end;

procedure TForm_DFMInspector.CheckListBox1Exit(Sender: TObject);
var i:integer;
    s:string;
begin
  s:='';
  for i:=0 to checklistbox1.Items.count-1 do
  begin
    if checklistbox1.Checked[i] then
    begin
      if s<>'' then s:=s+',';
      s:=s+checklistbox1.items[i];
    end;
  end;
  s:='['+s+']';
  stringgrid1.cells[1,Stringgrid1.row]:=s;
  changeproperty;
end;

procedure TForm_DFMInspector.DateTimePicker1Exit(Sender: TObject);
begin
  case DatetimePicker1.Kind of
    dtkDate:stringgrid1.cells[1,Stringgrid1.row]:=FloatToStr(DatetimePicker1.Date);
    dtkTime:stringgrid1.cells[1,Stringgrid1.row]:=FloatToStr(DatetimePicker1.Time);
  end;
  changeproperty;
end;

procedure TForm_DFMInspector.HotKey1Exit(Sender: TObject);
begin
  stringgrid1.cells[1,Stringgrid1.row]:=IntToStr(HotKey1.hotkey);
  changeproperty;
end;

end.
