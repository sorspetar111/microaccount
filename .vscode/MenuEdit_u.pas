unit MenuEdit_u;

{*
Program: DFMEdit
Unit: MenuEdit_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Menu-Editor

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids,menus;

const
  bgColor=clBtnFace;
  FontColor=clWindowText;
  selColor=clNavy;
  selFontColor=clWhite;

type
  TMenuEditor=class;
  TMenuEntry=class;
  TMoveEvent = procedure (Sender,Source: TObject;FromIndex,ToIndex:Integer) of object;
  TAddDelEvent = procedure (Sender: TObject;Index:Integer) of object;
  TChangeEvent = procedure (Sender: TObject;Index:Integer;Propname:string) of object;
(*
  TMenuPopup=class(TPopupMenu)
    public
      constructor create(AOwner:TComponent);override;
    private
      procedure AddEntry(Sender:TObject);
      procedure AddSubmenu(Sender:TObject);

  end; *)

  TMenuEntry=class(TComponent) //properties for each item
    private
      FSubmenu:TMenuEditor;
      FProperties:TStringlist;
      FMenu:TMenuEditor;
      FData:TObject;
      function GetWidth:integer;
      function GetIndex:Integer;
      function IsParentOf(pMenu:TMenuEditor):Boolean;
      procedure ShowSubMenu;
      //function PropertyCount:integer;
    public
      constructor Create(AOwner:TComponent);override;
      destructor Destroy;override;
      procedure DeleteProperty(Name:string);
      function GetPropertyName(index:integer):string;
      function GetPropertyValue(name:string):string;
      procedure SetPropertyValue(pName,pValue:string);
      
      property Data:TObject read FData write FData;
    published
      property Index:integer read GetIndex;
      property SubMenu:TMenuEditor read FSubMenu write FSubMenu;
      property Menu:TMenuEditor read FMenu;
  end;
  
  TMenuType=(mtPopup,mtMain);
  
  TMenuEditor=class(TListbox)
  private
    FOldpos:integer; //used for Drag&drop (painting line)
    FMenuVisible:TMenuEditor;
    FParentItem:TMenuEntry;
    FMenuType:TMenuType;
    FOnMove:TMoveEvent;
    FData:TObject;
    FOnAdd,FOnDelete:TAddDelEvent;
    FOnChange:TChangeEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize( var msg: TMessage ); message WM_NCCALCSIZE;
    procedure DragLine;
    function GetEntry(Index:integer):TMenuEntry;
    procedure setMenuType(Menutype:TMenuType);
  protected
    property items; //hide items in OI
    procedure Click; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DragDrop(Source: TObject; X, Y: Integer);  override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Realign;
    procedure SetParent(Parent:TWinControl); override;
    procedure SetAddEvent(e:TAddDelEvent);
    procedure SetDeleteEvent(e:TAddDelEvent);
    procedure SetChangeEvent(e:TChangeEvent);
  public
    constructor Create(AOwner:TComponent); override;
    destructor destroy; override;
    function AddEntry(caption:string):TMenuEntry;
    function AddSubMenu(Index:integer;ShowIt:Boolean):TMenuEditor;
    procedure Clear;
    function count:integer;
    procedure DeleteEntry(index:integer);
    function DeleteSubMenu(Index:integer):Boolean;
    function IndexOf(Entry:TMenuEntry):integer;
    procedure HideSubMenu;
    property Data:TObject read FData write FData;
    property Entries[index:integer]:TMenuEntry read GetEntry;
  published
    property MenuType:TMenuType read FMenuType write setMenuType;
    property OnMove:TMoveEvent read FOnMove write FOnMove;
    property OnChange:TChangeEvent read FOnChange write SetChangeEvent;
    property OnAdd:TAddDelEvent read FOnAdd write SetAddEvent;
    property OnDelete:TAddDelEvent read FOnDelete write SetDeleteEvent;
  end;


implementation

//help-functions
procedure Draw3dLine(Canvas:TCanvas;x1,y1,x2,y2:Integer);
const
  color1 = clBtnShadow;
  color2 = clBtnHighlight;
begin
  Canvas.Pen.Color:=color1;
  canvas.pen.Mode:=pmCopy;
  canvas.Pen.Width:=1;
  canvas.MoveTo(x1,y1);
  canvas.LineTo(x2,y2);
  Canvas.Pen.Color:=color2;
  canvas.MoveTo(x1,y1+1);
  canvas.LineTo(x2,y2+1);
end;

(*
constructor TMenuPopup.create(AOwner:TComponent);
begin
  with TMenuItem.create(self) do
  begin
    caption:='Add Entry';
    onClick:=AddEntry;
  end;
  with TMenuItem.create(self) do
  begin
    caption:='Add SubMenu';
    onClick:=AddSubmenu;
  end;
end;

procedure TMenuPopup.AddEntry(Sender:TObject);
begin

end;

procedure TMenuPopup.AddSubmenu(Sender:TObject);
begin

end;
*)
//******************************************  TMenuEntry  ************************************************
constructor TMenuEntry.Create(AOwner:TComponent);
begin
  FProperties:=TStringlist.create;
  FMenu:=AOwner as TMenuEditor;
end;

destructor TMenuEntry.Destroy;
begin
  FProperties.Free;
  if assigned(fSubmenu) then
  begin
    if FMenu.FMenuVisible=FSubMenu then
      FMenu.FMenuVisible:=nil;
    FSubMenu.OnDelete:=nil;
    FSubMenu.OnAdd:=nil;
    FSubMenu.OnChange:=nil;
    FSubMenu.Free;
  end;
  inherited;
end;

//private
function TMenuEntry.GetWidth:integer;
var sc:string;
begin
  result:=FMenu.Canvas.TextWidth(GetPropertyValue('Caption'))+10;
  if (FMenu.MenuType=mtPopup) then
  begin
    result:=result+30;
    sc:=GetPropertyValue('Shortcut');
    if (sc<>'') then
    begin
      sc:=ShortCutToText(StrToInt(sc));
      result:=result+FMenu.Canvas.TextWidth(sc)+20;
    end;
  end;
end;

function TMenuEntry.GetIndex:Integer;
begin
  result:=FMenu.IndexOf(self);
end;

function TMenuEntry.IsParentOf(pMenu:TMenuEditor):Boolean;
var entry:TMenuEntry;
begin
  result:=false;
  entry:=pMenu.FParentItem;
  while assigned(entry) and not result do
    if entry=self then
      result:=true
    else
      entry:=entry.FMenu.FParentItem;
end;

procedure TMenuEntry.ShowSubMenu;
begin
  FMenu.HideSubMenu;
  if assigned(FSubmenu) then
  begin
    FMenu.FMenuVisible:=FSubmenu;
    if FMenu.FMenuType=mtPopup then
    begin
      FSubmenu.Left:=FMenu.left+FMenu.width-2;
      FSubmenu.Top:=FMenu.top+FMenu.ItemRect(index).top;
    end else
    begin
      FSubmenu.Left:=(FMenu.width div (FMenu.Items.count+1))*index;
      FSubmenu.Top:=FMenu.top+FMenu.height-2;
    end;
    FSubmenu.ItemIndex:=-1;
    FSubmenu.Visible:=true;
  end;
end;
(*
function TMenuEntry.PropertyCount:integer;
begin
  result:=FProperties.count;
end;
*)
//public
procedure TMenuEntry.DeleteProperty(Name:string);
var i:integer;
begin
  i:=FProperties.Count-1;
  while i>=0 do
  begin
    if compareText(Fproperties.Names[i],name)=0 then
    begin
      FProperties.Delete(i);
      i:=0;
    end;
    dec(i);
  end;
end;

function TMenuEntry.GetPropertyName(index:integer):string;
begin
  if (index<0) or (index>=FProperties.Count) then
    result:=''
  else
    result:=FProperties.Names[index];
end;

function TMenuEntry.GetPropertyValue(name:string):string;
begin
  result:=FProperties.Values[name];
end;

procedure TMenuEntry.SetPropertyValue(pName,pValue:string);
var gi,i:integer;
    s:string;
begin
  if (ansiCompareText(pName,'default')=0) and (ansiCompareText(pValue,'true')=0) then
  begin
    for i:=0 to FMenu.count-1 do
    begin
      if i=Index then continue;
      if ansiCompareText(FMenu.Entries[i].getPropertyValue('default'),'true')=0 then
        FMenu.Entries[i].SetPropertyValue('Default','');
    end;
  end else
  if (ansiCompareText(pName,'checked')=0) and (ansiCompareText(pValue,'true')=0) and (ansiCompareText(getPropertyValue('RadioItem'),'true')=0) then
  begin
    s:=getPropertyValue('GroupIndex');
    if s='' then s:='0';
    gi:=StrToInt(s);
    for i:=0 to FMenu.count-1 do
    begin
      if i=Index then continue;
      s:=FMenu.Entries[i].getPropertyValue('GroupIndex');
      if s='' then s:='0';
      if StrToInt(s)=gi then FMenu.Entries[i].SetPropertyValue('Checked','');
    end;
  end;
  FProperties.Values[pName]:=pvalue;
  if assigned(FMenu.FOnChange) then
    FMenu.FOnChange(FMenu,FMenu.IndexOf(self),pName);
  FMenu.realign;
  FMenu.Invalidate;
end;

//****************************************  TMenuEditor  *******************************************************
constructor TMenuEditor.Create(AOwner:TComponent);
begin
  inherited;
  Style:=lbOwnerDrawFixed;
  ItemHeight:=20;
  Color:=bgColor;
  FOldpos:=-1;
  Menutype:=mtPopup;
end;

destructor TMenuEditor.destroy;
begin
  Clear;
  inherited;
end;

//private
procedure TMenuEditor.CMMouseLeave(var Message: TMessage);
begin
  //removing Drag-Line
  DragLine;
  FOldPos:=-1;
end;

procedure TMenuEditor.WMNCCalcSize(var msg: TMessage);
var
  style: Integer;
begin
  //hide scrollbars
  style := GetWindowLong( handle, GWL_STYLE );
  if (style and (WS_VSCROLL or WS_HSCROLL)) <> 0 then
    SetWindowLong( handle, GWL_STYLE, style and not WS_VSCROLL and not WS_HSCROLL );
  inherited;
end;

procedure TMenuEditor.DragLine;
begin
  if (FOldpos <> -1) then
  begin
    if Menutype=mtPopup then
    begin
      Canvas.MoveTo(0,fOldpos);
      Canvas.LineTo(ClientWidth,foldpos)
    end else
    begin
      Canvas.MoveTo(fOldpos,0);
      Canvas.LineTo(foldpos,ClientHeight)
    end;
  end;
end;

function TMenuEditor.GetEntry(Index:integer):TMenuEntry;
begin
  //if (index<0) or (index>=count) then exit;
  result:=items.Objects[index] as TMenuEntry;
end;

procedure TMenuEditor.setMenuType(Menutype:TMenuType);
begin
  FMenuType:=Menutype;
  height:=Itemheight+2;
end;

//protected
procedure TMenuEditor.Click;
begin
  inherited; 
  if assigned(Entries[itemindex]) then
    Entries[itemindex].ShowSubmenu;
end;

procedure TMenuEditor.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var cap,sc:string;
    enab:boolean;
begin
  canvas.brush.Color:=bgcolor;
  Canvas.Font.Color:=FontColor;
  if odSelected in State then
  begin
    canvas.brush.Color:=selColor;
  Canvas.Font.Color:=selFontColor;
  end;
  canvas.FillRect(rect);
  enab:=ansiCompareText(Entries[index].GetPropertyValue('Enabled'),'false')<>0;
  if not enab then
    canvas.Font.Color:=clBtnShadow
  else if odSelected in State then
    canvas.Font.Color:=clCaptionText
  else
    canvas.Font.Color:=clWindowText;

  canvas.Font.Style:=[];
  if ansiCompareText(Entries[index].GetPropertyValue('default'),'true')=0 then
    canvas.Font.Style:=[fsBold];

  cap:=Entries[index].GetPropertyValue('Caption');
  if cap='-' then
    Draw3dLine(Canvas,rect.left,rect.top+8,rect.right,rect.top+8)
  else
  begin
    rect.left:=rect.left+4;
    if MenuType=mtPopup then
    begin
      Rect.Left:=rect.left+10;

      Canvas.Font.Name:='Marlett';
      Canvas.Font.Size:=10;
      if assigned(Entries[index]) and
         assigned(Entries[index].Submenu) then
        canvas.TextOut(rect.right-15,rect.top+4,'4');
      if ansiCompareText(Entries[index].GetPropertyValue('checked'),'true')=0 then
      begin
        if ansiCompareText(Entries[index].GetPropertyValue('radioitem'),'true')=0 then
          canvas.TextOut(rect.Left-14,rect.top+4,'h')
        else
          canvas.TextOut(rect.Left-14,rect.top+2,'a');
      end;
      Canvas.Font.Name:='MS Sans Serif';
      Canvas.Font.Size:=8;
      sc:=Entries[index].GetPropertyValue('ShortCut');
      if sc<>'' then
      begin
        sc:=ShortCutToText(StrToInt(sc));
        rect.right:=rect.right-20;
        DrawText(Canvas.Handle, PChar(sc), Length(sc), Rect, DT_SINGLELINE or DT_VCENTER or DT_RIGHT);
      end;
    end;
    DrawText(Canvas.Handle, PChar(cap), Length(cap), Rect, DT_SINGLELINE or DT_VCENTER);
  end;
end;

procedure TMenuEditor.DragDrop(Source: TObject; X, Y: Integer);
var
  DropIndex,ii: integer;
begin
  DropIndex := ItemAtPos(point(X, Y), false);
  ii:=TListBox(Source).ItemIndex;
  if Dropindex>-1 then
  begin
    if Assigned(FOnMove) then FOnMove(self,source,ii,dropindex);
    Items.Insert(DropIndex, TMenuEditor(Source).Items[TMenuEditor(Source).ItemIndex]);
    Items.Objects[DropIndex]:=TMenuEditor(Source).Items.Objects[TMenuEditor(Source).ItemIndex];
    if Source<>Self then
      (Items.Objects[DropIndex] as TMenuEntry).FMenu:=self;
    TMenuEditor(Source).Items.Delete(TMenuEditor(Source).ItemIndex);
    if (source=self) and (ii<dropindex) then dropindex:=dropindex-1;
    foldpos:=-1;
    itemindex:=dropindex;
    if assigned(FMenuVisible) then Click;
    EndDrag(true);
  end else
  begin
    if (FOldpos <> -1) then
    begin 
      DragLine;
      fOldpos:=-1;
    end;
  end;
  if source<>self then
  begin
    self.realign;
    (Source as TMenuEditor).realign;
  end;
end;

procedure TMenuEditor.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var   spos,itemidx:integer;
      rect:Trect;
begin
  Accept := (Source is TMenuEditor) and not (Source as TMenuEditor).Entries[(Source as TMenuEditor).itemindex].isParentOf(Self);
(*
  Accept:=Source = Self;
  (*
  if (GetAsyncKeyState(VK_CONTROL) = 0) then
    DragCursor:=crDrag
  else
    DragCursor:=crMultiDrag;   *)
  itemidx:=ItemAtPos(point(X,Y),false);
  if (itemidx>items.count) then itemidx:=items.count;
  if itemidx>-1 then
  begin
    rect:=ItemRect(itemidx);
    if Menutype=mtPopup then
      sPos:=rect.top
    else
      sPos:=rect.Left;

    Canvas.Pen.Color:=clGray;
    Canvas.Pen.Mode:=pmXor;
    Canvas.Pen.width:=3;

    DragLine; //first hide old
    fOldpos:=sPos;
    DragLine; //draw new
    if (itemidx<items.count) and (Entries[itemidx].Submenu<>nil) and (Entries[itemidx].Submenu<>FMenuVisible) then
    begin
      Entries[itemidx].ShowSubmenu;
    end;
  end;
end;

procedure TMenuEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var idx:integer;
begin
  inherited;
  idx:=ItemAtPos(point(x,y),true);
  if idx>-1 then
    itemindex:=idx;
  if Button=mbLeft then
    BeginDrag(false);
end;

procedure TMenuEditor.Realign;
var i,mw,w:integer;
begin
  mw:=0;
  for i:=0 to items.count-1 do
  begin
    w:=entries[i].Getwidth;
    if w>mw then mw:=w;
  end;
  if Menutype=mtPopup then
  begin
    Width:=mw;
    columns:=0;
    Height:=ItemHeight*(Items.Count)+10;
  end else
  begin
    Width:=mw*(items.count+1);
    columns:=items.count+1;
  end;
end;

procedure TMenuEditor.SetParent(Parent:TWinControl);
begin
  inherited;
  Ctl3D:=false;   //must be set after parent is set
end;

procedure TMenuEditor.SetAddEvent(e:TAddDelEvent);
var i:integer;
begin
  FOnAdd:=e;
  for i:=0 to items.count-1 do
  begin
    if assigned(Entries[i].Submenu) then
      Entries[i].Submenu.OnAdd:=e;
  end;
end;

procedure TMenuEditor.SetDeleteEvent(e:TAddDelEvent);
var i:integer;
begin
  FOnDelete:=e;

  for i:=0 to items.count-1 do
  begin
    if assigned(Entries[i].Submenu) then
      Entries[i].Submenu.OnDelete:=e;
  end;
end;

procedure TMenuEditor.SetChangeEvent(e:TChangeEvent);
var i:integer;
begin
  FOnChange:=e;

  for i:=0 to items.count-1 do
  begin
    if assigned(Entries[i].Submenu) then
      Entries[i].Submenu.OnChange:=e;
  end;  
end;

//public
function TMenuEditor.AddEntry(caption:string):TMenuEntry;
begin
  result:=TMenuEntry.Create(self);
  Items.AddObject(caption,result);
  Result.SetPropertyValue('Caption',caption);
  realign;
  if assigned(FOnAdd) then
    FOnAdd(self,IndexOf(result));
end;

function TMenuEditor.AddSubMenu(Index:integer;ShowIt:Boolean):TMenuEditor;
begin
  result:=nil;
  if (index<0) or (index>=Items.count) then exit;
  if assigned(Entries[index].Submenu) then exit;
  result:=TMenuEditor.Create(self);
  result.Visible:=false;
  result.Parent:=parent;
  Entries[index].SubMenu:=result;
  result.FParentItem:=Entries[index];
  result.PopupMenu:=popupmenu;
  result.OnClick:=OnClick;
  result.OnMove:=OnMove;
  result.OnAdd:=OnAdd;
  result.OnDelete:=OnDelete;
  result.OnChange:=OnChange;
  refresh;
  //invalidate;
  if ShowIt then
  begin
    itemindex:=index;
    click;
  end;
end;

procedure TMenuEditor.Clear;
var i:integer;
begin
  for i:=items.count-1 downto 0 do
    deleteEntry(i);
end;

function TMenuEditor.Count:integer;
begin
  result:=items.count;
end;

procedure TMenuEditor.DeleteEntry(index:integer);
var mEntry:TMenuEntry;
begin
  if assigned(FOnDelete) then
    FOnDelete(self,index);
  mEntry:=(items.Objects[index] as TMenuEntry);
  mEntry.free;
  Items.Delete(index);
  if MenuType=mtPopup then
    Height:=ItemHeight*(Items.Count)+5;
  realign;
end;

function TMenuEditor.DeleteSubMenu(Index:integer):Boolean;
begin
  result:=false;
  if assigned(entries[index].Submenu) then
  begin
    HideSubMenu;
    entries[index].Submenu.Free;
    entries[index].Submenu:=nil;
    repaint;
    result:=true;
  end;
end;

function TMenuEditor.IndexOf(Entry:TMenuEntry):integer;
var i:integer;
    me:TMenuEntry;
begin
  result:=-1;
  if not assigned(Entry) then exit;
  for i:=0 to items.Count-1 do
  begin
    me:=GetEntry(i);
    if me=Entry then result:=i;
  end;
end;

procedure TMenuEditor.HideSubMenu;
begin
  if assigned(FMenuVisible) then
  begin
    FMenuVisible.HideSubMenu;
    FMenuVisible.Visible:=false;
    FMenuVisible:=nil;
  end;
end;

end.
