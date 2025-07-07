unit Classes_u;

{*
Program: DFMEdit
Unit: Classes_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Class/Const-Definition

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,typinfo,menus, ExtCtrls, Buttons, Mask, Grids, checklst,
  TeeProcs, TeEngine, Chart, ComCtrls, ToolWin, ExtDlgs, MPlayer, Spin,
  ColorGrd, Gauges,SizeControl,inifiles;

const
  Add_MinHeight=400;
  Add_MinWidth=300;
  ChooseRes_MinHeight=150;
  ChooseRes_MinWidth=200;
  Inspector_MinHeight=300;
  Inspector_MinWidth=200;
  Main_MinHeight=550;
  Main_MinWidth=450;
  Palette_MinHeight=110;
  Palette_MinWidth=400;

  tb_iw=24;
  tb_ih=24;

  SizeEditable:array[1..7,1..4] of boolean=(
  (true,true,false,false),
  (true,true,true,true),
  (true,true,true,true),
  (false,false,true,true),
  (true,true,true,true),
  (true,true,false,false),
  (false,false,true,true)
  );

  usg_save=0;
  usg_preview=1;

  iiObject=0;
  iiObjectSel=0;
  iiProperty=1;
  iiPropertySel=1;
  iiItem=2;
  iiItemSel=2;
  iiType=2;
  iiTypeSel=2;
  iiValue=3;
  iiValueSel=3;
  iiUnwanted=4;
  iiControl=5;
  iiReplace=6;
  iiHide=7;
  iiMenu=8;
  iiMenuPos=9;
  iiMenuImg=10;
  iiIgnore=11;
  iiReplaceProp=12;
  
  lng_prefix='dfm_';

  _ConfFile='dfmedit.ini';
  _CompFile='components.obj';
  _PropFile='properties.obj';
  _UnwantedFile='unwanted.obj';

  VersionFile='http://dfmedit.sourceforge.net/version.ini';
  page_stable='http://dfmedit.sourceforge.net';
  page_beta='http://dfmedit.sourceforge.net/beta';
  page_request='http://www.fw-web.de/scripts/support.php?app=dfmedit&version=%s&lang=%s';

  rsv:array [0..98] of string =(
    'and','downto','in','out','stringresource','array','else','inherited',
    'packed','then','as','end','initialization','procedure','threadvar','asm',
    'except','inline','program','to','begin','exports','interface','property',
    'try','case','file','is','raise','type','class','finalization','label',
    'record','unit','const','finally','library','repeat','until','constructor',
    'for','mod','resourcestring','uses','destructor','function','nil','set',
    'var','dispinterface','goto','not','shl','while','div','if','object','shr',
    'with','do','implementation','of','string','xor','or','absolute','dynamic',
    'name','public','safecall','abstract','export','near','published','stdcall',
    'assembler','external','nodefault','read','stored','automated','far',
    'override','readonly','virtual','cdecl','forward','pascal','register',
    'write','default','index','private','resident','writeonly','dispid',
    'message','protected');

  WM_RecreateForms=WM_USER+10;
  WM_DrawAlignGrid=WM_USER+11;

type
  TDFMType=(dtNew,dtFile,dtResource);
  TCopyAction=(caNone,caCopy,caCut);

  //Integer-List used e.g. for TStringgrid.RowHeights/ColWidths
  //same as dkStrings but without quotes;
  TDataKind=(dkBinary,dkStrings,dkCollection,dkIntegerList);
  TCheckIDResult=(cirValid,cirReserved,cirInvalidChar,cirWrongLength);
  TSetPropResult=(sprOK,sprPropNotExists,sprCompNotFound,sprInvalidValue,sprNoChangeAllowed,sprNotSupported,sprError);
  TCellControl=(ccNone,ccCombo,ccCheck,ccHotKey);

  TVersionInfo = record
    MainV,SubV,Release,Build:byte;
    FVersion,BetaDownload:string;
  end;

  TPackageRec = Record
    handle:integer;
    filename:string;
  end;

  TMenuArray=array[0..1] of TMenuItem;

  TFileHistory = class
    FHistory:TStringlist;
    FMax:integer;
    FMenus:TMenuArray;
    FClickEvent:TNotifyEvent;
  public
    constructor create;
    destructor destroy;override;
    procedure AddFile(filename:string);
    procedure Clear;
    function Count:integer;
    procedure Delete(index:integer); //if file doesnt exist
    function GetFilename(index:integer):string;
    //procedure SetMaxFiles(value:integer);
    procedure Load;
    procedure Save;
    procedure SetMenus(menus:TMenuArray;ClickEvent:TNotifyEvent);
    property Max:integer read FMax write FMax;
  end;

  TConfigRec = record
    {$IFNDEF NOPACKAGES}
    packages:array[0..9] of TPackageRec;
    {$ENDIF}
    GridSize,BackupCount:integer;
    lazres,lazunitext,language,ExtEditor,BackupDir:string;
    BinForm,ShowHidden,EnableDisabled,
    ShowToolbar,ShowMainMenu, DrawGrid,
    OpenCreatedPas,OpenModifiedPas,AskForOpen,
    ChangeFocus,IgnoreFormMove,SnapToGrid,
    UseDllForSaving:boolean;
    FormSizes:array[1..4,1..7] of integer;
    HistoryCount:Integer;
    FileHistory:TFileHistory;//TStringlist;
    DebugMode:boolean;
  end;

  TDataRec = class
    fKind:TDataKind;
    fValue:string;
  public
    property Kind:TDataKind read fKind write fKind;
    property Value:string read fValue write fValue;
  end;

  TCallbackEvent=procedure(percent:integer;Text:String) of object;
  TEnumTypeInfoFunc = function(AUserData: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
  TTypeObject = class
  private
    FPropKind:TTypeKind;
    FTypeName:String;
    FCompType: PTypeInfo;
  public
    procedure Assign(Src:TTypeObject);
    property PropKind:TTypeKind read FPropKind write FPropKind;
    property TypeName:String read FTypeName write FTypeName;
    property CompType: PTypeInfo read FCompType write FCompType;
  end;

  TCellOptions = class
  private
    FChangeEnabled:Boolean;
    FCellControl:TCellControl;
    FTypeObject:TTypeObject;
  public
    constructor create;
    destructor destroy;override;
    property ChangeEnabled:Boolean read FChangeEnabled write FChangeEnabled;
    property CellControl:TCellControl read FCellControl write FCellControl;
    property TypeObject:TTypeObject read FTypeObject;
  end;

  TUnknownControl = class (TCustomControl)
  private
    fTmpBmp:TBitmap;
    fText:string;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure SetText(value:string);
    procedure RebuildOffScreen;
  protected
  public
    procedure Paint;override;
    procedure Setbounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor create(AOwner:TComponent);override;
    destructor destroy;override;
  published
    property Text:string read ftext write SetText;
    property Align;
    property Visible;
    property Enabled;
  end;

  THackedWinCtrl=class(TWinControl)
  public
    property OnExit;
  end;
          (*
  THackedMenuItem=class(TMenuItem)
  public
    function GetParentComponent: TComponent; override;
  end; *)

  TFWComboType=(ctNone,ctColor,ctCursor,ctFont,ctCharset);
  TFWComboBox = class(TComboBox)
    private
      icon:TIcon;
      FItemheight,FComboHeight:integer;
      FType:TFWComboType;
      procedure setType(value:TFWComboType);
      procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure DrawItem(Index: Integer; Rect: TRect;
        State: TOwnerDrawState); override;
    public
      constructor create(AOwner: TComponent);override;
      destructor destroy;override;
    published
      property ComboType:TFWComboType read FType write SetType;
      property ComboHeight:integer read FComboHeight write FComboHeight;
  end;

var
  loading,Recreate:boolean;
  ConfFile,CompFile,PropFile,UnwantedFile:string;
  LoadedFile:String;
  dt:TDFMType;
  FileChanged:Boolean;
  cfg:TConfigRec;

implementation

//================================= TFileHistory ===============================
constructor TFileHistory.create;
begin
  FHistory:=TStringlist.create;
end;

destructor TFileHistory.destroy;
begin
  Save;
  FHistory.Free;
end;

procedure TFileHistory.AddFile(filename:string);
var i,p:integer;
    mi:TMenuItem;
begin
  p:=FHistory.IndexOf(filename);
  if p>=0 then
    FHistory.Move(p,0)
  else
    FHistory.Insert(0,filename);
  for i:=low(Fmenus) to High(Fmenus) do
  begin
    if assigned(FMenus[i]) then
    begin
      if p>=0 then
      begin
        MI:=FMenus[i].items[p];
        FMenus[i].Remove(mi);
        FMenus[i].Insert(0,MI);
      end else
      begin
        mi:=TMenuItem.create(FMenus[i]);
        mi.OnClick:=FClickEvent;
        mi.Caption:=filename;
        FMenus[i].insert(0,mi);
        if FMenus[i].Count>FMax then
        begin
          FMenus[i].delete(FMenus[i].Count-1);
        end;
      end;
    end;
  end;
  if FHistory.count>FMax then
    FHistory.Delete(FHistory.count-1);
end;

procedure TFileHistory.Clear;
var i:integer;
begin
  for i:=low(Fmenus) to High(Fmenus) do
  begin
    if assigned(FMenus[i]) then
    begin
      while FMenus[i].Count>0 do
        FMenus[i].Delete(0);
    end;
  end;
  FHistory.Clear;
end;

function TFileHistory.Count:integer;
begin
  result:=FHistory.Count;
end;

function TFileHistory.GetFilename(index:integer):string;
begin
  result:='';
  if (index>=0) and (index<FHistory.Count) then
    result:=FHistory.Strings[index];
end;

procedure TFileHistory.Delete(index:integer);
var i:integer;
begin
  if (index>=0) and (index<FHistory.count) then
  begin
    for i:=Low(FMenus) to High(FMenus) do
    begin
      if assigned(FMenus[i]) then
      begin
        FMenus[i].Delete(index);
      end;
    end;
    FHistory.Delete(index);
  end;
end;
(*
procedure TFileHistory.SetMaxFiles(value:integer);
begin
  FMax:=value;
  //evtl. delete to much items in menu
end;
*)

procedure TFileHistory.Load;
var ini:Tinifile;
    i:integer;
    s:string;
begin
  Clear;
  ini:=Tinifile.Create(Conffile);
  FMax:=ini.ReadInteger('History','Count',5);
  for i:=FMax-1 downto 0 do
  begin
    s:=ini.ReadString('History','File'+IntToStr(i),'');
    if s<>'' then
      AddFile(s);
  end;
  ini.free;
end;

procedure TFileHistory.Save;
var ini:Tinifile;
    i:integer;
begin
  ini:=Tinifile.Create(Conffile);
  ini.EraseSection('History');
  ini.WriteInteger('History','Count',FMax);
  for i:=0 to FHistory.Count-1 do
  begin
    ini.WriteString('History','File'+IntToStr(i),FHistory.Strings[i]);
  end;
  ini.free;
end;

procedure TFileHistory.SetMenus(menus:TMenuArray;ClickEvent:TNotifyEvent);
var i:integer;
begin
  for i:=low(menus) to High(menus) do
    FMenus[i]:=Menus[i];
  FClickEvent:=ClickEvent;
end;

constructor TCellOptions.create;
begin
  inherited;
  FChangeEnabled:=True;
  FCellControl:=ccNone;
  FTypeObject:=TTypeObject.create;
end;

destructor TCellOptions.destroy;
begin
  FTypeObject.free;
  inherited;
end;

//================================ TTypeObject =================================
procedure TTypeObject.Assign(Src:TTypeObject);
begin
  FPropKind:=Src.PropKind;
  FTypeName:=Src.TypeName;
  FCompType:=Src.CompType;
end;

//============================== TUnknownControl ===============================
constructor TUnknownControl.create(AOwner:TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  fTmpBmp:=TBitmap.create;
end;

destructor TUnknownControl.destroy;
begin
  fTmpBmp.free;
  inherited;
end;

procedure TUnknownControl.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  Msg.Result := 1;
end;

procedure TUnknownControl.RebuildOffScreen;
var w,h:integer;
begin
  fTmpBmp.width:=width;
  fTmpBmp.height:=height;
  fTmpBmp.Canvas.Brush.Style:=bsDiagCross;
  //if enabled then
  fTmpBmp.Canvas.Brush.color:=$5F5F5F;
  //else

  fTmpBmp.Canvas.FillRect(rect(0,0,width,height));
  fTmpBmp.canvas.Brush.Style:=bsSolid;
  if enabled then
    fTmpBmp.Canvas.Brush.Color:=clYellow
  else
    fTmpBmp.Canvas.Brush.Color:=$7f7f7f;
  w:=fTmpBmp.canvas.TextWidth(fText);
  h:=fTmpBmp.Canvas.TextHeight(fText);
  fTmpBmp.canvas.TextOut((width-w) div 2,(height-h) div 2,fText);
end;

procedure TUnknownControl.Setbounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  RebuildOffScreen;
end;

procedure TUnknownControl.SetText(value:string);
begin
  fText:=Value;
  RebuildOffScreen;
end;

procedure TUnknownControl.Paint;
begin
  bitblt(Canvas.handle,0,0,width,height,fTmpBmp.canvas.handle,0,0,srccopy);
end;

//============================== TFWComboBox ===================================
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure GetFontNames(Items:TStrings);
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(Items));
  ReleaseDC(0, DC);
end;

constructor TFWComboBox.create(AOwner: TComponent);
begin
  inherited;
  Style := csDropDown;
  FItemheight:=15;
  FComboHeight:=15;//size of the item in the combobox
  FType:=ctNone;
  icon:=TIcon.create;
  ShowHint:=true;
  if aOwner is TWinControl then
  begin
    parent:=(aOwner as TWinControl);
  end;
end;

destructor TFWComboBox.destroy;
begin
  icon.free;
  inherited;
end;

procedure TFWComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var c:integer;
begin
  canvas.brush.Style:=bsSolid;
  canvas.Fillrect(Rect);
  canvas.font.Name:='MS Sans Serif';
  case FType of
    ctColor:
    begin
      IdentToColor(items.Strings[index],c);
      canvas.Brush.Color:=c;
      Canvas.Rectangle(Rect.left+1,Rect.top+1,Rect.left+FItemheight,Rect.bottom-1);
    end;
    ctCursor:
    begin
      icon.Handle := Screen.Cursors[StringToCursor(items.strings[index])];
      canvas.Draw(rect.left,rect.top,icon);
    end;
    ctFont:
    begin
      canvas.Font.Name:=items.strings[index];
      Canvas.font.Size:=12
    end;
  end;
  Canvas.Brush.color:=clwhite;
  Canvas.Brush.Style:=bsClear;

  if FType in [ctNone,ctFont,ctCharSet] then
    Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + 1, Items[Index]) //show text without indentation
  else
    Canvas.TextRect(Rect, Rect.Left + FItemheight + 2, Rect.Top + 1, Items[Index]); //indent text
  Canvas.DrawFocusRect(Rect);
end;

procedure TFWComboBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  if Message.MeasureItemStruct^.ItemId>-1 then
    Message.MeasureItemStruct^.itemHeight := FItemheight //height of list-entries
  else
    Message.MeasureItemStruct^.itemHeight := FComboHeight; //height of item in CB itself
end;

procedure TFWComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or CBS_OWNERDRAWVARIABLE;
end;

procedure TFWComboBox.setType(value:TFWComboType);
begin
  if FType<>value then
  begin
    FType:=Value;
    Items.clear;
    Canvas.font.Size:=8;
    case value of
      ctColor:
      begin
        FItemHeight:=15;
        GetColorValues(items.append);
      end;
      ctCursor:
      begin
        FItemHeight:=32;
        GetCursorValues(items.append);
      end;
      ctFont:
      begin
        FItemheight:=21;
        GetFontNames(Items);
      end;
      ctCharSet:
      begin
        FItemHeight:=15;
        GetCharsetValues(items.append);
      end;
    end;
  end;
end;

initialization

RegisterClasses([
  TUnknownControl,TForm,TFont,TGraphic,TPicture,TIcon,TBitmap,TTabSheet,TControlScrollbar, TToolbutton,
  TMainMenu,TPopupmenu,TMenuItem,TLabel,TEdit,TMemo,TButton,TCheckbox,TRadioButton,TListbox,TCombobox,TScrollbar,TGroupBox,TRadioGroup,TPanel,
  TBitBtn,TSpeedButton,TMaskEdit,TStringGrid,TDrawGrid,TImage,TShape,TBevel,TScrollBox,TCheckListBox,TSplitter,TStaticText,TChart,
  TTabControl,TPageControl,TImageList,TRichEdit,TTrackBar,TProgressBar,TUpDown,THotKey,TAnimate,TDateTimePicker,TTreeView,TListView,THeaderControl,TStatusBar,TToolBar,TCoolBar,
  TTimer,TPaintBox,TMediaPlayer,
  TOpenDialog,TSaveDialog,TOpenPictureDialog,TSavePictureDialog,TFontDialog,TColorDialog,TPrintDialog,TPrinterSetupDialog,TFindDialog,TReplaceDialog,
  TGauge,TColorGrid,TSpinButton,TSpinedit
])
end.
