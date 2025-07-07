unit addproperty_u;

{*
Program: DFMEdit
Unit: addproperty_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Form/Functions for adding/editing controls

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,ComCtrls,typinfo, checklst, Spin, ExtCtrls,menus,

  Classes_u, Buttons{$IFDEF DELPHI4_UP}, ImgList{$ENDIF}, ExtDlgs ;

type
  TEditAction = (eaEditProperty,eaAddProperty,eaEditObject,eaAddObject);
  TObjectAction = eaEditObject..eaAddObject;
  TPropertyAction = eaEditProperty..eaAddProperty;

  TForm_DFMAdd = class(TForm)
    Pnl_Top: TPanel;
      Static_oName: TStaticText;
      Static_oType: TStaticText;
    Pnl_CB: TPanel;
      Lbl_Name: TLabel;
      CB_Name: TComboBox;
      CB_SubClass: TComboBox;
      Pnl_Special: TPanel;
        Lbl_DataType: TLabel;
        CB_SpecialType: TComboBox;
      Pnl_Count: TPanel;
        Lbl_Count: TLabel;
        Chk_ModifyCaption: TCheckBox;
        Spin_Count: TEdit;
        UD_Count: TUpDown;
        Chk_Overlap: TCheckBox;
    Pnl_Left: TPanel;
      Lbl_Value: TLabel;
      Lbl_ObjectType: TLabel;
    Pnl_Client: TPanel;
      Pnl_Value: TPanel;
        CB_Value: TComboBox;
        UD_Value: TUpDown;
        SB_Multiline:TSpeedButton;
        SB_ColorChooser: TSpeedButton;
        DateTimePicker1: TDateTimePicker;
        HotKey1: THotKey;
      PrevPanel: TPanel;
        Image1: TImage;
      Memo_Special: TMemo;
      CheckListBox1: TCheckListBox;
      PageControl1: TPageControl;
        Tab_DFMData: TTabSheet;
          Memo_DFMData: TMemo;
        Tab_ImagePreview: TTabSheet;
          ScrollBox1: TScrollBox;
            Image2: TImage;
          PageControl2: TPageControl;
            Tab_SingleImage: TTabSheet;
              SB_OpenImage: TSpeedButton;
              SB_SaveImage: TSpeedButton;
              Chk_PictureHeader: TCheckBox;
              SB_AssignImage: TSpeedButton;
            Tab_ImageList: TTabSheet;
              SB_AddImage: TSpeedButton;
              SB_DelImage: TSpeedButton;
              SB_ClearImagelist: TSpeedButton;
              SB_SaveImage2: TSpeedButton;
              Label1: TLabel;
              Spin_delete: TEdit;
              UD_Delete: TUpDown;
              Spin_MoveFrom: TEdit;
              UD_MoveFrom: TUpDown;
              Spin_MoveTo: TEdit;
              UD_MoveTo: TUpDown;
              SB_MoveImage: TSpeedButton;
              SB_SwapImages: TSpeedButton;
              SB_AssignImageList: TSpeedButton;
    Pnl_Bottom: TPanel;
      Btn_OK: TButton;
      Btn_Cancel: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    ColorDialog1: TColorDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure CB_NameChange(Sender: TObject);
    procedure CB_NameExit(Sender: TObject);
    procedure CB_NameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CB_NameKeyPress(Sender: TObject; var Key: Char);

    procedure CB_SpecialTypeChange(Sender: TObject);
    procedure CB_ValueChange(Sender: TObject);
    procedure CB_ValueExit(Sender: TObject);
    procedure SB_MultilineClick(Sender: TObject);
    procedure SB_ColorChooserClick(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Memo_SpecialChange(Sender: TObject);
    procedure HotKey1Exit(Sender: TObject);
    procedure DateTimePicker1Change(Sender: TObject);

    procedure SB_OpenImageClick(Sender: TObject);
    procedure SB_SaveImageClick(Sender: TObject);
    procedure SB_AssignImageClick(Sender: TObject);

    procedure SB_AddImageClick(Sender: TObject);
    procedure SB_DelImageClick(Sender: TObject);
    procedure SB_ClearImagelistClick(Sender: TObject);
    procedure SB_MoveImageClick(Sender: TObject);
    procedure SB_SwapImagesClick(Sender: TObject);
    procedure SB_AssignImageListClick(Sender: TObject);
    procedure UDChanging(Sender: TObject; var AllowChange: Boolean);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure Btn_OKClick(Sender: TObject);
    procedure Btn_CancelClick(Sender: TObject);
  private
    { Private-Deklarationen }
    CurrentAction:TEditAction;
    CurrentNode,ObjectNode:TTreenode;
    function getCurrentType:TTypeKind;
    procedure DrawImagelist;
    procedure ResetFields;
    procedure UpdatePreview(PropName:string);
    procedure updateValueField;
    procedure UpdateEditors;
    procedure WMGetMinMaxInfo(Var Msg: TMessage); Message WM_GETMINMAXINFO;
  public
    { Public-Deklarationen }
    procedure EditDFM(Action:TEditAction;BaseNode:TTreenode);
  end;

var
  Form_DFMAdd: TForm_DFMAdd;

implementation

uses main_u,dfmparse_u,localize_u, functions_u, rttifunctions_u, preview_u,
  inspector_u;

{$R *.DFM}
//private
function TForm_DFMAdd.getCurrentType:TTypeKind;
var cb:TCombobox;
begin
  if (CB_SubClass.Visible) and (CB_SubClass.Text<>'') then
  begin
    cb:=CB_Subclass;
  end else
  begin
    cb:=cb_Name;
  end;
  if (CB.Items.IndexOf(CB.Text)>-1) then
    result:=TTypeObject(CB.items.objects[CB.Items.IndexOf(CB.Text)]).PropKind
  else result:=tkUnknown;
end;

procedure TForm_DFMAdd.DrawImagelist;
var bmp:TBitmap;
    i:integer;
begin
  bmp:=TBitmap.Create;
  bmp.Height:=imagelist1.height;
  bmp.Width:=imagelist1.width*imagelist1.Count;
  for i:=0 to imagelist1.Count-1 do
  begin
    imagelist1.Draw(bmp.canvas,i*imagelist1.width,0,i);
  end;
  image2.picture.Assign(bmp);
  Image2.Transparent:=true;
  bmp.free;
end;

procedure TForm_DFMAdd.ResetFields;
begin
  CB_Name.Text:='';
  FreeSLObjects(CB_Name.Items);
  CB_Name.Items.clear;
  CB_Value.Text:='';
  CB_Value.Items.clear;
  CB_SubClass.Text:='';
  FreeSLObjects(CB_Subclass.Items);
  CB_SubClass.Items.clear;
  CB_SubClass.Visible:=false;
  CB_SpecialType.ItemIndex:=0;

  PrevPanel.Caption:='';
  PrevPanel.Visible:=False;

  Memo_Special.Text:='';
  Memo_Special.Visible:=False;
  Memo_Special.ReadOnly:=false;
  Memo_DFMData.Text:='';
  SB_Multiline.Down:=false;
  Checklistbox1.Visible:=False;
  PageControl1.Visible:=false;
  ud_value.Visible:=false;
  sb_multiline.visible:=false;
  SB_ColorChooser.Visible:=false;
  DateTimePicker1.Visible:=false;
  Hotkey1.Visible:=false;
  image1.Picture.Assign(nil);
  image2.Picture.Assign(nil);
  CB_Value.width:=CB_Name.width;
  UD_Count.Position:=1;
end;

procedure TForm_DFMAdd.UpdatePreview(PropName:string);
begin
  if CB_SpecialType.Itemindex=0 then
  begin
    //Stringgrid1.cells[1,Stringgrid1.Row]:=RemoveSingleQuotes(GetPropertyValue(tn_prop));
    Form_DFMInspector.UpdateControls(PropName,cb_Value.Text);//Stringgrid1.cells[0,Stringgrid1.Row],Stringgrid1.cells[1,Stringgrid1.Row]);
  end else
  begin
    //set Specialdata
    case CB_SpecialType.Itemindex of
      1:
      begin
        if pagecontrol1.ActivePage=Tab_ImagePreview then
        begin
          if pagecontrol2.ActivePage=Tab_SingleImage then
          begin
            if Chk_PictureHeader.checked then
              Form_DFMInspector.UpdateControls(CB_Name.Text,IntToStr(integer(image2.Picture)))
            else
            begin
              if image2.Picture.graphic.ClassName='TBitmap' then
                Form_DFMInspector.UpdateControls(CB_Name.Text,IntToStr(integer(image2.Picture.bitmap)))
              else if image2.Picture.graphic.ClassName='TIcon' then
                Form_DFMInspector.UpdateControls(CB_Name.Text,IntToStr(integer(image2.Picture.Icon)));
            end;
          end;
        end;
      end;
      2: Form_DFMInspector.UpdateControls(PropName,IntToStr(integer(memo_special.Lines)));
    end;
  end;     
end;

procedure TForm_DFMAdd.updateValueField;
begin
  if ud_value.visible or sb_multiline.visible or SB_ColorChooser.Visible then
    CB_Value.width:=CB_Name.width-20
  else
    CB_Value.width:=CB_Name.width;
end;

procedure TForm_DFMAdd.UpdateEditors;
var d:extended;
    ds:char;
begin
  if hotkey1.Visible then
  begin
    hotkey1.HotKey:=StrToInt(cb_Value.text);
  end else if DateTimePicker1.visible then
  begin
    ds:=decimalSeparator;
    decimalseparator:='.';
    d:=StrToFloat(CB_Value.Text);
    decimalSeparator:=ds;
    case DateTimePicker1.kind of
      dtkDate: Datetimepicker1.date:=d;
      dtkTime: Datetimepicker1.time:=d;
    end;
  end;
end;

Procedure TForm_DFMAdd.WMGetMinMaxInfo(Var Msg: TMessage);
Begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize:= Point(Add_MinWidth, Add_MinHeight);
End;

//public
procedure TForm_DFMAdd.EditDFM(Action:TEditAction;BaseNode:TTreenode);
var p,idx:integer;
    oName,oClass,pName,pSubName,pValue:string;
    propnode:TTreenode;
begin
  // bei action editProperty wird als Basenode die property-node übergeben, sonst object-node
  // hier auf existenz des neuen Properties prüfen;
  ResetFields;
  Pnl_Count.Visible:=Action = eaAddObject;
  if assigned(basenode) then
  begin
    CurrentAction:=Action;
    CurrentNode:=BaseNode;
    if CurrentAction in [eaAddObject,eaEditObject] then
      Memo_Special.ReadOnly:=true;
    if isObject(basenode) then
    begin
      ObjectNode:=basenode;
      oName:=GetObjectName(basenode);
      oClass:=GetObjectClass(basenode);
      //change class to TForm if node is root-node (for Properties)
      if (ObjectNode=Form_DFMMain.Treeview1.Items.GetFirstNode) and (action<>eaEditObject)  then
        oClass:='TForm';
      if action=eaEditObject then
      begin
        CB_Name.Text:=oName;
        CB_Value.Text:=oClass;
      end else if action=eaAddProperty then
      begin
        GetComponentProps(oClass,CB_Name.Items,false);
      end;
    end else
    begin
      if isitem(basenode)or (isItem(basenode.parent)) then
      begin
        objectnode:=getObjectFromNode(basenode);
        if isitem(basenode) then
          propnode:=basenode.parent
        else
          propnode:=basenode.parent.parent;
        pname:=GetPropertyName(propnode);
        oName:= GetObjectName(ObjectNode);
        oclass:=getCollectionClass(getobjectClass(objectnode),pname);
      end;
      if isProp(basenode) then
        ObjectNode:=basenode.parent
      else
        ObjectNode:=basenode;
      if isItem(objectnode) then
      begin
        idx:=objectNode.Index;
        oName:=oName+'.'+pname+'['+IntToStr(idx)+']'
      end else
        oName:=GetObjectName(ObjectNode);
      if isObject(objectnode) then
        oClass:=GetObjectClass(ObjectNode);
      //change class to TForm if node is root-node (for Properties)
      if (ObjectNode=Form_DFMMain.Treeview1.Items.GetFirstNode)  then
        oClass:='TForm';
      if oClass<>'' then
        GetComponentProps(oClass,CB_Name.Items,false);
      if Action=eaEditProperty then
      begin
        pName:=GetPropertyName(basenode);
        pValue:=removeSingleQuotes(GetPropertyValue(basenode));
        p:=pos('.',pName);
        if p>0 then
        begin
          pSubName:=copy(pName,p+1,length(pName)-p);
          pName:=copy(pName,1,p-1);
        end;
        CB_Name.Text:=pName;
        cb_namechange(cb_name);
        CB_SubClass.Text:=pSubName;
        CB_Value.Text:=pValue;
        UpdateEditors;
        //aus form_show
        if not (CurrentAction in [eaAddObject,eaEditObject]) then
          Memo_Special.Visible:=CB_SpecialType.ItemIndex>0;
        if cb_Subclass.Text<>'' then
          CB_NameChange(cb_Subclass)
        else
          CB_NameChange(cb_name);

        if assigned(basenode.Data) then
        begin
          case TDataRec(basenode.Data).Kind of
            dkBinary:
            begin
              CB_SpecialType.ItemIndex:=1;
              Memo_DFMData.Text:=TDataRec(basenode.Data).Value;
              pageControl1.ActivePage:=Tab_ImagePreview;
              if (oClass='TImageList') and (pName='Bitmap') then
              begin
                AssignDFMToImageList(imagelist1,memo_DFMData.text);
                DrawImageList;
                pagecontrol2.ActivePage:=Tab_Imagelist;
              end else
              begin
                if AssignDFMToImage(image2,memo_DFMData.text) then
                  pagecontrol2.ActivePage:=Tab_SingleImage
                else
                  pageControl1.ActivePage:=Tab_DFMData;
              end;
              CB_SpecialTypeChange(self);
            end;
            dkStrings,dkIntegerList:
            begin
              CB_SpecialType.ItemIndex:=2;
              Memo_Special.Text:=TDataRec(basenode.Data).Value;
            end;
            dkCollection: CB_SpecialType.ItemIndex:=3;
          end;
        end;

      end;
    end;
    Static_oName.caption:=oName;
    Static_oType.caption:=oClass;
    case action of
      eaEditProperty,eaAddProperty:
      begin
        Lbl_ObjectType.Visible:=false;
        Lbl_Value.Visible:=true;
        CB_Value.Sorted:=False;
        if action=eaAddProperty then
          Caption:=localizeString('Cap_NewProperty')
        else
          Caption:=localizeString('Cap_EditProperty')
      end;
      eaEditObject,eaAddObject:
      begin
        Lbl_ObjectType.Visible:=true;
        Lbl_Value.Visible:=False;
        CB_Value.Sorted:=true;
        GetComponents(CB_Value.Items);
        if action=eaAddObject then
          Caption:=localizeString('Cap_NewObject')
        else
          Caption:=localizeString('Cap_EditObject')
      end;
    end;
    Pnl_Special.Visible:=(Lbl_Value.visible);
    self.ShowModal;
  end;
end;

//events
procedure TForm_DFMAdd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeSLObjects(cb_Name.Items);
  FreeSLObjects(cb_SubClass.Items);
  CurrentAction:=eaAddProperty;
  CurrentNode:=nil;
  ObjectNode:=nil;
end;

procedure TForm_DFMAdd.FormResize(Sender: TObject);
begin
  Static_oName.width:=(clientwidth div 2)-2;
  Static_oType.Width:=Static_oName.width;
  Static_oType.Left:=ClientWidth-Static_oType.Width;

  CB_Name.width:=clientwidth-92;
  CB_Name.width:=CB_Name.width;
  CB_SubClass.width:=CB_Name.width;
  CB_SpecialType.width:=CB_Name.width;
  Hotkey1.width:=CB_Name.width;
  DateTimePicker1.width:=CB_Name.width;
  CB_Value.width:=CB_Name.width;
  Pnl_Count.Width:=cb_name.width;
  Pnl_Special.width:=Pnl_CB.width;
  ud_value.Left:=CB_Name.width-ud_value.width-2;
  sb_multiline.Left:=CB_Name.width-sb_multiline.width-2;
  sb_colorchooser.Left:=CB_Name.width-sb_colorchooser.width-2;
  SB_AssignImage.Left:=PageControl2.ClientWidth-SB_AssignImage.width-8;
  SB_AssignImageList.Left:=PageControl2.ClientWidth-SB_AssignImageList.width-8;
  Btn_ok.width:=Static_oName.width;
  Btn_Cancel.Width:=Btn_ok.width;
  Btn_Cancel.Left:=ClientWidth-Btn_Cancel.width;
  updateValueField;
end;

procedure TForm_DFMAdd.FormShow(Sender: TObject);
begin
  if CB_Name.Items.Count>0 then
    CB_Name.Style:=csDropDown
  else
    CB_Name.Style:=csSimple;
  if CB_Value.Items.Count>0 then
    CB_Value.Style:=csDropDown
  else
    CB_Value.Style:=csSimple;

  if CurrentAction=eaAddObject then
    ActiveControl:=CB_Value
  else
    ActiveControl:=CB_Name;
end;

procedure TForm_DFMAdd.CB_NameChange(Sender: TObject);
var i,j:integer;
    cb:TCombobox;
    cl:TClass;
    ncc:boolean;
    typeO:TTypeObject;
    ti:pTypeInfo;
    s:string;
begin
  Autocomplete(sender as TCombobox);
  if Lbl_Value.visible then
  begin
    ncc:=false;
    PrevPanel.visible:=false;
    PrevPanel.Caption:='';
    cb:=Sender as TCombobox;

    if (CurrentAction=eaAddProperty)and (copy(cb.Text,1,2)='On') then
    begin
      s:=copy((sender as TCombobox).Text,3,length(cb.Text)-2);
      CB_Value.Text:=Static_oName.Caption+s;;
    end else if (CurrentAction<>eaEditProperty) then
    begin
      cb_Value.Text:='';
      cb_Value.Items.clear;
    end;

    if cb=cb_name then
    begin
      cb_SubClass.Text:='';
      FreeSLObjects(CB_Subclass.Items);
      cb_SubClass.Items.clear;
    end;
    i:=CB.Items.IndexOf(CB.Text);
    if (i>=0) and (assigned(CB.Items.objects[i])) then
    begin
      typeO:=TTypeObject(CB.Items.objects[i]);
      CB_Value.Hint:=TypeO.TypeName;

      PrevPanel.color:=clBtnFace;
      PrevPanel.caption:=TypeO.TypeName;
      CB_SpecialType.ItemIndex:=0;
      if (TypeO.TypeName='TStrings') or (lowercase(TypeO.TypeName)='<tstrings>') then
      begin
        CB_SpecialType.ItemIndex:=2;
        CB_SpecialTypeChange(self);
      end;
      if (lowercase(TypeO.TypeName)='<intlist>') then
      begin
        CB_SpecialType.ItemIndex:=4;
        CB_SpecialTypeChange(self);
      end;

      if (lowercase(TypeO.TypeName)='<tcollection>') then
      begin
        CB_SpecialType.ItemIndex:=3;
        CB_SpecialTypeChange(self);
      end;

      if TypeO.PropKind=tkClass then
      begin
        //auf TComponent testen
        cl:=getclassDef(TypeO.TypeName);
        if assigned(cl) then
        begin
          ncc:= not (cl.InheritsFrom(Tcomponent));
          if ncc then
          begin
            GetComponentProps(TypeO.TypeName,CB_SubClass.items,false);
            if cl.InheritsFrom(TGraphic) or (TypeO.TypeName='TPicture') or (lowercase(TypeO.TypeName)='<binary>') then
            begin
              CB_SubClass.Text:='Data';
              CB_SpecialType.ItemIndex:=1
            end else if (TypeO.TypeName='TStrings') then
            begin
              CB_SubClass.Text:='Strings';
              CB_SpecialType.ItemIndex:=2
            end;
          end;
        end;
      end;
      CB_SpecialTypeChange(CB_SpecialType);

      if (TTypeObject(CB.Items.objects[i]).PropKind=tkSet) and (assigned(TTypeObject(CB.Items.objects[i]).CompType)) then
      begin
        GetEnumNames(TTypeObject(CB.Items.objects[i]).CompType,CheckListBox1.Items);
        for j:=0 to checklistbox1.items.count-1 do
          if pos(checklistbox1.Items.strings[j],cb_value.Text)>0 then
            checklistbox1.Checked[j]:=true;
      end;

      ti:=nil;
      if TTypeObject(CB.Items.objects[i]).TypeName='TCursor' then
        getCursorValues(CB_Value.Items.append)
      else if TTypeObject(CB.Items.objects[i]).TypeName='TColor' then
        getColorValues(CB_Value.Items.append)
      else if TTypeObject(CB.Items.objects[i]).TypeName='TFontCharset' then
        getCharsetValues(CB_Value.Items.append)
      else if not DateTimePicker1.Visible and (TTypeObject(CB.Items.objects[i]).TypeName='TDate') then
      begin
        DateTimePicker1.Kind:=dtkDate;
        DateTimePicker1.Visible:=true;
        Datetimepicker1.Date:=date();
        DateTimePicker1Change(nil);
      end else if not DateTimePicker1.Visible and (TTypeObject(CB.Items.objects[i]).TypeName='TTime') then
      begin
        DateTimePicker1.Kind:=dtkTime;
        DateTimePicker1.Visible:=true;
        Datetimepicker1.Time:=time();
        DateTimePicker1Change(nil);
      end else if not Hotkey1.Visible and (TTypeObject(CB.Items.objects[i]).TypeName='TShortCut') then
      begin
        Hotkey1.Visible:=true;
        Hotkey1.HotKey:=0;
        HotKey1Exit(nil);
      end
      else
        ti:=findTypeInfo(TTypeObject(CB.Items.objects[i]).TypeName);

      if assigned(ti) then
      begin
        if ti^.Kind=tkEnumeration then
          GetEnumNames(ti,CB_Value.Items);
      end else
      begin
        if CB_Value.Items.count>0 then
        begin
          PrevPanel.Visible:=true;
          image1.Picture.Assign(nil);
          CB_Value.Items.Delete(0);
          if cb_Value.Text<>'' then
            cb_ValueChange(cb_Value);
        end;
      end;
      CB_SubClass.Visible:=(TTypeObject(CB_Name.Items.objects[CB_Name.Items.IndexOf(CB_Name.Text)]).PropKind=tkClass) and (ncc or (CB_SubClass.text<>''));
      CheckListBox1.visible:=(TTypeObject(CB.Items.objects[i]).PropKind=tkSet);
      ud_value.Visible:=(TTypeObject(CB.Items.objects[i]).PropKind = tkInteger) and (cb_Value.Items.Count=0);
      sb_multiline.Visible:=(TTypeObject(CB.Items.objects[i]).PropKind in [tkString,tkWString,tkLString]) and (cb_Value.Items.Count=0);
      cb_ValueChange(cb_Value);
    end else
    begin
      checklistbox1.Visible:=false;
      ud_value.Visible:=false;
    end;
    if not assigned(currentNode) then
      cb_value.Text:=GetPropertyValue(GetPropertyNode(form_dfmmain.treeview1.selected,cb_name.Text));
    if cb_Value.items.count>0 then
        cb_Value.Style:=csDropDown
    else
      cb_Value.Style:=csSimple;
  end;
  CB_SpecialTypeChange(self);
  updateValueField;
  s:=lowercase(copy(CB_Value.Hint,length(CB_Value.Hint)-4,5));
  Pnl_Special.Visible:=(not (lowercase(s)='event')) and ((CurrentAction = eaAddProperty) or (CurrentAction = eaEditProperty));
  if (CB_SubClass.text<>'') then
    cb_SubClass.Visible:=true;
  if CB_SubClass.Items.Count>0 then
  begin
    CB_SubClass.Style:=csDropDown;
  end else
    CB_SubClass.Style:=csSimple;
end;

procedure TForm_DFMAdd.CB_NameExit(Sender: TObject);
var s:string;
begin
  if CB_Name.text<>'' then
  begin
    s:=CB_Name.text;
    s:=Uppercase(s[1])+copy(s,2,length(s)-1);
    CB_Name.Text:=s;
  end;
end;

procedure TForm_DFMAdd.CB_NameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //TCombobox with style csSimple ignores Tbutton.Default/Cancel
  if (sender as TCombobox).style=csSimple then
  begin
    case key of
      vk_Return: Btn_OkClick(Btn_Ok);
      vk_Escape: Btn_CancelClick(Btn_Cancel);
    end;
  end;
end;

procedure TForm_DFMAdd.CB_NameKeyPress(Sender: TObject; var Key: Char);
var
  oldlen: Integer;
begin
  if key = #8 then
  begin
    with sender as tcombobox do
    begin
      oldlen := sellength;
      if selstart > 0 then
      begin
        selstart := selstart - 1;
        sellength := oldlen + 1;
      end;
    end;
  end;
end;

procedure TForm_DFMAdd.CB_SpecialTypeChange(Sender: TObject);
begin
  Memo_Special.visible:=(CB_SpecialType.Itemindex=2) or (CB_SpecialType.Itemindex=4) or (Memo_Special.Text<>'');
  PageControl1.Visible:=(CB_SpecialType.Itemindex=1);
  if (CB_SpecialType.Itemindex=1) then
  begin
    cb_Value.text:='{}';
    if Static_oType.Caption<>'' then
    begin
      if (lowercase(Static_oType.Caption)='timagelist') then
        PageControl2.ActivePage:=Tab_ImageList
      else
        PageControl2.ActivePage:=Tab_SingleImage;
    end;
  end;
  Chk_PictureHeader.checked:=(cb_Value.Hint='TPicture') or (copy(cb_value.text,1,2)='{T');
  PrevPanel.visible:=(CB_SpecialType.Text='') and ((PrevPanel.Caption='TColor') or  (PrevPanel.Caption='TCursor'));
  SB_ColorChooser.Visible:=(CB_SpecialType.Text='') and (PrevPanel.Caption='TColor');
  if CurrentAction<>eaEditObject then
    CB_Value.Enabled:=(CB_SpecialType.Text='');
  FormResize(self);
end;

procedure TForm_DFMAdd.CB_ValueChange(Sender: TObject);
var
  theCursor: TIcon;
begin
  //Preview
  AutoComplete(CB_Value);
  if (CurrentAction in [eaAddObject,eaEditObject]) then
    memo_special.Text:=GetCompHierarchy(CB_Value.Text);
  memo_special.Visible:=memo_special.Text<>'';
  if ud_value.Visible and isnumeric(cb_value.text) then
    ud_value.Position:=strToInt(cb_value.text);
  if (CurrentAction=eaAddObject) then
  begin
    if cb_Value.text<>'' then
    begin
      CB_Name.Text:=GetNameFromClass(CB_Value.Text);
    end else CB_Name.Text:='';
  end;
  if PrevPanel.Caption='TColor' then
  begin
    if CB_Value.Items.IndexOf(CB_Value.text)>0 then
      PrevPanel.color:= stringtocolor(CB_Value.text)
    else if ((length(CB_Value.text)=7) and isFirstChar(PChar(CB_Value.text),'$')) or
      isNumeric(CB_Value.text) then
    begin
      if isNumeric(CB_Value.text) then
        PrevPanel.Color:=strToInt(CB_Value.text);
    end;
  end else if PrevPanel.Caption='TCursor' then
  begin
    if CB_Value.Items.IndexOf(CB_Value.text)>-1 then
    begin
      theCursor := TIcon.Create;
      try
        theCursor.Handle := Screen.Cursors[StringToCursor(CB_Value.text)];
        image1.Canvas.Brush.Color:=PrevPanel.Color;
        image1.canvas.fillrect(rect(0,0,32,32));
        image1.canvas.Draw(0,0,theCursor);
      finally
        theCursor.Free;
      end;
    end;
  end;
end;

procedure TForm_DFMAdd.CB_ValueExit(Sender: TObject);
var   s:string;
begin
  //Groß/Kleinschreibung
  s:=CB_Value.Text;
  if Lbl_ObjectType.visible then
  begin
    s:=Uppercase(copy(s,1,2))+copy(s,3,length(s)-2);
    CB_Value.Text:=s;
  end;
end;

procedure TForm_DFMAdd.SB_MultilineClick(Sender: TObject);
begin
  Memo_Special.Visible:=SB_Multiline.down;
  if SB_Multiline.down then
    Memo_Special.lines.Text:=cb_Value.Text;
end;

procedure TForm_DFMAdd.SB_ColorChooserClick(Sender: TObject);
var i:longint;
begin
  i:=0;
  if isNumeric(CB_Value.text) then
    i:=StrToInt(CB_Value.text);
  if CB_Value.Items.IndexOf(CB_Value.text)>-1 then
    IdentToColor(CB_Value.text,i);
  colordialog1.Color:=i;
  if colordialog1.Execute then
  begin
    cb_Value.Text:=ColorToString(colordialog1.Color);
    cb_Value.SetFocus;
    cb_ValueChange(cb_Value);
  end;
end;

procedure TForm_DFMAdd.CheckListBox1ClickCheck(Sender: TObject);
var i:integer;
    s:string;
begin
  s:='';
  for i:=0 to Checklistbox1.Items.Count-1 do
  begin
    if checklistbox1.Checked[i] then
    begin
      if s<>'' then s:=s+',';
      s:=s+Checklistbox1.Items.Strings[i];
    end;
  end;
  CB_Value.Text:='['+s+']';
end;

procedure TForm_DFMAdd.Memo_SpecialChange(Sender: TObject);
begin
  if (CB_SpecialType.Itemindex=0) and not (CurrentAction in [eaAddObject,eaEditObject]) then
    CB_Value.Text:=Memo_Special.Text;
end;

procedure TForm_DFMAdd.HotKey1Exit(Sender: TObject);
begin
  cb_Value.Text:=IntToStr(Hotkey1.hotkey);
end;

procedure TForm_DFMAdd.DateTimePicker1Change(Sender: TObject);
var ds:char;
begin
  ds:=decimalSeparator;
  decimalseparator:='.';
  case DateTimePicker1.kind of
    dtkDate:cb_Value.Text:=FloatToStr(DateTimePicker1.Date);
    dtkTime:cb_Value.Text:=FloatToStr(DateTimePicker1.Time);
  end;
  decimalSeparator:=ds;
end;

//################################# buttons #################################
procedure TForm_DFMAdd.SB_OpenImageClick(Sender: TObject);
begin
  if (image1.Picture.ClassName='TIcon') or (CB_Value.Hint='TIcon') then
    openpicturedialog1.filter:='Icons (*.ico)|*.ico'
  else
    openpicturedialog1.filter:='Bitmaps (*.bmp)|*.bmp';
  openpicturedialog1.Options:=openpicturedialog1.Options - [ofAllowMultiSelect];
  if openPictureDialog1.Execute then
  begin
    Image2.Picture.LoadFromFile(openPictureDialog1.filename);
  end;
end;

procedure TForm_DFMAdd.SB_SaveImageClick(Sender: TObject);
var bmp:TBitmap;
begin
  SaveDialog1.Filter:='Bitmaps|*.bmp';
  SaveDialog1.DefaultExt:='.bmp';
  if (Savedialog1.execute)then
  begin
    if PageControl2.ActivePage=Tab_SingleImage then
      Image2.Picture.Graphic.SaveToFile(savedialog1.filename)
    else
    begin
      bmp:=TBitmap.Create;
      imagelist1.GetBitmap(StrToInt(Spin_Delete.text),bmp);
      bmp.SaveToFile(savedialog1.filename);
      bmp.free;
    end;
  end;
end;

procedure TForm_DFMAdd.SB_AssignImageClick(Sender: TObject);
begin
  if not Image2.Picture.Graphic.Empty then
  begin
    Memo_DFMData.Text:=GetImageDFMData(Image2,Chk_PictureHeader.checked);
    if Chk_PictureHeader.checked then
      cb_Value.Text:='{'+Image2.Picture.Graphic.ClassName+'}'
    else
      cb_Value.Text:='{}';
  end;
end;

procedure TForm_DFMAdd.SB_AddImageClick(Sender: TObject);
var bmp:TBitmap;
    i:integer;
begin
  openpicturedialog1.Options:=openpicturedialog1.Options + [ofAllowMultiSelect];
  if openpicturedialog1.execute then
  begin
    bmp:=TBitmap.create;
    for i:=0 to openpicturedialog1.Files.Count-1 do
    begin
      bmp.LoadFromFile(openpicturedialog1.files.Strings[i]);
      bmp.Width:=imagelist1.width;
      bmp.height:=imagelist1.height;
      imagelist1.AddMasked(bmp,bmp.canvas.pixels[0,0]);
    end;
    bmp.Free;
    DrawImagelist;
  end;
end;

procedure TForm_DFMAdd.SB_DelImageClick(Sender: TObject);
begin
  if (UD_Delete.Position>-1) and (UD_Delete.Position<imagelist1.count) then
    imagelist1.Delete(UD_Delete.Position);
  DrawImagelist;
end;

procedure TForm_DFMAdd.SB_ClearImagelistClick(Sender: TObject);
begin
  imagelist1.Clear;
  DrawImageList;
end;

procedure TForm_DFMAdd.SB_MoveImageClick(Sender: TObject);
begin
  if (UD_MoveFrom.Position>-1) and (UD_MoveFrom.Position<imagelist1.Count) and
     (UD_MoveTo.Position>-1) and (UD_MoveTo.Position<imagelist1.Count) and
     (UD_MoveFrom.Position <> UD_MoveTo.Position) then
    imagelist1.Move(UD_MoveFrom.Position,UD_MoveTo.Position);
  DrawImagelist;
end;

procedure TForm_DFMAdd.SB_SwapImagesClick(Sender: TObject);
begin
  if (UD_MoveFrom.Position>-1) and (UD_MoveFrom.Position<imagelist1.Count) and
     (UD_MoveTo.Position>-1) and (UD_MoveTo.Position<imagelist1.Count) and
     (UD_MoveFrom.Position <> UD_MoveTo.Position) then
  begin
    imagelist1.Move(UD_MoveFrom.Position,UD_MoveTo.Position);
    if abs(UD_MoveFrom.Position - UD_MoveTo.Position)>1 then
    begin
      if UD_MoveFrom.Position < UD_MoveTo.Position then
        imagelist1.Move(UD_MoveTo.Position-1,UD_MoveFrom.Position)
      else
        imagelist1.Move(UD_MoveTo.Position+1,UD_MoveFrom.Position)
    end;
  end;
  DrawImagelist;
end;

procedure TForm_DFMAdd.SB_AssignImageListClick(Sender: TObject);
begin
  if imagelist1.Count>0 then
  begin
    Memo_DFMData.Text:=GetImageListDFMData(Imagelist1);
    cb_Value.Text:='{}';
  end;
end;

procedure TForm_DFMAdd.UDChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if sender=UD_Delete then
    Spin_Delete.Text:=IntToStr(UD_Delete.Position)
  else if sender=UD_MoveFrom then
    Spin_MoveFrom.Text:=IntToStr(UD_MoveFrom.Position)
  else if sender=UD_MoveTo then
    Spin_MoveTo.Text:=IntToStr(UD_MoveTo.Position)
  else if sender=UD_Value then
    cb_Value.Text:=IntToStr(ud_value.position);
end;

procedure TForm_DFMAdd.Image2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p:integer;
begin
  if (Pagecontrol2.activepage=Tab_ImageList) and (activecontrol is TEdit) then
  begin
    p:=(x div imagelist1.Width);
    if activecontrol=Spin_Delete then UD_Delete.Position:=p else
    if activecontrol=Spin_MoveFrom then UD_MoveFrom.Position:=p else
    if activecontrol=Spin_MoveTo then UD_MoveTo.Position:=p;
  end;
end;

procedure TForm_DFMAdd.Btn_OKClick(Sender: TObject);
var propnode:TTreenode;
    PropName:string;
    TK:TTypekind;
    r:TDataRec;
    c:integer;
    hProp,tProp:integer;
begin
  //eigene Methode zum Hinzufügen/Ändern von Eigenschaften+Prüfung ob Object schon existiert
  if assigned(currentNode) and assigned(ObjectNode) then
  begin
    if (CurrentAction=eaEditProperty) or (CurrentAction=eaAddProperty) then
    begin
      TK:=getCurrentType;
      PropName:=CB_Name.text;
      if (tk=tkString) or (tk=tkLString) or (tk=tkWString) then CB_Value.text:=''''+CB_Value.text+'''';
      if CB_SubClass.Visible and (CB_SubClass.Text<>'') then PropName:=PropName+'.'+CB_SubClass.Text;

      if (PropName<>'') and (PropName[1]<>'.') and ((not CB_Value.enabled) or (CB_Value.Text<>'')) and (lowercase(copy(Propname,1,6))<>'object') then
      begin
        currentNode:=AddPropertyNodeTxt(Static_oName.caption,Propname,CB_Value.Text);    //<==== AV bei items da objectnode kein object
        //data:
        if (CB_SpecialType.Text<>'') then
        begin
          if not assigned(CurrentNode.data) then
          begin
            r:=TDatarec.Create;
            CurrentNode.data:=r;
          end else r:=TDataRec(CurrentNode.data);
          //zuweisen der data-werte
          case CB_SpecialType.Itemindex of
            1:// BinaryData
            begin
              CurrentNode.Text:=copy(CurrentNode.Text,1,pos('=',CurrentNode.Text))+' '+cb_value.Text;
              r.kind:=dkBinary;
              r.Value:=Memo_DFMData.Text;
            end;
            2://TStrings
            begin
              CurrentNode.Text:=PropName+' = (TStrings)';
              r.kind:=dkStrings;
              r.Value:=Memo_Special.Text;
            end;
            3: // TCollection
            begin
              CurrentNode.Text:=PropName+' = <>';
              r.kind:=dkCollection;
            end;
            4: //integerlist
            begin
              CurrentNode.Text:=PropName+' = (IntList)';
              r.kind:=dkIntegerList;
              r.Value:=Memo_Special.Text;
            end;
          end;
        end else if assigned(CurrentNode.data) then
        begin
          Tdatarec(CurrentNode.data).free;
          CurrentNode.data:=nil;
        end;
        //Control setzen, wenn Form_DFMPreview sichtbar und editcontrol<>nil
        if Form_DFMPreview.visible then UpdatePreview(Propname);
        close;
      end else Showmessage(localizeString('Msg_Error'));
    end else  //object-functions
    begin
      if (CheckID(CB_Name.Text)) then
      begin
        if (not ObjectExists(CB_Name.Text)) or ((CurrentAction=eaEditObject) and (ObjectNode=GetObjectNode(CB_Name.Text))) then
        begin
          if CurrentAction=eaAddObject then
          begin
            for c:=1 to UD_Count.Position do
            begin
              if UD_Count.Position>1 then
                cb_Name.text:=GetNextObjectNumber(CB_Name.Text);

              ObjectNode:=CreateObject((ObjectNode.Treeview as TTreeview),ObjectNode,CB_Name.Text,CB_Value.Text,Chk_ModifyCaption.checked);
              if UD_Count.Position=1 then
              begin
                ObjectNode.Selected:=true;
                ObjectNode.Expand(false);
              end else
              begin    //realigning for multiple controls
                propNode:=getPropNode(ObjectNode,'Height');  //only for controls
                if assigned(propNode) then
                begin
                  hprop:=StrToInt(GetPropertyValue(PropNode));
                  propNode:=getPropNode(ObjectNode,'Top');  //only for controls
                  if assigned(propNode) then
                  begin
                    tProp:=StrToInt(GetPropertyValue(PropNode));
                    if chk_Overlap.checked then
                    begin
                      PropNode.Text:=GetPropertyName(PropNode)+' = '+IntToStr(tProp+(c-1)*10);
                      propNode:=getPropNode(ObjectNode,'Left');
                      if assigned(propNode) then
                      begin
                        tProp:=StrToInt(GetPropertyValue(PropNode));
                        PropNode.Text:=GetPropertyName(PropNode)+' = '+IntToStr(tProp+(c-1)*10);
                      end;
                    end else
                      PropNode.Text:=GetPropertyName(PropNode)+' = '+IntToStr(tProp+(c-1)*hprop);
                  end;
                end;
                ObjectNode:=objectNode.Parent; //go back to base-node
              end;
            end;
          end else
          begin
            Form_DFMMain.CB_Objects.Items[
              Form_DFMMain.CB_Objects.Items.IndexOf(GetObjectName(ObjectNode)+': '+GetObjectClass(ObjectNode))]:= (CB_Name.text+': '+CB_Value.text);
            ObjectNode.Text:='object '+CB_Name.text+': '+CB_Value.text;
          end;
          close;
        end else showmessage(localizeString('Msg_ObjectExists'));
      end;
    end; //Object-Functions
    filechanged:=true;
  end;//assigned currentnode/ObjectNode
end;

procedure TForm_DFMAdd.Btn_CancelClick(Sender: TObject);
begin
  close;
end;

end.
