unit functions_u;

{*
Program: DFMEdit
Unit: functions_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
various Functions (parsing, treeview-handling)

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  forms,windows, messages, Classes, sysutils, stdctrls, ComCtrls, typinfo, Classes_u,
  dialogs,controls,inifiles, wininet,commctrl,graphics,FileCtrl,MenuEdit_u;

procedure StartPerfCounter(var time:integer);
function StopPerfCounter(time:integer):integer;
procedure FreeAndNil(var Obj);
procedure FreeSLObjects(list:Tstrings);
function CheckID(id:string):Boolean;
function IsNumeric(const Value: string): boolean;
function ResizeRect(r:TRect;value:integer):TRect;

//stringparsing
function isCharAtPos(s:pchar; c:char; p: integer):boolean;
function isFirstChar(s:pchar; c:char):boolean;
function isLastChar(s:pchar; c:char):boolean;
function isNextChar(s:pchar;c:char;p:integer;ignore:array of char):boolean;

function GetNumber(str:String):String;
function RemoveNumber(str:string):string;
function RemoveChar(s:string;c:char):string;
function removeColon(str:string):string;
function PosNext(subS,S: string; index:Integer): Integer;
function DuplicateQuotes(Str:String):string;
function RemoveSingleQuotes(ins:string):string;
function ExtractBaseName(Filename:String):String;
function GetFullPath(filename:string):string;
function GetRelPath(full:string):string;
function GetPropDelimiter(propPath:string):Integer;
function ReplaceAll(src,old,new:string):string;
function GetPropertyNameTxt(NodeCaption:string):string;
function GetPropertyValueTxt(NodeCaption:string):string;
function GetNameFromClass(ClassName:string):string;
function indentLine(line:string;count:integer):string;
procedure IndentLines(count:integer;lines:TStrings);
function FindInStringlist(Stringlist:TStrings;s:string):integer;

procedure RenameControl(oldName,NewName,newClass:string);
procedure ChangeObjectName(node:TTreenode;newName:string);

function getCurrentLine(memo:TMemo):integer;
function getCurrentCol(memo:TMemo):integer;
function ObjectExists(value:string):boolean;
function GetNextObjectNumber(objectname:string):string;
procedure Autocomplete(Combobox:TCombobox);

//treeview-functions
function IsItem(node:TTreenode):boolean;
function IsObject(node:TTreenode):boolean;
function IsProp(node:TTreenode):boolean;
function IsControl(cName:string;node:TTreenode):boolean;
function GetObjectNode(obj:string):TTreenode;
function GetObjectFromNode(node:TTreenode):TTreenode;
function GetPropertyFromNode(node:TTreenode):TTreenode;
function GetItemNode(prop:TTreenode;idx:integer):TTreenode;
function GetPropertyNode(obj:TTreenode;Prop:string):TTreenode;
function CreateObject(Treeview:TTreeview;parent:TTreenode;oName,oType:string;SetCaption:boolean):TTreenode;
function FindNode(startnode:TTreenode;Text:string):TTreenode;
function NodeFromCaption(aCaption:string):TTreeNode;
function AddPropertyNodeTxt(AObject,AProperty,AValue:String):TTreenode;
function AddPropertyNode(AObject:TTreenode;AProperty,AValue:String):TTreenode;
function GetNextObject(node:TTreenode):TTreenode;

function GetFormNode:TTreenode;
function GetFormName:string;
function GetFormClass:string;
function GetObjectName(Node:TTreenode):string;
function GetObjectClass(Node:TTreenode):string;

function GetPropertyName(Node:TTreenode):string;
function GetPropertyValue(Node:TTreenode):string;

function NodeCopy(Dest,Source: TTreeNode): TTreeNode;
function isChild(childnode,ParentNode:TTreenode):boolean;
procedure MoveNodeUp(node:TTreenode);
procedure MoveNodeDown(node:TTreenode);
procedure MoveNodeTop(node:TTreenode);
procedure MoveNodeBottom(node:TTreenode);

//options-dialog
function GetCompNode(name:string):TTreenode;
function GetPropNode(tn:TTreenode;Name:String):TTreenode;
function GetTypeNode(tn:TTreenode):TTreenode;
function GetValueNode(tn:TTreenode):TTreenode;
function GetRuleNode(tn:TTreenode):TTreenode;

function DownloadFile(url,filename: string) : Boolean ;
procedure CreateBackup(filename:string);

procedure getComponents(output:TStrings);
procedure getProperties(Component:string;Output:TStrings);
function GetReplacement(Component:string;ignore:TStringlist):string;
procedure GetReplacementProperties(Component:string;properties:TStringList);

procedure SetLanguage(lang:string);
{$IFNDEF NOPACKAGES}
procedure unloadPackages;
{$ENDIF}
procedure RecreateMainForm;
procedure RecreateForms;

function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer;
function MyIndexOfTabAt(PageControl: TPageControl; X, Y: Integer): Integer;

procedure Loadimages(imagelist:TImagelist;filename:string);
procedure CreateTab(Pagecontrol:TPagecontrol;Imagelist:TImageList;ScrollBarChange:TNotifyEvent);
procedure SortButtons(tb:TToolbar);
procedure CreateToolButton(tb:TToolbar;Position,imageindex:integer;Caption:string;ClickEvent:TNotifyEvent);

procedure buildMenu(Editor:TMenuEditor;node:TTreenode);
procedure LoadMenuFromTreeview(Editor:TMenuEditor;node:TTreenode);

implementation

uses
  main_u, addproperty_u, localize_u, preview_u, about_u, chooseRes_u, unit_u,
  inspector_u, palette_u, options_u,rttifunctions_u;

procedure StartPerfCounter(var time:integer);
begin
  time := Gettickcount;
end;

function StopPerfCounter(time:integer):integer;
begin
  result:=(Gettickcount - time);
end;

function isValidID(id:string):TCheckIDResult;
var i:integer;
begin
  result:=cirValid;
  if (length(id)<2) or (length(id)>63) then
    result:=cirWrongLength;
  i:=1;
  while (result=cirValid) and (i<= length(id)) do
  begin
    if not (id[i] in ['_','a'..'z','A'..'Z']) and not
    ((i>1) and (id[i] in ['0'..'9'])) then result:=cirInvalidChar;
    inc(i);
  end;
  i:=low(rsv);
  while (result=cirValid) and (i<=high(rsv)) do
  begin
    if rsv[i]=lowercase(id) then result:=cirReserved;
    inc(i);
  end;
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

procedure FreeSLObjects(list:TStrings);
var i:integer;
begin
  for i:=0 to list.Count-1 do
    if assigned(list.Objects[i]) then
    begin
      list.Objects[i].free;
      list.Objects[i]:=nil;
    end;
end;

function CheckID(id:string):Boolean;
begin
  result:=false;
  case isValidID(id) of
    cirValid: result:=true;
    cirReserved: messagebox(Form_DFMAdd.handle,PCHAR(format(LocalizeString('Msg_ReservedWord'),[id])),PCHAR(LocalizeString('Cap_Error')),MB_ICONERROR or mb_OK);
    cirInvalidChar: messagebox(Form_DFMAdd.handle,PCHAR(format(LocalizeString('Msg_InvalidChar'),[id])),PCHAR(LocalizeString('Cap_Error')),MB_ICONERROR or mb_OK);
    cirWrongLength: messagebox(Form_DFMAdd.handle,PCHAR(format(LocalizeString('Msg_WrongLength'),[id])),PCHAR(LocalizeString('Cap_Error')),MB_ICONERROR or mb_OK);
  end;
end;

{$HINTS OFF}
function IsNumeric(const Value: string): boolean;
var
  ErrorC : LongInt;
  Number : LongInt;
begin
  Val(Value, Number, ErrorC);
  Result := (ErrorC = 0);
end;
{$HINTS ON}

function ResizeRect(r:TRect;value:integer):TRect;
begin
  result.left:=r.left-value;
  result.top:=r.top-value;
  result.Right:=r.Right+value;
  result.Bottom:=r.Bottom+value;
end;

//stringparsing
function isCharAtPos(s:pchar; c:char; p: integer):boolean; // ungefährliche funktion, da bei ein leerer oder zu kurzer string durch IsBadReadPtr abgefangen wird
begin
  result := false;
  if (p = 0) or (s=nil) then exit;
  inc(s,p-1);
  if not IsBadReadPtr(s,1) then begin
    if s^ = c then result := true;
  end;
end;

function isFirstChar(s:pchar; c:char):boolean;
begin
  result := isCharAtPos(s,c,1);
end;

function isLastChar(s:pchar; c:char):boolean;
begin
  result := isCharAtPos(s,c,length(s));
end;

function isNextChar(s:pchar;c:char;p:integer;ignore:array of char):boolean;
var j:integer;
    gotChar:boolean;
begin
  gotChar:=false;
  while not gotchar do //find next char which is not in ignore-list
  begin
    inc(p);
    gotchar:=true;
    for j:=low(ignore) to high(ignore) do
      gotchar:=gotchar and not isCharAtPos(s,ignore[j],p);
  end;
  result := isCharAtPos(s,c,p);
end;

function GetNumber(str:String):string;
var //s:string;
    i:integer;
begin
  i:=length(str);
  while str[i] in ['0'..'9'] do
    dec(i);
  result:=copy(str,i+1,length(str)-i);
  if result='' then result:='1';
end;

function RemoveNumber(str:string):string;
begin
  result:=str;
  while result[length(result)] in ['0'..'9','.'] do
    delete(result,length(result),1);
end;

function RemoveChar(s:string;c:char):string;
var p:integer;
begin
  result:=s;
  (*
  for i:=length(result) downto 1 do
  begin
    if result[i]=chr then delete(result,i,1);
  end;
  *)
  p:=pos(c,result);
  while p>0 do
  begin
    delete(result,p,1);
    p:=pos(c,result);
  end;
end;

function removeColon(str:string):string;
var p:integer;
begin
  result:=str;
  p:=pos(':',result);
  while p>0 do
  begin
    delete(result,p,1);
    p:=pos(':',result);
  end;
end;

function PosNext(subS,S: string; index:Integer): Integer;
var str1:string;
begin
    str1:=Copy(S,index,Length(S)-(index-1));
    if Pos(subS,str1)=0 then
      result:=0
    else
      result:=Pos(subS,str1)+(index-1);
end;

function duplicateQuotes(Str:String):string;
begin
  result:=Str;
  if length(str)>2 then
  begin
    if (Str[1]='''') and (Str[2]<>'''')  then
      result:=''''+Str+'''';
  end;
end;

function RemoveSingleQuotes(ins:string):string;
var s:string;
//    i:integer;
begin
  //s:=trim(ins);
  s:=ins;
  if copy(s,length(s),1)='+' then
  begin
    s:=trim(copy(s,1,length(s)-1));
  end;
  (*
  for i:=length(s) downto 1 do
  begin
    if (s[i]='''') then
    begin
      if ((i=length(s)) or (i=1)) then
        delete(s,i,1);
      if (i>1) and (s[i-1]='''') then
        delete(s,i,1);
    end;
  end; *)
  if copy(s,1,1)='''' then delete(s,1,1);
  if copy(s,length(s),1)='''' then delete(s,length(s),1);
  result:=s;
end;

function ExtractBaseName(Filename:String):String;
var i:integer;
begin
  i:=length(Filename);
  while (i>0) and (Filename[i]<>'.') do dec(i);
  if i>0 then
    result:=copy(filename,1,i-1)
  else
    result:=Filename;
end;

function GetFullPath(filename:string):string;
begin
  if filename <>'' then
  begin
    if copy(filename,2,1)<>':' then
      result:=Extractfilepath(paramstr(0))+filename
    else
      result:=filename;
  end else result:='';
end;

function getRelPath(full:string):string;
var dir:string;
begin
  dir:=lowercase(extractfilepath(paramstr(0)));
  result:=lowercase(full);
  if pos(dir,result)=1 then
    delete(result,1,length(dir));
end;

function getPropDelimiter(propPath:string):Integer;
var p:Integer;
begin
  p:=0;
  if pos('.',propPath)>0 then
  begin
    p:=length(propPath);
    while (propPath[p]<>'.') do dec(p);
  end;
  result:=p;
end;

function ReplaceAll(src,old,new:string):string;
var lowersrc,lowerold:string;
    i:integer;
begin
  lowersrc:=lowercase(src);
  lowerold:=lowercase(old);
  i:=pos(lowerold,lowersrc);
  result:=src;
  while i>0 do
  begin
    delete(result,i,length(old));
    insert(new,result,i);
    i:=posnext(lowerold,lowercase(result),i+length(old));
  end;
end;

function GetPropertyNameTxt(NodeCaption:string):string;
var p:integer;
begin
  result:='';
  p:=pos('=',NodeCaption);
  if p>0 then
  begin
    result:=trim(copy(NodeCaption,1,p-1));
  end else result:=NodeCaption;
end;

function GetPropertyValueTxt(NodeCaption:string):string;
var p:integer;
begin
  result:='';
  p:=pos('=',NodeCaption);
  if p>0 then
  begin
    result:=trim(copy(NodeCaption,p+1,length(NodeCaption)-p));
  end;
end;

function GetNameFromClass(ClassName:string):string;
var i:integer;
begin
  result:=copy(classname,2,length(classname)-1);
  i:=1;
  while objectExists(result+inttostr(i)) do inc(i);
  result:=result+inttoStr(i);
end;

function indentLine(line:string;count:integer):string;
begin
  result:=StringOfChar(' ',count*2)+line;
end;

procedure IndentLines(count:integer;lines:TStrings);
var i:integer;
begin
  for i:=0 to Lines.count-1 do
  begin
    lines.strings[i]:=indentLine(lines.strings[i],count);//StringOfChar(' ',count)+lines.strings[i];
  end;
end;

function FindInStringlist(Stringlist:TStrings;s:string):integer;
var i:integer;
begin
  result:=-1;
  if not assigned(stringlist) then exit;
  s:=lowercase(s);
  i:=0;
  while (i<Stringlist.count) and (result=-1) do
  begin
    if s=lowercase(stringlist.strings[i]) then result:=i;
    inc(i);
  end;
end;

procedure ChangeObjectName(node:TTreenode;newName:string);
var p:integer;
begin
  p:=pos(':',node.Text);
  node.text:=copy(node.Text,1,pos(' ',node.Text))+newName+ copy(node.Text,p,length(node.Text)-p+1);
end;

procedure RenameControl(oldName,NewName,newClass:string);
var tn:TTreenode;
    i,p:integer;
begin
  tn:=GetObjectNode(oldName);
  if assigned(tn) then
  begin
    if NewClass='' then NewClass:=GetObjectClass(tn);
    if (oldname<>newname) and (isValidID(newname)=cirValid) and (isValidID(newClass)=cirValid) then
    begin
      p:=pos(' ',tn.text);
      i:=Form_DFMMain.CB_Objects.Items.IndexOf(copy(tn.text,p+1,length(tn.text)-p));
      tn.Text:=copy(tn.text,1,pos(' ',tn.text))+newName+': '+NewClass;
      //objectlist
      if i>-1 then
        Form_DFMMain.CB_Objects.Items.Strings[i]:=copy(tn.text,p+1,length(tn.text)-p);
      if tn.selected then
        Form_DFMMain.TreeView1Change(Form_DFMMain.treeview1,tn);
    end;
  end;
end;

function getCurrentLine(memo:TMemo):integer;
begin
  result:=SendMessage(Memo.Handle, EM_LINEFROMCHAR, Memo.SelStart, 0)+1;
end;

function getCurrentCol(memo:TMemo):integer;
var l:integer;
begin
  l:=GetCurrentLine(memo)-1;
  result:=Memo.SelStart-SendMessage(Memo.Handle,EM_LINEINDEX,l,0)+1;
end;

function ObjectExists(value:string):boolean;
var s:string;
    i:integer;
begin
  result:=false;
  for i:=0 to Form_DFMMain.CB_Objects.Items.Count-1 do
  begin
    s:=Form_DFMMain.cb_objects.items.strings[i];
    if lowercase(value)=lowercase(copy(s,1,pos(':',s)-1)) then result:=true;
  end;
end;

function GetNextObjectNumber(objectname:string):string;
var i,d:integer;
    n,s,f:string;
begin
  //s:=copy(objectclass,2,length(objectclass)-1);
  n:=getNumber(objectName);
  s:=removeNumber(objectName);
  //using same length of numbers and startnumber
  d:=length(n);
  if d>0 then
    i:=StrToInt(n)
  else
    i:=1;
  f:='%.'+IntToStr(d)+'d';
  while objectExists(s+format(f,[i])) do inc(i);
  result:=s+format(f,[i]);
end;

procedure Autocomplete(Combobox:TCombobox);
var
  oldpos: Integer;
  item: Integer;
  changeproc:TNotifyEvent;
begin
  with combobox do
  begin
    oldpos := selstart;
    item := Perform( CB_FINDSTRING, -1,
                     lparam( Pchar( text )));
    if item >= 0 then
    begin
      changeproc:=onchange;
      onchange := nil;
      text := items[item];
      selstart := oldpos;
      sellength := gettextlen - selstart;
      onchange := changeproc;
    end;
  end;
end;

//treeview functions
function isItem(node:TTreenode):boolean;
begin
  if assigned(node) then
    result:=node.ImageIndex=iiItem
  else
    result:=false;
end;

function isObject(node:TTreenode):boolean;
begin
  if assigned(node) then
    result:=node.ImageIndex=iiObject
  else
    result:=false;
end;

function isProp(node:TTreenode):boolean;
begin
  if assigned(node) then
    result:=node.ImageIndex=iiProperty
  else
    result:=false;
end;

function isControl(cName:string;node:TTreenode):boolean;
var ini:Tinifile;
    cl:TClass;
begin
  result:=false;
  cl:=getclassdef(cName);
  if assigned(cl) then
    result:=cl.InheritsFrom(TControl);
  if not result then
  begin
    ini:=Tinifile.Create(CompFile);
    result:=ini.ReadBool(cName,'<isControl>',false);
    ini.free;
  end;
  if not result and (assigned(node)) then
    result:=assigned(GetPropertyNode(node,'Width'));
end;

function GetObjectNode(obj:string):TTreenode;
var tn:TTreenode;
begin
  result:=nil;
  tn:=Form_DFMMain.treeview1.Items.GetFirstNode;
  while assigned(tn) and not assigned(result) do
  begin
    if (isObject(tn)) then
    begin
      if (pos(' '+obj+':',tn.Text)<>0) then result:=tn;
    end;
    tn:=tn.GetNext;
  end;
end;

function GetObjectFromNode(node:TTreenode):TTreenode;
begin
  result:=node;
  while assigned(result) and (result.ImageIndex<>iiObject) do
    result:=result.Parent;
end;

function GetPropertyFromNode(node:TTreenode):TTreenode;
begin
  result:=node;
  while assigned(result) and (not (result.ImageIndex in [iiProperty,iiControl..iiReplaceProp])) do
    result:=result.Parent;
end;

function getItemNode(prop:TTreenode;idx:integer):TTreenode;
begin
  result:=nil;
  if (idx>-1) and (idx<prop.Count) then
    result:=prop.Item[idx];
end;

function GetPropertyNode(obj:TTreenode;Prop:string):TTreenode;
var tn:TTreenode;
    s:string;
begin
  result:=nil;
  s:=lowercase(prop);
  tn := obj.GetFirstChild;
  while (tn <> nil) do
  begin
    if pos(s+' =',lowercase(tn.Text))=1 then
      result:=tn;
    tn:=tn.GetNextSibling;
  end;
end;

function CreateObject(Treeview:TTreeview;parent:TTreenode;oName,oType:string;SetCaption:boolean):TTreenode;
var props:TStringlist;
    i:integer;
begin
  Result:=treeview.items.AddChild(parent,'object '+oName+': '+oType);
  Props:=TStringlist.create;
  GetProperties(oType,Props);
  for i:=0 to Props.Count-1 do
    Treeview.Items.AddChildFirst(result,Props.Strings[i]);
  Props.Free;
  Form_DFMMain.CB_Objects.Items.Add(oName+': '+oType);
  Result.ImageIndex:=iiObject;
  Result.SelectedIndex:=iiObjectSel;
  for i:=0 to Result.Count-1 do
  begin
    Result.Item[i].ImageIndex:=iiProperty;
    Result.Item[i].SelectedIndex:=iiProperty;
  end;
  if SetCaption then
  begin
    if IsPropertyEx(oType,'Caption') then
      AddPropertyNode(Result,'Caption', ''''+oName+'''');
  end;
end;

function FindNode(startnode:TTreenode;Text:string):TTreenode;
var s:string;
    found:boolean;
begin
  s:=lowercase(Text);
  result:=startnode;
  found:=false;
  while assigned(result) and not found do
  begin
    if pos(s,Lowercase(result.Text))>0 then
      found:=true
    else
      result:=result.GetNext;
  end;
end;

function NodeFromCaption(aCaption:string):TTreeNode;
var N:TTreeNode;
begin
  Result:=NIL;
  N:=Form_DFMMain.Treeview1.Items.GetFirstNode;
  while Assigned(N) and (Result=NIL) do
  begin
    if pos('object '+aCaption,N.Text)=1 then Result:=N;
    N:=N.GetNext;
  end;
end;

function AddPropertyNode(AObject:TTreenode;AProperty,AValue:String):TTreenode;
var tn_prop:TTreenode;
begin
  tn_Prop:=GetPropertyNode(AObject,AProperty);
  if not assigned(tn_Prop) then
  begin
    tn_Prop:=AObject.Owner.AddChildFirst(AObject,'');
    tn_Prop.ImageIndex:=iiProperty;
    tn_Prop.SelectedIndex:=iiPropertySel;
  end;
  tn_Prop.Text:=AProperty+' = '+AValue;
  result:=tn_Prop;
end;

function AddPropertyNodeTxt(AObject,AProperty,AValue:String):TTreenode;
var tn_Object,tn_p:TTreenode;
    obj,prop:string;
    p1,p2,idx:integer;
begin
  prop:='';
  idx:=-1;
  p1:=pos('.',aObject);
  if p1>0 then
  begin
    obj:=copy(AObject,1,p1-1);
    p2:=pos('[',AObject);
    prop:=copy(AObject,p1+1,p2-p1-1);
    if p2>0 then
      idx:=StrToInt(copy(AObject,p2+1,pos(']',AObject)-p2-1));
  end else obj:=AObject;
  tn_Object:=GetObjectNode(obj);
  if prop<>'' then
  begin
    tn_p:=GetPropertyNode(tn_Object,Prop);
    if idx>-1 then
      tn_Object:=getItemNode(tn_p,idx);
  end;
  result:=AddPropertyNode(tn_Object,AProperty,AValue);
end;

function GetNextObject(node:TTreenode):TTreenode;
begin
  result:=node.getNext;
  while assigned(result) and not isObject(result) do
    result:=result.getNext;
end;

function GetFormNode:TTreenode;
begin
  result:=Form_DFMMain.treeview1.Items.GetFirstNode;
end;

function GetFormName:string;
var tn:TTreenode;
begin
  tn:=GetFormNode;
  if assigned(tn) then
    Result:=GetObjectName(tn);
end;

function GetFormClass:string;
var tn:TTreenode;
begin
  tn:=GetFormNode;
  if assigned(tn) then
    Result:=GetObjectClass(tn);
end;

function GetObjectName(Node:TTreenode):string;
begin
  result:='';
  if assigned(node) then
  begin
    if isObject(node) then
      result:=trim(copy(node.text,7,pos(':',node.text)-7));
  end;
end;

function GetObjectClass(Node:TTreenode):string;
var p:integer;
begin
  result:='';
  if assigned(node) then
  begin
    p:=pos(':',node.text);
    if isObject(node) then
      result:=trim(copy(node.text,p+1,length(node.text)-p));
  end;
end;

function GetPropertyName(Node:TTreenode):string;
begin
  result:='';
  if assigned(node) then
  begin
    result:=getPropertyNameTxt(node.text);
  end;
  OutputDebugString(PChar(result));
end;

function GetPropertyValue(Node:TTreenode):string;
begin
  result:='';
  if assigned(node) then
  begin
    result:=GetPropertyValueTxt(node.text);
  end;
  OutputDebugString(PChar(result));
end;

function NodeCopy(Dest,Source: TTreeNode): TTreeNode;
var i : Integer; ANode : TTreeNode;
begin
// Copy TreeNode from Source to Dest
    ANode:=dest.Owner.AddChild(Dest,'');
    ANode.Assign(Source);
    for i:= 0 to Source.Count-1 do
        NodeCopy(ANode,Source.Item[i]);
    Result:=ANode;
end;

function isChild(childnode,ParentNode:TTreenode):boolean;
var tn:ttreenode;
begin
  result:=false;
  tn:=childnode;
  while assigned(tn) and not result do
  begin
    if tn=parentnode then result:=true;
    tn:=tn.parent;
  end;
end;

procedure MoveNodeUp(node:TTreenode);
var
  expanded: boolean;
begin
  if (not assigned(Node)) or (Node.getPrevSibling=nil) then Exit;
  expanded:=node.Expanded;
  //objects=0, properties=1, entries=2
  if node.Imageindex<>node.GetPrevSibling.ImageIndex then exit;

  Node.MoveTo( Node.getPrevSibling, naInsert ) ;
  Node.Expanded:=Expanded;
end;

procedure MoveNodeDown(node:TTreenode);
var
  expanded: boolean;
begin
  if (not assigned(Node)) or (Node.getNextSibling=nil) then Exit;
  expanded:=node.Expanded;
  //objects=0, properties=1, entries=2
  if node.Imageindex<>node.GetNextSibling.ImageIndex then exit;

  if Node.getNextSibling.getNextSibling = nil then
    Node.MoveTo( Node.getNextSibling, naAdd )
  else
    Node.MoveTo( Node.getNextSibling.getNextSibling, naInsert );
  Node.Expanded:=Expanded;
end;

procedure MoveNodeTop(node:TTreenode);
var
  tmpNode: TTreeNode;
  expanded: boolean;
begin
  if (not assigned(Node)) or (Node.getPrevSibling=nil) then Exit;
  expanded:=node.Expanded;
  tmpNode:=node.GetPrevSibling;
  while (tmpnode.getPrevSibling<>nil) and (node.Imageindex=tmpnode.getPrevSibling.ImageIndex) do
    tmpNode:=tmpNode.GetPrevSibling;
  if node.Imageindex<>tmpnode.ImageIndex then exit;

  Node.MoveTo( tmpNode, naInsert );
  Node.Expanded:=Expanded;
end;

procedure MoveNodeBottom(node:TTreenode);
var
  tmpNode: TTreeNode;
  expanded: boolean;
begin
  if (not assigned(Node)) or (Node.getNextSibling=nil) then Exit;
  tmpNode:=node.GetNextSibling;
  expanded:=node.Expanded;

  while (tmpnode.getNextSibling<>nil) and (node.Imageindex=tmpnode.getNextSibling.ImageIndex) do
    tmpNode:=tmpNode.GetNextSibling;

  if tmpNode.getNextSibling = nil then
    Node.MoveTo( tmpNode, naAdd )
  else
    Node.MoveTo( tmpNode.getNextSibling, naInsert );
  Node.Expanded:=Expanded;
end;

function GetCompNode(name:string):TTreenode;
begin
  result:=Form_DFMOptions.Treeview1.Items.GetFirstNode;
  while assigned(result) and (result.Text<>name) do
    result:=result.GetNextSibling;
end;

function GetPropNode(tn:TTreenode;Name:String):TTreenode;
begin
  result:=tn.GetFirstChild;
  while assigned(result) and (result.ImageIndex in [iiProperty,iiControl..iiReplaceProp]) and (GetPropertyName(result)<>Name) do
    result:=result.GetNextSibling;
end;

function GetTypeNode(tn:TTreenode):TTreenode;
begin
  result:=tn.GetFirstChild;
  while assigned(result) and (result.ImageIndex<>iiType) do
    result:=result.GetNextSibling;
end;

function GetValueNode(tn:TTreenode):TTreenode;
begin
  result:=tn.GetFirstChild;
  while assigned(result) and (result.ImageIndex<>iiValue) do
    result:=result.GetNextSibling;
end;

function GetRuleNode(tn:TTreenode):TTreenode;
begin
  result:=tn.GetFirstChild;
  while assigned(result) and (result.ImageIndex<>iiUnwanted) do
    result:=result.GetNextSibling;
end;

function DownloadFile(url,filename: string) : Boolean ;
var
  Stream : TFileStream ;
  NetHandle : HINTERNET ;
  UrlHandle : HINTERNET;
  BytesRead : DWORD ; // DWORD, not cardinal
  Buffer : array[0..1024] of Char ;
begin
  Result := false ;
  NetHandle := InternetOpen('Delphi download function',
                            INTERNET_OPEN_TYPE_DIRECT,
                            Nil,Nil,0);
  if Assigned(NetHandle) then
  begin
    UrlHandle := InternetOpenUrl(NetHandle, PChar(url), nil, 0,
                                 INTERNET_FLAG_RELOAD, 0);
    if Assigned(UrlHandle) then
    begin
      FillChar(Buffer, SizeOf(Buffer), 0);
      Stream := TFileStream.Create(filename,fmCreate);
      try
        while InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer), BytesRead) and (BytesRead > 0) do
          Stream.WriteBuffer(Buffer, BytesRead);
        Result := TRUE;
       finally
        Stream.Free;
      end;
    end; // if Assigned(UrlHandle)
    InternetCloseHandle(UrlHandle);
  end;
  InternetCloseHandle(NetHandle);
end;

procedure CreateBackup(filename:string);
var dir,fn,fn_1,fn_2:string;
    i:integer;
begin
  if fileexists(filename) then
  begin
    if cfg.BackupCount>0 then
    begin
      if copy(cfg.BackupDir,2,1)=':' then
        dir:=cfg.BackupDir
      else
        dir:=ExtractFilePath(filename)+cfg.BackupDir;
      ForceDirectories(dir);
      fn:=ExtractFileName(filename);
      fn_2:=dir+'\'+fn+'.'+IntToStr(cfg.BackupCount)+'.bak';
      if fileexists(fn_2) then deletefile(fn_2);
      for i:=cfg.BackupCount-1 downto 1 do
      begin
        fn_1:=dir+'\'+fn+'.'+IntToStr(i)+'.bak';
        if fileexists(fn_1) then
          renamefile(fn_1,fn_2);
        fn_2:=fn_1;
      end;
      copyfile(PChar(filename),PChar(dir+'\'+fn+'.1.bak'),false);
      //copyfile(PChar(filename),PChar(filename+'.bak'),false);
    end;
  end;
end;

procedure GetComponents(output:TStrings);
var ini:Tinifile;
    i:integer;
begin
  Output.Clear;
  ini:=Tinifile.Create(CompFile);
  ini.ReadSections(Output);
  Output.Delete(Output.indexof('default'));
  Output.Delete(Output.indexof('control'));
  Output.Delete(Output.indexof('TForm'));
  for i:=Output.Count-1 downto 0 do
  begin
    if ini.ReadBool(Output.Strings[i],'<hide>',false) {and (findTypeInfo(Output.Strings[i])=nil)} then
      Output.Delete(i);
  end;
  ini.Free;
end;

procedure GetProperties(Component:string;Output:TStrings);
var ini:Tinifile;
    sl,defProp,ctlProp:TStringlist;
    repName,s:string;
    i:integer;
    cl:TClass;
begin
  Output.Clear;
  defProp:=TStringlist.create;
  ctlProp:=TStringlist.Create;
  sl:=tstringlist.Create;
  sl.Duplicates:=dupIgnore;
  sl.Sorted:=true; //für duplicates
  ini:=Tinifile.Create(CompFile);
  ini.ReadSection('default',defProp);
  ini.ReadSection('control',ctlProp);
  ini.ReadSection(Component,sl);
  sl.AddStrings(defprop);
  cl:=getclassdef(component);
  if ini.ReadBool(Component,'<isControl>',false) or
    (assigned(cl) and cl.InheritsFrom(TControl)) then
  begin
    sl.AddStrings(ctlprop);
  end;
  repName:=GetReplacement(Component,nil);
  if repName<>'' then
  begin
    ini.ReadSection(repName,defprop);
    sl.AddStrings(defprop);
    cl:=getclassdef(repName);
    if ini.ReadBool(repName,'<isControl>',false) or
      (assigned(cl) and cl.InheritsFrom(TControl)) then
    begin
      sl.AddStrings(ctlprop);
    end;
  end;
  for i:=0 to sl.Count-1 do
  begin
    if not isFirstChar(PChar(sl.strings[i]),'<') then
    begin //beginning with < indicates a option for dfmedit not a property
      s:=ini.ReadString(Component,sl.strings[i],'');
      if (s='') and (repName<>'') then
        s:=ini.ReadString(repName,sl.strings[i],'');
      if s='' then
        s:=ini.ReadString('control',sl.strings[i],'');
      if s='' then
        s:=ini.ReadString('default',sl.strings[i],'');
      Output.Add(sl.strings[i]+' = '+s);
    end;
  end;
  ini.Free;
  sl.Free;
  ctlProp.Free;
  defprop.Free;
end;

function GetReplacement(Component:string;ignore:TStringlist):string;
var ini:Tinifile;
begin
  ini:=Tinifile.Create(CompFile);
  result:=ini.ReadString(Component,'<replace>','');
  if assigned(ignore) then
    ignore.commaText:=lowercase(ini.ReadString(Component,'<ignore>',''));
  ini.free;
end;

procedure GetReplacementProperties(Component:string;properties:TStringList);
var ini:Tinifile;
begin
  if assigned(properties) then
  begin
    properties.Clear;
    ini:=Tinifile.Create(CompFile);
    properties.commaText:=ini.ReadString(Component,'<replaceProperties>','');
    //showmessage(properties.text);
    ini.free;
  end;
end;

procedure SetLanguage(lang:string);
begin
  if (lang<>'') and (lowercase(lang)<>'de') then
  begin
    LocalizeControls(Form_DFMMain,Lang);
    LocalizeControls(Form_DFMAdd,Lang);
    LocalizeControls(Form_DFMAbout,Lang);
    LocalizeControls(Form_DFMChooseRes,Lang);
    LocalizeControls(Form_DFMInspector,Lang); //caption of form
    LocalizeControls(Form_DFMOptions,Lang);
  end else if not loading and (lang<>cfg.language) then showmessage(localizeString('Msg_MustRestart'));
  LocalizeControls(Form_DFMPalette,Lang);
  Form_DFMOptions.Stringgrid1.cells[0,1]:=LocalizeString('Cap_AboutForm');
  Form_DFMOptions.Stringgrid1.cells[0,2]:=LocalizeString('Cap_AddForm');
  Form_DFMOptions.Stringgrid1.cells[0,3]:=LocalizeString('Cap_ChooseResForm');
  Form_DFMOptions.Stringgrid1.cells[0,4]:=LocalizeString('Cap_InspectorForm');
  Form_DFMOptions.Stringgrid1.cells[0,5]:=LocalizeString('Cap_MainForm');
  Form_DFMOptions.Stringgrid1.cells[0,6]:=LocalizeString('Cap_OptionsForm');
  Form_DFMOptions.Stringgrid1.cells[0,7]:=LocalizeString('Cap_PaletteForm');

  Form_DFMOptions.Stringgrid1.cells[1,0]:=LocalizeString('Cap_Left');
  Form_DFMOptions.Stringgrid1.cells[2,0]:=LocalizeString('Cap_Top');
  Form_DFMOptions.Stringgrid1.cells[3,0]:=LocalizeString('Cap_Height');
  Form_DFMOptions.Stringgrid1.cells[4,0]:=LocalizeString('Cap_Width');

  Form_DFMInspector.HeaderControl1.Sections[1].Text:=LocalizeString('Cap_PropName');
  Form_DFMInspector.HeaderControl1.Sections[2].Text:=LocalizeString('Cap_PropValue');
end;
{$IFNDEF NOPACKAGES}
procedure unloadPackages;
var i:integer;
begin
  for i:=0 to 9 do
  begin
    if cfg.packages[i].handle>0 then
    begin
      unloadpackage(cfg.packages[i].handle);
      cfg.packages[i].handle:=0;
    end;
  end;
end;
{$ENDIF}
procedure RecreateMainForm;
var ms:TMemorystream;
    sl:TStringlist;
    tn:TTreenode;
begin
  ms:=TMemoryStream.Create;
  ms.WriteComponent(Form_DFMMain.TreeView1);
  ms.position:=0;
  sl:=TStringlist.create;
  sl.Assign(Form_DFMMain.CB_Objects.Items);

  Form_DFMMain.Free;
  Application.CreateForm(TForm_DFMMain,Form_DFMMain);
  ms.ReadComponent(Form_DFMMain.TreeView1);
  tn:=Form_DFMMain.TreeView1.Items.GetFirstNode;
  if assigned(tn) then
    tn.Expand(false);
  Form_DFMMain.CB_Objects.Items.Assign(sl);
  Form_DFMMain.Show;
  sl.Free;
  ms.free;
end;

procedure RecreateForms;
begin
  Form_DFMOptions.Free;
  Application.CreateForm(TForm_DFMOptions,Form_DFMOptions);
  Form_DFMAbout.Free;
  Application.CreateForm(TForm_DFMAbout,Form_DFMAbout);
  Form_DFMAdd.Free;
  Application.CreateForm(TForm_DFMAdd,Form_DFMAdd);
  Form_DFMChooseRes.Free;
  Application.CreateForm(TForm_DFMChooseRes,Form_DFMChooseRes);
  Form_DFMInspector.Free;
  Application.CreateForm(TForm_DFMInspector,Form_DFMInspector);
end;

function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer;
begin
  Result := SendMessage(hwndTC, TCM_HITTEST, 0, LPARAM(pinfo));
end;
//------------------------------------------------------------------------------

function MyIndexOfTabAt(PageControl: TPageControl; X, Y: Integer): Integer;
var
  HitTest: TTCHitTestInfo;
begin
  Result := -1;
  if PtInRect(PageControl.ClientRect, Point(X, Y)) then
    with HitTest do
    begin
      pt.X := X;
      pt.Y := Y;
      Result := TabCtrl_HitTest(PageControl.Handle, @HitTest);
    end;
end;

procedure Loadimages(imagelist:TImagelist;filename:string);
var src_bmp,bmp:TBitmap;
    c_w,c_h,vw,vh,pw,ph:integer;
begin
  imagelist.Width:=tb_iw;
  imagelist.height:=tb_ih;
  src_bmp:=TBitmap.create;
  try
    bmp:=TBitmap.create;
    try
      src_bmp.LoadFromFile(filename);
      c_w:=src_bmp.width div tb_iw;
      c_h:=src_bmp.height div tb_ih;
      bmp.width:=tb_iw;
      bmp.Height:=tb_ih;
      bmp.PixelFormat:=src_bmp.PixelFormat;
      for vh:=0 to c_h-1 do
      begin
        for vw:=0 to c_w-1 do
        begin
          pw:=vw*tb_iw;
          ph:=vh*tb_ih;
          bmp.canvas.CopyRect(Rect(0,0,tb_iw,tb_ih),src_bmp.Canvas,Rect(pw,ph,pw+tb_iw,ph+tb_ih));
          imagelist.AddMasked(bmp,bmp.Canvas.Pixels[0,0]);
        end;
      end;
    finally
      bmp.free;
    end;
  finally
    src_bmp.free;
  end;
end;

procedure CreateTab(Pagecontrol:TPagecontrol;Imagelist:TImageList;ScrollBarChange:TNotifyEvent);
var ts:TTabsheet;
    tb:TToolbar;
    sb:TScrollbar;
begin
  ts:=TTabsheet.Create(Pagecontrol.owner);
  ts.PageControl:=Pagecontrol;
  ts.Name:='Tab_'+IntToStr(pagecontrol.PageCount-1)+'_Components';
  ts.TabVisible:=false;
  tb:=TToolbar.create(Pagecontrol.owner);
  tb.name:='TBar_'+IntToStr(pagecontrol.PageCount-1);
  tb.Parent:=ts;
  tb.EdgeBorders:=[];
  tb.Align:=alnone;
  tb.ShowCaptions:=true;
  //tb.Color:=clBlue;
  tb.ShowHint:=true;
  tb.Images:=imagelist;
  tb.Flat:=true;
  //tb.BorderWidth:=1;
  tb.ShowCaptions:=false;
  tb.Wrapable:=false;
  //tb.Autosize:=true;  //autosize buggy in D3
  sb:=Tscrollbar.create(ts);
  sb.parent:=ts;
  sb.align:=alBottom;
  sb.onChange:=scrollbarChange;
  tb.Width:=0;
end;

procedure SortButtons(tb:TToolbar);
var b,j:integer;
    tbtn:TToolbutton;
begin
  for b := 0 to tb.ButtonCount - 1
  do begin
    j := b;

    while (tb.Buttons[j].Tag <> b) and (j < tb.ButtonCount - 1)
    do inc (j);

    if tb.Buttons[j].Tag = b
    then begin
      tbtn      := tb.Buttons[j];
      tbtn.Left := tbtn.Tag * tb.ButtonWidth;
    end;
  end;
end;

procedure CreateToolButton(tb:TToolbar;Position,imageindex:integer;Caption:string;ClickEvent:TNotifyEvent);
var tbtn:TToolbutton;
begin
  tbtn:=TToolbutton.Create(tb);
  //if imageindex>-1 then
    tbtn.ImageIndex:=imageindex;
  tbtn.name:='TB_'+Caption;
  tbtn.Parent:=tb;
  tbtn.Caption:=Caption;
  tbtn.Hint:=caption;
  tbtn.OnClick:=ClickEvent;
  tbtn.tag:=position;
  if tbtn.tag=-1 then
  begin
    if (imageindex>-1) and (imageindex<>99) then
      tbtn.tag:=imageindex
    else
      tbtn.tag:=tb.buttoncount;
  end;
end;

procedure getMenuItem(editor:TMenuEditor;node:TTreenode);
var tn:TTreenode;
    mEntry:TMenuEntry;
    mEditor:TMenuEditor;
begin
  //showmessage(node.text);
  mEntry:=Editor.AddEntry(getObjectName(node));
  mentry.Data:=node;
  mEntry.Name:=getObjectName(node);
  mEntry.SetPropertyValue('Name',mEntry.Name);
  tn:=node.GetFirstChild;
  while assigned(tn) do
  begin
    if isProp(tn) then
    begin
      mEntry.SetPropertyValue(GetPropertyName(tn),{RemoveStringQuotes}RemoveSingleQuotes(GetPropertyValue(tn)));
      tn:=tn.getNextSibling;
    end else
    begin
      mEditor:=Editor.AddSubMenu(mEntry.Index,false);
      LoadMenuFromTreeview(mEditor,tn);
      tn:=nil;
    end;
  end;
end;

procedure LoadMenuFromTreeview(Editor:TMenuEditor;node:TTreenode);
begin
  Editor.Data:=node.parent;
  while assigned(node) do
  begin
    if isObject(Node) then
      getMenuItem(Editor,node);
    node:=node.GetNextSibling;
  end;
end;

procedure buildMenu(Editor:TMenuEditor;node:TTreenode);
begin
  if pos('mainmenu',lowercase(GetObjectClass(node)))>0 then
    Editor.MenuType:=mtMain
  else
    Editor.MenuType:=mtPopup;
  LoadMenuFromTreeview(Editor,node.GetfirstChild);
end;

end.
