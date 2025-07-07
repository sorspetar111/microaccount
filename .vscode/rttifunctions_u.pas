unit rttifunctions_u;

{*
Program: DFMEdit
Unit: rttifunctions_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
various RTTI-Functions

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

interface

uses
  windows, messages, Classes, sysutils, stdctrls, ComCtrls,graphics, typinfo, Classes_u,
  dialogs,controls,inifiles,menus;

function IsProperty(Obj:TObject; AProperty: String): Boolean;
function IsPropertyEx(AClass, AProperty: String): Boolean;

function isValueValid(obj:TObject;AProperty:string;var AValue:string):boolean;

function FindTypeInfo(const ATypeName: string): PTypeInfo;
function GetEnumNames(AEnum: PTypeInfo; Names: TStrings): Boolean;
procedure GetComponentProps(AComponent:string;List:TStrings;AppendSubclasses:boolean);
function CreateComponent(aName,aType:string;Parent:TWinControl;Owner:TComponent):TComponent;
function CloneObject(tn:TTreenode;Item,Owner,Parent:TComponent):TComponent;
function SetProperty(comp:TComponent;prop:string;var value:string):TSetPropResult;
procedure AssignPropsFromNode(Node:TTreenode;Comp:TComponent);
procedure SetSet(instance:TObject;Prop,Value:string);
//function GetSetValue(setInfo:pTypeInfo;valueStr:string):Integer;
function GetValue(instance:TObject;Prop:string):string;
function getSubInstance (aRoot: TObject; aName: string): TObject;
function getSubclass (aComp: TComponent; aPropName: string): TObject;
function getCollectionClass(classname,propname:string):string;
function GetClassDef(classname:string):TClass;
function GetCompHierarchy(CompType:string):string;

implementation

uses
  //main_u, addproperty_u, localize_u;
  functions_u,localize_u;

function IsProperty(Obj:TObject; AProperty: String): Boolean;
var
  PropInfo : PPropinfo;
begin
  result:=false;
  if not assigned(obj) then exit;
  PropInfo := GetPropInfo(Obj.ClassInfo, AProperty);
  Result := (PropInfo <> nil);
  if assigned(obj) and (not result) then
  begin
    //Sondereigenschaften
    if (obj.classname='TImageList') and (AProperty='Bitmap') then result:=true;
    if (obj.classname='TStringGrid') then
    begin
      if (AProperty='ColWidths') or (AProperty='RowHeights') then result:=true;
    end;
  end;
end;

function IsPropertyEx(AClass, AProperty: String): Boolean;
var
   PropInfo: PPropInfo;
   TI:PTypeInfo;
begin
  PropInfo:=nil;
  ti:=FindTypeInfo(AClass);
  if assigned(ti) then
   PropInfo := GetPropInfo(ti, AProperty);
 Result := Assigned(PropInfo) ;
end;

function isValueValid(obj:TObject;AProperty:string;var AValue:string):boolean;
var
  PI: PPropInfo;
  TI,TI2 : PTypeInfo;
  TD: PTypeData;
  p,tmp:integer;
  s,s2,s3:string;
begin
  result:=false;
  if not assigned(obj) then exit;

  p:=pos('.',AProperty);
  if p>0 then
  begin
    PI:= GetPropInfo(Obj.ClassInfo, copy(AProperty,1,p-1));
    if not assigned(PI) then exit;
    TI:=PI^.PropType^;
    if not assigned(TI) then exit;
    PI:=GetPropInfo(ti,copy(AProperty,p+1,length(AProperty)-p));
  end else
  begin
    PI := GetPropInfo(Obj.ClassInfo, AProperty);
  end;
  if not assigned(PI) then exit;
  TI:=PI^.PropType^;
  if not assigned(TI) then exit;
  if ti.Kind=tkEnumeration then
    result:=GetEnumValue(TI,AValue)<>-1
  else if ti.Kind=tkSet then
  begin
    //if tkset includes unknown value, property will be deleted
    result:=true;
    TD:=GetTypeData(TI);
    TI2:=TD.CompType^;

    s:=copy(AValue,2,length(AValue)-2);
    s3:='';
    while (length(s)>0) and result do
    begin
      p:=pos(',',s);
      if p=0 then p:=length(s)+1;
      s2:=trim(copy(s,1,p-1));
      delete(s,1,p);
      tmp:=GetEnumValue(TI2,s2);
      if tmp=-1 then
      begin
        //result:=false;
      end else
      begin
        if s3<>'' then s3:=s3+', ';
        s3:=s3+s2;
      end;
    end;
    aValue:='['+s3+']';
  end else
    result:=true;
end;

//**********************  functions for FindTypeInfo  **********************
function EnumTypeInfos_base(AModule: integer; AFunc: TEnumTypeInfoFunc; AUserData: Pointer): PTypeInfo;
// copyright (c) 1998 Hagen Reddmann
  function GetBaseOfCode(AModule: integer; var ACodeStart, ACodeEnd: PChar): Boolean; register;
  // get Codesegment pointers, check if module is a valid PE
  asm
    PUSH  EDI
    PUSH  ESI
    AND   EAX,not 3
    JZ    @@2
    CMP   Word Ptr [EAX],'ZM';
    JNE   @@1
    MOV   ESI,[EAX + 03Ch]
    CMP   Word Ptr [ESI + EAX],'EP'
    JNE   @@1
    MOV   EDI,[EAX + ESI + 014h + 008h]
    ADD   EAX,[EAX + ESI + 014h + 018h]
    ADD   EDI,EAX
    MOV   [EDX],EAX
    MOV   [ECX],EDI
    XOR   EAX,EAX
    @@1:   SETE  AL
    @@2:   POP   ESI
           POP   EDI
  end;

type
  PLongWord = ^integer;
  PByte = ^Byte;
var
  P,E,K,N: PChar;
  L: Integer;
begin
  Result := nil;
  try
    if GetBaseOfCode(AModule, P, E) then
      while P < E do
      begin
        integer(P) := integer(P) and not 3;
        K := P + 4;
        if (PLongWord(P)^ = integer(K)) and (TTypeKind(K^) >= Low(TTypeKind)) and (TTypeKind(K^) <= High(TTypeKind)) then
        begin
          L := PByte(K + 1)^;  // length Info.Name
          N := K + 2;          // @Info.Name[1]
          if (L > 0) and (N^ in ['_', 'a'..'z', 'A'..'Z']) then  // valid ident ??
          begin
            repeat
              Inc(N);
              Dec(L);
            until (L = 0) or not (N^ in ['_', 'a'..'z', 'A'..'Z', '0'..'9']);
            if L = 0 then // length and ident valid
              if not Assigned(AFunc) or AFunc(AUserData, Pointer(K)) then // tell it and if needed abort iteration
              begin
                Result := Pointer(K);
                Exit;
              end else K := N;
          end;
        end;
        P := K;
      end;
  except
  end;
end;

function EnumTypeInfos(AFunc: TEnumTypeInfoFunc; AUserData: Pointer): PTypeInfo;
type
  PModulesEnumData = ^TModulesEnumData;
  TModulesEnumData = packed record
    AFunc: TEnumTypeInfoFunc;
    AUserData: Pointer;
    AResult: PTypeInfo;
  end;

  function EnumTypeInfosInModule(AModule: integer; AData: PModulesEnumData): Boolean; register;
  begin
    with AData^ do
    begin
      AResult := EnumTypeInfos_base(AModule, AFunc, AUserData);
      Result := AResult = nil;
    end;
  end;

var
  Data: TModulesEnumData;
begin
  Data.AFunc := AFunc;
  Data.AUserData := AUserData;
  Data.AResult := nil;
  EnumModules(TEnumModuleFunc(@EnumTypeInfosInModule), @Data);
  Result := Data.AResult;
end;

function IsTypeCorrespondingToName(AName: Pointer; ATypeInfo: PTypeInfo): Boolean;
begin
  Result := AnsiCompareText(PChar(AName), ATypeInfo.Name) = 0;
end;

function FindTypeInfo(const ATypeName: string): PTypeInfo;
begin
  Result := EnumTypeInfos(IsTypeCorrespondingToName, PChar(ATypeName));
end;
//******************** end - functions for FindTypeInfo  ********************

function GetEnumNames(AEnum: PTypeInfo; Names: TStrings): Boolean;
var
  I: Integer;
  TypeData: PTypeData;
begin
  try
    if not Assigned(Names) then Abort;
    Names.Clear;
    if not Assigned(AEnum) then Abort;
    if AEnum.Kind <> tkEnumeration then Abort;
    TypeData:= GetTypeData(AEnum);
    for i:= TypeData.MinValue to TypeData.MaxValue do
      Names.Add(GetEnumName(AEnum, I));
    Result:= True;
  except
    Result:= False;
  end;
end;

procedure AddSpecialProperties(classname:string;list:TStrings);
var oType:TTypeObject;
    ini:TInifile;
    sl:TStringlist;
    i,p:integer;
    s,s2:string;
begin
  ini:=Tinifile.create(propfile);
  sl:=TStringlist.create;
  ini.ReadSection(classname,sl);
  for i:=0 to sl.Count-1 do
  begin
    oType:=TTypeObject.create;
    oType.PropKind:=tkUnknown;
    s:=ini.ReadString(classname,sl.strings[i],'');
    s2:=lowercase(s);
    otype.CompType:=findTypeinfo(s);
    if assigned(otype.CompType) then
    begin
      otype.PropKind:=otype.CompType.Kind;
      if otype.PropKind=tkset then
        otype.CompType:=GetTypeData(otype.CompType).CompType^;
    end;
    oType.TypeName:=s;
    p:=list.IndexOf(sl.strings[i]);
    if p=-1 then
      list.AddObject(sl.strings[i],oType)
    else
    begin
      ttypeobject(list.Objects[p]).free;
      list.Objects[p]:=otype;
    end;
  end;
  sl.free;
  ini.free;
end;

procedure GetComponentProps(AComponent:string;List:TStrings;AppendSubclasses:boolean);
var
  dummy:PTypeInfo;
  d2:PTypeData;
  pl:PPropList;
  x,i,j:integer;
  classe:TClass;
  propname:string;
  oType:TTypeObject;
  sl:TStringlist;
begin
  //classe:=getclass(AComponent);
  FreeSLObjects(list);
  list.clear;
  classe:=getClassDef(AComponent);
  (*
  ti:=findTypeinfo(AComponent);
  if not assigned(ti) then exit;
  td:=getTypeData(ti);
  if not assigned(td) then exit;
  classe:=td.ClassType;
  *)
  if assigned(classe) then
  begin
    dummy:=classe.ClassInfo;
    d2:=gettypedata(dummy);
    getmem(pl,d2^.propcount*sizeof(pointer));
    x:=getproplist(dummy,tkAny,pl);
    for i:=0 to x-1 do
    begin
      propname:=pl^[i].name;
      oType:=TTypeObject.create;
      oType.PropKind:=pl^[i].PropType^.Kind;
      oType.TypeName:=pl^[i].PropType^.name;
      if pl^[i].PropType^.Kind=tkSet then
        oType.CompType:=GetTypeData(pl^[i].PropType^).CompType^
      else
        oType.CompType:=FindTypeInfo(oType.TypeName);
      if AppendSubclasses and (oType.propKind=tkClass) then
      begin
        classe:=getclass(otype.TypeName);
        if assigned(classe) and (not classe.InheritsFrom(TComponent)) then
        begin
          sl:=TStringlist.create;
          getcomponentProps(otype.TypeName,sl,true);
          if sl.count>0 then
          begin
            oType.Free; //if subclasses free typeobject of parent
            for j:=0 to sl.count-1 do
              list.AddObject(propname+'.'+sl.strings[j],sl.objects[j]);
          end else list.AddObject(propName,oType);
          sl.free;
        end else
          list.addObject(propName,oType);
      end else
        list.addObject(propName,oType);
    end;
    freemem(pl);
  end;
  AddSpecialProperties(AComponent,list);
end;

function CreateComponent(aName,aType:string;Parent:TWinControl;Owner:TComponent):TComponent;
var
  classe:TpersistentClass;
  CC: TComponentClass;
begin
  result:=nil;
  classe:=getclass(AType);
  if assigned(classe) then
  begin
    CC:=TComponentClass(classe);
    result:=cc.Create(owner);
  end else if assigned(parent) then
  begin
    result:=TUnknownControl.create(Owner);
  end;
  if assigned(result) then
  begin
    if result.InheritsFrom(TControl) then
      (result as TControl).parent:=parent;
    if (classe=TTabSheet) and (Parent is TPageControl) then
      TTabSheet(result).Pagecontrol:=TPageControl(parent);
    result.Name:=aName;
  end;
end;

function CloneObject(tn:TTreenode;Item,Owner,Parent:TComponent):TComponent;
var MS : TMemoryStream;
    A,i : integer;
    C : TControl;
    CName,OldName : string[63];
    tn2:TTreenode;
begin
  MS := TMemoryStream.Create;
  try
    i:=0;
    repeat
      Inc(i);
      CName := copy(Item.ClassName,2,Pred(Length(Item.ClassName)))+ IntToStr(i);
    until not Assigned(item.Owner.FindComponent(Cname));

    OldName:=Item.Name;
    Item.Name := CName;

    if assigned(tn) and (pos(lowercase(oldname),lowercase(tn.text))>0) then
    begin
      tn.Text:=copy(tn.text,1,pos(' ',tn.text))+cName+': '+item.ClassName;
    end;

    MS.WriteComponent(Item);
    Item.Name := OldName;
    Result := TComponentClass(Item.ClassType).Create(Owner);
    if Result is TControl then
      TControl(Result).Parent := TWinControl(Parent);
    MS.Seek(0,0);
    MS.ReadComponent(Result);
    //result.name:=cname;
    //AssignName(Result,CName);
    if Result is TControl then
      TControl(Result).Parent := TWinControl(Parent);
    if (Item is TWinControl)
       and (csAcceptsControls in TWinControl(Item).ControlStyle) then
      for A := 0 to TWinControl(Item).ControlCount-1 do
      begin
        C := TWinControl(Item).Controls[A];
        tn2:=tn.getFirstChild;
        while assigned(tn2) do
        begin
          if isObject(tn2) then
          begin
            if pos(lowercase(c.name),lowercase(tn2.text))>0 then
              tn:=tn2;
          end;
          tn2:=tn2.GetNextSibling;
        end;
        if C.Owner=Item.Owner then
          if CloneObject(tn,C,Owner,Result)=nil then;
      end;
  finally
    MS.Free;
  end;
end;

function isInherited(classname:string;baseClass:TClass):boolean;
var cl:TPersistentClass;
begin
  cl:=Getclass(ClassName);
  if assigned(cl) then
    result:=cl.InheritsFrom(BaseClass)
  else result:=false;
end;

function SetProperty(comp:TComponent;prop:string;var value:string):TSetPropResult;
var pi:PPropInfo;
    obj:TObject;
    p,v:integer;
    c:TComponent;
begin
  result:=sprOK;
  try
    p:=GetPropDelimiter(prop);
    if p>0 then
    begin
      obj:=GetSubInstance(comp,copy(prop,1,p-1));
      delete(prop,1,p);
    end else obj:=comp;
    pi:=getPropInfo(findTypeInfo(obj.classname),prop);
    if assigned(pi) then
    begin
      if pi.PropType^.Kind in [tkEnumeration] then
      begin
        v:=getEnumValue(pi.PropType^,value);
        if v>-1 then
          setOrdProp(obj,pi,v)
        else
          Raise ERangeError.Create('');
      end else if pi.PropType^.Kind in [tkInteger] then
      begin
        if pi.PropType^.Name='TColor' then
          v:=StringToColor(value)
        else if pi.PropType^.Name='TCursor' then
          v:=StringToCursor(value)
        else if pi.PropType^.Name='TFontCharset' then
          IdentToCharset(value,v)
        else if pi.PropType^=TypeInfo(TShortcut) then
          v:=TextToShortCut(value)
        else
          v:=StrToInt(value);
        setOrdProp(obj,pi,v);
      end else if pi.PropType^.Kind in [tkSet] then
        SetSet(obj,prop,value)
      else if pi.PropType^.Kind in [tkString, tkWChar, tkLString, tkWString] then
      begin
        setStrProp(obj,pi,value);
        value:=''''+value+'''';
      end else if pi.PropType^.Kind in [tkClass] then
      begin
        if isInherited(pi.PropType^.Name,TComponent) then
        begin
          c:=comp;
          while c.GetParentComponent<>nil do c:=c.GetParentComponent;
          c:=c.FindComponent(Value);
          if (value='') or (assigned(c)) then
            SetOrdProp(obj,pi,longint(TPersistent(c)))
          else
          begin
            showmessage(format(localizeString('Msg_ComponentNotFound'),[Value]));
            result:=sprCompNotFound;
          end;
        end else
        begin
          result:=sprNoChangeAllowed;  //prevent direct editing of non-component-classes
          setOrdProp(obj,pi,strToInt(Value));
        end;
      end else if pi.PropType^.Kind in [tkFloat] then
      begin
        //if pi.PropType^=TypeInfo(TDate) then
        //  SetFloatProp(obj,pi,StrToFloat(Value))
        //else if pi.PropType^=TypeInfo(TTime) then
        //  SetFloatProp(obj,pi,StrToFloat(Value))
        //else
          SetFloatProp(obj,pi,strToFloat(Value));
      end else
      begin
        result:=sprNotSupported;
        //showmessage(format(LocalizeString('Msg_PropertyNotSupported'),[prop]));
      end;
    end else
    begin
      result:=sprPropNotExists;
      //if Value<>'N/A' then
      //  showmessage(format(LocalizeString('Msg_UnknownProperty'),[prop]));
    end;
  except
    result:=sprError;
    //showmessage(format(LocalizeString('Msg_ErrorSettingProperty'),[prop]));
  end;
end;

procedure AssignPropsFromNode(Node:TTreenode;Comp:TComponent);
var tn:TTreenode;
    aName,aValue:string;
begin
  tn:=Node.GetFirstChild;
  while (assigned(tn)) and (not isObject(tn)) do
  begin
    if not isItem(tn) then
    begin
      aName:=getPropertyName(tn);
      aValue:=RemoveSingleQuotes(getPropertyValue(tn));
      setProperty(comp,aName,aValue);
    end;
    tn:=tn.GetNextSibling;
  end;
end;

function GetSetValue(setInfo:pTypeInfo;valueStr:string):Integer;
var ti:pTypeInfo;
    td:pTypedata;
    s,s2:string;
    p,v:integer;
begin
  result:=0;
  ti:=nil;
  if assigned(setInfo) then
  begin
    td:=GetTypeData(setinfo);
    if assigned(td) then ti:=td.CompType^;
    if assigned(ti) then
    begin
      s:=copy(valueStr,2,length(ValueStr)-2);
      p:=pos(',',s);
      while p>0 do
      begin
        s2:=copy(s,1,p-1);
        v:=getEnumValue(ti,s2);
        if v>-1 then
          result:=result or (1 shl v);
        delete(s,1,p);
        p:=pos(',',s);
      end;
      s2:=copy(s,1,length(s));
      v:=getEnumValue(ti,s2);
      if v>-1 then
        result:=result or (1 shl v);
    end;
  end;
end;

function GetStrValue(instance:TObject;PropInfo:pPropInfo):string;
begin
  result:='';
  result:=getStrProp(instance,propinfo);
end;

function GetEnumVal(instance:TObject;PropInfo:pPropInfo):string;
var v:integer;
begin
  result:='';
  v:=getOrdProp(instance,PropInfo);
  try
    result:=getEnumName(PropInfo.PropType^,v);
  except
    result:=IntToStr(v);
  end;
end;

function GetIntVal(instance:TObject;PropInfo:pPropInfo):string;
var v:integer;
begin
  v:=getOrdProp(instance,PropInfo);
  if propinfo.PropType^.name = 'TColor' then
    result:=colortoString(v)
  else if propinfo.PropType^.name = 'TCursor' then
    result:=cursortoString(v)
  else if propinfo.PropType^.name = 'TFontCharset' then
    CharsetToIdent(v,result)
  else result:=IntToStr(v);
end;

function getClassName(instance:TObject;PropInfo:pPropInfo):string;
var i:integer;
begin
  i:=getOrdProp(instance,PropInfo);
  //showmessage(inttostr(integer(instance))+' '+PropInfo.Name+':'+inttostr(integer(PropInfo)));
  if i<>0 then //=nil
  begin
    if TObject(i) is TComponent then
      result:=TComponent(i).Name
    else
      result:=TObject(i).ClassName;
  end else
    result:='';
end;

function GetSet(instance:TObject;PropInfo:pPropinfo):string;
var ti:pTypeInfo;
    td:pTypedata;
    val,i,v:integer;
begin
  result:='';
  ti:=PropInfo^.PropType^;
  td:=GetTypeData(ti);
  if assigned(td) then ti:=td.CompType^;
  val:=GetOrdProp(instance,propinfo);
  for i:=0 to 15 do
  begin
    v:=(1 shl i);
    if (v and val)>0 then
    begin
      result:=result+','+GetEnumName(ti,i);
    end;
  end;
  if result<>'' then delete(result,1,1);
  result:='['+result+']';
end;

function GetValue(instance:TObject;Prop:string):string;
var pi:pPropInfo;
    p:integer;
begin
  Result:='';
  p:=getPropDelimiter(prop);
  if p>0 then
  begin
    instance:=getSubInstance(instance,copy(prop,1,p-1));
    delete(prop,1,p);
  end;
  pi:=GetPropInfo(findTypeInfo(instance.classname),prop);
  if assigned(pi) then
  begin
    case pi.PropType^.Kind of
      tkInteger: Result:=GetIntVal(instance,pi);
      tkFloat: Result:=FloatToStr(GetFloatProp(instance,pi));
      tkEnumeration:Result:=GetEnumVal(instance,pi);
      tkSet:Result:=GetSet(instance,pi);
      tkString,tkWString,tkLString:Result:=GetStrValue(instance,pi);
      tkClass:Result:=getClassName(instance,pi);
    end;
  end else result:='N/A';
end;

procedure SetSet(instance:TObject;Prop,Value:string);
var ti:pTypeInfo;
    pi:pPropInfo;
    val:integer;
begin
  pi:=nil;
  ti:=findTypeInfo(instance.ClassName);
  if assigned(ti) then
    pi:=getPropInfo(ti,prop);

  ti:=pi^.PropType^;
  if ti.Kind=tkSet then
  begin
    val:=getsetValue(ti,value);
    if assigned(pi) then
      setOrdProp(instance,pi,val);
  end;
end;

function getSubInstance (aRoot: TObject; aName: string): TObject;
var ii,p : Integer;
    SL   : TStringlist;
    Info : PPropInfo;
begin
  SL := TStringList.Create;
  p:=pos('.',aName);
  while p>0 do
  begin
    sl.add(copy(aname,1,p-1));
    delete(aname,1,p);
    p:=pos('.',aName);
  end;
  sl.add(aName);
  if (SL.Count = 0) then
    Result := nil
  else
  begin
    //Result := aRoot.FindComponent(SL[0]);
    result:=aroot;
    ii := 0;
    while (Assigned(Result) and (ii < SL.Count)) do
    begin
      Info := GetPropInfo(Result.classinfo, SL[ii]);
      if ((Info = nil) or (Info.PropType^.Kind <> tkClass)) then
        Result := nil
      else
      begin
        Result := TObject(GetOrdProp(Result, Info));
        Inc (ii);
      end;
    end;
  end;
  SL.Free;
end;

function getSubclass (aComp: TComponent; aPropName: string): TObject;
var ii,p : Integer;
    SL   : TStringlist;
    Info : PPropInfo;
begin
  SL := TStringList.Create;
  p:=pos('.',aPropName);
  while p>0 do
  begin
    sl.add(copy(aPropName,1,p-1));
    delete(aPropName,1,p);
    p:=pos('.',aPropName);
  end;
  sl.add(aPropName);
  if (SL.Count = 0) then
    Result := nil
  else
  begin
    Result := aComp;
    ii := 0;
    while (Assigned(Result) and (ii < SL.Count)) do
    begin
      Info := GetPropInfo(Result.classinfo, SL[ii]);
      if ((Info = nil) or (Info.PropType^.Kind <> tkClass)) then
        Result := nil
      else
      begin
        Result := TObject(GetOrdProp(Result, Info));
        Inc (ii);
      end;
    end;
  end;
  SL.Free;
end;

function getCollectionClass(classname,propname:string):string;
var ti:pTypeinfo;
    td:pTypeData;
    Comp:TComponent;
    Coll:TCollection;
    pi:PPropinfo;
begin
  result:='';
  ti:=FindTypeInfo(classname);
  if not assigned(ti) then exit;
  td:=GetTypeData(ti);
  if not assigned(td) then exit;
  pi:=GetPropInfo(ti,propname);
  if not assigned(pi) then exit;
  Comp:=TComponentClass(TD.ClassType).Create(nil);
  try
    Coll:=TCollection(GetOrdProp(Comp, pi));
    Result:=Coll.ItemClass.ClassName;
  finally
    Comp.Free;
  end;
end;

function GetClassDef(classname:string):TClass;
var ti:ptypeinfo;
    td:pTypedata;
begin
  td:=nil;
  result:=nil;
  ti:=findTypeInfo(classname);
  if assigned(ti) then
    td:=getTypeData(ti);
  if assigned(td) then
    result:=td.classType;
end;

function GetCompHierarchy(CompType:string):string;
var cl:TClass;
begin
  Result:='';
  cl:=GetClassDef(CompType);
  while assigned(cl) do
  begin
    if result<>'' then result:=result+#13#10;
    result:=result+cl.classname;
    cl:=cl.ClassParent;
  end;
end;


end.
