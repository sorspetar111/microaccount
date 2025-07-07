unit unit_u;

{*
Program: DFMEdit
Unit: unit_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Functions for unit-creation/modification

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

interface

uses ComCtrls, Classes, sysutils, Classes_u, typinfo;

function getUnitName(ComponentName:String):string;

procedure CreatePas(filename:string;lazarus:boolean;Objectlist:TStrings;Treeview:TTreeview);
procedure ModifyPas(filename:string;Objectlist:TStrings;Treeview:TTreeview);

procedure GenerateRTC(FormName,ParentName:string;Node:TTreenode;Output,ObjectList:TStrings);

implementation

uses functions_u,main_u,localize_u;

function getParams(ev:string):string;
begin
  result:='Sender: TObject';
  //enter,exit,click,change have no other params
  if (ev='Click') or (ev='Change') or (ev='Enter') or (ev='Exit') then exit;
  if (ev='DragDrop') or (ev='DragOver') then
  begin
    insert(', Source',result,7);
    result:=result+'; X,'+#13#10+'  Y: Integer';
    if ev='DragOver' then
      result:=result+'; State: TDragState; var Accept: Boolean';
  end else if (ev='KeyDown') or (ev='KeyUp') then
    result:=result+'; var Key: Word;'+#13#10+'  Shift: TShiftState'
  else if (ev='KeyPress') then
    result:=result+'; var Key: Char'
  else if (ev='MouseDown') or (ev='MouseMove') or (ev='MouseUp') then
  begin
    result:=result+'; Shift: TShiftState; X, Y: Integer';
    if ev='MouseMove' then
      insert(#13#10+'  ',result,15)
    else
      insert(#13#10+'  Button: TMouseButton;',result,15);
  end;
end;

procedure GetMethods(Treeview:TTreeview;output:TStrings);
var p:integer;
    tn:TTreenode;
    me,ev,param:string;
begin
  if not assigned(output) then exit;
  output.Clear;
  tn:=treeview.Items.GetFirstNode;
  while assigned(tn) do
  begin
    if (tn.ImageIndex=iiProperty) and (lowercase(copy(tn.text,1,2))='on') and (pos('=',tn.text)>0) then
    begin
      p:=pos('=',tn.text);
      ev:=trim(copy(tn.text,3,p-3));
      me:=trim(copy(tn.text,p+1,length(tn.text)-p));
      param:=getParams(ev);
      output.add(me+'('+param+')');
    end;
    tn:=tn.GetNext;
  end;
end;

procedure replaceComponents(src,new,methods:TStrings;formname,formclass:string);
var p,p2,i:integer;
    s,oldformclass,oldformname:string;
    found:boolean;
begin
  //methods is containing the name and parameters not the 'procedure'
  p:=0;
  while pos('class(TForm)',src.strings[p])=0 do inc(p);
  p2:=pos('=',src.strings[p]);
  oldformclass:=trim(copy(src.strings[p],1,p2-1));
  src.strings[p]:='  '+formclass+' = class(TForm)';
  inc(p);
  s:= trim(lowercase(copy(trim(src.strings[p]),1,9)));
  While not ((s='function') or (s='procedure') or (s='private') or (s='end;')) do
  begin
    s:=lowercase(trim(src.strings[p]));
    if copy(s,1,2)<>'//' then
    begin
      found:=false;
      i:=0;
      while i< new.count do
      begin
        if (s=lowercase(trim(new.strings[i]))) then
        begin
          new.delete(i);
          i:=new.count;
          found:=true;
        end else
          inc(i);
      end;
      if (not found) and (s<>'') then src.Strings[p]:='//'+src.Strings[p];
    end;
    inc(p);
    s:= trim(lowercase(copy(trim(src.strings[p]),1,9)));
  end;
  if new.count>0 then
  begin
    //inc(p);
    for i:=new.Count-1 downto 0 do
    begin
      src.Insert(p,new.strings[i]);
      inc(p);
    end;
  end;
  //insert methode-headers
  s:= trim(lowercase(copy(trim(src.strings[p]),1,9)));
  While not ((s='protected') or (s='public') or (s='private') or (s='end;')) do
  begin
    s:=trim(src.Strings[p]);
    p2:=pos(' ',s);
    s:= trim(lowercase(copy(s,p2+1,pos('(',s)-1-p2)));
    i:=0;
    while i<methods.Count do
    begin
      if s=trim(lowercase(copy(methods.strings[i],1,pos('(',methods.Strings[i])-1))) then
      begin
        if pos(')',methods.strings[i])=0 then
        begin
          methods.Delete(i+1);
        end;
        methods.Delete(i);
      end else inc(i);
    end;
    inc(p);
    s:= trim(lowercase(copy(trim(src.strings[p]),1,9)));
  end;
  if methods.count>0 then
  begin
    for i:=0 to methods.Count-1 do
    begin
      s:=methods.strings[i];
      if pos('(',s)>0 then s:='procedure '+s;
      src.Insert(p,'    '+s+';');
      inc(p);
    end;
  end;
  while pos(': '+oldformclass+';',src.strings[p])=0 do inc(p);
  oldformname:=copy(src.strings[p],1,pos(':',src.strings[p])-1);
  //renaming form
  src.delete(p);
  src.Insert(p,'  '+FormName+': '+FormClass+';');
  //adding new methodes
  while (p<src.count) and (pos('end.',src.strings[p])=0) and
        (pos('initialization',src.strings[p])=0) and
        (pos('finalization',src.strings[p])=0) do inc(p);
  if methods.count>0 then
  begin
    for i:=0 to methods.Count-1 do
    begin
      s:=methods.strings[i];
      if pos('(',s)>0 then s:='procedure '+FormClass+'.'+s+';';
      src.Insert(p{src.Count-1},s);
      if pos(')',s)>0 then
      begin
        src.Insert(p+1{src.Count-1},'begin');
        src.Insert(p+2{src.Count-1},'//');
        src.Insert(p+3{src.Count-1},'end;');
        src.Insert(p+4{src.Count-1},'');
      end;
    end;
  end;
  if lowercase(formclass)<>lowercase(oldformclass) then
  begin
    //klasse ändern von proceduren
    src.text:=replaceall(src.text,oldformclass+'.',formclass+'.');
  end;
end;

procedure replaceUses(Src,new:TStrings);
var uses_SL:TStringlist;
    i,p,l:integer;
    s,s2:string;
const
  indent=2;
begin
  l:=0;
  p:=src.IndexOf('uses')+1;
  uses_SL:=TStringlist.create;
  //import existing uses
  s2:='';
  while src.Strings[p]<>'' do
  begin
    s:=trim(src.Strings[p]);
    //parsing s and adding to uses_sl
    if pos(';',s)>0 then
      delete(s,pos(';',s),1);
    s2:=s2+s;
    src.Delete(p);
  end;
  uses_sl.CommaText:=s2;
  for i:=0 to new.Count-1 do
    if uses_sl.IndexOf(new[i])=-1 then uses_sl.add(new[i]);
  s:=stringofChar(' ',indent);
  for i:=0 to uses_sl.count-1 do
  begin
    if i>0 then s:=s+',';
    if length(s)-l>70 then
    begin
      s:=s+#13#10+StringOfChar(' ',indent);
      l:=length(s)-indent;
    end;
    s:=s+uses_sl.strings[i];
  end;
  uses_sl.free;
  src.Insert(p,s+';');
end;

function getUnitName(ComponentName:String):string;
var
  dummy:PTypeInfo;
  d2:PTypeData;
  classe:TpersistentClass;
begin
  result:='';
  classe:=getclass(ComponentName);
  if assigned(classe) then
  begin
    dummy:=classe.ClassInfo;
    d2:=gettypedata(dummy);
    result:=d2^.Unitname;
  end;
end;

procedure CreatePas(filename:string;lazarus:boolean;Objectlist:TStrings;Treeview:TTreeview);
var sl,uses_sl,methods_sl:TStringlist;
    basename,formname,formclass,cl,s:string;
    i,j,p:integer;
const
  unit_header:array[0..6] of string=('unit %s;','','interface','','uses','','Type');
  main_units:array[0..4] of string=('Windows','Messages','SysUtils','Classes','Forms');
  middle_template:array[0..9] of string=('  private','','  public','','  end;',
    '','var','  %s: %s;','','implementation');
  method_template:array[0..3] of string=('begin','//','end;','');
begin
    //bei Lazarus-unit folgendes zusätzlich:
    //{$mode objfpc}{$H+} nach unit *;
    //LResources mit in der uses
    //initialization
    //  {$I unit1.lrs}
    //nach dem initialization-teil
    //kein $R *.dfm
  basename:=ExtractBaseName(extractfilename(filename));
  if CheckID(basename) then
  begin
    createBackup(filename);
    formname:=GetFormName;
    formclass:=GetFormClass;

    uses_sl:=TStringlist.Create;
    for i:=low(main_units) to high(Main_units) do
      uses_sl.Add(Main_Units[i]);
    if lazarus then
      uses_sl.Add('LResources');

    sl:=TStringlist.create;

    for i:=low(unit_header) to high(unit_header) do
      if pos('%s',unit_header[i])>0 then
        sl.Add(format(unit_header[i],[basename]))
      else
        sl.add(unit_header[i]);

    if lazarus then
      sl.Insert(2,'{$mode objfpc}{$H+}');
      
    methods_sl:=TStringlist.create;
    getMethods(treeview,methods_sl);

    //class definition
    sl.add('  '+formclass+' = class(TForm)');
    for i:=0 to Objectlist.count-1 do
    begin
      if copy(Objectlist.Strings[i],1,pos(':',Objectlist.Strings[i])-1)<>formname then
      begin
        p:=pos(':',Objectlist.Strings[i]);
        cl:=trim(copy(Objectlist.Strings[i],p+1,length(Objectlist.Strings[i])-p));
        sl.Add('    '+Objectlist.Strings[i]+';');
        cl:=getUnitName(cl);
        if (cl<>'') and (uses_sl.IndexOf(cl)=-1) then
          uses_sl.Add(cl);
      end;
    end;

    for i:=0 to methods_sl.Count-1 do
    begin
      s:='procedure '+methods_sl.strings[i];
      p:=pos(#13,s);
      if p>0 then
        insert('    ',s,p+2);
      sl.add('    '+s);
    end;
    for i:=low(middle_template) to high(middle_template) do
      if pos('%s',middle_template[i])>0 then
        sl.add(format(middle_template[i],[formname,formclass]))
      else
        sl.add(middle_template[i]);
    if not lazarus then
    begin
      sl.add('{$R *.DFM}');
      sl.add('');
    end;

    for i:=0 to methods_sl.Count-1 do
    begin
      sl.add(format('procedure %s.%s',[formclass,methods_sl.strings[i]]));
      for j:=low(method_template) to high(method_template) do
        sl.add(method_template[j]);
    end;
    sl.add('');
    if lazarus then
    begin
      sl.add('initialization');
      sl.add('  {$I '+basename+'.lrs}');
    end;
    sl.add('end.');

    s:='';
    p:=0;
    for i:=0 to uses_sl.Count-1 do
    begin
      if i>0 then s:=s+',';
      if length(s)-p > 70 then
      begin
        s:=s+#13#10+'  ';
        p:=length(s)-2;
      end;
      s:=s+uses_sl.Strings[i];
    end;
    sl.Insert(sl.IndexOf('uses')+1,'  '+s+';');
    sl.SaveToFile(filename);
    methods_sl.Free;
    uses_sl.free;
    sl.free;
  end;
end;

//modifypas
procedure ModifyPas(filename:string;Objectlist:TStrings;Treeview:TTreeview);
var file_sl,object_sl,uses_sl,methods_sl:TStringlist;
    basename,formname,formclass,cl:string;
    i,p:integer;
const
  main_units:array[0..4] of string=('Windows','Messages','SysUtils','Classes','Forms');
  method_template:array[0..3] of string=('begin','//','end;','');
begin
  basename:=ExtractBaseName(extractfilename(filename));
  if CheckID(basename) then
  begin
    createBackup(filename);
    formname:=GetFormName;
    formclass:=GetFormClass;

    file_sl:=TStringlist.create;
    object_sl:=TStringlist.create;
    uses_sl:=TStringlist.Create;
    methods_sl:=TStringlist.create;

    file_sl.LoadFromFile(filename);

    //uses laden
    for i:=0 to Objectlist.count-1 do
    begin
      if trim(copy(Objectlist.Strings[i],1,pos(':',Objectlist.Strings[i])-1))<>formname then
      begin
        p:=pos(':',Objectlist.Strings[i]);
        cl:=trim(copy(Objectlist.Strings[i],p+1,length(Objectlist.Strings[i])-p));
        object_sl.Add('    '+Objectlist.Strings[i]+';');
        cl:=getUnitName(cl);
        if (cl<>'') and (uses_sl.IndexOf(cl)=-1) then
          uses_sl.Add(cl);
      end;
    end;
    //form_dfmmain.memo_output.Lines.Assign(object_sl);
    replaceuses(file_sl,uses_sl);

    getMethods(treeview,methods_sl);

    replaceComponents(file_sl,object_sl,methods_sl,formname,formclass);

    file_sl.SaveToFile(filename);
    //form_dfmMain.memo_source.lines.assign(file_sl);

    methods_sl.Free;
    uses_sl.free;
    object_sl.Free;
    file_sl.free;
  end;
end;

//========================== generating runtime-code ==========================
function getclasspos(objectlist:TStrings;objClass:String):integer;
var i,p:integer;
    s:string;
begin
  result:=-1;
  objclass:=lowercase(objclass);
  i:=0;
  while (i<objectlist.Count) and (result=-1) do
  begin
    p:=pos(':',objectlist.strings[i]);
    s:=lowercase(trim(copy(objectlist.strings[i],p+1,length(objectlist.strings[i])-p-1)));
    if s=objclass then result:=i;
    inc(i);
  end;
end;

procedure addToObjectList(objectlist:TStrings;objvar,objClass:String);
var ip,p:integer;
    s:string;
begin
  ip:=getclasspos(objectlist,objclass);
  if ip=-1 then
    objectlist.Add(objVar+': '+ObjClass+';')
  else
  begin
    s:=objectlist.strings[ip];
    p:=pos(':',s);
    insert(', '+objvar,s,p);
    objectlist.strings[ip]:=s;
  end;
end;

procedure CollectionRTC(ObjName:string;Node:TTreenode;Output:TStrings);
var i:integer;
    itemnode,subnode:TTreenode;
    propName:string;
const
  withtemplate='with %s.%s.add do //%d'#13#10'begin';
begin
  if not assigned(node) then exit;
  propName:=GetPropertyName(node);
  i:=0;
  itemNode:=node.GetFirstChild;
  while assigned(itemNode) do
  begin
    Output.Add(format(withtemplate,[ObjName,PropName,i]));
    subnode:=ItemNode.GetFirstChild;
    while assigned(subNode) do
    begin
      Output.add('  '+GetPropertyName(subnode)+' := '+GetPropertyValue(subnode)+';');
      subNode:=subNode.GetNextSibling;
    end;
    Output.add('end;');
    itemNode:=itemNode.GetNextSibling;
    inc(i);
  end;
end;

procedure StringlistRTC(ObjName:string;Node:TTreenode;Output:TStrings);
var i,p:integer;
    propName:string;
    s:string;
    sl:TStringlist;
begin
  if not assigned(node) then exit;
  propName:=GetPropertyName(node);
  //now replacing .Strings with .Text
  p:=pos('.',Propname);
  if p>0 then
    PropName:=Copy(Propname,1,p)+'Text';
  Output.Add(ObjName+'.'+propName+' := ');
  sl:=TStringlist.Create;
  sl.Text:=TDataRec(node.Data).Value;
  s:=''''+sl.strings[0]+'''';
  for i:=1 to sl.count-1 do
  begin
    Output.Add('  '+s+'#13#10+');
    s:=''''+sl.strings[i]+'''';
  end;
  Output.Add('  '+s+';');
  sl.free;
end;
                                                       
procedure GenerateRTC(FormName,ParentName:string;Node:TTreenode;Output,ObjectList:TStrings);
var tn:TTreenode;
    ObjVar,ObjClass,addprop,pname:string;
    ignoreBounds,invisible:boolean;
    tn_left,tn_top,tn_width,tn_height:TTreenode;
const
  constr='%s := %s.Create(%s);';
  constrForm='%s := %s.CreateNew(Application);';
begin
  addprop:='';
  if not assigned(node) then exit;
  ObjVar:=getobjectName(Node);
  objclass:=getObjectClass(Node);
  invisible:=false;
  if (Parentname='') and assigned(Node.Parent) then
    parentname:=getObjectName(Node.Parent);
  if node.AbsoluteIndex=0 then
  begin
    formName:=ObjVar;
    Output.Add(format(constrForm,[ObjVar,ObjClass]));
  end else
  begin
    Output.Add(format(constr,[ObjVar,ObjClass,FormName]));
    invisible:=((node.level=1) and not isControl(ObjClass,node));
    if (lowercase(ObjClass)='tmenuitem') then
    begin
      if lowercase(getObjectClass(node.Parent))='tmenuitem' then
        addprop:=(getObjectName(node.Parent)+'.Add('+ObjVar+');')
      else
        addprop:=(getObjectName(node.Parent)+'.Items.Add('+ObjVar+');');
    end else
    begin
      if not (invisible) then
        Output.Add(ObjVar+'.Parent := '+ParentName+';');
      if lowercase(ObjClass)='ttabsheet' then
        Output.Add(ObjVar+'.PageControl := '+ParentName+';');
    end;
  end;
  AddToObjectlist(Objectlist,ObjVar,ObjClass);
  tn_left:=getPropertyNode(node,'left');
  tn_top:=getPropertyNode(node,'top');
  tn_width:=getPropertyNode(node,'width');
  tn_height:=getPropertyNode(node,'height');

  ignoreBounds:=assigned(tn_left) and assigned(tn_top) and assigned(tn_width) and assigned(tn_height);
  if ignorebounds then
    Output.Add(ObjVar+'.SetBounds('+GetPropertyValue(tn_Left)+','+GetPropertyValue(tn_Top)+','+GetPropertyValue(tn_Width)+','+GetPropertyValue(tn_Height)+');');
  tn:=node.GetFirstChild;
  while assigned(tn) do
  begin
    if isObject(tn) then
    begin
      if addprop<>'' then
      begin
        Output.Add(addprop);
        addprop:='';
      end;
      Output.Add('');
      GenerateRTC(FormName,ObjVar,tn,Output,ObjectList);
    end else
    begin
      if assigned(tn.data) then
      begin
        case TDataRec(tn.Data).Kind of
          dkBinary:;
          dkStrings:StringlistRTC(ObjVar,tn,Output);
          dkCollection:CollectionRTC(ObjVar,tn,Output);
          dkIntegerList:;
        end;
      end else
      begin
        pName:=lowercase(getPropertyName(tn));
        if pName='itemindex' then
          addprop:=ObjVar+'.'+getPropertyName(tn)+' := '+getPropertyValue(tn)+';'
        else if not ((ignorebounds or invisible) and ((pName='left') or (pName='top') or (pName='width') or (pName='height'))) then
          Output.Add(ObjVar+'.'+getPropertyName(tn)+' := '+getPropertyValue(tn)+';');
      end;
    end;
    tn:=tn.GetNextSibling;
  end;
  if addprop<>'' then
    Output.Add(addprop);
end;

end.
