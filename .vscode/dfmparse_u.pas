unit dfmparse_u;

{*
Program: DFMEdit
Unit: dfmparse_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Functions for dfm-parsing

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, typinfo, ExtCtrls, ComCtrls, inifiles,

  Classes_u;

procedure LoadDFM(TV:TTreeview;SourceSL,ObjectList:TStrings;callback:TCallbackEvent);
function Generate(treeview:TTreeview;usage:byte;{output:TStrings;}ShowHidden,EnableControls:Boolean;callback:TCallbackEvent):string;

function LoadDFMFile(filename:string;DestSL:TStrings):boolean;
procedure SaveDFMFile(filename:string;SourceSL:TStrings;Binary:boolean);

procedure AssignDFMToForm(SourceSL:TStrings);
function GetImageDFMData(image:TImage;PictureHeader:Boolean):string;
function AssignDFMToImage(image:TImage;data:string):boolean;
function GetImageListDFMData(imagelist:TImageList):string;
function AssignDFMToImageList(imagelist:TImagelist;data:string):boolean;

function RemoveUnwanted(Treeview:TTreeview):integer;

implementation

uses main_u, preview_u, functions_u, rttifunctions_u,localize_u,convert_u;

//laden
function RemoveLineBreaks(const S: string;RemoveSpaces:boolean): string;
var
  I: Integer;
  Ch: Char;
const
  LB=[#13, #10];
  WS=[#9,#32];
begin
  Result:= '';
  for I:= 1 to Length(S) do
  begin
    Ch:= S[I];
    if not (Ch in LB) then
    begin
      if (not RemoveSpaces) or (not (Ch in WS)) then
        Result:= Result + CH;
    end;
  end;
end;

function AddLineBreaks(srcstring:string;Indent:integer):string;
var sl:TStringlist;
    s:string;
begin
  sl:=Tstringlist.create;
  s:=removeLineBreaks(srcstring,true);
  while length(s)>0 do
  begin
    sl.Add(StringOfChar(' ',Indent)+copy(s,1,64));
    delete(s,1,64);
  end;
  result:=sl.Text;
  sl.free;
end;

function HexToStr(HexValue:String):String;
var i:integer;
    s:string;
begin
  result:='';
  for i:=0 to (length(HexValue) div 2)-1 do
  begin
    s:=copy(HexValue,i*2+1,2);
    result:=result+Chr(StrToInt('$'+s));
  end;
end;

function DecodeSpecialChars(ins:string;DeleteQuotes:boolean):string;
var
  i,st,en:integer;
  txt:Boolean;
  s:string;
begin
  i:=1;
  txt:=false;
  s:='';
  while i<=length(ins) do
  begin
    if ins[i]='''' then
    begin
      txt:=not txt;
      inc(i);
    end else
    begin
      if not txt then
      begin
        while (ins[i]='#') do
        //while (i<length(ins)) and (ins[i]='#') do
        begin
          inc(i); //# überspringen
          st:=i; //start für code
          while (i<=Length(ins)) and not (ins[i] in ['#','''','+']) do //bis #,' oder +
            inc(i);
          en:=i;
          s:=s+Chr(StrToInt(Trim(copy(ins,st,en-st))));
        end;
        while (i<length(ins)) and (ins[i] in ['+',' ']) do inc(i);
      end else
      begin
        s:=s+ins[i];
        inc(i);
      end;
    end;
  end;
  if not DeleteQuotes then
    s:=''''+s+'''';
  result:=s;
end;

function ParseProperty(prop:string):string;
var pName,pValue:string;
begin
  pName:=GetPropertyNameTxt(prop);
  pValue:=GetPropertyValueTxt(prop);
  if (copy(pValue,1,1)='''') or (copy(pValue,1,1)='#') then
    pValue:=DecodeSpecialChars(pValue,false);
  result:=pName+' = '+pValue;
end;

function ParseStrings(ins:string):string;
var s,s2:string;
    sl:TStringlist;
    i:integer;
begin
  sl:=TStringlist.create;
  sl.text:=trim(ins);
  i:=0;
  while i<sl.count do
  begin
    s:=trim(sl.strings[i]);
    s2:='';
    if s<>'' then
    begin
      while copy(s,length(s),1)='+' do
      begin
        if s2<>'' then
          sl.Delete(i)
        else
        begin
          inc(i);
        end;
        s2:=s2+s;
        s:=trim(sl.strings[i]);
      end;
      if s2<>'' then
      begin
        sl.Delete(i);
        dec(i);
        s:=s2+s;
      end;
      s:=DecodeSpecialChars(s,true);
      //s:=RemoveSingleQuotes(s);
      sl.strings[i]:=s;
      inc(i);
    end else sl.Delete(i);
  end;
  result:=sl.text;
  sl.free;
end;

procedure ParseBinary(TV:TTreeview;node:TTreenode;src:TStringlist;var line:integer);
var s,addstring,dataname:string;
      tn:TTreenode;
      r:TDataRec;
      l,p,ln:integer;
begin
  s:=trim(src.strings[line]);
  l:=length(s);
  p:=pos('{',s);
  ln:=line;
  addstring:=copy(s,p+1,l-p);
  tn:=TV.Items.AddChild(node,copy(s,1,p));
  while (line<src.count-1) and (copy(s,length(s),1)<>'}') do
  begin
    inc(line);
    s:=trim(src.strings[line]);
    addstring:=addstring+s;
  end;
  delete(addstring,length(addstring),1);//klammer am ende löschen
  try
    dataname:=HexToStr(copy(addstring,3,strtoint('$'+copy(addstring,1,2))*2));
  except
    showmessage(LocalizeString('Msg_ConvertError')+' @'+LocalizeString('Cap_Line')+' ['+IntToStr(ln+1)+'..'+IntToStr(line+1)+']');
  end;
  if (DataName<>'') and (DataName[1]<>'T') then DataName:='';
  tn.Text:=tn.Text+DataName+'}';
  r:=TDataRec.create;
  r.kind:=dkBinary;
  r.Value:=addstring;
  tn.data:=r;
  tn.ImageIndex:=iiProperty;
  tn.SelectedIndex:=iiPropertySel;
end;

procedure ParseStringList(TV:TTreeview;node:TTreenode;src:TStringlist;var line:integer);
var s,addstring:string;
      tn:TTreenode;
      r:TDataRec;
    stringlist:boolean;
begin
  s:=trim(src.strings[line]);
  addstring:='';
  tn:=TV.Items.AddChild(node,s);
  while (line<src.count-1) and (copy(s,length(s),1)<>')') do
  begin
    inc(line);
    s:=trim(src.strings[line]);
    if length(addstring)>0 then
      addstring:=addstring+#13+#10;
    if copy(s,length(s),1)<>')' then
      addstring:=addstring+s
    else
      addstring:=addstring+copy(s,1,length(s)-1);
  end;
  stringlist:=(copy(addstring,1,1)='''') or (copy(addstring,1,1)='#');
  if stringlist then
    addstring:=ParseStrings(addstring);
  r:=TDataRec.create;
  if stringlist then
  begin
    r.kind:=dkStrings;
    tn.Text:=tn.Text+'TStrings'+')';
  end else
  begin
    r.kind:=dkIntegerList;
    tn.Text:=tn.Text+'IntList'+')';
  end;
  r.Value:=addstring;
  tn.data:=r;
  tn.ImageIndex:=iiProperty;
  tn.SelectedIndex:=iiPropertySel;
end;

procedure ParseCollection(TV:TTreeview;node:TTreenode;src:TStringlist;var line:integer);
var s,addstring:string;
    tn_root,tn_item,tn_prop:TTreenode;
    r:TDataRec;
    p:integer;
begin
  s:=trim(src.strings[line]);
  addstring:='';
  p:=pos('<',s);
  tn_root:=TV.Items.AddChild(node,copy(s,1,p)+'>');
  tn_root.ImageIndex:=iiProperty;
  tn_root.SelectedIndex:=iiPropertySel;
  r:=TDataRec.create;
  r.kind:=dkCollection;
  r.Value:='';
  tn_root.data:=r;

  tn_item:=tn_root;
  while (line<src.count-1) and (pos('>',s)=0) do
  begin
    inc(line);
    s:=trim(src.strings[line]);
    p:=pos('>',s);
    if p=0 then
      p:=length(s)+1;
    if s='item' then
    begin
      tn_item:=TV.Items.AddChild(tn_root,s);
      tn_item.ImageIndex:=iiItem;
      tn_item.SelectedIndex:=iiItemSel;
    end else if copy(s,1,p-1)='end' then
      tn_item:=tn_root
    else
    begin
      p:=pos('=',s);
      //if copy(s,length(s),1)='{' then
      if isNextChar(Pchar(s),'{',p,[' ',#9]) then
        ParseBinary(TV,tn_item,src,line)
      //else if copy(s,length(s),1)='(' then
      else if isNextChar(Pchar(s),'(',p,[' ',#9]) then
        ParseStringList(TV,tn_item,src,line)
      //else if copy(s,length(s),1)='<' then
      else if isNextChar(Pchar(s),'<',p,[' ',#9]) then
        ParseCollection(TV,tn_item,src,line)
      else
      begin
        tn_prop:=TV.Items.AddChild(tn_item,ParseProperty(s));
        tn_prop.ImageIndex:=iiProperty;
        tn_prop.SelectedIndex:=iiPropertySel;
      end;
    end;
    //inc(line);
    //s:=trim(src.strings[line]);
  end;
end;

//functions for output
function breakBinaryData(s:string):string;
var i,c:integer;
const block=64;
begin
  c:=length(s) div block;
  result:='';
  for i:=0 to c do
  begin
    if result<>'' then
      result:=result+#13#10;
    result:=result+copy(s,i*block+1,block);
  end;
end;

function EncodeSpecialChars(ins:string):string;
var s,value:string;
    i,c,lc:integer;
begin
  lc:=0;
  value:=ins;
  if isLastChar(PChar(value),'''') then
  begin
    for i:=2 to length(value)-1 do
    begin
      c:=ord(value[i]);
      if not ((c<32) or (c>126) or (c=39)) then c:=-1;
      if (c>-1) {and (i>p) }and (i<length(value)) then
      begin
        //sonderzeichen zwischen den Stringbegrenzern
        s:=s+'''#'+IntToStr(c)+'''';
      end else s:=s+value[i];
      if ((length(s)-lc)>64) and (i<length(value)-1) then
      begin
        s:=s+''' +'+#13#10;
        lc:=length(s);
        s:=s+{StringOfChar(' ',indent*2+2)+}'''';
      end;
    end;
    {if length(s)>2 then
      while pos('''''',s)>0 do delete(s,pos('''''',s),2);  }
    result:=''''+s+'''';
  end else result:=ins;
end;

function OutputBinary(tn:TTreenode{;SLOut:TStrings}):string;
var s:string;
    c:integer;
begin
  //slout.Add
  result:='{'{(stringofchar(' ',2*tn.level)+copy(tn.text,1,pos('{',tn.text)))}+#13#10;
  s:=TDataRec(tn.Data).Value;
  while (length(s)>0) do
  begin
    if length(s)>64 then c:=64 else c:=length(s);
    //slout.Add
    result:=result+(stringofchar(' ',2*(tn.level+1))+copy(s,1,c))+#13#10;
    Delete(s,1,c);
  end;
  //slout.Add
  result:=result+(stringofchar(' ',2*tn.level)+'}')+#13#10;
end;

function OutputStringList(tn:TTreenode{;SLOut:TStrings}):string;
var sl:TStringlist;
    j:integer;
    s:string;
begin
  //slout.Add
  //result:=copy(tn.text,1,pos('(',tn.text)))+#13#10;
  result:='('+#13#10;
  sl:=TStringlist.create;
  sl.text:=TDataRec(tn.Data).Value;
  j:=0;
  while j<sl.count do
  begin
    s:=sl.strings[j];
    if TDataRec(tn.Data).Kind=dkStrings then
      //slout.add
      result:=result+indentLine(EncodeSpecialChars(''''+s+''''),tn.level+1)+#13#10
    else
      //slout.add
      result:=result+(stringOfChar(' ',(tn.level+1)*2)+s)+#13#10;
    inc(j);
  end;
  sl.Free;
  //slout.Add
  result:=result+(StringOfChar(' ',tn.level*2)+')')+#13#10;
end;

function OutputCollection(tn:TTreenode;usage:byte):string;
var tn_item,tn_prop:TTreenode;
    c1:TClass;
    propertyExists:boolean;
    cname:string;
    sl:TStringlist;
begin
  //slOut.Add
  result:='<'{(stringofchar(' ',tn.level*2)+copy(tn.Text,1,length(tn.text)-1))}+#13#10;
  tn_item:=tn.GetNext;
  cname:='';
  sl:=TStringlist.create;
  while assigned(tn_item) and (tn_item.Level>tn.level) do
  begin
    {slout.add}
    result:=result+(StringOfChar(' ',tn_item.level*2)+tn_item.Text)+#13#10;
    tn_prop:=tn_Item.GetFirstChild;
    while assigned(tn_prop) and (tn_prop.Level>tn_item.level) do
    begin
      if (cname='') then
      begin
        cname:=GetObjectClass(GetObjectFromNode(tn));
        C1:=GetClassDef(getCollectionClass(cname,GetPropertyName(tn)));
        if assigned(C1) then
          GetcomponentProps(c1.classname,sl,false);
      end;
      //ggf. lowercase implementieren
      propertyExists:=(sl.indexof(GetPropertyName(tn_prop))>=0);
      if propertyExists or (usage=usg_save) then
      begin
        if assigned(tn_prop.Data) then
        begin
          result:=result+stringofchar(' ',2*tn_prop.level)+GetPropertyName(tn_prop)+' = ';
          case TDataRec(tn_prop.Data).Kind of
            dkCollection: result:=result+OutputCollection(tn_Prop,{slout,}usage)+#13#10;
            dkStrings,dkIntegerlist: result:=result+OutputStringList(tn_Prop{,slout})+#13#10;
            dkBinary: result:=result+OutputBinary(tn_Prop{,slout})+#13#10;
          end;
        end else {slout.add}result:=result+IndentLine(GetPropertyName(tn_prop)+' = '+encodeSpecialChars(GetPropertyValue(tn_prop)),tn_prop.level)+#13#10;
      end;
      tn_Prop:=tn_Prop.GetNextSibling;
    end;
    //slout.add
    result:=result+(StringOfChar(' ',tn_item.level*2)+'end')+#13#10;
    tn_item:=tn_item.GetNextSibling;
  end;
  freeSLObjects(sl);//getcomponentprops
  sl.free;
  //hier noch > ausgeben
  //slout.Add
  result:=result+(StringOfChar(' ',tn.level*2)+'>')+#13#10;
end;

function ChangePropValue(prop,Value:string):string;
var s:string;
begin
  //change property-value for TUnknownControl (changed align-Value)
  if prop='Align' then
  begin
    s:=lowercase(value);
    if pos('top',s)>0 then result:='alTop' else
    if pos('left',s)>0 then result:='alLeft' else
    if pos('right',s)>0 then result:='alRight' else
    if pos('bottom',s)>0 then result:='alBottom' else
    if pos('client',s)>0 then result:='alClient' else
    result:='alNone';
  end else
    result:=Value;
  Result:=prop+' = '+Result;
end;

function AddUnknown(var tn:TTreenode;s:string;{output:TStrings;}ShowHidden,EnableControls:Boolean):string;
var p,j:integer;
    proptn,tn2:ttreenode;
    cname:string;
const
  Props:array[1..7] of string =('Width','Height','Left','Top','Align','Visible','Enabled');
begin
  p:=pos(':',s);
  cName:=trim(copy(s,p+1,length(s)-p));
  {output.add}result:=(stringofchar(' ',2*(tn.level))+copy(s,1,p)+'TUnknownControl')+#13#10;
  for j:=low(Props) to High(props) do
  begin
    proptn:=getPropertyNode(tn,Props[j]);
    if assigned(Proptn) then
    begin
      if (props[j]='Visible') and ShowHidden then
        //output.add(stringofchar(' ',2*(tn.level+1))+'Visible = True')
        result:=result+(stringofchar(' ',2*(tn.level+1))+'Visible = True')+#13#10
      else if (props[j]='Enabled') and EnableControls then
        //output.add(stringofchar(' ',2*(tn.level+1))+'Enabled = True')
        result:=result+(stringofchar(' ',2*(tn.level+1))+'Enabled = True')+#13#10
      else
        //output.add(stringofchar(' ',2*(tn.level+1))+PropTn.Text);
        result:=result+(stringofchar(' ',2*(tn.level+1))+ChangePropValue(GetPropertyName(PropTn),GetPropertyValue(PropTn)))+#13#10;
    end;
  end;
  //output.add(stringofchar(' ',2*(tn.level+1))+'Text = '''+LocalizeString('Cap_Unknown')+':'+cName+'''');
  result:=result+(stringofchar(' ',2*(tn.level+1))+'Text = '''+LocalizeString('Cap_Unknown')+':'+cName+'''')+#13#10;
  //skip all properties of Control
  repeat
    tn2:=tn;
    tn:=tn.getNext;
  until (not assigned(tn)) or (isObject(tn));
  if assigned(tn) then tn:=tn.getPrev else tn:=tn2;
end;

//import/export
procedure LoadDFM(TV:TTreeview;SourceSL,ObjectList:TStrings;callback:TCallbackEvent);
var DFMText:TStringlist;
    i,p,c,pStep,pNext:integer;
    tn,tn_prop:TTreeNode;
    s:string;
begin
  //showmessage(filename);
  TV.items.clear;
  ObjectList.Clear;
  DFMText:=Tstringlist.Create;
  try
    dfmtext.Assign(SourceSL);
    //import to treeview
    tn:=nil;
    i:=0;
    c:=SourceSL.Count;
    pStep:=c div 100;
    pNext:=pStep;
    while i<DFMText.count do
    begin
      s:=trim(DFMText.strings[i]);
      if (assigned(callback)) and (i>pNext) then
      begin
        callback((100*i) div c,'');
        inc(pNext,pStep);
      end;
      {$IFDEF DEBUGSYMBOLS}
      outputDebugString(PCHAR('processing Line '+IntToStr(i+1)));
      {$ENDIF}
      if length(s)>0 then
      begin
        while (copy(s,length(s),1)='=') or (copy(s,length(s),1)='+') do
        begin
          inc(i);
          s:=s+' '+trim(DFMText.strings[i]);
        end;
        p:=pos('=',s);
        if isNextChar(Pchar(s),'(',p,[' ',#9]) then//(s[length(s)]='(') then
        begin
          ParseStringList(TV,tn,DFMText,i);
        end else if isNextChar(Pchar(s),'<',p,[' ',#9]) then//if (s[length(s)]='<') then
        begin
          ParseCollection(TV,tn,DFMText,i);
        end else if isNextChar(Pchar(s),'{',p,[' ',#9]) then//if (s[length(s)]='{')} then
        begin
          ParseBinary(TV,tn,DFMText,i);
        end else
        begin
          if (s='end') then
          begin
            if tn.Level>0 then tn:=tn.Parent;
          end else
          begin
            p:=pos('object',s);
            if p=0 then
              p:=pos('inline',s);
            if p=0 then           //added 20 04 2006
              p:=pos('inherited',s);  //added 20 04 2006

            if (p=1) then
            begin
              ObjectList.Add(copy(s,8,length(s)-7));
              if not assigned(tn) then
                tn:=TV.Items.Add(nil,s)
              else
                tn:=TV.Items.AddChild(tn,s);
              tn.ImageIndex:=iiObject;
              tn.SelectedIndex:=iiObjectSel;
            end else
            begin
              //zeichencodierung auflösen (#)
              s:=ParseProperty(s);
              //tn_prop:=AddPropertyNode(tn,GetPropertyNameTxt(s),GetPropertyValueTxt(s));
              //tn_prop.Text:=ParseProperty(s);
              tn_prop:=GetPropertyNode(tn,GetPropertyNameTxt(s));
              if assigned(tn_prop) then
                tn_prop.Text:=s
              else
                tn_prop:=TV.Items.AddChild(tn,s);
              tn_prop.ImageIndex:=iiProperty;
              tn_prop.SelectedIndex:=iiPropertySel;
            end;
          end;
        end;
      end;
      inc(i);
    end;
    if assigned(tn) then
    begin
      tn.Expand(false);
      if tn.HasChildren then
        tn.Item[0].Expand(false);
    end;
  finally
    if (assigned(callback)) then
      callback(100,'');
    DFMText.Free;
  end;
end;

function Generate(treeview:TTreeview;usage:byte;{output:TStrings;}ShowHidden,EnableControls:Boolean;callback:TCallbackEvent):string;
  //usage 0=for saving, 1=form preview (change unknown classes)
var i,j,oldlevel,p,pStep,pNext,C:integer;
    tn,tn2:TTreenode;
    ignorecontrol,propertyExists:boolean;
    s,cName,prop,value,temp:string;
    CC: TComponentClass;
    Comp: TComponent;
    C1:TClass;
    ignoreSl,replacePropsSl:TStringlist;
begin
  result:='';
  ShowHidden:=ShowHidden and (usage=usg_Preview);
  EnableControls:=EnableControls and (usage=usg_Preview);
  //output.Clear;
  tn:=treeview.Items.GetFirstNode;
  tn2:=tn;
  oldlevel:=0;
  ignorecontrol:=false;
  comp:=nil;
  ignoreSl:=TStringlist.Create;
  replacePropsSl:=TStringlist.Create;
  c:=Treeview.Items.Count;
  pStep:=c div 100;
  pNext:=pStep;
  while assigned(tn) do
  begin
    s:=trim(tn.text);
    outputDebugString(PCHAR('processing "'+s+'"'));
    if isObject(tn) then
    begin
      if (usage=usg_preview) then
      //wenn ausgabe für preview, dann unbekannte controls/eigenschaften anpassen
      begin
        ignoreSl.Clear;
        if isFirstChar(pchar(s),'i') then
        //copy(s,1,1)='i' then
        begin
          p:=pos(' ',s);
          //if copy(s,1,6)='inline' then
          s:='object'+copy(s,p,length(s)-p+1);
          //if copy(s,1,9)='inherited' then
          //  s:='object'+copy(s,10,length(s)-9);
        end;
        //Klasse für Formular auf PreviewForm setzen
        p:=pos(':',s);
        if tn.level=0 then
        begin
          //output.Add
          result:=result+(copy(s,1,p)+'TForm_DFMPreview')+#13#10;
          cName:='TForm';
        end else cName:=trim(copy(s,p+1,length(s)-p));
        GetReplacementProperties(CName,replacePropsSl);
        C1:=GetClass(cName);
        if not assigned(C1) then //look for replacement in property-file if current type not found
        begin
          cName:=GetReplacement(cName,ignoreSl);
          C1:=GetClass(cName);
        end;
        if c1=nil then
        begin
          if (getPropertyNode(tn,'Width')<>nil) then//visible components have a width-property
          begin
            ignorecontrol:=true;
            //output.text:=Output.text+
            result:=result+AddUnknown(tn,s,{output,}ShowHidden,EnableControls);
            oldlevel:=tn.level;
            tn2:=tn;
          end else  //invisible controls, no subcontrols
          begin
            ignorecontrol:=true;
            tn2:=tn;
            while tn.haschildren do tn:=(tn.GetLastChild);
          end;
        end else
        begin
          if tn.level>0 then
            result:=result+stringofchar(' ',2*tn.level)+copy(s,1,p)+cName+#13#10;
          if assigned(Comp) then
          begin
            {comp.free; comp:=nil;}
            freeandnil(comp);
          end;
          CC := TComponentClass(C1);
          Comp := CC.Create(nil);
        end;
      end else result:=result+stringofchar(' ',2*tn.level)+tn.text+#13#10;
    end;
    if (not ignorecontrol) and (not isObject(tn) or (usage=usg_save)) then
    begin
      //prüfen, ob eigenschaft existiert, wenn vorschau
      //propertyExists:=copy(s,length(s),1)<>'='; //leere properties löschen
      //outputDebugString(PCHAR(s));
      propertyExists:=not isLastChar(pchar(s),'='); //leere properties löschen
      value:=getPropertyValueTxt(s);// trim(copy(tn.text,p+1,length(tn.text)-p));
      prop:=getPropertyNameTxt(s);//trim(copy(tn.text,1,p-1));
      if propertyExists and (usage=usg_preview) and assigned(Comp) and not isObject(tn) and (pos('=',tn.text)>0) then
      begin
        //if copy(tn.Text,1,2)<>'On' then //fehler wegen fehlender event-methoden beheben
        if not ((isFirstChar(pchar(s),'O')) and (isCharAtPos(pchar(s),'n',2))) then
        begin
          //p:=pos('=',tn.text);
          temp:=replacePropsSl.Values[prop];
          if temp<>'' then
            prop:=temp
          else
          begin
            if pos('.',prop)>0 then
              temp:=copy(prop,1,pos('.',prop)-1)
            else temp:=prop;
            propertyExists:=isProperty(Comp,temp) and (ignoreSl.IndexOf(lowercase(temp))=-1);
            if ((isFirstChar(pchar(value),'f')) and (isCharAtPos(pchar(value),'s',2))) then
            begin
              propertyexists:=propertyexists and not (value='fsMDIChild');
            end;
          end;
        end else propertyexists:=false;
      end;
      if propertyExists then
      begin
        if assigned(tn.data) then
        begin
          result:=result+stringofchar(' ',2*tn.level)+prop+' = ';
          case TDataRec(tn.Data).Kind of
            dkBinary: //output.Text:=Output.Text+
            result:=result+OutputBinary(tn{,output});
            dkCollection: //output.Text:=Output.Text+
            result:=result+OutputCollection(tn,usage);
            dkStrings,dkIntegerlist: //output.Text:=Output.Text+
            result:=result+OutputStringlist(tn);
          end;
        end else if (isLastChar(pchar(tn.text),'''')) then//copy(tn.text,length(tn.text),1)='''') then
        begin
          //output.add   GetPropertyName(tn)
          result:=result+IndentLine(prop+' = '+EncodeSpecialChars(getPropertyValue(tn)),tn.Level)+#13#10;
        end else
        begin
          temp:=lowercase(prop);
          if (temp='visible') and showhidden then
            //output.add
            result:=result+(stringofchar(' ',2*tn.level)+'Visible = True')+#13#10
          else if (temp='tabvisible') and showhidden then
            //output.add
            result:=result+(stringofchar(' ',2*tn.level)+'TabVisible = True')+#13#10
          else if (temp='enabled') and EnableControls then
            //output.add
            result:=result+(stringofchar(' ',2*tn.level)+'Enabled = True')+#13#10
          else
          begin
            if (prop='') then
              //output.Add
              result:=result+(stringofchar(' ',2*tn.level)+tn.text)+#13#10
            else if ( (value<>'') and ((usage=usg_save) or isValueValid(comp,trim(copy(tn.Text,1,pos('=',tn.text) -1)),value))) then
              //output.Add
              result:=result+(stringofchar(' ',2*tn.level)+copy(tn.Text,1,pos('=',tn.text)+1)+value)+#13#10;
          end;
        end;
      end;
    end;
    if not ignorecontrol then
      tn2:=tn;

    //if (copy(tn.text,length(tn.text),1)='>') then
    if isCharAtPos(pchar(tn.text),'>',length(s)) then
    begin
      while tn.haschildren do tn:=(tn.GetLastChild);
    end;

    if isObject(tn) and not ignorecontrol then
      oldlevel:=tn.Level+1;
    if (isObject(tn)) and (assigned(callback)) and (tn.AbsoluteIndex>pNext) then
    begin
      callback((100*tn.AbsoluteIndex) div c{treeview.items.count},'');
      inc(pNext,pStep);
    end;
    tn:=tn.GetNext;
    if assigned(tn) then
    begin
      if (tn.Level<oldlevel) then
      begin
        for j:=oldlevel downto tn.level+1 do
          //output.add
          result:=result+(stringofchar(' ',2*(j-1))+'end')+#13#10;
        oldlevel:=tn.level;
      end;
      ignorecontrol:=false;
    end;
    prop:='';
  end;
  if assigned(Comp) then
    freeandnil(comp);

  c:=tn2.level;
  if not isObject(tn2) then dec(c);
  for i:=c downto 0 do
    result:=result+(stringofchar(' ',2*i)+'end')+#13#10;
  //empty forms have no level-jump => no end-tag
  if Treeview.Items.Count=1 then
    //output.add
    result:=result+('end')+#13#10;
  if (assigned(callback)) then
      callback(100,'');
  replacePropsSl.Free;
  ignoreSl.Free;
end;

//filemethodes
function LoadDFMFile(filename:string;DestSL:TStrings):boolean;
var InStream,OutStream : TMemoryStream;
    buf:string;
begin
  result:=false;
  DestSL.Text:='';
  InStream := TMemoryStream.Create;
  OutStream := TMemoryStream.Create;
  try
    try
      try
        InStream.LOADFROMFILE(filename);
      finally
        instream.Position := 0;
        setlength(buf,6);
        instream.Read(buf[1],6);
        instream.Position := 0;
        buf:=lowercase(buf);
        result:=(buf <> 'object') and (buf <> 'inline') and (buf <> 'inheri');
        if result then
        begin
          //ObjectResourceToText(InStream, OutStream)
          ConvertForm(ct_oRes2Txt,inStream,outStream);
        end else
          Outstream.LoadFromStream(inStream);
        OutStream.Seek(0, soFromBeginning);
        DestSL.LoadFromStream(OutStream);
      end;
    except
      showmessage(format(LocalizeString('Msg_CannotLoad'),[Filename]));
    end;
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure SaveDFMFile(filename:string;SourceSL:TStrings;Binary:boolean);
var
  //OutStream: TFileStream;
  InStream,OutStream : TMemoryStream;
begin
  createBackup(filename);
  //OutStream := TFileStream.Create(filename, fmCreate or fmOpenReadWrite);
  OutStream := TMemoryStream.Create;
  InStream := TMemoryStream.Create;
  try
    sourcesl.SaveToStream(InStream);
    InStream.Seek(0, 0);
    if Binary then
      //ObjectTextToResource(InStream, OutStream)
      ConvertForm(ct_oTxt2Res,inStream,outStream)
    else
      InStream.SaveToStream(OutStream);
  finally
    OutStream.SaveToFile(filename);
    OutStream.Free;
    InStream.Free;
  end;
end;

//assignment
procedure AssignDFMToForm(SourceSL:TStrings);
var
  OutStream,InStream:TMemoryStream;
begin
  OutStream := TMemorystream.Create;
  InStream := TMemoryStream.Create;
  try
    sourceSL.SaveToStream(InStream);
    InStream.Seek(0, 0);
    //ObjectTextToResource(InStream, OutStream);
    ConvertForm(ct_oTxt2Res,InStream,OutStream);
    OutStream.Seek(0, 0);
    Form_DFMPreview.Free;
    loading:=true;
    Form_DFMPreview:=TForm_DFMPreview.create(Application);
    Form_DFMPreview.Popup_MenuEditor.free;
    outstream.ReadComponentRes(Form_DFMPreview);
    Form_DFMPreview.Show;
    loading:=false;
  finally
    OutStream.Free;
    InStream.Free;
  end;
end;

function GetImageDFMData(image:TImage;PictureHeader:Boolean):string;
var ImgS:TStringList;
    DataS:string;
    inS,outS:TMemoryStream;
    p1,p2:integer;
begin
  if assigned(image.picture) then
  begin
    inS:=TMemoryStream.create;
    outS:=TMemoryStream.create;
    inS.WriteComponent(image);
    inS.Seek(0,0);
    //ObjectBinaryToText(inS,outS);
    ConvertForm(ct_oBin2Txt,inS,OutS);
    outS.Seek(0,0);
    imgS:=TStringlist.create;
    ImgS.LoadFromStream(outS);
    // alles aus picture.data rausholen
    p1:=pos('{',imgS.text)+1;
    p2:=pos('}',imgS.text);
    DataS:=removeLineBreaks(copy(imgS.Text,p1,p2-p1),true);
    if (not PictureHeader) then //TPictureHeader löschen
    begin
      delete(dataS,1,(1+length(image.picture.Graphic.ClassName))*2);
      //if (image.Picture.Graphic.ClassName='TBitmap') then
      //  delete(dataS,1,16);
    end;
    imgS.free;
    inS.free;
    outS.free;
  end;
  result:=DataS;
end;

function AssignDFMToImage(image:TImage;data:string):boolean;
var ImgS:TStringList;
    DataS:string;
    inS,outS:TMemoryStream;
    i,indent:integer;
    err:boolean;
    eraseBmp:TBitmap;
begin
  eraseBmp:=TBitmap.create;
  eraseBmp.height:=1;
  eraseBmp.width:=1;
  err:=false;
  DataS:=removelinebreaks(data,true);
    image.Picture.Assign(eraseBmp);
    //TPicture-Header hinzufügen wenn noch nicht vorhanden
    if DataS<>'' then
    begin
      if (chr(StrtoInt('$'+copy(DataS,3,2)))<>'T') then
      //evtl. Prüfung, ob 3./4. byte eine #0
      begin
        if (copy(DataS,7,4)<>'0000') and (pos('424D',dataS)=9) then //TBitmap
          DataS:='07544269746D6170'+DataS//07+TBitmap+DataS
          //TICON-HEADER: 01000100202
        else if (pos('01000100',dataS)=5) then //TICON
          DataS:='055449636F6E'+DataS //05+TIcon+DataS
        else
        begin
          //showmessage('DFM-Daten enthalten kein bekanntes Format');
          err:=true;
        end;
      end;
    end;// else err:=true;
    if not err then
    begin
      indent:=4;
      //Zeilenumbrüche in Data hinzufügen
      DataS:=AddLineBreaks(DataS,indent+2);
      DataS:=DataS+StringOfChar(' ',indent)+'}';
      //showmessage('"'+DataS+'"');
      //ImageDFM holen
      inS:=TMemoryStream.create;
      outS:=TMemoryStream.create;
      inS.WriteComponent(image);
      inS.Seek(0,0);
      //ObjectBinaryToText(inS,outS);
      ConvertForm(ct_oBin2Txt,inS,OutS);
      outS.Seek(0,0);
      imgS:=TStringlist.create;
      ImgS.LoadFromStream(outS);
      //<ReplaceData>
      i:=0;
      //alles zwischen { und } aus dem image-dfm löschen und neues Data einfügen
      while (pos('picture.data',lowercase(imgS[i]))=0) and (i<imgS.count) do inc(i);
      inc(i);
      if (i<imgS.count) then
      begin
        while pos('}',ImgS[i])=0 do
        begin
          ImgS.delete(i);
        end;
        ImgS.delete(i);
        //ImgS.Insert(i,'}');
        //ggf. eine Leerezeile einfügen
        ImgS.Insert(i,DataS);
        // Streams für assign löschen
        inS.Size:= 0;
        inS.Position:= 0;
        outS.Size:= 0;
        outS.Position:= 0;
        //<Assign>
        //form1.memo1.lines.Assign(ImgS);
        imgS.SaveToStream(inS);
        inS.Seek(0, 0);
        //ObjectTextToResource(inS, outS);
        ConvertForm(ct_oTxt2Res,inS,outS);
        OutS.Seek(0, 0);
        try
          outS.ReadComponentRes(image);
        except
          err:=true;
          Abort;
        end;
      end else showmessage(LocalizeString('Msg_PictureDataNotFound'));
      // Freigeben
      imgS.free;
      inS.free;
      outS.free;
    end;
  erasebmp.Free;
  result:=not err;
end;

function GetImageListDFMData(imagelist:TImageList):string;
var ms,ms2:TMemoryStream;
    sl:Tstringlist;
    p1,p2:integer;
begin
  ms:=TMemoryStream.create;
  ms2:=TMemoryStream.create;
  ms.WriteComponent(imagelist);
  ms.position:=0;
  //ObjectBinaryToText(ms,ms2);
  ConvertForm(ct_oBin2Txt,ms,ms2);
  ms2.Position:=0;
  sl:=TStringlist.create;
  sl.LoadFromStream(ms2);
  p1:=pos('{',sl.text);
  p2:=pos('}',sl.text);
  result:=removelinebreaks(copy(sl.text,p1+1,p2-p1-1),true);
  sl.free;
  ms2.free;
  ms.free;
end;

function AssignDFMToImageList(imagelist:TImagelist;data:string):boolean;
var ms,ms2:TMemoryStream;
    //ss:TStringStream;
    sl:Tstringlist;
    p1,p2:integer;
    s:string;
    tmpBmp:TBitmap;
begin
  result:=true;
  ms:=TMemoryStream.create;
  ms2:=TMemoryStream.create;
  //ss:=TStringStream.Create('');
  //add a bitmap to create the binary-property
  tmpBmp:=TBitmap.create;
  tmpBmp.Width:=imagelist.Width;
  tmpBmp.Height:=imagelist.height;
  imagelist.Clear;
  imagelist.AddMasked(tmpbmp,clnone);
  tmpBmp.free;
  ms.WriteComponent(imagelist);
  ms.position:=0;
  //ObjectBinaryToText(ms,ss);
  ConvertForm(ct_oBin2Txt,ms,ms2);
  //ss.Position:=0;
  ms2.Position:=0;
  sl:=TStringlist.create;
  sl.LoadFromStream(ms2);
  //ss.free;
  s:=sl.Text;
  sl.free;
  p1:=pos('{',s);
  p2:=pos('}',s);

  delete(s,p1+1,p2-p1-1);
  insert(#13#10+breakbinarydata(removelinebreaks(data,true)),s,p1+1);
  //ss:=TStringstream.Create(s);
  ms2:=TMemoryStream.Create;
  ms2.Write(Pointer(s)^,length(s));
  ms2.Position:=0;
  ms.Size:=0;
  //ObjectTextToBinary(ss,ms);
  ConvertForm(ct_oTxt2Bin,ms2,ms);
  ms.position:=0;
  try
    ms.ReadComponent(imagelist);
  except
    result:=false;
    Abort;
  end;
  //ss.free;
  ms.free;
  ms2.free;
end;

function CheckCond(condvalue,value:string):boolean;
var sl:TStringlist;
begin
  result:=(condvalue=value);
  if copy(condvalue,1,1)='[' then
  begin
    sl:=TStringlist.create;
    condvalue:=copy(condvalue,2,length(condvalue)-2);
    sl.CommaText:=condvalue;
    result:=result or (FindInStringlist(sl,value)>-1);
    sl.free;
  end;
end;

function RemoveUnwanted(Treeview:TTreeview):integer;
var ini:TInifile;
    sl_all,sl_Class:TStringlist;
    node,delnode,objnode,propnode:TTreenode;
    objclass,propname,cond,cvalue:string;
    ip:integer;
    deleteit:boolean;
begin
  result:=0;
  node:=Treeview.Items.GetfirstNode;
  objNode:=node;
  sl_all:=TStringlist.create;
  sl_class:=TStringlist.create;
  ini:=Tinifile.create(UnwantedFile);
  while Assigned(node) do
  begin
    if isObject(Node) then
    begin
      if node.AbsoluteIndex=0 then
        objClass:='TForm'
      else
        objClass:=GetObjectClass(node);
      objNode:=node;
      ini.ReadSection(objClass,sl_class);
      ini.ReadSection('default',sl_all);
    end else if isProp(node) then
    begin
      propname:=GetPropertyName(node);
      ip:=FindInStringlist(sl_class,propname);
      cond:=ini.readstring(objClass,propname,'');
      if ip=-1 then
      begin
        ip:=FindInStringlist(sl_all,propname);
        cond:=ini.readstring('default',propname,'');
      end;
      if ip>-1 then
      begin
        deleteit:=false;
        if pos('=',cond)>0 then
        begin
          propnode:=GetPropNode(ObjNode,GetPropertyNameTxt(cond));
          cvalue:=GetPropertyValueTxt(cond);
          deleteit:=CheckCond(cvalue,GetPropertyValue(propnode));
        end;
        if (cond='*') or (cond=GetPropertyValue(node)) or (deleteit) then
        begin
          delnode:=node;
          node:=node.GetPrev;
          delnode.Delete;
          inc(result);
          filechanged:=true;
        end;
      end;
    end;
    Node:=Node.GetNext;
  end;
  ini.free;
  sl_class.free;
  sl_all.free;
end;

end.
