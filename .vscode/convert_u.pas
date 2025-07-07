unit convert_u;

{*
Program: DFMEdit
Unit: convert_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
functions for mapping convert-dll

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls,classes_u;

type
  TConvertType=(ct_oTxt2Bin,ct_oBin2Txt,ct_oTxt2Res,ct_oRes2Txt);
  TConvertFunction=function (ct:TConvertType;InData:Pointer;inLen:integer;var OutData:Pointer;var outLen:integer):boolean;stdcall;
  TInit=procedure;

const
  conv_dll='conv_del.dll';
  conv_laz_dll='conv_laz.dll';

function initdll(dll:string):THandle;
function finaldll(handle:THandle):boolean;
function convertForm(ct:TConvertType;inStream,OutStream:TCustomMemoryStream):boolean;

implementation

uses localize_u;

function initdll(dll:string):THandle;
var hDll:Thandle;
    i:TInit;
    dName:string;
begin
  {$IFDEF DEBUGSYMBOLS}
  outputDebugString(PCHAR('init conv.dll'));
  {$ENDIF}
  dName:=ExtractfilePath(paramstr(0))+dll;
  hDll:=LoadLibrary(PChar(dName));
  if hDll<>0 then
  begin
    @i:=getProcAddress(hDll,'createStream');
    if assigned(i) then i;
  end else showmessage(format(LocalizeString('Msg_CannotLoad'),[dName]));//showmessage('cannot load dll!');
  result:=hDll;
end;

function finaldll(handle:THandle):boolean;
var i:TInit;
begin
  {$IFDEF DEBUGSYMBOLS}
  outputDebugString(PCHAR('final conv.dll'));
  {$ENDIF}
  @i:=getProcAddress(handle,'freeStream');
  if assigned(i) then i;
  freelibrary(handle);
  result:=true;
end;

function convertForm(ct:TConvertType;inStream,OutStream:TCustomMemoryStream):boolean;
var hDll:Thandle;
    f:TConvertFunction;
    p:Pointer;
    mss:integer;
    fName:string;
begin
  {$IFDEF DEBUGSYMBOLS}
  outputDebugString(PCHAR('convert Form'));
  {$ENDIF}
  result:=false;
  try
    if cfg.UseDllForSaving or (ct in [ct_oBin2Txt,ct_oRes2Txt]) then
    begin
      fName:='ConvertForm';
      hDll:=initDll(conv_dll);//conv_laz_dll
      if hDll<>0 then
      begin
        @f:=getProcAddress(hDll,PChar(fName));
        if assigned(f) then
        begin
          f(ct,inStream.memory,inStream.size,p,mss);
          outStream.write(p^,mss);
          outStream.Position:=0;
          result:=true;
        end else
          showmessage(format(LocalizeString('Msg_FunctionNotFound'),[fName]));
        finalDll(hDll);
      end;
    end else
    begin
      case ct of
        ct_oTxt2Bin: ObjectTextToBinary(InStream,OutStream);
        ct_oTxt2Res: ObjectTextToResource(InStream,OutStream);
      end;
    end;
  except
    showmessage(localizeString('Msg_ConvertError'));
  end;
end;

end.
