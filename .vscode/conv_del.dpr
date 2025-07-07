library conv_del;

{ Wichtiger Hinweis zur DLL-Speicherverwaltung: ShareMem muß die
  erste Unit im Uses-Anweisungsteil des Interface-Abschnitts Ihrer
  Unit sein, wenn Ihre DLL Prozeduren oder Funktionen exportiert, die
  String-Parameter oder Funktionsergebnisse übergeben. Dies gilt für
  alle Strings die an und von Ihrer DLL übergeben werden --  selbst
  für diese, die in Records oder Klassen verschachtelt sind. ShareMem
  ist die Schnittstellen-Unit zur DELPHIMM.DLL, welche Sie mit Ihrer
  DLL weitergeben müssen. Um die Verwendung von DELPHIMM.DLL zu
  vermeiden, übergeben Sie String-Parameter unter Verwendung von
  PChar- oder ShortString-Parametern. }

uses
  SysUtils,
  Classes,windows;

type
  TConvertType=(ct_oTxt2Bin,ct_oBin2Txt,ct_oTxt2Res,ct_oRes2Txt);

var os:TMemoryStream;

procedure createStream; export;
begin
  os:=TMemoryStream.Create;
end;

procedure freeStream; export;
begin
  os.free;
end;

function checkoutput:boolean;
begin
  result:=assigned(os);
  if not result then
    messagebox(0,'Output not assigned!','error',mb_ok or mb_iconerror);
end;

function ConvertForm(ct:TConvertType;InData:Pointer;inLen:integer;var OutData:Pointer;var outLen:integer):boolean;stdcall;export;
var ms:TMemoryStream;
begin
  if checkoutput then
  begin
    ms:=TMemoryStream.Create;
    ms.Write(inData^,inLen);
    ms.Position:=0;
    ms.Position:=0;
    os.Size:=0;
    case ct of
      ct_oTxt2Bin:ObjectTextToBinary(ms,os);
      ct_oBin2Txt:ObjectBinaryToText(ms,os);
      ct_oTxt2Res:ObjectTextToResource(ms,os);
      ct_oRes2Txt:ObjectResourceToText(ms,os);
    end;
    os.Position:=0;
    OutData:=os.memory;
    outlen:=os.size;
    ms.free;
  end;
  result:=true;
end;

exports
  createStream,freeStream,ConvertForm;

begin

end.
 