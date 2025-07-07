unit Version_u;

{*
Program: DFMEdit
Unit: Version_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Functions for getting/comparing Fileversions and Internet-Update

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

interface

uses
  Windows, Messages, SysUtils, dialogs, inifiles, classes,
  classes_u;

function CompareVersions(vi_old,vi_new:TVersionInfo):boolean;
function GetFileVersion(const FileName: String): TVersionInfo;
function GetNewVersion(checkbeta:boolean):TVersionInfo;
function ParseVersionInfo(versionstr:string):  TVersionInfo;
function VersionInfo(MainV,SubV,Release,Build:Byte):TVersionInfo;
function VersionToStr(vi:TVersionInfo):string;

implementation

uses localize_u,functions_u;

function CompareVersions(vi_old,vi_new:TVersionInfo):boolean;
var Vold,Vnew:integer;
begin
  Vold:=(vi_old.MainV shl 24) or (vi_old.SubV shl 16) or (vi_old.Release shl 8) or vi_old.Build;
  Vnew:=(vi_new.MainV shl 24) or (vi_new.SubV shl 16) or (vi_new.Release shl 8) or vi_new.Build;
  result:=vnew>vold;
end;

function GetFileVersion(const FileName: String): TVersionInfo;
var
  VersionInfoSize, VersionInfoValueSize, Zero: DWord;
  VersionInfo, VersionInfoValue: Pointer;
  p:PChar;
begin
  if not FileExists(FileName) then
  begin
    Result.MainV:=0;
    Result.SubV:=0;
    Result.Release:=0;
    Result.Build:=0;
    Exit;
  end;
  VersionInfoSize := GetFileVersionInfoSize(PChar(FileName), Zero);
  if VersionInfoSize = 0 then Exit;
  GetMem(VersionInfo, VersionInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VersionInfoSize, VersionInfo) and
      VerQueryValue(VersionInfo, '\', VersionInfoValue,
      VersionInfoValueSize) and (0 <> LongInt(VersionInfoValueSize)) then
    begin
      with TVSFixedFileInfo(VersionInfoValue^) do
      begin
        Result.MainV:=HiWord(dwFileVersionMS);
        Result.SubV:=LoWord(dwFileVersionMS);
        Result.Release:=HiWord(dwFileVersionLS);
        Result.Build:=LoWord(dwFileVersionLS);
      end;
    end;
    if VerQueryValue(VersionInfo, PChar('\StringFileInfo\000004e4\FileVersion'), Pointer(P), VersionInfoValueSize) then
    begin
      result.FVersion:=strPas(P);
    end;
  finally
    FreeMem(VersionInfo);
  end;
end;

function GetNewVersion(checkbeta:boolean):TVersionInfo;
var
  DestFile,s:string;
  ini:Tinifile;
begin
  result:=Versioninfo(0,0,0,0);
  DestFile:=extractfilepath(paramstr(0))+'version.ini';
  if DownloadFile(VersionFile,DestFile) then
  begin
    ini:=TInifile.Create(DestFile);
    try
      if checkbeta then
      begin
        s:=ini.ReadString('versioninfo','lastbeta','error');
      end else
        s:=ini.ReadString('versioninfo','last','error');
      if s<>'error' then
        result:=ParseVersionInfo(s);
      result.BetaDownload:=ini.ReadString('versioninfo','betapage','');
    finally
      WritePrivateProfileString(nil,nil,nil,PCHAR(DestFile));
      ini.free;
      deletefile(DestFile);
    end;
  end else
    showmessage(LocalizeString('Msg_GetVersionError')+#13#13+SysErrorMessage(GetLastError));
end;

function ParseVersionInfo(versionstr:string):  TVersionInfo;
var i,p:integer;
    b:byte;
    s:string;
begin
  s:=versionstr;
  for i:=0 to 3 do
  begin
    p:=pos('.',s);
    if p=0 then p:=length(s)+1;
    try
      b:=strToInt(copy(s,1,p-1));
      case i of
        0: result.MainV:=b;
        1: result.SubV:=b;
        2: result.Release:=b;
        3: result.Build:=b;
      end;
    except
      Abort;
    end;
    delete(s,1,p);
  end;
end;

function VersionInfo(MainV,SubV,Release,Build:Byte):TVersionInfo;
begin
  result.MainV:=MainV;
  result.SubV:=SubV;
  result.Release:=Release;
  result.Build:=Build;
end;

function VersionToStr(vi:TVersionInfo):string;
begin
  result:=inttostr(vi.MainV)+'.'+inttostr(vi.SubV)+'.'+
          inttostr(vi.Release)+'.'+inttostr(vi.build);
end;

end.
