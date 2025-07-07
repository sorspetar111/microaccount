unit dialogs_u;
{
  Author: Mike Shkolnik
  Homepage: http://www.scalabium.com

  Today I want to display how you may use the simple functions as
  alternative for TOpenDialog/TSaveDialog components.

  Problem is that Borland incorrectly wrote those components and when
  Microsoft add some new extended features in own dialogs,
  standard TOpenDialog and TSaveDialog still use old style.
  For example, when Microsoft added placebar in own dialogs,
  VCL's dialog still use old style without placebars.

  Personally I prefer to use applications that support all
  features of installed OS.

  This is a reason why I wrote the function as wrapper for Windows
  API call for dialogs and now I use this function instead components.
  It allow not only to use all new features from Windows,
  but also reduce a size for exe-file and allow to use same function for
  both TOpenDialog and TSaveDialog functionality.

  For example:
  1. to display the "OpenDialog" for text files
    s := 'aaa.txt';
    if OpenSaveFileDialog(Application.Handle, 'txt', 'Text Files|*.txt', 'c:\',
    'Select text file', s, True) then
      ShowMessage(s + ' file was selected for open')

  2. to display the "Save dialog":
    s := 'data.dbf';
    if OpenSaveFileDialog(Application.Handle, 'dbf', 'dBase tables|*.dbf|All files|*.*',
    'c:\', 'Select table', s, False) then
      ShowMessage(s + ' table was selected for save')

  See full code below. Hope you'll find this code useful.

  support for flags and multiselection by Frank Wunderlich
  thanks to Michael Puff's Win32-API-Tutorial
}
interface

uses Windows;

const
  OFN_READONLY             = $00000001;
  OFN_OVERWRITEPROMPT      = $00000002;
  OFN_HIDEREADONLY         = $00000004;
  OFN_NOCHANGEDIR          = $00000008;
  OFN_SHOWHELP             = $00000010;
  OFN_ENABLEHOOK           = $00000020;
  OFN_ENABLETEMPLATE       = $00000040;
  OFN_ENABLETEMPLATEHANDLE = $00000080;
  OFN_NOVALIDATE           = $00000100;
  OFN_ALLOWMULTISELECT     = $00000200;
  OFN_EXTENSIONDIFFERENT   = $00000400;
  OFN_PATHMUSTEXIST        = $00000800;
  OFN_FILEMUSTEXIST        = $00001000;
  OFN_CREATEPROMPT         = $00002000;
  OFN_SHAREAWARE           = $00004000;
  OFN_NOREADONLYRETURN     = $00008000;
  OFN_NOTESTFILECREATE     = $00010000;
  OFN_NONETWORKBUTTON      = $00020000;
  OFN_NOLONGNAMES          = $00040000;
  OFN_EXPLORER             = $00080000;
  OFN_NODEREFERENCELINKS   = $00100000;
  OFN_LONGNAMES            = $00200000;
  OFN_ENABLEINCLUDENOTIFY  = $00400000;
  OFN_ENABLESIZING         = $00800000;
  OFN_DONTADDTORECENT      = $02000000;
  OFN_FORCESHOWHIDDEN      = $10000000;
  OFN_EX_NOPLACESBAR       = $00000001;

type
  POpenFilenameA = ^TOpenFilenameA;
  POpenFilename = POpenFilenameA;
  tagOFNA = packed record
    lStructSize: DWORD;
    hWndOwner: HWND;
    hInstance: HINST;
    lpstrFilter: PAnsiChar;
    lpstrCustomFilter: PAnsiChar;
    nMaxCustFilter: DWORD;
    nFilterIndex: DWORD;
    lpstrFile: PAnsiChar;
    nMaxFile: DWORD;
    lpstrFileTitle: PAnsiChar;
    nMaxFileTitle: DWORD;
    lpstrInitialDir: PAnsiChar;
    lpstrTitle: PAnsiChar;
    Flags: DWORD;
    nFileOffset: Word;
    nFileExtension: Word;
    lpstrDefExt: PAnsiChar;
    lCustData: LPARAM;
    lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
    lpTemplateName: PAnsiChar;
  end;
  TOpenFilenameA = tagOFNA;
  TOpenFilename = TOpenFilenameA;

function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter, InitialDir,
  Title: string; var FileName:string; IsOpenDialog: Boolean;optflags:integer): Boolean;
function OpenFolder(Root: Integer; const Caption: string; var directory:string): boolean;

implementation
uses ShlObj, SysUtils;

function GetOpenFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetOpenFileNameA';
function GetSaveFileName(var OpenFile: TOpenFilename): Bool; stdcall; external 'comdlg32.dll'  name 'GetSaveFileNameA';

function CharReplace(const Source: string; oldChar, newChar: Char): string;
var
  i: Integer;
begin
  Result := Source;
  for i := 1 to Length(Result) do
    if Result[i] = oldChar then
      Result[i] := newChar
end;

function decodefiles(szFile:PChar):string;
var s,dir:string;
const seperator=#13;
begin
  dir:=StrPas(szFile);
  inc(szFile,lstrlen(szFile)+1);
  s:='';
  while(szFile[0]<>#0)do
  begin
    s:=s+dir+'\'+StrPas(szFile)+seperator;
    inc(szFile,lstrlen(szFile)+1);
  end;
  if s='' then
    result:=dir+seperator //only one file
  else
    result:=s;
end;

function OpenSaveFileDialog(ParentHandle: THandle; const DefExt, Filter, InitialDir, Title: string; var Filename:string; IsOpenDialog: Boolean;optflags:integer): Boolean;
var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
begin
  Result := False;
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  with ofn do
  begin
    lStructSize := SizeOf(TOpenFileName);
    hwndOwner := ParentHandle;
    lpstrFile := szFile;
    nMaxFile := SizeOf(szFile);
    if (Title <> '') then
      lpstrTitle := PChar(Title);
    if (InitialDir <> '') then
      lpstrInitialDir := PChar(InitialDir);
    StrPCopy(lpstrFile, string(FileName));
    lpstrFilter := PChar(CharReplace(Filter, '|', #0)+#0#0);
    if DefExt <> '' then
      lpstrDefExt := PChar(DefExt);
    if optflags>0 then
      flags:=OFN_Explorer or optflags;
  end;
  if IsOpenDialog then
  begin
    if GetOpenFileName(ofn) then
    begin
      Result := True;
      if (optflags and OFN_AllowMultiSelect)>0 then
        filename:= decodefiles(szFile)
      else
        fileName:=StrPas(szFile);
    end;
  end
  else
  begin
    if GetSaveFileName(ofn) then
    begin
      Result := True;
      if (optflags and OFN_AllowMultiSelect)>0 then
        filename:= decodefiles(szFile)
      else
        fileName:=StrPas(szFile);
    end;
  end
end;

//folderbrowse-dialog
const
  BIF_NEWDIALOGSTYLE     = $0040;
  BIF_EDITBOX            = $0010;
  BIF_USENEWUI           = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;
  BIF_BROWSEINCLUDEURLS  = $0080;
  BIF_UAHINT             = $0100;
  BIF_NONEWFOLDERBUTTON  = $0200;
  BIF_NOTRANSLATETARGETS = $0400;
  BIF_SHAREABLE          = $8000;

  BFFM_IUNKNOWN          = 5;

  SLGP_SHORTPATH         = $0001;
  SLGP_UNCPRIORITY       = $0002;
  SLGP_RAWPATH           = $0004;

  function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: Cardinal;
    lpData: Cardinal): Integer; stdcall;
  var
    PathName: array[0..MAX_PATH] of Char;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Hwnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
      BFFM_SELCHANGED:
        begin
          SHGetPathFromIDList(PItemIDList(lParam), @PathName);
          SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Longint(PChar(@PathName)));
        end;
    end;
    Result := 0;
  end;

function OpenFolder(Root: Integer; const Caption: string; var directory:string): boolean;
var
  bi: TBrowseInfo;
  lpBuffer: PChar;
  pidlPrograms, pidlBrowse: PItemIDList;
begin
  result:=false;
  if (not SUCCEEDED(SHGetSpecialFolderLocation(GetActiveWindow, Root,
    pidlPrograms))) then
    exit;
  lpBuffer := StrAlloc(MAX_PATH);
  bi.hwndOwner := GetActiveWindow;
  bi.pidlRoot := pidlPrograms;
  bi.pszDisplayName := lpBuffer;
  bi.lpszTitle := PChar(Caption);
  bi.ulFlags := BIF_RETURNONLYFSDIRS {or BIF_STATUSTEXT} or BIF_NEWDIALOGSTYLE;
  bi.lpfn := @BrowseCallbackProc;
  bi.lParam := 0;
  pidlBrowse := SHBrowseForFolder(bi);
  if (pidlBrowse <> nil) then
    if SHGetPathFromIDList(pidlBrowse, lpBuffer) then
    begin
      directory := lpBuffer;
      result:=true;
    end;
  StrDispose(lpBuffer);
end;


end.
