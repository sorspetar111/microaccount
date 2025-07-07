unit chooseRes_u;

{*
Program: DFMEdit
Unit: chooseres_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Form to choose form from Binary (exe/dll)

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

{$I dfmedit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm_DFMChooseRes = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Btn_OK: TButton;
    Btn_Cancel: TButton;
    procedure Panel1Resize(Sender: TObject);
    procedure Btn_OKClick(Sender: TObject);
  private
    { Private-Deklarationen }
    hModule:hRsrc;
    fOutput:TStrings;
    FormName,fname:string;
    function loadFile(filename:string):integer;
    procedure getFormData(s:TStrings);
    procedure WMGetMinMaxInfo(Var Msg: TMessage); Message WM_GETMINMAXINFO;
  public
    { Public-Deklarationen }
    procedure ChooseForm(filename:string;s:TStrings);
    procedure ReplaceRes(s:TStrings;resname:string);
  end;

var
  Form_DFMChooseRes: TForm_DFMChooseRes;

function EnumDfmNameProc(hModule: THandle; lpszType, lpszName: PChar;
  lParam: Integer): Boolean; stdcall;

implementation

{$R *.DFM}
uses functions_u,localize_u,classes_u,convert_u;

function EnumDfmNameProc(hModule: THandle; lpszType, lpszName: PChar;
  lParam: Integer): Boolean; stdcall;
var
  rs: TResourceStream;
  Buf: String;
begin
  rs := TResourceStream.Create(hModule, lpszname, lpszType); // load resource in memory
  try
    try
      setlength(buf,4);
      rs.Read(Buf[1], 4); // read the first 4 bytes
      if Buf = 'TPF0' then // is it a DFM resource?
      begin
        TStrings(lParam).Add(StrPas(lpszName));
      end;
    except
      raise;
    end;
  finally
    rs.free;
  end;
  Result := True;
end;

//private
function TForm_DFMChooseRes.loadFile(filename:string):integer;
begin
  freelibrary(hModule);
  Listbox1.Clear;
  hModule:=LoadLibraryEx(PCHAR(filename),0,LOAD_LIBRARY_AS_DATAFILE);
  if hModule <> 0 then
  begin
    fName:=filename;
    EnumResourceNames(hModule, RT_RCDATA,@EnumDfmNameProc, Integer(Listbox1.Items));
  end;
  result:=hModule;
end;

procedure TForm_DFMChooseRes.getFormData(s:TStrings);
var rs:TResourceStream;
    ms:TMemoryStream;
    buf,f:string;
begin
  if hModule<>0 then
  begin
    setlength(buf,6);
    rs :=TResourceStream.Create(hModule,formname,RT_RCDATA);
    if cfg.debugMode then
    begin
      f:=extractfilePath(fName)+formname+'.bin';
      rs.SaveToFile(f);
    end;
    ms:=TMemoryStream.Create;
    //ObjectBinaryToText(rs,ms);
    ConvertForm(ct_oBin2Txt,rs,ms);
    ms.position:=0;
    s.LoadFromStream(Ms);
    ms.free;
    rs.free;
    close;
  end;
end;

Procedure TForm_DFMChooseRes.WMGetMinMaxInfo(Var Msg: TMessage);
Begin
  PMinMaxInfo(Msg.lParam)^.ptMinTrackSize:= Point(ChooseRes_MinWidth, ChooseRes_MinHeight);
End;

//public
procedure TForm_DFMChooseRes.ChooseForm(filename:string;s:TStrings);
begin
  loadfile(filename);
  if (hModule<>0) then
  begin
    if Listbox1.Items.Count>0 then
    begin
      FormName:='';
      fOutput:=s;
      Self.Showmodal;
    end else showmessage(localizeString('Msg_NoForms'));
  end else showmessage(localizeString('Msg_FailedToOpen'));
end;

procedure TForm_DFMChooseRes.ReplaceRes(s:TStrings;resname:string);
var h :THandle;
    //ms:TStringStream;
    ms,ms2: TMemoryStream;
begin
  if hModule<>0 then
  begin
    createbackup(fname);
    freelibrary(hModule);
    //ms:=TStringStream.create(s.Text);
    ms:=TMemoryStream.create;
    s.SaveToStream(ms);
    ms.Position:=0;
    ms2:=TMemoryStream.create;
    //ObjectTextToBinary(ms,ms2);
    ConvertForm(ct_oTxt2Bin,ms,ms2);
    ms2.Position:=0;
    if resname='' then
      resname:=FormName;
    h := BeginUpdateResource(PChar(fname), false);
    UpdateResource(h, RT_RCData, PCHAR(ResName), 0, ms2.Memory, ms2.size);
    EndUpdateResource(h, false);
    ms2.Free;
    ms.free;
  end;
end;

//events
procedure TForm_DFMChooseRes.Panel1Resize(Sender: TObject);
begin
  Btn_ok.width:=((sender as Tpanel).width div 2)-1;
  Btn_cancel.Width:=Btn_Ok.width;
  Btn_Cancel.Left:=Btn_ok.Width+1;
end;

procedure TForm_DFMChooseRes.Btn_OKClick(Sender: TObject);
begin
  if Listbox1.itemindex>-1 then
  begin
    formName:=Listbox1.Items.Strings[Listbox1.itemindex];
    if formName<>'' then
      getFormData(fOutput);
  end else ShowMessage(LocalizeString('Msg_NothingSelected'));
end;

end.
