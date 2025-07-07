unit localize_u;

{*
Program: DFMEdit
Unit: localize_u
Copyright: (C) 2006-2007 Frank Wunderlich (frank-w@users.sf.net)
Localization-Functions

License: GPL ( http://gnu.org/licenses/gpl.html )
*}

interface

uses
  SysUtils, Forms, Classes, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  inifiles, dialogs,checklst,menus,graphics;

procedure localizeControls(form:TForm;langID:string);
function localizeString(value:string):string;

implementation

uses main_u,Classes_u,options_u,functions_u;

function ConvertLineBreak(s:string):string;
var p:integer;
begin
  p:=pos('\n',s);
  while p>0 do
  begin
    s[p]:=#13;
    s[p+1]:=#10;
    p:=pos('\n',s);
  end;
  result:=s;
end;

function ConvertTab(s:string):string;
var p:integer;
begin
  p:=pos('\t',s);
  while p>0 do
  begin
    s[p]:=#9;
    delete(s,p+1,1);
    p:=pos('\t',s);
  end;
  result:=s;
end;

function IsMainTopItem(item: TMenuItem): Boolean;
begin
  //Result := item.Parent = menu.Items;
  result:= TComponent(item).getparentcomponent is TMainMenu;
end;

procedure localizeControls(form:TForm;langID:string);
var i,j:integer;
    c:TComponent;
    ini:Tinifile;
    s,cname:string;
begin
  if FileExists(extractfilepath(paramstr(0))+lng_prefix+langID+'.lng') then
  begin
    ini:=Tinifile.create(extractfilepath(paramstr(0))+lng_prefix+langID+'.lng');
    s:=ini.ReadString('Main','Font','');
    if s<>'' then form.Font.Name:=s;
    s:=ini.ReadString('Main','Charset','DEFAULT_CHARSET');
    if s<>'DEFAULT_CHARSET' then
    begin
      IdentToCharset(s,j);
      form.Font.Charset:=j;
    end;
    //showmessage(inttostr(j));
    s:=ini.ReadString('controls',form.name,'');
    if s<>'' then form.caption:=s;
    for i:=0 to form.ComponentCount-1 do
    begin
      if (form.Components[i] is TComponent) then
      begin
        c:=(form.Components[i] as TComponent);
        if (c is TListview) then
        begin
          for j:=1 to (c as TListview).Columns.count do
          begin
            s:=ConvertLineBreak(ini.ReadString('controls',c.name+'.Column'+inttostr(j),''));
            if s<>'' then (c as TListview).Columns[j-1].caption:=s;
          end;
          s:=ConvertLineBreak(ini.ReadString('controls',c.name+'.hint',''));
          if s<>'' then (c as TListview).Hint:=s;
        end else if (c is TCheckListBox) then
        begin
          for j:=1 to (c as TCheckListbox).items.count do
          begin
            s:=ConvertLineBreak(ini.ReadString('controls',c.name+'.'+inttostr(j),''));
            if s<>'' then (c as TCheckListbox).items.Strings[j-1]:=s;
          end; //for CheckListbox.items
        end else if (c is TCombobox) then
        begin
          for j:=1 to (c as TCombobox).items.count do
          begin
            s:=ini.ReadString('controls',c.name+'.'+inttostr(j),'');
            if s<>'' then (c as TCombobox).items.Strings[j-1]:=s;
          end;
        end else
        begin
          cname:=RemoveNumber(c.name);
          s:=ini.ReadString('controls',cname,'');
          //making menu-captions same as toolbutton-hints
          if (s='') then
          begin
            if (copy(cname,1,3)='MI_') {and (form.findcomponent('TB_'+copy(cname,4,length(cname)-3))<>nil)} then
              cname:='TB_'+copy(cname,4,length(cname)-3)
            else if (copy(cname,1,4)='Chk_') then
              cname:='TB_'+copy(cname,5,length(cname)-4)
            else if (copy(cname,1,4)='Btn_') then
              cname:='TB_'+copy(cname,5,length(cname)-4);
            s:=ini.ReadString('controls',cname,'');
          end;
          s:=ConvertTab(ConvertLineBreak(s));
          if not (c is TMenuItem) then
          begin
            if (pos(#9,s)>0) then
              s:=copy(s,1,pos(#9,s)-1);
            s:=removechar(s,'&');
          end;
          if s<>'' then
          begin
            if (c is TCheckbox) then (c as Tcheckbox).caption:=s;
            if (c is TLabel) then (c as TLabel).caption:=s;
            if (c is TSpeedButton) then (c as TSpeedButton).hint:=s;
            if (c is TButton) then (c as TButton).caption:=s;
            if (c is TRadioButton) then (c as TRadioButton).caption:=s;
            if (c is TMenuitem) then
            begin
              //if ((c as TMenuItem).parent=(c as TMenuItem)) then
              //if ((c as TMenuItem).parent.caption='') then
              if isMainTopItem(c as TMenuItem) then
                if (pos(#9,s)>0) then
                  s:=copy(s,1,pos(#9,s)-1);
              (c as TMenuitem).caption:=s;
            end;
            if (c is TToolButton) then (c as TToolbutton).Hint:=s;
            if (c is TTabsheet) then (c as TTabsheet).caption:=s;
            if (c is TGroupBox) then (c as TGroupBox).caption:=s;
            //if (c is TSpeedButton) then (c as TToolbutton).Hint:=s;
            if (c is TRadiogroup) then
            begin
              (c as TRadioGroup).caption:=s;
              //if not (c=Form_TsctMain.RdGrp_Eol) then
              //begin
                for j:=1 to (c as TRadioGroup).items.count do
                begin
                  s:=ConvertLineBreak(ini.ReadString('controls',c.name+'.'+inttostr(j),''));
                  if s<>'' then (c as TRadioGroup).items.Strings[j-1]:=s;
                end; //for radiogroup.items
              //end; //c=RdGrp_eol
            end; //c is TRadiogroup

          end; //s<>''
        end; //not a font-panel
      end; //c is TComponent
    end; //for 0 to component.count-1
    ini.free;
  end; //languagefile present
end;

function localizeString(value:string):string;
var ini:Tinifile;
    s:string;
begin
  ini:=Tinifile.create(extractfilepath(paramstr(0))+lng_prefix+cfg.language+'.lng');
  s:=convertLineBreak(ini.ReadString('Strings',value,''));
  if s='' then
  begin
    if value='Msg_ObjectExists' then s:='Objektname schon vergeben!';
    if value='Msg_Error' then s:='Fehler';
    if value='Msg_NothingSelected' then s:='Kein Eintrag markiert';
    if value='Msg_TextNotFound' then s:='Text nicht gefunden';
    if value='Msg_NoSearchText' then s:='kein Suchtext angegeben';
    if value='Msg_MustRestart' then s:='Sie müssen DFM-Editor neu starten, um diese Funktion anzuwenden';

    if value='Msg_NoOutput' then s:='Keine Ausgabe generiert';
    if value='Msg_NoForms' then s:='keine Formulare gefunden';
    if value='Msg_FailedToOpen' then s:='Fehler beim Laden';
    if value='Msg_ReservedWord' then s:='Bezeichner (%s) wird intern verwendet.';
    if value='Msg_WrongLength' then s:='Bezeichner (%s) hat eine ungültige Länge.'+#13+'Bezeichner dürfen 2 bis 63 Zeichen enthalten!';
    if value='Msg_InvalidChar' then s:='Bezeichner (%s) enthält ungültige Zeichen.'+#13'Ein Bezeichner darf aus'+#13+'  - Buchstaben (A-Z),'+#13+'  - Ziffern (0-9) und'+#13+'  - dem Unterstrich (_)'+#13+'bestehen, wobei das erste Zeichen keine Ziffer sein darf.';
    if value='Msg_EmptyFields' then s:='Feld(er) ''%s'' muss ausgefüllt werden!';
    //if value='Msg_SaveDFMFirst' then s:='Sie müssen zuerst das Formular speichern!';
    if value='Msg_GetVersionError' then s:='Fehler beim Holen der Versioninfos';
    if value='Msg_NoNewVersion' then s:='nix neues (%s)';
    if value='Msg_ConvertError' then s:='Fehler beim konvertieren';
    if value='Ask_VisitWebpage' then s:='neuere Version vorhanden (%s)'+#13#13+
      'Möchten Sie zur Projektseite wechseln, um die neue Version zu downloaden?';
    if value='Ask_DeleteObject' then
      s:='Möchten Sie dieses Objekt (%s) inklusive aller untergeordneter Objekte löschen?';
    if value='Ask_DeleteProperty' then s:='Möchten Sie diese Eigenschaft (%s) löschen?';
    //if value='Ask_Overwrite' then s:='Möchten Sie die vorhandene Datei überschreiben?';
    if value='Ask_LoadCreate' then s:='Wenn Sie dies tun, wird der Baum mit den neuen DFM-Daten überschrieben.'+#13+'Alle Änderungen seit dem letzten Speichern gehen dabei verloren.'#13'Wollen Sie das wirklich?';
    if Value='Ask_SaveChanges' then s:='Änderungen speichern?';
    if value='Ask_SureQuit' then s:='Alle Änderungen nach dem letzten Speichern gehen verloren.'+#13+'Wollen Sie DFMEdit wirklich beenden?';
    if value='Ask_ReportBug' then s:='Möchten Sie diesen Fehler melden?';
    if value='Ask_ReplaceProperty' then s:='Eigenschaft existiert existiert bereits. Möchten Sie die vorhandene überschreiben?';
    //if value='Wng_ReplacingUnit' then s:='Achtung: Sie versuchen eine vorhandene Pas-Datei zu überschreiben.'+#13+'Dabei werden die vorhandenen Prozeduren und Funktionen NICHT übernommen!';
    if value='Cap_NewObject' then s:='neues Objekt';
    if value='Cap_NewProperty' then s:='neue Eigenschaft';
    if value='Cap_EditObject' then s:='Objekt bearbeiten';
    if value='Cap_EditProperty' then s:='Eigenschaft bearbeiten';
    if value='Cap_Unknown' then s:='unbekannt';
    if value='Cap_Error' then s:='Fehler';
    if value='Cap_Question' then s:='Frage';
    if value='Cap_Line' then s:='Zeile';
    if Value='Cap_EntriesDeleted' then s:='%d Einträge gelöscht';
    if value='Cap_ChooseFolder' then s:='Bitte wählen Sie einen Ordner:';

    if value='Cap_Left' then s:='Links';
    if value='Cap_Top' then s:='Oben';
    if value='Cap_Height' then s:='Höhe';
    if value='Cap_Width' then s:='Breite';

    if value='Cap_AboutForm' then s:='Über-Dialog';
    if value='Cap_AddForm' then s:='Bearbeiten-Dialog';
    if value='Cap_ChooseResForm' then s:='Form-Auswahl';
    if value='Cap_InspectorForm' then s:='Inspektor';
    if value='Cap_MainForm' then s:='Hauptfenster';
    if value='Cap_OptionsForm' then s:='Einstellungen';
    if value='Cap_PaletteForm' then s:='Komponenten-Palette';

    if value='Cap_PropName' then s:='Eigenschaft';
    if value='Cap_PropValue' then s:='Wert';

    if value='Msg_PictureDataNotFound' then s:='kein Picture.Data in Zielkomponente gefunden';
    if value='Msg_ComponentNotFound' then s:='Komponente ''%s'' nicht gefunden';
    if value='Msg_PropertyNotSupported' then s:='Eigenschaft %s nicht unterstützt';
    if value='Msg_UnknownProperty' then s:='unbekannte Eigenschaft ''%s''';
    if value='Msg_ErrorSettingProperty' then s:='error setting property ''%s''';
    if value='Msg_ErrorLoadingPackage' then s:='Fehler beim Laden von Package ''%s''';
    if value='Msg_PackageNotFound' then s:='Package ''%s'' nicht gefunden';
    if value='Msg_LazresNotFound' then s:='Lazres (%s) wurde nicht gefunden\nlazres.exe wird zum Konvertieren der lfm in lrs benötigt.\nSie finden den Quellcode im Lazarus-Verzeichnis unter tools.';
    if value='Msg_ErrorWritingFile' then s:='Fehler beim Schreiben der Datei.\nStellen Sie sicher, dass Sie die nötigen Rechte haben und die Datei nicht durch ein anderes Programm gesperrt ist.';
    {$IFNDEF NOPACKAGES}
    if value='Msg_Only10Packages' then s:='nur 10 Packages erlaubt';
    {$ENDIF}
    if value='Msg_TypeOrValue' then s:='bitte Typ oder Wert abgeben!';
    if value='Ask_Delete' then s:='Wirklich löschen?';
    if value='Ask_Open' then s:='Datei öffnen?';
    if value='Msg_ValueMust' then s:='%s von %s muss mindestens %d sein!';
    if value='Msg_CannotLoad' then s:='Datei\n%s\nkann nicht geöffnet werden!';
    if value='Msg_FunctionNotFound' then s:='Function (%s) wurde nicht gefunden!'
  end;
  ini.free;
  result:=ConvertLineBreak(s);
end;

end.