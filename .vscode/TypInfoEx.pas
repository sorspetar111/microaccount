{Copyright:      Heiko Behrens, Hagen Reddmann
 Author:         Heiko Behrens (Initiator and Developer), Hagen Reddmann
 Revision:       1.4 (2005-08-14)
 Descriptions:   TypeInfoEx allows RTTI retrieval of all modules (BPLs, Dlls) in
                 a comfortable and reversed way
 Versions:       Delphi 5 and above, testet on D5, D7, D2005
 Remarks:        This Copyright must not be removed

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

}
{$IFDEF VER170}{$DEFINE TYPINFOEX_7UP}{$ENDIF}
{$IFDEF VER150}{$DEFINE TYPINFOEX_7UP}{$ENDIF}
{$IFDEF VER140}{$DEFINE TYPINFOEX_7UP}{$ENDIF}
unit TypInfoEx;

interface

uses
  TypInfo;

type
  TTypeList = array of PTypeInfo;

function TypeList(const ATypeInfos: array of PTypeInfo): TTypeList;

{
  EnumTypeInfos functions enumerate all type infos available in programm/module/list.
  For each type info the enumeration callback function/method is called.
  Enumeration continues until there are no more type infos or the callback returns True.

  The return value is the type info at which the callback returns True, otherwise nil or
  If you pass nil as callback EnumTypeInfos returns the first type info found.
}
type
  TEnumTypeInfoFunc = function(AUserData: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
  TEnumTypeInfoMethod = function(ATypeInfo: PTypeInfo): Boolean of object;

function EnumTypeInfos(AFunc: TEnumTypeInfoFunc; AUserData: Pointer = nil): PTypeInfo; overload;
function EnumTypeInfos(AModule: Longword; AFunc: TEnumTypeInfoFunc; AUserData: Pointer = nil): PTypeInfo; overload;
function EnumTypeInfos(const AList: TTypeList; AFunc: TEnumTypeInfoFunc; AUserData: Pointer = nil): PTypeInfo; overload;

function EnumTypeInfos(AMethod: TEnumTypeInfoMethod = nil): PTypeInfo; overload;
function EnumTypeInfos(AModule: Longword; AMethod: TEnumTypeInfoMethod): PTypeInfo; overload;
function EnumTypeInfos(const AList: TTypeList; AMethod: TEnumTypeInfoMethod): PTypeInfo; overload;


{
  CollectTypeInfos functions collect all type infos available in programm/module/list.
  For each type info the selection callback function/method is called

  The return value is the list of type info at which the callback returns True.
  If you pass nil as callback the CollectTypeInfos returns every type info found.
}
type
  TTypeInfoSelect = TEnumTypeInfoFunc;
  TTypeInfoSelectMethod = TEnumTypeInfoMethod;

function CollectTypeInfos(ASelect: TTypeInfoSelect; AUserData: Pointer = nil): TTypeList; overload;
function CollectTypeInfos(AModule: Longword; ASelect: TTypeInfoSelect; AUserData: Pointer = nil): TTypeList; overload;
function CollectTypeInfos(const AList: TTypeList; ASelect: TTypeInfoSelect; AUserData: Pointer = nil): TTypeList; overload;

function CollectTypeInfos(ASelect: TTypeInfoSelectMethod = nil): TTypeList; overload;
function CollectTypeInfos(AModule: Longword; ASelect: TTypeInfoSelectMethod): TTypeList; overload;
function CollectTypeInfos(const AList: TTypeList; ASelect: TTypeInfoSelectMethod): TTypeList; overload;


{
  Returns a list of type infos of a specific kind in programm/list
  See TypInfo.TTypeKind for a list of available types
}
function GetTypeInfos(AKind: TTypeKind): TTypeList; overload;
function GetTypeInfos(const AList: TTypeList; AKind: TTypeKind): TTypeList; overload;


{
  Returns a list of type infos that are type-compatible to a specific class or interface
  e.g. GetInheritedTypeInfos(IUnknown/IInterface) returns every interface type info since every
       interface is derived from IUnknown/IInterface. Same for ~(TObject)
}
function GetInheritedTypeInfos(AInheritsFrom: TClass): TTypeList; overload;
function GetInheritedTypeInfos(const AList: TTypeList; AInheritsFrom: TClass): TTypeList; overload;
function GetInheritedTypeInfos(AInheritsFrom: TGUID): TTypeList; overload;
function GetInheritedTypeInfos(const AList: TTypeList; AInheritsFrom: TGUID): TTypeList; overload;


{
  Several helper routines to find a specific type info by name or GUID
  Returns nil if a proper type info cannot be found in program/list
}
function FindTypeInfo(const ATypeName: string): PTypeInfo; overload
function FindTypeInfo(const AList: TTypeList; ATypeName: string): PTypeInfo; overload;
function FindTypeInfo(const AGUID: TGUID): PTypeInfo; overload
function FindTypeInfo(const AList: TTypeList; const AGUID: TGUID): PTypeInfo; overload;


{
  Finds a class by name whithout any need of a user-defined registration
  mechanism as Classes.FindClass():TPersistent does.
  Returns nil if a class with the given name cannot be found in program/list
}
function FindClass(const AClassName: string): TClass; overload;
function FindClass(const AList: TTypeList; const AClassName: string): TClass; overload;


{
  Lists all types of interfaces implemented by a given class in the following order:
    Hierarchy (outer loop): Descendant to Ancestor
    Class (inner loop): Left to right as declared
    So, the first value in list will probably be the left-most interface of the given class
}
function GetInterfaceTypesOfClass(const AClass: TClass): TTypeList; overload;
function GetInterfaceTypesOfClass(const AList: TTypeList; AClass: TClass): TTypeList; overload;


{
  Helper routines dealing with modules. Useful when working with BPLs 
}
function FindModuleOfTypeInfo(ATypeInfo: PTypeInfo): Longword;
function ModuleHasType(AModule: Longword; ATypeInfo: PTypeInfo): Boolean;


{
  Returns a fully qualified type name such as 'Project1.Unit1.TType1'
  Module name can be ommited. Unit name might not be included, depending on
  RTTI's capabilities, e.g. QualifiedTypeName(TypeInfo(Integer)) = 'rtlXX.Integer'
}
function QualifiedTypeName(ATypeInfo: PTypeInfo; AnIncludeModuleName: Boolean = True): string;


{
  GetSortedTypeList functions creates a new sorted list of type infos based on
  a given list and a used-defined compare function/method (see TListSortCompare
  at delphi's online help for details). If ACompare is empty GetSortedTypeList
  simply returns a copy of the given list
}
type
  TTypeInfoCompare = function(AUserData: Pointer; ATypeInfo1, ATypeInfo2: PTypeInfo): Integer; register;
  TTypeInfoCompareMethod = function(ATypeInfo1, ATypeInfo2: PTypeInfo): Integer of object;

function GetSortedTypeList(const AList: TTypeList; ACompare: TTypeInfoCompare; AUserData: Pointer = nil): TTypeList; overload;
function GetSortedTypeList(const AList: TTypeList; ACompare: TTypeInfoCompareMethod): TTypeList; overload;

implementation
uses
  Windows,
  SysUtils;

function TypeList(const ATypeInfos: array of PTypeInfo): TTypeList;
var
  I: Integer;
begin
  SetLength(Result, Length(ATypeInfos));
  for I := Low(ATypeInfos) to High(ATypeInfos) do
    Result[I-Low(ATypeInfos)+Low(Result)] := ATypeInfos[I];
end;

{-------------------------------------------------------------------------------
   EnumTypeInfos
-------------------------------------------------------------------------------}
function EnumTypeInfos(AModule: Longword; AFunc: TEnumTypeInfoFunc; AUserData: Pointer): PTypeInfo; overload;
// copyright (c) 1998 Hagen Reddmann

  function GetBaseOfCode(AModule: LongWord; var ACodeStart, ACodeEnd: PChar): Boolean; register;
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
  PLongWord = ^LongWord;
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
        LongWord(P) := LongWord(P) and not 3;
        K := P + 4;
        if (PLongWord(P)^ = LongWord(K)) and (TTypeKind(K^) >= Low(TTypeKind)) and (TTypeKind(K^) <= High(TTypeKind)) then
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

  function EnumTypeInfosInModule(AModule: Longword; AData: PModulesEnumData): Boolean; register;
  begin
    with AData^ do
    begin
      AResult := EnumTypeInfos(AModule, AFunc, AUserData);
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

function EnumTypeInfos(const AList: TTypeList; AFunc: TEnumTypeInfoFunc; AUserData: Pointer): PTypeInfo;
var
  I: Integer;
begin
  // same semantic as in other versions: No EnumFunction means first entry
  Result := nil;
  for I := Low(AList) to High(AList) do
    if not Assigned(AFunc) or AFunc(AUserData, AList[I]) then
    begin
      Result := AList[I];
      Break;
    end;
end;

type
  TMethodToFuncRecord = record
    AFunc: TEnumTypeInfoFunc;
    AUserData: Pointer;
  end;
    
function MethodToFunc(AMethod: TEnumTypeInfoMethod): TMethodToFuncRecord;
begin
  if Assigned(AMethod) then
    with TMethod(AMethod) do
    begin
      Result.AFunc := TEnumTypeInfoFunc(Code);
      Result.AUserData := Data;
    end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function EnumTypeInfos(AMethod: TEnumTypeInfoMethod): PTypeInfo;
begin
  with MethodToFunc(AMethod) do
    Result := EnumTypeInfos(AFunc, AUserData);
end;

function EnumTypeInfos(AModule: Longword; AMethod: TEnumTypeInfoMethod): PTypeInfo;
begin
  with MethodToFunc(AMethod) do
    Result := EnumTypeInfos(AModule, AFunc, AUserData);
end;

function EnumTypeInfos(const AList: TTypeList; AMethod: TEnumTypeInfoMethod): PTypeInfo;
begin
  with MethodToFunc(AMethod) do
    Result := EnumTypeInfos(AList, AFunc, AUserData);
end;

{-------------------------------------------------------------------------------
   CollectTypeInfos
-------------------------------------------------------------------------------}
type
  TCollectData =  record
    ASelect: TTypeInfoSelect;
    AUserData: Pointer;
    ACount: Longword;
    AResult: TTypeList;
  end;
  PCollectData = ^TCollectData;
  

procedure InitializeCollectData(out AData: TCollectData; ASelect: TTypeInfoSelect; AUserData: Pointer);
begin
  AData.ASelect := ASelect;
  AData.AUserData := AUserData;
  AData.ACount := 0;
  AData.AResult := nil;
end;

function FinalizeCollectDataAndReturnItsResult(var AData: TCollectData): TTypeList;
begin
  SetLength(AData.AResult, AData.ACount);
  Result := AData.AResult;
end;  

function DoCollect(AData: PCollectData; ATypeInfo: PTypeInfo): Boolean; register;
begin
  with AData^ do
    if not Assigned(ASelect) or ASelect(AUserData, ATypeInfo) then
    begin
      if ACount mod 256 = 0 then
        SetLength(AResult, ACount + 256);
      AResult[ACount] := ATypeInfo;
      Inc(ACount);
    end;
  Result := False;
end;

function CollectTypeInfos(ASelect: TTypeInfoSelect; AUserData: Pointer): TTypeList;
var
  CollectData: TCollectData;
begin
  InitializeCollectData(CollectData, ASelect, AUserData);
  EnumTypeInfos(TEnumTypeInfoFunc(@DoCollect), @CollectData);
  Result := FinalizeCollectDataAndReturnItsResult(CollectData)
end;

function CollectTypeInfos(AModule: Longword; ASelect: TTypeInfoSelect; AUserData: Pointer): TTypeList;
var
  CollectData: TCollectData;
begin
  InitializeCollectData(CollectData, ASelect, AUserData);
  EnumTypeInfos(AModule, TEnumTypeInfoFunc(@DoCollect), @CollectData);
  Result := FinalizeCollectDataAndReturnItsResult(CollectData)
end;

function CollectTypeInfos(const AList: TTypeList; ASelect: TTypeInfoSelect; AUserData: Pointer): TTypeList;
var
  CollectData: TCollectData;
  I: Integer;
begin
  InitializeCollectData(CollectData, ASelect, AUserData);
  for I := Low(AList) to High(AList) do
    DoCollect(@CollectData, AList[i]);
  Result := FinalizeCollectDataAndReturnItsResult(CollectData)
end;

function CollectTypeInfos(ASelect: TTypeInfoSelectMethod): TTypeList;
begin
  with MethodToFunc(ASelect) do
    Result := CollectTypeInfos(AFunc, AUserData);
end;

function CollectTypeInfos(AModule: Longword; ASelect: TTypeInfoSelectMethod): TTypeList;
begin
  with MethodToFunc(ASelect) do
    Result := CollectTypeInfos(AModule, AFunc, AUserData);
end;

function CollectTypeInfos(const AList: TTypeList; ASelect: TTypeInfoSelectMethod): TTypeList;
begin
  with MethodToFunc(ASelect) do
    Result := CollectTypeInfos(AList, AFunc, AUserData);
end;

{-------------------------------------------------------------------------------
   GetTypeInfos(Kind)
-------------------------------------------------------------------------------}
function IsTypeCorrespondingToKind(AKind: Pointer; ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = TTypeKind(AKind);
end;

function GetTypeInfos(AKind: TTypeKind): TTypeList;
begin
  Result := CollectTypeInfos(IsTypeCorrespondingToKind, Pointer(AKind));
end;

function GetTypeInfos(const AList: TTypeList; AKind: TTypeKind): TTypeList;
begin
  Result := CollectTypeInfos(AList, IsTypeCorrespondingToKind, Pointer(AKind));
end;

{-------------------------------------------------------------------------------
   GetInheritedTypeInfos(BaseClass)
-------------------------------------------------------------------------------}
function IsTypeCorrespondingToInheritedClass(AClass: Pointer; ATypeInfo: PTypeInfo): Boolean;
begin
  Result := (ATypeInfo.Kind = tkClass) and GetTypeData(ATypeInfo).ClassType.InheritsFrom(TClass(AClass));
end;

function GetInheritedTypeInfos(AInheritsFrom: TClass): TTypeList; overload;
begin
  Result := CollectTypeInfos(IsTypeCorrespondingToInheritedClass, AInheritsFrom);
end;

function GetInheritedTypeInfos(const AList: TTypeList; AInheritsFrom: TClass): TTypeList; overload;
begin
  Result := CollectTypeInfos(AList, IsTypeCorrespondingToInheritedClass, AInheritsFrom);
end;

{-------------------------------------------------------------------------------
   GetInheritedTypeInfos(GUID)
-------------------------------------------------------------------------------}

function AreGUIDsEqual(const AGUID1, AGUID2: TGUID): Boolean;
begin
  Result := (Int64(AGUID1.D1) = Int64(AGUID2.D1)) and
            (Int64(AGUID1.D4) = Int64(AGUID2.D4));
end;

function IsTypeCorrespondingToInheritedInterface(AGUID: Pointer; ATypeInfo: PTypeInfo): Boolean;
begin
  if ATypeInfo.Kind <> tkInterface then Result := False
  else
    with GetTypeData(ATypeInfo)^ do
      if (ifHasGuid in IntfFlags) and AreGUIDsEqual(GUID, PGUID(AGUID)^) then
        Result := True
      else
        Result := Assigned(IntfParent) and IsTypeCorrespondingToInheritedInterface(AGUID, IntfParent^);
end;

function GetInheritedTypeInfos(AInheritsFrom: TGUID): TTypeList;
begin
  Result := CollectTypeInfos(IsTypeCorrespondingToInheritedInterface, @AInheritsFrom);
end;

function GetInheritedTypeInfos(const AList: TTypeList; AInheritsFrom: TGUID): TTypeList; 
begin
  Result := CollectTypeInfos(AList, IsTypeCorrespondingToInheritedInterface, @AInheritsFrom);
end;

{-------------------------------------------------------------------------------
   FindTypeInfo(TypeName)
-------------------------------------------------------------------------------}
function IsTypeCorrespondingToName(AName: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
begin
  Result := AnsiCompareText(PChar(AName), ATypeInfo.Name) = 0;
end;

function FindTypeInfo(const ATypeName: string): PTypeInfo; overload
begin
  Result := EnumTypeInfos(IsTypeCorrespondingToName, PChar(ATypeName));
end;

function FindTypeInfo(const AList: TTypeList; ATypeName: string): PTypeInfo; overload;
begin
  Result := EnumTypeInfos(AList, IsTypeCorrespondingToName, PChar(ATypeName));
end;

{-------------------------------------------------------------------------------
   FindTypeInfo (GUID)
-------------------------------------------------------------------------------}
function IsTypeCorrespondingToGUID(AGUID: Pointer; ATypeInfo: PTypeInfo): Boolean; register;
begin
  if ATypeInfo.Kind <> tkInterface then
    Result := False
  else
    with GetTypeData(ATypeInfo)^ do
      Result := (ifHasGuid in IntfFlags) and AreGUIDsEqual(GUID, PGUID(AGUID)^);
end;

function FindTypeInfo(const AGUID: TGUID): PTypeInfo; overload
begin
  Result := EnumTypeInfos(IsTypeCorrespondingToGUID, @AGUID);
end;

function FindTypeInfo(const AList: TTypeList; const AGUID: TGUID): PTypeInfo; overload;
begin
  Result := EnumTypeInfos(AList, IsTypeCorrespondingToGUID, @AGUID);
end;

{-------------------------------------------------------------------------------
   FindClass (ClassName)
-------------------------------------------------------------------------------}
function TypeInfoToClass(ATypeInfo: PTypeInfo): TClass;
begin
  if not Assigned(ATypeInfo) or (ATypeInfo.Kind <> tkClass) then
    Result := nil
  else
    Result := GetTypeData(ATypeInfo).ClassType;
end;

function IsTypeCorrespondingToClassWithName(AName: PChar; ATypeInfo: PTypeInfo): Boolean; register;
begin
  // dont rely on IsTypeCorrespondingToName exclusively to bypass type coverage
  Result := (ATypeInfo.Kind = tkClass) and IsTypeCorrespondingToName(AName, ATypeInfo);
end;

function FindClass(const AClassName: string): TClass; overload;
begin
  Result := TypeInfoToClass(EnumTypeInfos(IsTypeCorrespondingToName, PChar(AClassName)));
end;

function FindClass(const AList: TTypeList; const AClassName: string): TClass; overload;
begin
  Result := TypeInfoToClass(EnumTypeInfos(AList, IsTypeCorrespondingToName, PChar(AClassName)));
end;


{-------------------------------------------------------------------------------
   Misc.
-------------------------------------------------------------------------------}
function FindModuleOfTypeInfo(ATypeInfo: PTypeInfo): Longword;
begin
  Result := FindHInstance(ATypeInfo);
end;

function ModuleHasType(AModule: Longword; ATypeInfo: PTypeInfo): Boolean;
begin
  Result := AModule = FindModuleOfTypeInfo(ATypeInfo);
end;

function GetInterfaceTypesOfClass(const AClass: TClass): TTypeList;
begin
  Result := GetInterfaceTypesOfClass(GetTypeInfos(tkInterface), AClass); 
end;

function GetInterfaceTypesOfClass(const AList: TTypeList; AClass: TClass): TTypeList;
var
  CollectData: TCollectData;
  IntfTable: PInterfaceTable;
  IntfType: PTypeInfo;
  I: Integer;
begin
  InitializeCollectData(CollectData, nil, nil);
  while Assigned(AClass) do
  begin
    IntfTable := AClass.GetInterfaceTable;
    if Assigned(IntfTable) then
      with IntfTable^ do
      for I := Pred(EntryCount) downto 0 do
      begin
        IntfType := FindTypeInfo(AList, Entries[I].IID);
        if Assigned(IntfType) then DoCollect(@CollectData, IntfType);
      end;
    AClass := AClass.ClassParent;
  end;
  Result := FinalizeCollectDataAndReturnItsResult(CollectData);
end;

function QualifiedTypeName(ATypeInfo: PTypeInfo; AnIncludeModuleName: Boolean): string;

  function GetModuleName(Module: HMODULE): string;
  var
    ModuleName: array[0..260] of Char;
  begin
    SetString(Result, ModuleName, GetModuleFileName(Module, ModuleName, SizeOf(ModuleName)));
    Result := ChangeFileExt(ExtractFileName(Result), '');
  end;

begin
  if Assigned(ATypeInfo) then
  begin
    // type itself
    Result := ATypeInfo^.Name;

    // optional unit name
    with GetTypeData(ATypeInfo)^ do
    case ATypeInfo^.Kind of
      tkClass:
        Result := UnitName + DotSep + Result;
      tkInterface:
        Result := IntfUnit  + DotSep + Result;
      {$IFDEF TYPINFOEX_7UP}
// Doesn't seem to work
//      {tkInteger, tkChar, }tkEnumeration, tkSet{, tkWChar}:
//        Result := EnumUnitName + '.' + Result;
      tkDynArray:
        Result := DynUnitName + DotSep + Result;
      {$ENDIF}
    else
      // no unit name -> module.type signals that type doesn't belog to a specfic unit
    end;

    // optional module name if not omitted
    if AnIncludeModuleName and (FindModuleOfTypeInfo(ATypeInfo)>0) then
      Result := GetModuleName(FindModuleOfTypeInfo(ATypeInfo)) + DotSep + Result;
  end;
end;

{-------------------------------------------------------------------------------
   GetSortedTypeList
-------------------------------------------------------------------------------}

function GetSortedTypeList(const AList: TTypeList; ACompare: TTypeInfoCompare; AUserData: Pointer): TTypeList;
  procedure QuickSort(L,R: Integer);
  var
    I,J: Integer;
    M,T: PTypeInfo;
  begin
    I := L;
    repeat
      L := I;
      J := R;
      M := Result[(L + R) shr 1];
      repeat
        while ACompare(AUserData, Result[I], M) < 0 do Inc(I);
        while ACompare(AUserData, Result[J], M) > 0 do Dec(J);
        if I > J then Break;
        T := Result[I];
        Result[I] := Result[J];
        Result[J] := T;
        Inc(I);
        Dec(J);
      until I > J;
      if L < J then QuickSort(L, J);
    until I >= R;
  end;

begin
  Result := Copy(AList);
  if Assigned(ACompare) and (Length(Result)>0) then
    QuickSort(Low(Result), High(Result));
end;

function GetSortedTypeList(const AList: TTypeList; ACompare: TTypeInfoCompareMethod): TTypeList;
begin
  if Assigned(ACompare) then
    with TMethod(ACompare) do
      Result := GetSortedTypeList(AList, TTypeInfoCompare(Code), Data)
  else
    Result := GetSortedTypeList(AList, nil);    
end;

end.
