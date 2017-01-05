{  Extended Attributes handling unit                                     }
{                                                                        }
{  Partially based on:                                                   }
{  Extended Attributes handling library (c) 2000 by Alexander Trunov [2:5069/10, jnc@os2.ru]}

{$Use32+}
{$Delphi-}
{ Optimise-}

unit EAOper;

interface

uses
  Collect, Os2Def, Advance1;

type PEAItem = ^TEAItem;
     TEAItem = record
       C_ea: Pointer;
       C_ulEASize: Cardinal;
     end;

function EnumEAs(FName: String; Var coll: PStringCollection): Integer;

function RetrieveEA(FName: String; pszName: PChar; var ea: Pointer;
  var ulEASize: Cardinal; Silent: Boolean): Integer;
function StoreEA(FName: String; pszName: PChar; ea: Pointer;
  ulEASize: ULONG): Integer;

(*  {�㦭� ��� EABrowser'�}
function GetEAType(ea: Pointer): SmallWord;
*)
    {�㦭� ��� EditLongName}
function RetrieveStringSize(ea: Pointer): ULONG;
function RetrieveString(ea: Pointer; pszValue: PChar): PChar;
function BuildEAFromString(pszValue: PChar; var ulEASize: Cardinal): Pointer;


implementation

uses

 Strings, Os2Base, Messages, Advance2, FlTl;

function EnumEAs(FName: String; Var coll: PStringCollection): Integer;
var
  fst4: FILESTATUS4;
  ulEntry, ulCount, ulSize: ULONG;
  pvBuf: PFEA2;
  PS: PChar;
  PSArr: array[0..255] of Char;
  Result: Integer;
begin
  PS := PSArr;
  PS := StrPCopy (PS, FName);
  Result := DosQueryPathInfo(PS, FIL_QUERYEASIZE, fst4, SizeOf(fst4));
  if (Result = NO_ERROR)
     and (fst4.cbList > 0) then {JO: 30-07-2002 - ������� �� �᫮���, �.�.  }
                                {    ���ਬ�� �� ����஢���� � ISO-��ࠧ��, }
                                {    �������஢����� �१ NDFS 諠         }
                                {    �����᫥���� �㣠�� �� �������������   }
                                {    ������ ᯨ᮪ EA                     }
  begin
    ulSize := fst4.cbList * 2;
    GetMem(pvBuf, ulSize);
    ulEntry := 1;
    while True do
    begin
      ulCount := 1;
      Result := DosEnumAttribute(enumea_Reftype_Path, PS,
        ulEntry, pvBuf^, ulSize, ulCount, ENUMEA_LEVEL_NO_VALUE);
      if Result = NO_ERROR then
      begin
        if ulCount = 0 then
          Break;
        coll^.AtInsert(coll^.Count, NewStr(StrPas(PChar(@pvBuf^.szName))+#0));
        Inc(ulEntry, ulCount);
      end
      else
        Break;
    end;
    FreeMem(pvBuf);
  end;
  EnumEAs := Result;
end;

function RetrieveEA(FName: String; pszName: PChar; var ea: Pointer;
  var ulEASize: Cardinal; Silent: Boolean): Integer;
var
  ulFEASize, ulGEASize, ulOffset: ULONG;
  eaop: EAOP2;
  fst4: FILESTATUS4;
  PS: PChar;
  PSArr: array[0..255] of Char; {???}
  Result: Longint;
begin
{( *}
  PS := PSArr; {???}
  PS := StrPCopy (PS, FName);

  Result := DosQueryPathInfo(PS, FIL_QUERYEASIZE, fst4, SizeOf(fst4));

  if (Result = NO_ERROR)
     and (fst4.cbList > 0) then
  begin

    ulFEASize := 4 + StrLen(pszName) + 1 + fst4.cbList * 2; // approx. :)
    ulGEASize := 4 + 4 + 1 + StrLen(pszName) + 1;

{JO: 31-07-2002  ���������� ��ப� - ���䨪� ������� �� ����⪥           }
{    ।���஢��� .LONGNAME �� NDFS . ��宦�, �஡���� � ⮬ ��           }
{    DosQueryPathInfo �� ���ࠩ���᪨� ��᪠� ��� 䠩��� ��� EA �뤠��     }
{    ���祭�� fst4.cbList �� 4, ��� ��� ��ଠ���� ��᪮�, � 2              }

    if ulFEASize < ulGEASize then ulFEASize := ulGEASize + 4;

    GetMem(eaop.fpFEA2List, ulFEASize);
    GetMem(eaop.fpGEA2List, ulGEASize);

    eaop.fpGEA2List^.cbList := ulGEASize;
    eaop.fpGEA2List^.list[0].oNextEntryOffset := 0;
    eaop.fpGEA2List^.list[0].cbName := StrLen(pszName);
    StrCopy(eaop.fpGEA2List^.list[0].szName, pszName);

    eaop.fpFEA2List^.cbList := ulFEASize;

    Result := DosQueryPathInfo(PS, FIL_QUERYEASFROMLIST, eaop,
      SizeOf(eaop));

    if Result = NO_ERROR then
    begin

      ulOffset := ULONG(@eaop.fpFEA2List^.list[0]) +
        eaop.fpFEA2List^.list[0].cbName + SizeOf(FEA2);

      ulEASize := eaop.fpFEA2List^.list[0].cbValue;

      {ea :=} GetMem(ea, ulEASize);

      Move(Pointer(ulOffset)^, ea^, ulEASize);

    end
    else
    begin
      if not Silent then MessageBox(StrPas(pszName)+#3'Failed to QUERY EAS FROM LIST , rc ::= %d.', @result, mfError or mfOkButton);
    end;

    FreeMem(eaop.fpGEA2List);
    FreeMem(eaop.fpFEA2List);

  end
  else
  begin
    if fst4.cbList = 0 then Result := 48; {JO: �訡�� 48 � �� ��१�ࢨ஢���}
    if not Silent then MessageBox(#3'Failed to QUERY EA SIZE for '+ FName+ ' , rc ::= %d.', @result, mfError or mfOkButton);
  end;

RetrieveEA := Result; {* ) RetrieveEA :=0;}
end;

function StoreEA(FName: String; pszName: PChar; ea: Pointer;
  ulEASize: ULONG): Integer;
var
  eaop: EAOP2;
  ulFEASize, ulOffset: ULONG;
  PS: PChar;
  PSArr: array[0..255] of Char;
  Result: Longint;
  FAttr: Word;
  LWr, Cr, LAc: Longint;
begin

  FAttr:=GetFileAttr(FName);
  GetFileAges(FName, LWr, Cr, LAc);
  SetFileAttr(FName, FAttr and not Dos.ReadOnly);

  PS := PSArr;
  PS := StrPCopy (PS, FName);

  ulFEASize := 4 + 4 + 1 + 1 + 2 + StrLen(pszName) + 1 + ulEASize;

  GetMem(eaop.fpFEA2List, ulFEASize);

  eaop.fpFEA2List^.cbList := ulFEASize;

  with eaop.fpFEA2List^.list[0] do
  begin

    oNextEntryOffset := 0;
    fEA := 0;
    cbName := StrLen(pszName);
    cbValue := ulEASize;
    StrCopy(PChar(@szName), pszName);
    ulOffset := ULONG(@eaop.fpFEA2List^.list[0]) + cbName + SizeOf(FEA2);
    Move(ea^, Pointer(ulOffset)^, ulEASize);

  end;

  Result := DosSetPathInfo(PS, FIL_QUERYEASIZE, eaop, SizeOf(eaop),dspi_WrtThru);

  FreeMem(eaop.fpFEA2List);

SetFileAttr(FName, FAttr);
SetFileAges(FName, LWr, Cr, LAc);
StoreEA := Result;
end;

(*
function GetEAType(ea: Pointer): SmallWord;
begin

  if ea = nil then
    Result := 0
  else
    Result := PUSHORT(ea)^;

end;
*)

function RetrieveStringSize(ea: Pointer): ULONG;
begin
 if ea = nil then RetrieveStringSize := 0
   else RetrieveStringSize := PUSHORT(ULONG(ea) + 2)^;
end;

function RetrieveString(ea: Pointer; pszValue: PChar): PChar;
var
  ulLen: ULONG;
begin

  ulLen := RetrieveStringSize(ea);

  if ulLen = 0 then pszValue[0] := #0
    else StrLCopy(pszValue, PChar(ULONG(ea) + 4), ulLen);

  RetrieveString := pszValue;

end;

{$Delphi+}
function BuildEAFromString(pszValue: PChar; var ulEASize: Cardinal): Pointer;
{Var Result: Pointer;}
begin
  ulEASize := StrLen(pszValue) + 1 + 2 + 2;
  GetMem(Result, ulEASize);
  PUSHORT(Result)^ := EAT_ASCII;
  PUSHORT(ULONG(Result) + 2)^ := StrLen(pszValue);
  StrLCopy(PChar(ULONG(Result) + 4), pszValue, StrLen(pszValue));
end;

end.
