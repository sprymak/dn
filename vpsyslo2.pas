{VPSysLow extension unit by Jaroslaw Osadtchiy (JO) <2:5030/1082.53>}
{Contribution to Dos Navigator /2 OSP project}
{&OrgName+,Speed+,AlignCode+,AlignRec-,CDecl-,Far16-,Frame+,Delphi+}
{$X+,W-,I-,J+,H-,Delphi+,R-,S-,Q-,B-,T-,Use32+}

unit VPSysLo2;

interface

uses VPSysLow
{$IFDEF OS2}, Os2Def, Os2Base  {$Undef KeyDll} {$ENDIF}
{$IFDEF WIN32}  ,Windows                       {$ENDIF};

function SysTVGetShiftState2: Byte;
{$IFNDEF OS2}
inline; begin SysTVGetShiftState2 := 0; end;
{$ENDIF}
(*
function SysTVSetShiftState(DShiftState: SmallWord): Byte;
{$IFNDEF OS2}
inline; begin SysTVSetShiftState := 0; end;
{$ENDIF}
*)

type

  TOSSearchRecNew = packed record
    Handle: Longint;
    NameLStr: Pointer;
    Attr: Byte;
    Time: Longint;
    Size: Longint;
    Name: ShortString;
    CreationTime: Longint;
    LastAccessTime: Longint;
    Filler: array[0..3] of Char;
{$IFDEF WIN32}
    ShortName:   ShortString;
    ExcludeAttr: Longint;
    FindData:    TWin32FindData;
{$ENDIF}
{$IFDEF DPMI32}
    attr_must:byte;
    dos_dta:
      record
        Fill: array[1..21] of Byte;
        Attr: Byte;
        Time: Longint;
        Size: Longint;
        Name: array[0..12] of Char;
      end;
{$ENDIF}
{$IFDEF LINUX}
    FindDir:  array[0..255] of Char;
    FindName: ShortString;
    FindAttr: LongInt;
{$ENDIF}
  end;

function SysFindFirstNew(Path: PChar; Attr: Longint; var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
function SysFindCloseNew(var F: TOSSearchRecNew): Longint;

implementation

{&OrgName-}

uses
  Strings{$IFDEF Win32}, DnIni{$ENDIF};

{$IFDEF OS2}
function SysTVGetShiftState2: Byte;
var
  Key  : ^KbdInfo;
  LKey : Array[1..2] of KbdInfo;

begin
  Key := Fix_64k(@LKey, SizeOf(Key^));
  Key^.cb := SizeOf(KbdInfo);
  KbdGetStatus(Key^, 0);
  Result := Hi(Key^.fsState);
end;

(*
function SysTVSetShiftState(DShiftState: SmallWord): Byte;
var
  Key  : ^KbdInfo;
  LKey : Array[1..2] of KbdInfo;
begin
  Key := Fix_64k(@LKey, SizeOf(Key^));
  Key^.cb := SizeOf(KbdInfo);
  Key^.fsState := {(Hi(Key^.fsState) shl 16) +} DShiftState;
  Result := KbdSetStatus(Key^, 0);
end;
*)
{$ENDIF}

type
  TDateTimeRec = record
    FTime,FDate: SmallWord;
  end;

{$IFDEF Win32}

function SetResult(Success: Boolean): Longint;
begin
  Result := 0;
  if not Success then
    Result := GetLastError;
end;

function DoFindFileNew(var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
var
  LocalFileTime: TFileTime;
  ExclAttr: Longint;
  InclAttr: Longint;
begin
  // Extract Include/Exclude attributes from F.ExcludeAttr field
  ExclAttr := not F.ExcludeAttr and (file_Attribute_Hidden or file_Attribute_System or $8 or file_Attribute_Directory or file_Attribute_Archive);
  InclAttr := (F.ExcludeAttr and $FF00) shr 8;
  // Make sure attributes are not both excluded and included
  ExclAttr := ExclAttr and not InclAttr;
  with F do
  begin
    // Reject entries where
    // - Attributes that are excluded are present.
    // - Attributes that must be present are not all there
    while (FindData.dwFileAttributes and ExclAttr <> 0) or
      (FindData.dwFileAttributes and InclAttr <> InclAttr) do
      if not FindNextFile(Handle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).FDate, TDateTimeRec(Time).FTime);
    FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(CreationTime).FDate, TDateTimeRec(CreationTime).FTime);
    FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(LastAccessTime).FDate, TDateTimeRec(LastAccessTime).FTime);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    if RecodeCyrillicNames then
      begin    // Convert filename to OEM character set
        CharToOem(FindData.cFileName, FindData.cFileName);
        CharToOem(FindData.cAlternateFileName, FindData.cAlternateFileName); {AK155}
      end;
    if IsPChar then
      begin
        StrCopy(PChar(@Name), FindData.cFileName);
        StrCopy(PChar(@ShortName), FindData.cAlternateFileName);
      end
    else
      begin
        Name := StrPas(FindData.cFileName);
        ShortName := StrPas(FindData.cAlternateFileName);
      end;
  end;
  Result := 0;
end;

function SysFindFirstNew(Path: PChar; Attr: Longint; var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
var
  AnsiPath: array[0..260] of char;  {AK155}
begin
  F.ExcludeAttr := Attr;
  if RecodeCyrillicNames then
    begin
      OemToChar(Path, AnsiPath);  {AK155}
      F.Handle := FindFirstFile(AnsiPath, F.FindData); {AK155}
    end
   else
      F.Handle := FindFirstFile(Path, F.FindData);
  if F.Handle <> invalid_Handle_Value then
    begin
      Result := DoFindFileNew(F, IsPChar);
      if Result <> 0 then
        begin
          FindClose(F.Handle);
          F.Handle := invalid_Handle_Value;
        end;
    end
  else
    Result := GetLastError;
end;

function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
begin
  if FindNextFile(F.Handle, F.FindData) then
    Result := DoFindFileNew(F, IsPChar)
  else
    Result := GetLastError;
end;

function SysFindCloseNew(var F: TOSSearchRecNew): Longint;
begin
  if F.Handle = invalid_Handle_Value then
    Result := 0
  else
    Result := SetResult(Windows.FindClose(F.Handle));
  F.Handle := invalid_Handle_Value;
end;
{$ENDIF}

{$IFDEF OS2}
function SysFindFirstNew(Path: PChar; Attr: Longint; var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
var
  Count: Longint;
  SR: FileFindBuf3;
  Path2: array[0..259] of char;
begin
  Attr := Attr and not $8; // No VolumeID under OS/2
  Count := 1;
  F.Handle := hdir_Create;
  Result := DosFindFirst(Path, F.Handle, Attr, SR, SizeOf(SR), Count, fil_Standard);

  // If a specific error occurs, and the call is to look for directories, and
  // the path is a UNC name, then retry
  if (Result = msg_Net_Dev_Type_Invalid) and
     (Hi(Attr) = $10) and
     (StrLen(Path) > Length('\\')) and
     (StrLComp(Path, '\\', Length('\\')) = 0) then
    begin
      DosFindClose(F.Handle);
      StrCat(StrCopy(Path2,Path), '\*.*');
      Result := DosFindFirst(Path2, F.Handle, Attr, SR, SizeOf(SR), Count, fil_Standard);
      if (Result = 0) and (Count <> 0) then
        Result := 0;
    end;

  if Result = 0 then
    with F,SR do
    begin
      Attr := attrFile;
      TDateTimeRec(Time).FTime := ftimeLastWrite;
      TDateTimeRec(Time).FDate := fdateLastWrite;
      TDateTimeRec(CreationTime).FTime := ftimeCreation;
      TDateTimeRec(CreationTime).FDate := fdateCreation;
      TDateTimeRec(LastAccessTime).FTime := ftimeLastAccess;
      TDateTimeRec(LastAccessTime).FDate := fdateLastAccess;
      Size := cbFile;
      if IsPChar then
        StrPCopy(PChar(@Name), achName)
      else
        Name := achName;
    end
  else
    F.Handle := hdir_Create;
end;

function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): Longint;
var
  Count: Longint;
  SR: FileFindBuf3;
begin
  Count := 1;
  Result := DosFindNext(F.Handle, SR, SizeOf(SR), Count);
  if Result = 0 then
    with F,SR do
    begin
      Attr := attrFile;
      TDateTimeRec(Time).FTime := ftimeLastWrite;
      TDateTimeRec(Time).FDate := fdateLastWrite;
      TDateTimeRec(CreationTime).FTime := ftimeCreation;
      TDateTimeRec(CreationTime).FDate := fdateCreation;
      TDateTimeRec(LastAccessTime).FTime := ftimeLastAccess;
      TDateTimeRec(LastAccessTime).FDate := fdateLastAccess;
      Size := cbFile;
      if IsPChar then
        StrPCopy(PChar(@Name), achName)
      else
        Name := achName;
    end;
end;

function SysFindCloseNew(var F: TOSSearchRecNew): Longint;
begin
  if F.Handle = hdir_Create then
    Result := 0
  else
    Result := DosFindClose(F.Handle);
end;

{$ENDIF}

end.
