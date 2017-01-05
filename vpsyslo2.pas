{VPSysLow extension unit by Jaroslaw Osadtchiy (JO) <2:5030/1082.53>}
{Modified for compatibility with DPMI32 by Aleksej Kozlov (Cat) <2:5030/1326.13>}
{Contribution to Dos Navigator /2 OSP project}
{&OrgName+,Speed+,AlignCode+,AlignRec-,CDecl-,Far16-,Frame+,Delphi+}
{$X+,W-,I-,J+,H-,Delphi+,R-,S-,Q-,B-,T-,Use32+}

unit VPSysLo2;

interface

uses
  VpSysLow
  {$IFDEF OS2}, Os2Def, Os2Base {$Undef KeyDll} {$ENDIF}
  {$IFDEF WIN32}, Windows, VpKbdW32 {$ENDIF}
  ;

function SysTVGetShiftState2: Byte;
{$IFDEF Win32}
  inline;
  begin
  Result := VpKbdW32.GetWinShiftState2;
  end;
{$ENDIF}
{$IFDEF DPMI32}
  inline;
  begin
  Result := 0;
  end;
{$ENDIF}

type
  POSSearchRec = ^TOSSearchRec;

  TOSSearchRecNew = packed record
    Handle: LongInt;
    NameLStr: Pointer;
    Attr: Byte;
    Time: LongInt;
    Size: Comp; {AK155 � ���� TSize}
    Name: ShortString;
    CreationTime: LongInt;
    LastAccessTime: LongInt;
    Filler: array[0..3] of Char;
    {$IFDEF OS2}
    {AK155 ��⠥� �� DosFindNext �� ����� 䠩��� �� ࠧ.
   ���� �� 2k �� ������ �।��� ����� �������� ������
   ��� �� 40 ����ᥩ ��⠫���}
    //JO: ��������! ࠧ��� FindBuf ������ ���� ᮣ��ᮢ�� � ࠧ��஬ �������筮�
    //    ��६����� � _Defines.lSearchRec
    FindBuf: array[0..2*1024-1] of Byte;
    FindCount: integer; {�᫮ ����ࠡ�⠭��� ����ᥩ}
    FindPtr: ^FileFindBuf3; {��ࢠ� ����ࠡ�⠭��� ������}
    {$ENDIF}
    {$IFDEF WIN32}
    ShortName: ShortString;
    ExcludeAttr: LongInt;
    FindData: TWin32FindData;
    {$ENDIF}
    {$IFDEF DPMI32}
    attr_must: Byte;
    dos_dta:
    record
      fill: array[1..21] of Byte;
      Attr: Byte;
      Time: LongInt;
      Size: LongInt;
      Name: array[0..12] of Char;
      end;
    {$ENDIF}
    {$IFDEF LINUX}
    FindDir: array[0..255] of Char;
    FindName: ShortString;
    FindAttr: LongInt;
    {$ENDIF}
    end;

function SysFindFirstNew(Path: PChar; Attr: LongInt;
     var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
{$IFDEF DPMI32} inline;
  begin
  SysFindFirstNew := SysFindFirst(Path, Attr, POSSearchRec(@F)^, IsPChar)
    ;
  end; {$ENDIF}
function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
{$IFDEF DPMI32} inline;
  begin
  SysFindNextNew := SysFindNext(POSSearchRec(@F)^, IsPChar);
  end; {$ENDIF}
function SysFindCloseNew(var F: TOSSearchRecNew): LongInt;
{$IFDEF DPMI32} inline;
  begin
  SysFindCloseNew := SysFindClose(POSSearchRec(@F)^);
  end; {$ENDIF}

procedure SysTVKbdDone;
{$IFNDEF OS2} inline;
  begin
  end; {$ENDIF}

implementation

{&OrgName-}

uses
  Strings
  ;

{$IFDEF OS2}
function SysTVGetShiftState2: Byte;
  var
    Key: ^KbdInfo;
    LKey: array[1..2] of KbdInfo;

  begin
  Key := Fix_64k(@LKey, SizeOf(Key^));
  Key^.cb := SizeOf(KbdInfo);
  KbdGetStatus(Key^, 0);
  Result := Hi(Key^.fsState);
  end;
{$ENDIF}

type
  TDateTimeRec = record
    FTime, FDate: SmallWord;
    end;

  {$IFDEF Win32}
function SetResult(Success: Boolean): LongInt;
  begin
  Result := 0;
  if not Success then
    Result := GetLastError;
  end;

function DoFindFileNew(var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
  var
    LocalFileTime: TFileTime;
    ExclAttr: LongInt;
    InclAttr: LongInt;
  type
    CompRec = record
      Lo, Hi: LongInt
      end;
  begin
  // Extract Include/Exclude attributes from F.ExcludeAttr field
  ExclAttr := not F.ExcludeAttr and (file_Attribute_Hidden or
       file_Attribute_System or $8 or file_Attribute_Directory or
       file_Attribute_Archive);
  InclAttr := (F.ExcludeAttr and $FF00) shr 8;
  // Make sure attributes are not both excluded and included
  ExclAttr := ExclAttr and not InclAttr;
  with F do
    begin
    // Reject entries where
    // - Attributes that are excluded are present.
    // - Attributes that must be present are not all there
    while (FindData.dwFileAttributes and ExclAttr <> 0) or
        (FindData.dwFileAttributes and InclAttr <> InclAttr)
    do
      if not FindNextFile(Handle, FindData) then
        begin
        Result := GetLastError;
        exit;
        end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, TDateTimeRec(Time).FDate,
       TDateTimeRec(Time).FTime);
    FileTimeToLocalFileTime(FindData.ftCreationTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime,
       TDateTimeRec(CreationTime).FDate, TDateTimeRec(CreationTime).FTime);
    FileTimeToLocalFileTime(FindData.ftLastAccessTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime,
       TDateTimeRec(LastAccessTime).FDate,
       TDateTimeRec(LastAccessTime).FTime);
    CompRec(Size).Lo := FindData.nFileSizeLow;
    CompRec(Size).Hi := FindData.nFileSizeHigh;
    Attr := FindData.dwFileAttributes;
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
  end { DoFindFileNew };

function SysFindFirstNew(Path: PChar; Attr: LongInt;
     var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
  var
    AnsiPath: array[0..260] of Char; {AK155}
  begin
  F.ExcludeAttr := Attr;
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

function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
  begin
  if FindNextFile(F.Handle, F.FindData) then
    Result := DoFindFileNew(F, IsPChar)
  else
    Result := GetLastError;
  end;

function SysFindCloseNew(var F: TOSSearchRecNew): LongInt;
  begin
  if F.Handle = invalid_Handle_Value then
    Result := 0
  else
    Result := SetResult(Windows.FindClose(F.Handle));
  F.Handle := invalid_Handle_Value;
  end;
{$ENDIF}

{$IFDEF OS2}

function SysFindFirstNew(Path: PChar; Attr: LongInt;
     var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
  var
    Count: LongInt;
    Path2: array[0..259] of Char;
  begin
  Attr := Attr and not $8; // No VolumeID under OS/2
  Count := 1;
  F.Handle := hdir_Create;
  F.FindPtr := @F.FindBuf;
  Result := DosFindFirst(Path, F.Handle, Attr,
      F.FindBuf, SizeOf(F.FindBuf), Count, fil_Standard);

  // If a specific error occurs, and the call is to look for directories, and
  // the path is a UNC name, then retry
  if  (Result = msg_Net_Dev_Type_Invalid) and
      (Hi(Attr) = $10) and
      (StrLen(Path) > Length('\\')) and
      (StrLComp(Path, '\\', Length('\\')) = 0)
  then
    begin
    DosFindClose(F.Handle);
    StrCat(StrCopy(Path2, Path), '\*.*');
    Result := DosFindFirst(Path2, F.Handle, Attr,
        F.FindBuf, SizeOf(F.FindBuf), Count, fil_Standard);
    F.FindPtr := @F.FindBuf;
    end;

  if Result = 0 then
    with F, F.FindPtr^ do
      begin
      Attr := attrFile;
      TDateTimeRec(Time).FTime := ftimeLastWrite;
      TDateTimeRec(Time).FDate := fdateLastWrite;
      TDateTimeRec(CreationTime).FTime := ftimeCreation;
      TDateTimeRec(CreationTime).FDate := fdateCreation;
      TDateTimeRec(LastAccessTime).FTime := ftimeLastAccess;
      TDateTimeRec(LastAccessTime).FDate := fdateLastAccess;
      Size := 0;
      Move(cbFile, Size, 4);
      if IsPChar then
        StrPCopy(PChar(@Name), achName)
      else
        Name := achName;
      FindCount := 0;
      end
  else
    F.Handle := hdir_Create;
  end { SysFindFirstNew };

function SysFindNextNew(var F: TOSSearchRecNew; IsPChar: Boolean): LongInt;
  begin
  if F.FindCount = 0 then
    begin
    F.FindCount := 100;
    Result := DosFindNext(F.Handle, F.FindBuf,
        SizeOf(F.FindBuf), F.FindCount);
    if Result <> 0 then
      exit;
    F.FindPtr := @F.FindBuf;
    end;
  with F, F.FindPtr^ do
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
    Dec(FindCount);
    Inc(PChar(FindPtr), oNextEntryOffset);
    Result := 0;
    end;
  end { SysFindNextNew };

function SysFindCloseNew(var F: TOSSearchRecNew): LongInt;
  begin
  if F.Handle = hdir_Create then
    Result := 0
  else
    Result := DosFindClose(F.Handle);
  end;
{$ENDIF}

{$IFDEF OS2}
procedure SysTVKbdDone;
  var
    Key: ^KbdInfo;
    LKey: array[1..2] of KbdInfo;

  begin
  Key := Fix_64k(@LKey, SizeOf(Key^));
  Key^.cb := SizeOf(KbdInfo);
  KbdGetStatus(Key^, 0); { Disable ASCII & Enable raw (binary) mode}
  Key^.fsMask := (Key^.fsMask and (not keyboard_Binary_Mode))
     or keyboard_Ascii_Mode;
  KbdSetStatus(Key^, 0);
  end;
{$ENDIF}

end.
