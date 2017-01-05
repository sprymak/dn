{ Win32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - в WinNT поддерживается копирование Security Attributes
}

unit FlTl;
interface

{<fltl.001>}
type
  TDrvTypeNew = ( dtnFloppy, dtnHDD, dtnInvalid,
                 dtnCDRom, dtnLAN, dtnUnknown, dtnOptical, dtnProgram);

function GetBytesPerCluster(Drive: Byte): LongInt;
procedure CopyEAs(FFromName, FToName: String);
{$IFDEF WIN32}
procedure CopySAs(FFromName, FToName: String);
{$ENDIF}

function GetFileAges(S: ShortString;
     var Age_LWr, Age_Cr, Age_LAc: LongInt): LongInt;
{JO: возвращает время и дату последней модификации (Age_LWr),                   }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function SetFileAges(S: ShortString; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: устанавливает время и дату последней модификации (Age_LWr),                }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function GetFSString(Dr: Char): String; {JO}
function GetShare(Drive: Char): string; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}
//JO: Функция, которую следует использовать в исходниках DN/2
//    вместо неудачной штатной для VP RTL функции SysGetDriveType,
//    в отличие от которой данная функция определяет тип диска
//    никогда к нему не обращаясь - используя DosDevIOCtl и не
//    делая никаких проверок файловой системы

implementation

uses
  Windows, Strings, Lfn
  ;

function GetBytesPerCluster(Drive: Byte): LongInt;
  var
    RootPath: array[0..3] of Char;
    RootPtr: PChar;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters: DWord;
  begin
  RootPtr := nil;
  if Drive > 0 then
    begin
    RootPath[0] := Char(Drive+(Ord('A')-1));
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
    end;
  if GetDiskFreeSpace(RootPtr, SectorsPerCluster, BytesPerSector,
       FreeClusters, TotalClusters)
  then
    GetBytesPerCluster := SectorsPerCluster*BytesPerSector
  else
    GetBytesPerCluster := 0;
  end;

{Cat}
type
  PFullEAInformation = ^TFullEAInformation;
  TFullEAInformation = packed record
    NextEntryOffset: DWord;
    Flags: Byte;
    EaNameLength: Byte;
    EaValueLength: Word;
    EaName: array[0..0] of Char;
    end;

  {*** copy extended attributes ***}
procedure CopyEAs(FFromName, FToName: String);
  var
    Size: DWord;
    Ptr: Pointer;
    hFileSource: Handle;
    StreamId: TWin32StreamId;
    StreamSize: DWord;
    dwBytesWritten: DWord;
    lpContext: Pointer;
    sl, sh: DWord;
  begin
  FFromName := FFromName+#0;
  FToName := FToName+#0;

  (*** пока не работает
  hFileSource := CreateFile(@FFromName[1], FILE_READ_EA,
                            FILE_SHARE_READ or FILE_SHARE_WRITE,
                            nil, OPEN_EXISTING, 0, 0);
  if hFileSource <> INVALID_HANDLE_VALUE then
    begin
      Size := 0;
      lpContext := nil;
      StreamSize := SizeOf(TWin32StreamId) - SizeOf(WChar);
      while BackupRead(hFileSource, @StreamId,
                       StreamSize, dwBytesWritten,
                       False, False, lpContext) do
        begin
          {no more Stream IDs?}
          if dwBytesWritten = 0 then
            Break;

          {skip StreamName}
          if StreamId.dwStreamNameSize <> 0 then
            begin
              GetMem(Ptr, StreamId.dwStreamNameSize);
              if Ptr = nil then
                Break;
              if not BackupRead(hFileSource, Ptr,
                                StreamId.dwStreamNameSize, dwBytesWritten,
                                False, False, lpContext) then
                begin
                  FreeMem(Ptr);
                  Break;
                end;
              FreeMem(Ptr);
            end;

          {is it EA stream?}
          if StreamId.dwStreamId = BACKUP_EA_DATA then
            begin
              GetMem(Ptr, StreamId.Size.LowPart);
              if Ptr = nil then
                Break;
              if not BackupRead(hFileSource, Ptr,
                                StreamId.Size.LowPart, dwBytesWritten,
                                False, False, lpContext) then {EA read error}
                begin
                  FreeMem(Ptr);
                  Break;
                end;
              Size := StreamId.Size.LowPart;
              Break;
            end;

          {skip current stream}
          if not BackupSeek(hFileSource,
                            StreamId.Size.LowPart, StreamId.Size.HighPart,
                            sl, sh, lpContext) then
            Break;
        end;

      {free context}
      BackupRead(hFileSource, nil,
                 0, dwBytesWritten,
                 True, False, lpContext);
      CloseHandle(hFileSource);

      {write EAs}
      if Size > 0 then
        begin
          hFileSource := CreateFile(@FToName[1], FILE_WRITE_EA,
                                    FILE_SHARE_READ or FILE_SHARE_WRITE,
                                    nil, OPEN_EXISTING, 0, 0);
          if hFileSource <> INVALID_HANDLE_VALUE then
            begin
              lpContext := nil;
              StreamSize := SizeOf(TWin32StreamId) - SizeOf(WChar);
              StreamId.dwStreamId := BACKUP_EA_DATA;
              StreamId.dwStreamAttributes := 0;
              StreamId.Size.HighPart := 0;
              StreamId.Size.LowPart := Size;
              StreamId.dwStreamNameSize := 0;

              if BackupWrite(hFileSource, @StreamId,
                             StreamSize, dwBytesWritten,
                             False, False, lpContext) then
                if BackupWrite(hFileSource, Ptr,
                               Size, dwBytesWritten,
                               False, False, lpContext) then
                  {success!};

              {free context}
              BackupRead(hFileSource, nil,
                         0, dwBytesWritten,
                         True, False, lpContext);
              CloseHandle(hFileSource);
            end;

          FreeMem(Ptr);
        end;
    end;
  ***)
  end { CopyEAs };

{*** copy security attributes ***}
procedure CopySAs(FFromName, FToName: String);
  var
    Size: DWord;
    Ptr: Pointer;
  begin
  FFromName := FFromName+#0;
  FToName := FToName+#0;

  if not GetFileSecurity(@FFromName[1], DACL_Security_Information, nil,
       0, Size)
  then
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
      begin
      GetMem(Ptr, Size);
      if GetFileSecurity(@FFromName[1], DACL_Security_Information, Ptr,
           Size, Size)
      then
        SetFileSecurity(@FToName[1], DACL_Security_Information, Ptr);
      FreeMem(Ptr, Size);
      end;
  end;
{/Cat}

{JO}
type
  TDateTimeRec = record
    FTime, FDate: SmallWord;
    end;

function SetResult(Success: Boolean): LongInt;
  begin
  SetResult := 0;
  if not Success then
    SetResult := GetLastError;
  end;

function GetFileAges(S: ShortString;
     var Age_LWr, Age_Cr, Age_LAc: LongInt): LongInt;
  var
    LocalFileTime_LWr,
    LocalFileTime_Cr,
    LocalFileTime_LAc: TFileTime;
    FindData: TWin32FindData;
    SH: THandle;
  begin
  S[Length(S)+1] := #0;
  SH := FindFirstFile(@S[1], FindData);
  if SH = invalid_Handle_Value then
    begin
    Result := GetLastError;
    Exit;
    end;
  FindClose(SH);
  with FindData do
    Result := SetResult(
        FileTimeToLocalFileTime(ftLastWriteTime, LocalFileTime_LWr) and
        FileTimeToDosDateTime(LocalFileTime_LWr,
           TDateTimeRec(Age_LWr).FDate, TDateTimeRec(Age_LWr).FTime) and
        FileTimeToLocalFileTime(ftCreationTime, LocalFileTime_Cr) and
        FileTimeToDosDateTime(LocalFileTime_Cr,
           TDateTimeRec(Age_Cr).FDate, TDateTimeRec(Age_Cr).FTime) and
        FileTimeToLocalFileTime(ftLastAccessTime, LocalFileTime_LAc) and
        FileTimeToDosDateTime(LocalFileTime_LAc,
           TDateTimeRec(Age_LAc).FDate, TDateTimeRec(Age_LAc).FTime));
  end { GetFileAges };

function SetFileAges(S: ShortString; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    Handle: LongInt;
    LocalFileTime_LWr, FileTime_LWr,
    LocalFileTime_Cr, FileTime_Cr,
    LocalFileTime_LAc, FileTime_LAc: TFileTime;
  begin
  S[Length(S)+1] := #0;
  Handle := CreateFile(@S[1], GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = invalid_Handle_Value then
    Result := 1
  else
    SetFileAges := SetResult(
        DosDateTimeToFileTime(TDateTimeRec(Age_LWr).FDate,
             TDateTimeRec(Age_LWr).FTime, LocalFileTime_LWr) and
        LocalFileTimeToFileTime(LocalFileTime_LWr, FileTime_LWr) and
        DosDateTimeToFileTime(TDateTimeRec(Age_Cr).FDate,
             TDateTimeRec(Age_Cr).FTime, LocalFileTime_Cr) and
        LocalFileTimeToFileTime(LocalFileTime_Cr, FileTime_Cr) and
        DosDateTimeToFileTime(TDateTimeRec(Age_LAc).FDate,
             TDateTimeRec(Age_LAc).FTime, LocalFileTime_LAc) and
        LocalFileTimeToFileTime(LocalFileTime_LAc, FileTime_LAc) and
        SetFileTime(Handle, @FileTime_Cr, @FileTime_LAc, @FileTime_LWr));
  CloseHandle(Handle);
  end { SetFileAges };

function GetFSString(Dr: Char): String;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  var
    FSName: array[0..255] of Char;
    MaxLength: LongInt;
    FSFlags: LongInt;
  begin
  GetFSString := '';
  Root[0] := Dr;
  if GetVolumeInformation(Root, nil, 0, nil, MaxLength, FSFlags, FSName,
       SizeOf(FSName))
  then
    GetFSString := StrPas(FSName);
  end;

{/JO}

function GetShare(Drive: Char): string;
  const
    LocDrive: array[0..2] of char = 'C:'#0;
    NetPath: record
      lpPath: PChar;
      Buf: array[0..255] of Char;
      end = (lpPath: @LocDrive);
    lNetPath: DWORD = SizeOf(NetPath);
  var
    RC: Longint;
  begin
  Result := '';
  LocDrive[0] := Drive;
  RC := WNetGetUniversalName(
    @LocDrive,  // path for network resource
    UNIVERSAL_NAME_INFO_LEVEL,    // level of information
    @NetPath,      // name buffer
    lNetPath  // size of buffer
  );
  if RC = 0 then
    Result := StrPas(NetPath.lpPath);
  end;

function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {<fltl.001>}
const
  Root: Array[0..4] of char = 'C:\'#0;

begin
  Root[0] := Drive;
  Result := dtnInvalid;
  case GetDriveType(Root) of
    Drive_Fixed     : Result := dtnHDD;
    Drive_Removable : Result := dtnFloppy;
    Drive_CDRom     : Result := dtnCDROM;
    Drive_Remote    :
                      begin
                      if GetShare(Drive) = '' then
                             Result := dtnProgram
                        else Result := dtnLAN;
                      end;
    0, 1            : Result := dtnInvalid;
  else                Result := dtnUnknown;
  end;
end;

begin
end.
