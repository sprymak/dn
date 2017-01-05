{ Win32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - в WinNT поддерживается копирование Security Attributes
}

unit FlW32Tl;
interface

function GetBytesPerCluster(Drive: Byte): Longint;
{$IFDEF EAOP}
procedure CopyEAs (FFromName, FToName: String);
{$IFDEF WIN32}
procedure CopySAs (FFromName, FToName: String);
{$ENDIF}
{$ENDIF}

function GetHFileAges(Handle: Longint; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint; {JO}
  {JO: возвращает время и дату последней модификации (Age_LWr),                   }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }
function SetHFileAges(Handle: Longint; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;     {JO}
  {JO: устанавливает время и дату последней модификации (Age_LWr),                }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }

implementation

uses
  Windows;

function GetBytesPerCluster(Drive: Byte): Longint;
var
  RootPath: array[0..3] of Char;
  RootPtr: PChar;
  SectorsPerCluster,BytesPerSector,FreeClusters,TotalClusters: DWord;
begin
  RootPtr := nil;
  if Drive > 0 then
  begin
    RootPath[0] := Char(Drive + (Ord('A') - 1));
    RootPath[1] := ':';
    RootPath[2] := '\';
    RootPath[3] := #0;
    RootPtr := RootPath;
  end;
  if GetDiskFreeSpace(RootPtr, SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters) then
      GetBytesPerCluster := SectorsPerCluster * BytesPerSector
    else GetBytesPerCluster := 0;
end;

{Cat}
{$IFDEF EAOP}
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
procedure CopyEAs (FFromName, FToName: String);
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
  FFromName := FFromName + #0;
  FToName := FToName + #0;

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
end;

{*** copy security attributes ***}
procedure CopySAs (FFromName, FToName: String);
var
  Size: DWord;
  Ptr: Pointer;
begin
  FFromName := FFromName + #0;
  FToName := FToName + #0;

  if not GetFileSecurity(@FFromName[1], DACL_Security_Information, nil, 0, Size) then
    if GetLastError = error_Insufficient_Buffer then
      begin
        GetMem(Ptr, Size);
        if GetFileSecurity(@FFromName[1], DACL_Security_Information, Ptr, Size, Size) then
          SetFileSecurity(@FToName[1], DACL_Security_Information, Ptr);
        FreeMem(Ptr, Size);
      end;
end;
{$ENDIF}
{/Cat}

{JO}
type
  TDateTimeRec = record
    FTime,FDate: SmallWord;
  end;

function SetResult(Success: Boolean): Longint;
begin
  SetResult := 0;
  if not Success then
    SetResult := GetLastError;
end;

function GetHFileAges(Handle: Longint; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
var
  FileTime_LWr, LocalFileTime_LWr,
  FileTime_Cr,  LocalFileTime_Cr,
  FileTime_LAc, LocalFileTime_LAc : TFileTime;
begin
  GetHFileAges := SetResult(GetFileTime(Handle, @FileTime_Cr, @FileTime_LAc, @FileTime_LWr) and
    FileTimeToLocalFileTime(FileTime_LWr, LocalFileTime_LWr) and
    FileTimeToDosDateTime(LocalFileTime_LWr, TDateTimeRec(Age_LWr).FDate, TDateTimeRec(Age_LWr).FTime) and
    FileTimeToLocalFileTime(FileTime_Cr, LocalFileTime_Cr) and
    FileTimeToDosDateTime(LocalFileTime_Cr, TDateTimeRec(Age_Cr).FDate, TDateTimeRec(Age_Cr).FTime) and
    FileTimeToLocalFileTime(FileTime_LAc, LocalFileTime_LAc) and
    FileTimeToDosDateTime(LocalFileTime_LAc, TDateTimeRec(Age_LAc).FDate, TDateTimeRec(Age_LAc).FTime));
end;

function SetHFileAges(Handle: Longint; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
var
  LocalFileTime_LWr, FileTime_LWr,
  LocalFileTime_Cr,  FileTime_Cr,
  LocalFileTime_LAc, FileTime_LAc : TFileTime;
begin
  SetHFileAges := SetResult(
    DosDateTimeToFileTime(TDateTimeRec(Age_LWr).FDate, TDateTimeRec(Age_LWr).FTime, LocalFileTime_LWr) and
    LocalFileTimeToFileTime(LocalFileTime_LWr, FileTime_LWr) and
    DosDateTimeToFileTime(TDateTimeRec(Age_Cr).FDate, TDateTimeRec(Age_Cr).FTime, LocalFileTime_Cr) and
    LocalFileTimeToFileTime(LocalFileTime_Cr, FileTime_Cr) and
    DosDateTimeToFileTime(TDateTimeRec(Age_LAc).FDate, TDateTimeRec(Age_LAc).FTime, LocalFileTime_LAc) and
    LocalFileTimeToFileTime(LocalFileTime_LAc, FileTime_LAc) and
    SetFileTime(Handle, @FileTime_Cr, @FileTime_LAc, @FileTime_LWr));
end;
{/JO}

begin
end.
