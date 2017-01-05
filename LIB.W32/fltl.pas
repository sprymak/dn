{ Win32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - � WinNT �����ন������ ����஢���� Security Attributes
}

unit FlTl;
interface

function GetBytesPerCluster(Drive: byte): longInt;
procedure CopyEAs(FFromName, FToName: String);
{$IFDEF WIN32}
procedure CopySAs(FFromName, FToName: String);
{$ENDIF}

function GetFileAges(s: ShortString; var Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
{JO: �����頥� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                   }
{    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
{    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

function SetFileAges(s: ShortString; Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
{JO: ��⠭�������� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                }
{    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
{    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

function GetFSString(dr: Char): String; {JO}

implementation

uses
  Windows, Strings, Lfn;

function GetBytesPerCluster(Drive: byte): longInt;
  var
    RootPath: array[0..3] of Char;
    RootPtr: PChar;
    SectorsPerCluster, BytesPerSector, FreeClusters, TotalClusters:
      DWord;
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
  end { GetBytesPerCluster };

{Cat}
type
  PFullEAInformation = ^TFullEAInformation;
  TFullEAInformation = packed record
    NextEntryOffset: DWord;
    Flags: byte;
    EaNameLength: byte;
    EaValueLength: word;
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
    SL, Sh: DWord;
  begin
    FFromName := FFromName+#0;
    FToName := FToName+#0;

    (*** ���� �� ࠡ�⠥�
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
procedure CopySAs(FFromName, FToName: String);
  var
    Size: DWord;
    Ptr: Pointer;
  begin
    FFromName := FFromName+#0;
    FToName := FToName+#0;

    if not GetFileSecurity(@FFromName[1], DACL_Security_Information,
        nil, 0, Size)
    then
      if GetLastError = error_Insufficient_Buffer then
        begin
          GetMem(Ptr, Size);
          if GetFileSecurity(@FFromName[1],
              DACL_Security_Information, Ptr, Size, Size)
          then
            SetFileSecurity(@FToName[1], DACL_Security_Information,
              Ptr);
          FreeMem(Ptr, Size);
        end;
  end { CopySAs };
{/Cat}

{JO}
type
  TDateTimeRec = record
    FTime, FDate: SmallWord;
    end;

function SetResult(Success: boolean): longInt;
  begin
    SetResult := 0;
    if not Success then
      SetResult := GetLastError;
  end;

function GetFileAges(s: ShortString; var Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
  var
    LocalFileTime_LWr,
    LocalFileTime_Cr,
    LocalFileTime_LAc: TFileTime;
    FindData: TWin32FindData;
    Sh: THandle;
  begin
    s[Length(s)+1] := #0;
    Sh := FindFirstFile(@S[1], FindData);
    if Sh = invalid_Handle_Value then
      begin
        Result := GetLastError;
        exit;
      end;
    FindClose(Sh);
    with FindData do
      Result := SetResult(
      FileTimeToLocalFileTime(ftLastWriteTime, LocalFileTime_LWr)
        and
      FileTimeToDosDateTime(LocalFileTime_LWr, TDateTimeRec(Age_LWr).
        FDate, TDateTimeRec(Age_LWr).FTime) and
      FileTimeToLocalFileTime(ftCreationTime, LocalFileTime_Cr) and
      FileTimeToDosDateTime(LocalFileTime_Cr, TDateTimeRec(Age_Cr).
        FDate, TDateTimeRec(Age_Cr).FTime) and
      FileTimeToLocalFileTime(ftLastAccessTime, LocalFileTime_LAc)
        and
      FileTimeToDosDateTime(LocalFileTime_LAc, TDateTimeRec(Age_LAc).
        FDate, TDateTimeRec(Age_LAc).FTime));
  end { GetFileAges };

function SetFileAges(s: ShortString; Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
  var
    Handle: longInt;
    LocalFileTime_LWr, FileTime_LWr,
    LocalFileTime_Cr, FileTime_Cr,
    LocalFileTime_LAc, FileTime_LAc: TFileTime;
  begin
    s[Length(s)+1] := #0;
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
      DosDateTimeToFileTime(TDateTimeRec(Age_Cr).FDate, TDateTimeRec(
        Age_Cr).FTime, LocalFileTime_Cr) and
      LocalFileTimeToFileTime(LocalFileTime_Cr, FileTime_Cr) and
      DosDateTimeToFileTime(TDateTimeRec(Age_LAc).FDate,
        TDateTimeRec(Age_LAc).FTime, LocalFileTime_LAc) and
      LocalFileTimeToFileTime(LocalFileTime_LAc, FileTime_LAc) and
      SetFileTime(Handle, @FileTime_Cr, @FileTime_LAc, @FileTime_LWr));
    CloseHandle(Handle);
  end { SetFileAges };

function GetFSString(dr: Char): String;
  const
    Root: array[0..4] of Char = 'C:\'#0;
  var
    FSName: array[0..255] of Char;
    MaxLength: longInt;
    FSFlags: longInt;
  begin
    GetFSString := '';
    Root[0] := dr;
    if GetVolumeInformation(Root, nil, 0, nil, MaxLength, FSFlags,
        FSName, SizeOf(FSName))
    then
      GetFSString := StrPas(FSName);
  end;

{/JO}

begin
end.
