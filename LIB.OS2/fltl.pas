{ OS/2-specific file tools unit by J.Osadtchiy (JO), A.Korop (AK155)}
{ Optimise-}

{$I STDEFINE.INC}
unit FlTl;

interface

const

  ageLastWrite = 1;
  ageCreation = 2;
  ageLastAccess = 3;
  ageAll = 4;

function GetFileAge(s: String; AgeType: byte): longInt;
{JO: �����頥� �६� � ���� 䠩�� ��� ��⠫��� �� ������� ��� (S); }
{    ⨯ �६��� � ���� �������� � AgeType � ����� �ਭ�����         }
{    ���祭�� ���� �⠭������ ��६�����  ageLastWrite,             }
{    ageCreation � ageLastAccess                                     }

function SetFileAge(s: String; Age: longInt; AgeType: byte): longInt;
{JO: ��⠭�������� �६� � ���� 䠩�� ��� ��⠫��� �� ������� ��� (S);}
{    ⨯ �६��� � ���� �������� � AgeType � ����� �ਭ�����           }
{    ���祭�� ���� �⠭������ ��६�����  ageLastWrite,               }
{    ageCreation � ageLastAccess                                       }

function GetFileAges(s: String; var Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
{JO: �����頥� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                   }
{    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
{    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

function SetFileAges(s: String; Age_LWr, Age_Cr, Age_LAc: longInt):
  longInt;
{JO: ��⠭�������� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                }
{    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
{    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

function GetVolSer(DriveNum: longInt; var Serial: longInt; VolLab:
    String): longInt;
function SetVolume(DriveNum: longInt; VolLab: String): longInt;
function GetBytesPerCluster(DriveNum: longInt): longInt;

procedure CopyEAs(FFromName, FToName: String);

function GetEAString(const FileName, Name: String; var Value: String;
    Silent: boolean): integer;
(*
function SetEAString(const Filename, Name, Value: string): Integer;
*)
procedure SetEALongname(FileName: String);
function GetDriveTypeString(Drive: Char): String; {AK155}

implementation

uses
  {SysUtils,}Os2Base, Strings {, Crt, Messages}
  {��� CopyEAs}, Collect, Messages, EAOper, advance1, Commands,
    DNApp;

(*
function GetFileAge(S: String): longint;
 begin
  GetFileAge := FileAge (S);
 end;
*)

function GetFileAge(s: String; AgeType: byte): longInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: longInt;
    rc: longInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
    ulBufSize := SizeOf(FileStatus3);
    PS := PSArr;
    PS := StrPCopy(PS, s);
    rc := DosQueryPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize);

    if rc > 0 then
      GetFileAge := 0
    else
      case AgeType of
        ageLastWrite:
          GetFileAge := (fsts3ConfigInfo.fdateLastWrite shl 16)+
            fsts3ConfigInfo.ftimeLastWrite;
        ageCreation:
          GetFileAge := (fsts3ConfigInfo.fdateCreation shl 16)+
            fsts3ConfigInfo.ftimeCreation;
        ageLastAccess:
          GetFileAge := (fsts3ConfigInfo.fdateLastAccess shl 16)+
            fsts3ConfigInfo.ftimeLastAccess;
        else
          GetFileAge := 0;
      end {case};
  end { GetFileAge };

function SetFileAge(s: String; Age: longInt; AgeType: byte): longInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: longInt;
    rc: longInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
    ulBufSize := SizeOf(FileStatus3);
    PS := PSArr;
    PS := StrPCopy(PS, s);
    rc := DosQueryPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize);

    case AgeType of
      ageLastWrite:
        begin
          fsts3ConfigInfo.fdateLastWrite := Age shr 16;
          fsts3ConfigInfo.ftimeLastWrite := Age and $FFFF;
        end;
      ageCreation:
        begin
          fsts3ConfigInfo.fdateCreation := Age shr 16;
          fsts3ConfigInfo.ftimeCreation := Age and $FFFF;
        end;
      ageLastAccess:
        begin
          fsts3ConfigInfo.fdateLastAccess := Age shr 16;
          fsts3ConfigInfo.ftimeLastAccess := Age and $FFFF;
        end;
      ageAll:
        begin
          fsts3ConfigInfo.fdateLastWrite := Age shr 16;
          fsts3ConfigInfo.ftimeLastWrite := Age and $FFFF;
          fsts3ConfigInfo.fdateCreation := Age shr 16;
          fsts3ConfigInfo.ftimeCreation := Age and $FFFF;
          fsts3ConfigInfo.fdateLastAccess := Age shr 16;
          fsts3ConfigInfo.ftimeLastAccess := Age and $FFFF;
        end;
      else
        begin
          SetFileAge := 1;
          exit;
        end;
    end {case};

    {ulBufSize := sizeof(FileStatus3);}
    rc := DosSetPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize,
    0);
    SetFileAge := rc;
  end { SetFileAge };

function GetFileAges(s: String; var Age_LWr, Age_Cr, Age_LAc:
    longInt): longInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: longInt;
    rc: longInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
    ulBufSize := SizeOf(FileStatus3);
    PS := PSArr;
    PS := StrPCopy(PS, s);
    rc := DosQueryPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize);
    if rc > 0 then
      begin
        Age_LWr := 0;
        Age_Cr := 0;
        Age_LAc := 0;
      end
    else
      begin
        Age_LWr := (fsts3ConfigInfo.fdateLastWrite shl 16)+
          fsts3ConfigInfo.ftimeLastWrite;
        Age_Cr := (fsts3ConfigInfo.fdateCreation shl 16)+
          fsts3ConfigInfo.ftimeCreation;
        Age_LAc := (fsts3ConfigInfo.fdateLastAccess shl 16)+
          fsts3ConfigInfo.ftimeLastAccess;
      end;
    GetFileAges := rc;
  end { GetFileAges };


function SetFileAges(s: String; Age_LWr, Age_Cr, Age_LAc: longInt):
    longInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: longInt;
    rc: longInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
    ulBufSize := SizeOf(FileStatus3);
    PS := PSArr;
    PS := StrPCopy(PS, s);
    rc := DosQueryPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize);

    fsts3ConfigInfo.fdateLastWrite := Age_LWr shr 16;
    fsts3ConfigInfo.ftimeLastWrite := Age_LWr and $FFFF;
    fsts3ConfigInfo.fdateCreation := Age_Cr shr 16;
    fsts3ConfigInfo.ftimeCreation := Age_Cr and $FFFF;
    fsts3ConfigInfo.fdateLastAccess := Age_LAc shr 16;
    fsts3ConfigInfo.ftimeLastAccess := Age_LAc and $FFFF;

    {ulBufSize := sizeof(FileStatus3);}
    rc := DosSetPathInfo(
    PS,
    fil_Standard,
    fsts3ConfigInfo,
    ulBufSize,
    0);
    SetFileAges := rc;
  end { SetFileAges };


function GetVolSer(DriveNum: longInt; var Serial: longInt; VolLab:
    String): longInt;
  type
    fsInfoBuf = record
      UlVolser: longInt {ULong}; // Volume serial number
      Vol: VolumeLabel; // Volume label
      end;
  var
    VolumeInfo: fsInfoBuf; // File system info buffer
    rc: longInt {ApiRet};

  begin
    rc := DosQueryFSInfo(DriveNum, fsil_VolSer, VolumeInfo, SizeOf(
      fsInfoBuf));
    if rc = 0 then
      begin
        Serial := VolumeInfo.UlVolser;
        VolLab := VolumeInfo.Vol;
      end
    else
      begin
        Serial := 0;
        VolLab := '';
      end;
    GetVolSer := rc;
  end { GetVolSer };

function SetVolume(DriveNum: longInt; VolLab: String): longInt;
  var
    fsInfoBuf: VolumeLabel; // File system info buffer
    rc: longInt;
  begin
    fsInfoBuf := VolLab;
    rc := DosSetFSInfo(DriveNum, fsil_VolSer, fsInfoBuf, SizeOf(
      VolumeLabel));
    SetVolume := rc;
  end;

function GetBytesPerCluster(DriveNum: longInt): longInt;
  var
    aulFSInfoBuf: FsAllocate;
    rc: longInt;
  begin
    rc := DosQueryFSInfo(DriveNum, fsil_Alloc, aulFSInfoBuf, SizeOf(
      FsAllocate));
    if rc = 0 then
      GetBytesPerCluster := aulFSInfoBuf.cSectorUnit*aulFSInfoBuf.
        cbSector
    else
      GetBytesPerCluster := 0;
  end;

procedure CopyEAs(FFromName, FToName: String);
  var
    coll: PStringCollection;
    ulrc, ulEASize: Cardinal;
    i: longInt;
    pszName: PChar;
    Params: array[0..1] of Pointer;
    ea: Pointer;

  begin
    coll := New(PStringCollection, Init(5, 10, False));
    ulrc := EnumEAs(FFromName, coll);
    if ulrc = 0 then
      begin
        if (coll^.Count > 0) then
            {JO: �. �������਩ �� 30-07-2002 � EAOper.EnumEAs }
          for i := coll^.Count-1 downto 0 do
            begin
              pszName := PChar(coll^.At(i));
              Inc(pszName);
                {JO ��祬� ��������� ����室������ ⠪ ��������� - ������⭮, �� ����}
              {�᫨ ���� ���� PString(coll^.At(i)) - ������}
              ulrc := RetrieveEA(FFromName, pszName, ea, ulEASize,
                False);
              if ulrc = 0 then
                begin
                  ulrc := StoreEA(FToName, pszName, ea, ulEASize);
                  if (ulrc <> 0)
                    and (ulrc <> 282)
                      { Destination file system does not support EAs }
                    and (ulrc <> 283)
                      { Destination file system does not support EAs }
                    { and the source file's EAs contain a need EA  }
                  then
                    begin
                      Params[0] := coll^.At(i);
                      Params[1] := Pointer(ulrc);
                      MessageBox(#3+GetString(dl_Failed_to_store_EA)+
                        ' "%s"'+^M^C'(RC=%d)', @params,
                      mfError or mfOKButton);
                    end;
                  if ulrc = 283 then
                    MessageBox(GetString(dl_Critical_EA_Copy_Fail)+
                      FFromName, nil, mfOKButton);
                  FreeMem(ea);
                end
              else
                begin
                  Params[0] := coll^.At(i);
                  Params[1] := Pointer(ulrc);
                  MessageBox(#3+GetString(dl_Failed_to_retrieve_EA)+
                    ' "%s"'+^M^C'(RC=%d)', @params,
                  mfError or mfOKButton);
                end;
            end;
      end
    else if ulrc <> 124 then
        {JO: �訡�� 124 - �� �।�ᬮ�७�� ��� �������      }
      {    ���ன�⢠ �஢��� ����祭��/������� ���ଠ樨  }
      MessageBox(#3+GetString(dl_Failed_to_enumerate_EA)+
        ^M^C'(RC=%d)',
      @ulrc, mfError or mfOKButton);
    Dispose(coll, Done);
  end { CopyEAs };

function GetEAString(const FileName, Name: String; var Value: String;
    Silent: boolean): integer;
  var
    ea: Pointer;
    ulEASize, ulSize: Cardinal;
    szName: array[0..255] of Char;
    pszValue: PChar;
  begin
    Value := '';
    Result := RetrieveEA(FileName, StrPCopy(szName, Name), ea,
      ulEASize, Silent);
    if (Result = 0) and (ea <> nil) then
      begin
        ulSize := RetrieveStringSize(ea);
        GetMem(pszValue, succ(ulSize));
        Value := StrPas(RetrieveString(ea, pszValue));
        FreeMem(pszValue);
        FreeMem(ea);
      end;
  end;

function SetEAString(const FileName, Name, Value: String): integer;
  var
    ea: Pointer;
    szValue, szName: array[0..255] of Char;
    ulEASize: Cardinal;
  begin
    ea := BuildEAFromString(StrPCopy(szValue, Value), ulEASize);
    SetEAString := StoreEA(FileName, StrPCopy(szName, Name), ea,
      ulEASize);
    FreeMem(ea);
  end;

procedure SetEALongname(FileName: String);
  var
    LNValue: String;
    Result_: integer;
  begin
    Result_ := GetEAString(FileName, '.LONGNAME', LNValue, True);
    if Result_ = 0 then
      begin
        if BigInputBox(GetString(dlEditEALongname), GetString(
            dl_EALongname), LNValue, 255, hsEditEALongname) <> cmOK
        then
          exit;
        Result_ := SetEAString(FileName, '.LONGNAME', LNValue);
        if Result_ <> 0 then
          MessageBox(
            #3'Failed to write .LONGNAME extended attribute'+
            ^M^C'(RC=%d)',
          @Result_, mfError or mfOKButton);
      end
    else if Result_ <> 48 then
        {JO: �訡�� 48, ����� � �� ��१�ࢨ஢���      }
      {    �� �ᯮ��㥬, ����� DosQueryPathInfo �뤠��  }
      {    fst4.cbList (����稭� ᯨ᪠ EA) ࠢ�� ���, }
      {    �� ��ண� ������ �� �訡��, �� ����      }
      {    ������� ��ࢠ�� ���쭥���� ࠡ��� � EA , �.�.}
      {    �������� �� ��᪠�, �� �����ন����� EA   }
      MessageBox(#3'Failed to read .LONGNAME extended attribute'+
        ^M^C'(RC=%d)', @Result_, mfError or mfOKButton)
    else
      MessageBox(GetString(dlOperationNotValidForDdrive), nil,
        mfOKButton);
  end { SetEALongname };

{AK155}
function GetDriveTypeString(Drive: Char): String;
  var
    BufLen: word;
    FSQb: pFSQBuffer2;
    DrvName: String[3];
    Ordinal: SmallWord;
    Name: PChar;
    rc: word;
    {DiskSize  : Word;}
  begin
    GetDriveTypeString := '';
    BufLen := 100;
    GetMem(FSQb, BufLen);
    DrvName := Drive+':'#0;
    Ordinal := 0;
    rc := DosQueryFSAttach(@DrvName[1], Ordinal, fsail_QueryName,
      FSQb, BufLen);
    if rc = 0 then
      with FSQb^ do
        begin
          Name := szName+cbName+1;
          GetDriveTypeString := StrPas(Name);
        end;

    FreeMem(FSQb, 100);
  end { GetDriveTypeString };
{/AK155}

end.
