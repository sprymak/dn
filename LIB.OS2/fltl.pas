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

function GetFileAge(S: String; AgeType: Byte): LongInt;
{JO: возвращает время и дату файла или каталога по полному пути (S); }
{    тип времени и даты задаётся в AgeType и может принимать         }
{    значение трёх стандартных переменных  ageLastWrite,             }
{    ageCreation и ageLastAccess                                     }

function SetFileAge(S: String; Age: LongInt; AgeType: Byte): LongInt;
{JO: устанавливает время и дату файла или каталога по полному пути (S);}
{    тип времени и даты задаётся в AgeType и может принимать           }
{    значение трёх стандартных переменных  ageLastWrite,               }
{    ageCreation и ageLastAccess                                       }

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: возвращает время и дату последней модификации (Age_LWr),                   }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
{JO: устанавливает время и дату последней модификации (Age_LWr),                }
{    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
{    файла или каталога по полному пути (S), принимает значение кода ошибки     }

function GetVolSer(DriveNum: LongInt; var Serial: LongInt; VolLab: String)
  : LongInt;
function SetVolume(DriveNum: LongInt; VolLab: String): LongInt;
function GetBytesPerCluster(DriveNum: LongInt): LongInt;

procedure CopyEAs(FFromName, FToName: String);

function GetEAString(const Filename, Name: String; var Value: String;
     Silent: Boolean): Integer;
(*
function SetEAString(const Filename, Name, Value: string): Integer;
*)
procedure SetEALongname(Filename: String);
function GetDriveTypeString(Drive: Char): String; {AK155}

implementation

uses
  {SysUtils,}Os2Base, Strings {, Crt, Messages}
  {для CopyEAs}, Collect, Messages, EAOper, Advance1, Commands, DNApp
  ;

(*
function GetFileAge(S: String): longint;
 begin
  GetFileAge := FileAge (S);
 end;
*)

function GetFileAge(S: String; AgeType: Byte): LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
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
        GetFileAge := (fsts3ConfigInfo.fdateLastWrite shl 16)
          +fsts3ConfigInfo.ftimeLastWrite;
      ageCreation:
        GetFileAge := (fsts3ConfigInfo.fdateCreation shl 16)
          +fsts3ConfigInfo.ftimeCreation;
      ageLastAccess:
        GetFileAge := (fsts3ConfigInfo.fdateLastAccess shl 16)
          +fsts3ConfigInfo.ftimeLastAccess;
      else {case}
        GetFileAge := 0;
    end {case};
  end { GetFileAge };

function SetFileAge(S: String; Age: LongInt; AgeType: Byte): LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
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
    else {case}
      begin
      SetFileAge := 1;
      Exit;
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

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
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
    Age_LWr := (fsts3ConfigInfo.fdateLastWrite shl 16)
      +fsts3ConfigInfo.ftimeLastWrite;
    Age_Cr := (fsts3ConfigInfo.fdateCreation shl 16)
      +fsts3ConfigInfo.ftimeCreation;
    Age_LAc := (fsts3ConfigInfo.fdateLastAccess shl 16)
      +fsts3ConfigInfo.ftimeLastAccess;
    end;
  GetFileAges := rc;
  end { GetFileAges };


function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  var
    fsts3ConfigInfo: FileStatus3;
    ulBufSize: LongInt;
    rc: LongInt;
    PS: PChar;
    PSArr: array[0..255] of Char;
  begin
  ulBufSize := SizeOf(FileStatus3);
  PS := PSArr;
  PS := StrPCopy(PS, S);
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


function GetVolSer(DriveNum: LongInt; var Serial: LongInt; VolLab: String)
  : LongInt;
  type
    fsInfoBuf = record
      ulVolser: LongInt {ULong}; // Volume serial number
      Vol: VolumeLabel; // Volume label
      end;
  var
    VolumeInfo: fsInfoBuf; // File system info buffer
    rc: LongInt {ApiRet};

  begin
  rc := DosQueryFSInfo(DriveNum, fsil_VolSer, VolumeInfo,
         SizeOf(fsInfoBuf));
  if rc = 0 then
    begin
    Serial := VolumeInfo.ulVolser;
    VolLab := VolumeInfo.Vol;
    end
  else
    begin
    Serial := 0;
    VolLab := '';
    end;
  GetVolSer := rc;
  end { GetVolSer };

function SetVolume(DriveNum: LongInt; VolLab: String): LongInt;
  var
    FSInfoBuf: VolumeLabel; // File system info buffer
    rc: LongInt;
  begin
  FSInfoBuf := VolLab;
  rc := DosSetFSInfo(DriveNum, fsil_VolSer, FSInfoBuf,
       SizeOf(VolumeLabel));
  SetVolume := rc;
  end;

function GetBytesPerCluster(DriveNum: LongInt): LongInt;
  var
    aulFSInfoBuf: FsAllocate;
    rc: LongInt;
  begin
  rc := DosQueryFSInfo(DriveNum, fsil_Alloc, aulFSInfoBuf,
         SizeOf(FsAllocate));
  if rc = 0 then
    GetBytesPerCluster := aulFSInfoBuf.cSectorUnit*aulFSInfoBuf.cbSector
  else
    GetBytesPerCluster := 0;
  end;

procedure CopyEAs(FFromName, FToName: String);
  var
    coll: PStringCollection;
    ulrc, ulEASize: Cardinal;
    i: LongInt;
    PszName: PChar;
    params: array[0..1] of Pointer;
    ea: Pointer;

  begin
  coll := New(PStringCollection, Init(5, 10, False));
  ulrc := EnumEAs(FFromName, coll);
  if ulrc = 0 then
    begin
    if  (coll^.Count > 0) then
      {JO: см. комментарий от 30-07-2002 к EAOper.EnumEAs }
      for i := coll^.Count-1 downto 0 do
        begin
        PszName := PChar(coll^.At(i));
        Inc(PszName);
        {JO почему возникает необходимость так извращаться - непонятно, но надо}
        {если брать просто PString(coll^.At(i)) - падает}
        ulrc := RetrieveEA(FFromName, PszName, ea, ulEASize, False);
        if ulrc = 0 then
          begin
          ulrc := StoreEA(FToName, PszName, ea, ulEASize);
          if  (ulrc <> 0)
            and (ulrc <> 282)
            { Destination file system does not support EAs }
            and (ulrc <> 283)
            { Destination file system does not support EAs }
            { and the source file's EAs contain a need EA  }
            then
            begin
            params[0] := coll^.At(i);
            params[1] := Pointer(ulrc);
            MessageBox
              (#3+GetString(dl_Failed_to_store_EA)+' "%s"'+^M^C'(RC=%d)'
              , @params,
              mfError or mfOKButton);
            end;
          if ulrc = 283 then
            MessageBox(GetString(dl_Critical_EA_Copy_Fail)+FFromName,
               nil, mfOKButton);
          FreeMem(ea);
          end
        else
          begin
          params[0] := coll^.At(i);
          params[1] := Pointer(ulrc);
          MessageBox
            (#3+GetString(dl_Failed_to_retrieve_EA)+' "%s"'+^M^C'(RC=%d)'
            , @params,
            mfError or mfOKButton);
          end;
        end;
    end
  else if ulrc <> 124 then
    {JO: ошибка 124 - не предусмотренный для данного      }
    {    устройства уровень получения/задания информации  }
    MessageBox(#3+GetString(dl_Failed_to_enumerate_EA)+^M^C'(RC=%d)',
      @ulrc, mfError or mfOKButton);
  Dispose(coll, Done);
  end { CopyEAs };

function GetEAString(const Filename, Name: String; var Value: String;
     Silent: Boolean): Integer;
  var
    ea: Pointer;
    ulEASize, ulSize: Cardinal;
    szName: array[0..255] of Char;
    pszValue: PChar;
  begin
  Value := '';
  Result := RetrieveEA(Filename, StrPCopy(szName, Name), ea, ulEASize,
       Silent);
  if  (Result = 0) and (ea <> nil) then
    begin
    ulSize := RetrieveStringSize(ea);
    GetMem(pszValue, Succ(ulSize));
    Value := StrPas(RetrieveString(ea, pszValue));
    FreeMem(pszValue);
    FreeMem(ea);
    end;
  end;

function SetEAString(const Filename, Name, Value: String): Integer;
  var
    ea: Pointer;
    szValue, szName: array[0..255] of Char;
    ulEASize: Cardinal;
  begin
  ea := BuildEAFromString(StrPCopy(szValue, Value), ulEASize);
  SetEAString := StoreEA(Filename, StrPCopy(szName, Name), ea, ulEASize);
  FreeMem(ea);
  end;

procedure SetEALongname(Filename: String);
  var
    LNValue: String;
    Result_: Integer;
  begin
  Result_ := GetEAString(Filename, '.LONGNAME', LNValue, True);
  if Result_ = 0 then
    begin
    if BigInputBox(GetString(dlEditEALongname),
         GetString(dl_EALongname), LNValue, 255, hsEditEALongname)
       <> cmOK
    then
      Exit;
    Result_ := SetEAString(Filename, '.LONGNAME', LNValue);
    if Result_ <> 0 then
      MessageBox
        (#3'Failed to write .LONGNAME extended attribute'+^M^C'(RC=%d)',
        @Result_, mfError or mfOKButton);
    end
  else if Result_ <> 48 then
    {JO: ошибку 48, которая в оси зарезервирована      }
    {    мы используем, когда DosQueryPathInfo выдаёт  }
    {    fst4.cbList (величина списка EA) равным нулю, }
    {    что строго говоря не ошибка, но является      }
    {    поводом прервать дальнейшую работу с EA , т.к.}
    {    наблюдается на дисках, не поддерживающих EA   }
    MessageBox
      (#3'Failed to read .LONGNAME extended attribute'+^M^C'(RC=%d)',
       @Result_, mfError or mfOKButton)
  else
    MessageBox(GetString(dlOperationNotValidForDdrive), nil, mfOKButton);
  end { SetEALongname };

{AK155}
function GetDriveTypeString(Drive: Char): String;
  var
    BufLen: Word;
    FSQb: pFSQBuffer2;
    DrvName: String[3];
    Ordinal: SmallWord;
    name: PChar;
    rc: Word;
    {DiskSize  : Word;}
  begin
  GetDriveTypeString := '';
  BufLen := 100;
  GetMem(FSQb, BufLen);
  DrvName := Drive+':'#0;
  Ordinal := 0;
  rc := DosQueryFSAttach(@DrvName[1], Ordinal, fsail_QueryName, FSQb,
       BufLen);
  if rc = 0 then
    with FSQb^ do
      begin
      name := szName+cbName+1;
      GetDriveTypeString := StrPas(name);
      end;

  FreeMem(FSQb, 100);
  end { GetDriveTypeString };
{/AK155}

end.
