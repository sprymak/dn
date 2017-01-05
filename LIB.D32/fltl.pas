{ Dpmi32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - в WinNT поддерживается копирование Security Attributes
}

unit FlTl;
interface

type
  TDrvTypeNew = ( dtnFloppy, dtnHDD, dtnInvalid,
                 dtnCDRom, dtnLAN, dtnUnknown, dtnOptical, dtnProgram);

function GetBytesPerCluster(Drive: Byte): Longint;

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  {JO: возвращает время и дату последней модификации (Age_LWr),                   }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }
function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
  {JO: устанавливает время и дату последней модификации (Age_LWr),                }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }

function GetFSString(Drive: Char): String; {AK155}
function GetShare(Drive: Char): String; {AK155}
function GetDriveTypeNew(Drive: Char): TDrvTypeNew; {JO} {<fltl.001>}

implementation

uses
  dpmi32df, dpmi32, DosLow, VpSysLow, VPUtils, Strings;

function GetBytesPerCluster(Drive: Byte): Longint;
begin
result := 0;
end;

{JO}
type
  TDateTimeRec = record
    FTime,FDate: SmallWord;
  end;

function SetResult(Success: Boolean): Longint;
begin
  SetResult := 0;
  if not Success then
    SetResult := 1;
end;

function GetFileAges(S: String; var Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
begin
  Result := 0;
end;

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: LongInt)
  : LongInt;
begin
  Result := 0;
end;
{/JO}

function GetFSString(Drive: Char): String; {AK155}
  type
    TDiskInfo=record
      InfoLevel: SmallWord;
      SerialNo: LongInt;
      VolumeLabel: array[0..10] of Char;
      FileSystem: array[0..7] of Char;
     end;
  var
    Regs: real_mode_call_structure_typ;
    i: Integer;
  begin
  init_register(Regs);
  with Regs do
    begin
    AX_ := $6900;
    BX_ := Byte(Drive) - (Byte('A') - 1);
    DS_ := DosSeg; DX_ := 0;  {DS:DX -> buffer for returned info}
    end;
  intr_realmode(Regs, $21);
  move(TDiskInfo(DosSegFlat^).FileSystem, Result[1], 8);
  if Regs.flags_ and 1 <> 0 then
    begin
    Result := '';
    exit;
    end;
  SetLength(Result, 8);
  {В ДОС-сессиях OS/2 и WinNT поле FS дополняется #0, а в голом ДОС
  (PC DOS 7, MS DOS 7.*) - пробелами. }
  for i := 1 to 8 do
    if Result[i] in [#0, ' '] then
      begin
      SetLength(Result, i);
      Break;
      end;
  end;

function GetShare(Drive: Char): String; {AK155}
  begin
  Result := '';
  end;

function GetDriveTypeNew(Drive: Char): TDrvTypeNew;
  var
    T: TDriveType;
  const
    DriveTypeToDriveTypeNew: array[TDriveType] of TDrvTypeNew =
{
   TDriveType ( dtFloppy, dtHDFAT, dtHDHPFS, dtInvalid,
                 dtNovellNet, dtCDRom, dtLAN, dtHDNTFS, dtUnknown,
                 dtTVFS, dtHDExt2, dtJFS );
}
              (dtnFloppy, dtnHDD,   dtnHDD,  dtnInvalid,
                 dtnLAN,    dtnCDRom,  dtnLAN, dtnHDD,  dtnUnknown,
                 dtnProgram, dtnHDD, dtnHDD, dtnHDD, dtnOptical);
  begin
  T := GetDriveType(Drive);
  Result := DriveTypeToDriveTypeNew[T];
  end;

begin
end.
