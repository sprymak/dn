{ Dpmi32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - в WinNT поддерживается копирование Security Attributes
}

unit FlTl;
interface

function GetBytesPerCluster(Drive: Byte): Longint;

function GetHFileAges(Handle: Longint; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint; {JO}
  {JO: возвращает время и дату последней модификации (Age_LWr),                   }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }
function SetHFileAges(Handle: Longint; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;     {JO}
  {JO: устанавливает время и дату последней модификации (Age_LWr),                }
  {    время и дату создания (Age_Cr) и время и дату последнего доступа (Age_LAc) }
  {    файла по хэндлу файла (Handle), принимает значение кода ошибки             }

implementation

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

function GetHFileAges(Handle: Longint; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
begin
  GetHFileAges := 0;
end;

function SetHFileAges(Handle: Longint; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
begin
  SetHFileAges := 0;
end;
{/JO}

begin
end.
