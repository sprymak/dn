{ Dpmi32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - � WinNT �����ন������ ����஢���� Security Attributes
}

unit FlTl;
interface

function GetBytesPerCluster(Drive: Byte): Longint;

function GetHFileAges(Handle: Longint; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint; {JO}
  {JO: �����頥� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                   }
  {    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
  {    䠩�� �� ���� 䠩�� (Handle), �ਭ����� ���祭�� ���� �訡��             }
function SetHFileAges(Handle: Longint; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;     {JO}
  {JO: ��⠭�������� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                }
  {    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
  {    䠩�� �� ���� 䠩�� (Handle), �ਭ����� ���祭�� ���� �訡��             }

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
