{ Dpmi32-specific file tools unit by J.Osadtchiy (JO), A.Kozlov (Cat)}
{ Optimise-}

{$I STDEFINE.INC}

{Cat
   04/12/2001 - � WinNT �����ন������ ����஢���� Security Attributes
}

unit FlTl;
interface

function GetBytesPerCluster(Drive: Byte): Longint;

function GetFileAges(S: ShortString; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
  {JO: �����頥� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                   }
  {    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
  {    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

function SetFileAges(S: String; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
  {JO: ��⠭�������� �६� � ���� ��᫥���� ����䨪�樨 (Age_LWr),                }
  {    �६� � ���� ᮧ����� (Age_Cr) � �६� � ���� ��᫥����� ����㯠 (Age_LAc) }
  {    䠩�� ��� ��⠫��� �� ������� ��� (S), �ਭ����� ���祭�� ���� �訡��     }

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

function GetFileAges(S: ShortString; Var Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
begin
  GetFileAges := 0;
end;

function SetFileAges(S: ShortString; Age_LWr, Age_Cr, Age_LAc: Longint): Longint;
begin
  SetFileAges := 0;
end;
{/JO}

end.
