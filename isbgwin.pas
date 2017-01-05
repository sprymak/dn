{$IFNDEF OS2} for os/2 only! {$ENDIF}
{AK155 15.08.2001
  �஢�ઠ, ���� �� DN 䮭��� ������ ᥠ�ᮬ.
(� �⮬ ��砥 �㦭� �����஢��� ॠ��� �� ���ﭨ� ������
Alt, Ctrl, Del. �� �������� � DnApp.)
  ��� �஢�ન � �� ��襫 ��祣� ����, 祬 㧭��� PID
⥪�饣� ����� � ��� த�⥫� � �ࠢ������ �� �
PID ��⨢���� �������� ᥠ��. �� PID ��⨢���� - ��
PID ⮣�, �� ᮧ��� ����. ��� �� �᫨, ���ਬ��,
�� cmd, ����� ᮧ��� ����, �맢��� FC, � �� FC �맢���
DN/2, � �㤥� ����� १���� true :(
}

unit IsBGWin;
interface

function IsBGWindow: boolean;

implementation
uses
   Os2Def, Os2Base, dnApp;

var
  ActiveWin:  ULong;  // System Information Data Buffer
  Tib  : PTib;   // Thread information block structure
  Pib  : PPib;   // Process information block structure
  AppType: longint;
  PID: longint;
  ParentPid: longint;

function IsBGWindow: boolean;
  begin
  IsBGWindow := false;
  if PMWindowed then
    begin
    DosQuerySysInfo(25, 25, ActiveWin, sizeof(ActiveWin));
    if (ActiveWin <> PID) and (ActiveWin <> ParentPID) then
      IsBGWindow := true;
    end;
  end;

begin
DosGetInfoBlocks(Tib, Pib);
with Pib^ do
  begin
  PMWindowed := (Pib_ulType = 2);
  PID := Pib_ulPid;
  ParentPid := Pib_ulPPid;
  end;
end.

