{$IFNDEF OS2} for os/2 only! {$ENDIF}
{AK155 15.08.2001
  Проверка, является ли DN фоновым оконным сеансом.
(В этом случае нужно блокировать реакцию на состояние клавиш
Alt, Ctrl, Del. Это делается в DnApp.)
  Для проверки я не нашел ничего лучше, чем узнать PID
текущего процесса и его родителя и сравнивать их с
PID активного оконного сеанса. Но PID активного - это
PID того, кто создал окно. Так что если, например,
из cmd, который создал окно, вызвать FC, а из FC вызвать
DN/2, то будет ложный результат true :(
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

