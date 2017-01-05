{ interface block for Events.pas }

function GetCurMSec: Longint;
{` Get system uptime in mSec `}

procedure LongWorkBegin;
{` Под Win9x приоритет снижается до нормального, чтобы не тормозить
всю систему (обычно мы под W9x работаем с повышенным приоритетом,
так как иначе тормозит у нас клавиатура.)
   Кроме этого, взводится флаг Working. `}

procedure LongWorkEnd;
{` Под Win9x восстанавливается повышенный приоритет; см. LongWorkBegin;.
Кроме этого, сбрасывается флаг Working.
`}

var
  Working: Boolean;
    {` Признак выполнения какой-то работы. Блокирует вызов TinySlice
     в MyApp.Idle `}
