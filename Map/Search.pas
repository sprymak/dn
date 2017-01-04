{┌┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┬┐}
{├┼┼ Поиск методом половинного деления     ┼┼┤}
{├┼┼  Автоp: Антон Федоpов aka DataCompBoy ┼┼┤}
{├┼┼  Ваял в честь Матаpыкиной Ульяны...   ┼┼┤}
{└┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘}
UNIT Search;
INTERFACE
 TYPE
  TCompareProc = function(a:longint) : longint;
  {Сpавнение элемента.
   -1 если a<необходимого
    0 если a=необходимомy
   +1 если a>необходимого
  }

 Function BinSearch(CompareProc: TCompareProc; LastEl: longint): longint;
  {Поиск методом двоичного сечения. Ищет в диапазоне [0..LastEl-1]}

IMPLEMENTATION

 Function BinSearch(CompareProc: TCompareProc; LastEl: longint): longint;
  var Max, Min: LongInt;
      Cur: LongInt;
      LP:  LongInt;
      d:  LongInt;
  begin
   Max := LastEl;
   Min := 0;
   Cur := Min;
   d   := -1;
   repeat
    LP:=Cur;
    Cur:=Min + ((Max-Min) div 2);
    d:=CompareProc(Cur);
    if D>0 then Max:=Cur
           else if D<0 then Min:=Cur;
   until (D=0) or (Cur=LP);
   BinSearch:=Cur;
  end;

END.