{�������������������������������������������¿}
{��� ���� ��⮤�� ����������� �������     �Ŵ}
{���  ���p: ��⮭ ����p�� aka DataCompBoy �Ŵ}
{���  ��� � ���� ���p모��� �����...   �Ŵ}
{���������������������������������������������}
UNIT Search;
INTERFACE
 TYPE
  TCompareProc = function(a:longint) : longint;
  {�p������� �����.
   -1 �᫨ a<����室�����
    0 �᫨ a=����室����y
   +1 �᫨ a>����室�����
  }

 Function BinSearch(CompareProc: TCompareProc; LastEl: longint): longint;
  {���� ��⮤�� ����筮�� �祭��. ��� � ��������� [0..LastEl-1]}

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