{ interface block for Events.pas }

function GetCurMSec: Longint;
{` Get system uptime in mSec `}

procedure LongWorkBegin;
{` ��� Win9x �ਮ��� ᭨������ �� ��ଠ�쭮��, �⮡� �� �ମ����
��� ��⥬� (���筮 �� ��� W9x ࠡ�⠥� � ����襭�� �ਮ��⮬,
⠪ ��� ���� �ମ��� � ��� ���������.)
   �஬� �⮣�, ��������� 䫠� Working. `}

procedure LongWorkEnd;
{` ��� Win9x ����⠭���������� ����襭�� �ਮ���; �. LongWorkBegin;.
�஬� �⮣�, ���뢠���� 䫠� Working.
`}

var
  Working: Boolean;
    {` �ਧ��� �믮������ �����-� ࠡ���. �������� �맮� TinySlice
     � MyApp.Idle `}