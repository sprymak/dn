unit Advance0;

interface

function MinBufSize(x: TFileSize; y: LongInt): LongInt;
  {` ����襥 �᫮ �� ࠧ��� 䠩�� � ࠧ��� ���� `}

function Positive(x: TFileSize): TFileSize;
  {` ������ 䠩��, ��࠭�祭�� ᭨�� ��� `}

function i32(x: TFileSize): LongInt;
  {` TFileSize -> LongInt `}
{$ifndef LargeFileSupport}
  inline;
  begin
  Result := x;
  end;
{$endif}

implementation

function MinBufSize(x: TFileSize; y: LongInt): LongInt;
  begin
  Result := y;
  if Result > x then
    Result := i32(x);
  end;

function Positive(x: TFileSize): TFileSize;
  begin
  Result := 0;
  if x > 0 then
    Result := x;
  end;

{$ifdef LargeFileSupport}
function i32(x: TFileSize): LongInt;
  begin
  Result := Round(x);
  end;
{$endif}

end.
