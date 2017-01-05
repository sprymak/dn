unit DebugMem;
(******

This unit saves to C:\DEBUGMEM.LOG all memory chunks,
which was GetMem'ed, but wasn't FreeMem'ed
Written by Cat 2:5030/1326.13
(for use in DN/2; if you want to use it
in other application, you are to mail me)

******)


{&Delphi+}
{$I STDEFINE.INC}

interface

implementation

{$IFDEF DEBUGMEM}

uses
  VpUtils;

const
  MaxAllocs = 128*1024;
  Allocs: LongInt = 0;

var
  AllocedAddr: array[1..MaxAllocs] of Pointer;
  AllocedSize: array[1..MaxAllocs] of LongInt;
  AllocedCall: array[1..MaxAllocs] of Pointer;
  OldMemoryManager: TMemoryManager;

function NewGetMem(Size: Longint): Pointer;
begin
  if Allocs >= MaxAllocs then
    RunError(203);
  Result := OldMemoryManager.GetMem(Size);
  Inc(Allocs);
  AllocedAddr[Allocs] := Result;
  AllocedSize[Allocs] := Size;
  AllocedCall[Allocs] := nil;
end;

function NewFreeMem(P: Pointer): Longint;
var
  I: LongInt;
begin
  if P = nil then
    Exit;
  for I := 1 to Allocs do
    if AllocedAddr[I] = P then
      begin
        Result := OldMemoryManager.FreeMem(P);
        for I := I to Allocs-1 do
          begin
            AllocedAddr[I] := AllocedAddr[I+1];
            AllocedSize[I] := AllocedSize[I+1];
            AllocedCall[I] := AllocedCall[I+1];
          end;
        Dec(Allocs);
        Exit;
      end;
 {RunError(203);}
end;

function NewReAllocMem(P: Pointer; Size: Longint): Pointer;
var
  I: LongInt;
begin
  for I := 1 to Allocs do
    if AllocedAddr[I] = P then
      begin
        Result := OldMemoryManager.ReAllocMem(P, Size);
        AllocedAddr[I] := Result;
        AllocedSize[I] := Size;
        Exit;
      end;
 {RunError(203);}
end;

procedure NewGetMemCallerNotify(Caller: Pointer);
begin
  AllocedCall[Allocs] := Caller;
end;

const
  NewMemoryManager: TMemoryManager =
    (GetMem: NewGetMem;
     FreeMem: NewFreeMem;
     ReAllocMem: NewReAllocMem;
     GetMemCallerNotify: NewGetMemCallerNotify);

procedure Done;
var
  F: Text;
  I, J: LongInt;
  B: Boolean;
  Ch: Char;
  FileName: ShortString;
  LineNo: LongInt;
begin
  SetMemoryManager(OldMemoryManager);
  if Allocs = 0 then
    Exit;
  Writeln(^M^J'Memory leak ', MemUsed, ' bytes, ', Allocs, ' memory allocs is not freed');
  if IOResult <> 0 then
    ;
  Assign(F, 'c:\debugmem.log');
  Rewrite(F);
  Writeln(F, 'Memory leak ', MemUsed, ' bytes, ', Allocs, ' memory allocs is not freed');
  for I := 1 to Allocs do
    Writeln(F, Ptr2Hex(AllocedAddr[I]):12, AllocedSize[I]:12);
  for I := 1 to Allocs do
    begin
      Writeln(F, ^M^J'--------------------------------------------------'^M^J,
                 Ptr2Hex(AllocedAddr[I]):12, AllocedSize[I]:12);
      if GetLocationInfo(AllocedCall[I], FileName, LineNo)<>nil then
        Writeln(F, '    allocated at ', FileName, ' line ',LineNo)
      else
        Writeln(F, '    allocated at ', Ptr2Hex(AllocedCall[I]));
      B := False;
      for J := 0 to AllocedSize[I]-1 do
        begin
          Ch := PChar(AllocedAddr[I])[J];
          if Ch >= ' ' then
            if B then
              Write(F, Ch)
            else
              begin
                Write(F, ''''+Ch);
                B := True;
              end
          else
            if B then
              begin
                Write(F, '''#'+Int2Str(Byte(Ch)));
                B := False;
              end
            else
              Write(F, '#'+Int2Str(Byte(Ch)));
        end;
      if B then
        Write(F, '''');
    end;
  Close(F);
  if IOResult <> 0 then
    Writeln('Error writing file C:\DEBUGMEM.LOG');
end;

procedure Init;
begin
  AddExitProc(Done);
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(NewMemoryManager);
end;

begin
  Writeln('Memory debugging');
  Init;
{$ENDIF DEBUGMEM}
end.
