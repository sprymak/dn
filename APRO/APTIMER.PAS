{$S-,R-,V-,I-,B-,F+,O+,A-}

{Conditional defines that may affect this unit}
{$I APDEFINE.INC}

{*********************************************************}
{*                    APTIMER.PAS 2.03                   *}
{*     Copyright (c) TurboPower Software 1991.           *}
{*                 All rights reserved.                  *}
{*********************************************************}

unit ApTimer;
  {-BIOS timing functions}

interface

uses
  Dpmi;

const
  TicsPerDay = 1573040;      {Assumes 18.20648 tics/sec}
  SecsPerDay = 86400;     {Number of seconds in one day}

  {Clock frequency of 1193180/65536 is reduced to 1675/92. This}
  {allows longint conversions of tics values upto TicsPerDay}
  TicsFreq = 1675;
  SecsFreq = 92;

var
  CountsPerMs : Word;         {Tight loop count for one millisecond}

{$I APTIMER.PA0}

implementation

var
  {!!.01 moved semicolons for Stony Brook}
  {Data area for BIOS tics value}
  BiosTics : ^LongInt {$IFDEF STONYBROOK} VOLATILE {$ENDIF}; {absolute $40:$6C;}
  BiosTicsLow : ^Word {$IFDEF STONYBROOK} VOLATILE {$ENDIF}; {absolute $40:$6C;}

function Tics2Secs(Tics : LongInt) : LongInt;
  {-Returns seconds value for Tics tics}
begin
  Tics2Secs := ((Tics + 9) * SecsFreq) div TicsFreq;
end;

function Secs2Tics(Secs : LongInt) : LongInt;
  {-Returns tics value for Secs seconds}
begin
  Secs2Tics := (Secs * TicsFreq) div SecsFreq;
end;

procedure NewTimer(var ET : EventTimer; Tics : LongInt);
  {-Returns a set EventTimer that will expire in Tics}
begin
  {Max acceptable value is 24 hours}
  if Tics > TicsPerDay then
    Tics := TicsPerDay;

  with ET do begin
    StartTics := BiosTics^;
    ExpireTics := StartTics + Tics;
  end;
end;

procedure NewTimerSecs(var ET : EventTimer; Secs : LongInt);
  {-Returns a set EventTimer}
begin
  NewTimer(ET, Secs2Tics(Secs));
end;

function TimerExpired(ET : EventTimer) : Boolean;
  {-Returns True if ET has expired}
var
  CurTics : LongInt;
begin
  with ET do begin
    {Get current tics; assume timer has expired}
    CurTics := BiosTics^;
    TimerExpired := True;

    {Check normal expiration}
    if CurTics > ExpireTics then
      Exit;
    {Check wrapped CurTics}
    if (CurTics < StartTics) and ((CurTics + TicsPerDay) > ExpireTics) then
      Exit;

    {If we get here, timer hasn't expired yet}
    TimerExpired := False;
  end;
end;

function ElapsedTime(ET : EventTimer) : LongInt;
  {-Returns elapsed time, in tics, for this timer}
var
  CurTics : LongInt;
begin
  with ET do begin
    CurTics := BiosTics^;
    if CurTics >= StartTics then
      {No midnight wrap yet}
      ElapsedTime := CurTics - StartTics
    else
      {Got a midnight wrap, account for it}
      ElapsedTime := (TicsPerDay - StartTics) + CurTics;
  end;
end;

function ElapsedTimeInSecs(ET : EventTimer) : LongInt;
  {-Returns elapsed time, in seconds, for this timer}
begin
  ElapsedTimeInSecs := Tics2Secs(ElapsedTime(ET));
end;

function ElapsedTimeInMSecs(ET : EventTimer) : LongInt;
  {-Returns elapsed time, in milliseconds, for this timer}
begin
  ElapsedTimeInMSecs := ElapsedTime(ET) * 55;
end;

function RemainingTime(ET : EventTimer) : LongInt;
  {-Returns remaining time, in tics, for this timer}
var
  CurTics : LongInt;
  RemainingTics : LongInt;
begin
  with ET do begin
    CurTics := BiosTics^;
    if CurTics >= StartTics then
      {No midnight wrap yet}
      RemainingTics := ExpireTics - CurTics
    else
      {Got a midnight wrap, account for it}
      RemainingTics := (ExpireTics - TicsPerDay) - CurTics;
  end;
  if RemainingTics < 0 then
    RemainingTime := 0
  else
    RemainingTime := RemainingTics;
end;

function RemainingTimeInSecs(ET : EventTimer) : LongInt;
  {-Returns remaining time, in seconds, for this timer}
begin
  RemainingTimeInSecs := Tics2Secs(RemainingTime(ET));
end;

function RemainingTimeInMSecs(ET : EventTimer) : LongInt;
  {-Returns remaining time (in milliseconds) for this timer}
begin
  RemainingTimeInMSecs := RemainingTime(ET) * 55;
end;

procedure DelayTics(Tics : LongInt);
  {-Delay for Tics tics}
var
  ET : EventTimer;
begin
  if Tics <= 0 then
    Exit
  else if Tics > TicsPerDay then
    Tics := TicsPerDay;

  NewTimer(ET, Tics);
  repeat
  until TimerExpired(ET);
end;

{!!.02}
{The following 3 BASM routines completely replace the old Pascal}
{versions of Delay and CalibrateDelay.                          }

procedure DelayMS; assembler;
asm
@@DMSdec:
  sub   ax,1
  sbb   dx,0
  jc    @@DMSexit
  cmp   bl,es:[di]
  je    @@DMSdec
@@DMSexit:
end;


procedure Delay(MS : Word); assembler;
asm
  mov   cx,MS
  jcxz  @@DelayExit
  mov   es,BiosDataSele
  xor   di,di
  mov   bl,es:[di];
@@DelayLoop:
  mov   ax,CountsPerMS
  xor   dx,dx
  call  DelayMS
  loop  @@DelayLoop
@@DelayExit:
end;

procedure CalibrateDelay; assembler;
asm
  mov   es,BiosDataSele
  mov   di,6ch
  sti
  mov   bl,es:[di]
@@WaitForChange:
  cmp   bl,es:[di]
  jz    @@WaitForChange

  mov   ax,0ffe4h
  cwd
  mov   bl,es:[di]
  call  DelayMS
  not   ax
  not   dx
  mov   cx,55
  test  dx,dx
  jne   @@speedfake
  cmp   ax,4A7h
  jg    @@speedfake
  div   cx
  jmp   @@speedset
@@speedfake:
  mov       ax,0FFFFh
@@speedset:
  mov   CountsPerMS,ax
end;

begin
  BiosTics := Ptr(BiosDataSele, $6C);
  BiosTicsLow := Ptr(BiosDataSele, $6C);
  CalibrateDelay;  {initializes CountsPerMs for Delay}
end.
