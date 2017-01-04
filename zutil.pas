{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//
//////////////////////////////////////////////////////////////////////////}

Unit ZUtil;

{
  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}

interface

{$I zconf.inc}

{ Type declarations }

type
  {Byte   = usigned char;  8 bits}
  Bytef  = byte;
  charf  = byte;

  int    = integer;
  intf   = int;
{$IFDEF MSDOS}
  uInt   = Word;
{$ELSE}
  uInt   = cardinal;     { 16 bits or more }
{$ENDIF}
  uIntf  = uInt;

  Long   = longint;
  uLong  = LongInt;      { 32 bits or more }
  uLongf = uLong;

  voidp  = pointer;
  voidpf = voidp;
  pBytef = ^Bytef;
  pIntf  = ^intf;
  puIntf = ^uIntf;
  puLong = ^uLongf;

  ptr2int = uInt;
{ a pointer to integer casting is used to do pointer arithmetic.
  ptr2int must be an integer type and sizeof(ptr2int) must be less
  than sizeof(pointer) - Nomssi }

type
  zByteArray = array[0..(MaxInt div SizeOf(Bytef))-1] of Bytef;
  pzByteArray = ^zByteArray;
type
  zIntfArray = array[0..(MaxInt div SizeOf(Intf))-1] of Intf;
  pzIntfArray = ^zIntfArray;
type
  zuIntArray = array[0..(MaxInt div SizeOf(uInt))-1] of uInt;
  PuIntArray = ^zuIntArray;

{ Type declarations - only for deflate }

type
  uch  = Byte;
  uchf = uch; { FAR }
  ush  = Word;
  ushf = ush;
  ulg  = LongInt;

  unsigned = uInt;

  pcharf = ^charf;
  puchf = ^uchf;
  pushf = ^ushf;

type
  zuchfArray = zByteArray;
  puchfArray = ^zuchfArray;
type
  zushfArray = array[0..(MaxInt div SizeOf(ushf))-1] of ushf;
  pushfArray = ^zushfArray;

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
procedure zmemzero(destp : pBytef; len : uInt);
procedure zcfree(opaque : voidpf; ptr : voidpf);
function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;

implementation


{$IFDEF CALLDOS}
uses
  WinDos;
{$ENDIF}
{$IFNDEF FPC}
  {$IFDEF DPMI}
  uses
    WinAPI;
  {$ENDIF}
{$ENDIF}

{$IFNDEF DPMI}
{$ifdef ver70}
  {$define HugeMem}
{$endif}
{$ifdef ver60}
  {$define HugeMem}
{$endif}
{$ENDIF}

{$IFDEF CALLDOS}
{ reduce your application memory footprint with $M before using this }
function dosAlloc (Size : Longint) : Pointer;
var
  regs: TRegisters;
begin
  regs.bx := (Size + 15) div 16; { number of 16-bytes-paragraphs }
  regs.ah := $48;                { Allocate memory block }
  msdos(regs);
  if regs.Flags and FCarry <> 0 then
    DosAlloc := NIL
  else
    DosAlloc := Ptr(regs.ax, 0);
end;


function dosFree(P : pointer) : boolean;
var
  regs: TRegisters;
begin
  dosFree := FALSE;
  regs.bx := Seg(P^);             { segment }
  if Ofs(P) <> 0 then
    exit;
  regs.ah := $49;                { Free memory block }
  msdos(regs);
  dosFree := (regs.Flags and FCarry = 0);
end;
{$ENDIF}

{$IFDEF HugeMem}

{ The code below is extremely version specific to the TP 6/7 heap manager!!}
type
  PFreeRec = ^TFreeRec;
  TFreeRec = record
    next: PFreeRec;
    size: Pointer;
  end;
type
  HugePtr = voidpf;

type
  LH = record
    L, H : word;
  end;
{ --- to avoid Mark and Release --- }
const
  MaxAllocEntries = 50;
type
  TMemRec = record
    orgvalue,
    value : pointer;
    size: longint;
  end;
const
  allocatedCount : 0..MaxAllocEntries = 0;
var
  allocatedList : array[0..MaxAllocEntries-1] of TMemRec;

 function NewAllocation(ptr0, ptr : pointer; memsize : longint) : boolean;
 begin
   if (allocatedCount < MaxAllocEntries) and (ptr0 <> NIL) then
   begin
     with allocatedList[allocatedCount] do
     begin
       orgvalue := ptr0;
       value := ptr;
       size := memsize;
     end;
     Inc(allocatedCount);  { we don't check for duplicate }
     NewAllocation := TRUE;
   end
   else
     NewAllocation := FALSE;
 end;


{ --------------------------------- }

 procedure IncPtr(var p:pointer;count:word);
 { Increments pointer }
 begin
   inc(LH(p).L,count);
   if LH(p).L < count then
     inc(LH(p).H,SelectorInc);  { $1000 }
 end;

 procedure DecPtr(var p:pointer;count:word);
 { decrements pointer }
 begin
   if count > LH(p).L then
     dec(LH(p).H,SelectorInc);
   dec(LH(p).L,Count);
 end;

 procedure IncPtrLong(var p:pointer;count:longint);
 { Increments pointer; assumes count > 0 }
 begin
   inc(LH(p).H,SelectorInc*LH(count).H);
   inc(LH(p).L,LH(Count).L);
   if LH(p).L < LH(count).L then
     inc(LH(p).H,SelectorInc);
 end;

 procedure DecPtrLong(var p:pointer;count:longint);
 { Decrements pointer; assumes count > 0 }
 begin
   if LH(count).L > LH(p).L then
     dec(LH(p).H,SelectorInc);
   dec(LH(p).L,LH(Count).L);
   dec(LH(p).H,SelectorInc*LH(Count).H);
 end;
 { The next section is for real mode only }

function Normalized(p : pointer)  : pointer;
var
  count : word;
begin
  count := LH(p).L and $FFF0;
  Normalized := Ptr(LH(p).H + (count shr 4), LH(p).L and $F);
end;

procedure FreeHuge(var p:HugePtr; size : longint);
const
  blocksize = $FFF0;
var
  block : word;
begin
  while size > 0 do
  begin
    { block := minimum(size, blocksize); }
    if size > blocksize then
      block := blocksize
    else
      block := size;

    dec(size,block);
    freemem(p,block);
    IncPtr(p,block);    { we may get ptr($xxxx, $fff8) and 31 bytes left }
    p := Normalized(p); { to free, so we must normalize }
  end;
end;

function FreeMemHuge(ptr : pointer) : boolean;
var
  i : integer; { -1..MaxAllocEntries }
begin
  FreeMemHuge := FALSE;
  i := allocatedCount - 1;
  while (i >= 0) do
  begin
    if (ptr = allocatedList[i].value) then
    begin
      with allocatedList[i] do
        FreeHuge(orgvalue, size);

      Move(allocatedList[i+1], allocatedList[i],
           SizeOf(TMemRec)*(allocatedCount - 1 - i));
      Dec(allocatedCount);
      FreeMemHuge := TRUE;
      break;
    end;
    Dec(i);
  end;
end;

procedure GetMemHuge(var p:HugePtr;memsize:Longint);
const
  blocksize = $FFF0;
var
  size : longint;
  prev,free : PFreeRec;
  save,temp : pointer;
  block : word;
begin
  p := NIL;
  { Handle the easy cases first }
  if memsize > maxavail then
    exit
  else
    if memsize <= blocksize then
    begin
      getmem(p, memsize);
      if not NewAllocation(p, p, memsize) then
      begin
        FreeMem(p, memsize);
        p := NIL;
      end;
    end
    else
    begin
      size := memsize + 15;

      { Find the block that has enough space }
      prev := PFreeRec(@freeList);
      free := prev^.next;
      while (free <> heapptr) and (ptr2int(free^.size) < size) do
      begin
        prev := free;
        free := prev^.next;
      end;

      { Now free points to a region with enough space; make it the first one and
        multiple allocations will be contiguous. }

      save := freelist;
      freelist := free;
      { In TP 6, this works; check against other heap managers }
      while size > 0 do
      begin
        { block := minimum(size, blocksize); }
        if size > blocksize then
          block := blocksize
        else
          block := size;
        dec(size,block);
        getmem(temp,block);
      end;

      { We've got what we want now; just sort things out and restore the
        free list to normal }

      p := free;
      if prev^.next <> freelist then
      begin
        prev^.next := freelist;
        freelist := save;
      end;

      if (p <> NIL) then
      begin
        { return pointer with 0 offset }
        temp := p;
        if Ofs(p^)<>0 Then
          p := Ptr(Seg(p^)+1,0);  { hack }
        if not NewAllocation(temp, p, memsize + 15) then
        begin
          FreeHuge(temp, size);
          p := NIL;
        end;
      end;

    end;
end;

{$ENDIF}

procedure zmemcpy(destp : pBytef; sourcep : pBytef; len : uInt);
begin
  Move(sourcep^, destp^, len);
end;

function zmemcmp(s1p, s2p : pBytef; len : uInt) : int;
var
  j : uInt;
  source,
  dest : pBytef;
begin
  source := s1p;
  dest := s2p;
  for j := 0 to pred(len) do
  begin
    if (source^ <> dest^) then
    begin
      zmemcmp := 2*Ord(source^ > dest^)-1;
      exit;
    end;
    Inc(source);
    Inc(dest);
  end;
  zmemcmp := 0;
end;

procedure zmemzero(destp : pBytef; len : uInt);
begin
  FillChar(destp^, len, 0);
end;

procedure zcfree(opaque : voidpf; ptr : voidpf);
begin
  {$IFDEF DPMI}
  {h :=} GlobalFreePtr(ptr);
  {$ELSE}
    {$IFDEF CALL_DOS}
    dosFree(ptr);
    {$ELSE}
      {$ifdef HugeMem}
      FreeMemHuge(ptr);
      {$else}
      FreeMem(ptr);  { Delphi 2,3 }
      {$endif}
    {$ENDIF}
  {$ENDIF}
end;

function zcalloc (opaque : voidpf; items : uInt; size : uInt) : voidpf;
var
  p : voidpf;
  memsize : LongInt;
begin
  memsize := Long(items) * size;
  {$IFDEF DPMI}
  p := GlobalAllocPtr(gmem_moveable, memsize);
  {$ELSE}
    {$IFDEF CALLDOS}
    p := dosAlloc(memsize);
    {$ELSE}
      {$ifdef HugeMem}
      GetMemHuge(p, memsize);
      {$else}
      GetMem(p, memsize);  { Delphi: p := AllocMem(memsize); }
      {$endif}
    {$ENDIF}
  {$ENDIF}
  zcalloc := p;
end;


end.


{ edited from a SWAG posting:

In Turbo Pascal 6, the heap is the memory allocated when using the Procedures 'New' and
'GetMem'. The heap starts at the address location pointed to by 'Heaporg' and
grows to higher addresses as more memory is allocated. The top of the heap,
the first address of allocatable memory space above the allocated memory
space, is pointed to by 'HeapPtr'.

Memory is deallocated by the Procedures 'Dispose' and 'FreeMem'. As memory
blocks are deallocated more memory becomes available, but..... When a block
of memory, which is not the top-most block in the heap is deallocated, a gap
in the heap will appear. to keep track of these gaps Turbo Pascal maintains
a so called free list.

The Function 'MaxAvail' holds the size of the largest contiguous free block
_in_ the heap. The Function 'MemAvail' holds the sum of all free blocks in
the heap.

TP6.0 keeps track of the free blocks by writing a 'free list Record' to the
first eight Bytes of the freed memory block! A (TP6.0) free-list Record
contains two four Byte Pointers of which the first one points to the next
free memory block, the second Pointer is not a Real Pointer but contains the
size of the memory block.

Summary

TP6.0 maintains a linked list with block sizes and Pointers to the _next_
free block. An extra heap Variable 'Heapend' designate the end of the heap.
When 'HeapPtr' and 'FreeList' have the same value, the free list is empty.


                     TP6.0     Heapend
                +---------+ <----
                |         |
                |         |
                |         |
                |         |
                |         |
                |         |
                |         |
                |         |  HeapPtr
             +->|---------| <----
             |  |         |
             |  |---------|
             +--|  Free   |
             +->|---------|
             |  |         |
             |  |---------|
             +--|  Free   |  FreeList
                |---------| <----
                |         |  Heaporg
                |---------| <----


}
