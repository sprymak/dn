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
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
UNIT
  ExtraMem;

INTERFACE uses Objects;

CONST
  EMSFound: Boolean = false;
  XMSFound: Boolean = false;
  XMSControl:    Pointer = nil;
  XMS_IOsts:     Byte    = 0;
  EMSSize:       Word    = 0;
  EMSBlockSize:  Word    = $C000;

{$IFNDEF NOEXTRA}
{ EMS stream state variables }

  EmsCurHandle: Word = $FFFF;
  EmsCurPage: Word = $FFFF;
{$ENDIF}


Function EMSPresent: Boolean;
function XMSFree: Word;
Function EMSFreePages: Word;

procedure InitExtraMem;

type

  PXMSStream = ^TXMSStream;
  TXMSStream =
{$IFNDEF NOEXTRA}
    object(TStream)
      Handle : Word;              { XMS handle }
      BlocksUsed : Word;          { Number of 1K blocks used. Always allocates
                                    at least one byte more than Size. }
      Size : LongInt;             { The current size of the stream }

      constructor Init(MinSize,MaxSize:longint);
      destructor Done; virtual;

      function  GetPos : LongInt; virtual;
      function  GetSize : LongInt; virtual;
      procedure Read(var Buf; Count : Word); virtual;
      procedure Seek(Pos : LongInt); virtual;
      procedure Truncate; virtual;
      procedure Write(const Buf; Count : Word); virtual;

      procedure NewBlock;         { Internal method to allocate a block }
      procedure FreeBlock;        { Internal method to free one block }
{$ELSE}{NOEXTRA}
    object(TMemoryStream)
{$ENDIF}
    end;

{ TEmsStream }

  PEmsStream = ^TEmsStream;
  TEmsStream =
{$IFNDEF NOEXTRA}
  object(TStream)
    Handle: Word;
    PageCount: Word;
    Size: Longint;
    constructor Init(MinSize, MaxSize: Longint);
    destructor Done; virtual;
    function  GetPos: Longint; virtual;
    function  GetSize: Longint; virtual;
    procedure Read(var Buf; Count: Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Word); virtual;
{$ELSE}{NOEXTRA}
  object(TMemoryStream)
{$ENDIF}
  end;

{$IFNDEF NOEXTRA}
function xms_MemAvail : Longint;
  { Returns total of available XMS bytes. }
function xms_MaxAvail : Longint;
  { Returns size of largest available XMS block in bytes. }
{$ENDIF}

IMPLEMENTATION uses Commands, Startup{$IFDEF DPMI}, Dpmi{$ENDIF};

function EMSPresent: Boolean;
{$IFDEF VIRTUALPASCAL} Begin EMSPresent:=False end; {$ELSE}
{$IFNDEF DPMI}
assembler;
asm
 push ds
 push cs
 pop  ds
 lea  dx, @EMSName
 mov  ax, 3d00H
 int  21H
 pop  ds
 jc   @Exit
 mov  bx, ax
 push bx
 mov  ax, 4407H
 int  21H
 pop  bx
 push ax
 mov  ah, 3eh
 int  21h
 pop  ax
 or   al, al
 jz   @Exit
 mov  ax, 3567h
 int  21h
 mov  ax, es
 or   ax, bx
 jz   @Exit
 mov  ax, 4000h
 int  67h
 or   ah, ah
 jnz  @Exit
 mov  al, 1
 jmp  @Exit1
@EMSName:
 db  'EMMXXXX0',0
@Exit:
 xor ax, ax
@Exit1:
end;
{$ELSE}
var
  F : file;
  R:  DPMIRegisters;
begin
  EMSPresent := false;
  Assign(F, 'EMMXXXX0');
  Reset(F);
  while IoResult = 0 do
  begin
    Close(F);
    FillChar(R, SizeOf(R), #0);
    R.AX:=$3567;
    SimulateRealModeInt($21,R);
    if (R.ES=0) and (R.BX=0) then break;
    R.AX:=$4000;
    SimulateRealModeInt($67,R);
    if (R.AH<>0) then break;
    R.AX:=$4100;
    SimulateRealModeInt($67,R);
    if (R.AH<>0) then break;
    EMSPresent:=True;
    R.AX:=$4200;
    SimulateRealModeInt($67,R);
    EMSSize:=R.DX*16;
    break;
  end;
end;
{$ENDIF}
{$ENDIF VP}

function EMSFreePages: Word;
{$IFDEF VIRTUALPASCAL} Begin EMSFreePages:=0 end; {$ELSE}
{$IFNDEF DPMI}
assembler;
asm
 mov ax, 4200h
 int 67h
 mov ax, bx
end;
{$ELSE}
var
  R:  DPMIRegisters;
begin
  EMSFreePages:=0;
  if not EMSFound then Exit;
  FillChar(R, SizeOf(R), 0);
  R.AX:=$4200;
  SimulateRealModeInt($67,R);
  EMSFreePages:=R.BX;
end;
{$ENDIF}
{$ENDIF VP}

procedure DetectXMS;
{$IFDEF VIRTUALPASCAL} Begin end; {$ELSE}
{$IFNDEF DPMI}
assembler;
asm
     mov     ax,4300h
     int     2Fh
     cmp     al,80h
     jne     @@1
     mov     ax,4310h
     int     2Fh

     mov     word ptr [XMSControl],bx
     mov     word ptr [XMSControl+2],es

     mov     ah,00h
     call    [XMSControl]
     or      ax, ax
     jz      @@1
     cmp     ax, 1
     jz      @@1
     mov     al, 1
     mov     XMSFound, al
@@1:
end;
{$ELSE}
var
  R: DPMIRegisters;
  S1, S2: Word;
begin
  xms_IOsts:=$80;
  FillChar(R, SizeOf(R), #0);
  R.AX:=$4300;
  SimulateRealModeInt($2F, R);
  if R.AL<>$80 then exit;
  R.AX:=$4310;
  SimulateRealModeInt($2F, R);
  if (R.ES=0) and (R.BX=0) then exit;
  PtrRec(XMSControl).Ofs:=R.BX;
  PtrRec(XMSControl).Seg:=R.ES;
  R.AX:=0;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  if (R.AX=0) or (R.AX=1) then exit;
{ XMSVersion:=R.AX;}
  XMSFound:=True;
  R.AX:=$0900; R.DX:=$0001;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  S1:=R.DX;
  R.AX:=$0900; R.DX:=$0001;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  S2:=R.DX;
{ R.AX:=$0f00; R.BX:=$0020; R.DX:=S1;}
{ R.IP:=PtrRec(XMSControl).Ofs;}
{ R.CS:=PtrRec(XMSControl).Seg;}
{ CallFarRealModeProc(0,nil,R);}
{ if R.AX<>0 then XMSReAlloc:=True;}
  R.AX:=$0A00; R.DX:=S1;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  R.AX:=$0A00; R.DX:=S2;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  xms_IOsts:=0;
end;
{$ENDIF}
{$ENDIF VP}

function XMSFree: Word;
{$IFDEF VIRTUALPASCAL} Begin XMSFree:=0 end; {$ELSE}
{$IFNDEF DPMI}
assembler;
asm
     xor ax, ax
     cmp XMSFound, True
     jne @@exit
     xor bx, bx          { for better error checking, since qemm
                           6.0 leaves bl unchanged on success }
     mov ah, 08h
     call [XMSControl]
     mov ax, dx
     or  bl, bl
     jz  @@exit
     xor ax,ax
     mov byte ptr XMS_IOsts,bl
@@exit:
end;
{$ELSE}
var
  R: DPMIRegisters;
begin
  XMSFree:=0;
  if not XMSFound then Exit;
  FillChar(R, SizeOf(R), #0);
  R.BX:=0; R.AX:=$0800;
  R.IP:=PtrRec(XMSControl).Ofs;
  R.CS:=PtrRec(XMSControl).Seg;
  CallFarRealModeProc(0,nil,R);
  if R.BL<>0 then
    XMS_IOsts:=R.BL else
    XMSFree:=R.DX;
end;
{$ENDIF}
{$ENDIF VP}

{$IFNDEF NOEXTRA}
{ Stream error handler                                  }
{ In    AX    = Error info                              }
{       DX    = Error code                              }
{       ES:DI = Stream object pointer                   }
{ Uses  AX,BX,CX,DX,SI                                  }

procedure DoStreamError; near; assembler;
asm
        PUSH    ES
        PUSH    DI
        PUSH    DX
        PUSH    AX
        PUSH    ES
        PUSH    DI
        MOV     DI,ES:[DI]
{I absolutely don't understand how could this construction be successfully
used. The only probable explanation could be absolute EMM386's (or whatever
memory manager is used during development) ignorance to such program faults.
But my QEMM386 horribly complains!
    Anyway it doesn't seem to do what was originally intended. I suspect it
should call Error method for current TStream (or descendant) instance (to
which a pointer is stored in ES:DI) but it jumps somewhere out of the
program code. QEMM reports exceptions and the tracing fails because the
target is unknown to the integrated BP debugger.
    I'm using the same method used by Borland in their Objects unit. To make
the solution more clear and correct I have included appropriate constants
in the DN's Objects unit.
    If someone knows better method how to fix this problem, apply it!}
        {CALL    DWORD PTR [DI].TStream.Error} {-JITR-}
        CALL    DWORD PTR [DI].TStream_Error   {-JITR-}
        POP     DI
        POP     ES
end;


var xms_Addr : Pointer;

  function MinLong(x,y:longint):longint;
  begin
    if x<y then
      MinLong := x
    else
      MinLong := y;
  end;

  function MaxLong(x,y:longint):longint;
  begin
    MaxLong := -MinLong(-x,-y);
  end;

const
  xms_Initialized : Boolean = False;
  { This allows us to avoid a unit initialization section }

  xms_BlockSize = 1024;

  { - Some Xms - Procedures that I need ! -}

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure MoveMem(ToAddress : Pointer; ToHandle : Word;
                    FromAddress : Pointer; FromHandle : Word;
                    Size : LongInt);
  begin
    asm
      mov     byte ptr xms_IOsts,0
      mov     ah,$0B
      lea     si,Size
      push    ds
      pop     es
      push    ss
      pop     ds
      call    es:[xms_Addr]
      push    es
      pop     ds
      or      ax,ax
      jnz     @@1
      mov     byte ptr xms_IOsts,bl
@@1:
    end;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  function GetByte(Handle : Word; FromAddress : LongInt) : Byte;
  var TempBuf : array[0..1] of Byte;
  begin
    MoveMem(@TempBuf, 0, Pointer(FromAddress and $FFFFFFFE), Handle, 2);
    GetByte := TempBuf[FromAddress and $00000001];
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure SetByte(Handle : Word; ToAddress : LongInt; Value : Byte);
  var TempBuf : array[0..1] of Byte;
  begin
    MoveMem(@TempBuf, 0, Pointer(ToAddress and $FFFFFFFE), Handle, 2);
    TempBuf[ToAddress and $00000001] := Value;
    MoveMem(Pointer(ToAddress and $FFFFFFFE), Handle, @TempBuf, 0, 2);
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure xms_Init;
  begin
    if not xms_Initialized then
    begin
      xms_IOsts := 0;
      xms_Addr := nil;
      asm
        mov     ax,$4300
        int     $2F
        cmp     al,$80
        jne     @@1
        mov     ax,$4310
        int     $2F
        mov     word ptr xms_Addr,bx
        mov     word ptr xms_Addr+2,es
        jmp     @@2
@@1:
        mov     byte ptr xms_IOsts,$80
@@2:
      end;
      if xms_IOsts = 0 then
        xms_Initialized := True;
    end;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  function xms_GetMem(KB : Word) : Word; Assembler;
  asm
    mov     xms_IOsts,0
    mov     ah,$09
    mov     dx,word ptr KB
    call    [xms_Addr]
    or      ax,ax
    jz      @@1
    mov     ax,dx
    jmp     @@2
@@1:
    mov     byte ptr xms_IOsts,bl
@@2:
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure xms_FreeMem(Handle : Word);
  begin
    asm
      mov     xms_IOsts,0
      mov     ah,$0A
      mov     dx,word ptr Handle
      call    [xms_Addr]
      or      ax,ax
      jnz     @@1
      mov     byte ptr xms_IOsts,bl
@@1:
    end;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure xms_ResizeMem(Size, Handle : Word);
  begin
    asm
      mov     ah,$0F
      mov     bx,word ptr Size
      mov     dx,word ptr Handle
      call    [xms_Addr]
      or      ax,ax
      jnz     @@1
      mov     byte ptr xms_IOsts,bl
@@1:
    end;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure xms_MoveFrom(Size, Handle : Word; FromAddress : LongInt;
                         ToAddress : Pointer);
  type ByteArr = array[0..MaxInt] of Byte;
    BytePtr = ^ByteArr;
  begin
    if Size = 0 then Exit;
    if Odd(FromAddress) then begin
      BytePtr(ToAddress)^[0] := GetByte(Handle, FromAddress);
      if xms_IOsts <> 0 then Exit;
      Dec(Size);
      Inc(FromAddress);
      Inc(LongInt(ToAddress));
    end;
    MoveMem(ToAddress, 0, Pointer(FromAddress), Handle, Size and $FFFE);
    if xms_IOsts <> 0 then Exit;
    if Odd(Size)
    then BytePtr(ToAddress)^[Size-1] := GetByte(Handle, FromAddress+Size-1);
    if xms_IOsts <> 0 then Exit;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  procedure xms_MoveTo(Size, Handle : Word; FromAddress : Pointer;
                       ToAddress : LongInt);
  type ByteArr = array[0..MaxInt] of Byte;
    BytePtr = ^ByteArr;
  begin
    if Size = 0 then Exit;
    if Odd(ToAddress) then begin
      SetByte(Handle, ToAddress, BytePtr(FromAddress)^[0]);
      if xms_IOsts <> 0 then Exit;
      Dec(Size);
      Inc(LongInt(FromAddress));
      Inc(ToAddress);
    end;
    MoveMem(Pointer(ToAddress), Handle, FromAddress, 0, Size and $FFFE);
    if xms_IOsts <> 0 then Exit;
    if Odd(Size)
    then SetByte(Handle, ToAddress+Size-1, BytePtr(FromAddress)^[Size-1]);
    if xms_IOsts <> 0 then Exit;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  constructor TXMSStream.Init(MinSize, MaxSize : longint);
  var
    MinBlocks,MaxBlocks : word;
  begin
    xms_IOsts := 0;
    TStream.Init;
    xms_Init;
    BlocksUsed := 0;
    Size := MaxSize;
    Position := 0;
    Handle := 0;
    MaxSize := MinLong(MaxSize,xms_Maxavail);
    MaxBlocks := (MaxSize + xms_Blocksize -1) div xms_Blocksize;
    MinBlocks := (MinSize + xms_Blocksize -1) div xms_Blocksize;
    if MinBlocks < 1 then
      MinBlocks := 1;
    if MaxBlocks < MinBlocks then
      MaxBlocks := MinBlocks;
    if xms_IOsts <> $00 then
      Error(stInitError, xms_IOsts)
    else
    begin
      Handle := xms_GetMem(MaxBlocks);
      if xms_IOsts <> $00 then
        Error(stInitError, xms_IOsts)
      else
      begin
        xms_ResizeMem(MinBlocks,Handle);
        BlocksUsed := MinBlocks;
        if xms_IOsts <> $00 then
          Error(stInitError, xms_IOsts);
      end;
    end;
  end;

  function TXMSStream.GetPos : LongInt;
  begin
    GetPos := Position;
  end;

  function TXMSStream.GetSize : LongInt;
  begin
    GetSize := Size;
  end;

  procedure TXMSStream.Read(var Buf; Count : Word);
  begin
    if Status = stOK then
      if Position+Count > Size then
        Error(stReaderror, 0)
      else
      begin
        xms_MoveFrom(Count, Handle, Position, @Buf);
        if xms_IOsts <> 0 then
          Error(stReaderror, xms_IOsts)
        else
          Inc(Position, Count);
      end;
  end;

  procedure TXMSStream.Seek(Pos : LongInt);
  begin
    if Status = stOK then
      if Pos > Size then            { 1.4:  bug fix }
        Error(stReaderror, Pos)
      else
        Position := Pos;
  end;

  procedure TXMSStream.Truncate;
  begin
    if Status = stOK then
    begin
      Size := Position;
      while (BlocksUsed > (Size div xms_BlockSize+1)) do FreeBlock;
    end;
  end;

  procedure TXMSStream.Write(const Buf; Count : Word);
  begin
    while (Status = stOK)
    and (Position+Count > LongMul(xms_BlockSize, BlocksUsed)) do
      NewBlock;
    if Status = stOK then
    begin
      xms_MoveTo(Count, Handle, @Buf, Position);
      if xms_IOsts <> 0 then
        Error(stWriteError, xms_IOsts)
      else
        Inc(Position, Count);
      if Position > Size then
        Size := Position;
    end;
  end;

  procedure TXMSStream.NewBlock;
  begin
    xms_ResizeMem(Succ(BlocksUsed), Handle);
    if xms_IOsts <> 0 then
      Error(stWriteError, xms_IOsts)
    else
      Inc(BlocksUsed);
  end;

  procedure TXMSStream.FreeBlock;
  begin
    Dec(BlocksUsed);
    xms_ResizeMem(BlocksUsed, Handle);
  end;

  function xms_MaxAvail : Longint;
  begin
    xms_Init;
    if xms_IOsts = 0 then
    asm
      xor       bx, bx          { for better error checking, since qemm
6.0 leaves bl unchanged on success }
      mov     ah,$08
      call    [xms_Addr]
      or      bl, bl            { extended error checking by MM 22.02.93 }
      jz      @OK
      mov     byte ptr xms_IOsts,bl
      xor     ax,ax
@OK:
      mov     dx,xms_Blocksize
      mul     dx
      mov     word ptr @result,ax
      mov     word ptr @result[2],dx
    end
    else
      xms_MaxAvail := 0;
  end;

  (* /////////////////////////////////////////////////////////////////////// *)

  function xms_MemAvail : Longint;
  begin
    xms_Init;
    if xms_IOsts = 0 then
    asm
      xor       bx, bx          { for better error checking, since qemm
                                  6.0 leaves bl unchanged on success }
      mov     ah,$08
      call    [xms_Addr]
      or      bl, bl            { extended error checking by MM 22.02.93 }
      jz      @OK
      mov     byte ptr xms_IOsts,bl
      xor     dx,dx
@OK:
      mov     ax,dx
      mov     dx,xms_blocksize
      mul     dx
      mov     word ptr @result,ax
      mov     word ptr @result[2],dx
    end
    else
      xms_MemAvail := 0;
  end;

  destructor TXMSStream.Done;
  begin
{    Seek(0);
    Truncate; }
    if xms_Initialized and (BlocksUsed > 0) then
      xms_FreeMem(Handle);
  end;

{ TEmsStream }

const
  EmsPageSize = $4000;

var
  EmsBaseSeg: Word;
  EmsVersion: Byte;

procedure EmsSelectPage; near; assembler;
asm
        MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        MOV     CX,EmsPageSize
        DIV     CX
        SUB     CX,DX
        MOV     SI,DX
        MOV     DX,ES:[DI].TEmsStream.Handle
        CMP     DX,EmsCurHandle
        JNE     @@1
        CMP     AX,EmsCurPage
        JE      @@3
@@1:    MOV     BX,AX
        MOV     AX,4400H
        INT     67H
        MOV     AL,AH
        AND     AX,0FFH
        JE      @@2
        MOV     DX,stError
        JMP     @@3
@@2:    MOV     EmsCurHandle,DX
        MOV     EmsCurPage,BX
@@3:
end;

procedure EmsSetPages; near; assembler;
asm
        CMP     EmsVersion,40H
        JAE     @@1
        MOV     AX,84H
        JMP     @@2
@@1:    MOV     DX,ES:[DI].TEmsStream.Handle
        MOV     BX,AX
        MOV     AH,51H
        INT     67H
        MOV     AL,AH
        AND     AX,0FFH
        JNE     @@2
        MOV     ES:[DI].TEmsStream.PageCount,BX
@@2:
end;

constructor TEmsStream.Init(MinSize, MaxSize: LongInt); assembler;
const
  EmsDeviceLen = 8;
  EmsDeviceStr: array[1..EmsDeviceLen] of Char = 'EMMXXXX0';
asm
        XOR     AX,AX
        PUSH    AX
        LES     DI,Self
        PUSH    ES
        PUSH    DI
        CALL    TStream.Init
        MOV     AX,3567H
        INT     21H
        MOV     CX,EmsDeviceLen
        MOV     SI,OFFSET EmsDeviceStr
        MOV     DI,0AH
        CLD
        REP     CMPSB
        LES     DI,Self
        MOV     AX,-1
        JNE     @@3
        MOV     AH,41H
        INT     67H
        MOV     EmsBaseSeg,BX
        MOV     AH,46H
        INT     67H
        MOV     EmsVersion,AL
        MOV     CX,EmsPageSize
        MOV     AX,MinSize.Word[0]
        MOV     DX,MinSize.Word[2]
        ADD     AX,EmsPageSize-1
        ADC     DX,0
        DIV     CX
        MOV     BX,AX
        CMP     EmsVersion,40H
        JAE     @@2
        PUSH    AX
        MOV     AX,MaxSize.Word[0]
        MOV     DX,MaxSize.Word[2]
        ADD     AX,EmsPageSize-1
        ADC     DX,0
        DIV     CX
        MOV     CX,AX
        MOV     AH,42H
        INT     67H
        POP     AX
        CMP     BX,CX
        JB      @@1
        MOV     BX,CX
@@1:    CMP     BX,AX
        JA      @@2
        MOV     BX,AX
@@2:    MOV     AH,43H
        INT     67H
        MOV     AL,AH
        AND     AX,0FFH
        JE      @@4
@@3:    MOV     DX,stInitError
        CALL    DoStreamError
        MOV     DX,-1
        XOR     BX,BX
@@4:    MOV     ES:[DI].TEmsStream.Handle,DX
        MOV     ES:[DI].TEmsStream.PageCount,BX
        XOR     AX,AX
        MOV     AX,MaxSize.Word[0]
        MOV     DX,MaxSize.Word[2]
        MOV     ES:[DI].TEmsStream.Size.Word[0],AX
        MOV     ES:[DI].TEmsStream.Size.Word[2],DX
end;

destructor TEmsStream.Done; assembler;
asm
        LES     DI,Self
        MOV     DX,ES:[DI].TEmsStream.Handle
        CMP     DX,-1
        JE      @@1
        MOV     AH,45H
        INT     67H
@@1:    XOR     AX,AX
        PUSH    AX
        PUSH    ES
        PUSH    DI
        CALL    TStream.Done
end;

function TEmsStream.GetPos: Longint; assembler;
asm
        LES     DI,Self
        CMP     ES:[DI].TEmsStream.Status,0
        JNE     @@1
        MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        JMP     @@2
@@1:    MOV     AX,-1
        CWD
@@2:
end;

function TEmsStream.GetSize: Longint; assembler;
asm
        LES     DI,Self
        CMP     ES:[DI].TEmsStream.Status,0
        JNE     @@1
        MOV     AX,ES:[DI].TEmsStream.Size.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Size.Word[2]
        JMP     @@2
@@1:    MOV     AX,-1
        CWD
@@2:
end;

procedure TEmsStream.Read(var Buf; Count: Word); assembler;
asm
        LES     DI,Self
        XOR     BX,BX
        CMP     BX,ES:[DI].TEmsStream.Status
        JNE     @@3
        MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        ADD     AX,Count
        ADC     DX,BX
        CMP     DX,ES:[DI].TEmsStream.Size.Word[2]
        JA      @@1
        JB      @@7
        CMP     AX,ES:[DI].TEmsStream.Size.Word[0]
        JBE     @@7
@@1:    XOR     AX,AX
        MOV     DX,stReadError
@@2:    CALL    DoStreamError
@@3:    LES     DI,Buf
        MOV     CX,Count
        XOR     AL,AL
        CLD
        REP     STOSB
        JMP     @@8
@@5:    PUSH    BX
        CALL    EmsSelectPage
        POP     BX
        JNE     @@2
        MOV     AX,Count
        SUB     AX,BX
        CMP     CX,AX
        JB      @@6
        MOV     CX,AX
@@6:    ADD     ES:[DI].TEmsStream.Position.Word[0],CX
        ADC     ES:[DI].TEmsStream.Position.Word[2],0
        PUSH    ES
        PUSH    DS
        PUSH    DI
        LES     DI,Buf
        ADD     DI,BX
        ADD     BX,CX
        MOV     DS,EmsBaseSeg
        CLD
        REP     MOVSB
        POP     DI
        POP     DS
        POP     ES
@@7:    CMP     BX,Count
        JB      @@5
@@8:
end;

procedure TEmsStream.Seek(Pos: Longint); assembler;
asm
        LES     DI,Self
        MOV     AX,Pos.Word[0]
        MOV     DX,Pos.Word[2]
        OR      DX,DX
        JNS     @@1
        XOR     AX,AX
        CWD
@@1:    MOV     ES:[DI].TEmsStream.Position.Word[0],AX
        MOV     ES:[DI].TEmsStream.Position.Word[2],DX
end;

procedure TEmsStream.Truncate; assembler;
asm
        LES     DI,Self
        XOR     BX,BX
        CMP     ES:[DI].TEmsStream.Status,BX
        JNE     @@2
        CMP     EmsVersion,40H
        JB      @@1
        MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        ADD     AX,EmsPageSize-1
        ADC     DX,BX
        MOV     CX,EmsPageSize
        DIV     CX
        CALL    EmsSetPages
        JE      @@1
        MOV     DX,stError
        CALL    DoStreamError
        JMP     @@2
@@1:    MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        MOV     ES:[DI].TEmsStream.Size.Word[0],AX
        MOV     ES:[DI].TEmsStream.Size.Word[2],DX
@@2:
end;

procedure TEmsStream.Write(const Buf; Count: Word); assembler;
asm
        LES     DI,Self
        XOR     BX,BX
        CMP     BX,ES:[DI].TEmsStream.Status
        JNE     @@7
        MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        ADD     AX,Count
        ADC     DX,BX
        ADD     AX,EmsPageSize-1
        ADC     DX,BX
        MOV     CX,EmsPageSize
        DIV     CX
        CMP     AX,ES:[DI].TEmsStream.PageCount
        JBE     @@4
        PUSH    BX
        CALL    EmsSetPages
        POP     BX
        JE      @@4
@@1:    MOV     DX,stWriteError
        CALL    DoStreamError
        JMP     @@7
@@2:    PUSH    BX
        CALL    EmsSelectPage
        POP     BX
        JNE     @@1
        MOV     AX,Count
        SUB     AX,BX
        CMP     CX,AX
        JB      @@3
        MOV     CX,AX
@@3:    ADD     ES:[DI].TEmsStream.Position.Word[0],CX
        ADC     ES:[DI].TEmsStream.Position.Word[2],0
        PUSH    ES
        PUSH    DS
        PUSH    DI
        MOV     DI,SI
        MOV     ES,EmsBaseSeg
        LDS     SI,Buf
        ADD     SI,BX
        ADD     BX,CX
        CLD
        REP     MOVSB
        POP     DI
        POP     DS
        POP     ES
@@4:    CMP     BX,Count
        JB      @@2
@@5:    MOV     AX,ES:[DI].TEmsStream.Position.Word[0]
        MOV     DX,ES:[DI].TEmsStream.Position.Word[2]
        CMP     DX,ES:[DI].TEmsStream.Size.Word[2]
        JB      @@7
        JA      @@6
        CMP     AX,ES:[DI].TEmsStream.Size.Word[0]
        JBE     @@7
@@6:    MOV     ES:[DI].TEmsStream.Size.Word[0],AX
        MOV     ES:[DI].TEmsStream.Size.Word[2],DX
@@7:
end;

{$ENDIF}{NOEXTRA}

procedure InitExtraMem;
BEGIN
{$IFNDEF NOEXTRA}
 if SystemData.Options and ossDisableXMS <> 0 then
 begin
   EMSFound := False; XMSFound := False
 end else
 begin
   EMSFound := EMSPresent;
   if EMSFound then
    asm
     mov ah, 42h
     int 67h
     mov cl, 4
     shl dx, cl
     mov EMSSize, dx
    end;
   DetectXMS;
 end;
{$ELSE NOEXTRA}
 EMSFound := EMSPresent;
 DetectXMS;
{$ENDIF}
end;

END.
