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
{$I Stdefine.inc}

(*
   This unit provides compiler-independent mechanisms to call special
   functions, i.e. local functions/procedures, constructors, methods,
   destructors, etc. As there are no procedural variables for these
   special functions, there is no Pascal way to call them directly.
*)

unit CallSpcB;

interface

type
{$IFNDEF BIT_32}
   FramePointer = Word;
{$ELSE BIT_32}
   FramePointer = Pointer;
{$ENDIF}

function CurrentFramePointer: FramePointer;
function PreviousFramePointer: FramePointer;

function CallVoidConstructor(Ctor: Pointer; Obj: Pointer; VMT: Pointer):
  Pointer;
function CallPointerConstructor(Ctor: Pointer; Obj: pointer; VMT: Pointer;
  Param1: Pointer): Pointer;

function CallVoidMethod(Method: pointer; Obj: pointer): Pointer;
function CallPointerMethod(Method: pointer; Obj: pointer; Param1: pointer):
  Pointer;

function CallVoidLocal(Func: Pointer; Frame: FramePointer): Pointer;
function CallPointerLocal(Func: Pointer; Frame: FramePointer;
  Param1: Pointer): Pointer;

function CallVoidMethodLocal(Func: Pointer; Frame: FramePointer;
  Obj: Pointer): Pointer;
function CallPointerMethodLocal(Func: Pointer; Frame: FramePointer;
  Obj: Pointer; Param1: Pointer): Pointer;


implementation

type
  VoidConstructor = function(VmtOfs: Word; Obj: Pointer): Pointer;
  PointerConstructor = function(Param1: Pointer; VmtOfs: Word;
    Obj: Pointer): Pointer;
  VoidMethod = function(Obj: Pointer): pointer;
  PointerMethod = function(Param1: Pointer; Obj: Pointer): pointer;

function CallVoidConstructor(Ctor: pointer; Obj: Pointer;
  VMT: Pointer): Pointer;
begin
  CallVoidConstructor := VoidConstructor(Ctor)(Ofs(VMT^), Obj)
end;


function CallPointerConstructor(Ctor: Pointer; Obj: Pointer;
  VMT: Pointer; Param1: Pointer): Pointer;
begin
  CallPointerConstructor := PointerConstructor(Ctor)(Param1, Ofs(VMT^), Obj)
end;


function CallVoidMethod(Method: Pointer; Obj: Pointer): Pointer;
begin
  CallVoidMethod := VoidMethod(Method)(Obj)
end;


function CallPointerMethod(Method: Pointer; Obj: Pointer;
  Param1: Pointer): Pointer;
begin
  CallPointerMethod := PointerMethod(Method)(Param1, Obj)
end;

                              {$IFNDEF BIT_32}

function CallVoidLocal(Func: Pointer; Frame: FramePointer): Pointer;
assembler;
asm
{$IFDEF Windows}
 MOV     AX,[Frame]
 AND     AL,0FEH
 PUSH    AX
{$ELSE}
 push    [Frame]
{$ENDIF}
 call    dword ptr Func
end;

function CallPointerLocal(Func: Pointer; Frame: FramePointer;
  Param1: Pointer): Pointer; assembler;
asm
 mov     ax, word ptr Param1
 mov     dx, word ptr Param1+2
 push    dx
 push    ax
{$IFDEF Windows}
 MOV     AX,[Frame]
 AND     AL,0FEH
 PUSH    AX
{$ELSE}
 push    [Frame]
{$ENDIF}
 call    dword ptr Func
end;

function CallVoidMethodLocal(Func: Pointer; Frame: FramePointer;
  Obj: Pointer): Pointer; assembler;
asm
{$IFDEF Windows}
 MOV     AX,[Frame]
 AND     AL,0FEH
 PUSH    AX
{$ELSE}
 push    [Frame]
{$ENDIF}
 call    dword ptr Func
end;

function CallPointerMethodLocal(Func: Pointer; Frame: FramePointer;
  Obj: Pointer; Param1: Pointer): Pointer; assembler;
asm
 mov     ax, word ptr Param1
 mov     dx, word ptr Param1+2
 push    dx
 push    ax
{$IFDEF Windows}
 MOV     AX,[Frame]
 AND     AL,0FEH
 PUSH    AX
{$ELSE}
 push    [Frame]
{$ENDIF}
 call    dword ptr Func
end;

function CurrentFramePointer: FramePointer; assembler; asm mov  ax, bp  end;

function PreviousFramePointer: FramePointer; assembler; asm mov  ax, ss:[bp] end;

                                {$ELSE BIT_32}

function CallVoidLocal(Func: pointer; Frame: FramePointer): pointer;
assembler;{$USES NONE}
asm
  push Frame
  call Func
  pop  ebp
end;


function CallPointerLocal(Func: pointer; Frame: FramePointer; Param1: pointer): pointer;
assembler;{$USES NONE}
asm
  push Frame
  push Param1
  call Func
  pop  ebp
end;


function CallVoidMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer): pointer;
assembler;{$USES NONE}
asm
  push Frame       ;{Set frame of func           }
  mov  Frame, esi  ;{Save value of ESI           }
  mov  esi, Obj    ;{Mov to ESI pointer to object}
  call Func        ;{Call method                 }
  mov  esi, Frame  ;{Restore ESI                 }
  pop  ebp         ;{remove frame from stack     }
end;


function CallPointerMethodLocal(Func: pointer; Frame: FramePointer; Obj: pointer; Param1: pointer): pointer;
assembler;{$USES NONE}
asm
  push Frame       ;{Set frame of func           }
  mov  Frame, esi  ;{Save value of ESI           }
  mov  esi, Obj    ;{Mov to ESI pointer to object}
  push Param1      ;{Save parameter              }
  call Func        ;{Call method                 }
  mov  esi, Frame  ;{Restore ESI                 }
  pop  ebp         ;{remove frame from stack     }
end;


function CurrentFramePointer: FramePointer;assembler;{&Frame-}{$Uses none}
asm
    mov eax, ebp
end;


function PreviousFramePointer: FramePointer;assembler;{&Frame-}{$Uses none}
asm
    mov eax, [EBP]
end;
                                {$ENDIF BITS}

end.
