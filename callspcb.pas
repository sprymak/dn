{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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
//////////////////////////////////////////////////////////////////////////}
{$I Stdefine.inc}
(*
   This unit provides compiler-independent mechanisms to call special
   functions, i.e. local functions/procedures, constructors, methods,
   destructors, etc. As there are no procedural variables for these
   special functions, there is no Pascal way to call them directly.
*)

unit CallSpcb;

interface

type
  FramePointer = Pointer;

function CurrentFramePointer: FramePointer;
function PreviousFramePointer: FramePointer;

function CallVoidConstructor(Ctor: Pointer; Obj: Pointer; VMT:
    Pointer):
Pointer;
function CallPointerConstructor(Ctor: Pointer; Obj: Pointer; VMT:
    Pointer;
  Param1: Pointer): Pointer;

function CallVoidMethod(Method: Pointer; Obj: Pointer): Pointer;
function CallPointerMethod(Method: Pointer; Obj: Pointer; Param1:
    Pointer):
Pointer;

function CallVoidLocal(func: Pointer; Frame: FramePointer): Pointer;
function CallPointerLocal(func: Pointer; Frame: FramePointer;
  Param1: Pointer): Pointer;

function CallVoidMethodLocal(func: Pointer; Frame: FramePointer;
  Obj: Pointer): Pointer;
function CallPointerMethodLocal(func: Pointer; Frame: FramePointer;
  Obj: Pointer; Param1: Pointer): Pointer;

implementation

type
  VoidConstructor = function (VmtOfs: word; Obj: Pointer): Pointer;
  PointerConstructor = function (Param1: Pointer; VmtOfs: word;
  Obj: Pointer): Pointer;
  VoidMethod = function (Obj: Pointer): Pointer;
  PointerMethod = function (Param1: Pointer; Obj: Pointer): Pointer;

function CallVoidConstructor(Ctor: Pointer; Obj: Pointer;
  VMT: Pointer): Pointer;
  begin
    CallVoidConstructor := VoidConstructor(Ctor)(Ofs(VMT^), Obj)
  end;

function CallPointerConstructor(Ctor: Pointer; Obj: Pointer;
  VMT: Pointer; Param1: Pointer): Pointer;
  begin
    CallPointerConstructor := PointerConstructor(Ctor)(Param1, Ofs(
      VMT^), Obj)
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

function CallVoidLocal(func: Pointer; Frame: FramePointer): Pointer;
  assembler; {$USES NONE}
asm
  push Frame
  call Func
  pop  ebp
end;

function CallPointerLocal(func: Pointer; Frame: FramePointer; Param1:
    Pointer): Pointer;
  assembler; {$USES NONE}
asm
  push Frame
  push Param1
  call Func
  pop  ebp
end;

function CallVoidMethodLocal(func: Pointer; Frame: FramePointer; Obj:
    Pointer): Pointer;
  assembler; {$USES NONE}
asm
  push Frame       ;{Set frame of func           }
  mov  Frame, esi  ;{Save value of ESI           }
  mov  esi, Obj    ;{Mov to ESI pointer to object}
  call Func        ;{Call method                 }
  mov  esi, Frame  ;{Restore ESI                 }
  pop  ebp         ;{remove frame from stack     }
end
  ;

function CallPointerMethodLocal(func: Pointer; Frame: FramePointer;
    Obj: Pointer; Param1: Pointer): Pointer;
  assembler; {$USES NONE}
asm
  push Frame       ;{Set frame of func           }
  mov  Frame, esi  ;{Save value of ESI           }
  mov  esi, Obj    ;{Mov to ESI pointer to object}
  push Param1      ;{Save parameter              }
  call Func        ;{Call method                 }
  mov  esi, Frame  ;{Restore ESI                 }
  pop  ebp         ;{remove frame from stack     }
end
  ;

function CurrentFramePointer: FramePointer; assembler; {&Frame-}
    {$Uses none}
asm
    mov eax, ebp
end;

function PreviousFramePointer: FramePointer; assembler; {&Frame-}
    {$Uses none}
asm
    mov eax, [EBP]
end;

end.
