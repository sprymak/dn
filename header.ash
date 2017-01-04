;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.12
;/  Based on Dos Navigator (C) 1991-99 RIT Research Labs
;/
;/  This programs is free for commercial and non-commercial use as long as
;/  the following conditions are aheared to.
;/
;/  Copyright remains RIT Research Labs, and as such any Copyright notices
;/  in the code are not to be removed. If this package is used in a
;/  product, RIT Research Labs should be given attribution as the RIT Research
;/  Labs of the parts of the library used. This can be in the form of a textual
;/  message at program startup or in documentation (online or textual)
;/  provided with the package.
;/
;/  Redistribution and use in source and binary forms, with or without
;/  modification, are permitted provided that the following conditions are
;/  met:
;/
;/  1. Redistributions of source code must retain the copyright
;/     notice, this list of conditions and the following disclaimer.
;/  2. Redistributions in binary form must reproduce the above copyright
;/     notice, this list of conditions and the following disclaimer in the
;/     documentation and/or other materials provided with the distribution.
;/  3. All advertising materials mentioning features or use of this software
;/     must display the following acknowledgement:
;/     "Based on Dos Navigator by RIT Research Labs."
;/
;/  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
;/  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;/  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;/  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
;/  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;/  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;/  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;/  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
;/  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;/  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;/  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;/
;/  The licence and distribution terms for any publically available
;/  version or derivative of this code cannot be changed. i.e. this code
;/  cannot simply be copied and put under another distribution licence
;/  (including the GNU Public Licence).
;/
;//////////////////////////////////////////////////////////////////////////

; -----------------------------------------------------------------------------
; HEADER.ASH - TMi0SDGL Revision 2 Assembler Header               Version 2.13
;
; Too-Much-in-0ne-So-Don't-Get-Lost(tm) Revision 2 CPU/FPU Detection Library
; Copyright(c) 1996-99 by B-coolWare. Written by Bobby Z.
; -----------------------------------------------------------------------------
; This file is INCLUDEd in all assembler modules of the library.
;
; defines that affect compilation:
;
; __32bit__     type: define
;               default: not defined
;               purpose: to compile code and data as USE32 segments as well
;                        as use 32-bit version of DPMICODE.ASM.
; should not be defined with Borland Pascal 7 - it does not support 32-bit
; protected mode program generation.
;
; __Windows__   type: define
;               default: not defined
;               purpose: to force TASM to add Windows entry/exit code to
;                        public routines (for use in DLL).

IF ??version LT 300h
 .out This code requires Turbo Assembler Version 3.x or later
 .err
ENDIF

IFDEF   __32bit__
        .386
IFDEF   __Windows__
        MODEL   FLAT, WINDOWS STDCALL
ELSE
        MODEL   FLAT, PASCAL
ENDIF
        DIST    equ   <>        ; use default distance
ELSE

        ; set memory model and calling convention

IFNDEF  __Windows__

IFNDEF __HUGE__
 IFNDEF __LARGE__
  IFNDEF __MEDIUM__
   IFNDEF __COMPACT__
    IFNDEF __SMALL__
     IFNDEF __TINY__
        %out Must define a memory model
        .err
     ELSE
      MODEL TINY, PASCAL
      DIST  EQU  <NEAR>
     ENDIF
    ELSE
     MODEL SMALL, PASCAL
     DIST  EQU   <NEAR>
    ENDIF
   ELSE
    MODEL COMPACT, PASCAL
    DIST   EQU   <NEAR>
   ENDIF
  ELSE
   MODEL MEDIUM, PASCAL
   DIST    EQU   <FAR>
  ENDIF
 ELSE
  MODEL LARGE, PASCAL
  DIST     EQU   <FAR>
 ENDIF
ELSE
 MODEL HUGE, PASCAL
 DIST      EQU   <FAR>
ENDIF

ELSE

IFNDEF __HUGE__
 IFNDEF __LARGE__
  IFNDEF __MEDIUM__
   IFNDEF __COMPACT__
    IFNDEF __SMALL__
     IFNDEF __TINY__
        %out Must define a memory model
        .err
     ELSE
      MODEL TINY, WINDOWS PASCAL
      DIST  EQU  <NEAR>
     ENDIF
    ELSE
     MODEL SMALL, WINDOWS PASCAL
     DIST  EQU   <NEAR>
    ENDIF
   ELSE
    MODEL COMPACT, WINDOWS PASCAL
    DIST   EQU   <NEAR>
   ENDIF
  ELSE
   MODEL MEDIUM, WINDOWS PASCAL
   DIST    EQU   <FAR>
  ENDIF
 ELSE
  MODEL LARGE, WINDOWS PASCAL
  DIST     EQU   <FAR>
 ENDIF
ELSE
 MODEL HUGE, WINDOWS PASCAL
 DIST      EQU   <FAR>
ENDIF

ENDIF   ; __Windows__

ENDIF   ; __32bit__

INCLUDE TMI0SDGL.INC            ; Include TMi0SDL-specific defines

        LOCALS  @@              ; enable local symbols
        JUMPS                   ; enable jump optimization
