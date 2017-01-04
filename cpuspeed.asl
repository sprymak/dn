;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source
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
;/
;/  Version history:
;/
;/  1.6.RC1
;/
;//////////////////////////////////////////////////////////////////////////

; -----------------------------------------------------------------------------
; CPUSPEED.ASM  CPU speed measurement routine for TMi0SDGL 2      Version 2.13
;
; Too-Much-In-One-So-Don't-Get-Lost(tm) Revision 2 CPU/FPU Detection Library
; Copyright(c) 1996-99 by B-coolWare.  Written by Bobby Z.
; -----------------------------------------------------------------------------
; This file contains Speed routine that plays part in CPU clock frequency
; calculation. Algorithm was taken from Norton SysInfo as well as several
; constants.

        INCLUDE HEADER.ASH

        PUBLIC  Speed

; --- Windows detection and critical section handling

IFNDEF  __32bit__

isUnderWin      proc near
        mov     ax,1600h
        int     2Fh
        or      al,al
        jz      @@nowin
        cmp     al,80h
        jz      @@nowin
        stc
        ret
@@nowin:
        clc
        ret
        endp

winStartCritical        proc near
        push    ax
        call    IsUnderWin
        jnc     @@Q
        mov     ax,1681h
        int     2Fh
@@Q:
        pop     ax
        ret
        endp

winEndCritical          proc near
        push    ax
        call    IsUnderWin
        jnc     @@Q
        mov     ax,1682h
        int     2Fh
@@Q:
        pop     ax
        ret
        endp

ENDIF

; ------ Speed routine implementation

Speed   PROC  DIST
IFNDEF  __32bit__
LOCAL   Stage : Word            ; local variable to differ test stages
ENDIF

IFDEF   __32bit__
USES    ebx, ecx, edx, esi, edi
ELSE
USES    si, di
ENDIF
        mov     cx,2
IFNDEF  __32bit__
        mov     Stage,0
        call    winStartCritical; enter Windows critical section
                                ; IBM OS/2 doesn't provide a critical section
                                ; mechanism, so you should set HW_TIMER = ON
                                ; in DOS Box settings to get correct result.
ENDIF
@@1:
        mov     speedShift,cx   ; initialize speedShift
IFNDEF  __32bit__
        push    Stage
ELSE
        push    0
ENDIF
        call    speedTest       ; do a code section 101 times
        cmp     ax,1000h
        jnb     @@2
        mov     cx,speedShift
IFDEF   __32bit__
        shl     cx,3
ELSE
        shl     cx,1            ; repeat test with increasing values until
        shl     cx,1            ; we get reasonable time value worth working
        shl     cx,1            ; with
ENDIF
        jmp     @@1
@@2:
        push    ax
        mov     cx,speedShift   ; now do the same code section 100 times less
IFNDEF  __32bit__
        mov     Stage,1
        push    Stage
ELSE
        push    1
ENDIF
        call    speedTest
IFNDEF  __32bit__
        call    winEndCritical  ; exit Windows critical section
ENDIF
        pop     dx
        sub     dx,ax           ; now we got time how long 100*shift instructions
        xchg    ax,dx           ; were performed - knowing number of ticks it
        ret                     ; takes for each CPU type we can compute the
        ENDP                    ; CPU clock frequency.

speedTest       PROC    NEAR
ARG     Stage : Word

        clr     dx
        mov     si,0AAAAh
        mov     bx,05555h
        in      al,61h          ; initialize hardware timer
        jmp     $+2
        and     al,0FCh
        out     61h,al
        jmp     $+2
        mov     al,0B4h
        out     43h,al
        jmp     $+2
        clr     al
        out     42h,al
        jmp     $+2
        out     42h,al
        jmp     $+2
        in      al,61h
        mov     di,ax
        or      al,01
        cmp     Stage,0
        jnz     @@2
@@1:
        cli
        out     61h,al
@@3:
        REPT    101
        mov     ax,si
        div     bx
        ENDM
        dec     cx
        jz      @@4
        jmp     @@3
@@2:
        cli
        out     61h,al
@@5:
        mov     ax,si
        div     bx
        dec     cx
        jz      @@4
        jmp     @@5

@@4:
        mov     ax,di           ; shut down timer and get results
        out     61h,al
        jmp     $+2
        sti
        in      al,42h
        jmp     $+2
        xchg    ah,al
        in      al,42h
        jmp     $+2
        xchg    ah,al
        neg     ax
        push    ax
        in      al,61h
        jmp     $+2
        and     al,0FDh
        out     61h,al
; fix the waveform back to default (suggested by Pascal Moulart)
        mov     al,0B6h
        out     43h,al
        jmp     $+2
        clr     al
        out     42h,al
        jmp     $+2
        out     42h,al
        pop     ax
        ret
        ENDP

        END
