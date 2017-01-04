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

; ---------------------------------------------------------------------------
; P5SPEED.ASM  TMi0SDGL R2 CPU Speed Detection Routine           Version 2.13
;
; Too-Much-in-0ne-So-Don't-Get-Lost(tm) Revision 2 CPU/FPU Detection Library
; Copyright(c) 1996-99 by B-coolWare. Written by Bobby Z.
; -----------------------------------------------------------------------------
;
; This module contains P5-specific clock speed detection routine using
; Time Stamp Counter (TSC) and RDTSC instruction. It will only work on
; processors that support TSC. Results under NT may be inconsistent because
; this OS handles hardware timer in a different way than Win95. Also, OS/2
; DOS Box session without HW_TIMER set to ON may be affected.

; This code is mostly borrowed from Intel's CPUINFO freeware package and
; rewritten for assembler (and optimized for 32-bit, because this code will
; only be invoked on at least 32-bit CPU). Some division adjustments from
; original CPUINFO were omitted.

        INCLUDE HEADER.ASH

        PUBLIC  getPentiumSpeed
        PUBLIC  TSCDisabled

SAMPLING_DELAY  equ  3500        ; how many timer ticks to run the test.
_Loops          equ  8           ; how much times to sample speed.

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

; --- grabTimer reads current clock value

grabTime     proc near           ; read current hardware timer value
        mov  al,10111000b        ; Channel 2, LSB+MSB, mode 4,
        out  43h,al              ; binary
        in   al,61h
        or   al,1
        out  61h,al

        in   al,42h
        mov  ah,al
        in   al,42h
        xchg al,ah
        neg  ax

        ret
        endp

        .386

getPentiumSpeed proc DIST
USES  ebx, ecx, edx, esi
IFNDEF  __32bit__
        call  WinStartCritical
ENDIF
        call  sampleSpeed       ; sample _Loops times and get an average
        mov   esi,eax
        REPT  (_Loops - 2)
        call  sampleSpeed
        add   esi,eax
        ENDM
        call  sampleSpeed
        add   eax,esi
IFNDEF  __32bit__
        call  WinEndCritical
ENDIF
        clr   edx
        mov   ebx,_Loops
        div   ebx

        ret
        endp

sampleSpeed   proc near
LOCAL   hiTSC : DWORD
@@10:
        call  grabTime
        cmp   ax,(0FFFFh-SAMPLING_DELAY) ; ensure we're not near overflow
        jae   @@10                       ; resample timer if so
        mov   bx,ax
        _rdtsc          ; get current TSC
        mov   ecx,eax
        mov   hiTSC,edx
@@1:                    ; allow enough ticks to pass
        call  grabTime
        sub   ax,bx
        cmp   ax,SAMPLING_DELAY
        jb    @@1
        mov   bx,ax     ; save ticks elapsed
        _rdtsc
        sub   edx,hiTSC ; high dword of cycles passed
        mov   hiTSC,edx ; save them
        sub   eax,ecx
        sbb   hiTSC,0
        mov   ecx,eax

        clr   eax
        mov   ax,bx
        mov   ebx,100000  ; convert ticks to microseconds
        mul   ebx
        mov   ebx,119318
        div   ebx
        cmp   edx,119318/2; adjust after division
        jb    @@2
        inc   eax
@@2:
        mov   ebx,eax     ; ebx = microseconds passed
        mov   eax,ecx     ; eax = low part of cycles passed
        mov   edx,hiTSC   ; edx:eax = cycles passed
        div   ebx         ; eax = cycles/ticks = clock speed in MHz

        inc   eax         ; adjust after division - always
        ret
        endp

TSCDisabled   proc DIST
        mov   al, 4
        _getcr4
        and   eax,4       ; mask out TSD bit
        shr   eax,2       ; and shift it to position 0
        ret

        endp

        END
