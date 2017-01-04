;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.6.RC1
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
; CACHETST.ASM Processor Cache Size Test Low-level Routines       Version 2.13
;
; Part of TMi0SDGL(tm) Revision 2 CPU/FPU Detection Library
; Copyright(c) 1996-99 by B-coolWare. Written by Bobby Z.
; -----------------------------------------------------------------------------
; Idea derived from Norbert Juffa's COMPTEST - PC Hardware Test program,
; public domain version 2.60.
;
; getCacheSize result is valid only on 486s and higher CPUs, because 386s
; usually got no cache on chip at all. On 386s it will detect external cache
; size if it's below 64K or will return 64K size.
; known problem: this method doesn't return correct result under multitasking
; environments like Windows or DESQview. The reason is unknown.

        INCLUDE HEADER.ASH

        .CODE

IFNDEF  __32bit__
        .386
ENDIF

        PUBLIC  getCacheSize    ; exported function

StartTimer      macro           ; start hardware timer 2 at max rate
        in      al,61h
        and     al,0FCh
        out     61h,al
        mov     al,0B4h
        out     43h,al
        clr     al,al
        out     42h,al
        jmp     $+2
        out     42h,al
        in      al,61h
        or      al,01h
        out     61h,al
        endm

StopTimer       macro           ; stop timer 2 and get clock count
        in      al,42h
        mov     bl,al
        in      al,42h
        mov     bh,al
        neg     bx
        in      al,61h
        and     al,0FDh
        out     61h,al
        endm


TestMemThroughput       proc    near
; writes the same memory block twice, measuring time elapsed for second block
; write. If the whole block fits internal cache, second time write is performed
; very fast. If it doesn't - we get significant speed dropdown (more than 0.5
; times less thruput than previous block).

        push    es
        push    ds
        pop     es
IFDEF   __32bit__
        mov     esi, offset cpu ; memory location we're sure to have write
        mov     edi, esi        ; access to
ELSE
        clr     esi
        clr     edi
ENDIF
        mov     ecx,edx
        rep     movsd
        clr     esi
        clr     edi
        mov     ecx,edx
        StartTimer
        rep     movsd
        StopTimer
        pop     es
        ret
        endp

TestCache   proc near
; performs memory thruput test with blocks of various sizes: 1K, 2K, 4K, 8K,
; 16K, 32K and 64K and saves timings in array.
        pushf
        cli
        cld
        mov     edx,256
        call    TestMemThroughput
        mov     [Time1K],bx
        mov     edx,512
        call    TestMemThroughput
        mov     [Time2K],bx
        mov     edx,1024
        call    TestMemThroughput
        mov     [Time4K],bx
        mov     edx,2048
        call    TestMemThroughput
        mov     [Time8K],bx
        mov     edx,4096
        call    TestMemThroughput
        mov     [Time16K],bx
        mov     edx,8192
        call    TestMemThroughput
        mov     [Time32K],bx
        mov     edx,16384
        call    TestMemThroughput
        mov     [Time64K],bx
        popf
        ret
        endp

getCacheSize    proc DIST
; Pascal:
;        function getCacheSize : Word; far; external;
; C/C++:
;        extern _dist word getCacheSize(void);
; Assembler:
;        EXTRN  getCacheSize : DIST
;
; returns first level data cache size in kilobytes.

IFDEF   __32bit__       ; preserve ESI and EBX registers
 USES   esi, ebx
ENDIF

        call    TestCache
        clr     bx
        inc     bx
IFDEF   __32bit__
        mov     esi, offset Time2K
ELSE
        mov     si, offset Time2K
ENDIF
@@1:
        shl     bx,1            ; cache size in kilobytes
IFDEF   __32bit__
        mov     ax,_wp [esi]
        mov     dx,_wp [esi-2]
ELSE
        mov     ax,_wp [si]
        mov     dx,_wp [si-2]
ENDIF
        shl     dx,1
        sub     ax,dx           ; compare timings
        jb      @@next
        shr     dx,1
        shr     dx,1
        sub     ax,dx
        test    ax,8000h        ; negative value?
        jz      @@found         ; if no then previous block size fits in cache
@@next:
IFDEF   __32bit__
        add     esi,2
        cmp     esi,offset Time64K
ELSE
        add     si,2
        cmp     si, offset Time64K
ENDIF
        jbe     @@1
@@found:
        mov     ax,bx           ; bx holds cache size in kilobytes.
        shr     ax,1            ; cache size before dropdown
        ret
        endp

        .DATA

Time1K      DW      0
Time2K      DW      0
Time4K      DW      0
Time8K      DW      0
Time16K     DW      0
Time32K     DW      0
Time64K     DW      0


        END
