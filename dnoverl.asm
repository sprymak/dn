;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.07/DOS
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

p286n

; ===========================================================================
                extrn FREELIST        :byte:3
                extrn HEAPEND         :byte:3
                extrn HEAPORG         :byte:3
                extrn HEAPPTR         :byte:3
                extrn OVERLAYHALT     :near
                extrn OVRCODELIST     :far
                extrn OVRDEBUGPTR     :byte:3
                extrn OVRDOSHANDLE    :far
                extrn OVRFILEBASE     :byte:3
                extrn OVRFILEMODE     :far
                extrn OVRHEAPEND      :far
                extrn OVRHEAPORG      :far
                extrn OVRHEAPPTR      :far
                extrn OVRHEAPSIZE     :far
                extrn OVRLOADCOUNT    :far
                extrn OVRLOADLIST     :far
                extrn OVRREADBUF      :byte:3
                extrn OVRRESULT       :far
                extrn OVRRETRYSIZE    :far
                extrn OVRTRAPCOUNT    :far
                extrn PREFIXSEG       :far
                extrn PFOPEN          :far
                extrn PFSEEK          :far
                extrn PFREAD          :far
                extrn PFCLOSE         :far

; ===========================================================================
DATA            segment word public ''
DATA            ends

; ===========================================================================
iCall           MACRO Proc
                pushf
                call Proc
                ENDM

; ===========================================================================
CODE            segment byte public ''
                assume cs:CODE
                ;org 10h
                assume es:nothing, ss:nothing, ds:nothing
aPath           db 'PATH='

; ***************************************************************************

;               S u b r o u t i n e
; Attributes: bp-based frame

                public OVRINIT
OVRINIT         proc far

var_A           = word ptr -0Ah
var_8           = word ptr -8
var_6           = word ptr -6
var_4           = word ptr -4
var_2           = byte ptr -2

                push    bp
                mov     bp, sp
                sub     sp, 82h
                cld
                cmp     word ptr ds:OVRCODELIST, 0
                jz      InitFail
                mov     al, byte ptr ds:OVRFILEMODE
                mov     [bp+var_2], al
                call    TryOpenParam
                jnb     OpenOk
                call    OpenSelf
                jnb     OpenOk
                call    OpenInPath
                jnb     OpenOk
                mov     ax, 0FFFEh
                jmp     ExitInit
; ---------------------------------------------------------------------------

CloseAndFail:
                mov     ah, 3Eh
                icall   PFCLOSE         ; DCB - CLOSE A PACKED FILE WITH HANDLE
                                        ; BX = file handle

InitFail:
                mov     ax, 0FFFFh
                jmp     ExitInit
; ---------------------------------------------------------------------------

OpenOk:
                mov     bx, ax
                xor     ax, ax
                xor     dx, dx

loc_4_4F:
                push    ds
                push    dx
                push    ax
                mov     cx, dx
                mov     dx, ax
                mov     ax, 4200h
                iCall   PFSEEK          ; DCB - MOVE PACKED FILE READ/WRITE POINTER (LSEEK)
                                        ; AL = method: offset from beginning of file
                                        ; Currently must be always 0
                jb      loc_4_6D
                lea     dx, [bp+var_A]
                push    ss
                pop     ds
                mov     cx, 8
                mov     ah, 3Fh
                iCall   PFREAD          ; DCB - READ FROM PACKED FILE WITH HANDLE
                                        ; BX = file handle, CX = number of bytes to read
                                        ; DS:DX -> buffer
                jb      loc_4_6D
                cmp     ax, cx

loc_4_6D:
                pop     ax
                pop     dx
                pop     ds
                jb      CloseAndFail
                cmp     [bp+var_A], 5A4Dh
                jnz     loc_4_91
                mov     ax, [bp-6]
                mov     dx, 200h
                mul     dx
                mov     cx, [bp+var_8]
                neg     cx
                and     cx, 1FFh
                sub     ax, cx
                sbb     dx, 0
                jmp     short loc_4_4F
; ---------------------------------------------------------------------------

loc_4_91:
                cmp     [bp+var_A], 4246h
                jnz     CloseAndFail
                cmp     [bp+var_8], 564Fh
                jz      loc_4_AD
                add     ax, [bp+var_6]
                adc     dx, [bp+var_4]
                add     ax, 8
                adc     dx, 0
                jmp     short loc_4_4F
; ---------------------------------------------------------------------------

loc_4_AD:
                mov     word ptr ds:OVRDOSHANDLE, bx
                mov     word ptr ds:OVRFILEBASE, ax
                mov     word ptr ds:OVRFILEBASE+2, dx
                mov     word ptr ds:OVRREADBUF, offset ReadBuf
                mov     word ptr ds:OVRREADBUF+2, cs
                mov     ax, word ptr ds:OVRDEBUGPTR
                or      ax, word ptr ds:OVRDEBUGPTR+2
                jnz     loc_4_D5
                mov     word ptr ds:OVRDEBUGPTR, offset NullDebug
                mov     word ptr ds:OVRDEBUGPTR+2, cs

loc_4_D5:
                push    ds
                mov     dx, offset Int3F_Handler
                push    cs
                pop     ds
                assume ds:CODE
                mov     ax, 253Fh
                int     21h             ; DOS - SET INTERRUPT VECTOR
                                        ; AL = interrupt number
                                        ; DS:DX = new vector to be used for specified interrupt
                pop     ds
                assume ds:nothing
                xor     ax, ax

ExitInit:
                mov     word ptr ds:OVRRESULT, ax
                mov     sp, bp
                pop     bp
                retf    4
OVRINIT         endp


; ***************************************************************************

;               S u b r o u t i n e

TryOpenParam    proc near
                push    ds
                lea     di, [bp-82h]
                push    ss
                pop     es
                call    TryOpen
                pop     ds
                retn
TryOpenParam    endp


; ***************************************************************************

;               S u b r o u t i n e

OpenSelf        proc near
                mov     ah, 30h ; '0'
                int     21h             ; DOS - GET DOS VERSION
                                        ; Return: AL = major version number (00h for DOS 1.x)
                cmp     al, 3
                jb      OpenSelfFail
                push    ds
                mov     ds, word ptr ds:PREFIXSEG
                assume ds:nothing
                mov     ds, ds:2Ch
                assume ds:nothing
                xor     si, si

FindEndNameLoop:
                lodsb
                or      al, al
                jnz     FindEndNameLoop
                lodsb
                or      al, al
                jnz     FindEndNameLoop
                lodsw
                lea     di, [bp-82h]
                push    ss
                pop     es
                mov     bx, di

FindNameLoop:
                lodsb
                stosb
                or      al, al
                jz      loc_4_12C
                cmp     al, '\'
                jnz     FindNameLoop
                mov     bx, di
                jmp     short FindNameLoop
; ---------------------------------------------------------------------------

loc_4_12C:
                mov     di, bx
                call    TryOpen
                pop     ds

OpenSelfFail:
                retn
OpenSelf        endp


; ***************************************************************************

;               S u b r o u t i n e

OpenInPath      proc near
                push    ds
                mov     ds, word ptr ds:PREFIXSEG
                assume ds:nothing
                mov     ds, ds:2Ch
                assume ds:nothing
                xor     si, si

loc_4_13E:
                mov     di, offset aPath
                push    cs
                pop     es
                assume es:CODE
                mov     cx, 5
                repe cmpsb
                jz      loc_4_157
                dec     si

loc_4_14B:
                lodsb
                or      al, al
                jnz     loc_4_14B
                cmp     al, [si]
                jnz     loc_4_13E

loc_4_154:
                pop     ds
                stc
                retn
; ---------------------------------------------------------------------------

loc_4_157:
                cmp     byte ptr [si], 0
                jz      loc_4_154
                lea     di, [bp-82h]
                push    ss
                pop     es
                assume es:nothing
                xor     al, al

loc_4_164:
                mov     ah, al
                lodsb
                or      al, al
                jz      loc_4_172
                cmp     al, 3Bh ; ';'
                jz      loc_4_173
                stosb
                jmp     short loc_4_164
; ---------------------------------------------------------------------------

loc_4_172:
                dec     si

loc_4_173:
                cmp     ah, 3Ah ; ':'
                jz      loc_4_180
                cmp     ah, 5Ch ; '\'
                jz      loc_4_180
                mov     al, 5Ch ; '\'
                stosb

loc_4_180:
                push    ds
                push    si
                call    TryOpen
                pop     si
                pop     ds
                jb      loc_4_157
                pop     ds
                retn
OpenInPath      endp


; ***************************************************************************

;               S u b r o u t i n e

TryOpen         proc near
                lds     si, [bp+6]
                lodsb
                mov     cl, al
                xor     ch, ch
                repe movsb
                xor     al, al
                stosb
                lea     dx, [bp-82h]
                push    ss
                pop     ds
                mov     al, [bp-2]
                mov     ah, 3Dh ; '='
                iCall   PFOPEN          ; DCB - OPEN PACKED DISK FILE WITH HANDLE
                                        ; DS:DX -> ASCIZ filename
                                        ; AL = access mode
                                        ; 0 - read, 1 - write, 2 - read & write
                retn
TryOpen         endp


; ***************************************************************************

;               S u b r o u t i n e

                public OVRSETBUF
OVRSETBUF       proc far
                xor     ax, ax
                cmp     ax, word ptr ds:OVRDOSHANDLE
                jz      FailSetBuf
                cmp     ax, word ptr ds:OVRLOADLIST
                jnz     FailSetBuf
                mov     bx, sp
                les     ax, ss:[bx+4]
                mov     dx, es
                call    Div_AX_16       ; AX=AX div 16
                cmp     ax, word ptr ds:OVRHEAPSIZE
                jb      FailSetBuf
                add     ax, word ptr ds:OVRHEAPORG
                jb      FailSetBuf2
                cmp     ax, word ptr ds:HEAPEND+2
                ja      FailSetBuf2
                mov     word ptr ds:OVRHEAPEND, ax
                mov     word ptr ds:HEAPORG+2, ax
                mov     word ptr ds:HEAPPTR+2, ax
                mov     word ptr ds:FREELIST+2, ax
                xor     ax, ax
                mov     word ptr ds:HEAPPTR, ax
                mov     word ptr ds:FREELIST, ax

ExitSetBuf:
                mov     word ptr ds:OVRRESULT, ax
                retf    4
; ---------------------------------------------------------------------------

FailSetBuf:
                mov     ax, 0FFFFh
                jmp     short ExitSetBuf
; ---------------------------------------------------------------------------

FailSetBuf2:
                mov     ax, 0FFFDh
                jmp     short ExitSetBuf
OVRSETBUF       endp


; ***************************************************************************

;               S u b r o u t i n e

                public OVRGETBUF
OVRGETBUF       proc far
                mov     ax, word ptr ds:OVRHEAPEND
                sub     ax, word ptr ds:OVRHEAPORG
                call    Mul_AX_16       ; AX=AX*16
                retf
OVRGETBUF       endp


; ***************************************************************************

;               S u b r o u t i n e

                public OVRSETRETRY
OVRSETRETRY     proc far
                mov     bx, sp
                les     ax, ss:[bx+4]
                mov     dx, es
                call    Div_AX_16       ; AX=AX div 16
                mov     word ptr ds:OVRRETRYSIZE, ax
                retf    4
OVRSETRETRY     endp


; ***************************************************************************

;               S u b r o u t i n e

                public OVRGETRETRY
OVRGETRETRY     proc far
                mov     ax, word ptr ds:OVRRETRYSIZE
                call    Mul_AX_16       ; AX=AX*16
                retf
OVRGETRETRY     endp

; ---------------------------------------------------------------------------

                public OVRCLEARBUF
OVRCLEARBUF:
                push    bp
                mov     bp, sp
                cmp     word ptr ds:OVRDOSHANDLE, 0
                jz      ClearBufFail
                mov     ax, word ptr ds:OVRHEAPORG
                mov     word ptr ds:OVRHEAPPTR, ax
                mov     ax, word ptr ds:OVRLOADLIST
                jmp     short loc_4_240
; ---------------------------------------------------------------------------

loc_4_22D:
                mov     es, ax
                call    sub_4_46E
                xor     ax, ax
                mov     es:10h, ax
                mov     es:12h, ax
                mov     ax, es:14h

loc_4_240:
                or      ax, ax
                jnz     loc_4_22D
                mov     word ptr ds:OVRLOADLIST, ax

loc_4_247:
                mov     word ptr ds:OVRRESULT, ax
                pop     bp
                retf
; ---------------------------------------------------------------------------

ClearBufFail:
                mov     ax, 0FFFFh
                jmp     short loc_4_247

; ***************************************************************************

;               S u b r o u t i n e
; AX=AX div 16

Div_AX_16       proc near
                mov     cl, 4
                shr     ax, cl
                ror     dx, cl
                and     dx, 0F000h
                or      ax, dx
                retn
Div_AX_16       endp


; ***************************************************************************

;               S u b r o u t i n e
; AX=AX*16

Mul_AX_16       proc near
                mov     cl, 4
                rol     ax, cl
                mov     dx, ax
                and     ax, 0FFF0h
                and     dx, 0Fh
                retn
Mul_AX_16       endp


; ***************************************************************************

;               S u b r o u t i n e

ReadBuf         proc far
                mov     bx, sp
                mov     es, ss:[bx+4]
                push    ds
                mov     bx, word ptr ds:OVRDOSHANDLE
                mov     di, word ptr ds:PREFIXSEG
                add     di, 10h
                mov     dx, es:4
                mov     cx, es:6
                add     dx, word ptr ds:OVRFILEBASE
                adc     cx, word ptr ds:OVRFILEBASE+2
                mov     ax, 4200h
                iCall   PFSEEK          ; DCB - MOVE PACKED FILE READ/WRITE POINTER (LSEEK)
                                        ; AL = method: offset from beginning of file
                                        ; Currently must be always 0
                mov     ds, es:10h
                xor     dx, dx
                mov     cx, es:8
                mov     ah, 3Fh ; '?'
                iCall   PFREAD          ; DCB - READ FROM PACKED FILE WITH HANDLE
                                        ; BX = file handle, CX = number of bytes to read
                                        ; DS:DX -> buffer
                jb      loc_4_2E4
                cmp     ax, cx
                jb      loc_4_2E1
                add     ax, 0Fh
                mov     cl, 4
                shr     ax, cl
                add     ax, es:10h
                mov     ds, ax
                xor     dx, dx
                mov     cx, es:0Ah
                jcxz    loc_4_2DD
                mov     ah, 3Fh ; '?'
                iCall   PFREAD          ; DCB - READ FROM PACKED FILE WITH HANDLE
                                        ; BX = file handle, CX = number of bytes to read
                                        ; DS:DX -> buffer
                jb      loc_4_2E4
                cmp     ax, cx
                jb      loc_4_2E1
                shr     cx, 1
                xor     si, si
                mov     es, es:10h
                cld

loc_4_2D5:
                lodsw
                mov     bx, ax
                add     es:[bx], di
                loop    loc_4_2D5

loc_4_2DD:
                xor     ax, ax
                jmp     short loc_4_2E4
; ---------------------------------------------------------------------------

loc_4_2E1:
                mov     ax, 64h ; 'd'

loc_4_2E4:
                pop     ds
                retf    2
ReadBuf         endp


; ***************************************************************************

;               S u b r o u t i n e

NullDebug       proc far
                retf
NullDebug       endp


; ***************************************************************************

;               S u b r o u t i n e
; Attributes: bp-based frame

Int3F_Handler   proc far

var_6           = word ptr -6

                push    bp
                mov     bp, sp
                push    ax
                push    bx
                push    cx
                push    dx
                push    si
                push    di
                push    ds
                push    es
                mov     ax, seg DATA
                mov     ds, ax
                assume ds:DATA
                sti
                les     bx, [bp+2]
                push    word ptr es:[bx]
                sub     word ptr [bp+2], 2
                jnz     loc_4_30B
                call    sub_4_33B
                jmp     short loc_4_323
; ---------------------------------------------------------------------------

loc_4_30B:
                add     bp, 6
                mov     ax, [bp+0]
                xchg    ax, [bp+var_6]
                mov     [bp+0], ax
                call    sub_4_33B
                mov     ax, [bp+0]
                xchg    ax, [bp+var_6]
                mov     [bp+0], ax

loc_4_323:
                pop     bx
                mov     ax, es:12h
                cmp     ax, 1
                sbb     ax, ax
                call    dword ptr ds:OVRDEBUGPTR
                pop     es
                pop     ds
                assume ds:nothing
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                pop     bp
                iret
Int3F_Handler   endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_33B       proc near
                inc     word ptr ds:OVRTRAPCOUNT
                cmp     word ptr es:10h, 0
                jz      loc_4_350
                mov     word ptr es:12h, 1
                jmp     short loc_4_3B9
; ---------------------------------------------------------------------------

loc_4_350:
                inc     word ptr ds:OVRLOADCOUNT
                push    es
                call    sub_4_556
                mov     dx, es:0Ah
                add     dx, 0Fh
                mov     cl, 4
                shr     dx, cl
                add     dx, ax
                call    sub_4_53B
                jmp     short loc_4_39F
; ---------------------------------------------------------------------------

loc_4_36B:
                push    dx
                call    sub_4_3EC
                mov     es, word ptr ds:OVRLOADLIST
                mov     ax, es:14h
                mov     word ptr ds:OVRLOADLIST, ax
                cmp     word ptr es:12h, 0
                jnz     loc_4_391
                call    sub_4_46E
                mov     word ptr es:10h, 0
                call    sub_4_556
                jmp     short loc_4_39E
; ---------------------------------------------------------------------------

loc_4_391:
                dec     word ptr es:12h
                call    sub_4_4A1
                call    sub_4_4E9
                xor     ax, ax

loc_4_39E:
                pop     dx

loc_4_39F:
                sub     dx, ax
                ja      loc_4_36B
                pop     es
                mov     ax, word ptr ds:OVRHEAPPTR
                mov     es:10h, ax
                push    es
                push    es
                call    dword ptr ds:OVRREADBUF
                pop     es
                or      ax, ax
                jnz     loc_4_3E9
                call    sub_4_4E9

loc_4_3B9:
                call    sub_4_438
                push    es
                call    sub_4_53B
                mov     es, word ptr ds:OVRLOADLIST

loc_4_3C4:
                mov     cx, es:14h
                jcxz    loc_4_3E7
                cmp     ax, word ptr ds:OVRRETRYSIZE
                jnb     loc_4_3E7
                push    cx
                push    ax
                cmp     word ptr es:12h, 0
                jnz     loc_4_3DE
                call    sub_4_46E

loc_4_3DE:
                call    sub_4_556
                pop     cx
                pop     es
                add     ax, cx
                jmp     short loc_4_3C4
; ---------------------------------------------------------------------------

loc_4_3E7:
                pop     es
                retn
; ---------------------------------------------------------------------------

loc_4_3E9:
                jmp     OVERLAYHALT
sub_4_33B       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_3EC       proc near
                mov     ax, word ptr ds:OVRLOADLIST
                or      ax, ax
                jz      loc_4_431
                mov     es, ax
                mov     dx, es:10h
                cmp     dx, word ptr ds:OVRHEAPPTR
                jnb     locret_4_437
                xor     cx, cx

loc_4_402:
                inc     cx
                push    ax
                mov     es, ax
                mov     ax, es:14h
                or      ax, ax
                jnz     loc_4_402
                mov     word ptr ds:OVRLOADLIST, ax
                mov     ax, word ptr ds:OVRHEAPEND
                mov     word ptr ds:OVRHEAPPTR, ax

loc_4_417:
                pop     es
                push    cx
                mov     ax, word ptr ds:OVRLOADLIST
                mov     es:14h, ax
                mov     word ptr ds:OVRLOADLIST, es
                call    sub_4_556
                sub     word ptr ds:OVRHEAPPTR, ax
                call    sub_4_4A1
                pop     cx
                loop    loc_4_417

loc_4_431:
                mov     ax, word ptr ds:OVRHEAPORG
                mov     word ptr ds:OVRHEAPPTR, ax

locret_4_437:
                retn
sub_4_3EC       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_438       proc near
                cmp     byte ptr es:20h, 0EAh
                jz      locret_4_46D
                mov     cx, es:2
                jcxz    loc_4_450
                mov     ax, es:10h
                mov     dx, es
                call    sub_4_509

loc_4_450:
                mov     bx, es:10h
                mov     cx, es:0Ch
                mov     di, 20h ; ' '
                cld

loc_4_45E:
                mov     dx, es:[di+2]
                mov     al, 0EAh
                stosb
                mov     ax, dx
                stosw
                mov     ax, bx
                stosw
                loop    loc_4_45E

locret_4_46D:
                retn
sub_4_438       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_46E       proc near
                cmp     byte ptr es:20h, 0CDh
                jz      locret_4_4A0
                mov     ax, es
                mov     dx, es:10h
                xor     cx, cx
                call    sub_4_509
                mov     es:2, cx
                mov     cx, es:0Ch
                mov     di, 20h ; ' '
                cld

loc_4_490:
                mov     dx, es:[di+1]
                mov     ax, 3FCDh
                stosw
                mov     ax, dx
                stosw
                xor     al, al
                stosb
                loop    loc_4_490

locret_4_4A0:
                retn
sub_4_46E       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_4A1       proc near
                mov     ax, word ptr ds:OVRHEAPPTR
                mov     dx, es:10h
                mov     es:10h, ax
                mov     cx, es:8
                inc     cx
                shr     cx, 1
                xor     si, si
                cld
                cmp     ax, dx
                jb      loc_4_4C2
                mov     si, cx
                dec     si
                shl     si, 1
                std

loc_4_4C2:
                mov     di, si
                push    ds
                push    es
                mov     ds, dx
                mov     es, ax
                repe movsw
                pop     es
                pop     ds
                cmp     byte ptr es:20h, 0CDh
                jz      locret_4_4E8
                call    sub_4_515
                mov     cx, es:0Ch
                mov     di, 23h ; '#'
                cld

loc_4_4E2:
                stosw
                add     di, 3
                loop    loc_4_4E2

locret_4_4E8:
                retn
sub_4_4A1       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_4E9       proc near
                call    sub_4_556
                add     word ptr ds:OVRHEAPPTR, ax
                push    ds
                mov     bx, offset OVRLOADLIST

loc_4_4F4:
                mov     ax, [bx]
                or      ax, ax
                jz      loc_4_501
                mov     ds, ax
                mov     bx, 14h
                jmp     short loc_4_4F4
; ---------------------------------------------------------------------------

loc_4_501:
                mov     [bx], es
                mov     es:14h, ax
                pop     ds
                retn
sub_4_4E9       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_509       proc near
                call    sub_4_515
                or      bx, bx
                jz      locret_4_514
                xchg    cx, ss:[bx+2]

locret_4_514:
                retn
sub_4_509       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_515       proc near
                xor     bx, bx
                push    bp
                push    cx
                jmp     short loc_4_521
; ---------------------------------------------------------------------------

loc_4_51B:
                shl     cx, 1
                jz      loc_4_538
                mov     bp, cx

loc_4_521:
                mov     cx, [bp+0]
                shr     cx, 1
                jb      loc_4_51B
                cmp     dx, [bp+4]
                jnz     loc_4_51B
                mov     [bp+4], ax
                or      bx, bx
                jnz     loc_4_51B
                mov     bx, bp
                jmp     short loc_4_51B
; ---------------------------------------------------------------------------

loc_4_538:
                pop     cx
                pop     bp
                retn
sub_4_515       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_53B       proc near
                mov     ax, word ptr ds:OVRLOADLIST
                or      ax, ax
                jz      loc_4_54E
                mov     es, ax
                mov     ax, es:10h
                sub     ax, word ptr ds:OVRHEAPPTR
                jnb     locret_4_555

loc_4_54E:
                mov     ax, word ptr ds:OVRHEAPEND
                sub     ax, word ptr ds:OVRHEAPPTR

locret_4_555:
                retn
sub_4_53B       endp


; ***************************************************************************

;               S u b r o u t i n e

sub_4_556       proc near
                mov     ax, es:8
                add     ax, 0Fh
                mov     cl, 4
                shr     ax, cl
                retn
sub_4_556       endp

CODE            ends


                end
