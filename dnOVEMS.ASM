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
                extrn EXITPROC      :byte:3
                extrn OVRCODELIST   :far
                extrn OVRDOSHANDLE  :far
                extrn OVREMSHANDLE  :far
                extrn OVREMSPAGES   :far
                extrn OVRHEAPORG    :far
                extrn OVRREADBUF    :byte:3
                extrn OVRRESULT     :far
                extrn PFCLOSE       :far
                extrn PREFIXSEG     :far

; ===========================================================================
DATA            segment word public ''
                assume cs:DATA
word_1_0        dw ?
OldExitProc     db 4 dup(?)
DATA            ends

; ===========================================================================
iCall           MACRO Proc
                pushf
                call Proc
                ENDM

; ===========================================================================
CODE            segment byte public ''
                assume cs:CODE
                assume es:nothing, ss:nothing, ds:nothing
EMM_Device      db 'EMMXXXX0'

; ***************************************************************************

;               S u b r o u t i n e

public          OVRINITEMS
OVRINITEMS      proc far
                xor     ax, ax
                cmp     ax, word ptr ds:OVRDOSHANDLE
                jnz     FileOpened
                dec     ax
                jmp     short ExitEmsInit
; ---------------------------------------------------------------------------

FileOpened:
                call    CheckEMM
                jz      EMM_Present
                mov     ax, 0FFFBh
                jmp     short ExitEmsInit
; ---------------------------------------------------------------------------

EMM_Present:
                call    InitEMM
                jnb     EMM_InitOk
                mov     ax, 0FFFAh
                jmp     short ExitEmsInit
; ---------------------------------------------------------------------------

EMM_InitOk:
                call    EMM_SaveContent
                jnb     EMMInitOk
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 45h
                int     67h             ;  - LIM EMS - RELEASE HANDLE AND MEMORY
                                        ; DX = EMM handle
                                        ; Return: AH = status
                mov     ax, 0FFFCh
                jmp     short ExitEmsInit
; ---------------------------------------------------------------------------

EMMInitOk:
                mov     bx, word ptr ds:OVRDOSHANDLE
                mov     ah, 3Eh
                iCall   PFCLOSE         ; DCB - CLOSE A PACKED FILE WITH HANDLE
                                        ; BX = file handle
                mov     word ptr ds:OVRREADBUF, offset EmmReadBuf
                mov     word ptr ds:OVRREADBUF+2, cs
                les     ax, dword ptr ds:EXITPROC
                mov     word ptr ds:OldExitProc, ax
                mov     word ptr ds:OldExitProc+2, es
                mov     word ptr ds:EXITPROC, offset NewExitProc
                mov     word ptr ds:EXITPROC+2, cs
                xor     ax, ax

ExitEmsInit:
                mov     word ptr ds:OVRRESULT, ax
                retf
OVRINITEMS      endp


; ***************************************************************************

;               S u b r o u t i n e

NewExitProc     proc far
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 45h
                int     67h             ;  - LIM EMS - RELEASE HANDLE AND MEMORY
                                        ; DX = EMM handle
                                        ; Return: AH = status
                les     ax, dword ptr ds:OldExitProc
                mov     word ptr ds:EXITPROC, ax
                mov     word ptr ds:EXITPROC+2, es
                retf
NewExitProc     endp


; ***************************************************************************

;               S u b r o u t i n e

CheckEMM        proc near
                mov     ax, 3567h
                int     21h             ; DOS - 2+ - GET INTERRUPT VECTOR
                                        ; AL = interrupt number
                                        ; Return: ES:BX = value of interrupt vector
                mov     cx, 8
                mov     si, offset EMM_Device
                mov     di, 0Ah
                push    ds
                push    cs
                pop     ds
                assume ds:CODE
                cld
                repe cmpsb
                pop     ds
                assume ds:nothing
                retn
CheckEMM        endp


; ***************************************************************************

;               S u b r o u t i n e

InitEMM         proc near
                mov     ah, 41h
                int     67h             ;  - LIM EMS - GET PAGE FRAME SEGMENT
                                        ; Return: AH = 00h function successful, BX = segment of page frame
                                        ; AH = error code (see AH=40h)
                shl     ah, 1
                jb      locret_2_D6
                mov     ds:word_1_0, bx
                mov     ax, 3FFFh
                xor     dx, dx
                mov     bx, word ptr ds:OVRCODELIST

loc_2_A5:
                add     bx, word ptr ds:PREFIXSEG
                add     bx, 10h
                mov     es, bx
                assume es:nothing
                add     ax, es:8
                adc     dx, 0
                mov     bx, es:0Eh
                or      bx, bx
                jnz     loc_2_A5
                mov     bx, 4000h
                div     bx
                mov     bx, ax
                mov     ah, 43h
                int     67h             ;  - LIM EMS - GET HANDLE AND ALLOCATE MEMORY
                                        ; BX = number of logical pages to allocate
                                        ; Return: AH = status
                shl     ah, 1
                jb      locret_2_D6
                mov     word ptr ds:OVREMSPAGES, bx
                mov     word ptr ds:OVREMSHANDLE, dx

locret_2_D6:
                retn
InitEMM         endp


; ***************************************************************************

;               S u b r o u t i n e
; Attributes: bp-based frame

EMM_SaveContent proc near
                push    bp
                mov     bp, sp
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 47h
                int     67h             ;  - LIM EMS - SAVE MAPPING CONTEXT
                                        ; DX = handle
                                        ; Return: AH = status
                mov     ax, word ptr ds:OVRCODELIST
                xor     cx, cx

loc_2_E7:
                add     ax, word ptr ds:PREFIXSEG
                add     ax, 10h
                mov     es, ax
                push    ax
                inc     cx
                mov     ax, es:0Eh
                or      ax, ax
                jnz     loc_2_E7
                xor     bx, bx
                xor     di, di

loc_2_FE:
                pop     es
                assume es:nothing
                push    cx
                mov     ax, word ptr ds:OVRHEAPORG
                mov     es:10h, ax
                mov     es:16h, bx
                mov     es:18h, di
                push    bx
                push    di
                push    es
                push    es
                call    dword ptr ds:OVRREADBUF
                pop     es
                pop     di
                pop     bx
                mov     word ptr es:10h, 0
                neg     ax
                jb      loc_2_12F
                call    sub_2_13D
                jb      loc_2_12F
                pop     cx
                loop    loc_2_FE

loc_2_12F:
                pushf
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 48h
                int     67h             ;  - LIM EMS - RESTORE MAPPING CONTEXT
                                        ; DX = handle
                                        ; Return: AH = status
                popf
                mov     sp, bp
                pop     bp
                retn
EMM_SaveContent endp


; ***************************************************************************

;               S u b r o u t i n e

sub_2_13D       proc near
                mov     dx, es:8
                xor     si, si

loc_2_144:
                or      di, di
                jnz     loc_2_157
                push    dx
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ax, 4400h
                int     67h             ;  - LIM EMS - MAP MEMORY
                                        ; AL = physical page number (0-3)
                                        ; BX = logical page number, DX = handle
                                        ; Return: AH = status
                pop     dx
                shl     ah, 1
                jb      locret_2_180

loc_2_157:
                mov     cx, 4000h
                sub     cx, di
                cmp     cx, dx
                jb      loc_2_162
                mov     cx, dx

loc_2_162:
                sub     dx, cx
                push    ds
                push    es
                mov     es, ds:word_1_0
                mov     ds, word ptr ds:OVRHEAPORG
                assume ds:nothing
                cld
                repe movsb
                pop     es
                pop     ds
                assume ds:nothing
                cmp     di, 4000h
                jnz     loc_2_17C
                inc     bx
                xor     di, di

loc_2_17C:
                or      dx, dx
                jnz     loc_2_144

locret_2_180:
                retn
sub_2_13D       endp


; ***************************************************************************

;               S u b r o u t i n e

EmmReadBuf      proc far
                mov     bx, sp
                mov     es, ss:[bx+4]
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 47h
                int     67h             ;  - LIM EMS - SAVE MAPPING CONTEXT
                                        ; DX = handle
                                        ; Return: AH = status
                mov     bx, es:16h
                mov     dx, es:8
                mov     si, es:18h
                xor     di, di

loc_2_1A0:
                mov     cx, 4000h
                sub     cx, si
                cmp     cx, dx
                jb      loc_2_1AB
                mov     cx, dx

loc_2_1AB:
                sub     dx, cx
                push    dx
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ax, 4400h
                int     67h             ;  - LIM EMS - MAP MEMORY
                                        ; AL = physical page number (0-3)
                                        ; BX = logical page number, DX = handle
                                        ; Return: AH = status
                pop     dx
                or      ah, ah
                jnz     loc_2_1D3
                push    ds
                push    es
                mov     ds, ds:word_1_0
                mov     es, es:10h
                cld
                repe movsb
                pop     es
                pop     ds
                inc     bx
                xor     si, si
                or      dx, dx
                jnz     loc_2_1A0

loc_2_1D3:
                mov     al, ah
                xor     ah, ah
                push    ax
                mov     dx, word ptr ds:OVREMSHANDLE
                mov     ah, 48h
                int     67h             ;  - LIM EMS - RESTORE MAPPING CONTEXT
                                        ; DX = handle
                                        ; Return: AH = status
                pop     ax
                retf    2
EmmReadBuf      endp

CODE            ends


                end
