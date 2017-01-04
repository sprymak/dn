;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.09
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

; reconstruction of BP70 RTL module
; (an original was taken from CN distribution)

        .model  tpascal
        include se.ash

        .data
        extrn OvrCodeList:  Word
        extrn OvrHeapOrg:   Word
        extrn OvrDosHandle: Word
        extrn OvrEmsHandle: Word
        extrn ExitProc:     DWord
        extrn PrefixSeg:    Word
        extrn OvrResult:    Word
        extrn OvrReadBuf:   DWord
        extrn OvrEmsPages:  Word

EmsSeg   dw ?
SaveExit dd ?

        .code
        extrn pfClose: far

EmsName db 'EMMXXXX0'

iCall   MACRO aProc
         pushf
         ; call aProc
         db 9Ah
         dd aProc
        ENDM

        public OvrInitEms
OvrInitEms proc far
        xor ax,ax
        cmp ax,OvrDosHandle
        jne @@1
        dec ax
        jmp short @@5
@@1:    call CheckEms
        jz @@2
        mov ax,-5     ; no EMM driver found
        jmp short @@5
@@2:    call AllocEms
        jnc @@3
        mov ax,-6
        jmp short @@5
@@3:    call SwitchToEms
        jnc @@4
        mov dx,OvrEmsHandle
        mov ah,45h      ; release handle and memory
        int 67h
        mov ax,-4
        jmp short @@5
@@4:    mov bx,OvrDosHandle
        mov ah,DosClose
        ; int dos
        iCall pfClose
        mov word ptr OvrReadBuf[0],offset OvrReadEms
        mov word ptr OvrReadBuf[2],cs
        les ax,ExitProc
        mov word ptr SaveExit[0],ax
        mov word ptr SaveExit[2],es
        mov word ptr ExitProc[0],offset NewExit
        mov word ptr ExitProc[2],cs
        xor ax,ax
@@5:    mov OvrResult,ax
        ret
OvrInitEms endp

NewExit proc far
        mov dx,OvrEmsHandle
        mov ah,45h
        int 67h
        les ax,SaveExit
        mov word ptr ExitProc[0],ax
        mov word ptr ExitProc[2],es
        ret
NewExit endp

CheckEms proc near
        mov ax,DosGetInt*256+67h
        int dos
        mov cx,8
        mov si,offset EmsName
        mov di,0ah
        push ds cs
        pop ds          ; DS:SI to EmsName
        cld             ; ES:[0Ah] to int67h handler
        repe cmpsb
        pop ds
        ret
CheckEms endp

AllocEms proc near
        mov ah,41h      ; get page frame segment
        int 67h
        mov EmsSeg,bx
        mov ax,16383
        xor dx,dx
        mov bx,OvrCodeList
@@1:    add bx,PrefixSeg
        add bx,10h
        mov es,bx
        add ax,es:OvCodeSize ; ES:8
        adc dx,0
        mov bx,es:OvLink ; ES:14
        or bx,bx
        jnz @@1
        mov bx,16384
        div bx
        mov bx,ax
        mov ah,43h      ; get handle and allocate memory
        int 67h
        shl ah,1
        jc @@2
        mov OvrEmsPages,bx
        mov OvrEmsHandle,dx
@@2:    ret
AllocEms endp

SwitchToEms proc near
        push bp
        mov bp,sp
        mov dx,OvrEmsHandle
        mov ah,47h      ; save old mapping context and switch to OvrEmsHandle
        int 67h
        mov ax,OvrCodeList
        xor cx,cx
@@1:    add ax,PrefixSeg
        add ax,10h
        mov es,ax
        push ax
        inc cx
        mov ax,es:OvLink
        or ax,ax
        jnz @@1
        xor bx,bx
        xor di,di
@@2:    pop es
        push cx
        mov ax,OvrHeapOrg
        mov es:OvSegment,ax
        mov es:OvEmsPage,bx
        mov es:OvEmsOffset,di
        push bx di es es
        call OvrReadBuf
        pop es di bx
        mov es:OvSegment,0
        neg ax
        jc @@3
        call PutToEms
        jc @@3
        pop cx
        loop @@2
@@3:    pushf
        mov dx,OvrEmsHandle
        mov ah,48h      ; restore mapping context
        int 67h
        popf
        mov sp,bp
        pop bp
        ret
SwitchToEms endp

PutToEms proc near
        mov dx,es:OvCodeSize
        xor si,si
@@1:    or di,di
        jnz @@2
        push dx
        mov dx,OvrEmsHandle
        mov ax,4400h    ; map memory
                        ; AL = physical page number (0-3)
                        ; BX = logical page number
        int 67h
        pop dx
        shl ah,1
        jc @@5
@@2:    mov cx,16384
        sub cx,di
        cmp cx,dx
        jb @@3
        mov cx,dx
@@3:    sub dx,cx
        push ds es
        mov es,EmsSeg
        mov ds,OvrHeapOrg
        cld
        rep movsb
        pop es ds
        cmp di,16384
        jne @@4
        inc bx
        xor di,di
@@4:    or dx,dx
        jnz @@1
@@5:    ret
PutToEms endp

OvrReadEms  proc far
        mov bx,sp
        mov es,ss:[bx+4]
        mov dx,OvrEmsHandle
        mov ah,47h
        int 67h      ; save mapping context
        mov bx,es:OvEmsPage
        mov dx,es:OvCodeSize
        mov si,es:OvEmsOffset
        xor di,di
@@1:    mov cx,16384
        sub cx,si
        cmp cx,dx
        jb @@2
        mov cx,dx
@@2:    sub dx,cx
        push dx
        mov dx,OvrEmsHandle
        mov ax,4400h
        int 67h   ; map memory
        pop dx
        or ah,ah
        jnz @@3
        push ds es
        mov ds,EmsSeg
        mov es,es:OvSegment
        cld
        rep movsb
        pop es ds
        inc bx
        xor si,si
        or dx,dx
        jnz @@1
@@3:    mov al,ah
        xor ah,ah
        push ax
        mov dx,OvrEmsHandle
        mov ah,48h ; restore mapping context
        int 67h
        pop ax
        ret 2
OvrReadEms endp
        end
