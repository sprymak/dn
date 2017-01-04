;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.11
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
        extrn OvrDebugPtr:   DWord
        extrn OvrFileMode:   Byte
        extrn OvrFileBase:   DWord
        extrn OvrDosHandle:  Word
        extrn OvrResult:     Word
        extrn OvrReadBuf:    DWord
        extrn OvrRetrySize:  Word
        extrn OvrTrapCount:  Word
        extrn OvrLoadCount:  Word
        extrn OvrCodeList:   Word
        extrn OvrLoadList:   Word
        extrn OvrHeapSize:   Word
        extrn OvrHeapOrg:    Word
        extrn OvrHeapPtr:    Word
        extrn OvrHeapEnd:    Word
        extrn HeapOrg:       DWord
        extrn HeapPtr:       DWord
        extrn HeapEnd:       DWord
        extrn FreeList:      DWord
        extrn PrefixSeg:     Word

        .code
        extrn OverlayHalt: near
        extrn pfOpen:      far
        extrn pfSeek:      far
        extrn pfRead:      far
        extrn pfClose:     far

        public OvrGetBuf
        public OvrGetRetry
        public OvrSetRetry
        public OvrSetBuf
        public OvrInit
        public OvrClearBuf
Path    db 'PATH='

iCall   MACRO aProc
         pushf
         ; call aProc
         db 9Ah
         dd aProc
        ENDM

OvrInit proc far FileName:DWord
        local String:byte:120, Buf:Word:4, Mode:Byte
        cld
        cmp OvrCodeList,0
        je @@2
        mov al,OvrFileMode
        mov Mode,al
        call OpenCurDir
        jnc @@3
        call OpenExeDir
        jnc @@3
        call OpenPath
        jnc @@3
        mov ax,-2
        jmp @@9
@@1:    mov ah,DosClose
        ; int dos
        iCall pfClose
@@2:    mov ax,-1
        jmp @@9
@@3:    mov bx,ax
        xor ax,ax
        xor dx,dx
@@4:    push ds dx ax
        mov cx,dx
        mov dx,ax
        mov ax,DosSeek*256
        ; int dos
        iCall pfSeek
        jc @@5
        lea dx,Buf
        push ss
        pop ds
        mov cx,8
        mov ah,DosRead
        ; int dos
        iCall pfRead
        jc @@5
        cmp ax,cx
@@5:    pop ax dx ds
        jc @@1
        cmp Buf[0],'ZM'
        jne @@6
        mov ax,Buf[4]
        mov dx,512
        mul dx
        mov cx,Buf[2]
        neg cx
        and cx,1ffh
        sub ax,cx
        sbb dx,0
        jmp @@4
@@6:    cmp buf[0],'BF'
        jne @@1
        cmp buf[2],'VO'
        je @@7
        add ax,Buf[4]
        adc dx,Buf[6]
        add ax,8
        adc dx,0
        jmp @@4
@@7:    mov OvrDosHandle,bx
        mov word ptr OvrFileBase[0],ax
        mov word ptr OvrFileBase[2],dx
        mov word ptr OvrReadBuf[0],offset OvrRead
        mov word ptr OvrReadBuf[2],cs
        mov ax,word ptr OvrDebuGptr[0]
        or  ax,word ptr OvrDebugPtr[2]
        jnz @@8
        mov word ptr OvrDebugPtr[0],offset OvrDebug
        mov word ptr OvrDebugPtr[2],cs
@@8:    push ds
        mov dx,offset int3f
        push cs
        pop ds
        mov ax,DosSetInt*256+3fh
        int dos
        pop ds
        xor ax,ax
@@9:    mov OvrResult,ax
        ret

 OpenCurDir proc near
        push ds
        lea di,String
        push ss
        pop es
        call OpenFile
        pop ds
        ret
 OpenCurDir endp

 OpenExeDir proc near
        mov ah,DosGetVersion
        int dos
        cmp al,3
        jb @@4
        push ds
        mov ds,PrefixSeg
        mov ds,ds:PspEnvSeg
        xor si,si
@@1:    lodsb
        or al,al
        jnz @@1
        lodsb
        or al,al
        jnz @@1
        lodsw
        lea di,String
        push ss
        pop es
        mov bx,di
@@2:    lodsb
        stosb
        or al,al
        jz @@3
        cmp al,'\'
        jne @@2
        mov bx,di
        jmp @@2
@@3:    mov di,bx
        call OpenFile
        pop ds
@@4:    ret
 OpenExeDir endp

 OpenPath proc near
        push ds
        mov ds,PrefixSeg
        mov ds,ds:PspEnvSeg
        xor si,si
@@1:    mov di,offset Path
        push cs
        pop es
        mov cx,5
        repe cmpsb
        je @@4
        dec si
@@2:    lodsb
        or al,al
        jnz @@2
        cmp al,[si]
        jne @@1
@@3:    pop ds
        stc
        ret
@@4:    cmp byte ptr [si],0
        je @@3
        lea di,String
        push ss
        pop es
        xor al,al
@@5:    mov ah,al
        lodsb
        or al,al
        jz @@6
        cmp al,';'
        je @@7
        stosb
        jmp @@5
@@6:    dec si
@@7:    cmp ah,':'
        je @@8
        cmp ah,'\'
        je @@8
        mov al,'\'
        stosb
@@8:    push ds si
        call OpenFile
        pop si ds
        jc @@4
        pop ds
        ret
 OpenPath endp

 OpenFile proc near
        lds si,FileName
        lodsb
        mov cl,al
        xor ch,ch
        rep movsb
        xor al,al
        stosb
        lea dx,String
        push ss
        pop ds
        mov al,Mode
        mov ah,DosOpen
        ; int dos
        iCall pfOpen
        ret
 OpenFile endp
OvrInit endp

OvrSetBuf proc far
        xor ax,ax
        ;cmp ax,OvrDosHandle
        ;je  @@2
        cmp ax,OvrLoadList
        jne @@2
        mov bx,sp
        les ax,ss:[bx+4]
        mov dx,es
        call div16
        cmp ax,OvrHeapSize
        jb @@2
        add ax,OvrHeapOrg
        jc @@3
        cmp ax,word ptr HeapEnd[2]
        ja @@3
        mov OvrHeapEnd,ax
        mov word ptr HeapOrg[2],ax
        mov word ptr HeapPtr[2],ax
        mov word ptr FreeList[2],ax
        xor ax,ax
        mov word ptr HeapPtr[0],ax
        mov word ptr FreeList[0],ax
@@1:    mov OvrResult,ax
        ret 4
@@2:    mov ax,-1
        jmp @@1
@@3:    mov ax,-3
        jmp @@1
OvrSetBuf endp

OvrGetBuf proc far
        mov ax,OvrHeapEnd
        sub ax,OvrHeapOrg
        call mul16
        ret
OvrGetBuf endp

OvrSetRetry proc far
        mov bx,sp
        les ax,ss:[bx+4]
        mov dx,es
        call div16
        mov OvrRetrySize,ax
        ret 4
OvrSetRetry endp

OvrGetRetry proc far
        mov ax,ovrretrysize
        call mul16
        ret
OvrGetRetry endp

OvrClearBuf proc far
        push bp
        mov bp,sp
        cmp OvrDosHandle,0
        je @@4
        mov ax,OvrHeapOrg
        mov OvrHeapPtr,ax
        mov ax,OvrLoadList
        jmp short @@2
@@1:    mov es,ax
        call GenInts
        xor ax,ax
        mov es:OvSegment,ax
        mov es:OvRetryCount,ax
        mov ax,es:OvNext
@@2:    or ax,ax
        jnz @@1
        mov OvrLoadList,ax
@@3:    mov OvrResult,ax
        pop bp
        ret
@@4:    mov ax,-1
        jmp @@3
OvrClearBuf endp

div16   proc near
        mov cl,4
        shr ax,cl
        ror dx,cl
        and dx,0f000h
        or ax,dx
        ret
div16   endp

mul16   proc near
        mov cl,4
        rol ax,cl
        mov dx,ax
        and ax,0fff0h
        and dx,0fh
        ret
mul16   endp

OvrRead proc far
        mov bx,sp
        mov es,ss:[bx+4]
        push ds
        mov bx,OvrDosHandle
        mov di,PrefixSeg
        add di,10h
        mov dx,es:OvFilePos[0]
        mov cx,es:OvFilePos[2]
        add dx,word ptr OvrFileBase[0]
        adc cx,word ptr OvrFileBase[2]
        mov ax,DosSeek*256
        ; int dos
        iCall pfSeek
        mov ds,es:OvSegment
        xor dx,dx
        mov cx,es:OvCodeSize
        mov ah,DosRead
        ; int dos
        iCall pfRead
        jc @@4
        cmp ax,cx
        jb @@3
        add ax,15
        mov cl,4
        shr ax,cl
        add ax,es:OvSegment
        mov ds,ax
        xor dx,dx
        mov cx,es:OvFixupSize
        jcxz @@2
        mov ah,DosRead
        ; int dos
        iCall pfRead
        jc @@4
        cmp ax,cx
        jb @@3
        shr cx,1
        xor si,si
        mov es,es:OvSegment
        cld
@@1:    lodsw
        mov bx,ax
        add es:[bx],di
        loop @@1
@@2:    xor ax,ax
        jmp short @@4
@@3:    mov ax,100
@@4:    pop ds
        ret 2
OvrRead endp

OvrDebug proc far
        ret
OvrDebug endp

int3f   proc far
        push bp
        mov bp,sp
        push ax bx cx dx si di ds es
        mov ax,seg data
        mov ds,ax
        sti
        les bx,[bp+2]
        push es:[bx]
        sub word ptr [bp+2],2
        jnz @@1
        call ovrtrap
        jmp short @@2
@@1:    add bp,6
        mov ax,[bp]
        xchg ax,[bp-6]
        mov [bp],ax
        call ovrtrap
        mov ax,[bp]
        xchg ax,[bp-6]
        mov [bp],ax
@@2:    pop bx
        mov ax,es:ovretrycount
        cmp ax,1
        sbb ax,ax
        call ovrdebugptr
        pop es ds di si dx cx bx ax bp
        iret
int3f   endp

OvrTrap proc near
        inc OvrTrapCount
        cmp es:OvSegment,0
        je @@1
        mov es:OvRetryCount,1
        jmp short @@6
@@1:    inc OvrLoadCount
        push es
        call OvrSize
        mov dx,es:OvFixupSize
        add dx,15
        mov cl,4
        shr dx,cl
        add dx,ax
        call OvrOffset
        jmp short @@5
@@2:    push dx
        call OvrMove
        mov es,OvrLoadList
        mov ax,es:OvNext
        mov OvrLoadList,ax
        cmp es:OvRetryCount,0
        jne @@3
        call GenInts
        mov es:OvSegment,0
        call OvrSize
        jmp short @@4
@@3:    dec es:OvRetryCount
        call MoveSeg
        call InsertSeg
        xor ax,ax
@@4:    pop dx
@@5:    sub dx,ax
        ja @@2
        pop es
        mov ax,OvrHeapPtr
        mov es:OvSegment,ax
        push es es
        call OvrReadBuf
        pop es
        or ax,ax
        jnz @@10
        call InsertSeg
@@6:    call GenJumps
        push es
        call OvrOffset
        mov es,OvrLoadList
@@7:    mov cx,es:OvNext
        jcxz @@9
        cmp ax,OvrRetrySize
        jae @@9
        push cx ax
        cmp es:OvRetryCount,0
        jne @@8
        call GenInts
@@8:    call OvrSize
        pop cx es
        add ax,cx
        jmp @@7
@@9:    pop es
        ret
@@10:   jmp OverlayHalt
OvrTrap endp

OvrMove proc near
        mov ax,OvrLoadList
        or ax,ax
        jz @@3
        mov es,ax
        mov dx,es:OvSegment
        cmp dx,OvrHeapPtr
        jae @@4
        xor cx,cx
@@1:    inc cx
        push ax
        mov es,ax
        mov ax,es:OvNext
        or ax,ax
        jnz @@1
        mov OvrLoadList,ax
        mov ax,OvrHeapEnd
        mov OvrHeapPtr,ax
@@2:    pop es
        push cx
        mov ax,OvrLoadList
        mov es:OvNext,ax
        mov OvrLoadList,es
        call OvrSize
        sub OvrHeapPtr,ax
        call MoveSeg
        pop cx
        loop @@2
@@3:    mov ax,OvrHeapOrg
        mov OvrHeapPtr,ax
@@4:    ret
OvrMove endp

GenJumps proc near
        cmp es:OvVectors,0eah
        je @@3
        mov cx,es:OvSaveReturn
        jcxz @@1
        mov ax,es:OvSegment
        mov dx,es
        call FixStack
@@1:    mov bx,es:OvSegment
        mov cx,es:OvJumpCount
        mov di,offset OvVectors
        cld
@@2:    mov dx,es:[di+2]
        mov al,0eah
        stosb
        mov ax,dx
        stosw
        mov ax,bx
        stosw
        loop @@2
@@3:    ret
GenJumps endp

GenInts proc near
        cmp es:OvVectors,0cdh
        je @@2
        mov ax,es
        mov dx,es:OvSegment
        xor cx,cx
        call FixStack
        mov es:OvSaveReturn,cx
        mov cx,es:OvJumpCount
        mov di,offset OvVectors
        cld
@@1:    mov dx,es:[di+1]
        mov ax,3fcdh
        stosw
        mov ax,dx
        stosw
        xor al,al
        stosb
        loop @@1
@@2:    ret
GenInts endp

MoveSeg proc near
        mov ax,OvrHeapPtr
        mov dx,es:OvSegment
        mov es:OvSegment,ax
        mov cx,es:OvCodeSize
        inc cx
        shr cx,1
        xor si,si
        cld
        cmp ax,dx
        jb @@1
        mov si,cx
        dec si
        shl si,1
        std
@@1:    mov di,si
        push ds es
        mov ds,dx
        mov es,ax
        rep movsw
        pop es ds
        cmp es:OvVectors,0cdh
        je @@3
        call UpdateStack
        mov cx,es:OvJumpCount
        mov di,offset OvVectors+3
        cld
@@2:    stosw
        add di,3
        loop @@2
@@3:    ret
MoveSeg endp

InsertSeg proc near
        call OvrSize
        add OvrHeapPtr,ax
        push ds
        mov bx,offset OvrLoadList
@@1:    mov ax,[bx]
        or ax,ax
        jz @@2
        mov ds,ax
        mov bx,offset OvNext
        jmp @@1
@@2:    mov [bx],es
        mov es:OvNext,ax
        pop ds
        ret
InsertSeg endp

FixStack proc near
        call UpdateStack
        or bx,bx
        jz @@1
        xchg cx,ss:[bx+2]
@@1:    ret
FixStack endp

UpdateStack proc near
        xor bx,bx
        push bp cx
        jmp short @@2
@@1:    shl cx,1
        jz @@3
        mov bp,cx
@@2:    mov cx,[bp]
        shr cx,1
        jc @@1
        cmp dx,[bp+4]
        jne @@1
        mov [bp+4],ax
        or bx,bx
        jnz @@1
        mov bx,bp
        jmp @@1
@@3:    pop cx bp
        ret
UpdateStack endp

OvrOffset proc near
        mov ax,OvrLoadList
        or ax,ax
        jz @@1
        mov es,ax
        mov ax,es:OvSegment
        sub ax,OvrHeapPtr
        jae @@2
@@1:    mov ax,OvrHeapEnd
        sub ax,OvrHeapPtr
@@2:    ret
OvrOffset endp

OvrSize proc near
        mov ax,es:OvCodeSize
        add ax,15
        mov cl,4
        shr ax,cl
        ret
OvrSize endp
        end
