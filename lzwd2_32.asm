;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.10
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

;
; Lempel-Ziv Welch Decompression routine
;
; Copyright (C) 1994 by Roman Cramar
;
; 32-bit corrections by DataCompBoy, 17.07.2000
;
                .386p
                ;jumps


OutByte         macro
                mov     byte ptr [ebp],al
                inc     ebp
                endm

DATA            segment 'DATA'

extrn InputOffs  : dword
extrn Temp_offs  : dword
extrn OutPutOffs2: dword

extrn le6A       : word
extrn le6C       : word
extrn le6E       : word
extrn le70       : word
extrn le72       : word
extrn le74       : word
extrn le78       : word
extrn le7A_0     : word
extrn le7A_2     : word
extrn le7A_4     : word
extrn le7A_6     : word
extrn le82a      : word
extrn le82b      : word

extrn le76       : byte
extrn le77       : byte

DATA            ends

CODE            segment
                assume  cs:CODE, ds:DATA

                public  LZWDecompr

;Decompress - decompresses data compressed by Lempel-Ziv Welch method.
;        EBX - Pointer to temporary buffer (17K min.).
;        EDX - Pointer to compressed data.
;        EDI - Pointer to buffer to decompress.
; Exit: AX - size of decompressed data.

LZWDecompr      proc
                push    ebp

                mov     Temp_offs,  ebx
                mov     InputOffs,  edx
                mov     ebp,        edi
                mov     OutPutOffs2,edi

                call    LZWDecomp

                mov     eax,ebp
                sub     eax,OutPutOffs2
                pop     ebp

                ret
LZWDecompr      endp

LZWDecomp       proc    near
                push    esi
                push    edi
                mov     le72,0
                mov     le78,9
                mov     le70,102h
                mov     le74,200h
                xor     ax,ax
                mov     le6a,ax
                mov     le6c,ax
                mov     le6e,ax
                mov     le76,al
                mov     le77,al
                mov     le82a,ax
                mov     le82b,ax
                mov     le7a_0,1FFh
                mov     le7a_2,3FFh
                mov     le7a_4,7FFh
                mov     le7a_6,0FFFh
le58b:          call    GetNextCode
                cmp     ax,101h
                jnz     le596
                jmp     le63b
le596:          cmp     ax,100h
                jnz     le5b5
                call    InitTable
                call    GetNextCode
                mov     le6a,ax
                mov     le6c,ax
                mov     le77,al
                mov     le76,al
                mov     al,le77
                OutByte
                jmp     le58b
le5b5:          mov     le6a,ax
                mov     le6e,ax
                cmp     ax,le70
                jb      t2
                mov     ax,le6c
                mov     le6a,ax
                mov     al,le76
                push    ax
                inc     le72
t2:             cmp     le6a,0ffh
                jbe     le5f6
                lea     esi,dword ptr temp_offs
                xor     ebx,ebx
                mov     bx,le6a
                shl     bx,1
                add     bx,le6a
                mov     al,[esi+ebx+2]
                push    ax
                inc     le72
                mov     ax,[esi+ebx]
                mov     le6a,ax
                jmp     t2
le5f6:          mov     ax,le6a
                mov     le76,al
                mov     le77,al
                push    ax
                inc     le72
                xor     ecx,ecx
                mov     cx,le72
                jcxz    le610
t1:             pop     ax
                OutByte
                loop    t1
le610:          mov     le72,0
                call    AddInTable
                mov     ax,le6e
                mov     le6c,ax
                mov     bx,le70
                cmp     bx,le74
                jl      le638
                cmp     byte ptr le78,0ch
                jz      le638
                inc     byte ptr le78
                shl     le74,1
le638:          jmp     le58b
le63b:          pop     edi
                pop     esi
                ret

InitTable       proc    near
                mov     byte ptr le78,9
                mov     le74,200h
                mov     le70,102h
                ret
InitTable       endp

GetNextCode     proc    near
                xor     ecx,ecx
                mov     bx,le82a
                mov     ax,le82b
                add     bx,le78
                adc     ax,0
                xchg    bx,le82a
                xchg    ax,Le82b
                mov     cx,bx
                and     cx,7     ;!!!!!
                shr     ax,1
                rcr     bx,1
                shr     ax,1
                rcr     bx,1
                shr     ax,1
                rcr     bx,1
                les     si,dword ptr InputOffs
                mov     ax,es:[si+bx]
                mov     dl,es:[si+bx+2]
                or      cx,cx
                jz      GetCode2
GetCode1:       shr     dl,1
                rcr     ax,1
                loop    GetCode1
GetCode2:       mov     bx,le78
                sub     bx,9
                shl     bx,1
                and     ax,[di+bx+le7a_0]
                ret
GetNextCode     endp

AddInTable      proc    near
                push    esi
                xor     ebx,ebx
                mov     bx,le70
                shl     bx,1
                add     bx,le70
                lea     esi,dword ptr temp_offs
                mov     al,le77
                mov     [esi+ebx+2],al
                mov     ax,le6c
                mov     [esi+ebx],ax
                inc     le70
                pop     esi
                ret
AddInTable      endp

LZWDecomp       endp

CODE            ends
                end
