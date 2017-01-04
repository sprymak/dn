;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.08
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
; Lempel-Ziv Welch Compression routine
;
; Copyright (C) 1994 by Roman Cramar
;
; 32-bit corrections by DataCompBoy, 17.07.2000
; MODE "IDEAL" MUST DIE!!!
;

DATA            segment         'DATA'
extrn MaxCode        : word
extrn RunCode        : word
extrn RunningBits    : word
extrn CodeBuffer     : dword
extrn CurCode        : word
extrn OldCode        : word
extrn CurBuffShift   : word
extrn InputOffs      : dword
extrn OutPutOffs     : dword
extrn Temp           : word
extrn DataSize       : dword
extrn DataSize2      : dword
extrn OutPutOffs2    : dword
extrn CodeTable      : dword
extrn PrefixTable    : dword
extrn SymbolTable    : dword
DATA            ends



CODE            segment
                assume  cs:CODE, ds:DATA

                public  LZWCompr

                .386p

_GetByte        macro
                dec     DataSize
                jz      LZWComprEnd
                mov     esi, InputOffs
                mov     al, [esi]
                mov     ah, 0
                inc     InputOffs
                endm

_ComprCode      macro
local           Rotate2,NoRotate2,CheckFull,NoIncBits
                xor     ebx,ebx
                xor     ecx,ecx
                mov     cx, CurBuffShift
                jcxz    NoRotate2
Rotate2:        shl     ax,1
                rcl     bx,1
                loop    Rotate2
NoRotate2:      or      word ptr CodeBuffer+0,ax
                or      word ptr CodeBuffer+2,bx
                mov     ax, RunningBits
                add     CurBuffShift,ax
CheckFull:      cmp     CurBuffShift,8
                jc      NoIncBits
                lea     edi,dword ptr OutputOffs
                mov     ax,word ptr CodeBuffer
                mov     [edi],al
                inc     OutPutOffs

                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1
                 shr     word ptr CodeBuffer+2,1
                 rcr     word ptr CodeBuffer+0,1

                sub     CurBuffShift,8
                jmp     CheckFull
NoIncBits:
                endm

_InitCodeTable  macro
                lea     edi,CodeTable
                mov     ecx,LZMax
                xor     eax,eax
                cld
                rep stosd
                endm

_ResetCodeBuffer macro
local Again

Again:
                lea     edi,dword ptr OutPutOffs
                mov     ax,word ptr CodeBuffer
                mov     [edi],al
                inc     OutPutOffs

                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1
                 shr     word ptr CodeBuffer+2, 1
                 rcr     word ptr CodeBuffer+0, 1

                sub     CurBuffShift,8
                jnc     Again
ResetBuffEnd:   mov     CurBuffShift,0
                endm

;Constants
ClearCode       equ     100h
EOICode         equ     101h
LZMax           equ     4096

;Pointers
cCodeTable       equ     20h
cPrefixTable     equ     2020h
cSymbolTable     equ     4020h

;Compress - compresses data using Lempel-Ziv Welch method.
;        EBX - Pointer to temporary buffer (21K min.)
;        EDX - Pointer to uncompressed data.
;        EDI - Pointer to buffer to compress.
; Exit:  EAX - size of compressed data.

LZWCompr        proc

                push    ebp

                mov     InputOffs  ,edx
                mov     OutPutOffs ,edi
                mov     OutPutOffs2,edi

                mov     eax, ebx
                add     eax, cCodeTable
                mov     CodeTable, eax

                mov     eax, ebx
                add     eax, cPrefixTable
                mov     PrefixTable, eax

                mov     eax, ebx
                add     eax, cSymbolTable
                mov     SymbolTable, eax

                inc     DataSize

                call    LZWCompress
                mov     eax,OutPutOffs
                sub     eax,OutPutOffs2
                pop     ebp
                ret

LZWCompr        endp

LZWCompress     proc near
                xor     eax,eax
                xor     ebx,ebx
                xor     esi,esi
               _InitCodeTable
                mov     RunCode,102h
                mov     RunningBits,9
                mov     MaxCode,200h
                mov     CurBuffShift,ax
                mov     CurCode,ax
                mov     word ptr CodeBuffer+0,ax
                mov     word ptr CodeBuffer+2,ax
                mov     ax,ClearCode
               _ComprCode
               _GetByte
                mov     [OldCode],ax
NextByte:
                mov     edi,OutPutOffs
                sub     edi,OutPutOffs2
                cmp     edi,DataSize2
                jge     PackFail

               _GetByte
                mov     CurCode,ax
                mov     cl,5
                shl     ax,cl
                xor     ax,OldCode
                and     ax,0FFFh
                mov     esi,eax
                mov     [Temp],1
SearchLoop:     mov     ebx,esi
                shl     bx,1

                lea     ebp, CodeTable
                cmp     word ptr [ebp+ebx],0

                jnz     IsInTable
                mov     ax,OldCode
               _ComprCode
                mov     ebx,esi
                shl     bx,1
                mov     ax,RunCode
                mov     Temp,ax
                cmp     ax,LZMax
                jnc     CheckOverflow

                lea     ebp, [CodeTable]
                mov     [ebp+ebx],ax

                mov     ax,OldCode

                lea     ebp,[PrefixTable]
                mov     [ebp+ebx],ax

                mov     al,byte ptr CurCode

                lea     ebp, [SymbolTable]
                mov     [ebp+esi],al

                inc     RunCode
CheckOverflow:  mov     ax,Temp
                cmp     ax,MaxCode
                jnz     ChangeOldCode
                cmp     byte ptr RunningBits,12
                jnc     SendClearCode
                inc     byte ptr RunningBits
                shl     MaxCode,1
                jmp     ChangeOldCode
SendClearCode:  mov     ax,ClearCode
               _ComprCode
                mov     RunCode,102h
                mov     byte ptr RunningBits,9
                mov     MaxCode,200h
               _InitCodeTable
ChangeOldCode:  mov     al,byte ptr CurCode
                xor     ah,ah
                mov     OldCode,ax
                jmp     NextByte
IsInTable:      mov     ax,OldCode

                lea     ebp,[PrefixTable]
                cmp     ax,[ebp+ebx]

                jnz     NotTheSame

                lea     ebp, [SymbolTable]
                mov     al,byte ptr [ebp+esi]
                cmp     al,byte ptr [CurCode]
                jnz     NotTheSame

                lea     ebp, [CodeTable]
                mov     ax,[ebp+ebx]

                mov     OldCode,ax
                jmp     NextByte

NotTheSame:     add     si,Temp
                add     Temp,2
                cmp     si,LZMax
                jc      NoOverflow
                sub     si,LZMax
NoOverflow:     jmp     SearchLoop
LZWComprEnd:    mov     ax,OldCode
               _ComprCode
                mov     ax,EOICode
               _ComprCode
               _ResetCodeBuffer
                ret
PackFail:
                xor eax, eax
                mov OutPutOffs, eax
                inc eax
                mov OutPutOffs2, eax
                ret
LZWCompress     endp


CODE            ends

                end
