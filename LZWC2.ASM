;/////////////////////////////////////////////////////////////////////////
;/
;/  Dos Navigator Open Source 1.51.04/DOS
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
                ideal
                jumps

segment         DATA  'DATA'
extrn MaxCode        : word
extrn RunCode        : word
extrn RunningBits    : word
extrn CodeBuffer     : dword
extrn CurCode        : word
extrn OldCode        : word
extrn CurBuffShift   : word
extrn InputOffs      : word
extrn InputSeg       : word
extrn OutPutOffs     : word
extrn OutPutSeg      : word
extrn Temp           : word
extrn DataSize       : word
extrn OutPutOffs2    : word
extrn CodeTable      : dword
extrn PrefixTable    : dword
extrn SymbolTable    : dword
ends            DATA



segment         CODE
                assume  cs:CODE, ds:DATA

                public  LZWCompr

                p386

macro           _GetByte
                dec     [DataSize]
                jz      LZWComprEnd
                les     si, [dword ptr InputOffs]
                mov     al, [es:si]
                mov     ah, 0
                inc     [InputOffs]
                endm

macro           _ComprCode
local           Rotate2,NoRotate2,CheckFull,NoIncBits
                xor     bx,bx
                mov     cx,[CurBuffShift]
                or      cx,cx
                jz      NoRotate2
Rotate2:        shl     ax,1
                rcl     bx,1
                loop    Rotate2
NoRotate2:      or      [word ptr CodeBuffer+0],ax
                or      [word ptr CodeBuffer+2],bx
                mov     ax,[RunningBits]
                add     [CurBuffShift],ax
CheckFull:      cmp     [CurBuffShift],8
                jc      NoIncBits
                les     di,[dword ptr OutputOffs]
                mov     ax,[word ptr CodeBuffer]
                mov     [es:di],al
                inc     [OutPutOffs]
                rept    8
                shr     [word ptr CodeBuffer+2],1
                rcr     [word ptr CodeBuffer+0],1
                endm
                sub     [CurBuffShift],8
                jmp     CheckFull
NoIncBits:
                endm

macro           _InitCodeTable
                les     di,[CodeTable]
                mov     cx,LZMax
                xor     ax,ax
                cld
                rep stosw
                endm

macro           _ResetCodeBuffer
local Again

Again:
                les     di,[dword ptr OutPutOffs]
                mov     ax,[word ptr CodeBuffer]
                mov     [es:di],al
                inc     [OutPutOffs]
                rept    8
                shr     [word ptr CodeBuffer+2],1
                rcr     [word ptr CodeBuffer+0],1
                endm
                sub     [CurBuffShift],8
                jnc     Again
ResetBuffEnd:   mov     [CurBuffShift],0
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
;        BX:AX - Pointer to temporary buffer (21K min.)
;        DX:CX - Pointer to compressed data.
;        DI:SI - Pointer to buffer to decompress.
; Exit:  AX - size of compressed data.

proc            LZWCompr  near

                push    bp

                mov     [InputOffs],  cx
                mov     [InputSeg],   dx
                mov     [OutPutOffs], si
                mov     [OutPutOffs2],si
                mov     [OutputSeg],  di

                mov     [word ptr CodeTable+2],bx
                mov     [word ptr PrefixTable+2],bx
                mov     [word ptr SymbolTable+2],bx

                mov     bx, ax
                lea     ax, [cCodeTable+bx]
                mov     [word ptr CodeTable], ax
                lea     ax, [cPrefixTable+bx]
                mov     [word ptr PrefixTable], ax
                lea     ax, [cSymbolTable+bx]
                mov     [word ptr SymbolTable], ax

                inc     [DataSize]

                call    LZWCompress
                mov     ax,[OutPutOffs]
                sub     ax,[OutPutOffs2]
                pop     bp
                ret

endp            LZWCompr

proc            LZWCompress near
               _InitCodeTable
                mov     [RunCode],102h
                mov     [RunningBits],9
                mov     [MaxCode],200h
                xor     ax,ax
                mov     [CurBuffShift],ax
                mov     [CurCode],ax
                mov     [word ptr CodeBuffer+0],ax
                mov     [word ptr CodeBuffer+2],ax
                mov     ax,ClearCode
               _ComprCode
               _GetByte
                mov     [OldCode],ax
NextByte:      _GetByte
                mov     [CurCode],ax
                mov     cl,5
                shl     ax,cl
                xor     ax,[OldCode]
                and     ax,0FFFh
                mov     si,ax
                mov     [Temp],1
SearchLoop:     mov     bx,si
                shl     bx,1

                les     bp, [CodeTable]
                add     bp,bx
                cmp     [word ptr es:bp],0

                jnz     IsInTable
                mov     ax,[OldCode]
               _ComprCode
                mov     bx,si
                shl     bx,1
                mov     ax,[RunCode]
                mov     [Temp],ax
                cmp     ax,LZMax
                jnc     CheckOverflow

                les     bp, [CodeTable]
                add     bp,bx
                mov     [es:bp],ax

                mov     ax,[OldCode]

                les     bp,[PrefixTable]
                add     bp,bx

                mov     [es:bp],ax

                mov     al,[byte ptr ds:CurCode]

                les     bp, [SymbolTable]
                mov     [byte ptr es:bp+si],al

                inc     [RunCode]
CheckOverflow:  mov     ax,[Temp]
                cmp     ax,[MaxCode]
                jnz     ChangeOldCode
                cmp     [byte ptr ds:RunningBits],12
                jnc     SendClearCode
                inc     [byte ptr ds:RunningBits]
                shl     [MaxCode],1
                jmp     ChangeOldCode
SendClearCode:  mov     ax,ClearCode
               _ComprCode
                mov     [RunCode],102h
                mov     [byte ptr ds:RunningBits],9
                mov     [MaxCode],200h
               _InitCodeTable
ChangeOldCode:  mov     al,[byte ptr ds:CurCode]
                mov     ah,0
                mov     [OldCode],ax
                jmp     NextByte
IsInTable:      mov     ax,[OldCode]

                les     bp,[PrefixTable]
                add     bp,bx

                cmp     ax,[es:bp]

                jnz     NotTheSame

                les     bp, [SymbolTable]

                mov     al,[byte ptr es:bp+si]
                cmp     al,[byte ptr ds:CurCode]
                jnz     NotTheSame

                les     bp, [CodeTable]
                add     bp,bx

                mov     ax,[es:bp]

                mov     [OldCode],ax
                jmp     NextByte
NotTheSame:     add     si,[Temp]
                add     [Temp],2
                cmp     si,LZMax
                jc      NoOverflow
                sub     si,LZMax
NoOverflow:     jmp     SearchLoop
LZWComprEnd:    mov     ax,[OldCode]
               _ComprCode
                mov     ax,EOICode
               _ComprCode
               _ResetCodeBuffer
                ret
endp            LZWCompress


ends            CODE
                end
