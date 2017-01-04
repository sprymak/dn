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

; *******************************************************
; *                                                     *
; *     Turbo Pascal Run-time Library                   *
; *     Standard Textfile Open Routine                  *
; *                                                     *
; *     Copyright (c) 1988,92 Borland International     *
; *                                                     *
; *******************************************************

;SimulateRealModeInt by DataCompBoy,  3.08.2000

        TITLE   S-TFIO

        INCLUDE SE.ASH

DATA    SEGMENT BYTE PUBLIC 'DATA'
         EXTRN  RMR:BYTE  ;.32
         EXTRN  LowSeg: WORD
         EXTRN  LowBuf: DWORD
DATA    ENDS

DGROUP  GROUP   DATA

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

; Externals

        EXTRN   FileRead:FAR,FileWrDev:FAR,FileWrite:FAR
        EXTRN   FileClose:FAR

; Publics

        PUBLIC  SimFileOpen

; Standard textfile open procedure

        sEDI EQU 00h
        sESI EQU 04h
        sEBP EQU 08h
        sEBX EQU 10h
        sEDX EQU 14h
        sECX EQU 18h
        sEAX EQU 1Ch
        sFLG EQU 20h
        sES  EQU 22h
        sDS  EQU 24h
        sFS  EQU 26h
        sGS  EQU 28h
        sIP  EQU 2Ah
        sCS  EQU 2Ch
        sSP  EQU 2Eh
        sSS  EQU 30h

DOSFn:
        cmp     [di].fPrivate, 0
        jne     @@LFN
        INT     DOS
        ret
@@LFN:
        push    dx cx bx si di
        cmp     ah, dosOpen
        je      @@DoS
        cmp     ah, dosCreate
        je      @@DoS
        mov     ax, 1
        stc
        jmp     @@Exit

comment ~
INT 21h  AX=716C
 INT 21 - Windows95 - LONG FILENAME - CREATE OR OPEN FILE
         AX = 716Ch
         BX = access mode and sharing flags (see #1122,also AX=6C00h)
         CX = attributes
         DX = action (see #1121)
         DS:SI -> ASCIZ filename
         DI = alias hint (number to append to short filename for disambiguation)
 Return: CF clear if successful
             AX = file handle
             CX = action taken
                 0001h file opened
                 0002h file created
                 0003h file replaced
         CF set on error
             AX = error code (see #1020)
                 7100h if function not supported
 See: AX=6C00h,AX=7141h,AX=7156h,AX=71A9h
~
@@DoS:
        cld
        push    ds
        push    es

        push    ds
        pop     es ;ES=DS of Name

        push    ax
        push    es
        mov     ax, SEG DGROUP
        mov     ds, ax
        mov     es, ax
     ASSUME DS:DGROUP
        mov     di, OFFSET RMR
        mov     cx, 16h
        xor     ax, ax
    rep stosw
        pop     es ;ES=DS of Name

        mov     di, OFFSET RMR
        mov     ax, 716Ch ;Open
        mov     ds:[di+sEAX], ax
        mov     ax, ds:[LOWSEG]
        mov     ds:[di+sDS], ax ; ESI=0!
        pop     ax
        cmp     ah, dosOpen
        jnz     @@Create
        mov     ds:[di+sEBX], al
        mov     cl, 1h
        mov     ds:[di+sEDX], cl
        jmp     @@Next

@@Create:
        mov     al, 64 + 2
        mov     ds:[di+sEBX], al
        mov     cl, 12h
        mov     ds:[di+sEDX], cl

@@Next:
        push    di
        xor     al, al
        mov     di, dx ;{Name}
        xor     cx, cx
        dec     cx
  repne scasb
        sub     di, dx
        mov     cx, di ;{cx = name len}
        push    ds
        push    es

        push    es
        les     di, DS:[LOWBUF]
        pop     ds ;DS = old DS (segment of name...)

        push    si
        mov     si, dx
    rep movsb
        pop     si
        pop     es
        pop     ds
        pop     di

        mov     bx, 21h
        xor     cx, cx
        push    ds
        pop     es
        mov     ax,0300h
        int     31h      ;Simulate Real-mode int
        mov     ax,ds:[DI+sFLG]
        push    ax
        popf
        mov     ax,ds:[DI+sEAX]

        pop     es
        pop     ds

@@Exit: pop di si bx cx dx

        ret

SimFileOpen:

        MOV     BX,SP
        PUSH    DS
        LDS     DI,SS:[BX+4]
        XOR     CX,CX
        MOV     [DI].fHandle,CX
;       MOV     AX,dosOpen*256
        MOV     AX,dosOpen*256 + 64
        CMP     [DI].fMode,fmInput
        JE      @@1
        MOV     AL,2
        INC     [DI].fHandle
        CMP     [DI].fMode,fmInOut
        JE      @@1
        MOV     AH,dosCreate
@@1:    CMP     [DI].fLFName,0
        JE      @@2
        LEA     DX,[DI].fLFName
        call    DOSFn
        JC      @@6
        MOV     [DI].fHandle,AX
@@2:    MOV     AX,OFFSET FileRead
        MOV     DX,SEG FileRead
        XOR     CX,CX
        XOR     BX,BX
        CMP     [DI].fMode,fmInput
        JE      @@5
        MOV     BX,[DI].fHandle
        MOV     AX,dosIOCtl*256
        INT     DOS
        TEST    DL,80H
        MOV     AX,OFFSET FileWrDev
        MOV     DX,SEG FileWrDev
        MOV     CX,AX
        MOV     BX,DX
        JNE     @@4
        CMP     [DI].fMode,fmInOut
        JNE     @@3
        CALL    FileAppend
@@3:    MOV     AX,OFFSET FileWrite
        MOV     DX,SEG FileWrite
        XOR     CX,CX
        XOR     BX,BX
@@4:    MOV     [DI].fMode,fmOutput
@@5:    MOV     [DI].fInOutProc.ofs,AX
        MOV     [DI].fInOutProc._seg,DX
        MOV     [DI].fFlushProc.ofs,CX
        MOV     [DI].fFlushProc._seg,BX
        MOV     [DI].fCloseProc.ofs,OFFSET FileClose
        MOV     [DI].fCloseProc._seg,SEG FileClose
        XOR     AX,AX
@@6:    POP     DS
        RETF    4

; Prepare textfile for appending

FileAppend:

        XOR     DX,DX
        XOR     CX,CX
        MOV     BX,[DI].fHandle
        MOV     AX,dosSeek*256+2
        INT     DOS
        SUB     AX,128
        SBB     DX,0
        JNC     @@1
        XOR     AX,AX
        XOR     DX,DX
@@1:    MOV     CX,DX
        MOV     DX,AX
        MOV     BX,[DI].fHandle
        MOV     AX,dosSeek*256
        INT     DOS
        LEA     DX,[DI].fBuffer
        MOV     CX,128
        MOV     BX,[DI].fHandle
        MOV     AH,dosRead
        INT     DOS
        JNC     @@2
        XOR     AX,AX
@@2:    XOR     BX,BX
@@3:    CMP     BX,AX
        JE      @@5
;       CMP     [DI].fBuffer[BX],eof
;       JE      @@4
        INC     BX
        JMP     @@3
@@4:    MOV     DX,BX
        SUB     DX,AX
        MOV     CX,-1
        MOV     BX,[DI].fHandle
        MOV     AX,dosSeek*256+2
        INT     DOS
        XOR     CX,CX
        MOV     BX,[DI].fHandle
        MOV     AH,dosWrite
        INT     DOS
@@5:    RET

CODE    ENDS

        END
