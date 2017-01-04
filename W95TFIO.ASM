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

; *******************************************************
; *                                                     *
; *     Turbo Pascal Run-time Library                   *
; *     Standard Textfile Open Routine                  *
; *                                                     *
; *     Copyright (c) 1988,92 Borland International     *
; *                                                     *
; *******************************************************

        TITLE   TFIO

        INCLUDE SE.ASH

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

; Externals

        EXTRN   FileRead:FAR,FileWrDev:FAR,FileWrite:FAR
        EXTRN   FileClose:FAR

; Publics

        PUBLIC  FileOpen

; Standard textfile open procedure


DOSFn:
        cmp     [di].fPrivate, 0
        jne     @@LFN
        INT     DOS
        ret
@@LFN:
        push    dx cx bx si di
        cmp     ah, dosOpen
        je      @@Open
        cmp     ah, dosCreate
        je      @@Create
        mov     ax, 1
        stc
        jmp     @@Exit

comment ~
INT 21h  AH=3C
 INT 21 - DOS 2+ - "CREAT" - CREATE OR TRUNCATE FILE
         AH = 3Ch
         CX = file attributes (see #0748)
         DS:DX -> ASCIZ filename
 Return: CF clear if successful
             AX = file handle
         CF set on error
             AX = error code (03h,04h,05h) (see #1020 at AH=59h/BX=0000h)

INT 21h  AH=3D
 INT 21 - DOS 2+ - "OPEN" - OPEN EXISTING FILE
         AH = 3Dh
         AL = access and sharing modes (see #0749)
         DS:DX -> ASCIZ filename
         CL = attribute mask of files to look for (server call only)
 Return: CF clear if successful
             AX = file handle
         CF set on error
             AX = error code (01h,02h,03h,04h,05h,0Ch,56h) (see #1020 at AH=59h)


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
@@Create:
        mov     ax, 716Ch
        mov     si, dx
        mov     dx, 12h
        mov     bx, 64 + 2
        mov     di, 0
        int     21h
        jmp     @@Exit

@@Open:
        xor     ah, ah
        mov     bx, ax
        xor     ch, ch
        mov     ax, 716Ch
        mov     si, dx
        mov     di, 0
        mov     dx, 1
        int     21h

@@Exit: pop di si bx cx dx

        ret

FileOpen:

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
