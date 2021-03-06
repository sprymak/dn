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

; *******************************************************
; *                                                     *
; *     Turbo Pascal Runtime Library Version 5.5        *
; *     File Name Expansion Routine                     *
; *                                                     *
; *     Copyright (C) 1988,89 Borland International     *
; *                                                     *
; *******************************************************

; Modified by -=DarkLord=- 31-Aug-1999

        TITLE   EXPN

        LOCALS  @@

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE

        PUBLIC  FExpandLFN

FExpandLFN         PROC    FAR

ResultP         EQU     (DWORD PTR [BP+10])
PathP           EQU     (DWORD PTR [BP+6])

        PUSH    BP
        MOV     BP,SP
        PUSH    DS
        CLD
        LDS     SI,PathP
        LODSB
        MOV     CL,AL
        XOR     CH,CH
        ADD     CX,SI
        LES     DI,ResultP
        INC     DI
        LODSW
        CMP     SI,CX
        JA      @@1
        CMP     AH,':'
        JNE     @@1
        CMP     AL,'a'
        JB      @@2
        CMP     AL,'z'
        JA      @@2
        SUB     AL,20H
        JMP     SHORT @@2
@@1:    DEC     SI
        DEC     SI
        MOV     AH,19H                  ;Get current drive
        INT     21H
        ADD     AL,'A'
        MOV     AH,':'
@@2:    STOSW
        CMP     SI,CX
        JE      @@21
        CMP     BYTE PTR [SI],'\'
        JE      @@3
@@21:   SUB     AL,'A'-1
        MOV     DL,AL
        MOV     AL,'\'
        STOSB
        PUSH    SI
        PUSH    DS

        MOV     AX,7147H                  ;LFN - Get current directory
        MOV     SI,DI
        PUSH    ES
        POP     DS
        INT     21H

        POP     DS
        POP     SI
        JC      @@3
        CMP     BYTE PTR ES:[DI],0
        JE      @@3
        PUSH    CX
        MOV     CX,-1
        XOR     AL,AL
        REPNE   SCASB
        DEC     DI
        MOV     AL,'\'
        STOSB
        POP     CX
@@3:    SUB     CX,SI
        REP     MOVSB
        XOR     AL,AL
        STOSB
        LDS     SI,ResultP
        INC     SI
        MOV     DI,SI
        PUSH    DI
@@4:    LODSB
        OR      AL,AL
        JE      @@6
        CMP     AL,'\'
        JE      @@6
        CMP     AL,'a'
        JB      @@5
        CMP     AL,'z'
        JA      @@5
;        SUB     AL,20H
@@5:    STOSB
        JMP     @@4
@@6:    CMP     WORD PTR [DI-2],'.\'
        JNE     @@7
        DEC     DI
        DEC     DI
        JMP     SHORT @@9
@@7:    CMP     WORD PTR [DI-2],'..'
        JNE     @@9
        CMP     BYTE PTR [DI-3],'\'
        JNE     @@9
        SUB     DI,3
        CMP     BYTE PTR [DI-1],':'
        JE      @@9
@@8:    DEC     DI
        CMP     BYTE PTR [DI],'\'
        JNE     @@8
@@9:    OR      AL,AL
        JNE     @@5
        CMP     BYTE PTR [DI-1],':'
        JNE     @@10
        MOV     AL,'\'
        STOSB
@@10:   MOV     AX,DI
        POP     DI
        SUB     AX,DI
        DEC     DI
        STOSB
        POP     DS
        POP     BP
        RET     4

FExpandLFN         ENDP

CODE    ENDS

        END
