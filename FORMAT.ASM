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

    TITLE    FORMAT

    LOCALS    @@

FORMAT_TEXT    SEGMENT    BYTE PUBLIC 'CODE'

    ASSUME    CS:FORMAT_TEXT,DS:NOTHING

    PUBLIC    Format_Str

; function FormatStr(var Result: String; Format: String; var Params)

Format_Str:

Result        EQU    (DWORD PTR [BP+14])
Format        EQU    (DWORD PTR [BP+10])
Params        EQU    (DWORD PTR [BP+6])
ParOfs        EQU    (WORD PTR [BP-2])
Buffer        EQU    (BYTE PTR [BP-14])

    PUSH    BP
    MOV    BP,SP
    PUSH    WORD PTR Params
    SUB    SP,12
    PUSH    DS
    LDS    SI,Format
    LES    DI,Result
    INC    DI
    CLD
    LODSB
    XOR    AH,AH
    XCHG    AX,CX
@@1:    JCXZ    @@9
    LODSB
    DEC    CX
    CMP    AL,'%'
    JE    @@3
@@2:    STOSB
    JMP    @@1
@@3:    JCXZ    @@9
    LODSB
    DEC    CX
    CMP    AL,'%'
    JE    @@2
    MOV    BX,' '
    XOR    DX,DX
    CMP    AL,'0'
    JNE    @@4
    MOV    BL,AL
@@4:    CMP    AL,'-'
    JNE    @@5
    INC    BH
    JCXZ    @@9
    LODSB
    DEC    CX
@@5:    CMP    AL,'0'
    JB    @@6
    CMP    AL,'9'
    JA    @@6
    SUB    AL,'0'
    XCHG    AX,DX
    MOV    AH,10
    MUL    AH
    ADD    AL,DL
    XCHG    AX,DX
    JCXZ    @@9
    LODSB
    DEC    CX
    JMP    @@5
@@6:    CMP    AL,'#'
    JNE    @@10
    SHL    DX,1
    SHL    DX,1
    ADD    DX,ParOfs
    MOV    WORD PTR Params,DX
    JMP    @@1
@@9:    XCHG    AX,DI
    LES    DI,Result
    SUB    AX,DI
    DEC    AX
    STOSB
    POP    DS
    MOV    SP,BP
    POP    BP
    RETF    12
@@10:    PUSH    DS
    PUSH    SI
    PUSH    CX
    PUSH    DX
    PUSH    BX
    CALL    Convert
    POP    BX
    POP    DX
    OR    DX,DX
    JE    @@12
    SUB    DX,CX
    JAE    @@12
    OR    BH,BH
    JNE    @@11
    SUB    SI,DX
@@11:    ADD    CX,DX
    XOR    DX,DX
@@12:    OR    BH,BH
    JE    @@13
    REP    MOVSB
@@13:    XCHG    CX,DX
    MOV    AL,BL
    REP    STOSB
    XCHG    CX,DX
    REP    MOVSB
    POP    CX
    POP    SI
    POP    DS
    JMP    @@1

; Convert next parameter to string
; In    AL    = Conversion character
; Out    DS:SI = Pointer to string
;    CX    = String length

Convert:

    XCHG    AX,DX
    LDS    SI,Params
    LODSW
    XCHG    AX,BX
    LODSW
    XCHG    AX,BX
    MOV    WORD PTR Params,SI
    XOR    CX,CX
    LEA    SI,Buffer[12]
    PUSH    SS
    POP    DS
    AND    DL,0DFH
    CMP    DL,'C'
    JE    ConvertChar
    CMP    DL,'S'
    JE    ConvertStr
    CMP    DL,'D'
    JE    ConvertDec
    CMP    DL,'X'
    JE    ConvertHex
    RET

ConvertStr:

    MOV    DX,AX
    OR    DX,BX
    JE    @@1
    MOV    SI,AX
    MOV    DS,BX
    LODSB
    MOV    CL,AL
@@1:    RET

ConvertHex:

    MOV    CL,16
    JMP    SHORT ConvertNum

ConvertDec:

    MOV    CL,10
    OR    BX,BX
    JNS    ConvertNum
    NEG    BX
    NEG    AX
    SBB    BX,0
    CALL    ConvertNum
    MOV    AL,'-'

ConvertChar:

    INC    CX
    DEC    SI
    MOV    [SI],AL
    RET

ConvertNum:

    PUSH    SI
@@1:    XOR    DX,DX
    XCHG    AX,BX
    DIV    CX
    XCHG    AX,BX
    DIV    CX
    ADD    DL,'0'
    CMP    DL,'0'+10
    JB    @@2
    ADD    DL,'A'-'0'-10
@@2:    DEC    SI
    MOV    [SI],DL
    MOV    DX,AX
    OR    DX,BX
    JNE    @@1
    POP    CX
    SUB    CX,SI
    RET

FORMAT_TEXT    ENDS

    END
