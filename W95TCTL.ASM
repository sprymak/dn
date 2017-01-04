;/////////////////////////////////////////////////////////////////////////
;//
;//            Этот файл был позаимствован из Connect''а
;//                  В честь Матаpыкиной Ульяны...
;//////////////////////////////////////////////////////////////////////////

; *******************************************************
; *                                                     *
; *     Turbo Pascal Run-time Library                   *
; *     Textfile Control Routines                       *
; *                                                     *
; *     Copyright (c) 1988,92 Borland International     *
; *                                                     *
; *******************************************************

        TITLE   TCTL

        INCLUDE SE.INC

DATA    SEGMENT WORD PUBLIC

        EXTRN   InOutRes:WORD

DATA    ENDS

CODE    SEGMENT BYTE PUBLIC

        ASSUME  CS:CODE,DS:DATA

; Externals

        EXTRN   FileOpen:FAR

; Publics

         PUBLIC  FileRead,FileWrDev,FileWrite,FileClose

FileRead:

        MOV     BX,SP
        PUSH    DS
        LES     DI,SS:[BX+4]
        LDS     DX,ES:[DI].fBufPtr
        MOV     CX,ES:[DI].fBufSize
        MOV     BX,ES:[DI].fHandle
        MOV     AH,dosRead
        INT     DOS
        JC      @@2
        MOV     ES:[DI].fBufEnd,AX
        XOR     AX,AX
@@1:    MOV     ES:[DI].fBufPos,0
        POP     DS
        RETF    4
@@2:    MOV     ES:[DI].fBufEnd,0
        JMP     SHORT @@1

; Standard textfile disk write procedure

FileWrite:

        MOV     BX,SP
        PUSH    DS
        LES     DI,SS:[BX+4]
        LDS     DX,ES:[DI].fBufPtr
        XOR     CX,CX
        XCHG    CX,ES:[DI].fBufPos
        MOV     BX,ES:[DI].fHandle
        MOV     AH,dosWrite
        INT     DOS
        JC      @@1
        SUB     AX,CX
        JE      @@1
        MOV     AX,101
@@1:    POP     DS
        RETF    4

; Standard textfile device write procedure

FileWrDev:

        MOV     BX,SP
        PUSH    DS
        LES     DI,SS:[BX+4]
        LDS     DX,ES:[DI].fBufPtr
        XOR     CX,CX
        XCHG    CX,ES:[DI].fBufPos
        MOV     BX,ES:[DI].fHandle
        MOV     AH,dosWrite
        INT     DOS
        JC      @@1
        XOR     AX,AX
@@1:    POP     DS
        RETF    4

; Standard textfile close procedure

FileClose:

        MOV     BX,SP
        LES     DI,SS:[BX+4]
        MOV     BX,ES:[DI].fHandle
        CMP     BX,4
        JBE     @@1
        MOV     AH,dosClose
        INT     DOS
        JC      @@2
@@1:    XOR     AX,AX
@@2:    RETF    4

CODE    ENDS

        END
