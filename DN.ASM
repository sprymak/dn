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

; Interface data:
;     Note that command line must be in PASCAL format and contain 0Dh at
;     line end. 0Dh mustn't be included in string length.
;
; 2F func 99h
;     sbf's     0  return info
;               1  return pointer 2 commandline
;               2  set 'true' DN's errorlevel
;               3  set how2run
;               4  set timer
;               5  get timer
;               6  get/set misc info

; Some EQUs
;ClearKeyboard   EQU    1    ;used in ClearKBD
;CtrlCviaINT21   EQU    1

CBEnable        EQU    0EAH ; Jmp Far - used in DataCompBoy's Ctrl-Break handling
CBDisable       EQU    0CFH ; iRet    - used in DataCompBoy's Ctrl-Break handling

; Structures
EPB             struc
Environment     dw     0
CMD_Offset      dw     0
CMD_Seg         dw     0
FCB1_Offset     dw     5Ch
FCB1_Seg        dw     0
FCB2_Offset     dw     6Ch
FCB2_Seg        dw     0
EPBLength       db     0Eh
EPB             ends

; MACRO definitions

ClearKBD        MACRO
                IFDEF ClearKeyboard
                sub    ax,ax
                mov    ds,ax
                mov    ax,ds:[41Ah]
                mov    ds:[41Ch],ax
                ENDIF
                ENDM

;size & speed optimizations by Max Piwamoto

                IFDEF _DPMI_
DNversionTXT    equ    'Open Source 1.51.08/DPMI'
                ELSE
DNversionTXT    equ    'Open Source 1.51.08/DOS'
                ENDIF
DNversionBIN    equ    15108
DNversionSIGN   equ    'DN'

                .model tiny
                .code
                .286                    ;used for code optimization
                                        ;since DN won't work on 8086
                org    100h

Start:
                jmp    @Begin
                db     29+24 dup (0)    ;stack size debugged by piwamoto
                                        ;minimal required size is 32 bytes
                                        ;32=29+length(jmp @Begin)
_Stack:         ;stack pointer


IFNDEF CtrlCviaINT21

CBByte          db     CBDisable        ; (DataCompBoy) Ctrl-Break handling
OldInt1B        dw     0,0

ENDIF

Int2Fentry:
                cmp    ah,99h
                je     @@MyCall
@@Old:          db     0EAh
OldInt2F:       dw     0,0
@@MyCall:       cmp    al, 6                     ; maximal supported function
                ja     @@Old
                jz     @@My6
                sub    ax, 9900h
                jnz    @@My1                     ; fn 0 - get info:
                mov    ax,word ptr cs:NumRun     ; al-NumRun, ah-FirstRun
                mov    cl,byte ptr cs:ExecErrl   ; cl-child errorlevel
                mov    bx,DNversionSIGN
                mov    dx,DNversionBIN
                iret
@@My1:          dec    ax                        ; fn 1
                jnz    @@my2                     ; return pointer
                push   cs                        ; to cmd line
                pop    es                        ; in ES:BX
                mov    bx,offset CmdLine
                iret
@@My2:          dec    ax                        ; fn 2
                jnz    @@my3                     ; set 'true' errorlevel
                mov    ch,1                      ; of DN.PRG in cl...
                mov    word ptr CS:_DN_Errl,cx
                iret
@@My3:          dec    ax                        ; fn 3
                jnz    @@my4
                mov    byte ptr cs:How2run,Cl    ; How2run? dos/or 2Eh??
                iret
@@My4:                                           ; fn 4
@@My5:                                           ; fn 5
                xchg   word ptr CS:DNTimer, cx
                xchg   word ptr CS:DNTimer+2, dx
                iret
@@My6:          mov    cx, word ptr CS:DN_Misc
                test   dh, 80h
                jz     @@IRETpos
                mov    CS:DN_Misc, dx
@@IRETpos:
                iret


; Run DN.PRG ...
RunDN:
                push   cs
                pop    ds
                push   cs
                pop    es

IFDEF CtrlCviaINT21
                sub    dl, dl           ; Ctrl-C off
                call   CtrlBRK1
ELSE
                mov    al, CBDisable
                mov    CBByte, al
ENDIF
                mov    DNEPB.CMD_Offset,80h
                mov    DNEPB.CMD_Seg,cs
                mov    ax,ds:[2Ch]
                mov    DNEPB.Environment,ax
                mov    dx,offset DNPath
                mov    bx,offset DNEPB
                mov    ax,4B00h
                int    21h
                push   cs
                pop    ds
                push   cs
                pop    es
                jc     ExecError
; Query DN.PRG execution status ...
                sub    ax,ax                      ; not for first time...
                mov    FirstRun,al                ; if DN sets errorllevel
                xchg   ax,word ptr _DN_Errl       ; himself, then
                or     ah,ah                      ; assume this is
                jnz    @@Errl                     ; true, else
                mov    ah,4dh
                int    21h
@@Errl:         cmp    al,1                       ; believe in DOS
                jne    @halt                      ; errorlevel...
; Execute a DOS command ...
@@RunCMd:
IFDEF CtrlCviaINT21
                call   CtrlBRK          ; Ctrl-C on
ELSE
                mov    al, CBEnable
                mov    CBByte, al
ENDIF
                mov    SI,offset CMDline
                sub    ax,ax
                xchg   al,How2Run
                dec    ax
                jnz    @@RunByDos
;si=CMDline
                int    2Eh
                sub    al,al    ; f..d errorlevel!
                jmp    short @@RunDone
@@RunByDOS:
                mov    al,' '
;si=CMDline
                xchg   al,byte ptr [si]
                add    al,3
                mov    byte ptr COMMANDparm,al
                mov    DNEPB.CMD_Offset,offset COMMANDparm
                 ; lds    dx,COMSPECptr
                mov    dx,COMSPECptr ; Get Comspec ptr
                mov    bx,offset DNEPB
                mov    ax,4B00h
                int    21h
                mov    ah,4dh                  ; Save errorlevel
                int    21h                     ; of child process
@@RunDone:      mov    byte ptr cs:ExecErrl,al ;
; ... and return back to DN ...
                ClearKbd                ; Clear Keyboard stack
                jmp    RunDN            ; after execution child program

ExecError:      mov    dx,offset CanNotRun
                mov    al,-1
@Halt1:
                push   ax
                mov    ah,9
                int    21h
                pop    ax
@Halt:          push   ax

                ;mov    ax,CS:OrigEnv    ; restore local enviropment
                ;mov    CS:[2ch],ax      ;

                lds    dx,dword ptr ds:OldInt2F
                mov    ax,252Fh
                int    21h

IFDEF CtrlCviaINT21
                call   CtrlBRK          ; Ctrl-C on
ELSE
                lds    dx,dword ptr cs:OldInt1B
;ah=25
                mov    al,1Bh
                int    21h
ENDIF
                ClearKbd                 ; Clear keyboard when exit from DN

                pop    ax       ; errorlevel
                mov    ah,4ch
                int    21h

IFDEF CtrlCviaINT21
CtrlBRK:
                mov    dl,1
CtrlBRK1:
                mov    ax, 3301h        ; Ctrl-Break on/off
                int    21h
                retn
ENDIF

CanNotRun       db     'Can''t run DN.PRG',0dh,0ah,7,'$'
COMMANDparm     db     ' /C'
CmdLine:        db     128-3 dup (0)    ;CMDline is only 80h bytes in DOS
;CmdLine:        db     4,'exit',13,256-9 dup (0)
DNEPB:          EPB    <>

;DNinfo          db     DNversionSIGN ;not used
NumRun:         db     1    ;used
FirstRun        db     1    ;used
;RunErrl         db     0    ;not used
_DN_Errl        db     0    ;used
_DN_was_Errl    db     0    ;used
ExecErrl        db     0    ;used
How2Run         db     0    ;used - by exec or via 2Eh?
DNTimer         dd     0    ;used
DN_Misc         dw     0    ;used - Miscellaneous information

COMSPECptr      dw     0
DNPath:         db     160 dup (0) ; DN PATH and COMSPEC are here ...
COMSPEC         db     80 dup (0)  ; Temporary comspec here

; DN.COM leave resident only valable part of DNPath to save memory...

Copyright       db     13,'Dos Navigator ',DNversionTXT,'  Based on DN (C) 1991-99 RIT Labs',0Dh,0AH,'$'
DNfound         db     '(*)',0Dh,0Ah,'$'
CanNotFound     db     'No path to main file.',0Dh,0Ah,'$'
CanNotAllocate  db     'Not enough memory.',0Dh,0Ah,'$'
NotFndCOMMAND   db     'Cannot find COMSPEC variable.',0Dh,0Ah,'$'
NotEnoughFiles  db     'WARNING: Not enough file handles. Set at least FILES=20 in your CONFIG.SYS file.',0Dh,0Ah,'$'
FlagCError      db     'Unable to create flag file '
FlagFile        db     0

@Begin:

IFDEF CtrlCviaINT21
                sub    dl,dl            ; Ctrl-C off
                call   CtrlBRK1
ENDIF
;print (c)
                mov    dx,offset Copyright
                mov    ah,9
                int    21h

; Enabling close event handling...
;               mov    ax, 168fh
;               mov    dx, 1
;               int    2fh

;search for master ENV
                mov    ah,52h
                int    21h
                les    BX, dword ptr ES:[BX+4]
                sub    ax,ax
         @@1:   add    ax,word ptr ES:[BX+4]
                cmp    word ptr ES:[BX],0FFFFH
                je     @@2
                les    BX, dword ptr ES:[BX]
                jmp    @@1
         @@2:
                cmp    AX,20    ; check that we have enough file
                jae    @@3      ; handles

;                mov    AH , 67h
;                mov    BX , 30  ; Set 30 file handles
;                int    21h
;                jnc    @@3
                mov    dx,offset NotEnoughFiles
                mov    ah,9
                int    21h
;                mov    ax,4C01h
;                int    21h
@@3:
                mov    ax,9900h
                int    2fh
                cmp    BX,DNversionSIGN
                jne    @NoHere

                mov    dx, offset DNfound
                mov    ah,9
                int    21h
@NoHere:
                mov    ax,352Fh
                int    21h
                mov    word ptr OldInt2F,bx
                mov    word ptr OldInt2F+2,es
                mov    dx,offset Int2Fentry
                mov    ah,25h
                int    21h

;Control Break Handler
IFNDEF CtrlCviaINT21
                mov    ax,351Bh
                int    21h
                mov    word ptr OldInt1B,bx
                mov    word ptr OldInt1B+2,es
;                mov    dx,offset Int1Bentry
                mov    dx, offset CBbyte
                mov    ah,25h
                int    21h
ENDIF
; Intialize Exec Parameter Block for DN.PRG ...
                mov    ax,cs
                push   cs
                pop    es
                ;mov    DNEPB.CMD_Seg,ax
                mov    DNEPB.FCB1_Seg,ax
                mov    DNEPB.FCB2_Seg,ax
                ;mov    ds:CmdLineOffset,offset CmdLine
                ;mov    ds:[5Ch],cs

; ±±³ Scan ENV for Flag Directory ³±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

                cld
;ds=cs
                mov    ax,ds:[2Ch]
                mov    ds,ax
                mov    cx,8000h
                sub    si,si
@@ScanFlg:      push   si
                lodsw              ; DNSWP=
                cmp    ax,'ND'
                jne    @NoFnd
                lodsw
                dec    si
                cmp    al, '='
                je     @FLGfound
                inc    si
                cmp    ax,'WS'
                jne    @NoFnd
@DNFnd:
                lodsw
                cmp    ax,'=P'
                je     @FLGfound
@NoFnd:         pop    si

@NextEnv:       LODSB
                or     al,al
                loopnz @NextEnv
;al=0 after @NextEnv
                cmp    byte ptr [SI],al
                je     @EndEnv
                or     si,si
                jns    @@ScanFlg        ;if < 8000h
@EndEnv:        jmp    @@ScanComsp

@FlgFound:
                pop    ax
                mov    di,offset FlagFile
@FL0:           lodsb
                stosb
                or     al,al
                loopnz @FL0
                dec    di
                cmp    byte ptr ES:[DI-1],'\'
                jne    @BKSlash
                dec    di
@BKSlash:
                mov    ax,'D\'
                stosw
                mov    ax,'.N'
                stosw
                mov    ax,'LF'
                stosw
                mov    ax,'G' ;ah=0, al='G'
                stosw

; Scan environment and copy COMSPEC variable

@@ScanComsp:
;ds=cs:[2c] already
                mov    cx,8000h
                sub    si,si
ScanCOMSPEC:    push   si
                lodsw
                cmp    ax,'OC'
                jne    @NoEQ
                lodsw
                cmp    ax,'SM'
                jne    @NoEQ
                lodsw
                cmp    ax,'EP'
                jne    @NoEQ
                lodsw
                cmp    ax,'=C'
                jne    @NoEQ

                mov    DI,offset COMSPEC
@CopyComspec:   lodsb
                stosb
                or     al,al
                loopnz @CopyComspec
                ; mov    word ptr cs:COMSPECptr,si
                ; mov    word ptr cs:COMSPECptr+2,ds
                pop    ax
                dec    si
                jmp    @COMFound
@NoEQ:          pop    si

@NextEnv1:      LODSB
                or     al,al
                loopnz @NextEnv1

                or     si,si            ;if < 8000h
                jns    ScanCOMSPEC
                mov    dx,offset NotFndCOMMAND
@ERR1:
                jmp    PrintAndStop

; Find path of DN.COM and assume that it's also a path to DN.PRG ...
@COMFound:      mov    di,offset DNPath
@L0:            lodsb
                or     al,ds:[si]

                loopnz @L0
                mov    dx,offset CanNotFound
                jcxz   @ERR1

                lodsw
                lodsb

@L1:            lodsb
                stosb
                or     al,al
                jne    @L1
                sub    di,4
                mov    ax,'RP'
                stosw
                mov    ax,'G'
                stosw

                push   cs
                pop    ds

                mov    COMSPECptr,DI
                mov    si, offset COMSPEC
@@CopyCMS:      lodsb
                stosb
                or     al,al
                loopnz @@CopyCMS
                inc    di
                push   di        ; save ptr to free space

; Check if we have flag file

                mov    si,offset DNpath
                mov    di,offset FlagFile
                mov    dx,di
;al=0 after @@CopyCMS
                cmp    byte ptr [di],al
                jne    @OpenFlag
@NextChar:
                lodsb
                stosb
                or     al,al
                jnz    @NextChar
;changing .PRG to .FLG - only 2 bytes must be changed
                mov    ax,'LF'
                mov    [di-4],ax
@OpenFlag:
;dx = offset FlagFile
                mov    ax,3D02h
                int    21h
                mov    bx,ax
                jnc    @ReadFromFlag
;dx = offset FlagFile
                sub    cx,cx
                mov    ah,3Ch
                int    21h
                mov    bx,ax
                jnc    @writeToflag

; Here is comming a code if both functions opening and creating failed.
; This may happen if a long directory name is placed in DN environment
; variable.

                mov    si,dx            ; FlagFile
;                sub    cx,cx            ; attributes
;cx = 0 after FileCreate via ah=3ch, int21h
                sub    di,di            ; no short alias
                mov    dx,0011h         ; action flags
                mov    bx,0002h         ; access mode and sharing flags
                mov    ax,716Ch         ; WIN95 Long Open File
                int    21h
;ax=7100h isn't checked, coz CX can be used for error checkin'
                mov    bx,ax
                jc     @@FlagError
                dec    cx               ; $0001 file opened
                jz     @ReadFromFlag
                dec    cx               ; $0002 file created
                jz     @writeToflag

; Win95 support - END

@@FlagError:
                mov    si,offset FlagFile
@@nxx:          lodsb
                or     al,al
                jnz    @@nxx
                mov    byte ptr [si-1],0dh
                mov    word ptr [si],240Ah      ;cr,lf,eos
                mov    dx,offset FlagCError
                mov    ah,9
                int    21h
                jmp    @NextStep
@ReadFromFlag:
;bx=FlagHandle
                mov    ah,3Fh
                mov    dx,offset NumRun
                mov    cx,1
                int    21h

                inc    byte ptr NumRun
@writeToFlag:
;bx=FlagHandle
                mov    ax,4200h
                sub    cx,cx
                sub    dx,dx
                int    21h
;bx=FlagHandle
                mov    ah,40h
                mov    dx,offset NumRun
                mov    cx,1
                int    21h
;bx=FlagHandle
                mov    ah,3eh
                int    21h

@NextStep:
                pop    bx       ;free mem addr
                shr    bx,4
                inc    bx
; Compute maximal needed offset and free memory ...
                mov    sp,offset _Stack
                mov    ah,4Ah
                int    21h
                mov    dx,offset CanNotAllocate
                jc     PrintAndStop
; Jump to execution start ...
                jmp    RunDN

PrintAndStop:
                push   cs
                pop    ds
                mov    al,1
                jmp    @Halt1

                end    Start
