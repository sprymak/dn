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

; Cyrix detection on 440BX chipset workaround code
; uses PCI BIOS 2.0c+ calls to detect 440BX and apply workaround if found

BX_Cyrix_Workaround_Start       proc near
LOCAL   dev : Word
        .386
IFDEF   __debug__
        push    dx
        lea     dx, BXWAS
        call    printDebug
        pop     dx
ENDIF
        mov     ax,0B101h       ; get PCI BIOS version
        clr     edi
        int     1Ah
        test    ah,ah           ; ah is set to 0 if PCI BIOS present
        jnz     @@noPCI
        mov     ax,0B102h       ; find PCI device
        mov     cx,7190h        ; 82443BX id #1
        mov     dx,8086h        ; Intel
        clr     si              ; device 0
        int     1Ah
        jnc     @@Configure
        mov     ax,0B102h       ; find PCI device
        mov     cx,7192h        ; 82443BX id #2 (only one of these may present)
        mov     dx,8086h        ; Intel
        clr     si              ; device 0
        int     1Ah
        jc      @@noPCI
@@Configure:
        mov     dev,bx          ; save bus/device/function
        mov     ax,0B108h       ; read configuration byte
        mov     di,7Ah          ; PMSR index in configuration space
        int     1Ah
        jc      @@noPCI         ; error occured
        test    cl,40h          ; bit 6 set?
        jz      @@noPCI         ; not set, no need to reprogram
        mov     al,cl
        mov     ah,1            ; data read ok
        push    ax
        and     cl, (not 40h)   ; reset bit 6 of PMSR
                                ; I/O port 22h data forwarded to PCI bus
        mov     bx,dev
        mov     ax,0B10Bh       ; write configuration byte
        mov     di,7Ah          ; PMSR index in configuration space
        int     1Ah
        pop     ax              ; ah set to 1 if data read from 440BX
                                ; al then contains original PMSR state
        jmp     @@Q
@@noPCI:
        clr     ax
@@Q:
        ret
        endp

BX_Cyrix_Workaround_End         proc near
LOCAL   PMSR : Byte
IFDEF   __debug__
        push    dx
        lea     dx, BXWAE
        call    printDebug
        pop     dx
ENDIF
        test    ah,ah           ; on entry ah = 0|1, al = PMSR if ah = 1
        jz      @@Q
        mov     PMSR,al
        mov     ax,0B102h
        mov     cx,7190h        ; 82443BX id #1
        mov     dx,8086h        ; Intel
        clr     si              ; device 0
        int     1Ah
        jnc     @@Configure
        mov     ax,0B102h       ; find PCI device
        mov     cx,7192h        ; 82443BX id #2 (only one of these may present)
        mov     dx,8086h        ; Intel
        clr     si              ; device 0
        int     1Ah
        jc      @@Q
@@Configure:
        mov     ax,0B10Bh       ; restore configuration
                                ; bx already has bus/device/function
        mov     cl,PMSR
        mov     di,7Ah          ; PMSR index in configuration space
        int     1Ah
@@Q:
        ret
        endp
