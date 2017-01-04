{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
//  Based on Dos Navigator (C) 1991-99 RIT Research Labs
//
//  This programs is free for commercial and non-commercial use as long as
//  the following conditions are aheared to.
//
//  Copyright remains RIT Research Labs, and as such any Copyright notices
//  in the code are not to be removed. If this package is used in a
//  product, RIT Research Labs should be given attribution as the RIT Research
//  Labs of the parts of the library used. This can be in the form of a textual
//  message at program startup or in documentation (online or textual)
//  provided with the package.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//  1. Redistributions of source code must retain the copyright
//     notice, this list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright
//     notice, this list of conditions and the following disclaimer in the
//     documentation and/or other materials provided with the distribution.
//  3. All advertising materials mentioning features or use of this software
//     must display the following acknowledgement:
//     "Based on Dos Navigator by RIT Research Labs."
//
//  THIS SOFTWARE IS PROVIDED BY RIT RESEARCH LABS "AS IS" AND ANY EXPRESS
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
//  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//  The licence and distribution terms for any publically available
//  version or derivative of this code cannot be changed. i.e. this code
//  cannot simply be copied and put under another distribution licence
//  (including the GNU Public Licence).
//
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn230-change_drive.patch
//  dn269-remove_IOmega_detection.patch
//
//  2.7.0
//  dn270-fix_drivetype_detection.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

{ --------------------------------------------------------------------------- }
{ DRVTYPES.PAS  Drive Type Detection.                             Version 1.0 }
{                                                                             }
{ CopyRight (C) 1999 by Luzin Aleksey                                         }
{ E-Mail: Aleksey@iis.nsk.su                                                  }
{ ICQ: 35258235                                                               }

{$S-,R-,I-,X+,O+}
{ disable stack, I/O and range checking, enable extended syntax,              }
{ overlays allowed                                                            }

unit DrvTypes;
{ drive types }

interface

const
  dtError      = $00; { Unknown drive type         }
  dtFixed      = $01; { Fixed disk                 }
  dtRemovable  = $02; { Removable drive            }
  dtRemote     = $03; { Network drive              }
  dtCDROM      = $04; { CD-ROM drive               }
  dtDblSpace   = $05; { DoubleSpace                }
  dtSUBST      = $06; { SUBST                      }
  dtStacker    = $07; { Stacker                    }
  dtRAMDrive   = $08; { RAM                        }
  dtDublDisk   = $09; { Vertisoft DoubleDisk 2.6+  }
  dtDiskreet   = $0A; { Norton Diskreet            }
  dtSuperStor  = $0B; { SuperStor                  }

function getDriveType(Drive : Byte) : Byte;

function countValidDrives : Byte;

implementation

function checkStacker( Drive : Byte ) : Boolean; near; assembler;

var
       StackerDriver : Pointer;
asm
       mov     word ptr StackerDriver,0
       mov     word ptr StackerDriver[2],0
       mov     ax,4404h
       mov     bl,Drive
       mov     cx,4
       lea     dx,StackerDriver
       push    ds
       push    ss
       pop     ds
       int     21h
       pop     ds
       mov     ax,word ptr StackerDriver
       or      ax,word ptr StackerDriver[2]
       jz      @@Q
       mov     al,1
@@Q:
end; { checkStacker }

function checkDiskreet( Drive : Byte ) : Boolean; near; assembler;
type
  TDiskreetPacket = record
    Header : array [1..6] of Byte;
    Drive  : Char;
    Size   : LongInt;
  end;
const
  DrvName  : PChar = '@DSKREET';
var
  Packet   : TDiskreetPacket;
asm
        push    ds
        push    es
        mov     ax,0FE00h
        mov     di,'NU'
        mov     si,'DC'
(* X-Man >>> *)
{$IFDEF DPMI}
       push    es
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       pushf
       push    0
       push    ax
       push    0
       push    cx
       push    0
       push    dx
       push    0
       push    bx
       push    0
       push    0
       push    0
       push    bp
       push    0
       push    si
       push    0
       push    di
       push    ss
       pop     es
       mov     di,sp
       xor     cx,cx
       mov     bx,2Fh
       mov     ax,0300h
       int     31h
       pop     di
       add     sp,2
       pop     si
       add     sp,2
       pop     bp
       add     sp,6
       pop     bx
       add     sp,2
       pop     dx
       add     sp,2
       pop     cx
       add     sp,2
       pop     ax
       add     sp,2
       popf
       add     sp,16
       pop     es
{$ELSE}
       int     2Fh
{$ENDIF}
(* X-Man <<< *)
        cmp     ax,$101
        ja      @@DSKReetNotFound
        lds     dx,DrvName
        mov     ax,3D02h
        int     21h
        jc      @@DSKReetNotFound
        mov     bx,ax
        push    ss
        push    ss
        pop     ds
        pop     es
        lea     dx,[Packet]
        mov     di,dx
        mov     cx,type TDiskreetPacket
        sub     al,al
        cld
        rep     stosb
        lea     di,[Packet.Header]
        mov     ax,12FFh
        stosw
        lea     di,[Packet.Drive]
        mov     al,Drive
        add     al,64
        stosb
        mov     ax,4403h
        mov     cx,7
        mov     si,'dc'
        mov     di,'NU'
        int     21h
        mov     ah,3Eh
        int     21h
        lea     si,[Packet.Size]
        lodsw
        or      ax,ax
        jnz     @@DSKReetFound
        lodsw
        or      ax,ax
        jz      @@DSKReetNotFound
@@DSKReetFound:
        mov     al,True
        jmp     @@Exit
@@DSKReetNotFound:
        mov     al,False
@@Exit:
        pop     es
        pop     ds
end; { checkDiskreet }

function checkSuperStor( Drive : Byte ) : Boolean; near; assembler;
type
  TSSPacket = record
    Sign  : Word;
    Sign1 : Word;
    P     : Pointer;
    Res   : array [1..4] of Byte;
  end;
var
  Packet : TSSPacket;
asm
        push    ds
        push    es

        push    ss
        push    ss
        pop     es
        pop     ds

        cld
        lea     di,[Packet]
        mov     dx,di
        mov     ax,0AA55h
        stosw
        mov     ax,0201h
        stosw
        xor     ax,ax
        stosw
        stosw
        stosw
        stosw

{        mov     dx,offset [Packet]}
        mov     ax,4404h
        mov     cx,type TSSPacket
        mov     bl,Drive
        int     21h
        jc      @@2
        lea     si,[Packet]
        lodsw
        or      ax,ax
        jnz     @@2
        lodsw
        cmp     ax,0201h
        jne     @@2
        les     di,[Packet.P]
        mov     ax,[es:di+5Dh]
        test    ax,40h
        jz      @@2
        mov     cl,byte ptr es:[di+24h]
        add     cl,'A'
        mov     ah,30h
        int     21h
        cmp     ah,4
        jb      @@1
        inc     di
@@1:
        les     di,dword ptr es:[di+5Fh]
        mov     bl,[es:di]
        add     bl,'A'
        cmp     cl,Drive
        jne     @@2
        mov     al,True
        jmp     @@3
@@2:
        sub     al,al
@@3:
        pop     es
        pop     ds
end; { checkSuperStor }



function getDriveType; assembler;

asm
       cmp     Drive,0
       jne     @@1
       mov     ah,19h
       int     21h
       mov     Drive,al
       inc     Drive
@@1:
       push    word ptr [Drive]
       call    checkDiskreet
       test    al,al
       jz      @@CDCheck
       mov     al,dtDiskreet
       jmp     @@Done
@@CDCheck:
       mov     ax,1500h
       sub     bx,bx
(* X-Man >>> *)
{$IFDEF DPMI}
       push    es
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       pushf
       push    0
       push    ax
       push    0
       push    cx
       push    0
       push    dx
       push    0
       push    bx
       push    0
       push    0
       push    0
       push    bp
       push    0
       push    si
       push    0
       push    di
       push    ss
       pop     es
       mov     di,sp
       xor     cx,cx
       mov     bx,2Fh
       mov     ax,0300h
       int     31h
       pop     di
       add     sp,2
       pop     si
       add     sp,2
       pop     bp
       add     sp,6
       pop     bx
       add     sp,2
       pop     dx
       add     sp,2
       pop     cx
       add     sp,2
       pop     ax
       add     sp,2
       popf
       add     sp,16
       pop     es
{$ELSE}
       int     2Fh
{$ENDIF}
(* X-Man <<< *)
       or      bx,bx
       jz      @@2
       mov     ax,150Bh
       sub     ch,ch
       mov     cl,Drive
       dec     cl
(* X-Man >>> *)
{$IFDEF DPMI}
       push    es
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       pushf
       push    0
       push    ax
       push    0
       push    cx
       push    0
       push    dx
       push    0
       push    bx
       push    0
       push    0
       push    0
       push    bp
       push    0
       push    si
       push    0
       push    di
       push    ss
       pop     es
       mov     di,sp
       xor     cx,cx
       mov     bx,2Fh
       mov     ax,0300h
       int     31h
       pop     di
       add     sp,2
       pop     si
       add     sp,2
       pop     bp
       add     sp,6
       pop     bx
       add     sp,2
       pop     dx
       add     sp,2
       pop     cx
       add     sp,2
       pop     ax
       add     sp,2
       popf
       add     sp,16
       pop     es
{$ELSE}
       int     2Fh
{$ENDIF}
(* X-Man <<< *)
       cmp     bx,0ADADh
       jne     @@2
       or      ax,ax
       jz      @@2
       mov     bl,dtCDROM
       jmp     @@Done
@@2:
       mov     ax,4409h
       mov     bl,Drive
       int     21h
       jc      @@s
       test    dh,80h
       jz      @@s
       mov     bl,dtSUBST
       jmp     @@Done
@@s:
       mov     ax,4A11h
       mov     bx,1
       mov     dl,Drive
       dec     dl
(* X-Man >>> *)
{$IFDEF DPMI}
       push    es
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       push    0
       pushf
       push    0
       push    ax
       push    0
       push    cx
       push    0
       push    dx
       push    0
       push    bx
       push    0
       push    0
       push    0
       push    bp
       push    0
       push    si
       push    0
       push    di
       push    ss
       pop     es
       mov     di,sp
       xor     cx,cx
       mov     bx,2Fh
       mov     ax,0300h
       int     31h
       pop     di
       add     sp,2
       pop     si
       add     sp,2
       pop     bp
       add     sp,6
       pop     bx
       add     sp,2
       pop     dx
       add     sp,2
       pop     cx
       add     sp,2
       pop     ax
       add     sp,2
       popf
       add     sp,16
       pop     es
{$ELSE}
       int     2Fh
{$ENDIF}
(* X-Man <<< *)
       or      ax,ax
       jnz     @@network
       cmp     dl,bl
       je      @@network
       test    bl,80h

       jz      @@SStorChk
       inc     dl
       cmp     Drive,dl
       jne     @@SStorChk
       mov     bl,dtDblSpace
       jmp     @@Done
@@SStorChk:
       push    word ptr [Drive]
       call    checkSuperStor
       test    al,al
       jz      @@network
       mov     al,dtSuperStor
       jmp     @@Done
@@network:
       mov     ax,4409h
       mov     bl,Drive
       int     21h
       jc      @@5
       and     dh,10h
       jz      @@4
       mov     bl,dtRemote
       jmp     @@Done
@@4:
       mov     al,Drive
@@goStac:
       push    ax
       call    checkStacker
       test    al,al
       jz      @@8
       mov     bl,dtStacker
       jmp     @@Done
@@8:
        mov     ax,4408h
        mov     bl,Drive
        int     21h
        jc      @@5
       test    al,al
       jz      @@Removable
(* X-Man >>> *)
{$IFDEF DPMI}
       push    es
       push    ss
       pop     es
       mov     ax,0100h
       mov     bx,3
       int     31h
       jc      @@allocfail
       push    dx
       mov     cx,25
       jmp     @@lp
@@allocfail:
       push    ds
       mov     si,sp
       stc
       jmp     @@cleanup
@@lp:  push    0
       loop    @@lp
       mov     di,sp
       mov     cx,440Dh
       mov     es:[di+1Ch],cx
       mov     cl,Drive
       mov     es:[di+10h],cl
       mov     cx,0860h
       mov     es:[di+18h],cx
       mov     es:[di+24h],ax
       mov     es:[di+04h],cx
       pushf
       pop     cx
       mov     es:[di+20h],cx
       mov     ax,0300h
       mov     bx,21h
       xor     cx,cx
       int     31h
       mov     cx,es:[di+20h]
       add     sp,50
       pop     dx
       pop     es
       push    ds
       mov     si,sp
       mov     ds,dx
       xor     dx,dx
       push    cx
       popf
{$ELSE}
       push    ds
       push    ss
       pop     ds
       mov     si,sp
       sub     sp,28h
       mov     dx,sp
       mov     ax,440Dh
       mov     cx,860h
       int     21h
{$ENDIF}
(* X-Man <<< *)
       jc      @@cleanup
       pushf
       mov     di,dx
       cmp     byte ptr ds:[di+6],0F8h
       jz      @@dubldsk
       popf
       jmp     @@cleanup
@@dubldsk:
       popf
       mov     bl,dtDublDisk
       mov     sp,si
(* X-Man >>> *)
{$IFDEF DPMI}
       mov     dx,ds
{$ENDIF}
       pop     ds
{$IFDEF DPMI}
       mov     ax,0101h
       int     31h
{$ENDIF}
       jmp     @@Done
@@cleanup:
       mov     sp,si
{$IFDEF DPMI}
       mov     dx,ds
{$ENDIF}
       pop     ds
{$IFDEF DPMI}
       pushf
       mov     ax,0101h
       int     31h
       popf
{$ENDIF}
(* X-Man <<< *)
       mov     bl,dtRAMDrive
       jc      @@Done
@@fixed:
        mov     bl,dtFixed
        jmp     @@Done
@@5:
       sub     bl,bl
       jmp     @@Done
@@Removable:
       mov     bl,dtRemovable
@@Done:
       mov     al,bl
end; { getDriveType }

function countValidDrives;
 var i : Byte;
     C : Byte;
 begin
  C := 0;
  for i := 1 to 26 do
   if GetDriveType(i) <> dtError then
    Inc(C);
  countValidDrives := C;
 end;

end.
