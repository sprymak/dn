{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{ --------------------------------------------------------------------------- }
{ DRVTYPES.PAS  Drive Type Decetion.                              Version 1.0 }
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
  dtBernoully  = $0A; { IOmega Bernoully           }
  dtDiskreet   = $0B; { Norton Diskreet            }
  dtSuperStor  = $0C; { SuperStor                  }

const
  sdtError      = 'Unknown   ';
  sdtFixed      = 'Fixed     ';
  sdtRemovable  = 'Removable ';
  sdtRemote     = 'Remote    ';
  sdtCDROM      = 'CD-ROM    ';
  sdtDblSpace   = 'DblSpace  ';
  sdtSUBST      = 'SUBST     ';
  sdtStacker    = 'Stacker   ';
  sdtRAMDrive   = 'RAM       ';
  sdtDublDisk   = 'DoubleDisk';
  sdtBernoully  = 'Bernoully ';
  sdtDiskreet   = 'Diskreet  ';
  sdtSuperStor  = 'SuperStor ';

function getDriveType(Drive : Byte) : Byte;

function countValidDrives : Byte;

implementation
{$IFDEF NONBP}
function getDriveType(Drive : Byte) : Byte;
begin getDriveType:=dtError end;

function countValidDrives : Byte;
begin countValidDrives:=0 end;

{$ELSE NONBP}

function checkStacker( Drive : Byte ) : Boolean; assembler;

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
        mov     ax,0FE00h
        mov     di,'NU'
        mov     si,'DC'
        int     2Fh
        or      al,al
        je      @@2
        cmp     al,1
        je      @@2
@@1:
        sub     al,al
        jmp     @@4
@@2:
        lds     dx,DrvName
        mov     ax,3D02h
        int     21h
        jc      @@1
        mov     bx,ax
        mov     ax,seg [Packet]
        mov     ds,ax
        mov     dx,offset [Packet]
        mov     es,ax
        mov     di,dx
        mov     cx,type TDiskreetPacket
        sub     al,al
        cld
        rep     stosb
        mov     di,offset [Packet.Header]
        mov     ax,12FFh
        stosw
        mov     di,offset [Packet.Drive]
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
        mov     si,offset [Packet.Size]
        lodsw
        or      ax,ax
        jnz     @@3
        lodsw
        or      ax,ax
        jz      @@1
@@3:
        mov     al,True
@@4:
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
        mov     ax,seg [Packet]
        mov     es,ax
        mov     di,offset [Packet]
        mov     cx,type TSSPacket
        cld
        rep     stosb
        mov     di,offset [Packet.Sign]
        mov     ax,0AA55h
        stosw
        mov     ax,0201h
        stosw
        mov     ax,4404h
        mov     dx,seg [Packet]
        mov     ds,dx
        mov     dx,offset [Packet]
        mov     cx,12
        mov     bl,Drive
        int     21h
        jc      @@2
        mov     si,offset [Packet.Sign]
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
       int     2Fh
       or      bx,bx
       jz      @@2
       mov     ax,150Bh
       sub     ch,ch
       mov     cl,Drive
       dec     cl
       int     2Fh
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
       int     2Fh
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
       jz      @@check_Bernoulli
       push    ds
       push    ss
       pop     ds
        mov     si,sp
        sub     sp,28h
        mov     dx,sp
        mov     ax,440Dh
        mov     cx,860h
        int     21h
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
       pop     ds
       jmp     @@Done
@@cleanup:
        mov     sp,si
        pop     ds
        mov     bl,dtRAMDrive
        jc      @@Done
       jmp     @@fixed
@@check_Bernoulli:
       cmp     Drive,2
       jbe     @@6
       push    ds
       mov     ah,1Ch
       mov     dl,Drive
       int     21h
       cmp     byte ptr ds:[bx],0FDh
       pop     ds
       jnz     @@6
       push    ds
       mov     ah,32h
       mov     dl,Drive
       int     21h
       cmp     byte ptr ds:[bx+0Bh],2
       pop     ds
       jz      @@6
       mov     bl,dtBernoully
       jmp     @@Done
@@fixed:
        mov     bl,dtFixed
        jmp     @@Done
@@5:
       sub     bl,bl
       jmp     @@Done
@@6:
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

{$ENDIF}

end.
