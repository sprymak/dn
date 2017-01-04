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

{$S-}
UNIT Drivers2;

{-----------------------------------------------------}
{ This module is based on Turbo Vision Drivers Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

INTERFACE

procedure DoDump(Er: byte; ErrAdd: Pointer);

IMPLEMENTATION
Uses Dos, Advance, LFN, {$IFNDEF NONBP}StakDump, {$ENDIF}Advance1, Drivers
     {$IFDEF VIRTUALPASCAL}, SysUtils{$ENDIF}
     ;

{$S-}
procedure DoDump(Er: byte; ErrAdd: Pointer);
{$IFNDEF DNPRG}begin{$ELSE}
type PByteArray = ^TByteArray;
     TByteArray = array [0..65528] of byte;
var
  C,AC,PP,FH : Word;
  {!!!!!}
  PPP: TByteArray absolute FreeStr;
  PhysAddr : pointer;

  I : Byte;
  PExit, ActiveButton: Byte;

  Function StoreBuffer(var Buf; Size:word ; FH : word ):word;
    {$IFDEF VIRTUALPASCAL}
  begin
   StoreBuffer := FileWrite(FH, Buf, Size);
  end;
    {$ELSE}
    assembler;
    asm
       mov  bx,FH
       push ds
        mov cx,Size
          mov  ah,40h
          sub  al,al
          lds  dx,Buf
          int  21h
          mov  ax,0
          jnc  @@1
          inc  ax
         @@1:
       pop  ds
    end;
    {$ENDIF}

begin
    { Create Report File. DRIVERS.PAS contains full dupe of this routine }

    I := Byte(SourceDir[0]);
    SourceDir := SourceDir + 'DN.ERR'{$IFNDEF VIRTUALPASCAL}+#0{$ENDIF};

    {$IFDEF VIRTUALPASCAL}
    FH := FileOpen(SourceDir, fmOpenWrite);
    if FH<0 then FH := FileCreate(SourceDir);
    if FH>=0 then FileSeek(FH, 0, 2);
    {$ELSE}
    FH := Word(-1) ;
 asm
    push ds
    push bp

    mov ax,6C00h { extended dos 4+ open  }
    mov si,offset SourceDir
    inc si
    mov cx,0
    mov dx,11h   { if exist open, if not create and open }
    mov bx,7042h { 42 file mode ; do not use int 24 , write
                   w/o buffering }
    int 21h
    jnc @@1
    jmp @@99     { other error - exit }
@@1:
    mov bx,ax
    mov ax,4202H { lseek to e.nd of file }
    sub dx,dx
    sub cx,cx
    int 21h
    jnc @@2
    jmp @@99      { other error - exit }
@@2:
    mov FH,bx    { save handle        }
@@99:
    pop bp
    pop ds
 end;
   {$ENDIF}

 if FH<>Word(-1) then
   begin
     {$IFNDEF NONBP}
     AssignOutput(FH);
     {$ENDIF}
     FreeStr :=
          ^M^J +
       '----<'+ GetDateTime(false) +' '+ GetDateTime(true)+'>'^M^J +
       'VER :' + VersionName  + ^M^J +
       'DATE:' + VersionDate  + ^M^J +
       'ERR :' + Hex2(Er)     + ^M^J +
       'ADDR:' + Hex8(LongInt(ErrAdd)) + ^M^J ;
     StoreBuffer( FreeStr[1] , length(FreeStr), FH );

     FreeStr :=
       {$IFNDEF NONBP}
       'PSP :' + Hex4( PrefixSeg ) + ^M^J +
       {$ENDIF}
       'CS  :' + Hex4(CSeg) + ^M^J +
       'DS  :' + Hex4(DSeg) + ^M^J +
       'SS  :' + Hex4(SSeg) + ^M^J +
       'SP  :' + Hex4(SPtr) + ^M^J +
       'MEMm:' + Hex8(MaxAvail) + ^M^J +
       'MEMa:' + Hex8(MemAvail) + ^M^J +
{$IFNDEF DPMI}{$IFNDEF NONBP}
       'hOrg:' + Hex8(Longint(HeapOrg)) + ^M^J +
       'hEnd:' + Hex8(Longint(HeapEnd)) + ^M^J +
       'hPtr:' + Hex8(Longint(HeapPtr)) + ^M^J +
       'hLim:' + Hex4( StackLimit ) + ^M^J +
{$ENDIF}{$ENDIF}
       '';
     StoreBuffer( FreeStr[1] , length(FreeStr), FH );
{$IFNDEF NONBP}
     FreeStr :=
       'STCK:' + ^M^J;
     StoreBuffer( FreeStr[1] , length(FreeStr), FH );
(*   if ErrAdd<>nil
      then begin
            {$IFDEF DPMI}
            PhysAddr := FindPhysAddr(ErrAdd);
            {$ELSE}
            PhysAddr := Ptr(OS(ErrAdd).S+PrefixSeg+$10, OS(ErrAdd).O);
            {$ENDIF}
            if PhysAddr <> nil then
             TraceStack(PhysAddr)
           end
      else *)DumpStack;
{$ENDIF}
     FreeStr :=
       'SCR :' + Hex8(Longint(ScreenBuffer)) + ^M^J ;
     StoreBuffer( FreeStr[1] , length(FreeStr), FH );

     for C:=0 to Pred(ScreenHeight) do
      begin
          TempFile:='';
          PP := (ScreenWidth*C) shl 1;
          for AC := 0 to Pred(ScreenWidth) do Begin
           PPP[AC] :=  PByteArray(ScreenBuffer)^[PP+AC*2];
           TempFile:=TempFile+Hex2(PByteArray(ScreenBuffer)^[PP+AC*2+1]);
          End;
          TempFile := TempFile+#13#10 ;
          if StoreBuffer(PPP,         ScreenWidth,      FH) <> 0 then Break;
          if StoreBuffer(TempFile[1], Length(TempFile), FH) <> 0 then Break;
      end; { for lines }
     {$IFDEF VIRTUALPASCAL}
     FileClose(FH);
     {$ELSE}
     asm
       mov  ah,3Eh     { close file }
       mov  bx,FH
       int  21h
     end
     {$ENDIF}
   end; { file write ok }

  SourceDir[0] := Char(I);
{$ENDIF DNPRG}
end;
{$S+}

END.
