{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.09
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
Unit TitleSet;
Interface

 Procedure SetTitle(Text: String);

Implementation
{$IFDEF NONBP}Procedure SetTitle(Text: String); begin end;{$ELSE}
 uses Advance {$IFDEF DPMI}, DPMI, DosMem{$ENDIF}, Dos;

const DNTITL: String[3] = 'DN'#0;
{$IFDEF DPMI}
var r: DPMIRegisters;
    DosB: pointer;
{$ELSE}
var r: registers;
{$ENDIF}

 Procedure SetTitle(Text: String);
  begin
      {Setting Win9x application title}
   Text:=Advance.FormatLongName(Text,79,0,flnUseCutChar,nfmNull,nil)+#0;
   r.ax:=$168E;
   r.dx:=$0000;
{$IFDEF DPMI}
   DosB:=GetDosMem(Length(Text), r.es);
   if r.es=0 then exit;
   r.di:=0;
   move(Text[1], DosB^, length(Text));
   SimulateRealModeInt($2F,r);
   FreeDosMem(DosB);
{$ELSE}
   r.es:=Seg(Text[1]);
   r.di:=Ofs(Text[1]);
   intr($2F,r);
{$ENDIF}
  end;

var OldOS2Title: array[0..12] of byte;
    OldMDTitle: array[0..30] of byte;
    oep: pointer;
procedure exp; {$IFDEF BIT_16}far;{$ENDIF}
begin
 ExitProc:=oep;
{$IFDEF DPMI}
 DosB:=GetDosMem(30, r.es);
 if r.es=0 then exit;
 r.di:=0;
{$ENDIF}
      {Restoring old OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0001;
 r.CX:=$636C;
 r.BX:=$0000;
{$IFNDEF DPMI}
 r.ES:=Seg(OldOS2Title);
 r.DI:=Ofs(OldOS2Title);
 intr($21, r);
{$ELSE}
 move(OldOS2Title, DosB^, 12);
 SimulateRealModeInt($21,r);
{$ENDIF}
      {Restoring old Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0001;
{$IFNDEF DPMI}
 r.es:=Seg(OldMDTitle);
 r.di:=Ofs(OldMDTitle);
 intr($2F,r);
{$ELSE}
 move(OldMDTitle, DosB^, 30);
 SimulateRealModeInt($2F,r);
{$ENDIF}

{$IFDEF DPMI}
 FreeDosMem(DosB);
{$ENDIF}
end;

begin
{$IFDEF DPMI}
 DosB:=GetDosMem(30, r.es);
 if r.es=0 then exit;
 r.di:=0;
{$ENDIF}

      {Getting old OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0002;
 r.CX:=$636C;
 r.BX:=$0000;
{$IFNDEF DPMI}
 r.ES:=Seg(OldOS2Title);
 r.DI:=Ofs(OldOS2Title);
 intr($21, r);
{$ELSE}
 SimulateRealModeInt($21,r);
 move(DosB^, OldOS2Title, 12);
{$ENDIF}

      {Setting new OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0001;
 r.CX:=$636C;
 r.BX:=$0000;
{$IFDEF DPMI}
 move(DNTITL[1], DosB^, Length(DNTITL));
 SimulateRealModeInt($21,r);
{$ELSE}
 r.ES:=Seg(DNTITL[1]);
 r.DI:=Ofs(DNTITL[1]);
 intr($21, r);
{$ENDIF}

      {Getting old Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0003;
 r.cx:=SizeOf(OldMDTitle);
{$IFNDEF DPMI}
 r.es:=Seg(OldMDTitle);
 r.di:=Ofs(OldMDTitle);
 intr($2F,r);
{$ELSE}
 SimulateRealModeInt($2F,r);
 move(DosB^, OldMDTitle, 30);
{$ENDIF}

      {Setting new Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0001;
{$IFDEF DPMI}
 move(DNTITL[1], DosB^, Length(DNTITL));
 SimulateRealModeInt($2F,r);
{$ELSE}
 r.es:=Seg(DNTITL[1]);
 r.di:=Ofs(DNTITL[1]);
 intr($2F,r);
{$ENDIF}

{$IFDEF DPMI}
 FreeDosMem(DosB);
{$ENDIF}

 oep:=ExitProc;
 ExitProc:=@exp;
{$ENDIF NONBP}
end.
