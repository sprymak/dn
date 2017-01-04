{ÚÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂ¿}
{ÃÅÅ ˆ§¬¥­¥­¨¥ § £®«®¢ª  ¯p®æ¥áá  ¤«ï ®â®¡p ¦¥­¨ï á®áâ®ï­¨ï „H' ! ÅÅ´}
{ÃÅÅ  €¢â®p: €­â®­ ”¥¤®p®¢ aka DataCompBoy                        ÅÅ´}
{ÃÅÅ  ‚ ï« ¢ ç¥áâì Œ â pëª¨­®© “«ìï­ë...                          ÅÅ´}
{ÀÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÙ}
Unit TitleSet;
Interface
 uses Dos;

 Procedure SetTitle(Text: String);

Implementation
const DNTITL: String = 'DN'#0;
var r: registers;

 Procedure SetTitle(Text: String);
  var r: registers;
  begin
      {Setting Win9x application title}
   Text:=Copy(Text,1,79)+#0;
   r.ax:=$168E;
   r.dx:=$0000;
   r.es:=Seg(Text[1]);
   r.di:=Ofs(Text[1]);
   intr($2F,r);
  end;

var OldOS2Title: array[0..12] of byte;
    OldMDTitle: array[0..30] of byte;
    oep: pointer;
procedure exp; far;
begin
 ExitProc:=oep;
      {Restoring old OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0001;
 r.CX:=$636C;
 r.BX:=$0000;
 r.ES:=Seg(OldOS2Title);
 r.DI:=Ofs(OldOS2Title);
 intr($21, r);
      {Restoring old Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0001;
 r.es:=Seg(OldMDTitle);
 r.di:=Ofs(OldMDTitle);
 intr($2F,r);
end;

begin
      {Getting old OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0002;
 r.CX:=$636C;
 r.BX:=$0000;
 r.ES:=Seg(OldOS2Title);
 r.DI:=Ofs(OldOS2Title);
 intr($21, r);
      {Setting new OS/2 session title}
 r.ax:=$6400;
 r.DX:=$0001;
 r.CX:=$636C;
 r.BX:=$0000;
 r.ES:=Seg(DNTITL[1]);
 r.DI:=Ofs(DNTITL[1]);
 intr($21, r);
      {Getting old Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0003;
 r.cx:=SizeOf(OldMDTitle);
 r.es:=Seg(OldMDTitle);
 r.di:=Ofs(OldMDTitle);
 intr($2F,r);
      {Setting new Win9x virtual machine title}
 r.ax:=$168E;
 r.dx:=$0001;
 r.es:=Seg(DNTITL[1]);
 r.di:=Ofs(DNTITL[1]);
 intr($2F,r);

 oep:=ExitProc;
 ExitProc:=@exp;
end.