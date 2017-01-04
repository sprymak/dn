{ÚÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂ¿}
{ÃÅÅ p®£p ¬¬  y¯ ª®¢ª¨ ä ©«®¢, ¤«ï ¯®á« p ¡®âë á ­¨¬¨ ç¥p¥§ RSPFile ÅÅ´}
{ÃÅÅ  €¢â®p: €­â®­ ”¥¤®p®¢ aka DataCompBoy                           ÅÅ´}
{ÃÅÅ  ‚ ï« ¢ ç¥áâì Œ â pëª¨­®© “«ìï­ë...                             ÅÅ´}
{ÀÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÙ}
Program PFile; {$I-}
uses LZW4p, RW_IO;

Type TPR = Record LcPos: longint end;
     Poss = array[0..1000] of TPR;

var GlPoss: Poss;
    CP: Word;
    PsBuf: Word;
    pp: longint;
    lps:word;
const PageSiz: Word = 32767;

Const C: Word = 0;

function RP : Integer;
begin
 if C<PageSiz then
  begin
   RP:=Reader;
   inc(C);
  end
 else RP:=-1;
end;

function WP(TheByte : Byte) : Integer;
begin
 WP:=Writer(TheByte);
end;

const Sig: Word = $0DCBF;
var   fp: longint;
begin
 if ParamCount<>2 then halt(1);
 if ReaderOpen(ParamStr(1))<>0 then Halt(1);
 if WriterOpen(ParamStr(2))<>0 then halt(1);
 CP:=0;
 Repeat
  with GlPoss[CP] do
   begin
    Fp:=ReaderCount;
    pp:=fp;
    LcPos:=WriterCount;
    if FP<>0 then
     Write(#13, FP, ' -> ', LcPos, ' = ', ((LcPos/FP)*100):2:2, '%')
    else
     Write(#13, FP, ' -> ', LcPos);
   end;
  C:=0;
  InitLZW(@LZAllocMemory, 14);
  Compress(@RP, @WP);
  TermLZW(@LZFreeMemory);
  Inc(CP);
 Until ReaderEnd;
 lps:=ReaderCount-pp;
 GlPoss[CP].LcPos:=WriterCount;
 WriterBuf(GlPoss, SizeOf(TPR)*(CP+1));
 WriterBuf(Sig,     SizeOf(Sig));
 WriterBuf(CP,      SizeOf(CP));
 WriterBuf(PageSiz, SizeOf(PageSiz));
 WriterBuf(lps,     SizeOf(lps));
 Writeln(#13, ReaderCount, ' -> ', WriterCount,
         ' = ', ((WriterCount/ReaderCount)*100):2:2, '%');
 ReaderClose;
 WriterClose;
end.