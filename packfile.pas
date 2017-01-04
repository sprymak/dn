{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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

(* Pack/Expand file which allow Read/Seek (use Pack_RS unit) *)
(* Autor: Anton Fedorov AKA DataCompBoy (DN OSP v1.51.05) *)
(* Debugger: Sergey Korshunoff AKA SeYKo *)

program PackFile;
uses
  Pack_LZW, LFN;

Type
  TSeekArray = Array[0..1] of LongInt;
  PSeekArray = ^TSeekArray;

  TBytes = Array[0..1] of byte;
  PBytes = ^TBytes;
const
  PackedSig:  String[12] = 'Packed file'#26;
  Vers:       Word = 1;
  PackFlag:   Boolean = False;

  PageSiz: Word = 6600;
  ToSize:  Word = 0;

  FromBuf:  PBytes     = nil;
  ToBuf:    PBytes     = nil;
  SeekArr:  PSeekArray = nil;

const
  AddOnS = 4*2 + SizeOf(PackedSig);

var
  InpF, OutF: lFile;
  FS, AllocedPages: LongInt;
  ReaderCount, WriterCount: LongInt;
  I, J, C: Word;
  W, CP, CP2, LPS: Word;
  S: String;

procedure Usage;
begin
  WriteLn('PackFile v0.2: make packed file which allow read/seek');
  WriteLn('Autor: Anton Fedorov AKA DataCompBoy (DN OSP v1.51.05)');
  WriteLn('Debugger: Sergey Korshunoff AKA SeYKo');
  WriteLn('Usage:');
  WriteLn('  packfile [-x] [-pN] IN_FILE OUT_FILE');
  WriteLn('where');
  WriteLn('  -x   -- expand packed file');
  WriteLn('  -pN  -- set a packed file page size to N');
  Halt(2);
end;

function WriterBuf(var Buf; BufSize: word): Integer;
var
  Cnt: Word;
begin
  BlockWrite(OutF.F, Buf, BufSize, Cnt);
  Inc(WriterCount,Cnt);
  WriterBuf:=Cnt;
end;

begin
  if ParamCount=0 then Usage;
  PackFlag:=True;
  C:=1; S:=ParamStr(C);
  if S='-x' then begin PackFlag:=False; Inc(C); end else
  if Copy(S,1,2)='-p' then begin Val(Copy(S,3,255),PageSiz,I); Inc(C); end;
  if (ParamCount<>C+1) or (PageSiz=0) then Usage;
  lAssignFile(InpF, ParamStr(C)); lResetFile(InpF,1);
  if InOutRes<>0 then begin
    WriteLn('Can not open input file:', ParamStr(C),
      ' (error code', InOutRes,')');
    Close(InpF.F); Halt(2);
  end;
  FS:=FileSize(InpF.F); S:='';
  if FS>AddOnS then begin
    Seek(InpF.F, FS-AddOnS); BlockRead(InpF.F, S, SizeOf(PackedSig));
  end;
  if PackFlag and (S=PackedSig) then begin
    WriteLn(ParamStr(C),': file is already packed');
    BlockRead(InpF.F, W, SizeOf(W));
    if W<>0 then WriteLn('Unknown version of file: ',W)
    else begin
      WriteLn('Version of file: ',W);
      BlockRead(InpF.F, W, SizeOf(W)); WriteLn('Pages: ',W);
      BlockRead(InpF.F, W, SizeOf(W)); WriteLn('Page Size: ',W);
      BlockRead(InpF.F, W, SizeOf(W)); WriteLn('Last Page Size: ',W);
    end;
    Close(InpF.F); Halt(1);
  end;
  if not PackFlag then
  begin
    if S<>PackedSig then begin
      WriteLn(ParamStr(C),': file is NOT packed');
      Close(InpF.F); Halt(1);
    end;
    WriteLn('Expanding file ', ParamStr(C),' to ', ParamStr(C+1));
    BlockRead(InpF.F, W, SizeOf(W)); WriteLn('Version of file: ',W);
    if W<>Vers then begin
      WriteLn('Unknown file format'); Close(InpF.F); Halt(1);
    end;
    BlockRead(InpF.F, CP, SizeOf(CP)); WriteLn('Pages: ',CP);
    if CP=0 then begin
      WriteLn('Bad file format'); Close(InpF.F); Halt(1);
    end;
    BlockRead(InpF.F, PageSiz, SizeOf(PageSiz));
    WriteLn('Page Size: ',PageSiz);
    if PageSiz=0 then begin
      WriteLn('Bad file format'); Close(InpF.F); Halt(1);
    end;
    BlockRead(InpF.F, LPS, SizeOf(LPS)); WriteLn('Last Page Size: ',LPS);
    AllocedPages:=CP+1;
  end else begin
    AllocedPages:=FS div PageSiz;
    If AllocedPages=0 then AllocedPages := 1;
  end;
  Seek(InpF.F,0);
  if (AllocedPages>65500) or (AllocedPages*SizeOf(LongInt)>65500) then
  begin
    WriteLn('Input file is to big: ', ParamStr(C));
    Close(InpF.F); Halt(2);
  end;
  lAssignFile(OutF, ParamStr(C+1)); lRewriteFile(OutF,1);
  if InOutRes<>0 then
  begin
    WriteLn('Can not create output file:', ParamStr(C+1),
      ' (error code', InOutRes,')');
    Close(InpF.F); Close(OutF.F); Halt(2);
  end;
  LZW_Init;
  GetMem(FromBuf, 2*PageSiz); GetMem(ToBuf, 2*PageSiz);
  GetMem(SeekArr, AllocedPages * SizeOf(LongInt));
  W:=0;
  if PackFlag then
  begin
    ReaderCount:=0; WriterCount:=0; CP:=0;
    WriteLn('Packing file ', ParamStr(C),' to ', ParamStr(C+1));
    repeat
       BlockRead(InpF.F,FromBuf^,PageSiz,I);
       if I>0 then
       begin
         Inc(ReaderCount,I); LPS:=I;
         J:=LZW_Compress(FromBuf^, ToBuf^[1], I);
         if J=$FFFF then begin
          ToBuf^[0]:=1;
          Move(FromBuf^, ToBuf^[1], I);
          J := I;
         end else ToBuf^[0]:=0;
         inc(J);
         if J>PageSiz+1 then begin
           WriteLn(#13,'Error: compressed block size ',J,' > PageSize (',
             PageSiz,')');
           W:=2; break;
         end;
         BlockWrite(OutF.F,ToBuf^,J ,I);
         if I<>J then begin
           WriteLn(#13,'Error: can not write to file ',ParamStr(C+1));
           W:=2; break;
         end;
         SeekArr^[CP]:=WriterCount; Inc(CP);
         Inc(WriterCount,J);
         if ReaderCount<>0 then Write(#13,ReaderCount, ' -> ', WriterCount,
           ' = ', ((WriterCount/ReaderCount)*100):2:2, '%');
       end else break;
    until EOF(InpF.F);
    if not EOF(InpF.F) then W:=2;
    if W<>0 then begin
      WriteLn(#13,'Output file is bad: ', ParamStr(C+1))
    end else
    begin
      WriteLn;
      SeekArr^[CP]:=WriterCount;
      WriterBuf(SeekArr^,  SizeOf(LongInt)*(CP+1));
      WriterBuf(PackedSig, SizeOf(PackedSig));
      WriterBuf(Vers,      SizeOf(Vers));
      WriterBuf(CP,        SizeOf(CP));
      WriterBuf(PageSiz,   SizeOf(PageSiz));
      WriterBuf(LPS,       SizeOf(LPS));
      Writeln('Pages: ',CP);
      Writeln('Page size: ',PageSiz);
      Writeln('Last page size: ',LPS);
    end;
  end else
  begin
    Seek(InpF.F,FS-AddOnS-SizeOf(LongInt)*(CP+1));
    BlockRead(InpF.F,SeekArr^,SizeOf(LongInt)*(CP+1));
    Seek(InpF.F,0);
    ReaderCount:=0; WriterCount:=0; CP2:=0;
    while CP2 < CP do
    begin
       J:=SeekArr^[CP2+1]-SeekArr^[CP2];
       BlockRead(InpF.F,FromBuf^,J,I);
       if I>0 then
       begin
         Inc(ReaderCount,I);
         if FromBuf^[0]=0 then J:=LZW_Decompress(FromBuf^[1], ToBuf^)
         else begin
          Move(FromBuf^[1], ToBuf^, I-1);
          J:=I-1;
         end;
         if ((CP2<CP-1) and (J<>PageSiz)) or
            ((CP2=CP-1) and (J<>LPS))
         then begin
           WriteLn(#13,'Error: compressed block size ',J,' is bad');
           W:=2; break;
         end;
         BlockWrite(OutF.F,ToBuf^,J ,I);
         if I<>J then begin
           WriteLn(#13,'Error: can not write to file ',ParamStr(C+1));
           W:=2; break;
         end;
         Inc(CP2); Inc(WriterCount,J);
         if ReaderCount<>0 then Write(#13,ReaderCount, ' -> ', WriterCount,
           ' = ', ((WriterCount/ReaderCount)*100):2:2, '%');
       end else break;
    end;
    WriteLn;
    if InOutRes<>0 then W:=2;
    if W<>0 then WriteLn('Output file is bad: ', ParamStr(C+1)) else
                 WriteLn('Output file is OK');
  end;
  LZW_Done;
  FreeMem(FromBuf, 2*PageSiz); FreeMem(ToBuf, 2*PageSiz);
  FreeMem(SeekArr, AllocedPages * SizeOf(LongInt));
  Close(InpF.F); Close(outF.F);
  Halt(W);
end.
