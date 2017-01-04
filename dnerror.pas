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
//
//////////////////////////////////////////////////////////////////////////}

{$i-}{$m 8192, 102400, 655360}{$g+}
program DnError;

uses Search, KeyBoard, MapEngn, Dos, Crt;

var Err, TS: Text;
    F: File;
    HasMap: Boolean;
    s, s2: string;
    FFC: String[5];
    c: char;
    R: Registers;
    ERC: Word;
    IS: Byte;
    i: integer;

begin
 Writeln(' DNError v2.5 (c) Anton Fedorov aka DataCompBoy 1999-2000');
 Assign(Err, 'DN.ERR');
 Reset(Err);
 if IOResult<>0 then begin
  Writeln('Nothing to do - DN.ERR not found.');
  halt;
 end;
 Assign(TS, 'DN2SEND.ERR');
 Append(TS);
 If IOResult<>0 then Rewrite(TS);
 if IOResult<>0 then begin
  writeln('Can''t create DN2SEND.ERR');
  halt(1);
 end;

 HasMap := LoadMap('DN.MPP');
 if not HasMap then begin
  Assign(F, 'DN.MAP');
  Reset(F);
  if IOResult = 0 then begin
   Close(F);
   writeln('Found uncompiled DN.MAP. Recommend to compile them before run DNError.');
  end else
   writeln('DN.MPP not found! Recommend to put them to current directory.');
  write('Continue (y/N) ?');
  C:=ReadKey;
  Case C of
   'y', 'Y':  Writeln('Y');
   else begin Writeln('N'); halt end;
  end;
 end;

 s2:='='; S:='=';
 While (Not EOF(ERR)) or (IS>0) do begin
  S2:=S;
  if not EOF(Err) then ReadLn(ERR, S) else s:='';
  FFC:=Copy(S, 1, 5);
  if IS>0 then begin
   if (s<>'') and (FFC<>'DESC:') then begin
    if length(S)<80
     then Writeln(S)
     else if Length(S)=80
           then Write(S)
           else begin
            s2:='';
            For i:=1 to 80 do s2:=s2+s[i]+Chr(FromHex(Copy(s, ((i-1)*2)+81, 2)));
            s2:=s2+#0;
            r.ax:=$1303;
            r.bx:=0;
            r.cx:=80;
            r.dh:=WhereY-1;
            r.dl:=WhereX-1;
            r.es:=Seg(s2);
            r.bp:=Ofs(s2)+1;
            intr($10,r);
           end;
    inc(IS);
   end else begin
    IS:=0;
    if FFC<>'DESC:' then begin
     write(' Enter description of problem:');
     readln(S);
     S:='DESC:'+S;
    end;
   end;
  end else if FFC='ERR :' then begin
   ERC:=FromHex(Copy(S, 6, 2));
   case ERC of
      0 : S:=S + ' State dump';
      1 : S:=S + ' Invalid function number ';
      2 : S:=S + ' File not found ';
      3 : S:=S + ' Path not found ';
      4 : S:=S + ' Too many open files ';
      5 : S:=S + ' File access denied ';
      6 : S:=S + ' Invalid file handle ';
     12 : S:=S + ' Invalid file access code ';
     15 : S:=S + ' Invalid drive number ';
     16 : S:=S + ' Cannot remove current directory ';
     17 : S:=S + ' Cannot rename across drives ';
     18 : S:=S + ' No more files ';
    100 : S:=S + ' Disk read error ';
    101 : S:=S + ' Disk write error ';
    102 : S:=S + ' File not assigned ';
    103 : S:=S + ' File not open ';
    104 : S:=S + ' File not open for input ';
    105 : S:=S + ' File not open for output ';
    106 : S:=S + ' Invalid numeric format ';
    150 : S:=S + ' Disk is write-protected ';
    151 : S:=S + ' Bad drive request struct length ';
    152 : S:=S + ' Drive not ready ';
    154 : S:=S + ' CRC error in data ';
    156 : S:=S + ' Disk seek error ';
    157 : S:=S + ' Unknown media type ';
    158 : S:=S + ' Sector Not Found ';
    159 : S:=S + ' Printer out of paper ';
    160 : S:=S + ' Device write fault ';
    161 : S:=S + ' Device read fault ';
    162 : S:=S + ' Hardware failure ';
    200 : S:=S + ' Division by zero ';
    201 : S:=S + ' Range check error ';
    202 : S:=S + ' Stack overflow error ';
    203 : S:=S + ' Heap overflow error ';
    204 : S:=S + ' Invalid pointer operation ';
    205 : S:=S + ' Floating point overflow ';
    206 : S:=S + ' Floating point underflow ';
    207 : S:=S + ' Invalid floating point operation ';
    208 : S:=S + ' Overlay manager not installed ';
    209 : S:=S + ' Overlay file read error ';
    210 : S:=S + ' Object not initialized ';
    211 : S:=S + ' Call to abstract method ';
    212 : S:=S + ' Stream registration error ';
    213 : S:=S + ' Collection index out of range ';
    214 : S:=S + ' Collection overflow error ';
    215 : S:=S + ' Arithmetic overflow error ';
    216 : S:=S + ' General Protection fault ';
    219 : S:=S + ' Resource access error ';
    221 : S:=S + ' Navigator Link error ';
    $E8 : S:=S + ' TBigArray: GetMem failed ';
    $E9 : S:=S + ' TBigArray: Stream error ';
    $EA : S:=S + ' TBigArray: Nil pointer ';
    $EB : S:=S + ' TBigArray: Count2 out of range ';
    $EC : S:=S + ' TBigArray: Count1 out of range ';
    $ED : S:=S + ' TBigArray: Index2 out of range ';
    $EE : S:=S + ' TBigArray: Index1 out of range ';
    $EF : S:=S + ' TBigArray: Index out of range ';
    $F0 : S:=S + ' TBigArray: no error ';
    $F7 : S:=S + ' TArray: GetMem failed ';
    $F8 : S:=S + ' TArray: Stream error ';
    $F9 : S:=S + ' TArray: Nil pointer ';
    $FA : S:=S + ' TArray: Count2 out of range ';
    $FB : S:=S + ' TArray: Count1 out of range ';
    $FC : S:=S + ' TArray: Index2 out of range ';
    $FD : S:=S + ' TArray: Index1 out of range ';
    $FE : S:=S + ' TArray: Index out of range ';
    $FF : S:=S + ' TArray: no error ';
    else  S:=S + ' Unknown error ';
   end;
  end else if FFC='ADDR:' then begin
     If HasMap
      then S := Copy(S, 1, 14) + GetInfo(FromHex(Copy(S, 6, 4)),
                                         FromHex(Copy(S, 10,4)));
  end else if (FFC[1] in hexch) and
              (FFC[2] in hexch) and
              (FFC[3] in hexch) and
              (FFC[4] in hexch) then begin
   If HasMap then S:=Copy(S, 1, 10) + GetInfo(FromHex(Copy(s, 1, 4)),
                                              FromHex(Copy(S, 6, 4)));
  end else if FFC='SCR :' then IS:=1;
  InOutRes:=0;
  Writeln(TS, S);
 end;

 Close(ERR); Erase(ERR);
 Close(TS);

 CloseMap;
end.
