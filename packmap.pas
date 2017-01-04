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
{$i-}{$m 8192, 102400, 655360}
program PackMap;

uses Objects, Collect, Advance1;

function FromHex(S: string): longint;
 var b: byte;
     l: longint;
 begin
  l:=0;
  for b:=1 to length(s) do
   if s[b] in ['0'..'9']
    then l:=l shl 4+byte(s[b])-byte('0')
    else
     if UpCase(s[b]) in ['A'..'F']
      then l:=l shl 4+byte(UpCase(s[b]))-byte('A')+10;
  FromHex:=l;
 end;

function FromDec(S: string): longint;
 var b: byte;
     l: longint;
 begin
  l:=0;
  for b:=1 to length(s) do
   if s[b] in ['0'..'9']
    then l:=l*10+byte(s[b])-byte('0');
  FromDec:=l;
 end;

var FileNames: TStringCollection;

Type TSR = Record
            FileInd:     Word;
            FuncInd:     Word;
            LineNum:     Word;
            StrtAddrSeg: Word;
            StrtAddrOfs: Word;
           End;

var OData: TBufStream;
    MapDt: TBufStream;
    Map: Text;
    ps: string;
    s: string;
    readstate: (_StartRead, _ReadFuncs, _ReadLines, _EndRead);
    clr: TSR;
    cfi: word;
    nb, ne: byte;
    a: array[0..4095] of byte;
    i,j: longint;
    q: longint;
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
Function f(var x,y:TSR):Boolean;
  Begin
   f:=false;
   if x.StrtAddrSeg>y.StrtAddrSeg then f:=true  else
   if x.StrtAddrSeg<y.StrtAddrSeg then f:=false else
   if x.StrtAddrOfs>y.StrtAddrOfs then f:=true;
  End;
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
Procedure PoglSort; Label 1;
 Const r=SizeOf(TSR); mm=65520 div r; nseg=10; mr=mm*r;
 Type mas = Array[1..mm] of TSR;
 Var A:Array[1..nseg] of ^mas; c,cc,cr,cw,n,np:longint;
     i,j,k,mk,ni,nw,q,u,v,w,z:word; x:TSR;
 Procedure Vnut(c,cc:longint; m:byte);
  Var og: Array[1..nseg] of word;  i,j,u: word;
  Procedure Merg; Label 1;
   Begin x:= A[u]^[1]; i:=j+j;
    Repeat If i < q-m Then
     If f(A[og[i+1]]^[1], A[og[i]]^[1]) Then i:=i+1;
     If f(A[og[i]]^[1],x) Then og[j]:= og[i] Else Goto 1;
     j:=i; i:=j+j
    Until i > q-m;  1: og[j]:=u
   End;
  Procedure Init; Var k:word;
   Begin For k:= 1 to q-m do og[k]:=k+m;
    For k:= (q-m) div 2 downto 1 do
     Begin u:= og[k]; j:=k; Merg End
   End;
  Procedure Sipka(var A:mas; k,n1:word); Label 1;
   Begin If n1 < 2 Then Exit;
    Repeat x:= A[k]; j:=k; i:=j+j;
     While i <= n1 do
      Begin If i < n1 Then If f(A[i+1], A[i]) Then i:=i+1;
      If f(A[i],x) Then A[j]:= A[i] Else Goto 1; j:=i; i:=j+j
      End;  1: A[j]:=x; k:=k-1
    Until k = 0
   End;
  Begin q:=m; OData.Seek(c*r);
   Repeat mk:=mm; If cc-c < mm Then mk:= cc-c; q:=q+1; z:=q;
    OData.Read(A[q]^,mk*r); c:= c+mk; Sipka(A[q]^,mk div 2, mk)
   Until c = cc;          Init; np:= mk;
   Repeat  u:=og[1];
    x:=A[q]^[np]; A[q]^[np]:=A[u]^[1]; A[u]^[1]:=x;
    np:=np-1; If u=q Then ni:=np Else ni:=mm; Sipka(A[u]^,1,ni);
    j:=1;  If np = 0 Then Begin np:=mm; q:=q-1; Init End
           Else If q > m+1 Then Merg
   Until q = m
  End;
 Procedure Sli(var B,D,G:mas);
  Begin Repeat If k=mm Then Begin OData.Seek(cw*r); OData.Write(G,mm*r);
                            cw:= cw+mm; k:=0 End;
         k:=k+1; If f(B[i],D[j]) Then Begin G[k]:=D[j]; j:=j+1 End
                 Else Begin G[k]:=B[i]; i:=i+1 End
        Until (i > ni) Or (j > mk)
  End;
 BEGIN
  n:= OData.GetSize div r; q:=n div mm; w:=1; v:=0; c:=0;
  While (MaxAvail >= mr) And (w < nseg) do
   Begin  v:=v+mm; GetMem(A[w], mr); nw:=n-q*mm;
    w:=w+1; If (w > q) And (MaxAvail >= nw*r) Then Goto 1
   End;
  nw:= MaxAvail div r; v:=v+nw-2*mm;  c:= n mod v;
  If c <= 2*mm Then c:= c+v; c:= n-c;
1:If nw > 0 Then GetMem(A[w],nw*r) Else Begin w:=w-1; nw:=mm End;
  Vnut(c,n,0); OData.Seek(c*r); cw:=c;
  For q:=1 to z do If q=z Then OData.Write(A[q]^, mk*r)
                   Else OData.Write(A[q]^, mm*r);
  While c <> 0 do
   Begin cc:=c; c:=c-v; Vnut(c,cc,2); OData.Read(A[1]^,mm*r); k:=0;
    cr:= OData.GetPos div r; cw:=c; q:=3; ni:=mm; mk:=mm; i:=1; j:=1;
    Repeat If q=w Then ni:=nw;           Sli(A[q]^,A[1]^,A[2]^);
     If i > ni Then Begin i:=1; q:=q+1 End
     Else Begin j:=1; OData.Seek(cr*r); If n-cr < mm Then mk:=n-cr;
           If mk > 0 Then OData.Read(A[1]^,mk*r); cr:=cr+mk End
    Until (q > w) Or (mk=0); OData.Seek(cw*r); OData.Write(A[2]^,k*r);
    If mk=0 Then
           Begin For j:=i to ni do A[q]^[j-i+1]:= A[q]^[j];
            OData.Write(A[q]^,(ni-i+1)*r);
            For u:=q+1 to w do If u=w Then OData.Write(A[w]^,nw*r)
                               Else OData.Write(A[u]^,mm*r)
           End
   End;
  FreeMem(A[w], nw*r); For u:= w-1 downto 1 do FreeMem(A[u], mr)
 END;
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}

label ReadLines;
begin
 Writeln(' PackMap v2.2 (c) Anton Fedorov aka DataCompBoy 1999-2000');
 ps:=paramstr(1);
 if (paramcount<1) or (ps='?') or (ps='/?') or (ps='-?') then
  begin
   Writeln('PackMap - compiles liker MAP output to binary format, expected by mapfind');
   Writeln('     (c) 2000 Anton Fedorov aka DataCompBoy');
   Writeln('Usage:');
   Writeln('   PackMap MapName   - compile MapName.Map to MapName.Mpp');
   Writeln('   PackMap /?        - this help text');
   Writeln('   PackMap /i        - .Mpp file format info');
   Writeln('   PackMap /a        - info about the author');
   halt(1);
  end;
 if ps[1] in ['/','-'] then
  begin
   case UpCase(ps[2]) of
    'I': begin
          Writeln('PackMap - compiles liker MAP output to binary format, expected by mapfind');
          Writeln('     (c) 2000 Anton Fedorov aka DataCompBoy');
          writeln('.mpp file format:');
          writeln(' An .mpp consists of three parts, following each other.');
          writeln(' 1. Source file names list as a sequence of ASCIIZ strings,');
          writeln('terminated with an empty string');
          writeln(' 2. Routine names list formatted same as above. At the present moment,');
          writeln('contains only ''!unknown!''');
          writeln(' 3. Address list as a sequence of records up to the end of file');
          writeln('  Record structure:');
          Writeln(' DW FileInd         ; Source file name index');
          Writeln(' DW FuncInd         ; Routine name index (now just 0)');
          Writeln(' DW LineNum         ; Line number');
          Writeln(' DW StrtAddrSeg     ; Object''s starting address segment');
          Writeln(' DW StrtAddrOfs     ; Object''s starting address offset');
          halt(1)
         end;
    'A': begin
          Writeln('PackMap - compiles liker MAP output to binary format, expected by mapfind');
          Writeln('     (c) 2000 Anton Fedorov aka DataCompBoy');
          Writeln(' Deticated to Uliana Matarykina');
          Writeln;
          Writeln(' Author''s coordinates:');
          Writeln('  FTN: 2:5000/111.33@Fidonet.Org aka Fedorov@p33.f111.n5000.z2.fidone.org');
          Writeln('       911:911/1@Codernet aka DataCompBoy@codernet.cjb.net');
          Writeln('  INET: DataCompBoy@mail.ru');
          Writeln('  Tel. no: +7-383-245-2133');
          Writeln('  Standard mail: 007 Russia, 630057 Novosibirsk, P.O.B. 209');
          halt(1)
         end;
   end;
  end
 else
  begin
   FileNames.Init($8, $8);
   OData.Init('OData.Tmp', stCreate, $8000);

   FileNames.AtInsert(0, NewStr('!unknown!'));

   with clr do begin
    FileInd:=0;
    LineNum:=0;
    StrtAddrSeg:=0;
    StrtAddrOfs:=0;
    FuncInd:=0;
    OData.Write(clr, sizeof(clr));
   end;

   assign(Map, ps+'.map');
   reset(Map);
   if IOResult<>0 then
    begin
     writeln(' Cannot open '+ps+'.map');
     halt(1);
    end;

   i:=0;
   readstate:=_StartRead;
   while not EOF(Map) do
    begin
     readln(Map, s);
     inc(i);
     if i mod 10 = 0 then begin
      write(#13,i:5,': ',s);
     end;
     case ReadState of
      _StartRead: if s='  Address         Publics by Value' then
                   ReadState:=_ReadFuncs;
      _ReadFuncs: begin
                   if s='' then continue;
                   if copy(s, 1, 12)='Line numbers'
                    then begin ReadState:=_ReadLines; goto ReadLines end
                  end;
      _ReadLines: begin
ReadLines:
                   if s='' then continue;
                   if copy(s, 1, 22)='Program entry point at' then ReadState:=_EndRead;
                   if copy(s, 1, 12)='Line numbers' then
                    begin
                     cfi:=FileNames.count;
                     NB:=17; while S[NB]<>'(' do inc(NB); inc(NB);
                     NE:=0; while S[NB+NE]<>')' do inc(NE);
                     FileNames.AtInsert(FileNames.Count, NewStr(Copy(S, NB, NE)));
                    end
                   else
                    while length(s)>0 do
                     with clr do begin
                      FileInd:=cfi;
                      LineNum:=FromDec(Copy(S, 1, 6));
                      StrtAddrSeg:=FromHex(Copy(S, 8, 4));
                      StrtAddrOfs:=FromHex(Copy(S,13, 4));
                      FuncInd:=0;
                      OData.Write(clr, sizeof(clr));
                      delete(S, 1, 16);
                     end;
                  end;
      _EndRead: break;
     end;
    end;
   Close(Map);
   writeln(#13,i:5,': ',s);
   FileNames.AtInsert(FileNames.Count, NewStr('!unknown!'));
   with clr do begin
    FileInd:=FileNames.Count;
    LineNum:=0;
    StrtAddrSeg:=$FFFF;
    StrtAddrOfs:=$FFFF;
    FuncInd:=0;
    OData.Write(clr, sizeof(clr));
   end;
   Write('Sorting...');
   PoglSort;
   Writeln('done');

   MapDt.Init(ps+'.mpp', stCreate, $2048);
   if MapDt.Status<>0 then
    begin
     writeln(' Cannot create '+ps+'.mpp');
     halt(1);
    end;

   While FileNames.Count>0 do
    begin
     S:=PString(FileNames.At(0))^;
     FileNames.AtFree(0);
     S[Length(S)+1]:=#0;
     MapDt.Write(S[1], Length(S)+1);
    end;
   S[0]:=#0; MapDt.Write(S[0], 1);
   FileNames.Done;

   S:='!unknown!'#0#0;
   MapDt.Write(S[1], Length(S));

   OData.Seek(0);
   j:=(OData.GetSize div 4096)-1;
   q:=OData.GetSize mod 4096;
   for i:=0 to j do begin
    OData.Read(a, 4096);
    MapDt.Write(a,4096);
   end;
   if q > 0 then begin
     OData.Read(a, q);
     MapDt.Write(a, q);
   end;
   OData.Done;
   Assign(Map, 'OData.Tmp'); Erase(Map);

   MapDt.Done;
  end;
end.
