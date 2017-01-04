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

{$i-}{$f+}
unit mapengn;

interface

{Main MAP engine}
function  LoadMap(MapName: string): boolean;
function  GetInfo(Seg, Ofs: word): string;
procedure CloseMap;

{Misc}
const HexCh: Set Of Char = ['0'..'9','A'..'F', 'a'..'f'];
function FromHex(S: string): longint;

implementation
uses Objects, Collect, Search, Advance1 {$IFNDEF DPMI}, ExtraMem{$ENDIF};

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

function ToDec(i: word): string; var s: string[5];
begin Str(i, s); ToDec:=s; end;

function Expl(s: string; n: byte): string;
var i: byte;
begin
 for i:=length(s) to n do s:=s+' ';
 Expl:=s;
end;

var {FuncNames: TStringCollection;}
    FileNames: TStringCollection;

Type TSR = Record
            FileInd:     Word;
            FuncInd:     Word;
            LineNum:     Word;
            StrtAddrSeg: Word;
            StrtAddrOfs: Word;
           End;

var {$IFDEF DPMI}
    MapDt: TMemoryStream;
    {$ELSE}
    MapDt: TEMSStream;
    {$ENDIF}
    Mpp: TBufStream;
    HasMap: Boolean;
    s: string;
    sr: TSR;
    SS, SO: Word;
    c: char;
    np: longint;
    NumEl: longint;

Function Compare(a: longint): longint;
var fr: TSR;
    Offset: Longint;
begin
 Offset := a;
 Offset := a*SizeOf(TSR);
 MapDt.Seek(Offset);
 MapDt.Read(fr, SizeOf(TSR));
 if fr.StrtAddrSeg<SS then Compare:=-1 else
 if fr.StrtAddrSeg>SS then Compare:=+1 else
 if fr.StrtAddrOfs<SO then Compare:=-1 else
 if fr.StrtAddrOfs>SO then Compare:=+1 else
                           Compare:=0;
end;

function LoadMap;
begin
 FileNames.Init($8, $8);
 {FuncNames.Init($8, $8);}
 Mpp.Init(MapName, stOpenPacked, $1000);
 if Mpp.Status<>stOk then begin
  LoadMap:=false;
  exit;
 end else begin
  MapDt.Init($300000, $1000);
  repeat
   S:='';
   c:=' ';
   while c<>#0 do begin
    Mpp.Read(C, SizeOf(C));
    if c<>#0 then S:=S+C;
   end;
   if s<>'' then FileNames.AtInsert(FileNames.Count, NewStr(S));
  until s='';
  repeat
   S:='';
   c:=' ';
   while c<>#0 do begin
    Mpp.Read(C, SizeOf(C));
    if c<>#0 then S:=S+C;
   end;
   {if s<>'' then FuncNames.AtInsert(FuncNames.Count, NewStr(S));}
  until s='';
  MapDt.CopyFrom(Mpp, Mpp.GetSize-Mpp.GetPos);
  Mpp.Done;
  NumEl:=MapDt.GetPos div SizeOf(TSR);
  LoadMap:=(NumEl>0) and (Mpp.Status=stOk) and (MapDt.Status=stOk);
 end;
end;

function GetInfo(Seg, Ofs: word): string;
begin
 SS:=Seg; SO:=Ofs;
 np:=BinSearch(Compare, NumEl);
 np:=np*SizeOf(TSR);
 MapDt.Seek(np);
 MapDt.Read(sr, SizeOf(TSR));
 GetInfo:={' Function - '+Expl(PString(FuncNames.At(sr.FuncInd))^, 15)+';'}
         +' File - '+Expl(PString(FileNames.At(sr.FileInd))^, 15)+';'
         +' Line - '+Expl(ToDec(sr.LineNum), 15);
end;

Procedure CloseMap;
begin
 FileNames.Done;
 {FuncNames.Done;}
 MapDt.Done;
end;

end.
