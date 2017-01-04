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
unit Arc_lim; {LIM}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, LFNCol, Dos, lfn;

type
    PLIMArchive = ^TLIMArchive;
    TLIMArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     LIMHdr = record
      ID: Array [1..2] of Char;
      Method: Byte;
      ThreeZeros: Array [1..3] of Byte;
      Date: LongInt;
      ThreeSym: Array [1..3] of Byte;
      OriginSize: LongInt;
      PackedSize: LongInt;
      Data: Array[1..4] of Byte;
     end;

implementation
{ ----------------------------- LIM ------------------------------------}

constructor TLIMArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'LIMIT.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'LIMIT.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'e -p'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a -whs'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm -whs'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'Del'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       '-p'));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     '-r'));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   '-m0'));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, '-ms'));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-ms'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m1'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    '-mx'));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-mx'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, '@');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TLIMArchive.GetID;
begin
  GetID := arcLIM;
end;

function TLIMArchive.GetSign;
begin
  GetSign := sigLIM;
end;

Procedure TLIMArchive.GetFile;
var HS,i : AWord;
    FP   : Longint;
    P    : LIMHdr;
    Q    : Array [1..40] of Char absolute P;
    S    : String;
    C    : Char;
    label 1;
begin
1:
 ArcFile^.Read(P, 4);
 if (ArcFile^.Status = stOK) and (P.ID = #$13#$F8) and (P.Method = 5)
  then begin FileInfo.Last := 1;Exit;end;
 if (ArcFile^.Status = stOK) and (P.ID = #$80#$D1)
  then
   begin
    FileInfo.Last := 0;
    i := 1; S := ''; C := #1;
    While (I < 80) and (C <> #0) and not Abort and (ArcFile^.Status = stOK) do
     begin ArcFile^.Read(C, 1); if C <> #0 then S := S + C; Inc(I); end;
     CDir := S;
    Goto 1;
   end;
 if (ArcFile^.Status <> stOK) or (P.ID <> #35#241)
  then begin FileInfo.Last := 2;Exit;end;
 ArcFile^.Read(P.ThreeZeros[2], Sizeof(P)-4);
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
 {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
 FileInfo.Last := 0;
 FileInfo.Attr := 0;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date  := P.Date{P.Date shl 16) or (P.Date shr 16)};
 i := 1; S := '';
 C := #1;
 While (I < 80) and (C <> #0) and not Abort and (ArcFile^.Status = stOK) do
  begin ArcFile^.Read(C, 1); if C <> #0 then S := S + C; Inc(I); end;
  While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
 While Pos(#255, S) > 0 do S[Pos(#255, S)] := '\';
 FileInfo.LFN  := AddLFN(CDir+'\'+S);      {DataCompBoy}
 FileInfo.FName := CDir + '\' + S; {DataCompBoy}
 if P.ThreeZeros[3] and Directory <> 0 then Goto 1;
 FP := ArcFile^.GetPos;
 ArcFile^.Seek(FP + P.PackedSize);
end;

end.
