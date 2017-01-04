{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.12
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
unit Arc_CHZ; {CHZ}

interface
 uses Archiver, Advance1, Objects{, FViewer}, Advance, LFNCol, Dos, Arc_ZOO,
      lfn;

type
    PCHZArchive = ^TCHZArchive;
    TCHZArchive = object(TARJArchive)
        constructor Init;
        procedure GetFile; virtual;
        function GetID: Byte; virtual;
        function GetSign: TStr4; virtual;
    end;

type
     CHZHdr = record
      ID: Array [1..4] of Char;
      PackedSize: LongInt;
      OriginSize: LongInt;
      Data: Array[1..4] of Byte;
      Date: LongInt;
      QQQ: AWord;
      NameLen: AWord;
     end;


implementation
{ ----------------------------- CHZ ------------------------------------}

constructor TCHZArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'CHARC.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'CHARC.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            '-E'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          '-E'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                '-A -T'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               '-A -T -M'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             '-D'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             ''));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               ''));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       ''));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       ''));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          '-Y'));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        '-S'));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  RecurseSubDirs        := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecurseSubDirs,     ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, ''));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    ''));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  ''));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   ''));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, ' ');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
end;

function TCHZArchive.GetID;
begin
  GetID := arcCHZ;
end;

function TCHZArchive.GetSign;
begin
  GetSign := sigCHZ;
end;

Procedure TCHZArchive.GetFile;
var HS,i : AWord;
    FP   : Longint;
    P    : CHZHdr;
    Q    : Array [1..40] of Char absolute P;
    P1   : ZOOPHdr;
    S    : String;
    C    : Char;
    label 1;
begin
1:
 FP := ArcFile^.GetPos;
 if FP = ArcFile^.GetSize then begin FileInfo.Last := 1; Exit;end;
 ArcFile^.Read(P, 4);
 if (ArcFile^.Status <> stOK) or (Copy(P.ID,1,3) <> 'SCh')
  then begin FileInfo.Last := 2;Exit;end;
 if P.ID[4] = 'D' then
  begin
   ArcFile^.Seek(FP+4+5);
   ArcFile^.Read(S[0],1);
   ArcFile^.Read(S[1],Length(S));
   CDir := CDir + S+'\';
   Goto 1;
  end else if P.ID[4] = 'd' then
   begin
    if CDir <> '' then
     begin
      Dec(CDir[0]);
      while (CDir <> '') and (CDir[Byte(CDir[0])] <> '\') do Dec(CDir[0]);
     end;
    Goto 1;
   end;
 ArcFile^.Read(P.PackedSize, Sizeof(P)-4);
 if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
 {if (P.Method > 20) then begin FileInfo.Last:=2;Exit;end;}
 FileInfo.Last := 0;
 FileInfo.Attr := 0;
 FileInfo.USize := P.OriginSize;
 FileInfo.PSize := P.PackedSize;
 FileInfo.Date  := P.Date{P.Date shl 16) or (P.Date shr 16)};
 i := 1;
 S[0] := Char(P.NameLen);
 ArcFile^.Read(S[1], P.NameLen and 255);
 While Pos('/', S) > 0 do S[Pos('/', S)] := '\';
 While Pos(#255, S) > 0 do S[Pos(#255, S)] := '\';
 FileInfo.LFN  := AddLFN(CDir+S);    {DataCompBoy}
 FileInfo.FName := CDir + S; {DataCompBoy}
 ArcFile^.Seek(FP + P.PackedSize);
end;

end.
