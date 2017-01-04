{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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
unit Arc_UFA; {UFA}

interface
 uses Archiver, Advance1, Objects, FViewer, Advance, LFNCol, Dos;

type
   PUFAArchive = ^TUFAArchive;
   TUFAArchive = object(TARJArchive)
     FilesNumber:  LongInt;
     constructor Init;
     procedure GetFile; virtual;
     function GetID: Byte; virtual;
     function GetSign: TStr4; virtual;
   end;

type
  PUFACFHEADER = ^TUFACFHEADER;
  TUFACFHEADER = record
             Sign:Array[1..3]of Char;
             tmp:array[1..5]of char;
end;

implementation
{ ---------------------- UFA (by Luzin Aleksey)---------------------------}
constructor TUFAArchive.Init;
var Sign: TStr5;
    q: String;
begin
  Sign := GetSign; Dec(Sign[0]); Sign := Sign+#0;
  FreeStr := SourceDir + DNARC;
  TObject.Init;
  Packer                := NewStr(GetVal(@Sign[1], @FreeStr[1], PPacker,             'UFA.EXE'));
  UnPacker              := NewStr(GetVal(@Sign[1], @FreeStr[1], PUnPacker,           'UFA.EXE'));
  Extract               := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtract,            'e'));
  ExtractWP             := NewStr(GetVal(@Sign[1], @FreeStr[1], PExtractWP,          'x'));
  Add                   := NewStr(GetVal(@Sign[1], @FreeStr[1], PAdd,                'a'));
  Move                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PMove,               'm'));
  Delete                := NewStr(GetVal(@Sign[1], @FreeStr[1], PDelete,             'd'));
  Garble                := NewStr(GetVal(@Sign[1], @FreeStr[1], PGarble,             '-p'));
  Test                  := NewStr(GetVal(@Sign[1], @FreeStr[1], PTest,               't'));
  IncludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PIncludePaths,       '-r'));
  ExcludePaths          := NewStr(GetVal(@Sign[1], @FreeStr[1], PExcludePaths,       '-ep'));
  ForceMode             := NewStr(GetVal(@Sign[1], @FreeStr[1], PForceMode,          ''));
  RecoveryRec           := NewStr(GetVal(@Sign[1], @FreeStr[1], PRecoveryRec,        ''));
  SelfExtract           := NewStr(GetVal(@Sign[1], @FreeStr[1], PSelfExtract,        ''));
  Solid                 := NewStr(GetVal(@Sign[1], @FreeStr[1], PSolid,              ''));
  StoreCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PStoreCompression,   ''));
  FastestCompression    := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastestCompression, ''));
  FastCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PFastCompression,    '-m3'));
  NormalCompression     := NewStr(GetVal(@Sign[1], @FreeStr[1], PNormalCompression,  '-m1'));
  GoodCompression       := NewStr(GetVal(@Sign[1], @FreeStr[1], PGoodCompression,    ''));
  UltraCompression      := NewStr(GetVal(@Sign[1], @FreeStr[1], PUltraCompression,   '-m5'));
  q := GetVal(@Sign[1], @FreeStr[1], PListChar, 'l');
  if q<>'' then ListChar := q[1] else ListChar:=' ';
  q := GetVal(@Sign[1], @FreeStr[1], PSwap, '1');
  if q='0' then Swap := False else Swap := True;
  q := GetVal(@Sign[1], @FreeStr[1], PUseLFN, '0');
  if q='0' then UseLFN := False else UseLFN := True;
  PutTempBefore := 1;
  FilesNumber := 1;
end;

function TUFAArchive.GetID;
begin
  GetID := arcUFA;
end;

function TUFAArchive.GetSign;
begin
  GetSign := sigUFA;
end;

Procedure TUFAArchive.GetFile;
var
  C:   Char;
  S:   string;
  Tmp  :Word;
  FH: record
      tmp:array[1..$2A]of char;
      DateTime:LongInt;
      PackSize:LongInt;
      OriginalSize:LongInt;
      FileNameSize:Word;
 End;
var TTmp:longint;
begin
{  if (FilesNumber = 0) then
    begin
      FileInfo.Last := 1;
      Exit;
    end;}
  Dec(FilesNumber);
  TTmp:=ArcFile^.GetPos;
  if (TTmp = ArcFile^.GetSize) or (TTmp = 0) then
     begin FileInfo.Last := 1; Exit end;
   if (ArcFile^.Status <> stOK) then begin FileInfo.Last := 2;Exit;end;
  ArcFile^.Read(FH, SizeOf(FH));
  if (ArcFile^.Status <> 0) then begin FileInfo.Last:=2;Exit;end;
{  ArcFile^.Seek(TTmp+FH.HeadSize+4-FH.FileNameSize);}
  S[0] := #0;
  Tmp:=0;
         if FH.FileNameSize > 512 then begin FileInfo.Last := 2;Exit;end;
         if FH.FileNameSize > 250 then FH.FileNameSize := 250;

         ArcFile^.Read(S[1], FH.FileNameSize); S[0] :=Char(FH.FileNameSize);
       if S = ''  then
       begin FileInfo.Last := 2; Exit; end;
       inc(FilesNumber);
       FileInfo.Attr := 0;
       FileInfo.USize := FH.OriginalSize;
       FileInfo.PSize := FH.PackSize;
       FileInfo.Date := FH.DateTime;
       FileInfo.FName := S;
       FileInfo.LFN  := AddLFN(S);
       TTmp:=ArcFile^.GetPos;
       ArcFile^.Seek(TTmp+FH.PackSize);
       FileInfo.Last := 0;
end;

end.
