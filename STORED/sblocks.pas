{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.08
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
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   18/10/2001 - ��� ����� ⥯��� �� �㦥�, �� ���������
   � ��室�����, �易��� � ��� �모�뢠����, ����祭�
   �������ਥ� "-SBlocks"
}

unit SBlocks;

interface

uses
  ExtraMem, Advance, Advance1, Advance2, Advance3, Objects, Dos, Memory,
  Collect;

const
     MaxBlockSize  = 1024;
     MaxCacheSize  = 120;

     asXMS  = 0;
     asEMS  = 1;
     asMem  = 2;

type

{Cat: �� ��ꥪ�� � DN/2 �� �ᯮ�������}
(*
    PCollector = ^TCollector;
    TCollector = object(TObject)
      Count: LongInt;
      procedure Insert(P: PLongString); virtual;
      function  At(N: LongInt): PLongString; virtual;
      procedure AtPut(N: LongInt; P: PLongString); virtual;
      procedure AtInsert(N: LongInt; P: PLongString); virtual;
      procedure AtReplace(N: LongInt; P: PLongString); virtual;
      procedure AtDelete(N: LongInt); virtual;
      procedure AtFree(N: LongInt); virtual;
      procedure AddStr(S: LongString); virtual;
      procedure SwapItems(Idx1, Idx2: CondInt); virtual;
    end;

    PStdCollector = ^TStdCollector;
    TStdCollector = object(TCollector)
      Collection: PCollection;
      constructor Init(MaxLines: CondInt);
      destructor Done; virtual;
      procedure Insert(P: PLongString); virtual;
      function  At(N: LongInt): PLongString; virtual;
      procedure AtInsert(N: LongInt; P: PLongString); virtual;
      procedure AtPut(N: LongInt; P: PLongString); virtual;
      procedure AtReplace(N: LongInt; P: PLongString); virtual;
      procedure AtDelete(N: LongInt); virtual;
      procedure AtFree(N: LongInt); virtual;
      procedure AddStr(S: LongString); virtual;
      procedure SwapItems(Idx1, Idx2: CondInt); virtual;
    end;
*)
    PCollector = PLineCollection;
    TCollector = TLineCollection;
    PStdCollector = PLineCollection;
    TStdCollector = TLineCollection;
{/Cat}


(*  PMemBlock = ^TMemBlock;
    TMemBlock = record
      Pos: LongInt;
      Len: LongInt;
    end;

    PNumBlock = ^TNumBlock;
    TNumBlock = Array [0..MaxBlockSize] of TMemBlock;

    PBlocker = ^TBlocker;
    TBlocker = object(TCollector)
       Blocks: Array [0..29] of PNumBlock;
{$IFNDEF NOEXTRA}
       XMSStream: PXMSStream;
       EMSStream: PEMSStream;
{$ENDIF}
       MemStream: PMemoryStream;
{$IFNDEF NOEXTRA}
       NoXMS: Boolean;
       NoEMS: Boolean;
{$ENDIF}
       NoMem: Boolean;
       Deleted: PSortedCollection;

       UseCache: Boolean;
       CachePos: LongInt;
{$IFDEF OldCollection}
       CacheSize: Word;
{$ELSE}
       CacheSize: LongInt;
{$ENDIF}
       CacheModified: Boolean;
       Cache: Array [0..MaxCacheSize] of
               record S: PLongString; Modified: Boolean; end;

       constructor Init(Index: CondInt);
       procedure InsertIndex(N: CondInt; L: LongInt; Len: LongInt);
       procedure DeleteIndex(N: CondInt);

       procedure Insert(P: PLongString); virtual;
       function  At(N: LongInt): PLongString; virtual;
       procedure AtInsert(N: LongInt; P: PLongString); virtual;
       procedure AtPut(N: LongInt; P: PLongString); virtual;
       procedure AtReplace(N: LongInt; P: PLongString); virtual;
       procedure AtDelete(N: LongInt); virtual;
       procedure AtFree(N: LongInt); virtual;
       procedure AddStr(S: LongString); virtual;
       procedure SwapItems(Idx1, Idx2: CondInt); virtual;

       procedure ReadBlock(var B: TMemBlock; var S: LongString);
       procedure WriteBlock(var B: TMemBlock; var S: LongString);
       procedure PlaceDeleted(var B: TMemBlock);
       destructor Done; virtual;

       procedure SeekCache(N: LongInt);
       procedure FlushCache(Release: Boolean);
    end;
*)

function GetCollector(MaxSize, MaxLines: LongInt): PCollector;

implementation
 uses Commands, Startup;

{Cat: �� ��ꥪ�� � DN/2 �� �ᯮ�������}
(*

{ TCollector }

procedure TCollector.Insert;
begin
end;

function  TCollector.At;
begin
end;

procedure TCollector.AtInsert;
begin
end;

procedure TCollector.AtReplace;
begin
end;

procedure TCollector.AtPut;
begin
end;

procedure TCollector.AtDelete;
begin
end;

procedure TCollector.AtFree;
begin
end;

procedure TCollector.AddStr;
begin
end;

procedure TCollector.SwapItems;
begin
end;

{ TStdCollector }

constructor TStdCollector.Init;
begin
  inherited Init;
  Collection := New(PLineCollection, Init(MaxLines+200, 1000, True));
end;

destructor TStdCollector.Done;
begin
  if Collection <> nil then Dispose(Collection,Done); Collection:=nil;
  inherited Done;
end;

procedure TStdCollector.Insert;
begin
  Collection^.Insert(P);
  Count := Collection^.Count;
end;

function  TStdCollector.At;
begin
  At := Collection^.At(N);
{Cat:warn �㦭� ��?}
  Count := Collection^.Count;
end;

procedure TStdCollector.AtInsert;
begin
  Collection^.AtInsert(N, P);
  Count := Collection^.Count;
end;

procedure TStdCollector.AtReplace;
begin
  Collection^.AtReplace(N, P);
{Cat:warn �㦭� ��?}
  Count := Collection^.Count;
end;

procedure TStdCollector.AtPut;
begin
  Collection^.AtPut(N, P);
  Count := Collection^.Count;
end;

procedure TStdCollector.AtDelete;
begin
  Collection^.AtDelete(N);
  Count := Collection^.Count;
end;

procedure TStdCollector.AtFree;
begin
  Collection^.AtFree(N);
  Count := Collection^.Count;
end;

procedure TStdCollector.AddStr;
begin
  Collection^.Insert(NewLongStr(S));
{Cat:warn � ��� ����� ��������� Count, ����⭮, �㦭� - �������}
  Count := Collection^.Count;
end;

procedure TStdCollector.SwapItems;
  var P1, P2: Pointer;
begin
  P1 := Collection^.At(Idx1);
  P2 := Collection^.At(Idx2);
  Collection^.AtPut(Idx1, P2);
  Collection^.AtPut(Idx2, P1);
end;

*)
{/Cat}

(*
{ TDeletedCollection }

type
     PDeletedCollection = ^TDeletedCollection;
     TDeletedCollection = object(TSortedCollection)
        function Compare(P1, P2: Pointer): Integer; virtual;
        procedure FreeItem(P: Pointer); virtual;
     end;

function TDeletedCollection.Compare(P1, P2: Pointer): Integer;
begin
  if PMemBlock(P1)^.Len < PMemBlock(P2)^.Len then Compare := 1 else
    if PMemBlock(P1)^.Len > PMemBlock(P2)^.Len then Compare := -1 else
      Compare := 0;
end;

procedure TDeletedCollection.FreeItem(P: Pointer);
begin
  if P <> nil then Dispose(PMemBlock(P));
end;

{ TBlocker }

constructor TBlocker.Init(Index: CondInt);
begin
  TObject.Init;
  UseCache := True;
  {$IFNDEF NOEXTRA}
  NoXMS := not XMSFound or (EditorDefaults.EdOpt and ebfXMS = 0);
  NoEMS := not EMSFound or (EditorDefaults.EdOpt and ebfEMS = 0);
  {$ENDIF}
  CachePos:=0;
  CacheSize:=0;
  CacheModified:=false;
  FillChar(Cache, SizeOf(Cache), 0);
end;

procedure TBlocker.InsertIndex;
  var K, I, K1, K2, KK: CondInt;
begin
  K := N div MaxBlockSize;
  KK := N mod MaxBlockSize;
  K1 := Count div MaxBlockSize;
  K2 := Count mod MaxBlockSize;
  if Blocks[K] = nil then Blocks[K] := MemAlloc(SizeOf(TNumBlock));
  if Blocks[K] = nil then Exit;
  if Blocks[K1] = nil then Blocks[K1] := MemAlloc(SizeOf(TNumBlock));
  if Blocks[K1] = nil then Exit;
  Inc(Count);
  if (K=K1) and (KK=K2) then
    begin Blocks[K]^[KK].Pos := L; Blocks[K]^[KK].Len := Len; Exit end;
  for I := K1 downto K+1 do
    begin
      if Blocks[I] <> nil then
        begin
          Move(Blocks[I]^[0], Blocks[I]^[1], (K2) * SizeOf(TMemBlock));
          Blocks[I]^[0] := Blocks[I-1]^[MaxBlockSize-1];
        end;
      K2 := MaxBlockSize-1;
    end;
  Move(Blocks[K]^[KK], Blocks[K]^[KK+1], (K2-KK) * SizeOf(TMemBlock));
  Blocks[K]^[KK].Pos := L; Blocks[K]^[KK].Len := Len;
end;

procedure TBlocker.DeleteIndex;
  var K, I, K1, K2, KK: CondInt;
begin
  if Count = 0 then Exit;
  K := N div MaxBlockSize;
  KK := N mod MaxBlockSize;
  K1 := (Count-1) div MaxBlockSize;
  K2 := (Count-1) mod MaxBlockSize;
  if Blocks[K] = nil then Blocks[K] := MemAlloc(SizeOf(TNumBlock));
  if Blocks[K] = nil then Exit;
  if Blocks[K1] = nil then Blocks[K1] := MemAlloc(SizeOf(TNumBlock));
  if Blocks[K1] = nil then Exit;
  Dec(Count);
  if Blocks[K] <> nil then PlaceDeleted(Blocks[K]^[KK]);
  for I := K to K1-1 do
    begin
      if Blocks[I] <> nil then
        begin
          Move(Blocks[I]^[KK+1], Blocks[I]^[KK], (MaxBlockSize-1-KK) * SizeOf(TMemBlock));
          Blocks[I]^[MaxBlockSize-1] := Blocks[I+1]^[0];
        end;
      KK := 0;
    end;
  if (Blocks[K1] <> nil) and (K2-KK > 0) then
      Move(Blocks[K1]^[1+KK], Blocks[K1]^[KK], (K2-KK) * SizeOf(TMemBlock));
end;

function TBlocker.At(N: LongInt): PLongString;
  var
      L: LongInt;
begin
  if (N >= 0) and (N < Count) then
    begin
      if (CachePos > N) or (CachePos+CacheSize <= N) then
      begin
        if UseCache then L := Max(0, N - (MaxCacheSize div 2)*Byte(CacheSize>0) - 1)
                    else L := Max(0, N - 1);
        SeekCache(L);
      end;
      At := Cache[N-CachePos].S;
    end else At := nil;
end;

procedure TBlocker.PlaceDeleted;
  var P: PMemBlock;
      I: CondInt;
begin
  if B.Pos < 0 then Exit;
  if Deleted = nil then
     begin
       Deleted := New(PDeletedCollection, Init(100, 100));
       New(P); P^ := B;
       Deleted^.AtInsert(0, P);
     end else
     begin
       Deleted^.Search(@B, I);
       if I < 200 then
          begin
            New(P); P^ := B;
            Deleted^.AtInsert(0, P);
            if Deleted^.Count = 201 then Deleted^.AtFree(200);
          end;
     end;
end;

procedure TBlocker.SeekCache;
  var K, I, KK: CondInt;
      S: LongString;
begin
  for I := 0 to CacheSize-1 do
          DisposeLongStr(Cache[I].S);
  FillChar(Cache, SizeOf(Cache), 0);

  K := N div MaxBlockSize;
  KK := N mod MaxBlockSize;
  CachePos := N;

  if (CacheSize > 0) and UseCache then I := MaxCacheSize
                                  else I := 3;

  CacheSize := 0;

  while (N < Count) and (Blocks[K] <> nil) and (CacheSize < I) do
    begin
      ReadBlock(Blocks[K]^[KK], S);
      Cache[CacheSize].S := NewLongStr(S);
      Inc(N);
      Inc(CacheSize);
      K := N div MaxBlockSize;
      KK := N mod MaxBlockSize;
    end;

end;

procedure TBlocker.ReadBlock;
begin
writeln('----ReadBlock----');readln;
{$IFNDEF NOEXTRA}
  if B.Pos < 2000000 then
    begin
      XMSStream^.Status := 0;
      XMSStream^.Seek(B.Pos);
      XMSStream^.Read(S, B.Len+1);
    end else
  if B.Pos < 4000000 then
    begin
      EMSStream^.Status := 0;
      EMSStream^.Seek(B.Pos-2000000);
      EMSStream^.Read(S, B.Len+1);
    end else
{$ENDIF}
    begin
      MemStream^.Status := 0;
      MemStream^.Seek(B.Pos-4000000);
      MemStream^.Read(S, B.Len+1);
    end;
end;

procedure TBlocker.WriteBlock;
  label {$IFNDEF NOEXTRA}0, 1, 2, {$ENDIF}3, 4;
  var I: CondInt;
      BB: Boolean;
      L: LongInt;
begin
writeln('----WriteBlock----');readln;
  if (B.Pos < 0) or (B.Len < Length(S)) then
    begin
       {
       if B.Pos >= 0 then PlaceDeleted(B);
       }
       B.Len := Min(MaxStringLen, Length(S)+5);
       {
       if (Deleted <> nil) and (Deleted^.Count > 0) then
         begin
           BB := Deleted^.Search(@B, I);
           if (I > 0) or BB then
             begin
               B := PMemBlock(Deleted^.At(I-Byte(not BB)))^;
               Deleted^.AtFree(I-Byte(not BB));
               Goto 4;
             end;
         end;
       }
{$IFNDEF NOEXTRA}
0:
       if not NoXMS then
         begin
           if XMSStream <> nil then
             begin
1:
               XMSStream^.Status := 0;
               L := XMSStream^.GetSize;
               XMSStream^.Seek(L);
               XMSStream^.Write(S, B.Len+1);
               if XMSStream^.Status <> stOK then begin NoXMS := On; goto 0 end
                 else begin B.Pos := L; Exit end;
             end else
             begin
               New(XMSStream, Init(1024,1024));
               if XMSStream^.Status = stOK then Goto 1;
               Dispose(XMSStream,Done);
               XMSStream:=nil;
               NoXMS := On;
               Goto 0;
             end;
         end;
       if not NoEMS then
         begin
           if EMSStream <> nil then
             begin
2:
               EMSStream^.Status := 0;
               L := EMSStream^.GetSize;
               EMSStream^.Seek(L);
               EMSStream^.Write(S, B.Len+1);
               if EMSStream^.Status <> stOK then begin NoEMS := On; goto 0 end
                 else begin B.Pos := L + 2000000; Exit end;
             end else
             begin
               EMSCurHandle := $FFFF;
               EMSCurPage := $FFFF;
               New(EMSStream, Init(16384,16384));
               if EMSStream^.Status = stOK then Goto 2;
               EMSCurHandle := $FFFF;
               EMSCurPage := $FFFF;
               Dispose(EMSStream,Done);
               EMSStream := nil;
               EMSCurHandle := $FFFF;
               EMSCurPage := $FFFF;
               NoEMS := On;
               Goto 0;
             end;
         end else
{$ENDIF}
         begin
           if MemStream <> nil then
             begin
3:
               MemStream^.Status := 0;
               B.Pos := MemStream^.GetSize+4000000;
               MemStream^.Seek(MemStream^.GetSize);
               MemStream^.Write(S, B.Len+1);
               if MemStream^.Status <> stOK then NoMem := On
                 else Exit;
             end else
             begin
               New(MemStream, Init(8192, 2048));
               if MemStream^.Status = stOK then goto 3;
               Dispose(MemStream,Done);
               MemStream := nil;
               NoMem := On;
             end;
         end;
    end else
    begin
4:
{$IFNDEF NOEXTRA}
     if B.Pos < 2000000 then
       begin
         XMSStream^.Status := 0;
         XMSStream^.Seek(B.Pos);
         XMSStream^.Write(S, Length(S)+1);
       end else
     if B.Pos < 4000000 then
       begin
         EMSStream^.Status := 0;
         EMSStream^.Seek(B.Pos-2000000);
         EMSStream^.Write(S, Length(S)+1);
       end else
{$ENDIF}
       begin
         MemStream^.Status := 0;
         MemStream^.Seek(B.Pos-4000000);
         MemStream^.Write(S, Length(S)+1);
       end;
    end;
end;

procedure TBlocker.AtInsert(N: LongInt; P: PLongString);
  var B: TMemBlock;
begin
  if CacheSize > 0 then FlushCache(On);
  if P = nil then FreeStr := '' else FreeStr := P^;
  B.Pos := -1;
  WriteBlock(B, FreeStr);
  InsertIndex(N, B.Pos, B.Len);
  DisposeLongStr(P);
end;

procedure TBlocker.AtPut(N: LongInt; P: PLongString);
  var K, I, K1, K2, KK: CondInt;
begin
  if CacheSize > 0 then FlushCache(On);
  K := N div MaxBlockSize;
  KK := N mod MaxBlockSize;
  if Blocks[K] = nil then
    begin
      Blocks[K] := MemAlloc(MaxBlockSize * SizeOf(TMemBlock));
      if Blocks[K] = nil then Exit;
      FillChar(Blocks[K], MaxBlockSize * SizeOf(TMemBlock), $FF);
    end;
  if P = nil then FreeStr := '' else FreeStr := P^;
  WriteBlock(Blocks[K]^[KK], FreeStr);
  DisposeLongStr(P);
end;

procedure TBlocker.Insert(P: PLongString);
begin
  AtInsert(Count, P);
end;

procedure TBlocker.AtReplace(N: LongInt; P: PLongString);
begin
  AtPut(N, P);
end;

procedure TBlocker.AtDelete(N: LongInt);
begin
  {if CacheSize > 0 then FlushCache(On);}
  if (CachePos <= N) and (CachePos+CacheSize > N) then
     begin
       DisposeStr(Cache[N-CachePos].S);
       Move(Cache[N-CachePos+1], Cache[N-CachePos], SizeOf(Cache[0])*(CacheSize-(N-CachePos+1)));
       Dec(CacheSize);
     end else if CachePos > N then Dec(CachePos);
  DeleteIndex(N);
end;

procedure TBlocker.AtFree(N: LongInt);
begin
  AtDelete(N);
end;

procedure TBlocker.AddStr;
  var B: TMemBlock;
begin
  if CacheSize > 0 then FlushCache(On);
  B.Pos := -1;
  WriteBlock(B, S);
  InsertIndex(Count, B.Pos, B.Len);
end;

procedure TBlocker.FlushCache;
  var K, I, K1, K2, KK: CondInt;
begin
  K := CachePos div MaxBlockSize;
  KK := CachePos mod MaxBlockSize;
  if Release and (CacheSize > 0) then
    begin
       for I := 0 to CacheSize-1 do
          DisposeLongStr(Cache[I].S);
       FillChar(Cache, SizeOf(Cache), 0);
       CacheSize := 0;
    end;
  CacheModified := Off;
end;

procedure TBlocker.SwapItems;
  var B1, B2: TMemBlock;
      K,KK,A,AA: CondInt;
begin
  if CacheSize > 0 then FlushCache(On);
  K := Idx1 div MaxBlockSize;
  KK := Idx1 mod MaxBlockSize;
  B1 := Blocks[K]^[KK];
  A := Idx2 div MaxBlockSize;
  AA := Idx2 mod MaxBlockSize;
  Blocks[K]^[KK] := Blocks[A]^[AA];
  Blocks[A]^[AA] := B1;
end;

destructor TBlocker.Done;
  var I: CondInt;
begin
  FlushCache(On);
  for I := 0 to 29 do
    if Blocks[I] <> nil then FreeMem(Blocks[I], SizeOf(TNumBlock));
{$IFNDEF NOEXTRA}
  if XMSStream <> nil then Dispose(XMSStream,Done); XMSStream := nil;
  if EMSStream <> nil then Dispose(EMSStream,Done); EMSStream := nil;
  EMSCurHandle := $FFFF;
  EMSCurPage := $FFFF;
{$ENDIF}
  if MemStream <> nil then Dispose(MemStream,Done); MemStream := nil;
end;
*)

function GetCollector(MaxSize, MaxLines: LongInt): PCollector;
begin
{$IFNDEF NOEXTRA}
   if (XMSFound and (XMSFree > 1) and (EditorDefaults.EdOpt and ebfXMS <> 0)
     or EMSFound and (EMSFreePages > 1) and (EditorDefaults.EdOpt and ebfEMS <> 0))
      and MemOK then
     GetCollector := New(PBlocker, Init(MaxSize)) else
{$ENDIF}
       {GetCollector := New(PStdCollector, Init(MaxLines))}
        GetCollector := New(PStdCollector, Init(MaxLines+200, 1000, True))
end;

end.
