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
{$I STDEFINE.INC}
unit BigArray;
(* Autor -- SeYKo, 1 Mach 2000 *)

interface
Uses
  Objects;

type

{ TArray object }

  PArray = ^TArray;
  TArray = object(TObject)
    constructor Init(AMaxCount, AItemSize: Word);
    destructor  Done; virtual;
{    procedure Load(var S: TStream); }
{    procedure Store(var S: TStream);}
    function  Count:    Word;
    function  MaxCount: Word;
    function  ItemSize: Word;
    function  At(Index: Word): Pointer;
    function  AtArray(Index: Word; var ACount: Word): Pointer;
    procedure AtGet(Index, ACount: Word; Item: Pointer);
    procedure AtDelete(Index, ACount: Word; Item: Pointer);
    procedure AtInsert(Index, ACount: Word; Item: Pointer);
    procedure AtReplace(Index, ACount: Word; Item: Pointer);
    procedure Error(Code, Info: Integer); virtual;
  private
    _MaxCount: Word;
    _ItemSize: Word;
    CurCount:  Word; { 0 <= CurCount <= MaxCount }
    UpIndex:   Word; { 0 <= UpIndex <= CurCount }
    Items:   PChar;
    UpItems: PChar; { Items <= UpItems <= Items + MaxCount*ItemSize }
    function  CheckFail(Index, ACount: Word): Boolean;
    procedure SplitAt(Index: Word);
  end;

type

{ TBigArray object }

  PBigArrayNode = ^TBigArrayNode;
  TBigArrayNode = object(TArray)
    Next: PBigArrayNode;
    Prev: PBigArrayNode;
    constructor Init(AMaxCount, AItemSize: Word);
    destructor  Done; virtual;
  end;

  PBigArray = ^TBigArray;
  TBigArray = object(TObject)
    constructor Init(AItemSize, AMaxNodeCount: Word);
    destructor  Done; virtual;
    function  Count:    LongInt;
    function  MaxCount: LongInt;
    function  ItemSize: Word;
    function  At(Index: LongInt): Pointer;
    function  AtArray(Index: LongInt; var ACount: Word): Pointer;
    procedure AtGet(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtDelete(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtInsert(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtReplace(Index: LongInt; ACount: Word; Item: Pointer);
    procedure InsertFrom(IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray);
    procedure ReplaceFrom(IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray);
    procedure DeleteTo(IndexF,IndexT,ACount: LongInt; ArrayT: PBigArray);
    procedure Error(Code: Integer; Info: LongInt); virtual;
  private
    _MaxCount: LongInt;
    CurCount:  LongInt; { 0 <= CurCount <= MaxCount }
    CurIndex:  LongInt; { Index of first Item in CurNode }
    CurNode:   PBigArrayNode;
    function  CheckFail(Index, ACount: LongInt): Boolean;
    procedure SeekTo(Index: LongInt);
  end;

const

{ TArray error codes }

  coIndexError   = -1;          { Index out of range }
  coIndex1Range  = -2;
  coIndex2Range  = -3;
  coCount1Range  = -4;
  coCount2Range  = -5;
  coNilPointer   = -6;
  coStreamError  = -7;
  coGetMemFailed = -8;

implementation
Uses Drivers2,
  {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF};

  const MaxArrayBytes = 65520;

constructor TArray.Init(AMaxCount, AItemSize: Word);
var
  S: LongInt;
begin
  inherited Init;
  S:=LongInt(AMaxCount) * AItemSize;
  if (AMaxCount = 0) or
     (S <= 0) or (S > MaxArrayBytes)
  then begin
    Error(coIndexError,-1); Fail;
  end;
  GetMem(Items, S); if Items=nil then Fail;
  _MaxCount:=AMaxCount; _ItemSize:=AItemSize;
  { CurCount:=0; UpIndex:=0; }
  UpItems:=Items + (_MaxCount-(CurCount-UpIndex))*_ItemSize;
end;

destructor TArray.Done;
begin
  if (Items<>nil) then begin
    FreeMem(Items, _MaxCount * _ItemSize); { Items:=nil; }
    UpItems:=nil;
    _MaxCount:=0; CurCount:=0;
    _ItemSize:=0; UpIndex:=0;
  end;
end;

{procedure TArray.Load(var S: TStream);
var
  tMaxCount, tItemSize, tCurCount, tUpIndex: Word;
  S_OLD, S_NEW: LongInt;
begin
  S.Read(tMaxCount, Sizeof(tMaxCount));
  S.Read(tItemSize, Sizeof(tItemSize));
  S.Read(tCurCount, Sizeof(tCurCount));
  S.Read(tUpIndex,  Sizeof(tUpIndex));
  S_OLD:=LongInt(_MaxCount) * _ItemSize;
  S_NEW:=LongInt(tMaxCount) * tItemSize;
  if (S_NEW > MaxArrayBytes) or
     (S_NEW = 0) or
     (tCurCount > tMaxCount) or
     (tUpIndex  > tCurCount)
  then begin
    Error(coStreamError, 0);
    Exit;
  end;
  if (S_OLD <> S_NEW) or (Items=nil) then begin
    if (Items<>nil) then FreeMem(Items, S_OLD);
    GetMem(Items, S_NEW);
    if Items=nil then begin
      Error(coGetMemFailed, 0);
      _MaxCount:=0; _ItemSize:=0;
      CurCount :=0;  UpIndex:=0;
      UpItems:=Items+(_MaxCount-(CurCount-UpIndex))*_ItemSize;
      Exit;
    end;
  end;
  _MaxCount:=tMaxCount;
  _ItemSize:=tItemSize;
  CurCount :=tCurCount;
  UpIndex  :=tUpIndex;
  S.Read(Items^, S_NEW);
  UpItems:=Items+(_MaxCount-(CurCount-UpIndex))*_ItemSize;
end;}

{procedure TArray.Store(var S: TStream);
begin
  S.Write(_MaxCount, Sizeof(_MaxCount));
  S.Write(_ItemSize, Sizeof(_ItemSize));
  S.Write(CurCount, Sizeof(CurCount));
  S.Write(UpIndex, Sizeof(UpIndex));
  S.Write(Items^, _MaxCount * _ItemSize);
end;}

function TArray.Count: Word;
begin
  Count:=CurCount;
end;

function TArray.MaxCount: Word;
begin
  MaxCount:=_MaxCount;
end;

function TArray.ItemSize: Word;
begin
  ItemSize:=_ItemSize;
end;

function TArray.CheckFail(Index, ACount: Word): Boolean;
begin
  if (ACount = 0) or
     (LongInt(Index)+ACount > CurCount)
  then begin
    Error(coIndexError, Index);
    CheckFail:=True;
  end else
    CheckFail:=False;
end;

procedure TArray.SplitAt(Index: Word);
var
  tFrom, tTo: PChar;
  tMoveBytes: Word;
begin
  if Index=UpIndex then Exit;
  if Index < UpIndex then begin
    tMoveBytes:=(UpIndex-Index)* _ItemSize;
    tFrom:=Items+Index* _ItemSize;
    tTo:=Items+ (_MaxCount-(CurCount-Index))*_ItemSize;
    UpItems:=tTo;
  end
  else begin
    tMoveBytes:=(Index-UpIndex)* _ItemSize;
    tFrom:=UpItems;
    tTo:=Items+UpIndex* _ItemSize;
    UpItems:=Items+(_MaxCount-(CurCount-Index))*_ItemSize;
  end;
  StrMove(tTo,tFrom,tMoveBytes); UpIndex:=Index;
end;

function TArray.At(Index: Word): Pointer;
begin
  if (Index >= CurCount) then begin
    Error(coIndexError, Index);
    Index:=CurCount - 1;
  end;
  if Index < UpIndex then
    At:=Items + Index* _ItemSize else
    At:=UpItems + (Index - UpIndex) * _ItemSize;
end;

function TArray.AtArray(Index: Word; var ACount: Word): Pointer;
begin
  if (ACount = 0) then
    AtArray:=nil
  else begin
    if (Index >= CurCount) then begin
      Error(coIndexError, Index);
      Index:=CurCount - 1;
    end;
    if (LongInt(Index)+ACount > CurCount) then
      ACount:=CurCount-Index;
    SplitAt(Index+ACount);
    AtArray:=Items + Index * _ItemSize;
  end;
end;

procedure TArray.AtGet(Index, ACount: Word; Item: Pointer);
begin
  if CheckFail(Index,ACount) or (Item=nil) then Exit;
  SplitAt(Index+ACount);
  StrMove(Item,Items + Index * _ItemSize,ACount* _ItemSize);
end;

procedure TArray.AtDelete(Index, ACount: Word; Item: Pointer);
begin
  if CheckFail(Index, ACount) then Exit;
  SplitAt(Index+ACount);
  if Item<>nil then
    StrMove(Item,Items + Index * _ItemSize,ACount* _ItemSize);
  Dec(UpIndex,ACount); Dec(CurCount,ACount);
end;

procedure TArray.AtInsert(Index, ACount: Word; Item: Pointer);
begin
  if (Index > CurCount) or
     (ACount = 0) or
     (LongInt(CurCount)+ACount > MaxCount) or
     (Item=nil)
  then begin
    Error(coIndexError, Index); Exit;
  end;
  SplitAt(Index);
  StrMove(Items + Index * _ItemSize,Item,ACount* _ItemSize);
  Inc(UpIndex,ACount); Inc(CurCount,ACount);
end;

procedure TArray.AtReplace(Index, ACount: Word; Item: Pointer);
begin
  if CheckFail(Index, ACount) or (Item=nil) then Exit;
  SplitAt(Index+ACount);
  StrMove(Items + Index * _ItemSize,Item,ACount* _ItemSize);
end;

procedure TArray.Error(Code, Info: Integer);
begin
  WriteLn('TArray.Error: Code=',Code,' Info=',Info);
  DoDump($FF+Code, nil);
end;

constructor TBigArrayNode.Init(AMaxCount, AItemSize: Word);
begin
  inherited Init(AMaxCount, AItemSize);
  { Next:=nil; Prev:=nil; Pack:=nil; }
end;

destructor TBigArrayNode.Done;
begin
  Prev:=nil; Next:=nil;
  inherited Done;
end;

constructor TBigArray.Init(AItemSize, AMaxNodeCount: Word);
begin
  inherited Init;
  CurNode:=New(PBigArrayNode, Init(AMaxNodeCount,AItemSize));
  if CurNode=nil then begin
    Error(coGetMemFailed, 0); Fail; Exit;
  end;
  _MaxCount:=AMaxNodeCount;
  { CurCount:=0; CurIndex:=0; }
end;

destructor TBigArray.Done;
begin
  if CurCount<>0 then DeleteTo(0,0,CurCount,nil);
  FreeObject(CurNode);
  _MaxCount:=0; CurCount:=0; CurIndex:=0;
  inherited Done;
end;

function TBigArray.Count: LongInt;
begin
  Count:=CurCount;
end;

function TBigArray.MaxCount: LongInt;
begin
  MaxCount:=_MaxCount;
end;

function TBigArray.ItemSize: Word;
begin
  ItemSize:=CurNode^.ItemSize;
end;

function TBigArray.CheckFail(Index, ACount: LongInt): Boolean;
begin
  if (ACount = 0) or
     (Index+ACount <= 0) or
     (Index+ACount > CurCount)
  then begin
    Error(coIndexError, Index);
    CheckFail:=True;
  end else
    CheckFail:=False;
end;

procedure TBigArray.SeekTo(Index: LongInt);
begin
  while Index < CurIndex do begin
    CurNode:=CurNode^.Prev;
    Dec(CurIndex,CurNode^.Count);
  end;
  while (Index >= CurIndex + CurNode^.Count) and
    (CurNode^.Count <> 0)
  do begin
    Inc(CurIndex,CurNode^.Count);
    CurNode:=CurNode^.Next;
  end;
end;

function TBigArray.At(Index: LongInt): Pointer;
begin
  if CheckFail(Index, 1) then Index:=CurCount - 1;
  SeekTo(Index);
  At:=CurNode^.At(Index-CurIndex);
end;

function TBigArray.AtArray(Index: LongInt; var ACount: Word): Pointer;
begin
  if (ACount = 0) then
    AtArray:=nil
  else begin
    if CheckFail(Index, 1) then Index:=CurCount - 1;
    SeekTo(Index);
    AtArray:=CurNode^.AtArray(Index-CurIndex,ACount);
  end;
end;

procedure TBigArray.AtGet(Index: LongInt; ACount: Word; Item: Pointer);
var
  tIndex, tCount: Word;
begin
  if CheckFail(Index,ACount) or (Item=nil) then Exit;
  while ACount<>0 do begin
    SeekTo(Index);
    tIndex:=Index-CurIndex;
    tCount:=CurNode^.Count - tIndex;
    if tCount > ACount then tCount:=ACount;
    if tCount<>0 then begin
      CurNode^.AtGet(tIndex,tCount,Item);
      Item:=PChar(Item)+tCount*CurNode^.ItemSize;
      Dec(ACount,tCount);
    end;
    Inc(Index,tCount);
  end;
end;

procedure TBigArray.AtDelete(Index: LongInt; ACount: Word; Item: Pointer);
var
  tIndex, tCount, tCount2, tFree: Word;
  tNode: PBigArrayNode;
  P: PChar;
begin
  if CheckFail(Index,ACount) then Exit;
  while ACount<>0 do begin
    SeekTo(Index);
    tIndex:=Index-CurIndex;
    tCount:=CurNode^.Count - tIndex;
    if tCount > ACount then tCount:=ACount;
    if tCount<>0 then
    begin
      CurNode^.AtDelete(tIndex,tCount,Item);
      if Item<>nil then Item:=PChar(Item)+tCount*CurNode^.ItemSize;
      Dec(ACount,tCount); Dec(CurCount,tCount);
      if CurNode^.Count=0 then
      begin
        if CurNode^.Next<>nil then begin
          Dec(_MaxCount,CurNode^._MaxCount); tNode:=CurNode;
          CurNode^.Next^.Prev:=CurNode^.Prev;
          if CurNode^.Prev<>nil then CurNode^.Prev^.Next:=CurNode^.Next;
          CurNode:=CurNode^.Next;
          FreeObject(tNode);
        end else
        if CurNode^.Prev<>nil then begin
          Dec(_MaxCount,CurNode^._MaxCount); tNode:=CurNode;
          CurNode^.Prev^.Next:=CurNode^.Next;
          if CurNode^.Next<>nil then CurNode^.Next^.Prev:=CurNode^.Prev;
          CurNode:=CurNode^.Prev; Dec(CurIndex,CurNode^.Count);
          FreeObject(tNode);
        end;
      end else
      if (CurNode^.Next <> nil) then
      begin
        tCount:=CurNode^.Count; tFree:=CurNode^.MaxCount - tCount;
        tNode:=CurNode^.Next; tCount2:=tNode^.Count;
        if tFree >= tCount2 then begin
          P:=tNode^.AtArray(0,tCount2);
          CurNode^.AtInsert(tCount,tCount2,P);
          CurNode^.Next:=tNode^.Next;
          if tNode^.Next<>nil then tNode^.Next^.Prev:=CurNode;
          Dec(_MaxCount,tNode^._MaxCount);
          FreeObject(tNode);
        end;
        Inc(Index,tCount);
      end else
        Inc(Index,CurNode^.Count);
    end else
      Inc(Index,CurNode^.Count);
  end;
end;

procedure TBigArray.AtInsert(Index: LongInt; ACount: Word; Item: Pointer);
var
  tIndex, tCount, tCount2, tFree: Word;
  tNode: PBigArrayNode;
  P: PChar;
  BigInsert: Boolean;
begin
  if (Index < 0) or
     (Index > CurCount) or
     (ACount = 0) or
     (Item=nil)
  then begin
    Error(coIndexError, Index); Exit;
  end;
  if Index=0 then begin
    SeekTo(0); tIndex:=0;
  end else
  begin
    SeekTo(Index-1); tIndex:=Index-CurIndex;
    if tIndex=CurNode^.MaxCount then begin
      if CurNode^.Next=nil then begin
        tNode:=New(PBigArrayNode, Init(CurNode^.MaxCount,CurNode^.ItemSize));
        tNode^.Prev:=CurNode;
        CurNode^.Next:=tNode;
        Inc(_MaxCount,tNode^._MaxCount);
      end;
      SeekTo(Index); tIndex:=0;
    end;
  end;
  tFree:=CurNode^.MaxCount - CurNode^.Count;
  if tFree < ACount then BigInsert:=True else BigInsert:=False;
  if BigInsert and (tIndex < CurNode^.Count) then
  begin
    tCount:=CurNode^.Count - tIndex;
    P:=CurNode^.AtArray(tIndex,tCount);
    tNode:=New(PBigArrayNode, Init(CurNode^.MaxCount,CurNode^.ItemSize));
    tNode^.Prev:=CurNode; tNode^.Next:=CurNode^.Next;
    if CurNode^.Next<>nil then CurNode^.Next^.Prev:=tNode;
    CurNode^.Next:=tNode;
    tNode^.AtInsert(0, tCount, P); CurNode^.AtDelete(tIndex,tCount,nil);
    Inc(_MaxCount,tNode^._MaxCount);
  end;
  while True do begin
    tCount:=CurNode^.MaxCount - tIndex;
    if tCount > ACount then tCount:=ACount;
    CurNode^.AtInsert(tIndex,tCount,Item);
    Dec(ACount,tCount); Inc(CurCount,tCount);
    if ACount=0 then break;
    Item:=PChar(Item)+tCount*CurNode^.ItemSize; tIndex:=0;
    tNode:=New(PBigArrayNode, Init(CurNode^.MaxCount,CurNode^.ItemSize));
    tNode^.Prev:=CurNode; tNode^.Next:=CurNode^.Next;
    if CurNode^.Next<>nil then CurNode^.Next^.Prev:=tNode;
    CurNode^.Next:=tNode; Inc(_MaxCount,tNode^._MaxCount);
    Inc(CurIndex,CurNode^.Count); CurNode:=tNode;
  end;
  if BigInsert and (CurNode^.Next <> nil) then
  while True do
  begin
    tCount:=CurNode^.Count; tFree:=CurNode^.MaxCount - tCount;
    tNode:=CurNode^.Next; tCount2:=tNode^.Count;
    if tFree < tCount2 then break;
    P:=tNode^.AtArray(0,tCount2);
    CurNode^.AtInsert(tCount,tCount2,P);
    CurNode^.Next:=tNode^.Next;
    if tNode^.Next<>nil then tNode^.Next^.Prev:=CurNode;
    Dec(_MaxCount,tNode^._MaxCount);
    FreeObject(tNode);
  end;
end;

procedure TBigArray.AtReplace(Index: LongInt; ACount: Word; Item: Pointer);
var
  tIndex, tCount: Word;
begin
  if CheckFail(Index,ACount) or (Item=nil) then Exit;
  while ACount<>0 do begin
    SeekTo(Index);
    tIndex:=Index-CurIndex;
    tCount:=CurNode^.Count - tIndex;
    if tCount > ACount then tCount:=ACount;
    if tCount<>0 then begin
      CurNode^.AtReplace(tIndex,tCount,Item);
      Item:=PChar(Item)+tCount*CurNode^.ItemSize;
      Dec(ACount,tCount);
    end;
    Inc(Index,tCount);
  end;
end;

procedure TBigArray.InsertFrom(
  IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray);
var
  tCount: Word;
  P: PChar;
begin
  if ArrayF = nil then begin Error(coNilPointer,0); Exit; end;
  if (IndexF < 0) or (IndexF >= ArrayF^.Count) then begin
    Error(coIndex2Range,IndexF); Exit;
  end;
  if (ACount <= 0) or
     (IndexF+ACount <=0) or
     (IndexF+ACount > ArrayF^.Count)
  then begin
    Error(coCount2Range,ACount); Exit;
  end;
  if (IndexT < 0) or (IndexT > CurCount) then begin
    Error(coIndex1Range,IndexT); Exit;
  end;
  while ACount <> 0 do begin
    tCount:=$FFFF; if tCount > ACount then tCount:=ACount;
    P:=ArrayF^.AtArray(IndexF,tCount);
    AtInsert(IndexT,tCount,P);
    Inc(IndexF,tCount); Inc(IndexT,tCount); Dec(ACount,tCount);
  end;
end;

procedure TBigArray.ReplaceFrom(
  IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray);
var
  tCount: Word;
  P: PChar;
begin
  if ArrayF = nil then begin Error(coNilPointer,0); Exit; end;
  if (IndexF < 0) or (IndexF >= ArrayF^.Count) then begin
    Error(coIndex2Range,IndexF); Exit;
  end;
  if (ACount <= 0) or
     (IndexF+ACount <=0) or
     (IndexF+ACount > ArrayF^.Count)
  then begin
    Error(coCount2Range,ACount); Exit;
  end;
  if (IndexT < 0) or (IndexT >= CurCount) then begin
    Error(coIndex1Range,IndexT); Exit;
  end;
  if (IndexT+ACount <=0) or
     (IndexT+ACount > CurCount)
  then begin
    Error(coCount1Range,ACount); Exit;
  end;
  while ACount <> 0 do begin
    tCount:=$FFFF; if tCount > ACount then tCount:=ACount;
    P:=ArrayF^.AtArray(IndexF,tCount);
    AtReplace(IndexT,tCount,P);
    Inc(IndexF,tCount); Inc(IndexT,tCount); Dec(ACount,tCount);
  end;
end;

procedure TBigArray.DeleteTo(
  IndexF,IndexT,ACount: LongInt; ArrayT: PBigArray);
var
  tCount: Word;
  P: PChar;
begin
  if (IndexF < 0) or (IndexF >= CurCount) then begin
    Error(coIndex1Range,IndexF); Exit;
  end;
  if (ACount <= 0) or
     (IndexF+ACount <=0) or
     (IndexF+ACount > CurCount)
  then begin
    Error(coCount1Range,ACount); Exit;
  end;
  if (ArrayT<>nil) and ((IndexT < 0) or (IndexT > ArrayT^.Count)) then begin
    Error(coIndex2Range,IndexT); Exit;
  end;
  while ACount <> 0 do begin
    tCount:=$FFFF; if tCount > ACount then tCount:=ACount;
    if ArrayT<>nil then begin
      P:=AtArray(IndexF,tCount);
      ArrayT^.AtInsert(IndexT,tCount,P);
    end;
    AtDelete(IndexF,tCount,nil);
    Inc(IndexT,tCount); Dec(ACount,tCount);
  end;
end;

procedure TBigArray.Error(Code: Integer; Info: LongInt);
begin
  WriteLn('TBigArray.Error: Code=',Code,' Info=',Info);
  DoDump($F0+Code, nil);
end;

end.
