{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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
{-----------------------------------------------------}
{ This module is based on Turbo Vision Objects Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}

unit Collect;

interface
Uses
  {$IFDEF BIGCOLLECTION}BigArray,{$ENDIF}
  Objects;

const
  {$IFNDEF COLLECTION_IN_STREAM}
   {$IFNDEF BIGCOLLECTION}
   MaxCollectionSize = MaxBytes DIV SizeOf(Pointer);
   {$ELSE}
   NumNodes = {MaxArrayBytes div SizeOf(Pointer)}128;
   MaxCollectionSize = MaxBytes DIV SizeOf(Pointer) * NumNodes;
   {$ENDIF}
  {$ELSE}
  MaxCollectionSize = $7FFF0000;
  {$ENDIF}


{ TCollection error codes }

  coIndexError = -1;              { Index out of range }
  coOverflow   = -2;              { Overflow }

type
  {$IFNDEF COLLECTION_IN_STREAM}{$IFNDEF BIGCOLLECTION}
  PItemList = ^TItemList;
  TItemList = array[0..MaxCollectionSize - 1] of Pointer;
  {$ENDIF}{$ENDIF}

{ TCollection object }

  PCollection = ^TCollection;
  TCollection = object(TObject)
    {$IFNDEF COLLECTION_IN_STREAM}
     {$IFNDEF BIGCOLLECTION}
     Items: PItemList;
     {$ELSE}
     BigArrs: Word;
     Items: PBigArray;
     {$ENDIF}
    {$ELSE}
    CStream: PStream;
    {$ENDIF}
    Count: LongInt;
    Limit: LongInt;
    Delta: LongInt;
    Status, ErrorInfo: Integer;
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    destructor  Done; virtual;
    function  At(Index: LongInt): Pointer;
    procedure AtDelete(Index: LongInt);
    procedure AtFree(Index: LongInt);
    procedure AtInsert(Index: LongInt; Item: Pointer);
    procedure AtPut(Index: LongInt; Item: Pointer);
    procedure AtReplace(Index: LongInt; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure DeleteAll;
    procedure Error(Code, Info: Integer); virtual;
    function  FirstThat(Test: Pointer): Pointer;
    procedure ForEach(Action: Pointer);
    procedure Free(Item: Pointer);
    procedure FreeAll;
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    function  IndexOf(Item: Pointer): LongInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function  LastThat(Test: Pointer): Pointer;
    procedure Pack;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    procedure SetLimit(ALimit: LongInt); virtual;
    procedure Store(var S: TStream);
  end;

{ TSortedCollection object }

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    Duplicates: Boolean;
    constructor Init(ALimit, ADelta: LongInt);
    constructor Load(var S: TStream);
    function  Compare(Key1, Key2: Pointer): Integer; virtual;
    function  IndexOf(Item: Pointer): LongInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function  KeyOf(Item: Pointer): Pointer; virtual;
    function  Search(Key: Pointer; var Index: LongInt): Boolean; virtual;
    procedure Store(var S: TStream);
    procedure Sort;
{$IFDEF QSort}
    procedure QSort;
    procedure QuickSort(L, R: LongInt);
{$ELSE}
 {$IFDEF POGLSORT}
    procedure PoglSort;
 {$ELSE}
    procedure KPutSort;
 {$ENDIF}
{$ENDIF}
  end;

{ TLineCollection }

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
  end;


{ TStringCollection object }

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    function  Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TStrCollection object }

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    function  Compare(Key1, Key2: Pointer): Integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TUnSortedStrCollection - UNSORTED STRING COLLECTION OBJECT }

{ This is a completely >> NEW << object which holds a collection of  }
{ strings but does not alphabetically sort them. It is a very useful }
{ object for insert ordered list boxes!                              }

   PUnSortedStrCollection = ^TUnSortedStrCollection;
   TUnSortedStrCollection = object(TStringCollection)
      procedure Insert(Item: Pointer); virtual;
   end;

{ TResourceCollection object }

  PResourceCollection = ^TResourceCollection;
  TResourceCollection = object(TStringCollection)
    procedure FreeItem(Item: Pointer); virtual;
    function  GetItem(var S: TStream): Pointer; virtual;
    function  KeyOf(Item: Pointer): Pointer; virtual;
    procedure PutItem(var S: TStream; Item: Pointer); virtual;
  end;

{ TResourceFile object }

  PResourceFile = ^TResourceFile;
  TResourceFile = object(TObject)
    Stream: PStream;
    Modified: Boolean;
    constructor Init(AStream: PStream);
    destructor  Done; virtual;
    function  Count: LongInt;
    procedure Delete(Key: String);
    procedure Flush;
    function  Get(Key: String): PObject;
    function  KeyAt(I: LongInt): String;
    procedure Put(Item: PObject; Key: String);
    function  SwitchTo(AStream: PStream; Pack: Boolean): PStream;
  private
    BasePos: Longint;
    IndexPos: Longint;
    Index: TResourceCollection;
  end;

{ TStringList object }

  TStrIndexRec = record
    Key, Count, Offset: AWord;
  end;

  PStrIndex = ^TStrIndex;
  TStrIndex = array[0..9999] of TStrIndexRec;

  PStringList = ^TStringList;
  TStringList = object(TObject)
    constructor Load(var S: TStream);
    destructor Done; virtual;
    function Get(Key: AWord): String;
  private
    Stream: PStream;
    BasePos: Longint;
    IndexSize: AWord;
    Index: PStrIndex;
    procedure ReadStr(var S: String; Offset, Skip: AWord);
  end;

        {-DataCompBoy-}
 Type PDirCol = ^TDirCol;
      TDirCol = Object (TStringCollection)
                 function Compare(Key1, Key2: pointer): integer; virtual;
                end;
        {-DataCompBoy-}

implementation
uses
  Advance1 {DisposeStr},
  {$IFDEF Windows} WinProcs, OMemory,
  {$ELSE}          Memory,            {$ENDIF}
  {$IFNDEF NONBP}BStrings,{$ELSE}Strings,{$ENDIF}
  CallSpcb, ExtraMem;

{$IFDEF Windows} {$DEFINE NewExeFormat} {$ENDIF}
{$IFDEF DPMI}    {$DEFINE NewExeFormat} {$ENDIF}

{$IFNDEF COLLECTION_IN_STREAM}
constructor TCollection.Init(ALimit, ADelta: LongInt);
begin
  inherited Init;
  Delta:=ADelta;
  SetLimit(ALimit);
end;

constructor TCollection.Load (var S: TStream);
var
  C, I: LongInt;
begin
  TObject.Init;
  S.Read(Count, Sizeof(Count));
  S.Read(Limit, Sizeof(Limit));
  S.Read(Delta, Sizeof(Delta));
  if (Count > Limit) or (Delta < 0) then Fail;
  C:=Count; I:=Limit;
  Count:=0; Limit:=0;
  SetLimit(I);
  for I:=0 to C-1 do begin
    AtInsert(I,GetItem(S));
    if (S.Status<>stOK) then
    begin
      SetLimit(0); Fail;
    end;
  end;
end;

function TCollection.At(Index: LongInt): Pointer;
begin
  if (Index < 0) or (Index >= Count) or (Items = nil)
  then begin
    Error(coIndexError, Index);
    At:=Nil;
  end else
    {$IFDEF BIGCOLLECTION}
     Items^.AtGet(Index, 1, @Index);
     At:=Pointer(Index);
    {$ELSE}
     At:=Items^[Index];
    {$ENDIF}
end;

procedure TCollection.AtDelete(Index: LongInt);
begin
  if (Index >= 0) and (Index < Count) and (Items<>nil) then
  begin
    Dec(Count);
    if Count > Index then
     {$IFDEF BIGCOLLECTION}
     Items^.AtDelete(Index, 1, nil);
     {$ELSE}
     Move(Items^[Index+1],Items^[Index], (Count-Index)*Sizeof(Pointer));
     {$ENDIF}
  end else
    Error(coIndexError, Index);
end;

procedure TCollection.AtInsert(Index: LongInt; Item: Pointer);
var
  I: LongInt;
begin
  if (Index >= 0) and (Index <= Count) then begin
    if Count=Limit then SetLimit(Limit+Delta);
    if Limit>Count then begin
      {$IFNDEF BIGCOLLECTION}
      if Index < Count then
        for I:=Count-1 DownTo Index do Items^[I+1]:=Items^[I];
      Items^[Index]:=Item;
      {$ELSE}
      Items^.AtInsert(Index, 1, @Item);
      {$ENDIF}
      Inc(Count);
    end else Error(coOverflow, Index);
  end else Error(coIndexError, Index);
end;

procedure TCollection.AtPut(Index: LongInt; Item: Pointer);
begin
  if (Index >= 0) and (Index < Count) and (Items <> nil)
   then {$IFNDEF BIGCOLLECTION}
        Items^[Index]:=Item
        {$ELSE}
        Items^.AtReplace(Index, 1, @Item)
        {$ENDIF}
   else Error(coIndexError, Index);
end;

procedure TCollection.SetLimit(ALimit: LongInt);
  {$IFNDEF BIGCOLLECTION}
var
  AItems: PItemList;
begin
  if ALimit < Count then ALimit:=Count;
  if ALimit > MaxCollectionSize then ALimit:=MaxCollectionSize;
  if ALimit <> Limit then
  begin
    if ALimit = 0 then AItems:=nil else
    begin
      GetMem(AItems, ALimit * SizeOf(Pointer));
      if (AItems<>Nil) then FillChar(AItems^,ALimit * SizeOf(Pointer), #0);
    end;
    if (AItems<>Nil) or (ALimit=0) then begin
      if (AItems <>Nil) and (Items <> Nil) and (Count<>0) then
        Move(Items^, AItems^, Count*SizeOf(Pointer));
      If (Limit <> 0) and (Items <> Nil) Then
        FreeMem(Items, Limit * SizeOf(Pointer));
    end;
    Items:=AItems;
    Limit:=ALimit;
  end;
  {$ELSE}
begin
 if ALimit = 0 then begin
  if Items <> nil then Dispose(Items, Done);
  Items := nil;
  Limit := 0;
 end else begin
  if ALimit < Count then ALimit := Count;
  if ALimit > MaxCollectionSize then ALimit := MaxCollectionSize;
  Limit := ALimit;
  if Items = nil then New(Items, Init(SizeOf(Pointer), NumNodes));
 end;
  {$ENDIF}
end;

{$ELSE}
constructor TCollection.Init(ALimit, ADelta: LongInt);
begin
  inherited Init;
  Delta:=ADelta;
  SetLimit(ALimit);
  if (ALimit<>0) and (CStream = nil) then Fail;
end;

constructor TCollection.Load(var S: TStream);
var
  C, I: LongInt;
begin
  TObject.Init;
  S.Read(Count, Sizeof(Count));
  S.Read(Limit, Sizeof(Limit));
  S.Read(Delta, Sizeof(Delta));
  if (Count > Limit) or (Delta < 0) then Fail;
  C:=Count; I:=Limit;
  Count:=0; Limit:=0;
  SetLimit(I);
  if (I<>0) and (CStream = nil) then Fail;
  Count:=C;
  for I:=0 to C-1 do begin
    AtPut(I,GetItem(S));
    if (S.Status<>stOK) or (CStream^.Status<>stOK) then
    begin
      SetLimit(0); Fail;
    end;
  end;
end;

function TCollection.At(Index: LongInt): Pointer;
  var Buf: Pointer;
begin
  if (Index < 0) or (Index >= Count)
     or (CStream = nil)
  then begin
    Error(coIndexError,0);
    Buf:=nil;
  end else
  begin
    CStream^.Status:=0;
    CStream^.Seek(Index * sizeOf(Pointer));
    CStream^.Read(Buf, sizeOf(Pointer));
    if (CStream^.Status<>stOK) then
      Buf:=nil;
  end;
  At:=Buf;
end;

procedure TCollection.AtDelete(Index: LongInt);
  var S: PStream;
begin
  if (Index < 0) or (Index >= Count)
     or (CStream = nil)
  then begin
    Error(coIndexError,0);
    Exit;
  end;
  S:=GetAnyMemoryStream; if S=nil then Error(coOverflow,0);
  CStream^.Status:=0;
  if Index>0 then begin
    CStream^.Seek(0);
    S^.CopyFrom(CStream^, Index * sizeOf(Pointer));
  end;
  Dec(Count);
  if Index < Count then begin
    CStream^.Seek((Index + 1) * sizeOf(Pointer));
    S^.CopyFrom(CStream^, (Count - Index) * sizeOf(Pointer));
  end;
  if (CStream^.Status=stOK) and (S^.Status=stOK) then
  begin
    Dispose(CStream,Done);
    CStream:=S;
  end;
end;

procedure TCollection.AtInsert(Index: LongInt; Item: Pointer);
var
  S: PStream;
  L: LongInt;
begin
  if (Index < 0) or (Index > Count)
     or (CStream = nil)
  then begin
    Error(coIndexError,0);
    Exit;
  end;
  CStream^.Status:=0;
  if Index = Count then
  begin
    L:=Index * sizeOf(Pointer);
    CStream^.Seek(L);
    CStream^.Write(Item, sizeOf(Pointer));
  end else
  begin
    S:=GetAnyMemoryStream; if S=nil then begin
      Error(coOverflow,0);
      Exit;
    end;
    CStream^.Seek(0);
    if Index>0 then S^.CopyFrom(CStream^, Index * sizeOf(Pointer));
    S^.Write( Item, sizeOf( Pointer ) );
    S^.CopyFrom( CStream^, (Count - Index) * sizeOf(Pointer));
    if (CStream^.Status=stOK) and (S^.Status=stOK) then
    begin
      Dispose(CStream,Done);
      CStream:=S;
    end;
  end;
  Inc(Count);
end;

procedure TCollection.AtPut(Index: LongInt; Item: Pointer);
begin
  if (Index < 0) or (Index >= Count)
     or (CStream = nil)
  then begin
    Error(coIndexError,0);
    Exit;
  end;
  CStream^.Status:=0;
  CStream^.Seek(Index * sizeOf(Pointer));
  CStream^.Write(Item, sizeOf(Pointer));
end;

procedure TCollection.SetLimit(ALimit: LongInt);
begin
  if ALimit < Count then ALimit:=Count;
  if ALimit > MaxCollectionSize then ALimit:=MaxCollectionSize;
  if ALimit <> Limit then
  begin
   if ALimit = 0 then begin
     FreeAll;
     Dispose(CStream,Done);
     CStream:=nil;
   end else
   if CStream=nil then begin
     CStream:=GetAnyMemoryStream;
     if CStream<>nil then begin
       CStream^.Seek(0);
       CStream^.Truncate;
     end;
   end;
   Limit:=ALimit;
  end;
end;
{$ENDIF}

destructor TCollection.Done;
begin
  FreeAll;
  SetLimit(0);
end;

procedure TCollection.AtFree(Index: LongInt);
var
  Item: Pointer;
begin
  Item:=At(Index);
  AtDelete(Index);
  FreeItem(Item);
end;

procedure TCollection.AtReplace;
var
  P: Pointer;
begin
  If (Limit < Index) and (Delta > 0) then SetLimit(Index + 1);
  while Count < (Index + 1) do Insert(nil);
  P:=At(Index);
  AtPut(Index, Item);
  if P <> nil then FreeItem( P );
end;

procedure TCollection.Delete(Item: Pointer);
begin
  AtDelete(IndexOf(Item));
end;

procedure TCollection.DeleteAll;
begin
  if @Self<>nil then Count:=0;
end;

procedure TCollection.Error(Code, Info: Integer);
begin
  Status:=Code;
  ErrorInfo:=Info;
  {RunError(212 - Code);}
end;

function TCollection.FirstThat(Test: Pointer): Pointer;
var
  I: LongInt;
  P: Pointer;
begin
  for I:=0 to Count-1 do begin
    P:=At(I);
    if Boolean(Byte(Longint(CallPointerLocal(Test,PreviousFramePointer,P))))
    then begin
      FirstThat:=P; Exit;
    end;
  end;
  FirstThat:=nil;
end;

procedure TCollection.ForEach (Action: Pointer);
var
  I: LongInt;
  C: Longint;
begin
  C := Count;
  I := 0;
  while I < Count do begin
   CallPointerLocal(Action,PreviousFramePointer,At(I));
   if C = Count then inc(I) else C := Count;
  end;
end;

procedure TCollection.Free(Item: Pointer);
begin
  Delete(Item);
  FreeItem(Item);
end;

procedure TCollection.FreeAll;
var
  I: LongInt;
begin
  for I:=Count - 1 downto 0 do FreeItem(At(I));
  Count:=0;
end;

procedure TCollection.FreeItem(Item: Pointer);
begin
  FreeObject(Item);
end;

function TCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem:=S.Get;
end;

function TCollection.IndexOf(Item: Pointer): LongInt;
var
  I: LongInt;
begin
  IndexOf:= -1;
  for I:=0 to Count - 1 do
    if At(I) = Item then begin
      IndexOf:=I;
      break;
    end;
end;

procedure TCollection.Insert(Item: Pointer);
begin
  AtInsert(Count, Item);
end;

function TCollection.LastThat(Test: Pointer): Pointer;
var
  I: LongInt;
  P: Pointer;
begin
  for I:=Count-1 DownTo 0 do
  begin
    P:=At(I);
    if Boolean(Byte(Longint(CallPointerLocal(Test,PreviousFramePointer,P))))
    then begin
      LastThat:=P;
      Exit;
    end;
  end;
  LastThat:=nil;
end;

procedure TCollection.Pack;
var
  I: LongInt;
begin
  for I:=Count - 1 downto 0 do if At(I) = nil then AtDelete(I);
end;

procedure TCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Put(Item);
end;

procedure TCollection.Store(var S: TStream);
  procedure DoPutItem(P: Pointer); {$IFDEF BIT_16} far; {$ENDIF}
  begin
    PutItem(S, P);
  end;
begin
  S.Write(Count, Sizeof(Count));
  S.Write(Limit, Sizeof(Limit));
  S.Write(Delta, Sizeof(Delta));
  ForEach(@DoPutItem);
end;

{ TSortedCollection }

constructor TSortedCollection.Init(ALimit, ADelta: LongInt);
begin
  TCollection.Init(ALimit, ADelta);
  Duplicates:=False;
end;

constructor TSortedCollection.Load(var S: TStream);
begin
  inherited Load(S);
  S.Read(Duplicates, SizeOf(Duplicates));
end;

function TSortedCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Abstract;
end;

function TSortedCollection.IndexOf(Item: Pointer): LongInt;
var
  I: LongInt;
begin
  IndexOf:=-1;
  if Search(KeyOf(Item), I) then
  begin
    if Duplicates then
      while (I < Count) and (Item <> At(I)) do Inc(I);
    if I < Count then IndexOf:=I;
  end;
end;

procedure TSortedCollection.Insert(Item: Pointer);
var
  I: LongInt;
begin
  if not Search(KeyOf(Item), I) or Duplicates then AtInsert(I, Item);
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf:=Item;
end;

function TSortedCollection.Search
  (Key: Pointer; var Index: LongInt): Boolean;
var
  L, H, I, C: LongInt;
begin
 Search:=False;
 L:=0;
 H:=Count - 1;
 while L <= H do begin
  I:=(L + H) shr 1;
  C:=Compare(KeyOf(At(I)), Key);
  if C < 0
   then L:=I + 1
   else begin
    if C = 0 then begin
     Search:=True;
     if not Duplicates then L:=I;
     Break;
    end;
    H:=I - 1;
   end;
 end;
 Index:=L;
end;

procedure TSortedCollection.Store(var S: TStream);
begin
  TCollection.Store(S);
  S.Write(Duplicates, SizeOf(Duplicates));
end;

{ TStringCollection }

function TStringCollection.Compare(Key1, Key2: Pointer): Integer;
var
  I, J: Integer;
  P1, P2: PString;
begin
  P1:=PString(Key1);
  P2:=PString(Key2);
  if Length(P1^) < Length(P2^) then
    J:=Length(P1^) else
    J:=Length(P2^);
  I:=1;
  while (I<J) and (P1^[I]=P2^[I]) do Inc(I);
  if (I=J) then begin
    if (P1^[I]<P2^[I]) then Compare:=-1 else
    If (P1^[I]>P2^[I]) Then Compare:= 1 else
    if Length(P1^)>Length(P2^) then Compare := 1 else
    If Length(P1^)<Length(P2^) then Compare :=-1 else
                                    Compare := 0;
  end else if (P1^[I]<P2^[I]) then  Compare :=-1 else
                                    Compare := 1;
end;

procedure TStringCollection.FreeItem(Item: Pointer);
begin
  DisposeStr(PString(Item));
end;

function TStringCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem:=S.ReadStr;
end;

procedure TStringCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.WriteStr(Item);
end;

{ TLineCollection }
procedure TLineCollection.FreeItem(P: Pointer);
begin
  DisposeStr(PString(P));
end;

procedure TLineCollection.PutItem;
begin
 S.WriteStr(Item);
end;

function TLineCollection.GetItem;
begin
 GetItem:=S.ReadStr;
end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare:=StrComp(Key1, Key2);
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(PChar(Item));
end;

function TStrCollection.GetItem(var S: TStream): Pointer;
begin
  GetItem:=S.StrRead;
end;

procedure TStrCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.StrWrite(Item);
end;

procedure TUnSortedStrCollection.Insert(Item: Pointer);
begin
  AtInsert(Count, Item);
end;

{ Private resource manager types }

const
  RStreamMagic: Longint = $52504246; { 'FBPR' }
  RStreamBackLink: Longint = $4C424246; { 'FBBL' }

type
  PResourceItem = ^TResourceItem;
  TResourceItem = record
    Posn: Longint;
    Size: Longint;
    Key: String;
  end;

{ TResourceCollection }

procedure TResourceCollection.FreeItem(Item: Pointer);
begin
  FreeMem(Item, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

function TResourceCollection.GetItem(var S: TStream): Pointer;
var
  Pos: Longint;
  Size: Longint;
  L: Byte;
  P: PResourceItem;
begin
  S.Read(Pos, SizeOf(Pos));
  S.Read(Size, SizeOf(Size));
  S.Read(L, 1);
  GetMem(P, L + (SizeOf(TResourceItem) - SizeOf(String) + 1));
  P^.Posn:=Pos;
  P^.Size:=Size;
  {$IFDEF PPC_DELPHI3}
  SetLength(P^.Key, L);
  {$ELSE}
  P^.Key[0]:=Char(L);
  {$ENDIF}
  S.Read(P^.Key[1], L);
  GetItem:=P;
end;

function TResourceCollection.KeyOf(Item: Pointer): Pointer;
begin
  KeyOf:=@PResourceItem(Item)^.Key; { Pointer to key }
end;

procedure TResourceCollection.PutItem(var S: TStream; Item: Pointer);
begin
  S.Write(PResourceItem(Item)^, Length(PResourceItem(Item)^.Key) +
    (SizeOf(TResourceItem) - SizeOf(String) + 1));
end;

{ TResourceFile }

constructor TResourceFile.Init(AStream: PStream);
type

  {$IFDEF NewExeFormat}
  TExeHeader = record
    eHdrSize:   Word;
    eMinAbove:  Word;
    eMaxAbove:  Word;
    eInitSS:    Word;
    eInitSP:    Word;
    eCheckSum:  Word;
    eInitPC:    Word;
    eInitCS:    Word;
    eRelocOfs:  Word;
    eOvlyNum:   Word;
    eRelocTab:  Word;
    eSpace:     Array[1..30] of Byte;
    eNewHeader: Word;
  end;
  {$ENDIF}

  THeader = record
    Signature: Word;
    case Integer of
      0: (
        LastCount: Word;
        PageCount: Word;
        ReloCount: Word);
      1: (
        InfoType: Word;
        InfoSize: Longint);
  end;
var
  Found, Stop: Boolean;
  Header: THeader;

  {$IFDEF NewExeFormat}
  ExeHeader: TExeHeader;
  {$ENDIF}

begin
  TObject.Init;
  Stream:=AStream;
  Found:=False;
  if (Stream<>Nil) then begin
    BasePos:=Stream^.GetPos;
    repeat
      Stop:=True;
      if BasePos <= Stream^.GetSize - SizeOf(THeader) then
      begin
        Stream^.Seek(BasePos);
        Stream^.Read(Header, SizeOf(THeader));
        case Header.Signature of

        {$IFDEF NewExeFormat}
          $5A4D:
            begin
              Stream^.Read(ExeHeader, SizeOf(TExeHeader));
              BasePos:=ExeHeader.eNewHeader;
              Stop:=False;
            end;
          $454E:
            begin
              BasePos:=Stream^.GetSize - 8;
              Stop:=False;
            end;
          $4246:
            begin
              Stop:=False;
              case Header.Infotype of
                $5250:                                    {Found Resource}
                  begin
                    Found:=True;
                    Stop:=True;
                  end;
                $4C42: Dec(BasePos, Header.InfoSize - 8); {Found BackLink}
                $4648: Dec(BasePos, SizeOf(THeader) * 2); {Found HelpFile}
              else
                Stop:=True;
              end;
            end;
          $424E:
            if Header.InfoType = $3230 then               {Found Debug Info}
            begin
              Dec(BasePos, Header.InfoSize);
              Stop:=False;
            end;

        {$ELSE}
          $5A4D:
            begin
              Inc(BasePos, LongMul(Header.PageCount, 512) -
                (-Header.LastCount and 511));
              Stop:=False;
            end;
          $4246:
            if Header.InfoType = $5250 then Found:=True else
            begin
              Inc(BasePos, Header.InfoSize + 8);
              Stop:=False;
            end;

        {$ENDIF}
        end;
      end;
    until Stop;
  end;
  if Found then
  begin
    Stream^.Seek(BasePos + SizeOf(Longint) * 2);
    Stream^.Read(IndexPos, SizeOf(Longint));
    Stream^.Seek(BasePos + IndexPos);
    Index.Load(Stream^);
  end else
  begin
    IndexPos:=SizeOf(Longint) * 3;
    Index.Init(0, 8);
  end;
end;

destructor TResourceFile.Done;
begin
  Flush;
  Index.Done;
  If (Stream <> nil) then Dispose(Stream,Done); Stream:=nil;
end;

function TResourceFile.Count: LongInt;
begin
  Count:=Index.Count;
end;

procedure TResourceFile.Delete(Key: String);
var
  I: LongInt;
begin
  if Index.Search(@Key, I) then
  begin
    Index.Free(Index.At(I));
    Modified:=True;
  end;
end;

procedure TResourceFile.Flush;
var
  ResSize: Longint;
  LinkSize: Longint;
begin
  if Modified and (Stream <> nil) then
  begin
    Stream^.Seek(BasePos + IndexPos);
    Index.Store(Stream^);
    ResSize:=Stream^.GetPos - BasePos;
    LinkSize:=ResSize + SizeOf(Longint) * 2;
    Stream^.Write(RStreamBackLink, SizeOf(Longint));
    Stream^.Write(LinkSize, SizeOf(Longint));
    Stream^.Seek(BasePos);
    Stream^.Write(RStreamMagic, SizeOf(Longint));
    Stream^.Write(ResSize, SizeOf(Longint));
    Stream^.Write(IndexPos, SizeOf(Longint));
    Stream^.Flush;
  end;
  Modified:=False;
end;

function TResourceFile.Get(Key: String): PObject;
var
  I: LongInt;
begin
  if (Stream = nil) or (not Index.Search(@Key, I)) then Get:=nil else
  begin
    Stream^.Seek(BasePos + PResourceItem(Index.At(I))^.Posn);
    Get:=Stream^.Get;
  end;
end;

function TResourceFile.KeyAt(I: LongInt): String;
begin
  KeyAt:=PResourceItem(Index.At(I))^.Key;
end;

procedure TResourceFile.Put(Item: PObject; Key: String);
var
  I: LongInt;
  P: PResourceItem;
begin
  if Stream = Nil Then Exit;
  if Index.Search(@Key, I) then P:=Index.At(I) else
  begin
    GetMem(P, Length(Key) + (SizeOf(TResourceItem) - SizeOf(String) + 1));
    if (P <> nil) then begin
      P^.Key:=Key;
      Index.AtInsert(I, P);
    end;
  end;
  If (P <> nil) Then Begin
    P^.Posn:=IndexPos;
    Stream^.Seek(BasePos + IndexPos);
    Stream^.Put(Item);
    IndexPos:=Stream^.GetPos - BasePos;
    P^.Size:=IndexPos - P^.Posn;
    Modified:=True;
  end;
end;

function TResourceFile.SwitchTo(AStream: PStream; Pack: Boolean): PStream;
var
  NewBasePos: Longint;

  procedure DoCopyResource(Item: PResourceItem);
  {$IFDEF BIT_16} far; {$ENDIF}
  begin
    Stream^.Seek(BasePos + Item^.Posn);
    Item^.Posn:=AStream^.GetPos - NewBasePos;
    AStream^.CopyFrom(Stream^, Item^.Size);
  end;

begin
  SwitchTo:=Stream;
  If (AStream <> Nil) AND (Stream <> Nil) then begin
    NewBasePos:=AStream^.GetPos;
    if Pack then
    begin
      AStream^.Seek(NewBasePos + SizeOf(Longint) * 3);
      Index.ForEach(@DoCopyResource);
      IndexPos:=AStream^.GetPos - NewBasePos;
    end else
    begin
      Stream^.Seek(BasePos);
      AStream^.CopyFrom(Stream^, IndexPos);
    end;
    Stream:=AStream;
    Modified:=True;
    BasePos:=NewBasePos;
  end;
end;

{ TStringList }

constructor TStringList.Load(var S: TStream);
var
  Size: AWord;
begin
  TObject.Init;
  Stream:=@S;
  S.Read(Size, SizeOf(Size));
  BasePos:=S.GetPos;
  S.Seek(BasePos + Size);
  S.Read(IndexSize, SizeOf(IndexSize));
  GetMem(Index, IndexSize * SizeOf(TStrIndexRec));
  S.Read(Index^, IndexSize * SizeOf(TStrIndexRec));
end;

destructor TStringList.Done;
begin
  FreeMem(Index, IndexSize * SizeOf(TStrIndexRec));
end;

function TStringList.Get(Key: AWord): String;
var
  I: AWord;
  S: String;
begin
  S:='';
  if (IndexSize > 0) then begin
    I:=0;
    while (I < IndexSize) and (S = '') do begin
      {There's a bug in the condition below. It may become True even when
      the Key searched for can't be found after the current position.
      Therefore additional condition must be added here.
          In addition I have modified the original condition to make it more
      similar to the one in TStrListMaker.Put where the structure being read
      by this procedure is being generated. To achieve this a simple
      mathematical rule has been applied:
                     (A - B < C) is equivalent to (A < B + C).
      Now, the problem occurs, when Key < (Index^[I].Key + Index^[I].Count),
      which is the original conditon after rewrite using the above rule, and
      at the same time Key < Index^[I].Key! When the second condition is true
      it's impossible to find the key required at the position currently
      found. Therefore we must make sure, that the second missing condition
      is *not* true, ie. it's true that Key >= Index^[I].Key. -JITR-}
      {if ((Key - Index^[I].Key) < Index^[I].Count) then} {-JITR-}
      if ((Key < (Index^[I].Key + Index^[I].Count)) and   {-JITR-}
        (Key >= Index^[I].Key)) then
        ReadStr(S, Index^[I].Offset, Key-Index^[I].Key);
      Inc(I);
    end;
  end;
  Get:=S;
end;

procedure TStringList.ReadStr(var S: String; Offset, Skip: AWord);
var
  B: Byte;
begin
  Stream^.Seek(BasePos + Offset);
  Stream^.Status:=0;
  Inc(Skip);
  repeat
    Stream^.Read(B, 1);
    {$IFDEF PPC_DELPHI3}
    SetLength(S, B);
    {$ELSE}
    S[0]:=Chr(B);
    {$ENDIF}
    Stream^.Read(S[1],B);
    Dec(Skip);
  until Skip = 0;
end;

procedure TSortedCollection.Sort;
begin
 if Count>1 then
{$IFDEF QSort}
 QSort;
{$ELSE}
 {$IFDEF POGLSORT}
  PoglSort;
 {$ELSE}
  KPutSort;
 {$ENDIF}
{$ENDIF}
end;

{$IFDEF QSort}
var
  P, T: Pointer;

procedure TSortedCollection.QuickSort(L, R: LongInt);
var
  I, J: LongInt;
begin
  repeat
    I:=L;
    J:=R;
    P:=At((L + R) shr 1);
    repeat
      while Compare(At(I), P) < 0 do Inc(I);
      while Compare(At(J), P) > 0 do Dec(J);
      if I <= J then
      begin
        T := At(I); AtPut(I,At(J)); AtPut(J,T);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L:=I;
  until I >= R;
end;

procedure TSortedCollection.QSort;
begin
  if Count > 0 then QuickSort(0, Count-1);
end;

{$ELSE}{Not QSort}

{$IFDEF POGLSORT}

{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
procedure TSortedCollection.PoglSort;
type TSR = pointer; Label 1;
 Const r=SizeOf(TSR); mm=65520 div r; nseg=20; mr=mm*r;
 Type mas = Array[1..mm] of TSR;
 Var A:Array[1..nseg] of ^mas; c,cc,cr,cw,n,np:longint;
     i,j,k,mk,ni,nw,q,u,v,w,z:longint; x:TSR;

{$IFDEF COLLECTION_IN_STREAM}
function GetSize: longint; begin GetSize:=CStream^.GetSize end;
procedure Seek(Pos: longint);begin CStream^.Seek(Pos*r) end;
procedure Read(var A; count: word);begin CStream^.Read(A, count*r) end;
procedure Write(var A; count: word);begin CStream^.Write(A, count*r) end;
function GetPos: longint; begin GetPos:=CStream^.GetPos end;
{$ELSE}
var Posit: longint;
function  GetSize: longint; begin GetSize:=Count*r; end;
procedure Seek(Pos: longint);begin Posit:=Pos end;
procedure Read(var A; count: word);
 begin
  {$IFDEF BIGCOLLECTION}
  Items^.AtGet(Posit, count, @A);
  {$ELSE}
  move(Items^[Posit], A, count*r);
  {$ENDIF}
  inc(Posit, count);
 end;
procedure Write(var A; count: word);
 begin
  {$IFDEF BIGCOLLECTION}
  Items^.AtReplace(Posit, count, @A);
  {$ELSE}
  move(A, Items^[Posit], count*r);
  {$ENDIF}
  inc(Posit, count);
 end;
function GetPos: longint; begin GetPos:=Posit*r end;
{$ENDIF}

 Procedure Vnut(c,cc:longint; m:byte);
  Var og: Array[1..nseg] of word;  i,j,u: word;
  Procedure Merg; Label 1;
   Begin x:= A[u]^[1]; i:=j+j;
    Repeat If i < q-m Then
     If Compare(A[og[i+1]]^[1], A[og[i]]^[1])>0 Then i:=i+1;
     If Compare(A[og[i]]^[1],x)>0 Then og[j]:= og[i] Else Goto 1;
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
      Begin If i < n1 Then If Compare(A[i+1], A[i])>0 Then i:=i+1;
      If Compare(A[i],x)>0 Then A[j]:= A[i] Else Goto 1; j:=i; i:=j+j
      End;  1: A[j]:=x; k:=k-1
    Until k = 0
   End;
  Begin q:=m; Seek(c);
   Repeat mk:=mm; If cc-c < mm Then mk:= cc-c; q:=q+1; z:=q;
    Read(A[q]^,mk); c:= c+mk; Sipka(A[q]^,mk div 2, mk)
   Until c = cc;          Init; np:= mk;
   Repeat  u:=og[1];
    x:=A[q]^[np]; A[q]^[np]:=A[u]^[1]; A[u]^[1]:=x;
    np:=np-1; If u=q Then ni:=np Else ni:=mm; Sipka(A[u]^,1,ni);
    j:=1;  If np = 0 Then Begin np:=mm; q:=q-1; Init End
           Else If q > m+1 Then Merg
   Until q = m
  End;
 Procedure Sli(var B,D,G:mas);
  Begin Repeat If k=mm Then Begin Seek(cw); Write(G,mm); cw:= cw+mm; k:=0 End;
         k:=k+1; If Compare(B[i],D[j])>0 Then Begin G[k]:=D[j]; j:=j+1 End
                 Else Begin G[k]:=B[i]; i:=i+1 End
        Until (i > ni) Or (j > mk)
  End;
 BEGIN
  n:= GetSize div r; q:=n div mm; w:=1; v:=0; c:=0;
  While (MaxAvail >= mr) And (w < nseg) do
   Begin  v:=v+mm; GetMem(A[w], mr); nw:=n-q*mm;
    w:=w+1; If (w > q) And (MaxAvail >= nw*r) Then Goto 1
   End;
  nw:= MaxAvail div r; v:=v+nw-2*mm;  c:= n mod v;
  If c <= 2*mm Then c:= c+v; c:= n-c;
1:If nw > 0 Then GetMem(A[w],nw*r) Else Begin w:=w-1; nw:=mm End;
  Vnut(c,n,0); Seek(c); cw:=c;
  For q:=1 to z do If q=z Then Write(A[q]^, mk)
                   Else Write(A[q]^, mm);
  While c <> 0 do
   Begin cc:=c; c:=c-v; Vnut(c,cc,2); Read(A[1]^,mm); k:=0;
    cr:= GetPos div r; cw:=c; q:=3; ni:=mm; mk:=mm; i:=1; j:=1;
    Repeat If q=w Then ni:=nw;           Sli(A[q]^,A[1]^,A[2]^);
     If i > ni Then Begin i:=1; q:=q+1 End
     Else Begin j:=1; Seek(cr); If n-cr < mm Then mk:=n-cr;
           If mk > 0 Then Read(A[1]^,mk); cr:=cr+mk End
    Until (q > w) Or (mk=0); Seek(cw); Write(A[2]^,k);
    If mk=0 Then
           Begin For j:=i to ni do A[q]^[j-i+1]:= A[q]^[j];
            Write(A[q]^,ni-i+1);
            For u:=q+1 to w do If u=w Then Write(A[w]^,nw)
                               Else Write(A[u]^,mm)
           End
   End;
  FreeMem(A[w], nw*r); For u:= w-1 downto 1 do FreeMem(A[u], mr)
END;
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}

{$ELSE}{Not PoglSort}

{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
Procedure TSortedCollection.KputSort;
 type TSR = pointer; Label 1;
 Const r=SizeOf(TSR); mm=65520 div r; nseg=9; mr=mm*r;
 Type mas = Array[1..mm] of TSR;
 Var A:Array[1..nseg] of ^mas; kp,nk,pe,pk,q,u,w,z:byte;
     c,cc,cw,dc,n,np,mp,v,v1,ww:longint;  i,j,k,mk,ni,s:word;
     kb,og,l: Array[1..nseg] of word; p,nn:integer; x:TSR;
     cr,kk: Array[1..nseg] of longint;

{$IFDEF COLLECTION_IN_STREAM}
function GetSize: longint; begin GetSize:=CStream^.GetSize/r end;
procedure Seek(Pos: longint);begin CStream^.Seek(Pos*r) end;
procedure Read(var A; count: word);begin CStream^.Read(A, count*r) end;
procedure Write(var A; count: word);begin CStream^.Write(A, count*r) end;
function GetPos: longint; begin GetPos:=CStream^.GetPos/r end;
{$ELSE}
var Posit: longint;
function  GetSize: longint; begin GetSize:=Count; end;
procedure Seek(Pos: longint);begin Posit:=Pos end;
procedure Read(var A; count: word);
 begin
  {$IFDEF BIGCOLLECTION}
  Items^.AtGet(Posit, count, @A);
  {$ELSE}
  move(Items^[Posit], A, count*r);
  {$ENDIF}
  inc(Posit, count);
 end;
procedure Write(var A; count: word);
 begin
  {$IFDEF BIGCOLLECTION}
  Items^.AtReplace(Posit, count, @A);
  {$ELSE}
  move(A, Items^[Posit], count*r);
  {$ENDIF}
  inc(Posit, count);
 end;
function GetPos: longint; begin GetPos:=Posit end;
{$ENDIF}

 Procedure Merg; Label 1;
  Begin x:= A[u]^[l[u]]; i:=j+j;
   Repeat If i < pk Then
    If Compare(A[og[i]]^[l[og[i]]], A[og[i+1]]^[l[og[i+1]]])>0 Then i:=i+1;
    If Compare(x,A[og[i]]^[l[og[i]]])>0 Then og[j]:= og[i]
    Else Goto 1;   j:=i; i:=j+j
   Until i > pk;     1: og[j]:= u
  End;
 Procedure AInit; Var k:word;
  Begin For k:= 1 to pk do og[k]:=k;
   For k:= pk div 2 downto 1 do
    Begin u:= og[k]; j:=k; Merg End
  End;
 Procedure VnutSort(c,cc:longint);
  Var i,j,u: word; og: Array[1..nseg] of word;
  Procedure Merg; Label 1;
   Begin x:= A[u]^[1]; i:=j+j;
    Repeat If i < q Then
     If Compare(A[og[i+1]]^[1], A[og[i]]^[1])>0 Then i:=i+1;
     If Compare(A[og[i]]^[1],x)>0 Then og[j]:= og[i] Else Goto 1;
     j:=i; i:=j+j
    Until i > q;  1: og[j]:=u
   End;
  Procedure AInit; Var k:word;
   Begin For k:= 1 to q do og[k]:=k;
    For k:= q div 2 downto 1 do  Begin u:= og[k]; j:=k; Merg End
   End;
  Procedure Sipka(var A:mas; k,n1:word); Label 1;
   Begin If n1 < 2 Then Exit;
    Repeat x:= A[k]; j:=k; i:=j+j;
     While i <= n1 do
      Begin If i < n1 Then If Compare(A[i+1], A[i])>0 Then i:=i+1;
      If Compare(A[i],x)>0 Then A[j]:= A[i] Else Goto 1; j:=i; i:=j+j
      End;  1: A[j]:=x; k:=k-1
    Until k = 0
   End;
  Begin cw:=c + (p+1) div 2*(kp*v-v); q:=0; Seek(c);
   Repeat mk:=mm; If cc-c < mm Then mk:= cc-c; q:=q+1; z:=q;
    Read(A[q]^,mk);  c:= c+mk;
    Sipka(A[q]^,mk div 2, mk)
   Until c = cc; Seek(cw); AInit; np:= mk;
   Repeat  u:=og[1];
    x:=A[q]^[np]; A[q]^[np]:=A[u]^[1]; A[u]^[1]:=x;
    np:=np-1; If u=q Then ni:=np Else ni:=mm; Sipka(A[u]^,1,ni);
    j:=1; If np = 0 Then Begin np:=mm; q:=q-1; AInit End
          Else If q > 1 Then Merg
   Until q = 0;
   For j:=1 to z-1 do Write(A[j]^,mm); Write(A[z]^,mk);
  End;
 Procedure Output;
  Begin Seek(cw); Write(A[kp+1]^,s); cw:=cw+s; s:=0 End;
 Procedure Input;
  Begin  If kk[u] > mm Then kb[u]:= mm Else kb[u]:= kk[u];
   Seek(cr[u]); Read(A[u]^,kb[u]); kk[u]:= kk[u]-kb[u];
   cr[u]:=cr[u]+kb[u]; l[u]:=1
  End;
 BEGIN   Seek(0); n:= GetSize; w:=0; np:=0;
   While (MaxAvail >= mr) And (w < nseg) do
    Begin  w:=w+1; np:=np+mm; GetMem(A[w], mr) End;
   kp:=w-1; p:=1; v1:=(n+1) div 2; nn:= 0; v:=np;
   While v1 > np do
    Begin p:=-p; nn:= nn+1; ww:=v1; v1:=(v1+kp-1) div kp End; nk:=1;
   If nn > 0 Then Repeat nk:= nk+1; v:= (ww+nk-1) div nk Until v < np;
   c:= n - n mod v; cc:=n;
   Repeat If cc > c Then VnutSort(c,cc); cc:=c; c:=c-v Until c < 0;
   ww:= v*(kp-1); dc:=ww;
   REPEAT   If nn = 1 Then kp:=nk;   If nn = 0 Then kp:= 2;
    v1:= kp*v; c:=ww; np:= n+c;
    If p < 0 Then Begin c:= n - n mod v1; np:=n;
                        dc:=(kp-1)*v1; If nn=1 Then dc:= v1 End;
    Repeat pk:=0; cw:= c - p*dc; ww:= cw;
    For u:=1 to kp do
    If c + v*(u-1) < np Then
         Begin pk:=pk+1; cr[u]:= c + v*(u-1);
         If cr[u]+v < np Then kk[u]:=v Else kk[u]:=np-cr[u]; Input
         End;                s:=0; AInit;
    If c < np Then
     Repeat j:=1; u:=og[1]; s:=s+1; A[kp+1]^[s]:= A[u]^[l[u]];
     If s = mm Then Output;   l[u]:= l[u]+1;
     If l[u] > kb[u] Then  If kk[u] > 0 Then Input
                Else Begin og[1]:= og[pk]; pk:= pk-1; u:=og[1] End;
     If pk > 1 Then Merg
     Until pk=0; If s > 0 Then Output;
     c:= c+p*v1
    Until (c >= np) Or (c < 0); p:=-p; v:= v1; nn:=nn-1
   UNTIL nn < 0; {Truncate(t);} For u:= 1 to kp+1 do FreeMem(A[u],mr)
  END;
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
{$ENDIF}{NOT POGLSORT}

{$ENDIF}{NOT QSort}

        {-DataCompBoy-}
 Function TDirCol.Compare;
  var a, b, c: byte;
  begin
   Compare:=0;
   a:=0; for c:=1 to Length(PString(Key1)^) do if PString(Key1)^[c] in ['/','\'] then inc(a);
   b:=0; for c:=1 to Length(PString(Key1)^) do if PString(Key1)^[c] in ['/','\'] then inc(b);
   If a>b then Compare:=-1 else Compare:=+1
  end;
        {-DataCompBoy-}
end.
