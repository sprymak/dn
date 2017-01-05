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
{$DEFINE NoCallspcb}

{-----------------------------------------------------}
{ This module is based on Turbo Vision Objects Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - переделал строковые коллекции для обеспечения возможности
   хранить в них не только String-и, но и LongString-и
}

unit Collect;

interface

uses
  Objects;

const
  MaxCollectionSize = MaxBytes div SizeOf(Pointer);

  { TCollection error codes }

  coIndexError = -1; { Index out of range }
  coOverflow = -2; { Overflow }

type
  PItemList = ^TItemList;
  TItemList = array[0..MaxCollectionSize-1] of Pointer;

  { TCollection object }

  PCollection = ^TCollection;
  TCollection = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Items: PItemList;
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer; {!!! в OldColection}
    Constructor Init(ALimit, ADelta: longInt);
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function At(Index: longInt): Pointer;
    procedure AtDelete(Index: longInt);
    procedure AtFree(Index: longInt);
    procedure AtInsert(Index: longInt; Item: Pointer);
    procedure AtPut(Index: longInt; Item: Pointer);
    procedure AtReplace(Index: longInt; Item: Pointer);
    procedure Delete(Item: Pointer);
    procedure DeleteAll;
    procedure Error(Code, Info: integer); virtual;
    function FirstThat(Test: Pointer): Pointer;
    procedure ForEach(Action: Pointer);
    procedure Free(Item: Pointer);
    procedure FreeAll;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    function IndexOf(Item: Pointer): longInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function LastThat(Test: Pointer): Pointer;
    procedure Pack;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    procedure SetLimit(ALimit: longInt); virtual;
    procedure Store(var s: TStream);
    end;

  { TSortedCollection object }

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Duplicates: boolean;
    Constructor Init(ALimit, ADelta: longInt);
    Constructor Load(var s: TStream);
    function Compare(Key1, Key2: Pointer): integer; virtual;
    function IndexOf(Item: Pointer): longInt; virtual;
    procedure Insert(Item: Pointer); virtual;
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: longInt): boolean;
      virtual;
    procedure Store(var s: TStream);
    procedure Sort;
    procedure QSort;
    procedure QuickSort(l, R: longInt);
    end;

  { TLineCollection }

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    LongStrings: boolean; {Cat}
    Constructor Init(ALimit, ADelta: longInt; ALongStrings: boolean);
      {Cat}
    procedure FreeItem(P: Pointer); virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    end;

  { TStringCollection object }

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    LongStrings: boolean; {Cat}
    Constructor Init(ALimit, ADelta: longInt; ALongStrings: boolean);
      {Cat}
    function Compare(Key1, Key2: Pointer): integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    end;

  { TStrCollection object }

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    function Compare(Key1, Key2: Pointer): integer; virtual;
    procedure FreeItem(Item: Pointer); virtual;
    function GetItem(var s: TStream): Pointer; virtual;
    procedure PutItem(var s: TStream; Item: Pointer); virtual;
    end;

  { TUnSortedStrCollection - UNSORTED STRING COLLECTION OBJECT }

  { This is a completely >> NEW << object which holds a collection of  }
  { strings but does not alphabetically sort them. It is a very useful }
  { object for insert ordered list boxes!                              }

  PUnSortedStrCollection = ^TUnSortedStrCollection;
  TUnSortedStrCollection = object(TStringCollection)
    procedure Insert(Item: Pointer); virtual;
    end;
  (*
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
*)
  { TStringList object }

  TStrIndexRec = record
    Key, Count, Offset: AWord;
    end;

  PStrIndex = ^TStrIndex;
  TStrIndex = array[0..9999] of TStrIndexRec;

  PStringList = ^TStringList;
  TStringList = object(TObject)
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function Get(Key: AWord): String;
    private
    Stream: PStream;
    BasePos: longInt;
    IndexSize: AWord;
    Index: PStrIndex;
    procedure ReadStr(var s: String; Offset, Skip: AWord);
    end;

  {-DataCompBoy-}
type
  PDirCol = ^TDirCol;
  TDirCol = object(TStringCollection)
    function Compare(Key1, Key2: Pointer): integer; virtual;
    end;
  {-DataCompBoy-}

implementation
uses
  Memory,
  Strings,
  advance1
  ;

Constructor TCollection.Init(ALimit, ADelta: longInt);
  begin
    inherited Init;
    Delta := ADelta;
    SetLimit(ALimit);
  end;

Constructor TCollection.Load(var s: TStream);
  var
    C, i: longInt;
  begin
    TObject.Init;
    s.Read(Count, SizeOf(Count));
    s.Read(Limit, SizeOf(Limit));
    s.Read(Delta, SizeOf(Delta));
    if (Count > Limit) or (Delta < 0) then
      Fail;
    C := Count;
    i := Limit;
    Count := 0;
    Limit := 0;
    SetLimit(i);
    for i := 0 to C-1 do
      begin
        AtInsert(i, GetItem(s));
        if (s.Status <> stOK) then
          begin
            SetLimit(0);
            Fail;
          end;
      end;
  end { TCollection.Load };

function TCollection.At(Index: longInt): Pointer;
  begin
    if (Index < 0) or (Index >= Count) or (Items = nil)
    then
      begin
        Error(coIndexError, Index);
        At := nil;
      end
    else
      At := Items^[Index];
  end;

procedure TCollection.AtDelete(Index: longInt);
  begin
    if (Index >= 0) and (Index < Count) and (Items <> nil) then
      begin
        Dec(Count);
        if Count > Index then
          Move(Items^[Index+1], Items^[Index], (Count-Index)*SizeOf(
            Pointer));
      end
    else
      Error(coIndexError, Index);
  end;

procedure TCollection.AtInsert(Index: longInt; Item: Pointer);
  var
    i: longInt;
  begin
    if (Index >= 0) and (Index <= Count) then
      begin
        if Count = Limit then
          SetLimit(Limit+Delta);
        if Limit > Count then
          begin
            if Index < Count then
              for i := Count-1 downto Index do
                Items^[i+1] := Items^[i];
            Items^[Index] := Item;
            Inc(Count);
          end
        else
          Error(coOverflow, Index);
      end
    else
      Error(coIndexError, Index);
  end { TCollection.AtInsert };

procedure TCollection.AtPut(Index: longInt; Item: Pointer);
  begin
    if (Index >= 0) and (Index < Count) and (Items <> nil)
    then
      Items^[Index] := Item
    else
      Error(coIndexError, Index);
  end;

procedure TCollection.SetLimit(ALimit: longInt);
  var
    AItems: PItemList;
  begin
    if ALimit < Count then
      ALimit := Count;
    if ALimit > MaxCollectionSize then
      ALimit := MaxCollectionSize;
    if ALimit <> Limit then
      begin
        if ALimit = 0 then
          AItems := nil
        else
          begin
            GetMem(AItems, ALimit*SizeOf(Pointer));
            if (AItems <> nil) then
              FillChar(AItems^, ALimit*SizeOf(Pointer), #0);
          end;
        if (AItems <> nil) or (ALimit = 0) then
          begin
            if (AItems <> nil) and (Items <> nil) and (Count <> 0)
            then
              Move(Items^, AItems^, Count*SizeOf(Pointer));
            if (Limit <> 0) and (Items <> nil) then
              FreeMem(Items, Limit*SizeOf(Pointer));
          end;
        Items := AItems;
        Limit := ALimit;
      end;
  end { TCollection.SetLimit };

destructor TCollection.Done;
  begin
    FreeAll;
    SetLimit(0);
  end;

procedure TCollection.AtFree(Index: longInt);
  var
    Item: Pointer;
  begin
    Item := At(Index);
    AtDelete(Index);
    FreeItem(Item);
  end;

procedure TCollection.AtReplace;
  var
    P: Pointer;
  begin
    if (Limit < Index) and (Delta > 0) then
      SetLimit(Index+1);
    while Count < (Index+1) do
      Insert(nil);
    P := At(Index);
    AtPut(Index, Item);
    if P <> nil then
      FreeItem(P);
  end;

procedure TCollection.Delete(Item: Pointer);
  begin
    AtDelete(IndexOf(Item));
  end;

procedure TCollection.DeleteAll;
  begin
    if @Self <> nil then
      Count := 0;
  end;

procedure TCollection.Error(Code, Info: integer);
  begin
    Status := Code;
    ErrorInfo := Info;
    {RunError(212 - Code);}
  end;

{AK155: replace with TCollection.FirstThat from VP2.1 Objects.pas }
function TCollection.FirstThat(Test: Pointer): Pointer; assembler;
    {$USES ebx} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     ebx,Test
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                add     edx,4
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx
end
  ; {/AK155}

{AK155: pick ForEach from VP 2.1.231 RTL source}
procedure TCollection.ForEach(Action: Pointer); assembler;
    {$USES ebx} {$FRAME-}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@2
                mov     ebx,Action
                mov     edx,[edx].TCollection.Items
              @@1:
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                add     edx,4
                loop    @@1
              @@2:
end
  ; {/AK155}

procedure TCollection.Free(Item: Pointer);
  begin
    Delete(Item);
    FreeItem(Item);
  end;

procedure TCollection.FreeAll;
  var
    i: longInt;
  begin
    for i := Count-1 downto 0 do
      FreeItem(At(i));
    Count := 0;
  end;

procedure TCollection.FreeItem(Item: Pointer);
  begin
    FreeObject(Item);
  end;

function TCollection.GetItem(var s: TStream): Pointer;
  begin
    GetItem := s.Get;
  end;

function TCollection.IndexOf(Item: Pointer): longInt;
  var
    i: longInt;
  begin
    IndexOf := -1;
    for i := 0 to Count-1 do
      if At(i) = Item then
        begin
          IndexOf := i;
          break;
        end;
  end;

procedure TCollection.Insert(Item: Pointer);
  begin
    AtInsert(Count, Item);
  end;

function TCollection.LastThat(Test: Pointer): Pointer; assembler;
    {$USES ebx} {$FRAME-} {AK155}
asm
                mov     edx,Self
                mov     ecx,[edx].TCollection.Count
                jecxz   @@3
                mov     edx,[edx].TCollection.Items
                lea     edx,[edx+ecx*4]
                mov     ebx,Test
              @@1:
                sub     edx,4
                push    edx
                push    ecx
                push    DWord Ptr [edx]         { [1]:Pointer = Item }
                Call    ebx
                pop     ecx
                pop     edx
                test    al,al
                jnz     @@2
                loop    @@1
                jmp     @@3
              @@2:
                mov     ecx,[edx]
              @@3:
                mov     eax,ecx

end
  ; {/AK155}

procedure TCollection.Pack;
  var
    i: longInt;
  begin
    for i := Count-1 downto 0 do
      if At(i) = nil then
        AtDelete(i);
  end;

procedure TCollection.PutItem(var s: TStream; Item: Pointer);
  begin
    s.Put(Item);
  end;

procedure TCollection.Store(var s: TStream);
  procedure DoPutItem(P: Pointer);
    begin
      PutItem(s, P);
    end;
  begin
    s.Write(Count, SizeOf(Count));
    s.Write(Limit, SizeOf(Limit));
    s.Write(Delta, SizeOf(Delta));
    ForEach(@DoPutItem);
  end;

{ TSortedCollection }

Constructor TSortedCollection.Init(ALimit, ADelta: longInt);
  begin
    TCollection.Init(ALimit, ADelta);
    Duplicates := False;
  end;

Constructor TSortedCollection.Load(var s: TStream);
  begin
    inherited Load(s);
    s.Read(Duplicates, SizeOf(Duplicates));
  end;

function TSortedCollection.Compare(Key1, Key2: Pointer): integer;
  begin
    Abstract;
  end;

function TSortedCollection.IndexOf(Item: Pointer): longInt;
  var
    i: longInt;
  begin
    IndexOf := -1;
    if Search(KeyOf(Item), i) then
      begin
        if Duplicates then
          while (i < Count) and (Item <> At(i)) do
            Inc(i);
        if i < Count then
          IndexOf := i;
      end;
  end;

procedure TSortedCollection.Insert(Item: Pointer);
  var
    i: longInt;
  begin
    if not Search(KeyOf(Item), i) or Duplicates then
      AtInsert(i, Item);
  end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
  begin
    KeyOf := Item;
  end;

function TSortedCollection.Search
  (Key: Pointer; var Index: longInt): boolean;
  var
    l, H, i, C: longInt;
  begin
    Search := False;
    l := 0;
    H := Count-1;
    while l <= H do
      begin
        i := (l+H) shr 1;
        C := Compare(KeyOf(At(i)), Key);
        if C < 0
        then
          l := i+1
        else
          begin
            if C = 0 then
              begin
                Search := True;
                if not Duplicates then
                  l := i;
                break;
              end;
            H := i-1;
          end;
      end;
    Index := l;
  end { TSortedCollection.Search };

procedure TSortedCollection.Store(var s: TStream);
  begin
    TCollection.Store(s);
    s.Write(Duplicates, SizeOf(Duplicates));
  end;

{ TStringCollection }

{Cat: добавил возможность хранить в коллекции длинные строки}
Constructor TStringCollection.Init(ALimit, ADelta: longInt;
    ALongStrings: boolean);
  begin
    inherited Init(ALimit, ADelta);
    LongStrings := ALongStrings;
  end;

function TStringCollection.Compare(Key1, Key2: Pointer): integer;
  var
    i, j: longInt;
    P1, P2: PString;
    PL1, PL2: PLongString;
  begin
    if LongStrings then
      begin
        PL1 := PLongString(Key1);
        PL2 := PLongString(Key2);
        if Length(PL1^) < Length(PL2^) then
          j := Length(PL1^)
        else
          j := Length(PL2^);
        i := 1;
        while (i < j) and (PL1^[i] = PL2^[i]) do
          Inc(i);
        if (i = j) then
          begin
            if (PL1^[i] < PL2^[i]) then
              Compare := -1
            else if (PL1^[i] > PL2^[i]) then
              Compare := 1
            else if Length(PL1^) > Length(PL2^) then
              Compare := 1
            else if Length(PL1^) < Length(PL2^) then
              Compare := -1
            else
              Compare := 0;
          end
        else if (PL1^[i] < PL2^[i]) then
          Compare := -1
        else
          Compare := 1;
      end
    else
      begin
        P1 := PString(Key1);
        P2 := PString(Key2);
        if Length(P1^) < Length(P2^) then
          j := Length(P1^)
        else
          j := Length(P2^);
        i := 1;
        while (i < j) and (P1^[i] = P2^[i]) do
          Inc(i);
        if (i = j) then
          begin
            if (P1^[i] < P2^[i]) then
              Compare := -1
            else if (P1^[i] > P2^[i]) then
              Compare := 1
            else if Length(P1^) > Length(P2^) then
              Compare := 1
            else if Length(P1^) < Length(P2^) then
              Compare := -1
            else
              Compare := 0;
          end
        else if (P1^[i] < P2^[i]) then
          Compare := -1
        else
          Compare := 1;
      end;
  end { TStringCollection.Compare };

procedure TStringCollection.FreeItem(Item: Pointer);
  begin
    if LongStrings then
      DisposeLongStr(PLongString(Item))
    else
      DisposeStr(PString(Item));
  end;

function TStringCollection.GetItem(var s: TStream): Pointer;
  begin
    if LongStrings then
      GetItem := s.ReadLongStr
    else
      GetItem := s.ReadStr;
  end;

procedure TStringCollection.PutItem(var s: TStream; Item: Pointer);
  begin
    if LongStrings then
      s.WriteLongStr(Item)
    else
      s.WriteStr(Item);
  end;
{/Cat}

{ TLineCollection }
{Cat: добавил возможность хранить в коллекции длинные строки}
Constructor TLineCollection.Init(ALimit, ADelta: longInt;
    ALongStrings: boolean);
  begin
    inherited Init(ALimit, ADelta);
    LongStrings := ALongStrings;
  end;

procedure TLineCollection.FreeItem(P: Pointer);
  begin
    if LongStrings then
      DisposeLongStr(PLongString(P))
    else
      DisposeStr(PString(P));
  end;

procedure TLineCollection.PutItem;
  begin
    if LongStrings then
      s.WriteLongStr(Item)
    else
      s.WriteStr(Item);
  end;

function TLineCollection.GetItem;
  begin
    if LongStrings then
      GetItem := s.ReadLongStr
    else
      GetItem := s.ReadStr;
  end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): integer;
  begin
    Compare := StrComp(Key1, Key2);
  end;

procedure TStrCollection.FreeItem(Item: Pointer);
  begin
    StrDispose(PChar(Item));
  end;

function TStrCollection.GetItem(var s: TStream): Pointer;
  begin
    GetItem := s.StrRead;
  end;

procedure TStrCollection.PutItem(var s: TStream; Item: Pointer);
  begin
    s.StrWrite(Item);
  end;

procedure TUnSortedStrCollection.Insert(Item: Pointer);
  begin
    AtInsert(Count, Item);
  end;
(*
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
{Cat:warn AnsiString}
  S.Read(L, 1);
  GetMem(P, L + (SizeOf(TResourceItem) - SizeOf(String) + 1));
  P^.Posn:=Pos;
  P^.Size:=Size;
  SetLength(P^.Key, L);
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
*)
{ TStringList }

Constructor TStringList.Load(var s: TStream);
  var
    Size: AWord;
  begin
    TObject.Init;
    Stream := @S;
    s.Read(Size, SizeOf(Size));
    BasePos := s.GetPos;
    s.Seek(BasePos+Size);
    s.Read(IndexSize, SizeOf(IndexSize));
    GetMem(Index, IndexSize*SizeOf(TStrIndexRec));
    s.Read(Index^, IndexSize*SizeOf(TStrIndexRec));
  end;

destructor TStringList.Done;
  begin
    FreeMem(Index, IndexSize*SizeOf(TStrIndexRec));
  end;

function TStringList.Get(Key: AWord): String;
  var
    i: AWord;
    s: String;
  begin
    s := '';
    if (IndexSize > 0) then
      begin
        i := 0;
        while (i < IndexSize) and (s = '') do
          begin
            if ((Key-Index^[i].Key) < Index^[i].Count) then
              ReadStr(s, Index^[i].Offset, Key-Index^[i].Key);
            Inc(i);
          end;
      end;
    Get := s;
  end;

procedure TStringList.ReadStr(var s: String; Offset, Skip: AWord);
  {
var
  B: Byte; }
  begin
    Stream^.Seek(BasePos+Offset);
    Stream^.Status := 0;
    Inc(Skip);
    repeat
      {Cat}
      (*
    Stream^.Read(B, 1);
    SetLength(S, B);
    Stream^.Read(S[1],B);
*)
      Stream^.ReadStrV(s);
      {/Cat}
      Dec(Skip);
    until Skip = 0;
  end;

procedure TSortedCollection.Sort;
  begin
    if Count <= 0 then
      exit; {JO не удалять! иначе падаем по Ctrl-H и т.п.}
    QSort;
  end;

var
  P, t: Pointer;

procedure TSortedCollection.QuickSort(l, R: longInt);
  var
    i, j: longInt;
  begin
    repeat
      i := l;
      j := R;
      P := At((l+R) shr 1);
      repeat
        while Compare(At(i), P) < 0 do
          Inc(i);
        while Compare(At(j), P) > 0 do
          Dec(j);
        if i <= j then
          begin
            t := At(i);
            AtPut(i, At(j));
            AtPut(j, t);
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if l < j then
        QuickSort(l, j);
      l := i;
    until i >= R;
  end { TSortedCollection.QuickSort };

procedure TSortedCollection.QSort;
  begin
    if Count > 0 then
      QuickSort(0, Count-1);
  end;

{NOT QSort}

{-DataCompBoy-}
function TDirCol.Compare;
  var
    A, B, C: byte;
  begin
    Compare := 0;
    A := 0;
    for C := 1 to Length(PString(Key1)^) do
      if PString(Key1)^[C] in ['/', '\'] then
        Inc(A);
    B := 0;
    for C := 1 to Length(PString(Key2)^) do
      if PString(Key2)^[C] in ['/', '\'] then
        Inc(B);
    if A > B then
      Compare := -1
    else
      Compare := +1
  end;
{-DataCompBoy-}

{OldCollection !!!}

end.
