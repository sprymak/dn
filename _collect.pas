unit _Collect;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects, _Streams;

type
  PCollection = ^TCollection;
  TCollection = object(TObject)
    Items: Pointer {PItemList};
    Count: longInt;
    Limit: longInt;
    Delta: longInt;
    Status, ErrorInfo: integer;
    Constructor Init(ALimit, ADelta: longInt);
    Constructor Load(var s: TStream);
    {destructor Done; virtual;}
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

  PSortedCollection = ^TSortedCollection;
  TSortedCollection = object(TCollection)
    Duplicates: boolean;
    Constructor Init(ALimit, ADelta: longInt);
    Constructor Load(var s: TStream);
    function Compare(Key1, Key2: Pointer): integer; virtual;
    {function IndexOf(Item: Pointer): LongInt; virtual;}
    {procedure Insert(Item: Pointer); virtual;}
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: longInt): boolean;
      virtual;
    procedure Store(var s: TStream);
    procedure Sort;
    end;

  PLineCollection = ^TLineCollection;
  TLineCollection = object(TCollection)
    LongStrings: boolean;
    Constructor Init(ALimit, ADelta: longInt; ALongStrings: boolean);
    {procedure FreeItem(P: Pointer); virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    end;

  PStringCollection = ^TStringCollection;
  TStringCollection = object(TSortedCollection)
    LongStrings: boolean;
    Constructor Init(ALimit, ADelta: longInt; ALongStrings: boolean);
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    {procedure FreeItem(Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    end;

  PStrCollection = ^TStrCollection;
  TStrCollection = object(TSortedCollection)
    Constructor Init(ALimit, ADelta: longInt);
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    {procedure FreeItem(Item: Pointer); virtual;}
    {function GetItem(var S: TStream): Pointer; virtual;}
    {procedure PutItem(var S: TStream; Item: Pointer); virtual;}
    end;

  PFilesCollection = ^TFilesCollection;
  TFilesCollection = object(TSortedCollection)
    SortMode: byte;
    Selected: longInt;
    Owner: Pointer;
    {$IFDEF WIN32}
    LFNActive: boolean;
    {$ENDIF}
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream); virtual;
    {procedure FreeItem(Item: Pointer); virtual;}
    {function Compare(Key1, Key2: Pointer): Integer; virtual;}
    end;

implementation

uses
  _DNFuncs;

Constructor TCollection.Init(ALimit, ADelta: longInt);
  begin
    _TCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

Constructor TCollection.Load(var s: TStream);
  begin
    _TCollection^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TCollection.At(Index: longInt): Pointer;
  begin
    Result := _TCollection^.At(Index, @Self);
  end;

procedure TCollection.AtDelete(Index: longInt);
  begin
    _TCollection^.AtDelete(Index, @Self);
  end;

procedure TCollection.AtFree(Index: longInt);
  begin
    _TCollection^.AtFree(Index, @Self);
  end;

procedure TCollection.AtInsert(Index: longInt; Item: Pointer);
  begin
    _TCollection^.AtInsert(Index, Item, @Self);
  end;

procedure TCollection.AtPut(Index: longInt; Item: Pointer);
  begin
    _TCollection^.AtPut(Index, Item, @Self);
  end;

procedure TCollection.AtReplace(Index: longInt; Item: Pointer);
  begin
    _TCollection^.AtReplace(Index, Item, @Self);
  end;

procedure TCollection.Delete(Item: Pointer);
  begin
    _TCollection^.Delete(Item, @Self);
  end;

procedure TCollection.DeleteAll;
  begin
    _TCollection^.DeleteAll(@Self);
  end;

procedure TCollection.Error(Code, Info: integer);
  assembler; {&Frame-}
asm
end;

function TCollection.FirstThat(Test: Pointer): Pointer;
  begin
    Result := _TCollection^.FirstThat(Test, @Self);
  end;

procedure TCollection.ForEach(Action: Pointer);
  begin
    _TCollection^.ForEach(Action, @Self);
  end;

procedure TCollection.Free(Item: Pointer);
  begin
    _TCollection^.Free(Item, @Self);
  end;

procedure TCollection.FreeAll;
  begin
    _TCollection^.FreeAll(@Self);
  end;

procedure TCollection.FreeItem(Item: Pointer);
  assembler; {&Frame-}
asm
end;

function TCollection.GetItem(var s: TStream): Pointer;
  assembler; {&Frame-}
asm
end;

function TCollection.IndexOf(Item: Pointer): longInt;
  assembler; {&Frame-}
asm
end;

procedure TCollection.Insert(Item: Pointer);
  assembler; {&Frame-}
asm
end;

function TCollection.LastThat(Test: Pointer): Pointer;
  begin
    Result := _TCollection^.LastThat(Test, @Self);
  end;

procedure TCollection.Pack;
  begin
    _TCollection^.Pack(@Self);
  end;

procedure TCollection.PutItem(var s: TStream; Item: Pointer);
  assembler; {&Frame-}
asm
end;

procedure TCollection.SetLimit(ALimit: longInt);
  assembler; {&Frame-}
asm
end;

procedure TCollection.Store(var s: TStream);
  begin
    _TCollection^.Store(_Model1.TStream(s), @Self);
  end;

Constructor TSortedCollection.Init(ALimit, ADelta: longInt);
  begin
    _TSortedCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

Constructor TSortedCollection.Load(var s: TStream);
  begin
    _TSortedCollection^.Load(_Model1.TStream(s), nil, @Self);
  end;

function TSortedCollection.Compare(Key1, Key2: Pointer): integer;
  assembler; {&Frame-}
asm
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
  assembler; {&Frame-}
asm
end;

function TSortedCollection.Search(Key: Pointer; var Index: longInt):
    boolean;
  assembler; {&Frame-}
asm
end;

procedure TSortedCollection.Store(var s: TStream);
  begin
    _TSortedCollection^.Store(_Model1.TStream(s), @Self);
  end;

procedure TSortedCollection.Sort;
  begin
    _TSortedCollection^.Sort(@Self);
  end;

Constructor TLineCollection.Init(ALimit, ADelta: longInt;
    ALongStrings: boolean);
  begin
    _TLineCollection^.Init(ALimit, ADelta, ALongStrings, nil, @Self);
  end;

Constructor TStringCollection.Init(ALimit, ADelta: longInt;
    ALongStrings: boolean);
  begin
    _TStringCollection^.Init(ALimit, ADelta, ALongStrings, nil,
      @Self);
  end;

Constructor TStrCollection.Init(ALimit, ADelta: longInt);
  begin
    _TStrCollection^.Init(ALimit, ADelta, nil, @Self);
  end;

Constructor TFilesCollection.Load(var s: TStream);
  begin
    _TFilesCollection^.Load(_Model1.TStream(s), nil, @Self);
  end;

procedure TFilesCollection.Store(var s: TStream);
  assembler; {&Frame-}
asm
end;

end.
