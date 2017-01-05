unit _Streams;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects;

type
  PStream = ^TStream;
  TStream = object(TObject)
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt;
    Position: longInt;
    Constructor Init;
    procedure CopyFrom(var s: TStream; Count: longInt);
    procedure Error(Code, Info: integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: longInt; virtual;
    function GetSize: longInt; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: longInt); virtual;
    function ReadStr: PString;
    function ReadLongStr: PLongString;
    procedure ReadStrV(var s: String);
    procedure ReadLongStrV(var s: LongString);
    procedure Reset;
    procedure Seek(Pos: longInt); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: longInt); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function Eof: boolean;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = object(TStream)
    Handle: integer;
    FName: AsciiZ;
    FMode: word;
    Constructor Init(FileName: String; Mode: word);
    procedure Open(FileName: String; Mode: word);
    {destructor Done; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    procedure ReadBlock(var Buf; Count: longInt; var BytesRead: word);
      virtual;
    {procedure Seek(Pos: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    {procedure DoOpen(OpenMode: Word); virtual;}
    {procedure Close; virtual;}
    end;

  PBufStream = ^TBufStream;
  TBufStream = object(TDOSStream)
    Buffer: PByteArray;
    BufSize: longInt;
    BufPtr: longInt;
    BufEnd: longInt;
    LastMode: byte;
    Constructor Init(FileName: String; Mode: word; Size: longInt);
    {destructor Done; virtual;}
    {procedure Flush; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    {procedure Seek(Pos: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    {procedure DoOpen(OpenMode: Word); virtual;}
    {procedure Close; virtual;}
    end;

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    BlkCount: longInt;
    BlkSize: word;
    MemSize: longInt;
    BlkList: PPointerArray;
    Constructor Init(ALimit: longInt; ABlockSize: word);
    {destructor Done; virtual;}
    {procedure Read(var Buf; Count: LongInt); virtual;}
    {procedure Truncate; virtual;}
    {procedure Write(const Buf; Count: LongInt); virtual;}
    end;

implementation

uses
  _DNFuncs;

Constructor TStream.Init;
  begin
    _TObject^.Init(nil, @Self);
  end;

procedure TStream.CopyFrom(var s: TStream; Count: longInt);
  begin
    _TStream^.CopyFrom(_Model1.TStream(s), Count, @Self);
  end;

procedure TStream.Error(Code, Info: integer);
  assembler; {&Frame-}
asm
end;

procedure TStream.Flush;
  assembler; {&Frame-}
asm
end;

function TStream.Get: PObject;
  begin
    Result := PObject(_TStream^.Get(@Self));
  end;

function TStream.GetPos: longInt;
  assembler; {&Frame-}
asm
end;

function TStream.GetSize: longInt;
  assembler; {&Frame-}
asm
end;

procedure TStream.Put(P: PObject);
  begin
    _TStream^.Put(_Model1.PObject(P), @Self);
  end;

procedure TStream.Read(var Buf; Count: longInt);
  assembler; {&Frame-}
asm
end;

function TStream.ReadStr: PString;
  begin
    Result := _TStream^.ReadStr(@Self);
  end;

function TStream.ReadLongStr: PLongString;
  begin
    Result := _TStream^.ReadLongStr(@Self);
  end;

procedure TStream.ReadStrV(var s: String);
  begin
    _TStream^.ReadStrV(s, @Self);
  end;

procedure TStream.ReadLongStrV(var s: LongString);
  begin
    _TStream^.ReadLongStrV(s, @Self);
  end;

procedure TStream.Reset;
  begin
    _TStream^.Reset(@Self);
  end;

procedure TStream.Seek(Pos: longInt);
  assembler; {&Frame-}
asm
end;

function TStream.StrRead: PChar;
  begin
    Result := _TStream^.StrRead(@Self);
  end;

procedure TStream.StrWrite(P: PChar);
  begin
    _TStream^.StrWrite(P, @Self);
  end;

procedure TStream.Truncate;
  assembler; {&Frame-}
asm
end;

procedure TStream.Write(const Buf; Count: longInt);
  assembler; {&Frame-}
asm
end;

procedure TStream.WriteStr(P: PString);
  begin
    _TStream^.WriteStr(P, @Self);
  end;

procedure TStream.WriteLongStr(P: PLongString);
  begin
    _TStream^.WriteLongStr(P, @Self);
  end;

function TStream.Eof: boolean;
  begin
    Result := _TStream^.Eof(@Self);
  end;

procedure TStream.DoOpen(OpenMode: word);
  assembler; {&Frame-}
asm
end;

procedure TStream.Close;
  assembler; {&Frame-}
asm
end;

Constructor TDOSStream.Init(FileName: String; Mode: word);
  begin
    _TDosStream^.Init(FileName, Mode, nil, @Self);
  end;

procedure TDOSStream.Open(FileName: String; Mode: word);
  begin
    _TDosStream^.Open(FileName, Mode, @Self);
  end;

procedure TDOSStream.ReadBlock(var Buf; Count: longInt; var
    BytesRead: word);
  assembler; {&Frame-}
asm
end;

Constructor TBufStream.Init(FileName: String; Mode: word; Size:
    longInt);
  begin
    _TBufStream^.Init(FileName, Mode, Size, nil, @Self);
  end;

Constructor TMemoryStream.Init(ALimit: longInt; ABlockSize: word);
  begin
    _TMemoryStream^.Init(ALimit, ABlockSize, nil, @Self);
  end;

end.
