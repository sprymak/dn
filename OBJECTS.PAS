{/////////////////////////////////////////////  ////////////////////////////
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
{-----------------------------------------------------}
{ This module is based on Turbo Vision Objects Unit   }
{ Copyright (c) 1990 by Borland International         }
{-----------------------------------------------------}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - переделал чтение/запись строк в потоки для совместимости с
   типом AnsiString; добавил чтение/запись длинных строк (LongString) в потоки
}

unit Objects;

interface

uses
  {SysUtils,}VpSysLow,
  Lfn;

{ Common defines }

type
  CondInt = longInt;

  Str2 = String[2];
  PStr2 = ^Str2;
  Str3 = String[3];
  PStr3 = ^Str3;
  Str4 = String[4];
  PStr4 = ^Str4;
  Str5 = String[5];
  PStr5 = ^Str5;
  Str6 = String[6];
  PStr6 = ^Str6;
  Str8 = String[8];
  PStr8 = ^Str8;
  Str12 = String[12];
  PStr12 = ^Str12;
  Str40 = String[40];
  PStr40 = ^Str40;
  Str50 = String[50];
  PStr50 = ^Str50;

  {Cat: VP не позволяет переопределять тип String, вместо этого используем
      $H+ или $H- в STDEFINE.INC}
  (*
{$IFDEF USEANSISTRING}
  String = AnsiString;
{$ELSE}
  String = ShortString;
{$ENDIF}
*)
  PString = ^String;
  LongString = AnsiString;
  PLongString = ^LongString;
  {/Cat}

  TCharSet = Set of Char;
  PCharSet = ^TCharSet;

  WordRec = record
    Lo, Hi: byte;
    end;

  LongRec = record
    Lo, Hi: word;
    end;

  PtrRec = record
    Ofs, Seg: word;
    end;

const
  MaxBytes = 128*1024*1024;

  MaxWords = MaxBytes div SizeOf(word);
  MaxPtrs = MaxBytes div SizeOf(Pointer);

type

  SW_Word = longInt;
  Sw_Integer = longInt;
  AInt = SmallInt;

  AWord = SmallWord;

  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxBytes-1] of byte;

  PWordArray = ^TWordArray;
  TWordArray = array[0..MaxWords-1] of AWord;

  PPointerArray = ^TPointerArray;
  TPointerArray = array[0..MaxPtrs-1] of Pointer;

const

  { TStream access modes }

  stCreate = $FFFF { $DC0B }; { Create new file }
  stOpenRead = Open_Access_ReadOnly or open_share_DenyNone;
    { Read access only }
  stOpenWrite = Open_Access_WriteOnly or open_share_DenyNone;
    { Write access only }
  stOpen = Open_Access_ReadWrite or open_share_DenyNone;
    { Read and write access }
  stOpenPacked = Open_Access_ReadWrite+1;
    { Read access only, packed files too }

  { File share mode constants }
  fmClean = $FF00; { Mask to clean low byte of file mode constatns }

  fmOpenMode = $FFF0;
    { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly = Open_Access_ReadOnly; { Open read-only file }
  fmWriteOnly = Open_Access_WriteOnly; { Open file for write only }
  fmReadWrite = Open_Access_ReadWrite;
    { Open file as for read, as for write }
  fmPacked = Open_Access_ReadWrite+1; { Open a packed file, if can }

  fmDeny = $FF0F; { Mask to apply fmDenyXXX }
  fmDenyAll = Open_Share_DenyReadWrite; { Exclusive file use }
  fmDenyWrite = Open_Share_DenyWrite; { Deny write access }
  fmDenyRead = Open_Share_DenyRead; { Deny read access }
  fmDenyNone = open_share_DenyNone; { Deny no access }
  fmDenyChild = open_share_DenyNone; { Don't give right's to child }

  { TStream error codes }

  stOK = 0; { No error }
  stError = -1; { Access error }
  stInitError = -2; { Cannot initialize stream }
  stReadError = -3; { Read beyond end of stream }
  stWriteError = -4; { Cannot expand stream }
  stGetError = -5; { Get of unregistered object type }
  stPutError = -6; { Put of unregistered object type }
  stSeekError = -7; { Stream seek error }
  stOpenError = -8; { Stream open error }

type

  { TStreamRec }

  PStreamRec = ^TStreamRec;
  TStreamRec = record
    ObjType: word;
    VmtLink: Pointer;
    {Cat}
    Load: Pointer;
    Store: Pointer;
    Next: PStreamRec;
    end;

  { TObject base object }

  PEmptyObject = ^TEmptyObject;
  TEmptyObject = object
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Constructor Init;
    procedure Free;
    destructor Done; virtual;
    end;

  PObject = ^TObject;
  TObject = object(TEmptyObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ObjectIsInited: boolean;
    Constructor Init;
    destructor Done; virtual;
    end;

  { TStream }

  PStream = ^TStream;
  TStream = object(TObject)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Status: integer;
    ErrorInfo: integer;
    StreamSize: longInt; { Stream current size }
    Position: longInt; { Current position }
    procedure CopyFrom(var s: TStream; Count: longInt);
    procedure Error(Code, Info: integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: longInt; virtual;
    function GetSize: longInt; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: SW_Word); virtual;
    function ReadStr: PString;
    function ReadLongStr: PLongString; {Cat}
    procedure ReadStrV(var s: String); {Cat}
    procedure ReadLongStrV(var s: LongString); {Cat}
    procedure Reset;
    procedure Seek(Pos: longInt); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function Eof: boolean; {-$VOL}
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  FNameStr = String;
  AsciiZ = array[0..255] of Char;
  THandle = integer;

  { File name string }

type

  { TDosStream }

  PDosStream = ^TDosStream;
  TDOSStream = object(TStream)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Handle: integer;
    FName: AsciiZ;
    FMode: word;
    Constructor Init(FileName: FNameStr; Mode: word);
    procedure Open(FileName: FNameStr; Mode: word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure ReadBlock(var Buf; Count: SW_Word; var BytesRead: word);
      virtual;
    procedure Seek(Pos: longInt); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  { TBufStream }

  PBufStream = ^TBufStream;
  TBufStream = object(TDOSStream)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Buffer: PByteArray;
    BufSize: SW_Word;
    BufPtr: SW_Word; { Buffer start }
    BufEnd: SW_Word;
    LastMode: byte; { Last buffer mode }
    Constructor Init(FileName: FNameStr; Mode: word; Size: SW_Word);
    destructor Done; virtual;
    procedure Flush; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Seek(Pos: longInt); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  { TMemoryStream }

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    BlkCount: longInt; { Number of segments }
    BlkSize: word; { Memory block size }
    MemSize: longInt; { Memory alloc size }
    BlkList: PPointerArray; { Memory block list }
    Constructor Init(ALimit: longInt; ABlockSize: word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    private
    function ChangeListSize(ALimit: longInt): boolean;
    end;

  { TPoint object }

  PPoint = ^TPoint;
  TPoint = object
    X, Y: longInt;
    function Equals(P: TPoint): boolean;
    function EqualsXY(AX, AY: longInt): boolean;
    procedure Assign(AX, AY: longInt);
    function isLE(P: TPoint): boolean; {less then or equal}
    function isGE(P: TPoint): boolean; {great thean or equal}
    end;

  { Rectangle object }

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: longInt);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: longInt);
    procedure Grow(ADX, ADY: longInt);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): boolean;
    function Equals(R: TRect): boolean;
    function Empty: boolean;
    end;

procedure FreeObject(var o);

{ Longint routines }

function longmul(X, Y: longInt): longInt;
function LongDiv(X: longInt; Y: longInt): longInt;

{ Stream routines }

procedure RegisterType(var s: TStreamRec);
procedure ReRegisterType(var s: TStreamRec);

function GetAnyMemoryStream: PStream;
procedure Abstract;

procedure Beep(Freq, Dur: longInt); {JO}

const
  { Stream error procedure }
  StreamError: Pointer = nil;

implementation
uses
  Memory,
  Strings,
  {$IFDEF WIN32}Windows, {$ENDIF}
  {$IFDEF PACKFILE}PackF_RS, {$ENDIF}
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  Commands {Cat}, advance;

procedure Abstract;
  begin
    {RunError(211);}
  end;

type
  DummyObject = object(TEmptyObject)
    Data: record
      end;
    end;

const
  LinkSize = SizeOf(DummyObject);

Constructor TEmptyObject.Init;
  {
var
  LinkSize: LongInt;
  Dummy: DummyObject;
}
  begin
    //LinkSize := LongInt(@Dummy.Data)-LongInt(@Dummy); { VMT link size }
    FillChar(Pointer(longInt(@Self)+LinkSize)^,
    SizeOf(Self)-LinkSize, #0); { Clear data fields }
  end;

{ Shorthand procedure for a done/dispose }
procedure TEmptyObject.Free;
  begin
    if @Self <> nil then
      Dispose(PEmptyObject(@Self), Done);
  end;

destructor TEmptyObject.Done;
  begin
  end;

Constructor TObject.Init;
  begin
    inherited Init;
    ObjectIsInited := True;
  end;

destructor TObject.Done;
  begin
    ObjectIsInited := False;
  end;

{ TStream type registration routines }

const
  StreamTypes: PStreamRec = nil;

procedure RegisterError;
  begin
    {RunError(212);}
  end;

procedure RegisterType(var s: TStreamRec);
  var
    P: PStreamRec;
  begin
    if s.ObjType = 0 then
      begin
        Writeln('RegisterError, ObjType=', s.ObjType);
        RegisterError;
      end;
    P := StreamTypes;
    while (P <> nil) and (P^.ObjType <> s.ObjType) do
      P := P^.Next;
    if P = nil then
      begin
        s.Next := StreamTypes;
        StreamTypes := @S;
      end
    else if (P^.VmtLink <> s.VmtLink) or (P^.Load <> s.Load) or (P^.
        Store <> s.Store)
    then
      begin
        Writeln('RegisterError, ObjType=', s.ObjType);
        RegisterError;
      end;
  end { RegisterType };

procedure ReRegisterType(var s: TStreamRec);
  var
    P, l: PStreamRec;
  begin
    if s.ObjType = 0 then
      RegisterError;
    P := StreamTypes;
    l := nil;
    while (P <> nil) and (P^.ObjType <> s.ObjType) do
      begin
        l := P;
        P := P^.Next;
      end;
    if P <> nil then
      begin
        if l <> nil then
          l^.Next := P^.Next
        else
          StreamTypes := P^.Next;
      end;
    s.Next := StreamTypes;
    StreamTypes := @S;
  end { ReRegisterType };

{ TStream support routines } {JO - from VP TV objects}

const
  vmtHeaderSize = 12;
  TStream_Error = vmtHeaderSize+$04;
  TStream_Flush = vmtHeaderSize+$08;
  TStream_Read = vmtHeaderSize+$14;
  TStream_Write = vmtHeaderSize+$20;

  StreamMagic = $590C5CF1; {Cat}

  { Stream error handler                                  }
  { In    eax   = Error info                              }
  {       dl    = Error code                              }
  {       ecx   = Stream object pointer                   }
  { Uses  eax,edx                                         }

procedure DoStreamError; assembler; {$USES ecx} {$FRAME-}
asm
                movsx   edx,dl
                push    edx             { [1]:Integer = Code    }
                push    eax             { [2]:Integer = Info    }
                push    ecx             { [3]:Pointer = Self    }
                mov     eax,[ecx]
                Call    DWord Ptr [eax].TStream_Error
end
  ;

{/JO}

procedure TStream.CopyFrom(var s: TStream; Count: longInt);
  var
    n: word;
    Buffer: PByteArray;
    BufSize: word;
    Allocated: boolean;
    TTempBuf: array[0..255] of byte;
  begin
    BufSize := 32768;
    if BufSize > Count then
      BufSize := Count;
    GetMem(Buffer, BufSize);
    if Buffer = nil then
      begin
        Allocated := False;
        Buffer := PByteArray(@TTempBuf);
        BufSize := 256;
      end
    else
      Allocated := True;
    while Count > 0 do
      begin
        if Count > BufSize then
          n := BufSize
        else
          n := Count;
        s.Read(Buffer^, n);
        Write(Buffer^, n);
        Dec(Count, n);
      end;
    if Allocated then
      FreeMem(Buffer, BufSize);
  end { TStream.CopyFrom };

procedure TStream.Error(Code, Info: integer);
  type
    TErrorProc = procedure (var s: TStream);
  begin
    Status := Code;
    ErrorInfo := Info;
    if StreamError <> nil then
      TErrorProc(StreamError)(Self);
  end;

procedure TStream.Flush;
  begin
  end;

function TStream.Get: PObject; assembler; {$USES None} {$FRAME+}
asm
{Cat: добавил проверку StreamMagic}
                push    eax
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf   }
                push    4                       { [2]:DWord   = Count }
                mov     eax,Self
                push    eax                     { [3]:Pointer = Self  }
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Read
                pop     eax
                cmp     eax,StreamMagic
                jne     @@Error
{/Cat}
                push    eax
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf   }
                push    4                       { [2]:DWord   = Count }
                mov     eax,Self
                push    eax                     { [3]:Pointer = Self  }
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Read
                pop     eax
                test    eax,eax                 { Return nil }
                jz      @@4
{Cat: если пытаемся прочитать объект, регистрируемый плагином, то надо
      сначала запустить этот плагин, чтобы он провёл регистрацию}
                {$IFDEF PLUGIN}
                push    eax
                push    eax
                Call    PluginRegisterObject
                pop     eax
                {$ENDIF}
{/Cat}
                mov     edx,StreamTypes
        JMP     @@2
              @@1:
                cmp     eax,[edx].TStreamRec.ObjType
                je      @@3
                mov     edx,[edx].TStreamRec.Next
              @@2:
                test    edx,edx
                jnz     @@1
              @@Error:
                mov     ecx,Self
                mov     dl,stGetError
                Call    DoStreamError
                xor     eax,eax                 { Return nil }
                jmp     @@4
              @@3:
                push    Self                    { [1]:Pointer = TStream }
                push    [edx].TStreamRec.VmtLink{ [2]:DWord   = VMT     }
                push    0                       { [3]:Pointer = Self = nil: allocate in dynamic memory }
                Call    [edx].TStreamRec.Load
              @@4:                              { Return Self or nil }
  end
  ;

function TStream.GetPos: longInt;
  begin
    if (Status = stOK) then
      GetPos := Position
    else
      GetPos := -1;
  end;

function TStream.GetSize: longInt;
  begin
    if (Status = stOK) then
      GetSize := StreamSize
    else
      GetSize := -1;
  end;

procedure TStream.Put(P: PObject); assembler; {$USES None} {$FRAME+}
asm
{Cat: добавил запись StreamMagic}
                push    StreamMagic
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf  }
                push    4                       { [2]:DWord   = Size }
                mov     eax,Self                { [3]:Pointer = Self }
                push    eax
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Write
                pop     ecx
{/Cat}
                mov     ecx,P
                jecxz   @@4
                mov     eax,[ecx]               { VMT pointer }
                mov     edx,StreamTypes
        JMP     @@2
              @@1:
                cmp     eax,[edx].TStreamRec.VmtLink
        JE      @@3
                mov     edx,[edx].TStreamRec.Next
              @@2:
                test    edx,edx
        JNE     @@1
                mov     ecx,Self
                mov     dl,stPutError
        CALL    DoStreamError
        JMP     @@5
{/Cat}
              @@3:
                mov     ecx,[edx].TStreamRec.ObjType
              @@4:
                push    edx
                push    ecx                     { Write object type  }
                mov     eax,esp
                push    eax                     { [1]:Pointer = Buf  }
                push    4                       { [2]:DWord   = Size }
                mov     eax,Self                { [3]:Pointer = Self }
                push    eax
                mov     eax,[eax]
                Call    DWord Ptr [eax].TStream_Write
                pop     ecx
                pop     edx
                jecxz   @@5
                push    Self                    { [1]:Pointer = TStream }
                push    P                       { [2]:Pointer = Self    }
                Call    [edx].TStreamRec.Store
              @@5:
  end
  ;
procedure TStream.Read(var Buf; Count: SW_Word);
  begin
    Abstract;
  end;

{Cat: переписал ReadStr для совместимости с AnsiString
      добавил ReadLongStr
      добавил ReadStrV
      добавил ReadLongStrV
  !!  в данных процедурах в любой момент возможны изменения  !!
      при изменении обязательно согласовать со следующими процедурами:
        Advance1.NewStr
        Advance1.DisposeStr
        Advance1.NewLongStr
        Advance1.DisposeLongStr
        WinClpVP.Str2Collection}
function TStream.ReadStr: PString;
  var
    l: byte;
    P: PString;
  begin
    Read(l, SizeOf(l));
    if l > 0 then
      begin
        GetMem(P, l+1);
        SetLength(P^, l);
        Read(P^[1], l);
        ReadStr := P;
      end
    else
      ReadStr := nil;
  end;

function TStream.ReadLongStr: PLongString;
  var
    l: longInt;
    P: PLongString;
  begin
    Read(l, SizeOf(l));
    if l > 0 then
      if l > MaxLongStringLength then
        begin
          New(P);
          SetLength(P^, MaxLongStringLength);
          Read(P^[1], MaxLongStringLength);
          Seek(Position-MaxLongStringLength+l);
          ReadLongStr := P;
        end
      else
        begin
          New(P);
          SetLength(P^, l);
          Read(P^[1], l);
          ReadLongStr := P;
        end
    else
      ReadLongStr := nil;
  end { TStream.ReadLongStr: };

procedure TStream.ReadStrV(var s: String);
  var
    l: byte;
  begin
    Read(l, SizeOf(l));
    if l > 0 then
      begin
        SetLength(s, l);
        Read(s[1], l);
      end
    else
      s := '';
  end;

procedure TStream.ReadLongStrV(var s: LongString);
  var
    l: longInt;
  begin
    Read(l, SizeOf(l));
    if l > 0 then
      if l > MaxLongStringLength then
        begin
          SetLength(s, MaxLongStringLength);
          Read(s[1], MaxLongStringLength);
          Seek(Position-MaxLongStringLength+l);
        end
      else
        begin
          SetLength(s, l);
          Read(s[1], l);
        end
    else
      s := '';
  end;
{/Cat}

procedure TStream.Reset;
  begin
    Status := 0;
    ErrorInfo := 0;
  end;

procedure TStream.Seek(Pos: longInt);
  begin
    if Status = stOK then
      begin
        if Pos < 0 then
          Pos := 0
        else if Pos <= StreamSize then
          Position := Pos
        else
          Error(stSeekError, Pos);
      end;
  end;

function TStream.StrRead: PChar;
  var
    l: word;
    P: PChar;
  begin
    Read(l, SizeOf(l));
    if l = 0 then
      StrRead := nil
    else
      begin
        GetMem(P, l+1);
        Read(P[0], l);
        P[l] := #0;
        StrRead := P;
      end;
  end;

procedure TStream.StrWrite(P: PChar);
  var
    l: word;
  begin
    if P = nil then
      l := 0
    else
      l := StrLen(P);
    Write(l, SizeOf(l));
    if P <> nil then
      Write(P[0], l);
  end;

procedure TStream.Truncate;
  begin
    Abstract;
  end;

procedure TStream.Write(const Buf; Count: SW_Word);
  begin
    Abstract;
  end;

{Cat: переписал WriteStr для совместимости с AnsiString
      добавил WriteLongStr
  !!  в данных процедурах в любой момент возможны изменения  !!
      при изменении обязательно согласовать со следующими процедурами:
        Advance1.NewStr
        Advance1.DisposeStr
        Advance1.NewLongStr
        Advance1.DisposeLongStr
        WinClpVP.Str2Collection}
procedure TStream.WriteStr(P: PString);
  var
    l: byte;
  begin
    if P <> nil then
      begin
        Write(P^[0], Length(P^)+1);
      end
    else
      begin
        l := 0;
        Write(l, SizeOf(l));
      end;
  end;

procedure TStream.WriteLongStr(P: PLongString);
  var
    l: longInt;
  begin
    if P <> nil then
      begin
        l := Length(P^);
        Write(l, 4);
        Write(P^[1], l);
      end
    else
      begin
        l := 0;
        Write(l, SizeOf(l));
      end
  end;
{/Cat}

function TStream.Eof: boolean;
  begin
    Eof := GetPos >= GetSize;
  end;

procedure TStream.DoOpen(OpenMode: word);
  begin
  end;

procedure TStream.Close;
  begin
  end;

{ TDosStream }
{$I FIO._VP}

Constructor TDOSStream.Init(FileName: FNameStr; Mode: word);
  var
    Success: integer;
    {$IFDEF PACKFILE}
  label 1;
  {$ENDIF}
  begin
    inherited Init;
    FileName := FileName+#0;
    Move(FileName[1], FName, Length(FileName));
    FMode := Mode;
    {$IFDEF PACKFILE}
    if (FMode and $000F) = fmPacked then
      begin
        Handle := pOpen(@FName, FMode and $FFF0);
        Success := 0;
        if Handle <> 0
        then
          StreamSize := pFileSize(Handle)
        else
          begin
            FMode := FMode and $FFF0;
            goto 1;
          end
      end
    else
      {$ENDIF}
      begin
        {$IFDEF PACKFILE}
1:
        {$ENDIF}
        Success := AFileOpen(FName, FMode, Handle);
        if Success = 0 then
          begin
            Success := ASetFilePos(Handle, 0, 2, StreamSize);
            if Success = 0 then
              Success := ASetFilePos(Handle, 0, 0, Position);
          end;
      end;
    if (Handle = 0) or (Success <> 0) then
      begin
        if Handle = 0 then
          Handle := -1;
        Error(stInitError, Success);
      end;
  end { TDOSStream.Init };

procedure TDOSStream.Open(FileName: FNameStr; Mode: word);
  var
    Success: integer;
  label 1;
  begin
    if (Handle <> -1) then
      begin
        {$IFDEF PACKFILE}
        if (FMode and $000F) = fmPacked
        then
          pClose(Handle)
        else
          {$ENDIF}
          AFileClose(Handle);
      end;
    FileName := FileName+#0;
    Move(FileName[1], FName, Length(FileName));
    FMode := Mode;
    {$IFDEF PACKFILE}
    if (FMode and $000F) = fmPacked then
      begin
        Handle := pOpen(@FName, FMode and $FFF0);
        Success := 0;
        if Handle <> 0
        then
          StreamSize := pFileSize(Handle)
        else
          begin
            FMode := FMode and $FFF0;
            goto 1;
          end
      end
    else
      {$ENDIF}
      begin
1:
        Success := AFileOpen(FName, FMode, Handle);
        if Success = 0 then
          begin
            Success := ASetFilePos(Handle, 0, 2, StreamSize);
            if Success = 0 then
              Success := ASetFilePos(Handle, 0, 0, Position);
          end;
      end;
    if (Handle = 0) or (Success <> 0) then
      begin
        if Handle = 0 then
          Handle := -1;
        Error(stInitError, Success);
      end;
  end { TDOSStream.Open };

destructor TDOSStream.Done;
  begin
    if (Handle <> -1) then
      begin
        {$IFDEF PACKFILE}
        if (FMode and $000F) = fmPacked
        then
          pClose(Handle)
        else
          {$ENDIF}
          AFileClose(Handle);
      end;
    Handle := -1;
    inherited Done;
  end;

procedure TDOSStream.Read(var Buf; Count: SW_Word);
  var
    Success: integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
    P := @Buf;
    if Handle = -1 then
      Error(stReadError, 103)
    else if Position+Count > StreamSize then
      Error(stReadError, 0)
    else
      begin
        while (Count > 0) and (Status = stOK) do
          begin
            W := Count;
            {$IFDEF PACKFILE}
            if (FMode and $000F) = fmPacked then
              begin
                BytesMoved := pRead(Handle, W, P);
                Success := IOResult;
              end
            else
              {$ENDIF}
              Success := AFileRead(Handle, P^, W, BytesMoved);
            if Success = 0 then
              begin
                Inc(Position, BytesMoved);
                P := Pointer(longInt(P)+BytesMoved);
                Dec(Count, BytesMoved);
              end;
            if (Success <> 0) or (BytesMoved <> W) then
              begin
                Error(stReadError, Success);
                break;
              end;
          end;
      end;
    if Count <> 0 then
      FillChar(P^, Count, #0); { Error clear buffer }
  end { TDOSStream.Read };

procedure TDOSStream.ReadBlock(var Buf; Count: SW_Word; var
    BytesRead: word);
  var
    Success: integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
    P := @Buf;
    Success := -1;
    BytesRead := 0;
    if Handle = -1 then
      Success := 103;
    if (Position+Count > StreamSize) or (Status <> 0) then
      Success := 0;
    if Success <> -1 then
      Error(stReadError, Success)
    else
      begin
        while (Count > 0) and (Status = stOK) do
          begin
            W := Count;
            {$IFDEF PACKFILE}
            if (FMode and $000F) = fmPacked then
              begin
                BytesMoved := pRead(Handle, W, P);
                Success := IOResult;
              end
            else
              {$ENDIF}
              Success := AFileRead(Handle, P^, W, BytesMoved);
            if Success = 0 then
              begin
                Inc(Position, BytesMoved);
                P := Pointer(longInt(P)+BytesMoved);
                Dec(Count, BytesMoved);
                Inc(BytesRead, BytesMoved);
              end;
            if (Success <> 0) then
              begin
                Error(stReadError, Success);
                break;
              end;
            if BytesMoved <> W then
              break;
          end;
      end;
  end { TDOSStream.ReadBlock };

procedure TDOSStream.Seek(Pos: longInt);
  var
    Success: integer;
    LI: longInt;
  begin
    if Status = stOK then
      begin
        if Pos < 0 then
          Pos := 0;
        if Handle = -1
        then
          Success := 03
        else
          {$IFDEF PACKFILE} if (FMode and $000F) = fmPacked then
            begin
            LI := pSeek(Handle, Pos);
            Success := 0;
            if IOResult <> 0 then
              Success := -1;
          end
        else
          {$ENDIF}
          Success := ASetFilePos(Handle, Pos, 0, LI);
        if (Success = -1) or (LI <> Pos) then
          begin
            if (Success = -1)
            then
              Error(stSeekError, 0)
            else
              Error(stSeekError, Success);
          end
        else
          Position := LI;
      end;
  end { TDOSStream.Seek };

procedure TDOSStream.Truncate;
  var
    Success: integer;
  begin
    if Status = stOK then
      begin
        if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or
            {$ENDIF}
          ((FMode and $000F) = fmReadOnly)
        then
          Success := 103
        else
          Success := ASetFileSize(Handle, Position);
        if Success = 0 then
          StreamSize := Position
        else
          Error(stError, Success);
      end;
  end;

procedure TDOSStream.Write(const Buf; Count: SW_Word);
  var
    Success: integer;
    W, BytesMoved: SW_Word;
    P: PByteArray;
  begin
    if Handle = -1
    then
      Error(stWriteError, 103)
    else
      begin
        if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or
            {$ENDIF}
          ((FMode and $000F) = fmReadOnly)
        then
          Error(stError, 103)
        else
          begin
            P := @Buf;
            while (Count > 0) and (Status = stOK) do
              begin
                W := Count;
                {$IFNDEF OS2}
                if Count > $FFFF then
                  W := $FFFF; { Cant read >64K bytes }
                {$ENDIF}
                Success := AFileWrite(Handle, P^, W, BytesMoved);
                if Success = 0 then
                  begin
                    Inc(Position, BytesMoved);
                    P := Pointer(longInt(P)+BytesMoved);
                    Dec(Count, BytesMoved);
                    if (Position > StreamSize) then
                      StreamSize := Position;
                  end;
                if Success <> 0 then
                  begin
                    Error(stWriteError, Success);
                    break;
                  end;
              end;
          end;
      end;
  end { TDOSStream.Write };

{Cat:warn правильно ли это работает?}
procedure TDOSStream.DoOpen(OpenMode: word);
  {$IFDEF PACKFILE}
  label 1;
  {$ENDIF}
  var
    Success: integer;
  begin
    if Status = stOK then
      begin
        if Handle = -1 then
          begin
            {$IFDEF PACKFILE}
1:
            {$ENDIF}
            FMode := OpenMode;
            {$IFDEF PACKFILE}
            if (FMode and $000F) = fmPacked then
              begin
                Handle := pOpen(@FName, FMode and $FFF0);
                if Handle = 0 then
                  begin
                    FMode := FMode and $FFF0;
                    goto 1;
                  end;
              end
            else
              {$ENDIF}
              Success := AFileOpen(FName, FMode, Handle);
            Position := 0;
            if Handle = 0 then
              begin
                Handle := -1;
                Error(stOpenError, Success);
              end;
          end
        else
          Error(stOpenError, 104); { File already open }
      end;
  end { TDOSStream.DoOpen };

procedure TDOSStream.Close;
  begin
    if Handle <> -1 then
      begin
        {$IFDEF PACKFILE}
        if (FMode and $000F) = fmPacked then
          pClose(Handle)
        else
          {$ENDIF}
          AFileClose(Handle);
      end;
    Position := 0;
    Handle := -1;
  end;

{ TBufStream }

Constructor TBufStream.Init(FileName: FNameStr; Mode: word; Size:
    SW_Word);
  begin
    inherited Init(FileName, Mode);
    BufSize := Size;
    if Size = 0 then
      Error(stInitError, 0)
    else
      begin
        GetMem(Buffer, Size);
        if Buffer = nil then
          Error(stInitError, 0);
      end;
  end;

destructor TBufStream.Done;
  begin
    Flush;
    if Buffer <> nil then
      begin
        FreeMem(Buffer, BufSize);
        Buffer := nil;
      end;
    inherited Done;
  end;

procedure TBufStream.Flush;
  var
    Success: integer;
    W: SW_Word;
  begin
    if (LastMode = 2) and (BufPtr <> 0) then
      begin
        if Handle = -1 then
          Error(stError, 103)
        else
          {$IFDEF PACKFILE} if (FMode and $000F) = fmPacked then
          Error(stError, 103)
        else
          {$ENDIF}
          begin
            (* inherited Write(Buffer^, BufPtr); *)
            Success := AFileWrite(Handle, Buffer^, BufPtr, W);
            if (Success <> 0) or (W <> BufPtr) then
              Error(stError, Success);
          end;
      end;
    BufPtr := 0;
    BufEnd := 0;
  end { TBufStream.Flush };

procedure TBufStream.Read(var Buf; Count: SW_Word);
  var
    Success: integer;
    W, Bw: SW_Word;
    P: PByteArray;
  begin
    P := @Buf;
    if Handle = -1 then
      Error(stReadError, 103)
    else if Position+Count > StreamSize then
      Error(stReadError, 0)
    else
      begin
        if LastMode = 2 then
          Flush;
        LastMode := 1;
        while (Count > 0) and (Status = stOK) do
          begin
            if BufPtr = BufEnd then
              begin{ Buffer is empty }
                if Position+BufSize > StreamSize then
                  Bw := StreamSize-Position
                else
                  Bw := BufSize;
                {$IFDEF PACKFILE}
                if (FMode and $000F) = fmPacked then
                  begin
                    W := pRead(Handle, Bw, Buffer);
                    Success := IOResult;
                  end
                else
                  {$ENDIF}
                  Success := AFileRead(Handle, Buffer^, Bw, W);
                if (Success <> 0) or (Bw <> W) then
                  Error(stReadError, Success)
                else
                  begin
                    BufPtr := 0;
                    BufEnd := W;
                  end;
              end;
            if Status = stOK then
              begin
                W := BufEnd-BufPtr;
                if Count < W then
                  W := Count;
                Move(Buffer^[BufPtr], P^, W);
                Dec(Count, W);
                Inc(BufPtr, W);
                P := Pointer(longInt(P)+W);
                Inc(Position, W);
              end;
          end;
      end;
    if (Status <> stOK) and (Count > 0) then
      FillChar(P^, Count, #0);
  end { TBufStream.Read };

procedure TBufStream.Seek(Pos: longInt);
  begin
    if (Status = stOK) and (Position <> Pos) then
      begin
        Flush;
        inherited Seek(Pos);
      end;
  end;

procedure TBufStream.Truncate;
  begin
    Flush;
    inherited Truncate;
  end;

procedure TBufStream.Write(const Buf; Count: SW_Word);
  var
    Success: integer;
    W: SW_Word;
    P: PByteArray;
  begin
    if Handle = -1 then
      Error(stWriteError, 103)
    else
      begin
        if LastMode = 1 then
          Flush;
        LastMode := 2;
        P := @Buf;
        while (Count > 0) and (Status = stOK) do
          begin
            if BufPtr = BufSize then
              begin{ Buffer is full }
                Success := AFileWrite(Handle, Buffer^, BufSize, W);
                if (Success <> 0) or (W <> BufSize) then
                  Error(stError, Success);
                BufPtr := 0;
              end;
            if Status = stOK then
              begin
                W := BufSize-BufPtr;
                if Count < W then
                  W := Count;
                Move(P^, Buffer^[BufPtr], W);
                Dec(Count, W);
                Inc(BufPtr, W);
                P := Pointer(longInt(P)+W);
                Inc(Position, W);
                if Position > StreamSize then
                  StreamSize := Position;
              end;
          end;
      end;
  end { TBufStream.Write };

procedure TBufStream.Close;
  begin
    Flush;
    inherited Close;
  end;

procedure TBufStream.DoOpen(OpenMode: word);
  begin
    if Status = stOK then
      begin
        BufPtr := 0;
        BufEnd := 0;
        inherited DoOpen(OpenMode);
      end;
  end;

{ TMemoryStream }

Constructor TMemoryStream.Init(ALimit: longInt; ABlockSize: word);
  var
    W: longInt;
  begin
    inherited Init;
    if ABlockSize = 0 then
      BlkSize := 8192
    else
      BlkSize := ABlockSize;
    if ALimit <= 0 then
      W := 1
    else
      W := (ALimit+BlkSize-1) div BlkSize;
    if not ChangeListSize(W) then
      Error(stInitError, 0);
    (*
  StreamSize:=MemSize;
{AK155 2-03-2002
   При работе с Clipboard StreamSize используется в качестве укзателя
конца потока, поэтому у свежесозданного потока он должен быть равен нулю.
А это присвоение создает возможость чтения мусора за пределами фактически
записанного потока. Например, если в dn.ini указать MaxClipboardSize=0,
то DN почти гарантировано виснет при первом же взятии в буфер в редакторе.
   Но в FileCopy GetSize (то есть StreamSize) используется в качестве
размера потока (что неестественно), поэтому там тоже есть соответствующая
коррекция. Других мест, где используется StreamSize для TMemoryStream,
я не нашел.}
*)
  end;

destructor TMemoryStream.Done;
  begin
    ChangeListSize(0);
    inherited Done;
  end;

function TMemoryStream.ChangeListSize(ALimit: longInt): boolean;
  var
    i, W: longInt;
    LI: longInt;
    P: PPointerArray;
  begin
    if (ALimit <> BlkCount) then
      begin
        ChangeListSize := False;
        if ALimit > MaxPtrs then
          exit;
        if ALimit > 0 then
          begin
            LI := ALimit*SizeOf(Pointer);
            if not LowMemory and (MaxAvail > LI) then
              begin
                GetMem(P, LI);
                FillChar(P^, LI, #0);
              end
            else
              exit;
            if (BlkCount <> 0) and (BlkList <> nil) then
              if BlkCount <= ALimit then
                Move(BlkList^, P^, BlkCount*SizeOf(Pointer))
              else
                Move(BlkList^, P^, LI);
          end
        else
          begin
            P := nil;
            ALimit := 0;
          end;
        if ALimit < BlkCount then
          for W := BlkCount-1 downto ALimit do
            FreeMem(BlkList^[W], BlkSize);
        if (P <> nil) and (ALimit > BlkCount) then
          begin
            for W := BlkCount to ALimit-1 do
              begin
                if LowMemory or (MaxAvail < BlkSize) then
                  begin
                    for i := BlkCount to W-1 do
                      FreeMem(P^[i], BlkSize);
                    FreeMem(P, LI);
                    exit;
                  end
                else
                  GetMem(P^[W], BlkSize);
              end;
          end;
        if (BlkCount <> 0) and (BlkList <> nil) then
          FreeMem(BlkList, BlkCount*SizeOf(Pointer));
        BlkList := P;
        BlkCount := ALimit;
        { * REMARK * - Do not shorten this, result can be > 64K }
        MemSize := BlkCount;
        MemSize := MemSize*BlkSize;
        { * REMARK END * - Leon de Boer }
      end;
    ChangeListSize := True;
  end { TMemoryStream.ChangeListSize };

procedure TMemoryStream.Read(var Buf; Count: SW_Word);
  var
    W, CurBlock, BlockPos: word;
    LI: longInt;
    P, Q: PByteArray;
  begin
    P := @Buf;
    if Position+Count > StreamSize then
      Error(stReadError, 0)
    else
      begin
        while (Count > 0) and (Status = stOK) do
          begin
            CurBlock := Position div BlkSize;
            { * REMARK * - Do not shorten this, result can be > 64K }
            LI := CurBlock;
            LI := LI*BlkSize;
            { * REMARK END * - Leon de Boer }
            BlockPos := Position-LI;
            W := BlkSize-BlockPos;
            if W > Count then
              W := Count;
            Q := Pointer(longInt(BlkList^[CurBlock])+BlockPos);
            Move(Q^, P^, W);
            Inc(Position, W);
            P := Pointer(longInt(P)+W);
            Dec(Count, W);
          end;
      end;
    if Count <> 0 then
      FillChar(P^, Count, #0);
  end { TMemoryStream.Read };

procedure TMemoryStream.Truncate;
  var
    W: word;
  begin
    if Status = stOK then
      begin
        if Position = 0 then
          W := 1
        else
          W := (Position+BlkSize-1) div BlkSize;
        if ChangeListSize(W) then
          StreamSize := Position
        else
          Error(stError, 0);
      end;
  end;

procedure TMemoryStream.Write(const Buf; Count: SW_Word);
  var
    W, CurBlock, BlockPos: word;
    LI: longInt;
    P, Q: PByteArray;
  begin
    if Position+Count > MemSize then
      begin
        if Position+Count = 0 then
          W := 1
        else
          W := (Position+Count+BlkSize-1) div BlkSize;
        if not ChangeListSize(W) then
          begin
            Error(stWriteError, 0);
            exit;
          end;
      end;
    P := @Buf;
    while (Count > 0) and (Status = stOK) do
      begin
        CurBlock := Position div BlkSize;
        { * REMARK * - Do not shorten this, result can be > 64K }
        LI := CurBlock;
        LI := LI*BlkSize;
        { * REMARK END * - Leon de Boer }
        BlockPos := Position-LI;
        W := BlkSize-BlockPos;
        if W > Count then
          W := Count;
        Q := Pointer(longInt(BlkList^[CurBlock])+BlockPos);
        Move(P^, Q^, W);
        Inc(Position, W);
        P := Pointer(longInt(P)+W);
        Dec(Count, W);
        if Position > StreamSize then
          StreamSize := Position;
      end;
  end { TMemoryStream.Write };

function GetAnyMemoryStream: PStream;
  var
    s: PStream;
  begin
    s := nil;
    {$IFDEF OS_DOS}
    {$ELSE}
    if s = nil then
      s := New(PMemoryStream, Init(2048, 2048));
    {$ELSE}
    if s = nil then
      s := New(PMemoryStream, Init(MaxBytes, 2048));
    {$ENDIF}
    if (s <> nil) and (s^.Status <> stOK) then
      FreeObject(s);
    GetAnyMemoryStream := s;
  end;

{ TRect }

procedure CheckEmpty(var Rect: TRect);
  begin
    with Rect do
      begin
        if (A.X >= B.X) or (A.Y >= B.Y) then
          begin
            A.X := 0;
            A.Y := 0;
            B.X := 0;
            B.Y := 0;
          end;
      end;
  end;

function TPoint.Equals(P: TPoint): boolean;
  begin
    Equals := (X = P.X) and (Y = P.Y);
  end;

function TPoint.EqualsXY(AX, AY: longInt): boolean;
  begin
    EqualsXY := (X = AX) and (Y = AY);
  end;

procedure TPoint.Assign(AX, AY: longInt);
  begin
    X := AX;
    Y := AY;
  end;

function TPoint.isLE(P: TPoint): boolean;
  begin
    isLE := False;
    if (Y = P.Y) and (X <= P.X) or (Y < P.Y) then
      isLE := True;
  end;

function TPoint.isGE(P: TPoint): boolean;
  begin
    isGE := False;
    if (Y = P.Y) and (X >= P.X) or (Y > P.Y) then
      isGE := True;
  end;

procedure TRect.Assign(XA, YA, XB, YB: longInt);
  begin
    A.X := XA;
    A.Y := YA;
    B.X := XB;
    B.Y := YB;
  end;

procedure TRect.Copy(R: TRect);
  begin
    A := R.A;
    B := R.B;
  end;

procedure TRect.Move(ADX, ADY: longInt);
  begin
    Inc(A.X, ADX);
    Inc(A.Y, ADY);
    Inc(B.X, ADX);
    Inc(B.Y, ADY);
  end;

procedure TRect.Grow(ADX, ADY: longInt);
  begin
    Dec(A.X, ADX);
    Dec(A.Y, ADY);
    Inc(B.X, ADX);
    Inc(B.Y, ADY);
    CheckEmpty(Self);
  end;

procedure TRect.Intersect(R: TRect);
  begin
    if (R.A.X > A.X) then
      A.X := R.A.X;
    if (R.A.Y > A.Y) then
      A.Y := R.A.Y;
    if (R.B.X < B.X) then
      B.X := R.B.X;
    if (R.B.Y < B.Y) then
      B.Y := R.B.Y;
    CheckEmpty(Self);
  end;

procedure TRect.Union(R: TRect);
  begin
    if (R.A.X < A.X) then
      A.X := R.A.X;
    if (R.A.Y < A.Y) then
      A.Y := R.A.Y;
    if (R.B.X > B.X) then
      B.X := R.B.X;
    if (R.B.Y > B.Y) then
      B.Y := R.B.Y;
  end;

function TRect.Contains(P: TPoint): boolean;
  begin
    Contains := (P.X >= A.X) and (P.X < B.X) and
    (P.Y >= A.Y) and (P.Y < B.Y);
  end;

function TRect.Equals(R: TRect): boolean;
  begin
    Equals := (A.X = R.A.X) and (A.Y = R.A.Y) and
    (B.X = R.B.X) and (B.Y = R.B.Y);
  end;

function TRect.Empty: boolean;
  begin
    Empty := (A.X >= B.X) or (A.Y >= B.Y);
  end;

function longmul(X, Y: longInt): longInt;
  begin
    longmul := X*Y;
  end;

function LongDiv(X: longInt; Y: longInt): longInt;
  begin
    LongDiv := X div Y;
  end;

procedure FreeObject(var o);
  var
    OO: PObject absolute o;
    Op: Pointer absolute o;
  begin
    if Op <> nil then
      begin
        Dispose(OO, Done);
        Op := nil
      end;
  end;

procedure Beep(Freq, Dur: longInt); {JO}
  begin
    SysBeepEx(Freq, Dur);
  end;

end.
