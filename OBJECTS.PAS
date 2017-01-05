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
  {$IFDEF VIRTUALPASCAL} {SysUtils,} VpSysLow, {$ENDIF}
  LFN;

{ Common defines }

type
{$IFDEF OldCollection}
  CondInt = Integer;
{$ELSE}
  CondInt = LongInt;
{$ENDIF}

  Str2 = string[2];   PStr2  = ^Str2;
  Str3 = string[3];   PStr3  = ^Str3;
  Str4 = string[4];   PStr4  = ^Str4;
  Str5 = string[5];   PStr5  = ^Str5;
  Str6 = string[6];   PStr6  = ^Str6;
  Str8 = string[8];   PStr8  = ^Str8;
  Str12 = string[12]; PStr12 = ^Str12;
  Str40 = string[40]; PStr40 = ^Str40;
  Str50 = string[50]; PStr50 = ^Str50;

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
{$IFDEF USELONGSTRING}
  LongString = AnsiString;
{$ELSE}
  LongString = String[255];
{$ENDIF}
  PLongString = ^LongString;
{/Cat}

  TCharSet = Set of Char;
  PCharSet = ^TCharSet;

  WordRec = record
    Lo, Hi: Byte;
  end;

  LongRec = record
    Lo, Hi: Word;
  end;

  PtrRec = record
    Ofs, Seg: Word;
  end;

const
  {$IFDEF BIT_16}
  MaxBytes = 65520;
  {$ELSE}
  MaxBytes = 128*1024*1024;
  {$ENDIF}

  MaxWords = MaxBytes DIV SizeOf(Word);
  MaxPtrs  = MaxBytes DIV SizeOf(Pointer);

type

  {$IFDEF BIT_16}
  Sw_Word    = Word;
  Sw_Integer = Integer;
  {$ELSE}
    {$IFDEF FILE_16}
    Sw_Word    = Word;
    Sw_Integer = Integer;
    {$ELSE}
    Sw_Word    = LongInt;
    Sw_Integer = LongInt;
    {$ENDIF}
  {$ENDIF}
   AInt = {$IFNDEF BIT_32}Integer{$ELSE BIT_32}SmallInt{$ENDIF};

   AWord= {$IFNDEF BIT_32}Word{$ELSE BIT_32}SmallWord{$ENDIF};

  PByteArray = ^TByteArray;
  TByteArray = Array[0..MaxBytes-1] of byte;

  PWordArray = ^TWordArray;
  TWordArray = Array[0..MaxWords-1] of Aword;

  PPointerArray = ^TPointerArray;
  TPointerArray = Array [0..MaxPtrs-1] of pointer;

const

{ TStream access modes }

 {$IFNDEF VIRTUALPASCAL}
  stCreate    = $3C02;           { Create new file }
  stOpenRead  = $3D40;           { Read access only }
  stOpenWrite = $3D41;           { Write access only }
  stOpen      = $3D42;           { Read and write access }
  stOpenPacked= $3D23;           { Read access only, packed files too }

{ File share mode constants }

  fmClean     = $FF00;           { Mask to clean low byte of file mode constatns }

  fmOpenMode  = $FFF0;           { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly  = $0000;           { Open read-only file }
  fmWriteOnly = $0001;           { Open file for write only }
  fmReadWrite = $0002;           { Open file as for read, as for write }
  fmPacked    = $0003;           { Open a packed file, if can }

  fmDeny      = $FF0F;           { Mask to apply fmDenyXXX }
  fmDenyAll   = $0010;           { Exclusive file use }
  fmDenyWrite = $0020;           { Deny write access }
  fmDenyRead  = $0030;           { Deny read access }
  fmDenyNone  = $0040;           { Deny no access }
  fmDenyChild = $0080;           { Don't give right's to child }

 {$ELSE VP}
  stCreate    = $FFFF { $DC0B };                              { Create new file }
  stOpenRead  = Open_Access_ReadOnly or Open_Share_DenyNone;  { Read access only }
  stOpenWrite = Open_Access_WriteOnly or Open_Share_DenyNone; { Write access only }
  stOpen      = Open_Access_ReadWrite or Open_Share_DenyNone; { Read and write access }
  stOpenPacked= Open_Access_ReadWrite+1;                      { Read access only, packed files too }

{ File share mode constants }
  fmClean     = $FF00;                    { Mask to clean low byte of file mode constatns }

  fmOpenMode  = $FFF0;                    { Mask to apply fmReadOnly/fmWriteOnly/fmReadWrite }
  fmReadOnly  = Open_Access_ReadOnly;     { Open read-only file }
  fmWriteOnly = Open_Access_WriteOnly;    { Open file for write only }
  fmReadWrite = Open_Access_ReadWrite;    { Open file as for read, as for write }
  fmPacked    = Open_Access_ReadWrite+1;  { Open a packed file, if can }

  fmDeny      = $FF0F;                    { Mask to apply fmDenyXXX }
  fmDenyAll   = Open_Share_DenyReadWrite; { Exclusive file use }
  fmDenyWrite = Open_Share_DenyWrite;     { Deny write access }
  fmDenyRead  = Open_Share_DenyRead;      { Deny read access }
  fmDenyNone  = Open_Share_DenyNone;      { Deny no access }
  fmDenyChild = Open_Share_DenyNone;      { Don't give right's to child }

 {$ENDIF}

{ TStream error codes }

  stOk         =  0;              { No error }
  stError      = -1;              { Access error }
  stInitError  = -2;              { Cannot initialize stream }
  stReadError  = -3;              { Read beyond end of stream }
  stWriteError = -4;              { Cannot expand stream }
  stGetError   = -5;              { Get of unregistered object type }
  stPutError   = -6;              { Put of unregistered object type }
  stSeekError  = -7;              { Stream seek error }
  stOpenError  = -8;              { Stream open error }

{$IFDEF OldCollection}
{ VMT header size }
 {$IFDEF BIT_32}
  vmtHeaderSize = 12;
 {$ELSE}
  vmtHeaderSize = 8;
 {$ENDIF}

{$ENDIF}

type

{ TStreamRec }

  PStreamRec = ^TStreamRec;
  TStreamRec = record
  {$IFNDEF NoCallspcb}
    ObjType: Word; {было AWord}
  {$ELSE}
    ObjType: Word;
  {$ENDIF}
  {$IFDEF VER70}{$IFNDEF DPMI}
    VmtLink: Word;
  {$ELSE} VmtLink: Pointer;{$ENDIF}
  {$ELSE} VmtLink: Pointer;{$ENDIF}
{Cat}
{$IFDEF LOGOBJLOADSTORE}
    ObjName: String[30];
{$ENDIF}
    Load:  Pointer;
    Store: Pointer;
    Next:  PStreamRec;
  end;

{ TObject base object }

  PEmptyObject = ^TEmptyObject;
  TEmptyObject = object
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    constructor Init;
    procedure   Free;
    destructor  Done; virtual;
  end;

  PObject = ^TObject;
  TObject = object(TEmptyObject)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    ObjectIsInited: Boolean;
    constructor Init;
    destructor  Done; virtual;
  end;

{ TStream }

  PStream = ^TStream;
  TStream = object(TObject)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Status: Integer;
    ErrorInfo: Integer;
    StreamSize: LongInt;                         { Stream current size }
    Position  : LongInt;                         { Current position }
    procedure CopyFrom(var S: TStream; Count: Longint);
    procedure Error(Code, Info: Integer); virtual;
    procedure Flush; virtual;
    function  Get: PObject;
    function  GetPos: Longint; virtual;
    function  GetSize: Longint; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: Sw_Word); virtual;
    function  ReadStr: PString;
    function  ReadLongStr: PLongString; {Cat}
    procedure ReadStrV(var S: String); {Cat}
    procedure ReadLongStrV(var S: LongString); {Cat}
    procedure Reset;
    procedure Seek(Pos: Longint); virtual;
    function  StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Sw_Word); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function  Eof: Boolean; {-$VOL}
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
  end;

 FNameStr = String;
 AsciiZ   = Array [0..255] Of Char;
 THandle  = Integer;

{ File name string }

type

{ TDosStream }

  PDosStream = ^TDosStream;
  TDosStream = object(TStream)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Handle: Integer;
    FName : AsciiZ;
    FMode:  Word;
    constructor Init(FileName: FNameStr; Mode: Word);
    procedure   Open(FileName: FNameStr; Mode: Word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: Sw_Word); virtual;
    procedure ReadBlock(var Buf;Count: Sw_Word;var BytesRead: Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Sw_Word); virtual;
    procedure DoOpen (OpenMode: Word); virtual;
    procedure Close; virtual;
  end;

{ TBufStream }

  PBufStream = ^TBufStream;
  TBufStream = object(TDosStream)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
    Buffer:   PByteArray;
    BufSize:  Sw_Word;
    BufPtr:   Sw_Word; { Buffer start }
    BufEnd:   Sw_Word;
    LastMode: Byte;    { Last buffer mode }
    constructor Init(FileName: FNameStr; Mode: Word; Size: Sw_Word);
    destructor Done; virtual;
    procedure Flush; virtual;
    procedure Read(var Buf; Count: Sw_Word); virtual;
    procedure Seek(Pos: Longint); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Sw_Word); virtual;
    procedure DoOpen(OpenMode: Word); virtual;
    procedure Close; virtual;
  end;

{ TMemoryStream }

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
  {Cat: этот объект вынесен в плагинную модель; изменять крайне осторожно!}
     BlkCount: LongInt;                           { Number of segments }
     BlkSize : Word;                              { Memory block size }
     MemSize : LongInt;                           { Memory alloc size }
     BlkList : PPointerArray;                     { Memory block list }
    constructor Init(ALimit: Longint; ABlockSize: Word);
    destructor  Done; virtual;
    procedure Read(var Buf; Count: Sw_Word); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: Sw_Word); virtual;
  private
    function ChangeListSize(ALimit: LongInt): Boolean;
  end;

{ TPoint object }

  PPoint = ^TPoint;
  TPoint = object
    X, Y: LongInt;
    function  Equals(P: TPoint): Boolean;
    function  EqualsXY(AX, AY: LongInt): Boolean;
    procedure Assign(AX, AY: LongInt);
    function  isLE(P: TPoint): Boolean; {less then or equal}
    function  isGE(P: TPoint): Boolean; {great thean or equal}
  end;

{ Rectangle object }

  TRect = object
    A, B: TPoint;
    procedure Assign(XA, YA, XB, YB: LongInt);
    procedure Copy(R: TRect);
    procedure Move(ADX, ADY: LongInt);
    procedure Grow(ADX, ADY: LongInt);
    procedure Intersect(R: TRect);
    procedure Union(R: TRect);
    function Contains(P: TPoint): Boolean;
    function Equals(R: TRect): Boolean;
    function Empty: Boolean;
  end;

procedure FreeObject(var O);

{ Longint routines }

function LongMul(X, Y: LongInt): Longint;
function LongDiv(X: Longint; Y: LongInt): LongInt;

{ Stream routines }

procedure RegisterType( var S: TStreamRec );
procedure ReRegisterType( var S: TStreamRec );

function  GetAnyMemoryStream: PStream;
procedure Abstract;

procedure Beep(Freq,Dur: LongInt); {JO}

const
  { Stream error procedure }
  StreamError: Pointer = nil;

implementation
uses
  {$IFDEF Windows} WinProcs,OMemory, {$ELSE} Memory, {$ENDIF}
  {$IFDEF StreamDebug} DnApp, {$ENDIF}
  {$IFNDEF NONBP} BStrings, {$ELSE} Strings, {$ENDIF}
  {$IFDEF FPC} CallSpec, {$ELSE} {$IFNDEF NoCallspcb} Callspcb, {$ENDIF} {$ENDIF}
  {$IFDEF WIN32} Windows, {$ENDIF}
  {$IFDEF PACKFILE} PackF_RS, {$ENDIF}
  {$IFNDEF VIRTUALPASCAL} ExtraMem, {$ENDIF}
  {$IFDEF PLUGIN} Plugin, {$ENDIF}
  Commands{Cat}, Advance;

procedure Abstract;
begin
  {RunError(211);}
end;

type
  DummyObject = object(TEmptyObject)
    Data: record end;
  end;

const
  LinkSize = SizeOf(DummyObject);

constructor TEmptyObject.Init;
{
var
  LinkSize: LongInt;
  Dummy: DummyObject;
}
begin
//LinkSize := LongInt(@Dummy.Data)-LongInt(@Dummy); { VMT link size }
  FillChar(Pointer(LongInt(@Self)+LinkSize)^,
   SizeOf(Self)-LinkSize, #0);                      { Clear data fields }
end;

{ Shorthand procedure for a done/dispose }
procedure TEmptyObject.Free;
begin
  If @Self<>nil then
   Dispose(PEmptyObject(@Self), Done);
end;

destructor TEmptyObject.Done;
begin
end;

constructor TObject.Init;
begin
  Inherited Init;
  ObjectIsInited:=True;
end;

destructor  TObject.Done;
begin
  ObjectIsInited:=False;
end;

{ TStream type registration routines }

const
  StreamTypes: PStreamRec = Nil;

procedure RegisterError;
begin
  {RunError(212);}
end;

procedure RegisterType (var S: TStreamRec);
var P: PStreamRec;
begin
  if S.ObjType = 0 then begin
    WriteLn('RegisterError, ObjType=', S.ObjType);
    RegisterError;
  end;
  P:=StreamTypes;
  while (P <> Nil) and (P^.ObjType <> S.ObjType) do P:=P^.Next;
  if P = Nil then begin
    S.Next:=StreamTypes;
    StreamTypes:=@S;
  end else
  if (P^.VmtLink<>S.VmtLink) or (P^.Load<>S.Load) or (P^.Store <> S.Store)
  then begin
    WriteLn('RegisterError, ObjType=', S.ObjType);
    RegisterError;
  end;
end;

procedure ReRegisterType( var S: TStreamRec );
var
  P, L: PStreamRec;
begin
   if S.ObjType = 0 then RegisterError;
   P:=StreamTypes; L:=nil;
   while (P <> nil) and (P^.ObjType <> S.ObjType) do begin
     L:=P; P:=P^.Next;
   end;
   if P <> nil then begin
     if L <> nil then L^.Next:=P^.Next else StreamTypes:=P^.Next;
   end;
   S.Next:=StreamTypes;
   StreamTypes:=@S;
end;

{$IFDEF NoCallspcb}
{ TStream support routines } {JO - from VP TV objects}

const
 {$IFNDEF OldCollection}
  vmtHeaderSize = 12;
 {$ENDIF}
  TStream_Error = vmtHeaderSize + $04;
  TStream_Flush = vmtHeaderSize + $08;
  TStream_Read  = vmtHeaderSize + $14;
  TStream_Write = vmtHeaderSize + $20;

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
end;

{/JO}
{$ENDIF}

procedure TStream.CopyFrom(var S: TStream; Count: Longint);
var
  N: Word;
  Buffer: PByteArray;
  BufSize: Word;
  Allocated: Boolean;
  TTempBuf: array[0..255] of byte;
begin
  BufSize:=32768;
  if BufSize > Count then BufSize:=Count;
  GetMem(Buffer, BufSize);
  if Buffer = nil then begin
    Allocated:=False;
    Buffer:=PByteArray(@TTempBuf);
    BufSize:=256;
  end else
    Allocated:=True;
  while Count > 0 do
  begin
    if Count > BufSize then N:=BufSize else N:=Count;
    S.Read(Buffer^, N);
    Write(Buffer^, N);
    Dec(Count, N);
  end;
  if Allocated then FreeMem(Buffer, BufSize);
end;

procedure TStream.Error(Code, Info: Integer);
type
  TErrorProc = procedure(var S: TStream);
begin
  Status:=Code;
  ErrorInfo:=Info;
  if StreamError <> nil then TErrorProc(StreamError)(Self);
end;

procedure TStream.Flush;
begin
end;

{$IFNDEF NoCallspcb}
function TStream.Get: PObject;
var
  Magic: Word;   {было AWord}
  ObjType: Word; {было AWord}
  P: PStreamRec;
  O: PObject;
{$IFDEF LOGOBJLOADSTORE}
  F: Text;
{$ENDIF}
begin
  {$IFNDEF CNPRG} Read(Magic, SizeOf(Magic)); {$ENDIF}
  Read(ObjType, SizeOf(ObjType));
  {$IFNDEF CNPRG}
  if Magic<>$7C1B then begin
    Error(stError, ObjType);
    Get:=nil;
  end else
  {$ENDIF}
  if (ObjType <> 0) then begin
    P:=StreamTypes;
    {Cat: если пытаемся прочитать объект, регистрируемый плагином, то надо
          сначала запустить этот плагин, чтобы он провёл регистрацию}
    {$IFDEF PLUGIN}
    PluginRegisterObject(ObjType);
    {$ENDIF}
    {/Cat}
    while (P <> nil) and (P^.ObjType <> ObjType) do P := P^.Next;
{Cat}
{$IFDEF LOGOBJLOADSTORE}
    if IOResult <> 0 then
      ;
    System.Assign(F, 'stream.log');
    System.Append(F);
    if (TypeOf(self) = TypeOf(TDosStream)) or (TypeOf(self) = TypeOf(TBufStream)) then
      System.Write(F, PDosStream(@self)^.FName)
    else
      System.Write(F, 'non-file stream');
    System.Write(F, ': Load ObjType=',ObjType);
    if (P = Nil) or (ObjType=0) then
      System.Writeln(F, ' (UNKNOWN OBJECT)')
    else
      System.Writeln(F, ' (',P^.ObjName,')');
    System.Close(F);
    if IOResult <> 0 then
      ;
{$ENDIF}
{/Cat}
    If (P = nil) then begin
      Error(stGetError, ObjType); Get:=Nil;
    end else
    begin
  {$IFDEF VER70}{$IFNDEF DPMI}
      O:=CallPointerConstructor(P^.Load,nil,Ptr(DSeg,P^.VMTLink),@Self);
  {$ELSE} O:=CallPointerConstructor(P^.Load,nil,P^.VMTLink,@Self);{$ENDIF}
  {$ELSE} O:=CallPointerConstructor(P^.Load,nil,P^.VMTLink,@Self);{$ENDIF}
      {$IFNDEF CNPRG}
      Read(Magic, SizeOf(Magic));
      if Magic<>$005D then begin
        Error(stGetError, ObjType);
        FreeObject(O);
      end;
      {$ENDIF}
      Get:=O;
    end;
  end else
    Get:=nil;
END;
{$ELSE}
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
  end;
{$ENDIF}

function TStream.GetPos: Longint;
begin
  if (Status = stOk) then GetPos:=Position else GetPos:=-1;
end;

function TStream.GetSize: Longint;
begin
  if (Status = stOk) then GetSize:=StreamSize else GetSize:=-1;
end;

{$IFNDEF NoCallspcb}
procedure TStream.Put(P: PObject);
var
  Q: PStreamRec;
  Magic: Word;   {было AWord}
  ObjType: Word; {было AWord}
  {$IFDEF BIT_16}
  VmtPtr: ^Word;     Link: Word;    {$ELSE}
  VmtPtr: ^LongInt;  Link: LongInt; {$ENDIF}
{$IFDEF LOGOBJLOADSTORE}
  F: Text;
{$ENDIF}
begin
  VmtPtr:=Pointer(P);
  ObjType:=0;
  If (P <> nil) and (VmtPtr^ <> 0) then begin
    Q:=StreamTypes;
    Link:=VmtPtr^;
    {$IFDEF BIT_16}
{$IFDEF VER70}{$IFNDEF DPMI}
    while (Q <> Nil) and (Q^.VMTLink <> Link) do Q:=Q^.Next;
{$ELSE}while (Q <> Nil) and (Ofs(Q^.VMTLink^) <> Link) do Q:=Q^.Next;{$ENDIF}
{$ELSE}while (Q <> Nil) and (Ofs(Q^.VMTLink^) <> Link) do Q:=Q^.Next;{$ENDIF}
    {$ELSE}
    while (Q <> Nil) and (LongInt(Q^.VMTLink) <> Link) do Q:=Q^.Next;
    {$ENDIF}
    if (Q = Nil) then begin
{$IFDEF StreamDebug}
      PApplication(Application)^.DoDump(212);
{$ENDIF}
{Cat: не следует так пугаться, если попался не зарегистрированный
      объект, просто не будем его записывать}
      {Error(stPutError, LongInt(P));} Exit;
{/Cat}
    end else
      ObjType:=Q^.ObjType;
  end;
  {$IFNDEF CNPRG} Magic:=$7C1B; Write(Magic, SizeOf(Magic)); {$ENDIF}
{Cat}
{$IFDEF LOGOBJLOADSTORE}
  if IOResult <> 0 then
    ;
  System.Assign(F, 'stream.log');
  System.Append(F);
    if (TypeOf(self) = TypeOf(TDosStream)) or (TypeOf(self) = TypeOf(TBufStream)) then
    System.Write(F, PDosStream(@self)^.FName)
  else
    System.Write(F, 'non-file stream');
  System.Write(F, ': Store ObjType=',ObjType);
  if (Q = Nil) or (ObjType=0) then
    System.Writeln(F, ' (UNKNOWN OBJECT)')
  else
    System.Writeln(F, ' (',Q^.ObjName,')');
  System.Close(F);
  if IOResult <> 0 then
    ;
{$ENDIF}
{/Cat}
  Write(ObjType, SizeOf(ObjType));
  If ObjType <> 0 then begin
    CallPointerMethod(Q^.Store,P, @Self);
    {$IFNDEF CNPRG} Magic:=$005D; Write(Magic, SizeOf(Magic)); {$ENDIF}
  end;
END;
{$ELSE}
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
  end;
{$ENDIF}
procedure TStream.Read(var Buf; Count: Sw_Word);
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
  L: {$IFDEF USEANSISTRING} LongInt {$ELSE} Byte {$ENDIF};
  P: PString;
begin
  Read(L, SizeOf(L));
  if L > 0 then
{$IFDEF USEANSISTRING}
    if L > MaxReasonableStringLength then
      begin
        New(P);
        SetLength(P^, MaxReasonableStringLength);
        Read(P^[1], MaxReasonableStringLength);
        Seek(Position-MaxReasonableStringLength+L);
        ReadStr:=P;
      end
    else
{$ENDIF}
      begin
{$IFDEF USEANSISTRING}
        New(P);
{$ELSE}
        GetMem(P, L+1);
{$ENDIF}
        SetLength(P^, L);
        Read(P^[1], L);
        ReadStr:=P;
      end
  else
    ReadStr:=nil;
end;

function TStream.ReadLongStr: PLongString;
var
  L: {$IFDEF USELONGSTRING} LongInt {$ELSE} Byte {$ENDIF};
  P: PLongString;
begin
  Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
{$IFDEF USELONGSTRING}
        New(P);
{$ELSE}
        GetMem(P, MaxLongStringLength+1);
{$ENDIF}
        SetLength(P^, MaxLongStringLength);
        Read(P^[1], MaxLongStringLength);
        Seek(Position-MaxLongStringLength+L);
        ReadLongStr:=P;
      end
    else
      begin
{$IFDEF USELONGSTRING}
        New(P);
{$ELSE}
        GetMem(P, L+1);
{$ENDIF}
        SetLength(P^, L);
        Read(P^[1], L);
        ReadLongStr:=P;
      end
  else
    ReadLongStr:=nil;
end;

procedure TStream.ReadStrV(var S: String);
var
  L: {$IFDEF USEANSISTRING} LongInt {$ELSE} Byte {$ENDIF};
begin
  Read(L, SizeOf(L));
  if L > 0 then
{$IFDEF USEANSISTRING}
    if L > MaxReasonableStringLength then
      begin
        SetLength(S, MaxReasonableStringLength);
        Read(S[1], MaxReasonableStringLength);
        Seek(Position-MaxReasonableStringLength+L);
      end
    else
{$ENDIF}
      begin
        SetLength(S, L);
        Read(S[1], L);
      end
  else
    S := '';
end;

procedure TStream.ReadLongStrV(var S: LongString);
var
  L: {$IFDEF USELONGSTRING} LongInt {$ELSE} Byte {$ENDIF};
begin
  Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
        SetLength(S, MaxLongStringLength);
        Read(S[1], MaxLongStringLength);
        Seek(Position-MaxLongStringLength+L);
      end
    else
      begin
        SetLength(S, L);
        Read(S[1], L);
      end
  else
    S := '';
end;
{/Cat}

procedure TStream.Reset;
begin
  Status:=0;
  ErrorInfo:=0;
end;

procedure TStream.Seek(Pos: LongInt);
begin
   if Status = stOk then begin
     if Pos < 0 then
       Pos:=0 else
     if Pos <= StreamSize then
       Position:=Pos else
       Error(stSeekError, Pos);
   end;
end;

function TStream.StrRead: PChar;
var
  L: Word;
  P: PChar;
begin
  Read(L, SizeOf(L));
  if L = 0 then StrRead:=nil else
  begin
    GetMem(P, L + 1);
    Read(P[0], L);
    P[L]:=#0;
    StrRead:=P;
  end;
end;

procedure TStream.StrWrite(P: PChar);
var
  L: Word;
begin
  if P = nil then L:=0 else L:=StrLen(P);
  Write(L, SizeOf(L));
  if P <> nil then Write(P[0], L);
end;

procedure TStream.Truncate;
begin
  Abstract;
end;

procedure TStream.Write(const Buf; Count: Sw_Word);
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
  L: {$IFDEF USEANSISTRING} LongInt {$ELSE} Byte {$ENDIF};
begin
  if P <> nil then
    begin
{$IFDEF USEANSISTRING}
      L := Length(P^);
      Write(L, 4);
      Write(P^[1], L);
{$ELSE}
      Write(P^[0], Length(P^)+1);
{$ENDIF}
    end
  else
    begin
      L := 0;
      Write(L, SizeOf(L));
    end;
end;

procedure TStream.WriteLongStr(P: PLongString);
var
  L: {$IFDEF USELONGSTRING} LongInt {$ELSE} Byte {$ENDIF};
begin
  if P <> nil then
    begin
{$IFDEF USELONGSTRING}
      L := Length(P^);
      Write(L, 4);
      Write(P^[1], L);
{$ELSE}
      Write(P^[0], Length(P^)+1);
{$ENDIF}
    end
  else
    begin
      L := 0;
      Write(L, SizeOf(L));
    end
end;
{/Cat}

function TStream.Eof: boolean;
begin
  Eof := GetPos >= GetSize;
end;

procedure TStream.DoOpen(OpenMode: Word);
begin
end;

procedure TStream.Close;
begin
end;

{ TDosStream }
{$IFDEF VIRTUALPASCAL}{$I FIO._VP}
 {$ELSE}
  {$IFDEF FPC}{$I FIO._FP}
   {$ELSE}
    {$I FIO._BP}
  {$ENDIF}
{$ENDIF}

constructor TDosStream.Init(FileName: FNameStr; Mode: Word);
var
  Success: Integer;
  {$IFDEF PACKFILE}
label 1;
  {$ENDIF}
begin
  inherited Init;
  {$IFDEF OS_WINDOWS}
  AnsiToOEM(FileName, FName);
  {$ELSE}
  FileName:=FileName+#0;
  Move(FileName[1], FName, Length(FileName));
  {$ENDIF}
  FMode:=Mode;
  {$IFDEF PACKFILE}
  if (FMode and $000F)=fmPacked then begin
    Handle:=pOpen(@FName, FMode and $FFF0);
    Success:=0;
    If Handle <> 0
     then StreamSize:=pFileSize(Handle)
     else begin
           FMode := FMode and $FFF0;
           goto 1;
          end
  end else
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
end;

procedure TDosStream.Open(FileName: FNameStr; Mode: Word);
var
  Success: Integer;
label 1;
begin
  if (Handle <> -1) then begin
  {$IFDEF PACKFILE}
    if (FMode and $000F)=fmPacked
     then pClose(Handle)
     else
  {$ENDIF}
     AFileClose(Handle);
  end;
  {$IFDEF OS_WINDOWS}
  AnsiToOEM(FileName, FName);
  {$ELSE}
  FileName:=FileName+#0;
  Move(FileName[1], FName, Length(FileName));
  {$ENDIF}
  FMode:=Mode;
  {$IFDEF PACKFILE}
  if (FMode and $000F)=fmPacked then begin
    Handle:=pOpen(@FName, FMode and $FFF0);
    Success:=0;
    If Handle <> 0
     then StreamSize:=pFileSize(Handle)
     else begin
           FMode := FMode and $FFF0;
           goto 1;
          end
  end else
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
end;

destructor TDosStream.Done;
begin
  if (Handle <> -1) then begin
  {$IFDEF PACKFILE}
    if (FMode and $000F)=fmPacked
     then pClose(Handle)
     else
  {$ENDIF}
          AFileClose(Handle);
  end;
  Handle:=-1;
  inherited Done;
end;

procedure TDosStream.Read(var Buf; Count: Sw_Word);
var
  Success: Integer;
  W, BytesMoved: Sw_Word;
  P: PByteArray;
begin
  P:=@Buf;
  If Handle = -1 then Error(stReadError, 103) else
  If Position + Count > StreamSize then Error(stReadError, 0)
  else begin
    while (Count > 0) and (Status = stOk) do begin
      W:=Count;
      {$IFDEF PACKFILE}
      if (FMode and $000F)=fmPacked then begin
        BytesMoved:=pRead(Handle, W, P);
        Success:=IOResult;
      end else
      {$ENDIF}
        Success:=AFileRead(Handle, P^, W, BytesMoved);
      if Success = 0 then begin
        Inc(Position, BytesMoved);
        P := Pointer(LongInt(P) + BytesMoved);
        Dec(Count, BytesMoved);
      end;
      If (Success <> 0) or (BytesMoved <> W) then begin
        Error(stReadError, Success);
        break;
      end;
    end;
 end;
 if Count <> 0 then FillChar(P^, Count, #0); { Error clear buffer }
end;

procedure TDosStream.ReadBlock(var Buf;Count: Sw_Word;var BytesRead: Word);
var
  Success: Integer;
  W, BytesMoved: Sw_Word;
  P: PByteArray;
begin
  P:=@Buf; Success:=-1; BytesRead:=0;
  If Handle = -1 then Success:=103;
  If (Position + Count > StreamSize) or (Status <> 0) then Success:=0;
  if Success<> -1 then Error(stReadError, Success)
  else begin
    while (Count > 0) and (Status = stOk) do begin
      W:=Count;
      {$IFDEF PACKFILE}
      if (FMode and $000F)=fmPacked then begin
        BytesMoved:=pRead(Handle, W, P);
        Success:=IOResult;
      end else
      {$ENDIF}
        Success:=AFileRead(Handle, P^, W, BytesMoved);
      if Success = 0 then begin
        Inc(Position, BytesMoved);
        P := Pointer(LongInt(P) + BytesMoved);
        Dec(Count, BytesMoved);
        Inc(BytesRead, BytesMoved);
      end;
      If (Success <> 0) then begin Error(stReadError, Success); break; end;
      if BytesMoved <> W then break;
    end;
 end;
end;


procedure TDosStream.Seek(Pos: LongInt);
var
  Success: Integer;
  Li: LongInt;
begin
  if Status = stOk then begin
    if Pos < 0 then Pos:=0;
    if Handle = -1
     then Success:= 03
     else
  {$IFDEF PACKFILE}
         if (FMode and $000F)=fmPacked then begin
           Li:=pSeek(Handle, Pos);
           Success:=0;
           if IOResult<>0 then Success:=-1;
          end else
  {$ENDIF}
                  Success:=ASetFilePos(Handle, Pos, 0, Li);
    if (Success = -1) or (Li <> Pos) then begin
      if (Success = -1)
       then Error(stSeekError, 0)
       else Error(stSeekError, Success);
    end else Position:=Li;
  end;
end;

procedure TDosStream.Truncate;
var
  Success: Integer;
begin
  if Status = stOk then begin
    if   {$IFDEF PACKFILE} ((FMode and $000F)=fmPacked) or {$ENDIF}
       ((FMode and $000F)=fmReadOnly)
     then Success:=103
     else Success:=ASetFileSize(Handle, Position);
    if Success = 0 then StreamSize:=Position
    else Error(stError, Success);
  end;
end;


procedure TDosStream.Write(const Buf; Count: Sw_Word);
var
  Success: Integer;
  W, BytesMoved: Sw_Word;
  P: PByteArray;
begin
  if Handle = -1
   then Error(stWriteError, 103)
   else begin
    if   {$IFDEF PACKFILE}((FMode and $000F)=fmPacked) or {$ENDIF}
       ((FMode and $000F)=fmReadOnly)
     then Error(stError, 103)
     else begin
      P:=@Buf;
      while (Count > 0) and (Status = stOk) do begin
        W:=Count;
        {$IFNDEF OS2}
        if Count > $FFFF then W:=$FFFF; { Cant read >64K bytes }
        {$ENDIF}
        Success:=AFileWrite(Handle, P^, W, BytesMoved);
        If Success = 0 then begin
          Inc(Position, BytesMoved);
          P:=Pointer(LongInt(P) + BytesMoved);
          Dec(Count, BytesMoved);
          If (Position > StreamSize) then StreamSize:=Position;
        end;
        if Success <> 0 then begin
          Error(stWriteError, Success);
          break;
        end;
      end;
    end;
  end;
end;

{Cat:warn правильно ли это работает?}
procedure TDosStream.DoOpen (OpenMode: Word);
{$IFDEF PACKFILE}
label 1;
{$ENDIF}
var
  Success: Integer;
begin
  if Status = stOk then
    begin
      if Handle = -1 then
        begin
{$IFDEF PACKFILE}
          1:
{$ENDIF}
          FMode:=OpenMode;
{$IFDEF PACKFILE}
          if (FMode and $000F)=fmPacked then
            begin
              Handle:=pOpen(@FName, FMode and $FFF0);
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
              Handle:=-1;
              Error(stOpenError, Success);
            end;
        end
      else Error(stOpenError, 104); { File already open }
  end;
end;

procedure TDosStream.Close;
begin
  if Handle <> -1 then begin
  {$IFDEF PACKFILE}
    if (FMode and $000F)=fmPacked then
      pClose(Handle) else
  {$ENDIF}
      AFileClose(Handle);
  end;
  Position:=0;
  Handle:=-1;
end;

{ TBufStream }

constructor TBufStream.Init(FileName: FNameStr; Mode: Word; Size: Sw_Word);
begin
  inherited Init(FileName, Mode);
  BufSize:=Size;
  if Size = 0 then Error(stInitError, 0) else begin
    GetMem(Buffer, Size);
    If Buffer = nil then Error(stInitError, 0);
  end;
end;

destructor TBufStream.Done;
begin
  Flush;
  if Buffer <> nil then begin
    FreeMem(Buffer, BufSize);
    Buffer:=nil;
  end;
  inherited Done;
end;

procedure TBufStream.Flush;
var
  Success: Integer;
  W: Sw_Word;
begin
  if (LastMode = 2) and (BufPtr <> 0) then begin
    if Handle = -1 then
      Error(stError, 103) else
  {$IFDEF PACKFILE}
    if (FMode and $000F)=fmPacked then
      Error(stError, 103) else
  {$ENDIF}
    begin
      (* inherited Write(Buffer^, BufPtr); *)
      Success:=AFileWrite(Handle, Buffer^, BufPtr, W);
      If (Success <> 0) or (W <> BufPtr) then Error(stError, Success);
    end;
  end;
  BufPtr:=0;
  BufEnd:=0;
end;

procedure TBufStream.Read(Var Buf; Count: Sw_Word);
var
  Success: Integer;
  W, Bw: Sw_Word;
  P: PByteArray;
begin
  P:=@Buf;
  if Handle = -1 then Error(stReadError, 103) else
  If Position + Count > StreamSize then Error(stReadError, 0)
  else begin
    If LastMode = 2 then Flush;
    LastMode:=1;
    while (Count > 0) and (Status = stOk) do begin
      if BufPtr = BufEnd then begin  { Buffer is empty }
        if Position + BufSize > StreamSize then Bw:=StreamSize - Position
        else Bw:=BufSize;
  {$IFDEF PACKFILE}
        if (FMode and $000F)=fmPacked then begin
          W:=pRead(Handle, Bw, Buffer);
          Success:=IOResult;
        end else
  {$ENDIF}
          Success:=AFileRead(Handle, Buffer^, Bw, W);
        if (Success <> 0) or (Bw <> W) then Error(stReadError, Success)
        else begin
          BufPtr:=0;
          BufEnd:=W;
        end;
      end;
      if Status = stOk then begin
        W:=BufEnd - BufPtr;
        If Count < W then W:=Count;
        Move(Buffer^[BufPtr], P^, W);
        Dec(Count, W);
        Inc(BufPtr, W);
        P:=Pointer(LongInt(P) + W);
        Inc(Position, W);
      end;
    end;
  end;
  if (Status <> stOk) and (Count > 0) then FillChar(P^, Count, #0);
end;

procedure TBufStream.Seek(Pos: LongInt);
begin
  if (Status = stOk) and (Position <> Pos) then begin
    Flush;
    inherited Seek(Pos);
  end;
end;

procedure TBufStream.Truncate;
begin
  Flush;
  inherited Truncate;
end;

procedure TBufStream.Write(const Buf; Count: Sw_Word);
var
  Success: Integer;
  W: Sw_Word;
  P: PByteArray;
begin
  if Handle = -1 then Error(stWriteError, 103) else
  begin
    if LastMode = 1 then Flush;
    LastMode:=2;
    P:=@Buf;
    while (Count > 0) and (Status = stOk) do begin
      if BufPtr = BufSize then begin  { Buffer is full }
        Success:=AFileWrite(Handle, Buffer^, BufSize, W);
        if (Success <> 0) or (W <> BufSize) then Error(stError, Success);
        BufPtr:=0;
      end;
      if Status = stOk then begin
        W:=BufSize - BufPtr;
        if Count < W then W:=Count;
        Move(P^, Buffer^[BufPtr], W);
        Dec(Count, W);
        Inc(BufPtr, W);
        P:=Pointer(LongInt(P) + W);
        Inc(Position, W);
        If Position > StreamSize then StreamSize := Position;
      end;
    end;
  end;
end;

procedure TBufStream.Close;
begin
  Flush;
  Inherited Close;
end;

procedure TBufStream.DoOpen(OpenMode: Word);
begin
  if Status = stOk then begin
    BufPtr := 0;
    BufEnd := 0;
    Inherited DoOpen(OpenMode);
  end;
end;

{ TMemoryStream }

constructor TMemoryStream.Init(ALimit: LongInt; ABlockSize: Word);
var
  W: LongInt;
begin
  inherited Init;
  if ABlockSize = 0 then BlkSize:=8192 else BlkSize:=ABlockSize;
  if ALimit <= 0 then W:=1 else W:=(ALimit + BlkSize - 1) div BlkSize;
  if not ChangeListSize(W) then Error(stInitError, 0);
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

function TMemoryStream.ChangeListSize(ALimit: LongInt): Boolean;
var
  I, W: LongInt;
  Li: LongInt;
  P: PPointerArray;
begin
  if (ALimit <> BlkCount) then begin
    ChangeListSize:=False;
    if ALimit > MaxPtrs then
      Exit;
    if ALimit > 0 then begin
      Li:=ALimit * SizeOf(Pointer);
      if not LowMemory and (MaxAvail > Li) then begin
        GetMem(P, Li);
        FillChar(P^, Li, #0);
      end else
        Exit;
      if (BlkCount <> 0) and (BlkList <> nil) then
        if BlkCount <= ALimit then
          Move(BlkList^, P^, BlkCount * SizeOf(Pointer)) else
          Move(BlkList^, P^, Li);
    end else
    begin
      P:=nil;
      ALimit:=0;
    end;
    if ALimit < BlkCount then
      for W:=BlkCount-1 DownTo ALimit do FreeMem(BlkList^[W], BlkSize);
    if (P <> nil) and (ALimit > BlkCount) then begin
      for W:=BlkCount To ALimit-1 do begin
        if LowMemory or (MaxAvail < BlkSize) then
        begin
          for I:=BlkCount to W-1 do FreeMem(P^[I], BlkSize);
          FreeMem(P, Li);
          Exit;
        end else
          GetMem(P^[W], BlkSize);
      end;
    end;
    if (BlkCount <> 0) and (BlkList<>nil) then
      FreeMem(BlkList, BlkCount * SizeOf(Pointer));
    BlkList:=P;
    BlkCount:=ALimit;
    { * REMARK * - Do not shorten this, result can be > 64K }
    MemSize:=BlkCount;
    MemSize:=MemSize * BlkSize;
    { * REMARK END * - Leon de Boer }
  end;
  ChangeListSize:=True;
end;

procedure TMemoryStream.Read(var Buf; Count: Sw_Word);
var
  W, CurBlock, BlockPos: Word;
  Li: LongInt;
  P, Q: PByteArray;
begin
  P:=@Buf;
  if Position + Count > StreamSize then Error(stReadError, 0) else
  begin
    while (Count > 0) and (Status = stOk) do begin
      CurBlock:=Position div BlkSize;
      { * REMARK * - Do not shorten this, result can be > 64K }
      Li:=CurBlock;
      Li:=Li * BlkSize;
      { * REMARK END * - Leon de Boer }
      BlockPos:=Position - Li;
      W:=BlkSize - BlockPos;
      if W > Count then W:=Count;
      Q:=Pointer(LongInt(BlkList^[CurBlock]) + BlockPos);
      Move(Q^, P^, W);
      Inc(Position, W);
      P:=Pointer(LongInt(P) + W);
      Dec(Count, W);
    end;
  end;
  if Count <> 0 then FillChar(P^, Count, #0);
end;

procedure TMemoryStream.Truncate;
var
  W: Word;
begin
  if Status = stOk then begin
    if Position = 0 then
      W:=1 else
      W:=(Position + BlkSize - 1) div BlkSize;
    if ChangeListSize(W) then StreamSize:=Position
    else Error(stError, 0);
  end;
end;

procedure TMemoryStream.Write(const Buf; Count: Sw_Word);
var
  W, CurBlock, BlockPos: Word;
  Li: LongInt;
  P, Q: PByteArray;
begin
  if Position + Count > MemSize then begin
    if Position + Count = 0 then
      W:=1 else
      W:=(Position+Count+BlkSize-1) div BlkSize;
    if not ChangeListSize(W) then begin
      Error(stWriteError, 0); Exit;
    end;
  end;
  P:=@Buf;
  while (Count > 0) and (Status = stOk) do begin
    CurBlock:=Position div BlkSize;
    { * REMARK * - Do not shorten this, result can be > 64K }
    Li:=CurBlock;
    Li:=Li * BlkSize;
    { * REMARK END * - Leon de Boer }
    BlockPos:=Position - Li;
    W:=BlkSize - BlockPos;
    if W > Count then W:=Count;
    Q:=Pointer(LongInt(BlkList^[CurBlock]) + BlockPos);
    Move(P^, Q^, W);
    Inc(Position, W);
    P:=Pointer(LongInt(P) + W);
    Dec(Count, W);
    if Position > StreamSize then StreamSize:=Position;
  end;
end;

function GetAnyMemoryStream: PStream;
var
  S: PStream;
begin
  S:=nil;
  {$IFDEF OS_DOS}
  {$IFNDEF NOEXTRA}
  if (OPSYS=opDOS) then begin
    if (S=nil) and EMSFound then S:=New(PEMSStream,Init(0,0));
    if (S=nil) and XMSFound then S:=New(PXMSStream,Init(0,0));
  end else begin
    if (S=nil) and XMSFound then S:=New(PXMSStream,Init(0,0));
    if (S=nil) and EMSFound then S:=New(PEMSStream,Init(0,0));
  end;
  {$ENDIF NOEXTRA}
  {$ELSE}
  if S=nil then S:=New(PMemoryStream,Init(2048,2048));
  {$ELSE}
  if S=nil then S:=New(PMemoryStream,Init(MaxBytes,2048));
  {$ENDIF}
  if (S<>nil) and (S^.Status<>stOk) then FreeObject(S);
  GetAnyMemoryStream:=S;
end;

{ TRect }

procedure CheckEmpty (Var Rect: TRect);
begin
  with Rect do begin
    if (A.X >= B.X) or (A.Y >= B.Y) then begin
      A.X:=0;
      A.Y:=0;
      B.X:=0;
      B.Y:=0;
    end;
  end;
end;

function  TPoint.Equals(P: TPoint): Boolean;
begin
  Equals:=(X=P.X) and (Y=P.Y);
end;

function  TPoint.EqualsXY(AX, AY: LongInt): Boolean;
begin
  EqualsXY:=(X=AX) and (Y=AY);
end;

procedure TPoint.Assign(AX, AY: LongInt);
begin
  X:=AX; Y:=AY;
end;

function TPoint.isLE(P: TPoint): Boolean;
begin
  isLE:=False;
  if (Y=P.Y) and (X<=P.X) or (Y<P.Y) then isLE:=True;
end;

function TPoint.isGE(P: TPoint): Boolean;
begin
  isGE:=False;
  if (Y=P.Y) and (X>=P.X) or (Y>P.Y) then isGE:=True;
end;

procedure TRect.Assign(XA, YA, XB, YB: LongInt);
begin
  A.X:=XA;
  A.Y:=YA;
  B.X:=XB;
  B.Y:=YB;
end;

procedure TRect.Copy(R: TRect);
begin
  A:=R.A;
  B:=R.B;
end;

procedure TRect.Move(ADX, ADY: LongInt);
begin
  Inc(A.X, ADX);
  Inc(A.Y, ADY);
  Inc(B.X, ADX);
  Inc(B.Y, ADY);
end;

procedure TRect.Grow(ADX, ADY: LongInt);
begin
  Dec(A.X, ADX);
  Dec(A.Y, ADY);
  Inc(B.X, ADX);
  Inc(B.Y, ADY);
  CheckEmpty(Self);
end;

procedure TRect.Intersect(R: TRect);
begin
  if (R.A.X > A.X) then A.X:=R.A.X;
  if (R.A.Y > A.Y) then A.Y:=R.A.Y;
  if (R.B.X < B.X) then B.X:=R.B.X;
  if (R.B.Y < B.Y) then B.Y:=R.B.Y;
  CheckEmpty(Self);
end;

procedure TRect.Union(R: TRect);
begin
  if (R.A.X < A.X) then A.X:=R.A.X;
  if (R.A.Y < A.Y) then A.Y:=R.A.Y;
  if (R.B.X > B.X) then B.X:=R.B.X;
  if (R.B.Y > B.Y) then B.Y:=R.B.Y;
end;

function TRect.Contains(P: TPoint): Boolean;
begin
  Contains:=(P.X >= A.X) and (P.X < B.X) and
            (P.Y >= A.Y) and (P.Y < B.Y);
end;

function TRect.Equals (R: TRect): Boolean;
begin
  Equals:=(A.X = R.A.X) and (A.Y = R.A.Y) and
          (B.X = R.B.X) and (B.Y = R.B.Y);
end;

function TRect.Empty: Boolean;
begin
  Empty:=(A.X >= B.X) OR (A.Y >= B.Y);
end;

function LongMul(X, Y: LongInt): LongInt;
begin
  LongMul:=X*Y;
end;

function LongDiv(X: LongInt; Y: LongInt): LongInt;
begin
  LongDiv:=X DIV Y;
end;

procedure FreeObject(var O);
var
  OO: PObject absolute O;
  OP: Pointer absolute O;
begin
  if OP <> nil then begin Dispose(OO, Done); OP:=nil end;
end;

procedure Beep(Freq,Dur: LongInt); {JO}
begin
  SysBeepEx(Freq,Dur);
end;

end.
