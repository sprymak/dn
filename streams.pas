unit Streams;

{$I STDEFINE.INC}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - ��।���� �⥭��/������ ��ப � ��⮪� ��� ᮢ���⨬��� �
   ⨯�� AnsiString; ������� �⥭��/������ ������� ��ப (LongString) � ��⮪�
}

interface

uses
  VpSysLow, Defines, Objects2
  ;

const
  { TStream access modes }
  stCreate = $FFFF; { Create new file }
  stOpenRead = Open_Access_ReadOnly or open_share_DenyNone;
  { Read access only }
  stOpenWrite = Open_Access_WriteOnly or open_share_DenyNone;
  { Write access only }
  stOpen = Open_Access_ReadWrite or open_share_DenyNone;
  { Read and write access }
  stOpenPacked = Open_Access_ReadWrite+1;
  { Read access only, packed files too }

  { File share mode constants }
  fmClean = $FF00;
  { Mask to clean low byte of file mode constatns }

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
  PStreamRec = ^TStreamRec;
  TStreamRec = record
    ObjType: word;
    VmtLink: Pointer;
    Load: Pointer;
    Store: Pointer;
    Next: PStreamRec;
    end;

  PStream = ^TStream;
  TStream = object(TObject)
    {Cat: ��� ��ꥪ� �뭥ᥭ � ��������� ������; �������� �ࠩ�� ���஦��!}
    Status: integer;
    ErrorInfo: integer;
    StreamSize: LongInt; { Stream current size }
    Position: LongInt; { Current position }
    procedure CopyFrom(var S: TStream; Count: LongInt);
    procedure Error(Code, Info: integer); virtual;
    procedure Flush; virtual;
    function Get: PObject;
    function GetPos: LongInt; virtual;
    function GetSize: LongInt; virtual;
    procedure Put(P: PObject);
    procedure Read(var Buf; Count: SW_Word); virtual;
    function ReadStr: PString;
    function ReadLongStr: PLongString;
    procedure ReadStrV(var S: String);
    procedure ReadLongStrV(var S: LongString);
    procedure Reset;
    procedure Seek(Pos: LongInt); virtual;
    function StrRead: PChar;
    procedure StrWrite(P: PChar);
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure WriteStr(P: PString);
    procedure WriteLongStr(P: PLongString);
    function Eof: Boolean;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  PDosStream = ^TDosStream;
  TDOSStream = object(TStream)
    {Cat: ��� ��ꥪ� �뭥ᥭ � ��������� ������; �������� �ࠩ�� ���஦��!}
    Handle: integer;
    FName: AsciiZ;
    FMode: word;
    constructor Init(FileName: FNameStr; Mode: word);
    procedure Open(FileName: FNameStr; Mode: word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure ReadBlock(var Buf; Count: SW_Word; var BytesRead: word);
     virtual;
    procedure Seek(Pos: LongInt); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  PBufStream = ^TBufStream;
  TBufStream = object(TDOSStream)
    {Cat: ��� ��ꥪ� �뭥ᥭ � ��������� ������; �������� �ࠩ�� ���஦��!}
    Buffer: PByteArray;
    BufSize: SW_Word;
    BufPtr: SW_Word; { Buffer start }
    BufEnd: SW_Word;
    LastMode: Byte; { Last buffer mode }
    constructor Init(FileName: FNameStr; Mode: word; Size: SW_Word);
    destructor Done; virtual;
    procedure Flush; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Seek(Pos: LongInt); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
    procedure DoOpen(OpenMode: word); virtual;
    procedure Close; virtual;
    end;

  PMemoryStream = ^TMemoryStream;
  TMemoryStream = object(TStream)
    {Cat: ��� ��ꥪ� �뭥ᥭ � ��������� ������; �������� �ࠩ�� ���஦��!}
    BlkCount: LongInt; { Number of segments }
    BlkSize: word; { Memory block size }
    MemSize: LongInt; { Memory alloc size }
    BlkList: PPointerArray; { Memory block list }
    constructor Init(ALimit: LongInt; ABlockSize: word);
    destructor Done; virtual;
    procedure Read(var Buf; Count: SW_Word); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: SW_Word); virtual;
  private
    function ChangeListSize(ALimit: LongInt): Boolean;
    end;

procedure RegisterType(var S: TStreamRec);
procedure ReRegisterType(var S: TStreamRec);

function GetAnyMemoryStream: PStream;

const
  StreamError: Pointer = nil;

implementation

uses
  {$IFDEF PLUGIN}Plugin, {$ENDIF}
  Strings, Memory
  ;

const
  StreamTypes: PStreamRec = nil;

procedure RegisterError;
  begin
  end;

procedure RegisterType(var S: TStreamRec);
  const
    SRegisterError = 'RegisterError, ObjType=';
  var
    P: PStreamRec;
  begin
  if S.ObjType = 0 then
    begin
    Writeln(SRegisterError, S.ObjType);
    RegisterError;
    end;
  P := StreamTypes;
  while (P <> nil) and (P^.ObjType <> S.ObjType) do
    P := P^.Next;
  if P = nil then
    begin
    S.Next := StreamTypes;
    StreamTypes := @S;
    end
  else if (P^.VmtLink <> S.VmtLink) or (P^.Load <> S.Load)
       or (P^.Store <> S.Store)
  then
    begin
    Writeln(SRegisterError, S.ObjType);
    RegisterError;
    end;
  end { RegisterType };

procedure ReRegisterType(var S: TStreamRec);
  var
    P, L: PStreamRec;
  begin
  if S.ObjType = 0 then
    RegisterError;
  P := StreamTypes;
  L := nil;
  while (P <> nil) and (P^.ObjType <> S.ObjType) do
    begin
    L := P;
    P := P^.Next;
    end;
  if P <> nil then
    if L <> nil then
      L^.Next := P^.Next
    else
      StreamTypes := P^.Next;
  S.Next := StreamTypes;
  StreamTypes := @S;
  end;

const
  TStream_Error = vmtHeaderSize+$04;
  TStream_Flush = vmtHeaderSize+$08;
  TStream_Read = vmtHeaderSize+$14;
  TStream_Write = vmtHeaderSize+$20;

  StreamMagic = $590C5CF1;

  { Stream error handler                                  }
  { In    eax   = Error info                              }
  {       dl    = Error code                              }
  {       ecx   = Stream object pointer                   }
  { Uses  eax,edx                                         }

procedure DoStreamError;
  assembler; {$USES ecx}
  {$FRAME-}
asm
  movsx   edx,dl
  push    edx             { [1]:Integer = Code    }
  push    eax             { [2]:Integer = Info    }
  push    ecx             { [3]:Pointer = Self    }
  mov     eax,[ecx]
  call    DWord Ptr [eax].TStream_Error
end;

procedure TStream.CopyFrom(var S: TStream; Count: LongInt);
  var
    N: word;
    Buffer: PByteArray;
    BufSize: word;
    Allocated: Boolean;
    TTempBuf: array[0..255] of Byte;
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
      N := BufSize
    else
      N := Count;
    S.Read(Buffer^, N);
    Write(Buffer^, N);
    Dec(Count, N);
    end;
  if Allocated then
    FreeMem(Buffer, BufSize);
  end { TStream.CopyFrom };

procedure TStream.Error(Code, Info: integer);
  type
    TErrorProc = procedure (var S: TStream);
  begin
  Status := Code;
  ErrorInfo := Info;
  if StreamError <> nil then
    TErrorProc(StreamError)(Self);
  end;

procedure TStream.Flush;
  begin
  end;

function TStream.Get: PObject;
  assembler; {$USES None}
  {$FRAME+}
asm
{Cat: ������� �஢��� StreamMagic}
  push    eax
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf   }
  push    4                       { [2]:DWord   = Count }
  mov     eax,Self
  push    eax                     { [3]:Pointer = Self  }
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Read
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
  call    DWord Ptr [eax].TStream_Read
  pop     eax
  test    eax,eax                 { Return nil }
  jz      @@4
{Cat: �᫨ ��⠥��� ������ ��ꥪ�, ॣ�����㥬� ��������, � ����
      ᭠砫� �������� ��� ������, �⮡� �� �஢� ॣ������}
  {$IFDEF PLUGIN}
  push    eax
  push    eax
  call    PluginRegisterObject
  pop     eax
  {$ENDIF}
{/Cat}
  mov     edx,StreamTypes
  jmp     @@2
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
  call    DoStreamError
  xor     eax,eax                 { Return nil }
  jmp     @@4
  @@3:
  push    Self
  { [1]:Pointer = TStream }
  push    [edx].TStreamRec.VmtLink
  { [2]:DWord   = VMT     }
  push    0
  { [3]:Pointer = Self = nil: allocate in dynamic memory }
  call    [edx].TStreamRec.Load
  @@4:
  { Return Self or nil }
end;

function TStream.GetPos: LongInt;
  begin
  if  (Status = stOK) then
    GetPos := Position
  else
    GetPos := -1;
  end;

function TStream.GetSize: LongInt;
  begin
  if  (Status = stOK) then
    GetSize := StreamSize
  else
    GetSize := -1;
  end;

procedure TStream.Put(P: PObject);
  assembler; {$USES None}
  {$FRAME+}
asm
{Cat: ������� ������ StreamMagic}
  push    StreamMagic
  mov     eax,esp
  push    eax                     { [1]:Pointer = Buf  }
  push    4                       { [2]:DWord   = Size }
  mov     eax,Self                { [3]:Pointer = Self }
  push    eax
  mov     eax,[eax]
  call    DWord Ptr [eax].TStream_Write
  pop     ecx
{/Cat}
  mov     ecx,P
  jecxz   @@4
  mov     eax,[ecx]               { VMT pointer }
  mov     edx,StreamTypes
  jmp     @@2
  @@1:
  cmp     eax,[edx].TStreamRec.VmtLink
  je      @@3
  mov     edx,[edx].TStreamRec.Next
  @@2:
  test    edx,edx
  jne     @@1
  mov     ecx,Self
  mov     dl,stPutError
  call    DoStreamError
  jmp     @@5
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
  call    DWord Ptr [eax].TStream_Write
  pop     ecx
  pop     edx
  jecxz   @@5
  push    Self
  { [1]:Pointer = TStream }
  push    P
  { [2]:Pointer = Self    }
  call    [edx].TStreamRec.Store
  @@5:
end;

procedure TStream.Read(var Buf; Count: SW_Word);
  begin
  end;

{Cat}
function TStream.ReadStr: PString;
  var
    L: LongInt;
    P: PString;
  begin
  L := 0;
  Read(L, 1);
  if L > 0 then
    begin
    GetMem(P, L+1);
    SetLength(P^, L);
    Read(P^[1], L);
    ReadStr := P;
    end
  else
    ReadStr := nil;
  end;

function TStream.ReadLongStr: PLongString;
  var
    L: LongInt;
    P: PLongString;
  begin
  Read(L, SizeOf(L));
  if L > 0 then
    if L > MaxLongStringLength then
      begin
      New(P);
      SetLength(P^, MaxLongStringLength);
      Read(P^[1], MaxLongStringLength);
      Seek(Position-MaxLongStringLength+L);
      ReadLongStr := P;
      end
    else
      begin
      New(P);
      SetLength(P^, L);
      Read(P^[1], L);
      ReadLongStr := P;
      end
  else
    ReadLongStr := nil;
  end { TStream.ReadLongStr: };

procedure TStream.ReadStrV(var S: String);
  var
    L: LongInt;
  begin
  L := 0;
  Read(L, 1);
  if L > 0 then
    begin
    SetLength(S, L);
    Read(S[1], L);
    end
  else
    S := '';
  end;

procedure TStream.ReadLongStrV(var S: LongString);
  var
    L: LongInt;
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
  Status := 0;
  ErrorInfo := 0;
  end;

procedure TStream.Seek(Pos: LongInt);
  begin
  if Status = stOK then
    if Pos < 0 then
      Pos := 0
    else if Pos <= StreamSize then
      Position := Pos
    else
      Error(stSeekError, Pos);
  end;

function TStream.StrRead: PChar;
  var
    L: word;
    P: PChar;
  begin
  Read(L, SizeOf(L));
  if L = 0 then
    StrRead := nil
  else
    begin
    GetMem(P, L+1);
    Read(P[0], L);
    P[L] := #0;
    StrRead := P;
    end;
  end;

procedure TStream.StrWrite(P: PChar);
  var
    L: word;
  begin
  if P = nil then
    L := 0
  else
    L := StrLen(P);
  Write(L, SizeOf(L));
  if P <> nil then
    Write(P[0], L);
  end;

procedure TStream.Truncate;
  begin
  end;

procedure TStream.Write(const Buf; Count: SW_Word);
  begin
  end;

{Cat}
procedure TStream.WriteStr(P: PString);
  var
    L: LongInt;
  begin
  if P <> nil then
    Write(P^[0], Length(P^)+1)
  else
    begin
    L := 0;
    Write(L, 1);
    end;
  end;

procedure TStream.WriteLongStr(P: PLongString);
  var
    L: LongInt;
  begin
  if P <> nil then
    begin
    L := Length(P^);
    Write(L, 4);
    Write(P^[1], L);
    end
  else
    begin
    L := 0;
    Write(L, SizeOf(L));
    end
  end;
{/Cat}

function TStream.Eof: Boolean;
  begin
  Eof := (GetPos >= GetSize);
  end;

procedure TStream.DoOpen(OpenMode: word);
  begin
  end;

procedure TStream.Close;
  begin
  end;

function AFileClose(Handle: word): Boolean;
  begin
  AFileClose := (SysFileClose(Handle) = 0);
  end;

function AFileOpen(var FileName: AsciiZ; Mode: word; var Handle: word)
  : word;
  begin
  if Mode = stCreate then
    AFileOpen := SysFileCreate(@FileName, stOpen, $20 {Archive}, Handle)
  else
    AFileOpen := SysFileOpen(@FileName, Mode, Handle);
  end;

function AFileRead(Handle: word; var Buf; Count: SW_Word;
     var Actual: SW_Word): word;
  begin
  Actual := 0;
  AFileRead := SysFileRead(Handle, Buf, Count, Actual);
  end;

function AFileWrite(Handle: word; var Buf; Count: SW_Word;
     var Actual: SW_Word): word;
  begin
  Actual := 0;
  AFileWrite := SysFileWrite(Handle, Buf, Count, Actual);
  end;

function ASetFilePos(Handle: word; Pos: SW_Word; MoveType: word;
     var Actual: SW_Word): word;
  begin
  Actual := 0;
  ASetFilePos := SysFileSeek(Handle, Pos, MoveType, Actual);
  end;

{AK155}
function ASetFileSize(Handle: word; FileSize: SW_Word): word;
  begin
  ASetFileSize := 0;
  if  (ASetFilePos(Handle, FileSize, 0, FileSize) <> 0) or
      (SysFileSetSize(Handle, FileSize) <> 0)
  then
    ASetFileSize := 1
  else
    ASetFileSize := 0;
  end;
{/AK155}

constructor TDOSStream.Init(FileName: FNameStr; Mode: word);
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
  if  (FMode and $000F) = fmPacked then
    begin
    Handle := pOpen(@FName, FMode and $FFF0);
    Success := 0;
    if Handle <> 0 then
      StreamSize := pFileSize(Handle)
    else
      begin
      FMode := FMode and $FFF0;
      goto 1;
      end;
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
  if  (Handle = 0) or (Success <> 0) then
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
  if  (Handle <> -1) then
    begin
    {$IFDEF PACKFILE}
    if  (FMode and $000F) = fmPacked then
      pClose(Handle)
    else
      {$ENDIF}
      AFileClose(Handle);
    end;
  FileName := FileName+#0;
  Move(FileName[1], FName, Length(FileName));
  FMode := Mode;
  {$IFDEF PACKFILE}
  if  (FMode and $000F) = fmPacked then
    begin
    Handle := pOpen(@FName, FMode and $FFF0);
    Success := 0;
    if Handle <> 0 then
      StreamSize := pFileSize(Handle)
    else
      begin
      FMode := FMode and $FFF0;
      goto 1;
      end;
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
  if  (Handle = 0) or (Success <> 0) then
    begin
    if Handle = 0 then
      Handle := -1;
    Error(stInitError, Success);
    end;
  end { TDOSStream.Open };

destructor TDOSStream.Done;
  begin
  if  (Handle <> -1) then
    begin
    {$IFDEF PACKFILE}
    if  (FMode and $000F) = fmPacked then
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
      if  (FMode and $000F) = fmPacked then
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
        P := Pointer(LongInt(P)+BytesMoved);
        Dec(Count, BytesMoved);
        end;
      if  (Success <> 0) or (BytesMoved <> W) then
        begin
        Error(stReadError, Success);
        break;
        end;
      end;
    end;
  if Count <> 0 then
    FillChar(P^, Count, #0); { Error clear buffer }
  end { TDOSStream.Read };

procedure TDOSStream.ReadBlock(var Buf; Count: SW_Word;
     var BytesRead: word);
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
  if  (Position+Count > StreamSize) or (Status <> 0) then
    Success := 0;
  if Success <> -1 then
    Error(stReadError, Success)
  else
    begin
    while (Count > 0) and (Status = stOK) do
      begin
      W := Count;
      {$IFDEF PACKFILE}
      if  (FMode and $000F) = fmPacked then
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
        P := Pointer(LongInt(P)+BytesMoved);
        Dec(Count, BytesMoved);
        Inc(BytesRead, BytesMoved);
        end;
      if  (Success <> 0) then
        begin
        Error(stReadError, Success);
        break;
        end;
      if BytesMoved <> W then
        break;
      end;
    end;
  end { TDOSStream.ReadBlock };

procedure TDOSStream.Seek(Pos: LongInt);
  var
    Success: integer;
    Li: LongInt;
  begin
  if Status = stOK then
    begin
    if Pos < 0 then
      Pos := 0;
    if Handle = -1 then
      Success := 03
    else
      {$IFDEF PACKFILE}
     if (FMode and $000F) = fmPacked then
      begin
      Li := pSeek(Handle, Pos);
      Success := 0;
      if IOResult <> 0 then
        Success := -1;
      end
    else
      {$ENDIF}
      Success := ASetFilePos(Handle, Pos, 0, Li);
    if  (Success = -1) or (Li <> Pos) then
      begin
      if  (Success = -1) then
        Error(stSeekError, 0)
      else
        Error(stSeekError, Success);
      end
    else
      Position := Li;
    end;
  end { TDOSStream.Seek };

procedure TDOSStream.Truncate;
  var
    Success: integer;
  begin
  if Status = stOK then
    begin
    if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or {$ENDIF}
        ( (FMode and $000F) = fmReadOnly)
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
  if Handle = -1 then
    Error(stWriteError, 103)
  else
    begin
    if {$IFDEF PACKFILE}((FMode and $000F) = fmPacked) or {$ENDIF}
        ( (FMode and $000F) = fmReadOnly)
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
          P := Pointer(LongInt(P)+BytesMoved);
          Dec(Count, BytesMoved);
          if  (Position > StreamSize) then
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

{Cat:warn �ࠢ��쭮 �� �� ࠡ�⠥�?}
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
      if  (FMode and $000F) = fmPacked then
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
    if  (FMode and $000F) = fmPacked then
      pClose(Handle)
    else
      {$ENDIF}
      AFileClose(Handle);
    end;
  Position := 0;
  Handle := -1;
  end;

constructor TBufStream.Init(FileName: FNameStr; Mode: word; Size: SW_Word);
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
  if  (LastMode = 2) and (BufPtr <> 0) then
    begin
    if Handle = -1 then
      Error(stError, 103)
    else
      {$IFDEF PACKFILE}
     if (FMode and $000F) = fmPacked then
      Error(stError, 103)
    else
      {$ENDIF}
      begin
      Success := AFileWrite(Handle, Buffer^, BufPtr, W);
      if  (Success <> 0) or (W <> BufPtr) then
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
        begin { Buffer is empty }
        if Position+BufSize > StreamSize then
          Bw := StreamSize-Position
        else
          Bw := BufSize;
        {$IFDEF PACKFILE}
        if  (FMode and $000F) = fmPacked then
          begin
          W := pRead(Handle, Bw, Buffer);
          Success := IOResult;
          end
        else
          {$ENDIF}
          Success := AFileRead(Handle, Buffer^, Bw, W);
        if  (Success <> 0) or (Bw <> W) then
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
        P := Pointer(LongInt(P)+W);
        Inc(Position, W);
        end;
      end;
    end;
  if  (Status <> stOK) and (Count > 0) then
    FillChar(P^, Count, #0);
  end { TBufStream.Read };

procedure TBufStream.Seek(Pos: LongInt);
  begin
  if  (Status = stOK) and (Position <> Pos) then
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
        begin { Buffer is full }
        Success := AFileWrite(Handle, Buffer^, BufSize, W);
        if  (Success <> 0) or (W <> BufSize) then
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
        P := Pointer(LongInt(P)+W);
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

constructor TMemoryStream.Init(ALimit: LongInt; ABlockSize: word);
  var
    W: LongInt;
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
   �� ࠡ�� � Clipboard StreamSize �ᯮ������ � ����⢥ 㪧�⥫�
���� ��⮪�, ���⮬� � ᢥ��ᮧ������� ��⮪� �� ������ ���� ࠢ�� ���.
� �� ��᢮���� ᮧ���� ���������� �⥭�� ���� �� �।����� 䠪��᪨
����ᠭ���� ��⮪�. ���ਬ��, �᫨ � dn.ini 㪠���� MaxClipboardSize=0,
� DN ���� ��࠭�஢��� ��᭥� �� ��ࢮ� �� ���⨨ � ���� � ।����.
   �� � FileCopy GetSize (� ���� StreamSize) �ᯮ������ � ����⢥
ࠧ��� ��⮪� (�� ������⢥���), ���⮬� ⠬ ⮦� ���� ᮮ⢥�������
���४��. ��㣨� ����, ��� �ᯮ������ StreamSize ��� TMemoryStream,
� �� ��襫.}
*)
  end { TMemoryStream.Init };

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
  if  (ALimit <> BlkCount) then
    begin
    ChangeListSize := False;
    if ALimit > MaxPtrs then
      exit;
    if ALimit > 0 then
      begin
      Li := ALimit*SizeOf(Pointer);
      if not LowMemory and (MaxAvail > Li) then
        begin
        GetMem(P, Li);
        FillChar(P^, Li, #0);
        end
      else
        exit;
      if  (BlkCount <> 0) and (BlkList <> nil) then
        if BlkCount <= ALimit then
          Move(BlkList^, P^, BlkCount*SizeOf(Pointer))
        else
          Move(BlkList^, P^, Li);
      end
    else
      begin
      P := nil;
      ALimit := 0;
      end;
    if ALimit < BlkCount then
      for W := BlkCount-1 DownTo ALimit do
        FreeMem(BlkList^[W], BlkSize);
    if  (P <> nil) and (ALimit > BlkCount) then
      begin
      for W := BlkCount to ALimit-1 do
        begin
        if LowMemory or (MaxAvail < BlkSize) then
          begin
          for I := BlkCount to W-1 do
            FreeMem(P^[I], BlkSize);
          FreeMem(P, Li);
          exit;
          end
        else
          GetMem(P^[W], BlkSize);
        end;
      end;
    if  (BlkCount <> 0) and (BlkList <> nil) then
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
    Li: LongInt;
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
      Li := CurBlock;
      Li := Li*BlkSize;
      { * REMARK END * - Leon de Boer }
      BlockPos := Position-Li;
      W := BlkSize-BlockPos;
      if W > Count then
        W := Count;
      Q := Pointer(LongInt(BlkList^[CurBlock])+BlockPos);
      Move(Q^, P^, W);
      Inc(Position, W);
      P := Pointer(LongInt(P)+W);
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
    Li: LongInt;
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
    Li := CurBlock;
    Li := Li*BlkSize;
    { * REMARK END * - Leon de Boer }
    BlockPos := Position-Li;
    W := BlkSize-BlockPos;
    if W > Count then
      W := Count;
    Q := Pointer(LongInt(BlkList^[CurBlock])+BlockPos);
    Move(P^, Q^, W);
    Inc(Position, W);
    P := Pointer(LongInt(P)+W);
    Dec(Count, W);
    if Position > StreamSize then
      StreamSize := Position;
    end;
  end { TMemoryStream.Write };

{Cat:warn �㦭� �� �� �㭪��?}
function GetAnyMemoryStream: PStream;
  var
    S: PStream;
  begin
  S := New(PMemoryStream, Init(MaxBytes, 2048));
  if  (S <> nil) and (S^.Status <> stOK) then
    FreeObject(S);
  GetAnyMemoryStream := S;
  end;

end.