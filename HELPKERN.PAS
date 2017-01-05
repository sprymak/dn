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

unit
HelpKern;

{ Used by TVHC and HELPFILE }

interface

uses
  Objects;

type

  THelpParameters = record
    Index: AWord;
    CursorXY,
    DeltaXY: TPoint;
    end;
  { TParagraph }

  PParagraph = ^TParagraph;
  TParagraph = record
    Next: PParagraph;
    Wrap: boolean;
    Size: AWord;
    text: record
      end;
    end;

  { THelpTopic }

  TCrossRef = record
    Ref: AWord;
    Offset: AInt;
    Length: byte;
    end;

  PCrossRefs = ^TCrossRefs;
  TCrossRefs = array[1..10000] of TCrossRef;
  TCrossRefHandler = procedure (var s: TStream; XRefValue: AInt);

  PHelpTopic = ^THelpTopic;
  THelpTopic = object(TObject)
    Constructor Init;
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    {DataCompBoy
    procedure AddCrossRef(Ref: TCrossRef);
}
    procedure AddParagraph(P: PParagraph);
    procedure GetCrossRef(i: AInt; var Loc: TPoint; var Length: byte;
    var Ref: AWord);
    function GetLine(Line: AInt): String;
    function GetNumCrossRefs: AInt;
    function NumLines: AInt;
    procedure SetCrossRef(i: AInt; var Ref: TCrossRef);
    procedure SetNumCrossRefs(i: AInt);
    procedure SetWidth(AWidth: AInt);
    procedure Store(var s: TStream);
    private
    Paragraphs: PParagraph;
    NumRefs: AInt;
    CrossRefs: PCrossRefs;
    Width: AInt;
    LastOffset: AInt;
    LastLine: AInt;
    LastParagraph: PParagraph;
    function WrapText(var text; Size: integer; var Offset: integer;
    Wrap: boolean): String;
    end;

  { THelpIndex }

  PIndexArray = ^TIndexArray;
  TIndexArray = array[0..16380] of longInt;

  PContextArray = ^TContextArray;
  TContextArray = array[0..16380] of AWord;

  PHelpIndex = ^THelpIndex;
  THelpIndex = object(TObject)
    Constructor Init;
    Constructor Load(var s: TStream);
    destructor Done; virtual;
    function Position(i: AWord): longInt;
    procedure Add(i: AWord; Val: longInt);
    procedure Store(var s: TStream);
    private
    Size: AWord;
    Used: AWord;
    Contexts: PContextArray;
    Index: PIndexArray;
    function Find(i: AWord): AWord;
    end;

  { THelpFile }

  PHelpFile = ^THelpFile;
  THelpFile = object(TObject)
    Stream: PStream;
    Modified: boolean;
    Constructor Init(s: PStream);
    destructor Done; virtual;
    function GetTopic(i: AWord): PHelpTopic;
    function InvalidTopic: PHelpTopic;
    procedure RecordPositionInIndex(i: AInt);
    procedure PutTopic(Topic: PHelpTopic);
    private
    Index: PHelpIndex;
    IndexPos: longInt;
    end;

procedure NotAssigned(var s: TStream; Value: AInt);

const
  CrossRefHandler: TCrossRefHandler = NotAssigned;

implementation

{ THelpTopic }

Constructor THelpTopic.Init;
  begin
    inherited Init;
    LastLine := High(LastLine);
  end;

Constructor THelpTopic.Load(var s: TStream);

  procedure ReadParagraphs;
    var
      i, Size: AInt;
      PP: ^PParagraph;
    begin
      s.Read(i, SizeOf(i));
      PP := @Paragraphs;
      while i > 0 do
        begin
          s.Read(Size, SizeOf(Size));
          GetMem(PP^, SizeOf(PP^^)+Size);
          PP^^.Size := Size;
          s.Read(PP^^.Wrap, SizeOf(boolean));
          s.Read(PP^^.text, Size);
          PP := @PP^^.Next;
          Dec(i);
        end;
      PP^:= nil;
    end;

  procedure ReadCrossRefs;
    begin
      s.Read(NumRefs, SizeOf(AInt));
      GetMem(CrossRefs, SizeOf(TCrossRef)*NumRefs);
      s.Read(CrossRefs^, SizeOf(TCrossRef)*NumRefs);
    end;

  begin { THelpTopic.Load }
    ReadParagraphs;
    ReadCrossRefs;
    Width := 0;
    LastLine := High(LastLine);
  end { THelpTopic.Load };

destructor THelpTopic.Done;

  procedure DisposeParagraphs;
    var
      P, t: PParagraph;
    begin
      P := Paragraphs;
      while P <> nil do
        begin
          t := P;
          P := P^.Next;
          FreeMem(t, SizeOf(t^)+t^.Size);
        end;
    end;

  begin
    DisposeParagraphs;
    FreeMem(CrossRefs, SizeOf(TCrossRef)*NumRefs);
    inherited Done
  end;
{DataCompBoy
procedure THelpTopic.AddCrossRef(Ref: TCrossRef);
var
  P: PCrossRefs;
begin
  GetMem(P, (NumRefs+1) * SizeOf(TCrossRef));
  if NumRefs > 0 then
  begin
    Move(CrossRefs^, P^, NumRefs * SizeOf(TCrossRef));
    FreeMem(CrossRefs, NumRefs * SizeOf(TCrossRef));
  end;
  CrossRefs^[NumRefs] := Ref;
  Inc(NumRefs);
end;
}
procedure THelpTopic.AddParagraph(P: PParagraph);
  var
    PP: ^PParagraph;
  begin
    PP := @Paragraphs;
    while PP^ <> nil do
      PP := @PP^^.Next;
    PP^:= P;
    P^.Next := nil;
  end;

procedure THelpTopic.GetCrossRef(i: AInt; var Loc: TPoint;
  var Length: byte; var Ref: AWord);
  var
    OldOffset, CurOffset, Offset, ParaOffset: integer;
    P: PParagraph;
    Line: AInt;
  begin
    ParaOffset := 0;
    CurOffset := 0;
    OldOffset := 0;
    Line := 0;
    Offset := CrossRefs^[i].Offset;
    P := Paragraphs;
    while ParaOffset+CurOffset < Offset do
      begin
        OldOffset := ParaOffset+CurOffset;
        WrapText(P^.text, P^.Size, CurOffset, P^.Wrap);
        Inc(Line);
        if CurOffset >= P^.Size then
          begin
            Inc(ParaOffset, P^.Size);
            P := P^.Next;
            CurOffset := 0;
          end;
      end;
    Loc.X := Offset-OldOffset-1;
    Loc.Y := Line;
    Length := CrossRefs^[i].Length;
    Ref := CrossRefs^[i].Ref;
  end { THelpTopic.GetCrossRef };

function THelpTopic.GetLine(Line: AInt): String;
  var
    Offset: integer;
    i: AInt;
    P: PParagraph;
  begin
    if LastLine < Line then
      begin
        i := Line;
        Dec(Line, LastLine);
        LastLine := i;
        Offset := LastOffset;
        P := LastParagraph;
      end
    else
      begin
        P := Paragraphs;
        Offset := 0;
        LastLine := Line;
      end;
    GetLine := '';
    while (P <> nil) do
      begin
        while Offset < P^.Size do
          begin
            Dec(Line);
            GetLine := WrapText(P^.text, P^.Size, Offset, P^.Wrap);
            if Line = 0 then
              begin
                LastOffset := Offset;
                LastParagraph := P;
                exit;
              end;
          end;
        P := P^.Next;
        Offset := 0;
      end;
    GetLine := '';
  end { THelpTopic.GetLine };

function THelpTopic.GetNumCrossRefs: AInt;
  begin
    GetNumCrossRefs := NumRefs;
  end;

function THelpTopic.NumLines: AInt;
  var
    Offset: integer;
    Lines: AInt;
    P: PParagraph;
  begin
    Offset := 0;
    Lines := 0;
    P := Paragraphs;
    while P <> nil do
      begin
        Offset := 0;
        while Offset < P^.Size do
          begin
            Inc(Lines);
            WrapText(P^.text, P^.Size, Offset, P^.Wrap);
          end;
        P := P^.Next;
      end;
    NumLines := Lines;
  end { THelpTopic.NumLines: };

procedure THelpTopic.SetCrossRef(i: AInt; var Ref: TCrossRef);
  begin
    if i <= NumRefs then
      CrossRefs^[i] := Ref;
  end;

procedure THelpTopic.SetNumCrossRefs(i: AInt);
  var
    P: PCrossRefs;
  begin
    if NumRefs = i then
      exit;
    GetMem(P, i*SizeOf(TCrossRef));
    if NumRefs > 0 then
      begin
        if i > NumRefs then
          Move(CrossRefs^, P^, NumRefs*SizeOf(TCrossRef))
        else
          Move(CrossRefs^, P^, i*SizeOf(TCrossRef));
        FreeMem(CrossRefs, NumRefs*SizeOf(TCrossRef));
      end;
    CrossRefs := P;
    NumRefs := i;
  end;

procedure THelpTopic.SetWidth(AWidth: AInt);
  begin
    Width := AWidth;
  end;

procedure THelpTopic.Store(var s: TStream);

  procedure WriteParagraphs;
    var
      i: AInt;
      P: PParagraph;
    begin
      P := Paragraphs;
      i := 0;
      while P <> nil do
        begin
          Inc(i);
          P := P^.Next;
        end;
      s.Write(i, SizeOf(i));
      P := Paragraphs;
      while P <> nil do
        begin
          s.Write(P^.Size, SizeOf(P^.Size));
          s.Write(P^.Wrap, SizeOf(P^.Wrap));
          s.Write(P^.text, P^.Size);
          P := P^.Next;
        end;
    end { WriteParagraphs };

  procedure WriteCrossRefs;
    var
      i: AInt;
    begin
      s.Write(NumRefs, SizeOf(NumRefs));
      if @CrossRefHandler = @NotAssigned then
        s.Write(CrossRefs^, SizeOf(TCrossRef)*NumRefs)
      else
        for i := 1 to NumRefs do
          begin
            CrossRefHandler(s, CrossRefs^[i].Ref);
            with CrossRefs^[i] do
              s.Write(Offset, SizeOf(Offset)+SizeOf(Length));
          end;
    end;

  begin { THelpTopic.Store }
    WriteParagraphs;
    WriteCrossRefs;
  end { THelpTopic.Store };
(*
function THelpTopic.WrapText(var Text; Size: AInt;
  var Offset: AInt; Wrap: Boolean): String;
type
  PCArray = ^CArray;
  CArray = array[0..32767] of Char;
var
  Line: String;
  I, P: AInt;

function IsBlank(Ch: Char): Boolean;
begin
  IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
end;

function Scan(var P; Size: AInt; Offs: AInt; C: Char): AInt; assembler;
{$IFNDEF BIT_32}
asm
        CLD
        LES     DI,P
        ADD     DI,Offs
        MOV     CX,Size
        MOV     AL, C
        REPNE   SCASB
        SUB     CX,Size
        NEG     CX
        XCHG    AX,CX
end;
{$ELSE} {$USES esi,edi,ecx} {&FRAME-}

asm
        cld
        xor     eax,eax
        mov     edi,P
        mov      ax,Offs
        add     edi,eax
        mov      ax,Size
        mov     edx,eax
        sub     edx,&Offset
        test    dh,dh
        jz      @@1
        mov     edx,256
      @@1:
        mov     ecx,edx
        mov     al,C
        repne   scasb
        sub     ecx,edx
        neg     ecx
        xchg    eax,ecx
end;
{$ENDIF}

procedure TextToLine(var Text; Offs, Length: AInt; var Line: String);
  assembler;
{$IFNDEF BIT_32}
asm
        CLD
        PUSH    DS
        LDS     SI,Text
        ADD     SI,Offs
        LES     DI,Line
        MOV     AX,Length
        STOSB
        XCHG    AX,CX
        REP     MOVSB
        POP     DS
end;
{$ELSE} {$USES esi,edi,ecx} {&FRAME-}
asm
        cld
        xor     eax,eax
        mov     esi,Text
        mov      ax,Offs
        add     esi,eax
        mov     edi,Line
        mov      ax,Length
        stosb
        xchg    eax,ecx
        rep     movsb
end;
{$ENDIF}

begin
  I := Scan(Text, Size-Offset, Offset, #13);
  if I + Offset > Size then I := Size - Offset;
  if (I >= Width) and Wrap then
  begin
    I := Offset + Width;
    if I > Size then I := Size
    else
    begin
      while (I > Offset) and not IsBlank(PCArray(@Text)^[I]) do Dec(I);
      if I = Offset then I := Offset + Width
      else Inc(I);
    end;
    if I = Offset then I := Offset + Width;
    Dec(I, Offset);
  end;
  TextToLine(Text, Offset, I, Line);
  if Line[Length(Line)] = #13 then SetLength(Line, Length(Line)-1);
  Inc(Offset, I);
  WrapText := Line;
end;
*)

{JO: взял THelpTopic.WrapText из VP 2.1 TVDEMO, так как в старой падала      }
{    регулярно ассемблерная функция Scan на repne   scasb ; решил, что       }
{    разбираться с этой хренью будет себе дороже и взял готовое решение      }
function THelpTopic.WrapText(var text; Size: integer;
  var Offset: integer; Wrap: boolean): String;
  type
    PCArray = ^CArray;
    CArray = array[0..32767] of Char;
  var
    Line: String;
    i, P: integer;

  function IsBlank(Ch: Char): boolean;
    begin
      IsBlank := (Ch = ' ') or (Ch = #13) or (Ch = #10);
    end;

  function Scan(var P; Offset, Size: integer; C: Char): integer;
      assembler;
    {&USES esi,edi} {&FRAME-}
  asm
        cld
        mov     edi,P
        add     edi,&Offset
        mov     edx,Size
        sub     edx,&Offset
        test    dh,dh
        jz      @@1
        mov     edx,256
      @@1:
        mov     ecx,edx
        mov     al,C
        repne   scasb
        sub     ecx,edx
        neg     ecx
        xchg    eax,ecx
end
    ;

  procedure TextToLine(var text; Offset, Length: integer; var Line:
      String);
    assembler;
    {&USES esi,edi} {&FRAME-}
  asm
        cld
        mov     esi,Text
        add     esi,&Offset
        mov     edi,Line
        mov     eax,Length
        stosb
        xchg    eax,ecx
        rep     movsb
end
    ;

  begin { THelpTopic.WrapText }
    i := Scan(text, Offset, Size, #13);
    if (i >= Width) and Wrap then
      begin
        i := Offset+Width-1;
        if i > Size then
          i := Size
        else
          begin
            while (i > Offset) and not IsBlank(PCArray(@Text)^[i]) do
                Dec(i);
            if i = Offset then
              i := Offset+Width
            else
              Inc(i);
          end;
        if i = Offset then
          i := Offset+Width;
        Dec(i, Offset);
      end;
    TextToLine(text, Offset, i, Line);
    if Line[Length(Line)] = #13 then
      SetLength(Line, Length(Line)-1);
    Inc(Offset, i);
    WrapText := Line;
  end { THelpTopic.WrapText };

{ THelpIndex }

Constructor THelpIndex.Init;
  begin
    inherited Init;
    Size := 0;
    Contexts := nil;
    Index := nil;
  end;

Constructor THelpIndex.Load(var s: TStream);
  begin
    s.Read(Used, SizeOf(Used));
    s.Read(Size, SizeOf(Size));
    if Size = 0 then
      begin
        Contexts := nil;
        Index := nil;
      end
    else
      begin
        GetMem(Contexts, SizeOf(Contexts^[0])*Size);
        s.Read(Contexts^, SizeOf(Contexts^[0])*Size);
        GetMem(Index, SizeOf(Index^[0])*Size);
        s.Read(Index^, SizeOf(Index^[0])*Size);
      end;
  end;

destructor THelpIndex.Done;
  begin
    FreeMem(Index, SizeOf(Index^[0])*Size);
    FreeMem(Contexts, SizeOf(Contexts^[0])*Size);
    inherited Done;
  end;

function THelpIndex.Find(i: AWord): AWord;
  var
    Hi, Lo, Pos: AInt;
  begin
    Lo := 0;
    Hi := Used-1;
    while Lo <= Hi do
      begin
        Pos := (Lo+Hi) div 2;
        if i > Contexts^[Pos] then
          Lo := Pos+1
        else
          begin
            Hi := Pos-1;
            if i = Contexts^[Pos] then
              Lo := Pos;
          end;
      end;
    Find := Lo;
  end;

function THelpIndex.Position(i: AWord): longInt;
  begin
    Position := Index^[Find(i)];
  end;

procedure THelpIndex.Add(i: AWord; Val: longInt);
  const
    Delta = 10;
  var
    P: PIndexArray;
    NewSize: AInt;
    Pos: AInt;

  function Grow(P: Pointer; OldSize, NewSize, ElemSize: AInt):
      Pointer;
    var
      NewP: PByteArray;
    begin
      GetMem(NewP, NewSize*ElemSize);
      if NewP <> nil then
        begin
          if P <> nil then
            Move(P^, NewP^, OldSize*ElemSize);
          FillChar(NewP^[OldSize*ElemSize], (NewSize-Size)*ElemSize,
            $FF);
        end;
      if OldSize > 0 then
        FreeMem(P, OldSize*ElemSize);
      Grow := NewP;
    end;

  begin { THelpIndex.Add }
    Pos := Find(i);
    if (Contexts = nil) or (Contexts^[Pos] <> i) then
      begin
        Inc(Used);
        if Used >= Size then
          begin
            NewSize := (Used+Delta) div Delta*Delta;
            Contexts := Grow(Contexts, Size, NewSize, SizeOf(
              Contexts^[0]));
            Index := Grow(Index, Size, NewSize, SizeOf(Index^[0]));
            Size := NewSize;
          end;
        if Pos < Used then
          begin
            Move(Contexts^[Pos], Contexts^[Pos+1], (Used-Pos-1)*
            SizeOf(Contexts^[0]));
            Move(Index^[Pos], Index^[Pos+1], (Used-Pos-1)*
            SizeOf(Index^[0]));
          end;
      end;
    Contexts^[Pos] := i;
    Index^[Pos] := Val;
  end { THelpIndex.Add };

procedure THelpIndex.Store(var s: TStream);
  begin
    s.Write(Used, SizeOf(Used));
    s.Write(Size, SizeOf(Size));
    s.Write(Contexts^, SizeOf(Contexts^[0])*Size);
    s.Write(Index^, SizeOf(Index^[0])*Size);
  end;

{ THelpFile }

const
  MagicHeader = $46484246; {'FBHF'}

Constructor THelpFile.Init(s: PStream);
  var
    magic: longInt;
  begin
    magic := 0;
    s^.Seek(0);
    if s^.GetSize > SizeOf(magic) then
      s^.Read(magic, SizeOf(magic));
    if magic <> MagicHeader then
      begin
        IndexPos := 12;
        s^.Seek(IndexPos);
        Index := New(PHelpIndex, Init);
        Modified := True;
      end
    else
      begin
        s^.Seek(8);
        s^.Read(IndexPos, SizeOf(IndexPos));
        s^.Seek(IndexPos);
        Index := PHelpIndex(s^.Get);
        Modified := False;
      end;
    Stream := s;
  end { THelpFile.Init };

destructor THelpFile.Done;
  var
    magic, Size: longInt;
  begin
    if Modified then
      begin
        Stream^.Seek(IndexPos);
        Stream^.Put(Index);
        Stream^.Seek(0);
        magic := MagicHeader;
        Size := Stream^.GetSize-8;
        Stream^.Write(magic, SizeOf(magic));
        Stream^.Write(Size, SizeOf(Size));
        Stream^.Write(IndexPos, SizeOf(IndexPos));
      end;
    Dispose(Stream, Done);
    Stream := nil;
    Dispose(Index, Done);
    Index := nil;
  end;

function THelpFile.GetTopic(i: AWord): PHelpTopic;
  var
    Pos: longInt;
  begin
    Pos := Index^.Position(i);
    if Pos > 0 then
      begin
        Stream^.Seek(Pos);
        GetTopic := PHelpTopic(Stream^.Get);
      end
    else
      GetTopic := InvalidTopic;
  end;

function THelpFile.InvalidTopic: PHelpTopic;
  var
    Topic: PHelpTopic;
    Para: PParagraph;
  const
    InvalidStr = #13' No help available in this context.';
    InvalidText: array[1..Length(InvalidStr)] of Char = InvalidStr;
  begin
    Topic := New(PHelpTopic, Init);
    GetMem(Para, SizeOf(Para^)+SizeOf(InvalidText));
    Para^.Size := SizeOf(InvalidText);
    Para^.Wrap := False;
    Para^.Next := nil;
    Move(InvalidText, Para^.text, SizeOf(InvalidText));
    Topic^.AddParagraph(Para);
    InvalidTopic := Topic;
  end;

procedure THelpFile.RecordPositionInIndex(i: AInt);
  begin
    Index^.Add(i, IndexPos);
    Modified := True;
  end;

procedure THelpFile.PutTopic(Topic: PHelpTopic);
  begin
    Stream^.Seek(IndexPos);
    Stream^.Put(Topic);
    IndexPos := Stream^.GetPos;
    Modified := True;
  end;

procedure NotAssigned(var s: TStream; Value: AInt);
  begin
  end;

end.
