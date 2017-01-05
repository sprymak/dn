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

unit DBWatch;

interface

uses
  Lfn, collect, Objects, Dos, advance, advance1, advance2, advance3, use16;

type
     PFieldRec = ^Fieldrec;
     FieldRec = record
      Name: String[10];
      Who: Char;
      Ln: Word;
      Len, Dec: Word;
      Pos: Word;
     end;

     PFieldCollection = ^TFieldCollection;
     TFieldCollection = object(TCollection)
      procedure FreeItem( P : Pointer); virtual;
     end;

     PDBFile = ^TDBFile;
     TDBFile = object(TObject)
      WriteMode: Byte;
      BaseName: String; {DataCompBoy}
      BaseFile: TBufStream;
      Date, NumRec: LongInt;
      HeaderLen, RecLen: Word;
      Unused: Array [0..31] of Byte;
      NumFields: Integer;
      Loc: LongInt;
      Fields: PCollection;
      constructor Init(FileName : String); {DataCompBoy}
      destructor Done; virtual;
      procedure Read(var Buf; Num: Word);
      procedure Seek(NewLoc: LongInt);
{      function GetField(Name: String;var Buf) : String;
      function GetNField(N: Word;var Buf) : String;
}      function GetFieldRec(N : Integer) : PFieldRec;
      procedure OpenMode(Mode: Word);
     end;

{AK155 извлечь текст поля из буфера записи }
procedure GetFrField(PFR: PFieldRec; var Buf; var S: string);

implementation

{AK155}
procedure GetFrField(PFR: PFieldRec; var Buf; var S: string);
assembler; {&uses esi,edi } {&FRAME-}
  asm
  mov   esi, PFR
  movzx ecx, word ptr[esi].FieldRec.Len
  cmp   ecx, 254 { Kirill }
  jbe   @@0      { 254, а не 255, так как в DelRight ошибка: "inc al" }
  mov   ecx,254  { Kirill } { For Long Char Field Read First 255 Char }
@@0:
  movzx esi, word ptr[esi].FieldRec.Pos
  add   esi,Buf
  mov   edi,S
  cld
  mov   byte ptr[edi],cl {длина}
  inc   edi
  jecxz @@1
  rep movsb
@@1:

(* Я зумаю, 16-разядный код такой:
  push  ds
  les   si,PFR
  mov   cx, word ptr [si].FieldRec.Len
  mov   ax, word ptr [si].FieldRec.Pos
  lds   si,Buf
  add   di,ax
  lds   di,S
  cld
  mov   byte ptr DS:[di],cl {длина}
  inc   di
  jecx  @@1
  rep movsb
@@1:
  pop   ds
*)
  end;
{/AK155}

procedure TFieldCollection.FreeItem;
begin
 Dispose(PFieldRec(P));
end;

function NewField(Name: String; Who: Char; Len : Word; Dec: Byte;Pos: Word) : PFieldRec;
 var P: PFieldRec;
begin
 New(P);
 P^.Name := Name;
 while Name[Length(Name)] = ' ' do SetLength(Name, Length(Name)-1);
 P^.Who := Who;
 if Len=0
  then P^.Len := 1
  else P^.Len := Len;
 P^.Dec := Dec;
 P^.Pos := Pos;
 NewField := P;
 If P^.Who='D'
  then If Length(Name) > 10  then P^.Ln := Length(Name) else P^.Ln := 10
  else If Length(Name) > Len then P^.Ln := Length(Name) else P^.Ln := Len;
 if P^.Ln > 255 then P^.Ln:=255; { Kirill }
end;

procedure TDBFile.OpenMode(Mode: Word);
begin
 BaseFile.Init(BaseName, Mode, 16384);
end;

constructor TDBFile.Init;

type
    TFldLenRec = record { Kirill }
      case integer of
        0: (Len, Dec : byte);
        1: (CharFieldLen : SmallWord);
    end;
    frec = record
     Name: Array [0..10] of Char;
     Who: Char;
     Info1: Array [0..3] of Char;
     FldLen : TFldLenRec; { Kirill }
     Info2: Array [0..13] of Char;
    end;

var FBuf: FRec;
    I, J, RL: Integer;
    S: String;
    FieldLen : SmallWord;
    FieldDec : byte;

begin
 inherited Init;
 BaseName := lFExpand(FileName); {DataCompBoy}
 OpenMode(stOpenRead);
 if BaseFile.Status <> stOk then begin BaseFile.Done;Fail;end;
 BaseFile.Read(Date, 32);
 if BaseFile.Status <> stOk then begin BaseFile.Done;Fail;end;
{piwamoto.src.begin}{bugfixed .DBF detection}
 NumFields := -1;
 repeat
   BaseFile.Read(FBuf,1); { Kirill } { For last field only one byte }
   if FBuf.Name[0] >= #32 then BaseFile.Seek(BaseFile.GetPos+31);
   inc(NumFields);
 until (BaseFile.Status <> stOk) or 
       (FBuf.Name[0] < #32) or
       (HeaderLen < BaseFile.GetPos);
 if (NumFields < 1) or
    (FBuf.Name[0] <> #13)  or
    (BaseFile.Status <> stOk) or
    (HeaderLen + 32 < BaseFile.GetPos) 
   then begin BaseFile.Done;Fail;end;
 BaseFile.Seek(32);
{piwamoto.src.end}
 Fields := New(PFieldCollection, Init(NumFields, NumFields));
 RL := 1;
 for I := 1 to NumFields do
  begin
   BaseFile.Read(FBuf, 32);
   if FBuf.Who = 'C' then begin { Kirill }
     FieldLen:=FBuf.FldLen.CharFieldLen;
     FieldDec:=0;
   end
   else begin
     FieldLen:=FBuf.FldLen.Len; 
     FieldDec:=FBuf.FldLen.Dec;
   end;
   Inc(RL, FieldLen); { Kirill }
   S := ''; J := 0;
   While FBuf.Name[J] >= #32 do
    begin
     S := S + FBuf.Name[J];
     Inc(J);
    end;
   SetLength(S, J);
   if (J = 0) or (BaseFile.Status <> stOk) then
      begin Dispose(Fields,Done);Fields:=nil;BaseFile.Done;Fail;end;
   with FBuf do Fields^.Insert(NewField(S, Who, FieldLen, FieldDec, RL - FieldLen));
  end;
 if RL <> RecLen then
   begin Dispose(Fields,Done);Fields:=nil;BaseFile.Done;Fail;end;
 Loc := 0;
 BaseFile.Seek(HeaderLen);
end;

procedure TDBFile.Read;
 var I: LongInt;
begin
 if Loc + Num >= NumRec then Num := NumRec - Loc;
 I := LongInt(RecLen)*LongInt(Num);
 if I > 65520 then I := 65520;
 BaseFile.Read(Buf, I);
 Inc(Loc, Num);
end;

procedure TDBFile.Seek;
begin
 BaseFile.Seek(NewLoc * RecLen + HeaderLen);
 Loc := NewLoc;
end;

function TDBFile.GetFieldRec;
begin
 GetFieldRec := Fields^.At(N);
end;
(*
function TDBFile.GetField;
 var I, K, N: Integer;
     B: Array [0..65000] of Char Absolute Buf;
     S: String;
     L: Word;
begin
 I := 0; K := 1;
 While (I < NumFields) and (GetFieldRec(I)^.Name <> Name) do Inc(I);
 K := GetFieldRec(I)^.Pos;
 S := ''; N := I;
 for I := K to K + GetFieldRec(N)^.Len - 1 do S[I - K] := B[I];
 SetLength(S, GetFieldRec(N)^.Len);
 GetField := S;
end;

function TDBFile.GetNField;
 var I, K, J: Integer;
     B: Array [0..65000] of Char Absolute Buf;
     S: String;
     L: Word;
begin
 K := GetFieldRec(N)^.Pos;
 S := '';
 for I := K to K + GetFieldRec(N)^.Len - 1 do S[I - K + 1] := B[I];
 S[0] := Char(GetFieldRec(N)^.Len);
 GetNField := S;
end;
*)
destructor TDBFile.Done;
begin
 Dispose(Fields,Done); Fields:=nil;
 BaseFile.Done;
end;

end.
