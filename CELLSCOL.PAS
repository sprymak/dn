{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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

unit CellsCol;

interface

uses  Objects, collect;

type
     PCellRec =^TCellRec;
     TCellRec = record
      Col: Byte;
      Row: Integer;
      Options: Word;
      Decimals: Byte;
      Value: Real;
      S: String;
     end;

     PCellCollection = ^TCellCollection;
     TCellCollection = object(TSortedCollection)
      constructor ShortLoad(var S: TStream);
      procedure ShortStore(var S: TStream);
      procedure FreeItem(Item: Pointer); virtual;
      function NewItem(Col: Byte; Row: Integer; S: String): PCellRec;
      function ReplaceItem(Col: Byte; Row: Integer; S: String): PCellRec;
      function Get(Col: Byte; Row: Integer): PCellRec;
      procedure SetValue(Col: Byte; Row: Integer; AValue: Real);
      procedure DelItem(Col: Byte; Row: Integer);
      procedure PutItem(var S: TStream; Item: Pointer); virtual;
      function GetItem(var S: TStream): Pointer; virtual;
      function MakeFormatString(AValue: Real): String;
      function Compare(K1, K2: Pointer): Integer; virtual;
     end;

const
    MaxCellY = 4096;


    LenDataRec = Sizeof(Byte) + SizeOf(Integer) + SizeOf(Word) +
                 SizeOf(Byte) + sizeof(Real) + SizeOf(Char);

{      Options :
          x x x x x x x
          | | | | | | `,
          | | | | | \--^- Cell format (Text, Formula, Value)
          | | | | \---,
          | | | \------^- Justify options (Right, Left, Center)
          | | \-------,
          | \----------<
          \------------^- Display format(As is, Dec, Comma, Exp, Currency,
                                            Logic, Don't display)
}
    coText         = $0000;
    coFormula      = $0001;
    coValue        = $0002;
    coLeft         = $0004;
    coCenter       = $0008;
    coRight        = $000C;
    coDec          = $0010;
    coComma        = $0020;
    coExp          = $0030;
    coBool         = $0040;
    coCurrency     = $0050;
    coDONT         = $0060;

    CommaChar : Char = ',';
    SeparatorChar: Char = '|';

 function MakeComma(S: String): String;
 function GetCellCoord(S: String; var X: Byte;var Y:Integer): Boolean;
 function GetCellName(X, Y: Integer): String;

implementation

uses Memory, advance, advance1, advance2, advance3, ObjType;

const VObjType:word=otCellCollection;
procedure TCellCollection.ShortStore(var S: TStream);
  procedure DoPutItem(P: Pointer); {$IFDEF BIT_16} far; {$ENDIF}
  begin
   PutItem(S, P);
  end;
begin
  S.Write(VObjType, 2);
  S.Write(Count, 2);
  S.Write(Limit, 2);
  S.Write(Delta, 2);
  ForEach(@DoPutItem);
  S.Write(Duplicates, SizeOf(Duplicates));
end;

constructor TCellCollection.ShortLoad(var S: TStream);
var q: word;
  C, I: Integer;
  ACount, ALimit, ADelta: integer;
begin
  S.Read(q, 2);
  if q<>VObjType then fail;
  S.Read(ACount, SizeOf(Integer));
  S.Read(ALimit, SizeOf(Integer));
  S.Read(ADelta, SizeOf(Integer));
  Inherited Init(ALimit, ADelta);
  SetLimit(ACount);
  for I := 0 to ACount - 1 do AtInsert(I, GetItem(S));
  S.Read(Duplicates, SizeOf(Boolean));
end;

procedure TCellCollection.FreeItem;
begin
 FreeMem(Item, LenDataRec + Length(PCellRec(Item)^.S));
end;

function TCellCollection.MakeFormatString;
 var S: String;
begin
 Str(AValue:20:2, S);
 DelLeft(S);
 MakeFormatString := S;
end;

function TCellCollection.NewItem;
 var P: PCellRec;
begin
 P := MemAlloc(LenDataRec + Length(S));
 P^.Row := Row; P^.Col := Col;
 P^.S := S;
 P^.Options := 0;
 P^.Decimals := 0;
 P^.Value := 0;
 Insert(P);
 NewItem := P;
end;

procedure TCellCollection.DelItem;
 var I: Integer;
     P: PCellRec;
begin
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin
     AtFree(I - 1); Exit;
    end;
  end;
end;

procedure TCellCollection.SetValue;
 var I: Integer;
     P: PCellRec;
begin
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin
     P^.Value := AValue;
     Exit;
    end;
  end;
end;

function TCellCollection.ReplaceItem;
 var I, D, O: Integer;
     P: PCellRec;
begin
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin
     O := P^.Options; D := P^.Decimals;
     FreeItem(P);
     P := MemAlloc(LenDataRec + Length(S));
     P^.Row := Row; P^.Col := Col;
     P^.S := S; AtPut(I - 1, P);
     P^.Options := O; P^.Decimals := D;
     ReplaceItem := P;
     Exit;
    end;
  end;
  ReplaceItem := NewItem(Col, Row, S);
end;

procedure TCellCollection.PutItem;
 var P: PCellRec;
begin
 P := Item;
 S.Write(P^, LenDataRec + Length(P^.S));
end;

function TCellCollection.GetItem;
 var R: TCellRec;
     P: PCellRec;
begin
 S.Read(R, LenDataRec);
 S.Read(R.S[1], Byte(R.S[0]));
 P := MemAlloc(LenDataRec + Byte(R.S[0]));
 Move(R, P^, LenDataRec + Byte(R.S[0]));
 GetItem := P;
end;

function TCellCollection.Get;
 var I: Integer;
     P: PCellRec;
begin
 Get := nil;
 for I := 1 to Count do
  begin
   P := At(I - 1);
   if (P^.Row = Row) and (P^.Col = Col) then
    begin Get := P; Exit end;
  end;
end;

function TCellCollection.Compare;
 var Key1, Key2: PCellRec;
begin
 Compare := 0;
 Key1 := K1; Key2 := K2;
 if Key1 = Key2 then Compare := 0 else
  if Key1 = nil then Compare := -1
   else if Key2 = nil then Compare := 1
    else if Key1^.Row < Key2^.Row then Compare := -1
     else if Key1^.Row > Key2^.Row then Compare := 1
      else if Key1^.Col < Key2^.Col then Compare := -1
       else if Key1^.Col > Key2^.Col then Compare := 1;
end;

{-----------------------------------------------------------------------}

function GetCellCoord(S: String; var X: Byte;var Y:Integer): Boolean;
 var S1: String[5];
     I: Integer;
begin
 UpStr(S);
 S1:=''; I := 1;
 if S[I] = '@' then Inc(I);
 While (I <= Length(S)) and (S[I]>='A') and (S[I]<='Z') do Inc(I);
 S1 := Copy(S, I+Byte(S[I]='@'), 100); S[0] := Char(I-1);
 Val(S1, Y, I); Dec(Y);
 GetCellCoord := not ((S[0]>#2) or (S='') or (S[0]=#2) and
                 (S[1]>'IV') or (Y<0) or (Y>MaxCellY));
 if S[0] = #1 then X := Byte(S[1]) - 65 else X := (Byte(S[1]) - 64) * 26 + Byte(S[2]) - 65;
end;

function GetCellName(X, Y: Integer): String;
 const Dig : String [26] = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
 var S: String[10];
begin
  Str(Y + 1, S);
  if X < 26 then S := Dig[X + 1] + S
      else begin S := Dig[X div 26] + Dig[X mod 26 + 1] + S end;
  GetCellName := S;
end;

function MakeComma(S: String): String;
 var S1: String;
     I: Integer;
 label Quit;
begin
 S1 := S;
 asm
  lea di, S
  mov cl, ss:[di]
  xor dx, dx
  jcxz Quit
  mov al, '.'
@@1:
  inc dl
  inc di
  cmp al, ss:[di]
  jz @@2
  dec cl
  jnz @@1
  inc dl
@@2:
  mov al, dl
  cmp al, 5
  jc  Quit
  dec al
  jz Quit
  mov bl, 3
  xor ah, ah
  div bl
  mov cl, al
  or ah, ah
  jnz @@6
  dec cl
@@6:
  xor ch, ch
  mov dl, al
  lea di, S
  lea si, S1
  add cl, ss:[di]
  mov ss:[si], cl
  mov bl, ah
  or bl, bl
  jnz @@3
  mov bl, 3
  dec dl
@@3:
  inc si
  or bl, bl
  jnz @@4
  mov al, CommaChar
  mov ss:[si], al
  mov bl, 3
  dec dl
  jnz @@5
  mov bl, 255
  jmp @@5
@@4:
  inc di
  mov al, ss:[di]
  dec bl
@@5:
  mov ss:[si], al
  loop @@3
 end;
Quit: MakeComma := S1;
end;

end.
