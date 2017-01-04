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
unit BigArray;
(* Autor -- Fedorov Anton aka DataCompBoy, 10 July 2000 *)

interface
Uses
  Objects;

type

{ TBigArray object }

  PBigArray = ^TBigArray;
  TBigArray = object(TObject)
    constructor Init(AItemSize: Word; AStartCount, ADelta: LongInt);
    destructor  Done; virtual;
    function  Count:    LongInt;
    function  Status:   Integer;
{   function  At(Index: LongInt): Pointer;                                   }
{   function  AtArray(Index: LongInt; var ACount: Word): Pointer;            }
    procedure AtGet(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtDelete(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtInsert(Index: LongInt; ACount: Word; Item: Pointer);
    procedure AtReplace(Index: LongInt; ACount: Word; Item: Pointer);
{   procedure InsertFrom(IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray);  }
{   procedure ReplaceFrom(IndexF,IndexT,ACount: LongInt; ArrayF: PBigArray); }
{   procedure DeleteTo(IndexF,IndexT,ACount: LongInt; ArrayT: PBigArray);    }
{   procedure Error(Code: Integer; Info: LongInt); virtual;                  }
  private
    _ItemSize: Word;
    CurrentCount: LongInt;
    Delta: LongInt;
    Stream: PStream;
  end;

implementation

 Function Min(A, B: LongInt): LongInt; begin if A<B then Min:=A else Min:=B end;

 constructor TBigArray.Init(AItemSize: Word; AStartCount, ADelta: LongInt);
 begin
  Inherited Init;
  _ItemSize    := AItemSize;
  CurrentCount := 0;
  Delta        := ADelta;
  if Delta*_ItemSize > 65520 then fail;
  Stream := New(PMemoryStream, Init(AStartCount*_ItemSize, Delta*_ItemSize));
  if Stream = nil then fail;
 end;

 Destructor  TBigArray.Done;
 begin
  Dispose(Stream, Done);
 end;

 function    TBigArray.Count:    LongInt;
 begin
  Count := CurrentCount;
 end;

 function    TBigArray.Status:   Integer;
 begin
  Status := Stream^.Status;
 end;

 procedure   TBigArray.AtGet(Index: LongInt; ACount: Word; Item: Pointer);
 var T: longint;
 begin
  Stream^.Reset;
  Stream^.Seek(Index*_ItemSize);
  if Stream^.Status<>stOk then exit;
  T := Index + ACount;
  T := T * _ItemSize;
  if T > Stream^.GetSize then begin
   T := Stream^.GetSize;
   T := T - (Index * _ItemSize);
   If T<0 then T := 0;
   FillChar(Item^, ACount * _ItemSize, 0);
   Stream^.Read(Item^, T);
  end else Stream^.Read(Item^, ACount * _ItemSize);
 end;

 const BufSize=8192;
 procedure   TBigArray.AtDelete(Index: LongInt; ACount: Word; Item: Pointer);
 var PA, PB: LongInt;
     SA: LongInt;
     Buf: Pointer;
 begin
  GetMem(Buf, BufSize);
  PA := (Index+ACount) * _ItemSize;
  SA := Min(BufSize, Stream^.GetSize - PA);
  PB := Index * _ItemSize;
  Stream^.Reset;
  repeat
   Stream^.Seek(PA);Stream^.Read(Buf^, SA); Inc(PA, SA);
   Stream^.Seek(PB);Stream^.Write(Buf^, SA);Inc(PB, SA);
   SA := Min(BufSize, Stream^.GetSize - PA);
  until (SA<=0) or (Stream^.Status<>stOk);
  CurrentCount := CurrentCount - ACount;
  If CurrentCount < 0 then CurrentCount := 0;
  FreeMem(Buf, BufSize);
 end;

 procedure   TBigArray.AtInsert(Index: LongInt; ACount: Word; Item: Pointer);
 var CP, AP, BP, NB, SS: Longint;
     Buf: Pointer;
 begin
  if Index<CurrentCount then begin
   GetMem(Buf, BufSize);
   CP := Index * _ItemSize;
   AP := Stream^.GetSize - BufSize;
   if AP < CP then AP := CP;
   BP := AP + ACount * _ItemSize;
   NB := Min(BufSize, Stream^.GetSize-AP);

   Stream^.Reset;
   repeat
    SS := Min(BufSize, AP - CP);
    Stream^.Seek(AP);Stream^.Read (Buf^, NB);Dec(AP, SS);
    Stream^.Seek(BP);Stream^.Write(Buf^, NB);Dec(BP, SS);
    NB := SS;
   until (SS=0) or (AP<CP) or (Stream^.Status<>stOk);
   FreeMem(Buf, BufSize);
  end;
  Stream^.Seek(Index*_ItemSize);
  Stream^.Write(Item^, ACount*_ItemSize);

  Inc(CurrentCount, ACount);
 end;

 procedure   TBigArray.AtReplace(Index: LongInt; ACount: Word; Item: Pointer);
 begin
  Stream^.Reset;
  Stream^.Seek(Index*_ItemSize);
  Stream^.Write(Item^, ACount*_ItemSize);
  if Index+ACount > CurrentCount then CurrentCount := Index+ACount;
 end;

end.
