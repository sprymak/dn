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
{Writted by DataCompBoy at 29.07.2000 21:04:26}
{AK155 = Alexey Korop, 2:461/155@fidonet}
{Cat = Aleksej Kozlov, 2:5030/1326.13@fidonet}

{Cat
   28/08/2001 - переделал функции для совместимости с типами AnsiString и
   LongString, а также для поддержки коллекций с длинными строками
}

Unit WinClpVp ;
interface

 Uses Objects, Collect ;

 Function SetWinClip( PC : PLineCollection ):boolean;
 Function GetWinClip( Var PCL : PLineCollection; NeedStream: boolean ):boolean;
 Function GetWinClipSize : boolean ;
 procedure SyncClipIn;
 procedure SyncClipOut(NeedStream: boolean);

 procedure CopyLines2Stream( PC: PCollection; var PCS: PStream);
 procedure CopyStream2Lines( PCS: PStream; var PC: PCollection);


Implementation
uses VpSysLow, Microed, Advance, Advance1, DnIni;

 procedure SyncClipIn;
 begin
    if Clipboard <> nil then
      SetWinClip(PLineCollection(Clipboard));
 end;

 procedure SyncClipOut(NeedStream: boolean);
 begin
    GetWinClip(PLineCollection(Clipboard), NeedStream);
 end;

{Cat: добавил обработку коллекций с длинными строками}
 Function SetWinClip( PC : PLineCollection ):boolean;
 var B: PChar;
     idx: PChar;
     Size: Longint;
     i: Longint;
     P: PString; {AK155}
     PL: PLongString;
     S: LongString;
 begin
  Size:=0;

  if PC^.LongStrings then
    begin
      for I:=0 to PC^.Count-1 do
        begin
          PL := PC^.At(I);
          if PL = nil then
            Inc(Size, 2)
          else
            Inc(Size, Length(PL^)+2);
        end;
    end
  else
{AK155 Исправлена ошибка в подсчете длины
(не учитывалось, что в коллекции могут быть nil) }
    begin
      for I:=0 to PC^.Count-1 do
        begin
          P := PC^.At(I);
          if P = nil then
            Inc(Size, 2)
          else
            Inc(Size, Length(P^)+2);
        end;
    end;
{/AK}
  Dec(Size);
  GetMem(B, Size);
  idx:=b;
  For I:=0 to PC^.Count-1 do
    begin
      if I>0 then
        begin
          S:=#$0D#$0A;
          move(S[1], idx^, Length(S));
          inc(idx, Length(S));
        end;
      if PC^.LongStrings then
        S:=CnvLongString(PC^.At(I))
      else
        S:=CnvString(PC^.At(I));
      move(S[1], idx^, Length(S));
      inc(idx, Length(S));
    end;
  byte(idx^):=0;
  SetWinClip:=SysClipCopy(B, Size);
  FreeMem(B, Size);
 end;
{/Cat}


{AK155 GetWinClip переписан почти полностью, так как та версия,
которая мне досталась, во-первых, не работала, а во-вторых,
делала это очень медленно :) }
 Function GetWinClip( Var PCL : PLineCollection; NeedStream: boolean ):boolean;
  var Size : longint;
      Buf : PChar;
     {idx : PChar;
      s: string;}
      Start, Stop: PChar;
      i: longint;
     {ODOA: boolean;}
      f0D: boolean; { только что был 0D }

{Cat: переписал для совместимости с типами AnsiString и LongString
      добавил обработку коллекций с длинными строками}

    { текст от Start^ (включительно) до Stop^ (исключительно)
     преобразовать в строку и добавить в PCL }
    procedure Str2Collection;
    var
      l: longint;
      P: PString;
      PL: PLongString;
    begin
      L := Stop-Start;
      if L = 0 then
        with PCL^ do
          AtInsert(Count, Nil)
      else
        if PCL^.LongStrings then
          begin
{$IFDEF USELONGSTRING}
            New(PL);
{$ELSE}
            GetMem(PL, L+1);
{$ENDIF}
            SetLength(PL^, L);
            move(Start^, PL^[1], L);
            with PCL^ do
              AtInsert(Count, PL);
          end
        else
          begin
{$IFDEF USEANSISTRING}
            New(P);
{$ELSE}
            GetMem(P, L+1);
{$ENDIF}
            SetLength(P^, L);
            move(Start^, P^[1], L);
            with PCL^ do
              AtInsert(Count, P);
          end;
      Start := Stop+1;
    end;
{/Cat}

  begin
   if not SysClipCanPaste then begin
    GetWinClip := false;
    exit
   end else GetWinClip := true;

   if PCL<>nil then PCL^.FreeAll
   else New(PCL, Init($10, $10, True));

   Buf := SysClipPaste(Size);
   Start := Buf;
   Stop := Start;

   for i:=1 to size-1 do begin
     case Stop^ of
       #$0D:
         begin
         Str2Collection;
         f0D := true;
         end;
       #$0A:
         begin
         if f0D then
           inc(Start)
         else
           Str2Collection;
         f0D := false;
         end;
       else
        f0D := false;
     end {case};
     inc(Stop);
   end;
   Str2Collection;
{/AK155}

 FreeMem(Buf, Size);
 Buf := nil;
 Size := 0;

   if NeedStream then begin
     I := 0;
     if ClipBoardStream<>nil then
      ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-{$IFDEF USELONGSTRING} 4 {$ELSE} 1 {$ENDIF}, 0));
     CopyLines2Stream( PCL, ClipBoardStream );
   end;
  end;

 Function GetWinClipSize : boolean ;
 begin
  GetWinClipSize := SysClipCanPaste;
 end;

{Cat: добавил обработку коллекций с длинными строками}
 procedure PackLinesStream(var PCS: PStream); {-$VOL begin}
 var
   ps: PString;
   pls: PLongString;
   sp: Longint;
   MS: PStream;
   LongStrings: Boolean;
   TempString: String;
   TempLongString: LongString;
 begin
  sp:=PCS^.GetSize-CBSize;
  PCS^.Seek(0);
  PCS^.Read(LongStrings, 1); {читаем флажок "длинноты" строк}
  if LongStrings then
    while PCS^.GetPos<sp do
      begin
(*
        pls:=PCS^.ReadLongStr;
        DisposeLongStr(pls);
*)
        PCS^.ReadLongStrV(TempLongString);
      end
  else
    while PCS^.GetPos<sp do
      begin
(*
        ps:=PCS^.ReadStr;
        DisposeStr(ps);
*)
        PCS^.ReadStrV(TempString);
      end;

  MS:=GetMeMemoStream;
  if ms=nil then
    exit;
  ms^.Write(LongStrings, 1); {пишем флажок "длинноты" строк}
  if LongStrings then
    while (PCS^.GetPos<PCS^.GetSize) do
      begin
        pls:=PCS^.ReadLongStr;
        ms^.WriteLongStr(pls);
        DisposeLongStr(pls);
      end
  else
    while (PCS^.GetPos<PCS^.GetSize) do
      begin
        ps:=PCS^.ReadStr;
        ms^.WriteStr(ps);
        DisposeStr(ps);
      end;
  Dispose(PCS,Done);
  PCS:=ms;
 end; {-$VOL end}

 procedure CopyLines2Stream(PC: PCollection; var PCS: PStream); {-$VOL begin}
 var
   i: Longint;
   P: PLineCollection absolute PC;
   Pos: Longint;
   LongStrings: Boolean;
   TempString: String;
   TempLongString: LongString;
 begin
   if PC = nil then
     Exit;
   if PCS = nil then
     PCS := GetMeMemoStream;
   if PCS = nil then
     Exit;
   Pos := PCS^.GetPos;
   if Pos = 0 then
     begin
       LongStrings := P^.LongStrings;
       PCS^.Write(LongStrings, 1) {пишем флажок "длинноты" строк}
     end
   else
     with PCS^ do
       begin      {выдёргиваем записанный когда-то давно в самое}
         Seek(0); {начало потока флажок "длинноты" строк}
         Read(LongStrings, 1);
         Seek(Pos);
       end;
   if LongStrings then
     if P^.LongStrings then
       begin {пишем длинные строки из коллекции в длинные строки потока}
         for i := 0 to P^.Count-1 do
           PCS^.WriteLongStr(P^.At(i));
         PCS^.WriteLongStr(nil);
       end
     else
       begin {пишем короткие строки из коллекции в длинные строки потока}
         for i := 0 to P^.Count-1 do
           begin
             TempLongString := PString(P^.At(i))^;
             PCS^.WriteLongStr(@TempLongString);
           end;
         PCS^.WriteLongStr(nil);
       end
   else
     if P^.LongStrings then
       begin {пишем длинные строки из коллекции в короткие строки потока}
         for i := 0 to P^.Count-1 do
           begin
             TempString := PLongString(P^.At(i))^;
             PCS^.WriteStr(@TempString);
           end;
         PCS^.WriteStr(nil);
       end
     else
       begin {пишем короткие строки из коллекции в короткие строки потока}
         for i := 0 to P^.Count-1 do
           PCS^.WriteStr(P^.At(i));
         PCS^.WriteStr(nil);
       end;
   if (PCS^.GetSize > CBSize) and (P^.Count > 1) then
     PackLinesStream(PCS);
 end; {-$VOL end}

 procedure CopyStream2Lines(PCS: PStream; var PC: PCollection); {-$VOL begin}
 var
   PS: PString;
   PLS: PLongString;
   LongStrings: Boolean;
 begin
   if PCS = nil then
     Exit;
   PCS^.Seek(0);
   PCS^.Read(LongStrings, 1); {читаем флажок "длинноты" строк}
   if PC = nil then
     PC := New(PLineCollection, Init(50,5,LongStrings));
   if PC = nil then
     Exit;
   if LongStrings then
     begin
       PLS:=PCS^.ReadLongStr;
       while (PLS<>nil) and not PCS^.EOF do
         begin
           PC^.AtInsert( 0, PLS );
           if (PCS^.GetPos > CBSize) and (PC^.Count > 1) then
             PC^.AtFree(0);
           PLS:=PCS^.ReadLongStr;
         end;
     end
   else
     begin
       PS:=PCS^.ReadStr;
       while (PS<>nil) and not PCS^.EOF do
         begin
           PC^.AtInsert( 0, PS );
           if (PCS^.GetPos > CBSize) and (PC^.Count > 1) then
             PC^.AtFree(0);
           PS:=PCS^.ReadStr;
         end;
     end;
   PC^.Pack;
 end; {-$VOL end}
{/Cat}

end.
