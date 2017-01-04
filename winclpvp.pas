{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.11
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
{Writted by DataCompBoy at 29.07.2000 21:04:26}
{$I STDEFINE.INC}

Unit WinClpVp ;
interface

 Uses Objects, collect ;

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

 Function SetWinClip( PC : PLineCollection ):boolean;
 var B: PChar;
     idx: PChar;
     Size: Longint;
     i: longint;
     S: String;
 begin
  Size:=0;
  for I:=0 to PC^.Count-1 do begin
   Size:=Size+Length(PString(PC^.At(I))^)+2;
  end;
  Dec(Size);
  GetMem(B, Size);
  idx:=b;
  For I:=0 to PC^.Count-1 do begin
   if I>0 then begin
    S:=#$0D#$0A;
    move(S[1], idx^, Length(S));
    inc(idx, Length(S));
   end;
   S:=CnvString(PC^.At(I));
   move(S[1], idx^, Length(S));
   inc(idx, Length(S));
  end;
  byte(idx^):=0;
  SetWinClip:=SysClipCopy(B, Size);
  FreeMem(B, Size);
 end;


 Function GetWinClip( Var PCL : PLineCollection; NeedStream: boolean ):boolean;
  var Size : longint ;
      Buf : PChar;
      idx : PChar;
      s: string;
      i: longint;
      ODOA: boolean;
  begin
   if not SysClipCanPaste then begin
    GetWinClip := false;
    exit
   end else GetWinClip := true;

   if PCL<>nil then PCL^.FreeAll
   else New(PCL, Init($10, $10));

   Buf := SysClipPaste(Size);

   s:=''; ODOA:=false;
   for i:=1 to size do begin
    if (idx^ in [#$0D, #$0A]) then begin
     if ODOA then begin
      PCL^.Insert(NewStr(S));
      S:='';
      ODOA:=False;
     end else ODOA:=true;
    end else
    if ODOA then begin
     PCL^.Insert(NewStr(S));
     S:='';
     ODOA:=False;
    end else
     S:=S+idx^;

    inc(idx);
   end;
   if ODOA then begin
    PCL^.Insert(NewStr(S));
    S:='';
    ODOA:=False;
   end;
   FreeMem(Buf, Size);

   if NeedStream then begin
     I := 0;
     if ClipBoardStream<>nil then
      ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
     CopyLines2Stream( PCL, ClipBoardStream );
   end;
  end;

 Function GetWinClipSize : boolean ;
 begin
  GetWinClipSize := SysClipCanPaste;
 end;

 procedure PackLinesStream( var PCS: PStream ); {-$VOL begin}
  var
   ps: PString;
   sp: longint;
   MS: PStream;
 begin
  sp:=PCS^.GetSize-CBSize;
  PCS^.Seek(0);
  while PCS^.GetPos<sp do
   begin
    ps:=PCS^.ReadStr;
    DisposeStr(ps);
   end;
  MS:=GetMeMemoStream;
  if ms=nil then exit;
  while (PCS^.GetPos<PCS^.GetSize) do
   begin
    ps:=PCS^.ReadStr;
    ms^.WriteStr(ps);
    disposestr(ps);
   end;
  Dispose(PCS,Done);
  PCS:=ms;
 end; {-$VOL end}

 procedure CopyLines2Stream( PC: PCollection; var PCS: PStream); {-$VOL begin}
   var i: longint;
       P: PLineCollection absolute PC;
 begin
   if PC = nil then Exit;
   if PCS = nil then PCS := GetMeMemoStream;
   if PCS = nil then Exit;
   for i := 0 to P^.Count - 1 do PCS^.WriteStr( P^.At(i) );
   PCS^.WriteStr(nil);
   if (PCS^.GetSize > CBSize) and (P^.Count > 1) then PackLinesStream( PCS );
 end; {-$VOL end}

 procedure CopyStream2Lines( PCS: PStream; var PC: PCollection); {-$VOL begin}
 var PS: PString;
 begin
   if PCS = nil then Exit;
   if PC = nil then PC := New(PLineCollection, Init(50,5));
   if PC = nil then Exit;
   PCS^.Seek(0);
   PS:=PCS^.ReadStr;
   while (PS<>nil) and not PCS^.EOF do begin
     PC^.AtInsert( 0, PS );
     if (PCS^.GetPos > CBSize) and (PC^.Count > 1) then PC^.AtFree(0);
     PS:=PCS^.ReadStr;
   end;
   PC^.Pack;
 end; {-$VOL end}
end.
