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

Unit WinClp ;
interface

 Uses Objects , advance, advance1, advance2, advance3, collect ;

 procedure PackClipStr(var S: string); {-$VOL begin}

 Function SetWinClip( PC : PLineCollection ):boolean;
 Function GetWinClip( Var PCL : PLineCollection; NeedStream: boolean ):boolean;
 Function GetWinClipSize : boolean ;
 procedure SyncClipIn;
 procedure SyncClipOut(NeedStream: boolean);

 procedure CopyLines2Stream( PC: PCollection; var PCS: PStream); {-$VOL}
 procedure CopyStream2Lines( PCS: PStream; var PC: PCollection); {-$VOL}


Implementation
uses MicroEd, DNini
{$IFNDEF NONBP}
{$IFDEF DPMI}, Dpmi, DosMem{$ENDIF}
{$ENDIF}
;

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

procedure PackClipStr(var S: string); {-$VOL begin}
  var j: byte absolute S;
      i: byte;
begin
  For i:=1 to j do if s[i]=#0 then s[i]:=' ';
end; {procedure PackClipStr}          {-$VOL end}

 procedure SyncClipIn;
 begin
    if Clipboard <> nil then
      SetWinClip(PLineCollection(Clipboard));
 end;


 procedure SyncClipOut(NeedStream: boolean);
 begin
    GetWinClip(PLineCollection(Clipboard),NeedStream);
 end;


 Function OpenClip : boolean ;
   {$IFNDEF NONBP}
   assembler ;
   asm    mov  ax,1701h
          int  2fh
          or   ax,ax
          jz   @@1
          mov  ax,1
   @@1:   end;
   {$ELSE}Begin end;
   {$ENDIF}

 Function CloseClip : boolean ;
   {$IFNDEF NONBP}
   assembler ;
   asm    mov  ax,1708h
          int  2fh
          or   ax,ax
          jz   @@1
          mov  ax,1
   @@1:   end;
   {$ELSE}Begin end;
   {$ENDIF}

 Function SetClip(var Buf; Size : word ) : boolean ;
{$IFNDEF NONBP}
{$IFNDEF DPMI}
   assembler ;
   asm
          les  bx,Buf
          mov  cx,size
          sub  si,si
          mov  dx, 7  { CF_OEMTEXT }
          mov  ax,1703h
          int  2fh
          or   ax,ax
          jz   @@1
          mov  ax,1
   @@1:   end;
{$ELSE}
   var Reg: DPMIRegisters;
       P: Pointer;
   begin
    Reg.AX:=$1703;
    Reg.CX:=Size;
    Reg.DX:=7; { CF_OEMTEXT }
    Reg.SI:=0;
    P:=GetDosMem(Size, Reg.ES);
    if P=nil then exit;
    Move(Buf, P^, Size);
    Reg.BX:=0;
    SimulateRealModeInt($2F,Reg);
    FreeDosMem(P);
    SetClip:=Reg.AX<>0;
   end;
{$ENDIF}
{$ELSE NONBP} begin end;
{$ENDIF NONBP}

 Function GetClipSize : longint ;
   {$IFNDEF NONBP}
   assembler ;
   asm
          mov  dx, 7  { CF_OEMTEXT }
          mov  ax,1704h
          int  2fh
   end;
   {$ELSE NONBP} begin end;
   {$ENDIF NONBP}

 Function EmptyClip : boolean ;
   {$IFNDEF NONBP}
   assembler ;
   asm
          mov  ax,1702h
          int  2fh
          or   ax,ax
          jz   @@1
          mov  ax,1
   @@1:   end;
   {$ELSE}Begin end;
   {$ENDIF}

 Function CompactClip( Size : longint  ) : longint ; assembler ;
{$IFDEF NONBP}
   asm
          mov  esi,Size
          mov  cx,si
          shr  esi,16
          mov  ax,1709h
          int  2fh
{$ELSE}
   asm
          mov  cx,Size.word
          mov  si,[Size+2].word
          mov  ax,1709h
          int  2fh
{$ENDIF}
   end;
 Function GetClip(var Buf; Size: LongInt ) : boolean ;
{$IFNDEF NONBP}
{$IFNDEF DPMI}
   assembler ;
   asm
          les  bx,Buf
          mov  dx, 7  { CF_OEMTEXT }
          mov  ax,1705h
          int  2fh
          or   ax,ax
          jz   @@1
          mov  ax,1
   @@1:   end;
{$ELSE}
   var Reg: DPMIRegisters;
       P: Pointer;
   begin
    Reg.AX:=$1705;
    Reg.DX:=7; { CF_OEMTEXT }
    P:=GetDosMem(Size, Reg.ES);
    if P=nil then exit;
    Reg.BX:=0;
    SimulateRealModeInt($2F,Reg);
    if Size>65528
     then Move(P^, Buf, 65528)
     else Move(P^, Buf, Size);
    FreeDosMem(P);
    GetClip:=Reg.AX<>0;
   end;
{$ENDIF}
{$ELSE NONBP}begin end;
{$ENDIF NONBP}

 Type
     DA  =  array [0..$FFF0] of byte ;
     pDA = ^DA ;

 Function SetWinClip( PC : PLineCollection ):boolean;
 var Size : longint ;
     Buf : PDA;
     BufPos : Word ;
    TmStr : String ;
   Function CopyLine( Line : PString ):boolean; {$IFDEF BIT_16}far;{$ENDIF}
    var L: byte absolute TmStr;
    begin
      CopyLine := False ;
      TmStr := CnvString(Line); PackClipStr(TmStr); TmStr := TmStr + #13#10;
      Replace(#0,' ',TmStr);
      if BufPos + L > Size then
          begin
           CopyLine := True ;
           Exit;
          end;
      Move(TmStr[1],Buf^[BufPos],L);
      inc(BufPos,L);
    end;
   begin
     SetWinClip := False ;
     if PC=nil then Exit;
     if PC^.Count=0 then Exit;
     if not OpenClip then Exit;
     if not EmptyClip then begin CloseClip; Exit; end;
     Size := $FFF0;
     if (Maxavail < Size ) then Size := (Maxavail - $FF0);
     GetMem(Buf,Size);
     BufPos := 0 ;
     PC^.FirstThat(@CopyLine);
     if BufPos<Size then
      begin Buf^[BufPos]:=0; inc(BufPos) end else Buf^[Size-1]:=0;
     if ( CompactClip( BufPos ) >= BufPos ) then
         SetWinClip := SetClip( Buf^ , BufPos );
     FreeMem(Buf,Size);
     CloseClip;
   end;


 Function GetWinClip( Var PCL : PLineCollection; NeedStream: boolean ):boolean;
    var Size : longint ;
        Buf : PDA;
        BufPos,SZ : Word ;
        TmStr : String ;
        PC: PLineCollection;
        I: longint;
    begin
     GetWinClip := False;
     if not OpenClip then Exit;
     Size := GetClipSize ;
     if Size > $EF00 then begin CloseClip; Exit; end;
     if (Size + $800) > (MaxAvail div 2) then begin CloseClip; Exit; end;
     GetMem(Buf,Size);
     if not GetClip(Buf^,Size) then
      begin FreeMem(Buf,Size); CloseClip; Exit; end;
     if PCL = nil then New(PCL,Init(10,10)); {-$VOL}
     New(PC,Init(10,10));
     BufPos := 0 ; SZ := Size;
     TmStr := '';
      while BufPos < SZ do
       begin
         case Buf^[BufPos] of
           10: begin
                 PC^.Insert(NewStr(TmStr));
                 TmStr:='';
                end;
          {13: if Buf^[BufPos+1]=10 then}
          13:;
          0:break;
         else
          begin
           AddStr(TmStr,char(Buf^[BufPos]));
           if Length(TmStr) >= 200 then
             if (Length(TmStr)=250)or(Buf^[BufPos]=$20) then begin
               Buf^[BufPos]:=10; continue; {-$VOL}
             end;
          end;
         end; { case }
         inc(BufPos);
       end;
     if TmStr<>'' then PC^.Insert(NewStr(TmStr));
     FreeMem(Buf,Size);
     CloseClip;
     {-$VOL begin}
     if NeedStream then begin
       I := 0;
       with PCL^ do
         while (Count > 0) and (I < PC^.Count) do begin
           if CnvString( At(0) ) <> CnvString( PC^.At(I) ) then break;
           AtFree(0); Inc(I);
         end;
       PCL^.FreeAll;
       with PCL^ do
         while I < PC^.Count do begin
           Insert( NewStr( CnvString( PC^.At(I) ) ) );
           Inc(I);
         end;
       if ClipBoardStream<>nil then
        ClipBoardStream^.Seek(Max(ClipBoardStream^.GetPos-1, 0));
       CopyLines2Stream( PCL, ClipBoardStream );
     end;
     Dispose(PCL,Done);
     PCL := PC;
     {-$VOL end}
     GetWinClip := PCL^.Count>0;
    end;

 Function GetWinClipSize : boolean ;
  var SZ : longint ;
    begin
      GetWinClipSize := false ;
      if not OpenClip then Exit;
       SZ := GetClipSize ;
        GetWinClipSize := ((SZ<$EF00) and (SZ>0));
      CloseClip ;
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
