{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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

unit PackF_RS;
(* Packed File: Read/Seek *)
(* Autor: Anton Fedorov AKA DataCompBoy (DN OSP v1.51.05) *)
(* Debugger: Sergey Korshunoff AKA SeYKo *)
(* 13.07.2000:                           *)
(*  Added paritally packed files support *)

interface

const
  {MaxHandles: Byte = 4;} {-JITR-}
  MaxHandles = 4;         {-JITR-} {to prevent modification}

function pOpen(Name: PChar; Mode: Byte): Word;
{$IFNDEF BIT_32}
procedure
  pfOpen(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); Interrupt;
  (*
  OPEN PACKED FILE WITH HANDLE
  IN:  AL    - Mode (open access mode)
       DS:DX - Name (pointer to an ASCIIZ file name)
  OUT: AX    - file handle if CF not set (error code if CF set)
  *)
{$ENDIF}

procedure pClose(F: word);
{$IFNDEF BIT_32}
procedure
  pfClose(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); Interrupt;
  (*
  CLOSE PACKED FILE USING HANDLE
  IN:  BX    - F (file handle)
  OUT: AX    - error code if CF set
  *)
{$ENDIF}

function pSeek(F: Word; Pos: LongInt): LongInt;
{$IFNDEF BIT_32}
procedure
  pfSeek(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); Interrupt;
  (*
  MOVE PACKED FILE POINTER USING HANDLE
  IN:  BX    - F (file handle)
       CX:DX - Pos (offset)
  OUT: DX:AX - new pointer location if CF not set (AX - error code if CF set)
  *)
{$ENDIF}

function pRead(F: Word; Num: Word; Buf: Pointer): Word;
{$IFNDEF BIT_32}
procedure
  pfRead (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word); Interrupt;
  (*
  READ FORM PACKED FILE USING HANDLE
  IN:  BX    - F (file handle)
       CX    - Num (number of bytes to read)
       DS:DX - Buf (pointer to read buffer)
  OUT: AX    - number of bytes readed if CF not set (error code if CF set)
  *)
{$ENDIF}

function pFileSize(F: Word): LongInt;

procedure CloseAllHandles;

implementation
uses
  {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF}, Dos, LFN, Pack_LZW;

Type
  TBuf = Array[0..65520] of Byte;
  PBuf = ^TBuf;

  TSeekArray = Array[0..1] of LongInt;
  PSeekArray = ^TSeekArray;

  TPackedFile = Record
    Used:      Boolean;
    PackedF:   Boolean;
    H:         lFile;
    SeekArr:   PSeekArray;
    PageSiz:   Word;
    Pages:     Word;
    LastPageS: Word;
    CurPage:   Word;
    CurPageOf: Word;
  end;

const
  MinRange = 1;
  MaxRange: Byte = 1;

type
  {TPackHandles = Array[MinRange..MinRange] of TPackedFile;} {-JITR-}
  TPackHandles = Array[MinRange..MaxHandles] of TPackedFile; {-JITR-}
  {To make the code more correct and prevent possible range check errors
  (when runtime checking is enabled) when dereferencing Handles^ below,
  maximum array size must be declared. -JITR-}

const
   Handles: ^TPackHandles = nil;

const
  PackedSig = 'Packed file'#26;
  Ver = 1;
  SizeOfPackedSig = 12+1;

function pOpen(Name: PChar; Mode: Byte): Word;
var
  SaveMode: Byte;
  FileSig: String[12];
  CP, CPS: Word;
  L: LongInt;
  i: Word;
  Vers: Word;
const
  AddOnS = 4*2 + SizeOfPackedSig;
begin
  if Handles = nil then begin
    if MaxHandles > MaxRange then MaxRange:=MaxHandles;
    GetMem(Handles, SizeOf(TPackedFile)*MaxRange);
    FillChar(Handles^, SizeOf(TPackedFile)*MaxRange, 0);
  end;
  pOpen:=0; InOutRes:=0;
  for i:=MinRange to MaxRange do if not Handles^[i].Used then break;
  if Handles^[i].Used then begin InOutRes:=04; Exit end;
  with Handles^[i] do
  begin
    SaveMode:=FileMode; FileMode:=Mode;
    lAssignFile(H, StrPas(Name)); lResetFile(H,1);
    FileMode:=SaveMode; if InOutRes<>0 then Exit;
    PackedF:=False; Used:=True;
    L:=FileSize(H.F);
    if L >= AddOnS then
    begin
      Seek(H.F, L-AddOnS); BlockRead(H.F, FileSig, SizeOf(FileSig));
      If FileSig=PackedSig then
      begin
        BlockRead(H.F, Vers, SizeOf(Vers));
        BlockRead(H.F, Pages, SizeOf(Pages));
        BlockRead(H.F, PageSiz, SizeOf(PageSiz));
        BlockRead(H.F, LastPageS, SizeOf(LastPageS));
        if (Vers<>Ver)
          or (Pages=0)
          or (PageSiz=0)
          or (L<AddOnS+SizeOf(LongInt)*(Pages+1))
        then
          InOutRes:=01
        else begin
          GetMem(SeekArr, SizeOf(LongInt)*(Pages+1)); PackedF:=True;
          Seek(H.F, L - AddOnS - SizeOf(LongInt)*(Pages+1));
          BlockRead(H.F, SeekArr^, SizeOf(LongInt)*(Pages+1));
          for CP:=0 to Pages-1 do begin
            CPS:=SeekArr^[CP+1] - SeekArr^[CP];
            if CPS > PageSiz then begin InOutRes:=01; break; end;
          end;
        end;
      end;
      if InOutRes<>0 then begin
        if PackedF then FreeMem(SeekArr, SizeOf(LongInt)*(Pages+1));
        Close(H.F); Used:=False;
      end else begin
        pOpen:=i; pSeek(i, 0);
      end;
    end;
  end;
end;

{$IFNDEF BIT_32}
procedure pfOpen(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word);
var
  Name: PChar;
  Mode, aHandle: Byte;
begin
  Name:=Ptr(DS, DX); Mode:=Lo(AX);
  aHandle:=pOpen(Name, Mode);
  AX:=IOResult;
  if AX<>0 then
    Flags:=Flags or fCarry
  else begin
    Flags:=Flags and not fCarry; AX:=aHandle;
  end;
end;
{$ENDIF}

procedure pClose(F: Word);
var
  i: Word;
  SomeOpen: Boolean;
begin
  InOutRes:=0;
  if (F<MinRange) or (F>MaxRange) or (Handles=nil)
    or not Handles^[F].Used then
    InOutRes:=06
  else with Handles^[F] do
  begin
    if PackedF then FreeMem(SeekArr, SizeOf(LongInt)*(Pages+1));
    Close(H.F); Used:=False;
    SomeOpen:=False;
    for i:=MinRange to MaxRange do if Handles^[i].Used then SomeOpen:=True;
    if not SomeOpen then begin
      FreeMem(Handles, SizeOf(TPackedFile)*MaxRange);
      Handles:=nil;
    end;
  end;
end;

{$IFNDEF BIT_32}
procedure pfClose(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word);
var
  I: Word;
begin
  pClose(BX); I:=IOResult;
  if I<>0 then begin
    Flags:=Flags or fCarry; AX:=I;
  end else
    Flags:=Flags and not fCarry;
end;
{$ENDIF}

function pSeek(F: Word; Pos: LongInt): LongInt;
begin
  pSeek := -1; InOutRes:=0;
  if (F<MinRange) or (F>MaxRange) or (Handles=nil)
    or not Handles^[F].Used then
    InOutRes:=06
  else with Handles^[F] do
  begin
    if not PackedF then begin
      Seek(H.F, Pos); pSeek:=Pos;
    end else
    begin
      if Pos > LongInt(PageSiz)*(Pages-1) + LastPageS then
        InOutRes:=$19
      else begin
        CurPage:=Pos div PageSiz;
        CurPageOf:=Pos mod PageSiz;
        pSeek:=Pos;
      end;
    end;
  end;
end;

{$IFNDEF BIT_32}
procedure pfSeek(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word);
var
  Pos: Longint;
  W: Word;
begin
  Flags:=Flags and not fCarry;
  Pos:=CX;
  Pos:=(Pos shl 16) + DX;
  Pos:=PSeek(BX, Pos);
  W:=IOResult;
  if W<>0 then begin Flags:=Flags or fCarry; AX:=W end
          else begin DX:=Pos shr 16; AX:=Pos and $FFFF end;
end;
{$ENDIF}

function pRead(F: Word; Num: Word; Buf: Pointer): Word;
var
  i, j, k: Word;
  InpBuf, OutBuf: PBuf;
  CurPageS, InpBufSize: Word;
begin
  pRead:=0; InOutRes:=0;
  if (F<MinRange) or (F>MaxRange) or (Handles=nil)
    or not Handles^[F].Used then
    InOutRes:=06
  else
  if not Handles^[F].PackedF then begin
    BlockRead(Handles^[F].H.F, Buf^, Num, I); pRead:=I;
  end
  else with Handles^[F] do
  begin
    InOutRes:=0; I:=Num; K:=0;
    LZW_Init; GetMem(InpBuf, PageSiz+1); GetMem(OutBuf, PageSiz+1);
    while True do
    begin
      if (CurPage>=Pages) or ((CurPage=Pages-1) and (CurPageOf>LastPageS))
      then begin InOutRes:=$1E; break; end;
      InpBufSize:=SeekArr^[CurPage+1]-SeekArr^[CurPage];
      if InpBufSize > PageSiz+1 then begin InOutRes:=$1E; break; end;
      Seek(H.F, SeekArr^[CurPage]); BlockRead(H.F, InpBuf^, InpBufSize, J);
      if (InOutRes<>0) or (J<>InpBufSize) then begin
        InOutRes:=$1E; break;
      end;
      if InpBuf^[0]=0 then CurPageS:=LZW_Decompress(InpBuf^[1], OutBuf^)
      else begin
       Move(InpBuf^[1], OutBuf^, InpBufSize);
       CurPageS := InpBufSize-1;
      end;
      if ((CurPage<Pages-1) and (CurPageS<>PageSiz))
        or ((CurPage=Pages-1) and (CurPageS<>LastPageS))
      then begin
        InOutRes:=$1E; break;
      end;
      if CurPageOf < CurPageS then
        j:=CurPageS-CurPageOf else
        j:=0;
      if j>i then j:=i;
      if j<>0 then Move(OutBuf^[CurPageOf],PBuf(Buf)^[k],j);
      Inc(CurPageOf,j); Inc(k,j); Dec(i,j);
      if i=0 then break;
      Inc(CurPage); CurPageOf:=0;
    end;
    FreeMem(OutBuf, PageSiz+1); FreeMem(InpBuf, PageSiz+1); LZW_Done;
    pRead:=Num-i;
  end;
end;

{$IFNDEF BIT_32}
procedure pfRead(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP: Word);
var
  W: Word;
begin
  Flags:=Flags and not fCarry; InOutRes:=0;
  AX:=pRead(BX, CX, Ptr(DS,DX));
  W:=IOResult;
  if W<>0 then begin Flags:=Flags or fCarry; AX:=06 end
end;
{$ENDIF}

function pFileSize(F: Word): LongInt;
begin
  pFileSize:=0; InOutRes:=0;
  if (F<MinRange) or (F>MaxRange) or (Handles=nil)
    or not Handles^[F].Used then
    InOutRes:=06
  else
  if not Handles^[F].PackedF then
    pFileSize:=FileSize(Handles^[F].H.F) else with Handles^[F] do
    pFileSize:=LongInt(PageSiz)*(Pages-1)+LastPageS;
end;

procedure CloseAllHandles;
var
  i: Word;
begin
  if Handles <> nil then begin
    for i:=MinRange to MaxRange do if Handles^[i].Used then pClose(i);
    FreeMem(Handles, SizeOf(TPackedFile)*MaxRange);
    Handles:=nil;
  end;
end;

end.
