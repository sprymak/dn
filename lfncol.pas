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
unit LFNCol;

interface

uses Hash, Advance1, Advance2, Objects,
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF}
     {$IFDEF LFNCache}, ExtraMem, Collect, startup
     {$ELSE} {$ENDIF}
     ;

const HashRange = $0FFFF;

type
 TLFNIndex = Pointer;

procedure InitLFNCol; {Initialise LFN collection}

function    AddLFN(const Name: string): TLFNIndex;   {Add name}
function    AddCFN(Name: pchar):  TLFNIndex;         {Add name}
procedure   DelLFN(var ind: TLFNIndex);              {Delere name}
function    UseLFN(ind: TLFNIndex):TLFNIndex;        {Increase name usage}
function    GetLFN(ind: TLFNIndex): string;          {Return name as String}
function    GetCFN(ind: TLFNIndex): pchar;           {Return name as PChar}
function    CmpLFN(LFN1, LFN2: TLFNIndex): integer;  {Compares 2 long names}
function    CmpLEX(LFN1, LFN2: TLFNIndex): integer;  {Compares 2 long extentions}
procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);{Store name}
function    LoadLFN(var S: TStream): TLFNIndex;      {Load name}

{$IFDEF LFNCache}
type
 PLFNRecord = ^TLFNRecord;
 TLFNRecord = record
               Hash: word;
               UsageCount: longint;
               Name: PChar;
              end;

Const CLFNRecMax = 65520 div sizeof(PLFNRecord);
Type  TLFNSRec   = array [0..CLFNRecMax] of PLFNRecord;
      PLFNSRec   = ^TLFNSRec;

type
 PLFNCollection = ^TLFNCollection;
 TLFNCollection = object(TSortedCollection)
                   constructor Init(AL, AD: LongInt);          {Create collection}
                   function    AddLFN(const Name: string): TLFNIndex;
                                                               {Add name}
                   function    AddCFN(Name: pchar):  TLFNIndex;{Add name}
                   procedure   DelLFN(var ind: TLFNIndex);     {Delete name}
                   function    UseLFN(ind: TLFNIndex):TLFNIndex;{Increase name usage}
                   function    GetLFN(ind: TLFNIndex): string; {Return name as String}
                   function    GetCFN(ind: TLFNIndex): pchar;  {Return name as PChar}
                   procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
                                                               {Store name}
                   function    LoadLFN(var S: TStream): TLFNIndex;{load name}
                  private
                   procedure   FreeItem(Item: pointer);               virtual;
                   function    Compare(p1, p2: pointer): integer;     virtual;
                  end;

{$IFNDEF DPMI}{Under DPMI EMS and XMS memory not needed (DataCompBoy)}
type
 PSTMRec = ^TSTMRec;
 TSTMRec = record
            Hash: Word;
            Size: Word;
            IsAvail: boolean;
           end;

type
 PEFNCollection = ^TEFNCollection; {LFNs in EMS}
 TEFNCollection = object(TObject)
                   constructor Init;
                   destructor  Done; virtual;
                   function    AddLFN(const Name: string): TLFNIndex;
                   function    AddCFN(Name: pchar):  TLFNIndex;
                   procedure   DelLFN(var ind: TLFNIndex);
                   function    UseLFN(ind: TLFNIndex):TLFNIndex;
                   function    GetLFN(ind: TLFNIndex): string;
                   function    GetCFN(ind: TLFNIndex): pchar;
                   procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
                   function    LoadLFN(var S: TStream): TLFNIndex;
                  private
                   stream:     PStream;
                   procedure   FreeItem(Item: pointer);               virtual;
                   Function    Search(Key: pointer; var Index: LongInt;
                                      var Size: word): Boolean;       virtual;
                  end;

type
 PXFNCollection = ^TXFNCollection; {LFNs in XMS}
 TXFNCollection = object(TEFNCollection)
                   constructor Init;
                  end;
{$ENDIF}

{$ELSE !LFNCache}

type
 ALFNIndex = Record
              Hash: Word;
              S: String;
             End;
 PLFNIndex = ^ALFNIndex;

{$ENDIF LFNCache}

implementation

{$IFDEF LFNCache}
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ TLFNCollection }
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ LFNs collector in convertional memory }

 constructor TLFNCollection.Init;
 begin inherited Init(AL, AD); Duplicates:=true; end;

 procedure   TLFNCollection.FreeItem(Item: pointer);
 begin
  if Item=nil then exit;
  if PLFNRecord(Item)^.Name<>nil then StrDispose(PLFNRecord(Item)^.Name);
  Dispose((PLFNRecord(Item)));
 end;

 function    TLFNCollection.Compare(p1, p2: pointer): integer;
 begin
  if (PLFNRecord(p1)^.hash<PLFNRecord(p2)^.hash) then Compare:=-1 else
   if (PLFNRecord(p1)^.hash>PLFNRecord(p2)^.hash) then Compare:=1 else
    if p1=nil then Compare:=-1 else
     if p2=nil then Compare:=1 else
      Compare:=StrComp(PLFNRecord(p1)^.Name, PLFNRecord(p2)^.Name);
 end;

 function    TLFNCollection.AddLFN(const Name: string): TLFNIndex;
 var l: byte; z: byte;
 begin
  l:=length(Name); z:=0;
  move(Name[1], (@(Name[0]))^, l);
  move(z, (@(Name[l]))^, 1);
  AddLFN:=AddCFN(@Name[0]);
  move(Name[0], (@(Name[1]))^, l);
  move(l, (@(Name[0]))^, 1);
 end;

 function    TLFNCollection.AddCFN(Name: pchar):  TLFNIndex;
 var plr: PLFNRecord; i: longint;
 begin
  New(plr);
  plr^.UsageCount:=1;
  plr^.Hash:=HashString(Name, StrLen(Name), HashRange);
  plr^.Name:=StrNew(Name);
  if Search(plr, i) then
   begin
    inc(PLFNRecord(At(I))^.UsageCount);
    FreeItem(plr);
    AddCFN:=At(i);
   end
  else
   begin
    Insert(plr);
    AddCFN:=plr;
   end;
 end;

 procedure   TLFNCollection.DelLFN(var ind: TLFNIndex);
 var i: longint;
 begin
  if ind=nil then exit;
  Dec(PLFNRecord(ind)^.UsageCount);
  if PLFNRecord(ind)^.UsageCount=0
   then if Search(ind, i) then AtFree(i) else FreeItem(ind);
  If Limit-Count>Delta then SetLimit((Count div Delta)*Delta);
  ind:=nil;
 end;

 function    TLFNCollection.UseLFN(ind: TLFNIndex): TLFNIndex;
 begin
  UseLFN:=ind;
  if ind<>nil then inc(PLFNRecord(ind)^.UsageCount);
 end;

 function    TLFNCollection.GetLFN(ind: TLFNIndex): string;
 begin if ind<>nil then GetLFN:=StrPas(PLFNRecord(ind)^.Name); end;

 function    TLFNCollection.GetCFN(ind: TLFNIndex): pchar;
 begin if ind<>nil then GetCFN:=PLFNRecord(ind)^.Name; end;

 procedure   TLFNCollection.StoreLFN(var S: TStream; LFN: TLFNIndex);
 begin S.StrWrite(PLFNRecord(LFN)^.Name); end;

 function    TLFNCollection.LoadLFN(var S: TStream): TLFNIndex;
 var pc: pchar;
 begin
  pc:=S.StrRead;
  LoadLFN:=AddCFN(pc);
  StrDispose(pc);
 end;

{$IFNDEF DPMI}
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ TEFNCollection }
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ LFNs collector in extended memory }

 constructor TEFNCollection.Init;
 var hdr: TSTMRec;
 begin
  inherited Init;
  Stream := new(PEMSStream, Init($100000, $1000000));
  hdr.size:=0;
  stream^.write(hdr, sizeof(hdr));
 end;

 destructor  TEFNCollection.Done;
 begin
  Dispose(Stream,Done); Stream:=nil;
  inherited done;
 end;

 procedure   TEFNCollection.FreeItem(Item: pointer);
 var hdr: TSTMRec;
 begin
  if longint(Item)=$0FFFFFFFF then exit;
  stream^.status:=stOk;
  stream^.Seek(longint(Item));
  stream^.Read(hdr, sizeof(hdr));
  hdr.IsAvail:=true;
  stream^.Seek(longint(Item));
  stream^.Write(hdr, sizeof(hdr));
 end;

 function    TEFNCollection.Search(Key: pointer; var Index: Longint; var Size: Word): Boolean;
 var hdr: TSTMRec;
 begin
  Search := False;
  Index := 0;
  stream^.status:=stOk;
  repeat
   stream^.seek(Index);
   stream^.read(hdr, sizeof(hdr));
   if (hdr.IsAvail) and (hdr.size>=Size)
    then begin
          Search:=true;
          Size:=hdr.Size;
          exit;
         end;
   inc(Index, hdr.size+SizeOf(TSTMRec));
  until hdr.size=0;
  dec(Index, SizeOf(TSTMRec));
 end;

 function    TEFNCollection.AddLFN(const Name: string): TLFNIndex;
 var l: byte; z: byte;
 begin
  l:=length(Name); z:=0;
  move(Name[1], (@(Name[0]))^, l);
  move(z, (@(Name[l]))^, 1);
  AddLFN:=AddCFN(@Name[0]);
  move(Name[0], (@(Name[1]))^, l);
  move(l, (@(Name[0]))^, 1);
 end;

 function    TEFNCollection.AddCFN(Name: pchar):  TLFNIndex;
 var plr: TLFNIndex;
     hdr: TSTMRec;
     b: boolean;
 begin
  hdr.Size:=StrLen(Name)+1;
  hdr.Hash:=HashString(Name, StrLen(Name), HashRange);
  hdr.IsAvail:=false;
  b:=Search(plr, longint(plr), hdr.size);
  Stream^.status:=stOk;
  Stream^.Seek(longint(plr));
  Stream^.Write(hdr, sizeof(hdr));
  Stream^.Write(Name^, hdr.size);
  if not b then
   begin
    hdr.size:=0;
    Stream^.Write(hdr, sizeof(hdr));
   end;
  AddCFN:=plr;
 end;

 procedure   TEFNCollection.DelLFN(var ind: TLFNIndex);
 begin
  if longint(ind)=$0FFFFFFFF then exit;
  FreeItem(ind);
  longint(ind):=$0FFFFFFFF;
 end;

 function    TEFNCollection.UseLFN(ind: TLFNIndex): TLFNIndex;
 begin
  if longint(ind)=$0FFFFFFFF then begin UseLFN:=ind; exit end;
  UseLFN:=AddCFN(GetCFN(ind));
 end;

 function    TEFNCollection.GetLFN(ind: TLFNIndex): string;
 var p: pchar;
 begin
  if longint(ind)<>$0FFFFFFFF then
   begin
    p:=GetCFN(ind);
    GetLFN:=StrPas(p);
   end else GetLFN:='';
 end;

 var NameBuf: array[0..300] of char;
 function    TEFNCollection.GetCFN(ind: TLFNIndex): pchar;
 var hdr: TSTMRec;
 begin
  if longint(ind)<>$0FFFFFFFF then
   begin
    stream^.status:=stOk;
    stream^.seek(longint(ind));
    stream^.read(hdr, sizeof(hdr));
    stream^.read(NameBuf, hdr.size);
    GetCFN:=@NameBuf;
   end else GetCFN:=nil;
 end;

 procedure   TEFNCollection.StoreLFN(var S: TStream; LFN: TLFNIndex);
  var pc: pchar;
  begin
   if longint(LFN)<>$0FFFFFFFF then S.StrWrite(GetCFN(LFN));
  end;

 function    TEFNCollection.LoadLFN(var S: TStream): TLFNIndex;
 var pc: pchar;
 begin
  pc:=S.StrRead;
  LoadLFN:=AddCFN(pc);
  StrDispose(pc);
 end;

{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ TXFNCollection }
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ LFNs collector in expanded memory }

 constructor TXFNCollection.Init;
 var hdr: TSTMRec;
 begin
  TObject.Init;
  Stream := new(PXMSStream, Init($100000, $1000000));
  hdr.size:=0;
  stream^.write(hdr, sizeof(hdr));
 end;

 var EFNs: PEFNCollection;
{$ENDIF}

 var MFNs: PLFNCollection;


{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ Interface functions }
{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ LFNs collector interface functions }

function    AddLFN(const Name: string): TLFNIndex;   {Add name}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then AddLFN:=EFNs^.AddLFN(Name)
              else
{$ENDIF}
  AddLFN:=MFNs^.AddLFN(Name)
end;

function    AddCFN(Name: pchar):  TLFNIndex;         {Add name}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then AddCFN:=EFNs^.AddCFN(Name)
              else
{$ENDIF}
  AddCFN:=MFNs^.AddCFN(Name)
end;

procedure   DelLFN(var ind: TLFNIndex);              {Delete name}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then EFNs^.DelLFN(ind)
              else
{$ENDIF}
  MFNs^.DelLFN(ind)
end;

function    UseLFN(ind: TLFNIndex):TLFNIndex;        {Increase name usage}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then UseLFN:=EFNs^.UseLFN(ind)
              else
{$ENDIF}
  UseLFN:=MFNs^.UseLFN(ind)
end;

function    GetLFN(ind: TLFNIndex): string;          {Return name as String}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then GetLFN:=EFNs^.GetLFN(ind)
              else
{$ENDIF}
  GetLFN:=MFNs^.GetLFN(ind)
end;

function    GetCFN(ind: TLFNIndex): pchar;           {Return name as  PChar}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then GetCFN:=EFNs^.GetCFN(ind)
              else
{$ENDIF}
  GetCFN:=MFNs^.GetCFN(ind)
end;

procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);{Store name}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then EFNs^.StoreLFN(S, LFN)
              else
{$ENDIF}
  MFNs^.StoreLFN(S, LFN)
end;

function    LoadLFN(var S: TStream): TLFNIndex;      {Load it}
begin
{$IFNDEF DPMI}
 if EFNs<>nil then LoadLFN:=EFNs^.LoadLFN(S)
              else
{$ENDIF}
  LoadLFN:=MFNs^.LoadLFN(S)
end;

{Memory freer}
var EP: pointer;
procedure EXP; {$IFDEF BIT_16}far;{$ENDIF} begin
 ExitProc:=ep;
{$IFNDEF DPMI}
 if EFNs<>nil then Dispose(EFNs,Done); EFNs:=nil;
{$ENDIF}
 if MFNs<>nil then Dispose(MFNs,Done); MFNs:=nil;
end;

procedure InitLFNCol;
begin
 if (MFNs<>nil)
{$IFNDEF NOEXTRA}
    or (EFNs<>nil)
{$ENDIF}
  then exit;
{$IFNDEF NOEXTRA}
 case SystemData.LFNContainer of
  0: if EMSFound and (EMSFreePages>$100)
      then begin New(EFNs, Init); MFNs:=nil end
      else
       if XMSFound and (XMSFree>$100)
        then begin EFNs:=New(PXFNCollection, Init); MFNs:=nil end
        else begin New(MFNs, Init($40, $40)); EFNs:=nil end;
  1: if XMSFound and (XMSFree>$100)
      then begin EFNs:=New(PXFNCollection, Init); MFNs:=nil end
      else
       if EMSFound and (EMSFreePages>$100)
        then begin New(EFNs, Init); MFNs:=nil end
        else begin New(MFNs, Init($40, $40)); EFNs:=nil end;
  else begin New(MFNs, Init($40, $40)); EFNs:=nil end;
 end;
{$ELSE NOEXTRA}
 New(MFNs, Init($40, $40));
{$ENDIF NOEXTRA}
 EP:=ExitProc; ExitProc:=@EXP;
end;

{$ELSE !LFNCache}

procedure InitLFNCol;
begin end;

function    AddLFN(const Name: string): TLFNIndex;
var P:TLFNIndex;
begin
(*  GetMem(P, Length(Name)+1+SizeOf(Word{HASH}));                             *)
(*  PLFNIndex(P)^.Hash:=HashString(@(Name[1]), Length(Name), HashRange);      *)
(*  PLFNIndex(P)^.S:=Name+#0;                                                 *)
  P:=NewStr(Name+#0);
  AddLFN:=P;
end;

function    AddCFN(Name: pchar):  TLFNIndex;
begin AddCFN:=AddLFN(StrPas(Name)) end;

procedure   DelLFN(var ind: TLFNIndex);
(*begin FreeMem(ind, Length(PLFNIndex(ind)^.S)+SizeOf(Word{HASH})) end;       *)
begin DisposeStr(PString(ind)) end;

function    UseLFN(ind: TLFNIndex): TLFNIndex;
begin UseLFN:=AddLFN(GetLFN(ind)) end;

function    GetLFN(ind: TLFNIndex): string;
begin
 if ind<>nil
(*  then GetLFN:=Copy(PLFNIndex(ind)^.S, 1, Length(PLFNIndex(ind)^.S)-1)      *)
  then GetLFN:=Copy(PString(ind)^, 1, Length(PString(ind)^)-1)
  else GetLFN:=''
end;

function    GetCFN(ind: TLFNIndex): pchar;
begin
 if ind<>nil then GetCFN:=@(PLFNIndex(ind)^.s[1]) else GetCFN:=nil
end;

procedure   StoreLFN(var S: TStream; LFN: TLFNIndex);
(* Var Len: Word;                                                             *)
begin
(* Len:=Length(PLFNIndex(LFN)^.S)+SizeOf(Word{HASH});                         *)
(* S.Write(Len, SizeOf(Len));                                                 *)
(* S.Write(PLFNIndex(LFN)^, Len);                                             *)
  S.WriteStr(LFN);
end;

function    LoadLFN(var S: TStream): TLFNIndex;
(* Var Len: Word;                                                             *)
(*     I: TLFNIndex;                                                          *)
begin
(* S.Read(Len, SizeOf(Len));                                                  *)
(* GetMem(I, Len);                                                            *)
(* S.Read(PLFNIndex(I)^, Len);                                                *)
(* LoadLFN:=I;                                                                *)
  LoadLFN:=S.ReadStr;
end;

{$ENDIF LFNCache}

function    CmpLFN(LFN1, LFN2: TLFNIndex): integer;
type PWord=^Word;
var S1, S2: string;
begin
(* If PWord(LFN1)^>PWord(LFN2)^ then CmpLFN:=+1 else                          *)
(* If PWord(LFN1)^<PWord(LFN2)^ then CmpLFN:=-1 else                          *)
 begin
  S1:=GetLFN(LFN1); S2:=GetLFN(LFN2);
  UpStr(S1);        UpStr(S2);
  if S1>S2 then CmpLFN:=+1 else
  if S1<S2 then CmpLFN:=-1 else
                CmpLFN:=0;
 end;
end;

function    CmpLEX(LFN1, LFN2: TLFNIndex): integer;
var S1, S2: string;
    E1, E2: string;
begin
 S1:=UpStrg(GetLFN(LFN1));             S2:=UpStrg(GetLFN(LFN2));
 E1:=GetExt(S1);                       E2:=GetExt(S2);
 S1[0]:=Char(Byte(S1[0])-Byte(E1[0])); S2[0]:=Char(Byte(S2[0])-Byte(E2[0]));
 if E1>E2 then CmpLEX:=+1 else
 if E1<E2 then CmpLEX:=-1 else
 if S1>S2 then CmpLEX:=+1 else
 if S1<S2 then CmpLEX:=-1 else
               CmpLEX:=0;
end;

end.
