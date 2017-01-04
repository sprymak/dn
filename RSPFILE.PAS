{⁄¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬ø}
{√≈≈ èÆ§§•p¶™† yØ†™Æ¢†≠≠ÎÂ ‰†©´Æ¢, ØÆ ™Æ®¨ ¨Æ≠† ØÆß®‚®‚·Ô ≈≈¥}
{√≈≈  Ä¢‚Æp: Ä≠‚Æ≠ î•§ÆpÆ¢ aka DataCompBoy                ≈≈¥}
{√≈≈  Ç†Ô´ ¢ Á•·‚Ï å†‚†pÎ™®≠Æ© ì´ÏÔ≠Î...                  ≈≈¥}
{¿¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡¡Ÿ}
{$i-}{$o-}
unit RSPFile;

interface
uses
 LZW4P,
 Dos, LFN;

Procedure PFOpen (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {OPEN PACKED FILE WITH HANDLE
  IN:  AL    - open access mode
       DS:DX - pointer to an ASCIIZ file name
  OUT: AX    - file handle if CF not set
             - error code if CF set
 }

Procedure PFSeek (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {MOVE PACKED FILE POINTER USING HANDLE
  IN:  AL    - always must be 0
       BX    - file handle
       CX    - high oder word of bytes to move
       DX    - low order word of bytes to move
  OUT: AX    - error code if CF set
       DX:AX - new pointer location if CF not set
 }

Procedure PFRead (Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {READ FORM PACKED FILE USING HANDLE
  IN:  BX    - file handle
       CX    - number of bytes to read
       DS:DX - pointer to read buffer
  OUT: AX    - number of bytes read if CF not set
             - error code if CF set
 }

Procedure PFClose(Flags, CS, IP, AX, BX, CX, DX, SI, DI, DS, ES, BP : Word);Interrupt;
 {CLOSE PACKED FILE USING HANDLE
  IN:  BX    - file handle
  OUT: AX    - error code if CF set
 }

Function POpen(Name: PChar; Mode: byte): word;
Function PSeek(F: word; Pos: longint): longint;
Function PRead(F: word; Num: word; Buf: pointer): word;
Procedure PClose(F: word);

implementation

Type PBuf = ^TBuf;
     TBuf = array[0..65520] of byte;

Type TPR = Record LcPos: longint end;
     Poss = array[0..70] of TPR;

const MinRange = 4;
      MaxRange = 4;

const Handles: array[MinRange..MaxRange] of record
                                Used:   Boolean;
                                H:      lFile;
                                GlPoss: Poss;
                                LP:     Word;
                                CP:     Word;
                                PageSiz:Word;
                                lps:    Word;
                                InBuf:  PBuf;
                                ibp:    Word;
                                ibs:    Word;
                                OuBuf:  PBuf;
                                obp:    Word;
                                PsBuf:  Word;
                               end
 = ((Used:false){,(Used:false),(Used:false),(Used:false),
    (Used:false),(Used:false),(Used:false)});

function StrPas(Str: PChar): String; assembler;
asm
        PUSH    DS
        CLD
        LES     DI,Str
        MOV     CX,0FFFFH
        XOR     AL,AL
        REPNE   SCASB
        NOT     CX
        DEC     CX
        LDS     SI,Str
        LES     DI,@Result
        MOV     AL,CL
        STOSB
        REP     MOVSB
        POP     DS
end;

var Sig: Word;

Function POpen;
var savemode:byte;
    s: string;
    i: byte;
begin
 for i:=MinRange to MaxRange do if not Handles[i].Used then break;
 if Handles[i].Used then begin InOutRes:=04; exit end;
 with Handles[i] do
  begin
   s:=StrPas(Name);
   savemode:=filemode;
   filemode:=mode;
   lAssignFile(H, s);
   lResetFile(H,1);
   filemode:=savemode;
   if InOutRes<>0 then exit;
   InBuf:=nil; OuBuf:=nil;
   ibs:=0; ibp:=0; obp:=0;
   LP:=0;
   Seek(H.F, FileSize(H.F)-SizeOf(lps)-SizeOf(PageSiz)-SizeOf(LP)-SizeOf(Sig));
   BlockRead(H.F, Sig, SizeOf(Sig));
   If Sig<>$0DCBF then begin close(H.F); InOutRes:=01; exit end;
   BlockRead(H.F, LP, SizeOf(LP));
   BlockRead(H.F, PageSiz, SizeOf(PageSiz)); GetMem(OuBuf, PageSiz);
   BlockRead(H.F, lps, SizeOf(lps));
   Seek(H.F, FileSize(H.F)-SizeOf(lps)-SizeOf(PageSiz)-SizeOf(LP)-SizeOf(Sig)-SizeOf(TPR)*(LP+1));
   BlockRead(H.F, GlPoss, SizeOf(TPR)*(LP+1));
   PSeek (i, 0);
   if InOutRes<>0 then exit;
   Used:=true;
  end;
 POpen:=i;
end;

Procedure PFOpen;
var Name: PChar;
    Mode: byte;
    i: byte;
begin
 Flags:=Flags and not fCarry;

 Name:=Ptr(DS, DX);
 Mode:=Lo(AX);
 I:=POpen(Name, Mode);
 AX:=IOResult;
 if AX<>0 then begin Flags:=Flags or fCarry; exit end;
 AX:=i;
end;

var cn: word;
function RP : integer;
begin
 with Handles[cn] do
  begin
   RP:= InBuf^[ibp];
   Inc(ibp);
  end;
end;

function Min(a, b: word): word; begin if a>b then Min:=b else Min:=a end;
function WP(TheByte: byte) : integer;
begin
 with Handles[cn] do
  begin
   if obp<PageSiz then
    OuBuf^[obp]:=TheByte;
   inc(obp)
  end;
 WP:=0;
end;

Function PFGetBuf(n: word): boolean;
var bq: word;
begin
 PFGetBuf:=true;
 with Handles[n] do
  begin
   if CP>=LP then begin PsBuf:=PageSiz; PFGetBuf:=false; exit end;
   obp:=0;
   ibp:=0; ibs:=GlPoss[CP+1].LcPos-GlPoss[CP].LcPos; GetMem(InBuf, ibs);
   Seek(H.F, GlPoss[CP].LcPos);
   blockread(H.F, InBuf^, ibs, bq);
   cn:=n;
   InitLZW(@LZAllocMemory, 14);
   Expand(@RP, @WP);
   TermLZW(@LZFreeMemory);
   FreeMem(InBuf, ibs); InBuf:=nil; ibs:=0; ibp:=0;
   PsBuf:=0;
  end;
end;

Function PSeek;
var l: longint;
begin
 With Handles[F] do
  begin
   PsBuf:=PageSiz;
   CP:=Pos div PageSiz;
   if CP>=LP then begin InOutRes:=$19; exit end;
   if PFGetBuf(F) then PsBuf:=Pos mod PageSiz;
   l:=CP; l:=l*PageSiz;
   PSeek:=l + PsBuf;
  end;
end;

Procedure PFSeek;
var Pos : Longint;
    w   : word;
begin
 Flags:=Flags and not fCarry;
 With Handles[BX] do
  begin
   if not Used then begin Flags:=Flags or fCarry; AX:=06; Exit end;
   Pos:=CX;
   Pos:=(Pos shl 16) + DX;
   Pos:=PSeek(BX, Pos);
   w:=IOResult;
   if w<>0 then begin Flags:=Flags or fCarry; AX:=w end
           else begin DX:=Pos shr 16; AX:=Pos and $FFFF end;
  end;
end;

Function PRead;
 function ReadByte: byte;
  begin
   with Handles[F] do
    begin
     if CP<LP-1 then
      if PsBuf=PageSiz then
       begin
        Inc(CP);
        if not PFGetBuf(F) then begin InOutRes:=$1E; exit end;
       end else
     else
      if PsBuf=lps then begin InOutRes:=$1E; exit end;
     ReadByte:=OuBuf^[PsBuf];
     Inc(PsBuf);
    end;
  end;
var i: word;
    b: byte;
begin
 InOutRes:=0;
 for i:=0 to num-1 do
  begin
   b:=ReadByte;
   if InOutRes<>0 then begin dec(i); break end;
   PBuf(Buf)^[i]:=b;
  end;
 PRead:=i+1;
end;

Procedure PFRead;
begin
 Flags:=Flags and not fCarry;
 With Handles[BX] do
  begin
   if not Used
    then begin Flags:=Flags or fCarry; AX:=06 end
    else AX:=PRead(BX, CX, Ptr(DS,DX));
  end;
end;

Procedure PClose;
begin
 With Handles[F] do
  begin
   Close(H.F);
   if OuBuf <>nil then FreeMem(OuBuf,  PageSiz);        OuBuf :=nil;
{  if GlPoss<>nil then FreeMem(GlPoss, SizeOf(TPR)*LP); GlPoss:=nil;}
   Used:=false;
  end;
end;

Procedure PFClose;
begin
 Flags:=Flags and not fCarry;
 if not Handles[BX].Used then begin Flags:=Flags or fCarry; AX:=06; exit end;
 PClose(BX);
end;

end.