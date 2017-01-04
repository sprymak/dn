{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.04/DOS
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
//     "Based on TinyWeb Server by RIT Research Labs."
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

Unit U_DOS;
interface
uses u_crt, strings;

const uAnyFile=$37;
      uNormFile=$31;

var  FloppyNumber:byte;
     GamesAdapter:boolean;
     LPT:array[1..3]of word;{Base adress of LPT}
     COM:array[1..4]of word;{Base adress of COM}

     IOResultLFN: Integer;


type TVolumeLabel=String[11];
     TFullName = String;
     TName = String;
     TExten = String;

const
   {result for CreateDir}
   drOk = 0;
   drCantCreate = -1;
   drAlreadyExist = 1;

   function  DriveValid(Drive: Char): Boolean;
   function  GetVolumeLabel(Drive: Char):TVolumeLabel;
   function  WherePoint(S:String):byte;
   procedure InitTimer;
   function  GetTimer:longint;
   procedure GetTimerFull(var H,M,S,S100:word);
   procedure ms2hms(ms: Longint; var H, M, S, S100: Word);
   function  hms2ms(H, M, S, S100: Word): Longint;
   function  ms2hms_str(const ms:longint):string;
   function  ProgParam(par:string):boolean;
   function  ProgParamExt(par:string; var Value: String):boolean;
   function  NoParams: Boolean;
   function  CutName(name:string): TFullName;
   function  CutNameWithOutExt(name:string): TName;
   function  CutExt(name:string): TExten;
   function  CutPath(name:string): String;
   function  CreateDir(Dir: String): Integer; {создание влож каталогов за раз}
   function  PChar2Str(var Buffer; Size: Byte): String;
   function  GetFileSize(fName: String): Longint;
   function  RefreshPathName(Path: String): String;
   function  WinIsHere: Boolean;
   procedure MkDirLFN(S: string);

implementation
uses dos,u_hex;
var SaveTime: Longint;

{piwamoto.change.begin}
function WinIsHere: Boolean; assembler;
  asm
    mov ax, $1600
    int $2F
    xor bx, bx
    and al,$7f
    jz  @1
    inc bl
 @1:
    mov ax, bx
  end;
{piwamoto.change.end}

{-=DL=-}
procedure MkDirLFN(S: String);
var Ptr: array[0..255] of char;
    ssSeg, ssOfs: word;
begin
     StrPCopy( Ptr, S );
     ssOfs := Ofs(Ptr); ssSeg := Seg(Ptr);
     asm
        push ds
        mov ax, ssSeg
        mov ds, ax
        mov dx, ssOfs
        mov ax, 7139h
        int 21h
        pop ds
        jc @@1
        xor ax,ax
 @@1:
        mov IOResultLFN, ax
     end;
end;

    function DriveValid(Drive: Char): Boolean; assembler;
        asm
            MOV     AH,19H          { Save the current drive in BL }
            INT     21H
            MOV     BL,AL
            MOV     DL,Drive    { Select the given drive }
            SUB     DL,'A'
            MOV     AH,0EH
            INT     21H
            MOV     AH,19H              { Retrieve what DOS thinks is current }
            INT     21H
            MOV     CX,0                { Assume false }
            CMP     AL,DL               { Is the current drive the given drive? }
            JNE     @@1
            MOV     CX,1                { It is, so the drive is valid }
            MOV     DL,BL               { Restore the old drive }
            MOV     AH,0EH
            INT     21H
    @@1:    XCHG    AX,CX               { Put the return value into AX }
 end;

 function WherePoint(S:String):byte;
  begin
   WherePoint:=Pos('.',S);
  end;

 function GetVolumeLabel(Drive: Char):TVolumeLabel;
  var SR:SearchRec;
      i:byte;
  begin
    FindFirst(Drive+':\*.*',VolumeID,SR);
    if DosError<>0 then GetVolumeLabel:=''else
     begin
      i:=WherePoint(SR.Name);
      if i<>0 then GetVolumeLabel:=copy(SR.Name,1,i-1)+copy(SR.Name,i+1,length(SR.Name))
      else GetVolumeLabel:=SR.Name;
     end;
  end;

procedure InitTimer;
var H, M, S, S100: Word;
begin
  GetTime(H, M, S, S100);
  SaveTime := hms2ms(H, M, S, S100);
end;

function  GetTimer:longint;
var H, M, S, S100: Word;
begin
  GetTime(H, M, S, S100);
  GetTimer := hms2ms(H, M, S, S100) - SaveTime;
end;

procedure ms2hms(ms: Longint; var H, M, S, S100: Word);
begin
  H := Word(ms div 360000);
  Dec(ms, Longint(H) * 360000);
  M := Word(ms div 6000);
  Dec(ms, Longint(M) * 6000);
  S := Word(ms div 100);
  Dec(ms, Longint(S) * 100);
  S100 := Word(ms);
end;

function  hms2ms(H, M, S, S100: Word): Longint;
begin
  hms2ms := Longint(H) * 360000 + Longint(M) * 6000 + Longint(S) * 100 + Longint(S100);
end;

function  ms2hms_str(const ms: Longint): String;
var H, M, S, S100: Word;
    st, s2: String;
begin
  ms2hms(ms, H, M, S, S100);
  Str(H,s2); if H < 10 then s2 := '0' + s2; st := s2;
  Str(M,s2); if M < 10 then s2 := '0' + s2; st := st + ':' + s2;
  Str(S,s2); if S < 10 then s2 := '0' + s2; st := st + ':' + s2;
  ms2hms_str := st;
end;

procedure GetTimerFull(var H,M,S,S100:word);
 var GT:longint;
 begin
   GT:=GetTimer;
   H:=GT div 360000;
   M:=(GT-H*360000) div 6000;
   S:=(GT-H*360000-M*6000) div 100;
   S100:=(GT-H*360000-M*6000-S*100);
 end;

function  ProgParam(par:string):boolean;
var i:integer;
begin
   par:=UpperCase(par);
   ProgParam:=false;
   for i:=1 to ParamCount do if UpperCase(ParamStr(i))=par then ProgParam:=true;
end;

function  ProgParamExt( Par:string; var Value: String): Boolean;
var I: Integer;
    P: Integer;
begin
  ProgParamExt := False;
  Value := '';
  Par := UpperCase(Par);
  for I := 1 to ParamCount do begin
    P := Pos(Par, UpperCase(ParamStr(I)));
    if P = 1 then begin
      ProgParamExt := True;
      Value := Copy(ParamStr(I), Length(Par) + 1, Length(ParamStr(I)) - Length(Par));
      Break;
    end;
  end;
end;

function  NoParams: Boolean;
begin
  NoParams := (ParamCount = 0);
end;

function  CutName(name:string): TFullName;
 var p:byte;
 begin
   repeat
     p:=Pos('\',name);
     if p<>0 then name:=copy(name,p+1,byte(name[0])-p);
   until p=0;
   CutName:=name;
 end;

function  CutExt(name:string): TExten;
var i: integer;
begin
  CutExt := '';
  for i:= Length(name) downto 1 do
  begin
    if name[i] = '\' then Break else
    if name[i] = '.' then
    begin
      CutExt := Copy(name, i + 1, 255);
      Break;
    end;
  end;
end;

function  CutNameWithoutExt(name:string): TName;
 var name2:string;
     p:byte;
 begin
   name2:=CutName(name);
   p:=Pos('.',name2);
   if p<>0 then name2:=copy(name2,1,p-1) else;
   CutNameWithoutExt:=name2;
 end;

function  CutPath(Name: String): String;
var i: Byte;
begin
  CutPath := '';
  for i := Length(name)  downto 1 do
   if Name[i] = '\' then
   begin
     CutPath := Copy(Name, 1, i - 1);
     Break;
   end;
end;

function  CreateDir(Dir: String): Integer;
var OldDir, CurDir: String;
    I, N, L: Integer;
    Result: Integer;
label DoNext;
begin
  Result := drOk;
  GetDir(0, OldDir);
  {$I-}
  ChDir(Dir);
  {$I+}
  if IOResult <> 0 then begin
    L := Length(Dir);
    CurDir := Dir;
    if Dir[2] = ':' then I := 3 else I := 0;
DoNext:
    N := 1;
    while (I + N < L) and (Dir[I + N] <> '\') do Inc(N);
    I := I + N;
    if Dir[I] = '\' then Byte(CurDir[0]) := I - 1
    else Byte(CurDir[0]) := I;
    {$I-}
    if WinIsHere then MkDirLFN(CurDir) else MkDir(CurDir); {-=DL=-}
    {$I+}
    if IOResult = 0 then; {don't forget analyse IOResultLFN   -=DL=-}
    if (I < L) then Goto DoNext;
    {$I-}
    ChDir(Dir);
    {$I+}
    if IOResult <> 0 then Result := drCantCreate;
  end else Result := drAlreadyExist;
  {$I-}
  ChDir(OldDir);
  {$I+}
  CreateDir := Result;
end;

function  PChar2Str(var Buffer; Size: Byte): String;
var I, N: Integer;
    S: String;
Type TByteArray = array[1..65535] of byte;
begin
  N := 1;
  for I := 1 to Size do
   if TByteArray(Buffer)[i] = 0 then begin
     N := I;
     Break;
   end;
  Byte(S[0]) := N - 1;
  Move(TByteArray(Buffer)[1], S[1], N);
  PChar2Str := S;
end;

function  GetFileSize(fName: String): Longint;
var F: File;
begin
  Assign(F, fName);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult = 0 then begin
    GetFileSize := FileSize(F);
    Close(F);
  end else GetFileSize := 0;
end;

function  RefreshPathName(Path: String): String;
begin
  Path := Replace(Path, '\\', '\');
  if Path[Length(Path)] <> '\' then Path := Path + '\';
  RefreshPathName := Path;
end;

begin
   FloppyNumber:=((Mem[Seg0040:$0010] shl 8)shr 14)+1;
   GamesAdapter:=boolean(((Mem[Seg0040:$0010] shl 4)shr 15));
   COM[1]:=MemW[Seg0040:$0000];
   COM[2]:=MemW[Seg0040:$0002];
   COM[3]:=MemW[Seg0040:$0004];
   COM[4]:=MemW[Seg0040:$0006];
   LPT[1]:=MemW[Seg0040:$0008];
   LPT[2]:=MemW[Seg0040:$000A];
   LPT[3]:=MemW[Seg0040:$000C];
end.
