{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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
{Written by Anton Fedorov aka DataCompBoy 18.07.2000}
{Interface part from LFN.PAS}

unit LFNVP;

interface

uses Dos, DnINI;

type
  lAPIType = (lDOS, lWIN95);  {API currently used}

  TSize = Comp; {64 bit integer type for file sizes}

   {Extended search structure to be used instead of SearchRec}
  lSearchRec = record
    SR: SearchRec;     {Basic field set (SR.Name - short name)}
    FileHandle: Word;  {Search handle, undefined in lDOS mode}
    FullSize: TSize; {True file size}
    LoCreationTime: Longint; {Time created (low-order byte)}
    HiCreationTime: Longint; {Time created (high-order byte)}
    LoLastAccessTime: Longint; {Time accessed (low-order byte)}
    HiLastAccessTime: Longint; {Time accessed (high-order byte)}
    LoLastModificationTime: Longint; {Time modified (low-order byte)}
    HiLastModificationTime: Longint; {Time modified (high-order byte)}
    FullName: string;  {True file name or short name if LFNs not available}
    { Other fields will be added later }
  end;

  TNameZ = Array[0..259] of Char;
  {The type for file names. (String is suitable for most purpuses)}

  lFile = record {Extended file record to be used instead of File}
    F: File;
    { Other fields will be added later }
  end;

  lText = record {Extended text file record to be used instead of Text}
    T: Text;
    { Other fields will be added later }
  end;

        {   Basic parameters   }
const
  lAPI = lWIN95;  {  Current API  }

  ltMod = 0;    {Store time modified}
  ltAcc = 1;    {Store time accessed}
  ltCre = 2;    {Store time created}
  LFNTimes: Byte = ltMod; { What time info to store in lSearchRec.SR.Time? }

  MaxPathLen: Byte = 255;  { Maximum name length for the present moment }

  LFNPresent: Boolean = true; { Are LFNs available }

  Const IllegalChars:String[15] = '<>|:'; { Characters invalid for short names }
      IllegalCharSet:Set Of Char = ['<','>','|',':']; { Characters invalid for short names }

  Const IllegalCharsDos:String[15] = ';,=+<>|"[]'; { Characters invalid for DOS names }
      IllegalCharSetDos:Set Of Char = [';',',','=','+','<','>','|','"','[',']']; { Characters invalid for DOS names }

           { Returns true if LFNs are unavailable near the given path }
function LFNDisabled(const Path: String): Boolean; inline; begin LFNDisabled := false {?} end;

       { File searching routines. lFindClose must be called /in all cases/ }
procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
procedure lFindNext(var R: lSearchRec);
procedure lFindClose(var R: lSearchRec);

        { Name retrieval functions }
{$IFNDEF OS2}
procedure lGetShortFileName(const Name: String; var ShortName: String);
function lfGetShortFileName(const Name: String): String;
procedure lGetLongFileName(const Name: String; var LongName: String);
function lfGetLongFileName(const Name: String): String;
{$ENDIF}

        { Name correction routine }
procedure lTrueName(const Name: String; var S: String);


   { Basic file operation routines. To use IO functions from standard units,
              specify lFile.F or lText.T }

procedure lAssignFile(var F: lFile; const Name: String);
procedure lAssignText(var T: lText; const Name: String);
procedure lResetFile(var F: lFile; RecSize: Word);
procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
procedure lRewriteFile(var F: lFile; RecSize: Word);
procedure lResetText(var F: lText); inline; begin Reset(F.T) end;
procedure lResetTextReadOnly(var F: lText);
procedure lRewriteText(var F: lText);
procedure lAppendText(var T: lText); inline; begin Append(T.T); end;
procedure lEraseFile(var F: lFile); inline; begin Erase(F.F); end;
procedure lEraseText(var T: lText); inline; begin Erase(T.T); end;
procedure lRenameFile(var F: lFile; const NewName: String);
procedure lRenameText(var T: lText; const NewName: String);
function  lFileNameOf(var lF: lFile): string;
function  lTextNameOf(var lT: lText): string;

                 { Extended file attributes manipulation }
procedure lGetFAttr(var F: lFile; var Attr: Word);
procedure lSetFAttr(var F: lFile; Attr: Word);
procedure lGetTAttr(var T: lText; var Attr: Word);
procedure lSetTAttr(var T: lText; Attr: Word);

                           { Directory manipulation }
procedure lMkDir(const Path: String);
procedure lRmDir(const Path: String);
procedure lChDir(const Path: String);
procedure lGetDir(D: Byte; var Path: String);

         { Name expansion and splitting }
function lFExpand(const Path: String): String;
procedure lFSplit(const Path: String; var Dir, Name, Ext: String);

implementation

uses Windows;

 Function StrPas(S: Array Of Char): String;
  var ss: string;
      i: word;
  Begin
   ss:='';
   For i:=Low(S) to High(S) do
    If (i<255) and (s[i]<>#0)
     then ss:=ss+s[i]
     else break;
   StrPas:=ss;
  End;

procedure NameToNameZ(const Name: String; var NameZ: TNameZ);
begin
  Move(Name[1], NameZ, Length(Name));
  NameZ[Length(Name)] := #0;
end;

procedure CheckColonAndSlash(const Name: String; var S: String);
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if (ColonPos > 2) and (Name[2] = ':') then
  begin
    Delete(S, 1, ColonPos - 1);
    S := Name[1] + S;
  end;

  if Name[Length(Name)] <> '\' then
    while S[Length(S)] = '\' do Dec(S[0])
  else if (Name[Length(Name)] = '\') and
    (S[Length(S)] <> '\') and (Length(S) < 255) then
  begin
    Inc(S[0]);
    S[Length(S)] := '\';
  end;
end;

(*
 Offset  Size    Description
  00h    DWORD   file attributes
                 bits 0-6 standard DOS attributes
                 bit 8: temporary file
  04h    QWORD   file creation time
                 (number of 100ns intervals since 1/1/1601)
  0Ch    QWORD   last access time
  14h    QWORD   last modification time
  1Ch    DWORD   file size (high 32 bits)
  20h    DWORD   file size (low 32 bits)
  24h  8 BYTEs   reserved
  2Ch 260 BYTEs  ASCIZ full filename
 130h 14 BYTEs   ASCIZ short filename (for backward compatibility)
*)

procedure CorrectSearchRec(var R: lSearchRec);
var NZ,NZ2: TNameZ;
begin
  R.FullName := R.SR.Name;
  NameToNameZ(R.SR.Name, NZ2);
  GetShortPathName(NZ2, NZ, SizeOf(NZ));
  R.SR.Name := StrPas(NZ);
  R.LoCreationTime:= R.SR.Time;
  R.HiCreationTime:= 0;
  R.LoLastAccessTime:= R.SR.Time;
  R.HiLastAccessTime:= 0;
  R.LoLastModificationTime:= R.SR.Time;
  R.HiLastModificationTime:= 0;
  R.FullSize:=R.SR.Size;
end;

procedure lFindFirst(const Path: String; Attr: Word; var R: lSearchRec);
begin
  R.FullName := '';
  FindFirst(Path, Attr, R.SR);
  CorrectSearchRec(R);
end;

procedure lFindNext(var R: lSearchRec);
begin
  R.FullName := '';
  FindNext(R.SR);
  CorrectSearchRec(R);
end;

procedure lFindClose(var R: lSearchRec);
begin
 DOS.FindClose(R.SR);
end;

{$IFNDEF OS2}
procedure lGetShortFileName(const Name: String; var ShortName: String);
var NZ, NZ2: TNameZ;
begin
 if LFNDisabled(Name)
  then ShortName := Name
  else begin
        NameToNameZ(Name, NZ);
        GetShortPathName(@NZ2, @NZ, SizeOf(NZ));
        ShortName := StrPas(NZ);
       end;
end;

function lfGetShortFileName(const Name: String): String;
 var s: String;
begin
  lGetShortFileName(Name, s);
 lfGetShortFileName:=s;
end;

procedure lGetLongFileName(const Name: String; var LongName: String);
var NZ, NZ2: TNameZ; P: PChar;
begin
{ if LFNDisabled(Name)
  then }LongName := Name
{  else begin
        NameToNameZ(Name, NZ);
        GetFullPathName(@NZ2, SizeOf(NZ), NZ, P);
        LongName := StrPas(NZ);
       end;} {?}
end;

function lfGetLongFileName(const Name: String): String;
 var s: String;
begin
  lGetLongFileName(Name, s);
 lfGetLongFileName:=s;
end;
{$ENDIF}

procedure lTrueName(const Name: String; var S: String);
 begin S:=FExpand(Name); end;

procedure lFSplit(const Path: String; var Dir, Name, Ext: String);
var
  HasColon: Boolean;
  DotPos, SlashPos, B: Byte;
  D:String;
  N:String;
  E:String;
begin
      begin
        Dir := '';
        Name := '';
        Ext := '';
        DotPos := 0;
        SlashPos := 0;
        HasColon := (Length(Path) > 1) and (Path[2] = ':');

        for B := Length(Path) downto 1 do
        begin
          if (Path[B] = '.') and (DotPos = 0) and (B>1) then DotPos := B;
          if ((Path[B] = ':') or (Path[B] = '\')) and (SlashPos = 0) and
             ((B > 2) or not HasColon) then SlashPos := B;
          if (DotPos <> 0) and (SlashPos <> 0) then Break;
        end;

        if DotPos + SlashPos = 0 then
          if HasColon then Dir := Path
          else Name := Path
        else
        begin
          if DotPos > SlashPos then Ext := Copy(Path, DotPos, 255)
          else DotPos := 255;

          if SlashPos <> 0 then Dir := Copy(Path, 1, SlashPos);

          Name := Copy(Path, SlashPos + 1, DotPos - SlashPos - 1);
        end;
      end;
end;

function lFileNameOf(var lF: lFile): string;
begin
  lFileNameOf := StrPas(FileRec(lF.F).Name);
end;

function lTextNameOf(var lT: lText): string;
begin
  lTextNameOf := StrPas(TextRec(lT.T).Name);
end;

procedure lResetFileReadOnly(var F: lFile; RecSize: Word);
var SaveMode: Byte;
begin SaveMode := FileMode;
      FileMode := 64;
      lResetFile(F, RecSize);
      FileMode := SaveMode;
end;

procedure lRewriteFile(var F: lFile; RecSize: Word);
var OldMode: byte;
begin OldMode:= FileMode;
      FileMode:=FileMode and $FC or 2;
      Rewrite(F.F, RecSize);
      FileMode:=OldMode;
end;

procedure lResetTextReadOnly(var F: lText);
var
  SaveMode: Byte;
begin
 SaveMode := FileMode;
 FileMode := 64;
 lResetText(F);
 FileMode := SaveMode;
end;

procedure lRewriteText(var F: lText);
var OldMode: byte;
begin OldMode:= FileMode;
      FileMode:=FileMode and $FC or 2;
      Rewrite(F.T);
      FileMode:=OldMode;
end;

{ Inline functions, which temporary compiled as not inline, because VP are   }
{ crased on compiling 8(                                                     }

function lFExpand(const Path: String): String; begin lFExpand := FExpand(Path); end;
procedure lAssignFile(var F: lFile; const Name: String); begin Assign(F.F, Name); end;
procedure lAssignText(var T: lText; const Name: String); begin Assign(T.T, Name); end;
procedure lResetFile(var F: lFile; RecSize: Word); begin Reset(F.F, RecSize); end;
procedure lRenameFile(var F: lFile; const NewName: String); begin Rename(F.F, NewName); end;
procedure lRenameText(var T: lText; const NewName: String); begin Rename(T.T, NewName); end;
procedure lGetFAttr(var F: lFile; var Attr: Word); begin DOS.GetFAttr(F.F, Attr); end;
procedure lSetFAttr(var F: lFile; Attr: Word);     begin DOS.SetFAttr(F.F, Attr); end;
procedure lGetTAttr(var T: lText; var Attr: Word); begin DOS.GetFAttr(T.T, Attr); end;
procedure lSetTAttr(var T: lText; Attr: Word);     begin DOS.SetFAttr(T.T, Attr); end;
procedure lMkDir(const Path: String); begin MkDir(Path); end;
procedure lRmDir(const Path: String); begin RmDir(Path); end;
procedure lChDir(const Path: String); begin ChDir(Path); end;
procedure lGetDir(D: Byte; var Path: String); begin GetDir(D, Path); end;

end.
