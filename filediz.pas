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

unit Filediz;

interface

uses
  Files,
  FilesCol, Startup, advance1, advance2, advance, Defines, Objects2,
   Lfn, Dos,
  Messages, DNApp, Commands, Drives, advance3
  ;

function GetPossibleDizOwner(N: integer): String;
function GetDizOwner(const Path, LastOwner: String; Add: Boolean): String;
function CalcDPath(P: PDiz; Owen: PString): String;
procedure ReplaceDiz(const DPath, Name: String;
    ANewName: PString; ANewDescription: PString);
{When (ANewName= nil) and (ANewDesc= nil) - erasing a string}
{When (ANewName= nil) and (ANewDesc<>nil) - change a description}
{When (ANewName<>nil) and (ANewDesc= nil) - change a name}
{When (ANewName<>nil) and (ANewDesc<>nil) - change a name & desc}
procedure DeleteDiz(const DPath, Name: String); {DataCompBoy}
function GetDiz(const DPath: String; var Line: LongInt;
     const Name: String): String; {DataCompBoy}
procedure SetDescription(PF: PFileRec; DizOwner: String);

implementation

function GetPossibleDizOwner(N: integer): String;
  var
    DIZ: String;
    I: integer;
  begin
  GetPossibleDizOwner := '';
  DIZ := FMSetup.DIZ;
  while (N > 0) and (DIZ <> '') do
    begin
    I := PosChar(';', DIZ);
    if I = 0 then
      I := Length(DIZ)+1;
    if CorrectFile(Copy(DIZ, 1, I-1)) then
      begin
      Dec(N);
      if N = 0 then
        begin
        GetPossibleDizOwner := Copy(DIZ, 1, I-1);
        exit;
        end;
      end;
    Delete(DIZ, 1, I);
    end;
  end { GetPossibleDizOwner };

{-DataCompBoy: Descriptions remain with short names!!!-}
{-DataCompBoy-}
function GetDizOwner;
  var
    SR: lSearchRec;
    I: integer;
    lAdd: Boolean;
    DEr: Boolean;

  procedure Prepare;
    var
      F: lFile;
      C: Char;
      I, J: word;
    begin
    if lAdd then
      exit;
    lAssignFile(F, MakeNormName(Path, SR.FullName));
    ClrIO;
    FileMode := $42;
    lResetFile(F, 1);
    if IOResult <> 0 then
      exit;
    System.Seek(F.F, FileSize(F.F)-1);
    BlockRead(F.F, C, 1, J);
    if  (IOResult = 0) and (C <> #13) and (C <> #10) then
      begin
      C := #13;
      BlockWrite(F.F, C, 1, J);
      C := #10;
      BlockWrite(F.F, C, 1, J);
      end;
    ClrIO;
    Close(F.F);
    ClrIO;
    end { Prepare };

  begin { GetDizOwner }
  lAdd := Add;
  SR.SR.Name := LastOwner;
  SR.FullName := LastOwner;
  Add := False;
  I := 1;
  if LastOwner = '' then
    GetDizOwner := ''
  else
    begin
    GetDizOwner := MakeNormName(Path, LastOwner);
    lFindFirst(MakeNormName(Path, LastOwner),
       Archive+ReadOnly+Hidden+SysFile, SR);
    DEr := (DosError <> 0);
    lFindClose(SR);
    if not DEr then
      begin
      Prepare;
      exit;
      end;
    I := 1;
    end;
  repeat
    SR.FullName := GetPossibleDizOwner(I);
    Inc(I);
    if SR.FullName = '' then
      exit;
    lFindFirst(MakeNormName(Path, SR.FullName),
       Archive+ReadOnly+Hidden+SysFile, SR);
    DEr := (DosError <> 0);
    lFindClose(SR);
    if not DEr then
      begin
      Prepare;
      GetDizOwner := MakeNormName(Path, SR.FullName);
      exit
      end;
  until False;
  end { GetDizOwner };
{-DataCompBoy-}

{-DataCompBoy-}
procedure ReplaceT(P: PTextReader; var F: lText; Del: Boolean);
  var
    I: integer;
    FName: String;
    Attr: word;
  begin
  FName := P^.FileName;
  Dispose(P, Done);
  ClrIO;
  Close(F.T);
  ClrIO;
  Attr := GetFileAttr(FName);
  ClrIO;
  EraseByName(FName);
  I := IOResult;
  ClrIO;
  if not Del then
    begin
    lRenameText(F, FName);
    if  (IOResult <> 0) then
      begin
      if  (I <> 0) then
        Erase(F.T);
      CantWrite(FName);
      end
    else
      SetFileAttr(FName, Attr);
    end
  else
    lEraseText(F);
  ClrIO;
  end { ReplaceT };
{-DataCompBoy-}

{-DataCompBoy-}
procedure SetDescription(PF: PFileRec; DizOwner: String);
  var
    DIZ: String;
    K: Byte;
  begin
  if  (PF = nil) then
    exit;
  if DizOwner = '' then
    begin
    DIZ := GetPossibleDizOwner(1);
    if DIZ = '' then
      begin
      MessageBox(GetString(dlNoPossibleName), nil, mfError);
      exit;
      end;
    DizOwner := DIZ;
    end
  else
    DizOwner := GetName(DizOwner);
  DizOwner := GetDizOwner(PF^.Owner^, DizOwner, False);
  if  (PF^.DIZ = nil) or (PF^.DIZ^.DIZ = nil) then
    DIZ := ''
  else
    DIZ := PF^.DIZ^.DIZ^;
  K := PosChar(#20, DIZ);
  {JO: проверяем на наличие "дочитанных" кусков в описании}
  if K > 0 then
    SetLength(DIZ, K-1);
  if BigInputBox(GetString(dlEditDesc), GetString(dl_D_escription), DIZ,
       255, hsEditDesc) <> cmOK
  then
    exit;
  DelLeft(DIZ);
  {$IFNDEF OS2}
  if DnIni.DescrByShortNames then
    ReplaceDiz(DizOwner, PF^.FlName[False], nil, @DIZ)
  else
    {$ENDIF}
    ReplaceDiz(DizOwner, PF^.FlName[True], nil, @DIZ);
  { if IOResult <> 0 then CantWrite(DIZOwner); }
  if not DnIni.AutoRefreshPanels then
    RereadDirectory(PF^.Owner^);
  end { SetDescription };
{-DataCompBoy-}

{-DataCompBoy-}
procedure ReplaceDiz(const DPath, Name: String;
    ANewName: PString; ANewDescription: PString);
  {When (ANewName= nil) and (ANewDesc= nil) - erasing a string}
  {When (ANewName= nil) and (ANewDesc<>nil) - change a description}
  {When (ANewName<>nil) and (ANewDesc= nil) - change a name}
  {When (ANewName<>nil) and (ANewDesc<>nil) - change a name & desc}
  var
    F1: PTextReader;
    F2: lText;
    I, j: integer;
    zz: Boolean;
    S, NewName: String;
    WasFilesInDIZ: Boolean;
    FNd: Boolean;
    K: Byte;
  begin
  if ANewName = nil then
    NewName := SquashesName(Name)
  else
    NewName := SquashesName(ANewName^);
  if  (ANewDescription <> nil) then
    begin
    K := PosChar(#20, ANewDescription^);
    if K > 0 then
      SetLength(ANewDescription^, K-1);
    end;
  F1 := New(PTextReader, Init(DPath));
  if F1 = nil then
    begin
    if  (ANewDescription <> nil) and
        (ANewDescription^ <> '')
    then
      begin
      ClrIO;
      lAssignText(F2, DPath);
      lRewriteText(F2);
      if Abort then
        begin
        Close(F2.T);
        exit;
        end;
      if IOResult <> 0 then
        { begin CantWrite(DPath); }exit; { end; }
      Writeln(F2.T, NewName+' '+ANewDescription^);
      Close(F2.T);
      end;
    exit;
    end;
  lAssignText(F2, GetPath(DPath)+'$DN'+ItoS(DNNumber)+'$.DIZ');
  lRewriteText(F2);
  if IOResult <> 0 then
    begin
    Dispose(F1, Done);
    exit;
    end;
  FNd := False;
  WasFilesInDIZ := False;
  repeat
    S := F1^.GetStr;
    if S = '' then
      continue;
    if not (S[1] in [' ', #9]) then
      begin
      zz := False;
      for I := 1 to Length(S) do
        if  (S[I] in [' ', #9]) and not zz
        then
          break
        else if S[I] = '"' then
          zz := not zz;
      if S[I] in [' ', #9] then
        Dec(I);
      j := I+1;
      while (S[j] in [' ', #9]) do
        Inc(j);

      if UpStrg(Copy(S, 1, I)) = UpStrg(NewName) then
        {JO: было UpStrg(Name), что приводило к потере}
        begin {    описаний у исходного файла при копировании}
        FNd := True; {    файла в собственный каталог с другим именем}

        if  ( (ANewDescription = nil) or (ANewDescription^ = '')) and
            (ANewName = nil) and
            (FMSetup.Options and fmoPreserveDesc = 0)
        then
          begin {delete multiline description}
          {piwamoto}
          repeat
            S := F1^.GetStr;
          until (S[1] <> ' ') or F1^.Eof or (IOResult <> 0);
          if not F1^.Eof then
            begin
            WasFilesInDIZ := True;
            Writeln(F2.T, S);
            end;
          continue;
          end;

        Delete(S, 1, j-1);

        if ANewDescription <> nil
        then
          S := NewName+' '+ANewDescription^
        else
          S := NewName+' '+S;

        end;
      end;
    WasFilesInDIZ := True;
    Writeln(F2.T, S);
  until F1^.Eof or (IOResult <> 0);
  if not FNd then
    begin
    if ANewDescription <> nil
    then
      Writeln(F2.T, NewName+' '+ANewDescription^);
    WasFilesInDIZ := True;
    end;
  ReplaceT(F1, F2, (not WasFilesInDIZ) and (FMSetup.Options and
       fmoKillContainer <> 0));
  end { ReplaceDiz };
{-DataCompBoy-}

procedure DeleteDiz(const DPath, Name: String);
  begin
  ReplaceDiz(DPath, Name, nil, nil);
  end;

function GetDiz(const DPath: String; var Line: LongInt;
     const Name: String): String; {DataCompBoy}
  var
    F1: PTextReader;
    S: String;
    zz: Boolean;
    i, j: integer;
  begin
  GetDiz := '';
  Line := 0;
  F1 := New(PTextReader, Init(DPath));
  if F1 = nil then
    exit;
  repeat
    S := F1^.GetStr;
    Inc(Line);
    if S = '' then
      continue;
    if not (S[1] in [' ', #9]) then
      begin
      zz := False;
      for i := 1 to Length(S) do
        if  (S[i] in [' ', #9]) and not zz
        then
          break
        else if S[i] = '"' then
          zz := not zz;
      if S[i] in [' ', #9] then
        Dec(i);
      j := i+1;
      while (S[j] in [' ', #9]) do
        Inc(j);

      if  (UpStrg(Copy(S, 1, i)) = SquashesName(UpStrg(Name)))
      then
        begin
        Delete(S, 1, j-1);
        while S[1] in [' ', #9] do
          Delete(S, 1, 1); {DelFC(S);}
        GetDiz := S;
        break;
        end;
      end;
  until F1^.Eof or (IOResult <> 0);
  Dispose(F1, Done);
  end { GetDiz };
{-DataCompBoy-}
function CalcDPath(P: PDiz; Owen: PString): String;
  var
    I: integer;
    DPath: String;
    SR: lSearchRec;
  begin
  if  (P = nil) or (P^.Owner = nil) then
    begin
    for I := 1 to 128 do
      begin
      DPath := GetPossibleDizOwner(I);
      if DPath = '' then
        exit;
      DPath := MakeNormName(CnvString(Owen), DPath);
      lFindFirst(DPath, Archive+ReadOnly+Hidden, SR);
      lFindClose(SR);
      if DosError = 0 then
        break;
      end;
    end
  else
    DPath := P^.Owner^;
  CalcDPath := DPath;
  {lFindClose(SR);}
  end { CalcDPath };
{-DataCompBoy-}

end.
