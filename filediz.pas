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
  FilesCol, Startup, advance1, advance2, advance, Objects, Lfn, Dos,
  Messages, DNApp, Commands, Drives, advance3;

function GetPossibleDizOwner(n: integer): String;
function GetDizOwner(const Path, LastOwner: String; Add: boolean):
  String;
function CalcDPath(P: PDiz; Owen: PString): String;
procedure ReplaceDiz(const DPath, Name: String;
  ANewName: PString; ANewDescription: PString);
{When (ANewName= nil) and (ANewDesc= nil) - erasing a string}
{When (ANewName= nil) and (ANewDesc<>nil) - change a description}
{When (ANewName<>nil) and (ANewDesc= nil) - change a name}
{When (ANewName<>nil) and (ANewDesc<>nil) - change a name & desc}
procedure DeleteDiz(const DPath, Name: String); {DataCompBoy}
function GetDiz(const DPath: String; var Line: longInt; const Name:
    String): String; {DataCompBoy}
procedure SetDescription(PF: PFileRec; DizOwner: String);

implementation

function GetPossibleDizOwner(n: integer): String;
  var
    DIZ: String;
    i: integer;
  begin
    GetPossibleDizOwner := '';
    DIZ := FMSetup.DIZ;
    while (n > 0) and (DIZ <> '') do
      begin
        i := PosChar(';', DIZ);
        if i = 0 then
          i := Length(DIZ)+1;
        if CorrectFile(Copy(DIZ, 1, i-1)) then
          begin
            Dec(n);
            if n = 0 then
              begin
                GetPossibleDizOwner := Copy(DIZ, 1, i-1);
                exit;
              end;
          end;
        Delete(DIZ, 1, i);
      end;
  end { GetPossibleDizOwner };

{-DataCompBoy: Descriptions remain with short names!!!-}
{-DataCompBoy-}
function GetDizOwner;
  var
    SR: lSearchRec;
    i: integer;
    lAdd: boolean;
    DEr: boolean;

  procedure Prepare;
    var
      F: lFile;
      C: Char;
      i, j: word;
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
      BlockRead(F.F, C, 1, j);
      if (IOResult = 0) and (C <> #13) and (C <> #10) then
        begin
          C := #13;
          BlockWrite(F.F, C, 1, j);
          C := #10;
          BlockWrite(F.F, C, 1, j);
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
    i := 1;
    if LastOwner = '' then
      GetDizOwner := ''
    else
      begin
        GetDizOwner := MakeNormName(Path, LastOwner);
        lFindFirst(MakeNormName(Path, LastOwner), Archive+ReadOnly+
          Hidden+SysFile, SR);
        DEr := (DOSError <> 0);
        lFindClose(SR);
        if not DEr then
          begin
            Prepare;
            exit;
          end;
        i := 1;
      end;
    repeat
      SR.FullName := GetPossibleDizOwner(i);
      Inc(i);
      if SR.FullName = '' then
        exit;
      lFindFirst(MakeNormName(Path, SR.FullName), Archive+ReadOnly+
        Hidden+SysFile, SR);
      DEr := (DOSError <> 0);
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
procedure ReplaceT(P: PTextReader; var F: lText; Del: boolean);
  var
    i: integer;
    FName: String;
    Attr: word;
  begin
    FName := P^.FileName;
    Dispose(P, Done);
    ClrIO;
    Close(F.t);
    ClrIO;
    Attr := GetFileAttr(FName);
    ClrIO;
    EraseByName(FName);
    i := IOResult;
    ClrIO;
    if not Del then
      begin
        lRenameText(F, FName);
        if (IOResult <> 0) then
          begin
            if (i <> 0) then
              Erase(F.t);
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
    k: byte;
  begin
    if (PF = nil) then
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
    if (PF^.DIZ = nil) or (PF^.DIZ^.DIZ = nil) then
      DIZ := ''
    else
      DIZ := PF^.DIZ^.DIZ^;
    k := PosChar(#20, DIZ);
      {JO: проверяем на наличие "дочитанных" кусков в описании}
    if k > 0 then
      SetLength(DIZ, k-1);
    if BigInputBox(GetString(dlEditDesc), GetString(dl_D_escription),
        DIZ, 255, hsEditDesc) <> cmOK
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
    i, j: integer;
    zz: boolean;
    s, NewName: String;
    WasFilesInDIZ: boolean;
    FNd: boolean;
    k: byte;
  begin
    if ANewName = nil then
      NewName := SquashesName(Name)
    else
      NewName := SquashesName(ANewName^);
    if (ANewDescription <> nil) then
      begin
        k := PosChar(#20, ANewDescription^);
        if k > 0 then
          SetLength(ANewDescription^, k-1);
      end;
    F1 := New(PTextReader, Init(DPath));
    if F1 = nil then
      begin
        if (ANewDescription <> nil) and
          (ANewDescription^ <> '')
        then
          begin
            ClrIO;
            lAssignText(F2, DPath);
            lRewriteText(F2);
            if Abort then
              begin
                Close(F2.t);
                exit;
              end;
            if IOResult <> 0 then{ begin CantWrite(DPath); }
              exit; { end; }
            Writeln(F2.t, NewName+' '+ANewDescription^);
            Close(F2.t);
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
      s := F1^.GetStr;
      if s = '' then
        continue;
      if not (s[1] in [' ', #9]) then
        begin
          zz := False;
          for i := 1 to Length(s) do
            if (s[i] in [' ', #9]) and not zz
            then
              break
            else if s[i] = '"' then
              zz := not zz;
          if s[i] in [' ', #9] then
            Dec(i);
          j := i+1;
          while (s[j] in [' ', #9]) do
            Inc(j);

          if UpStrg(Copy(s, 1, i)) = UpStrg(NewName) then
              {JO: было UpStrg(Name), что приводило к потере}
            begin{    описаний у исходного файла при копировании}
              FNd := True;
                {    файла в собственный каталог с другим именем}

              if ((ANewDescription = nil) or (ANewDescription^ = ''))
                  and
                (ANewName = nil) and
                (FMSetup.Options and fmoPreserveDesc = 0)
              then
                begin{delete multiline description}{piwamoto}
                  repeat
                    s := F1^.GetStr;
                  until (s[1] <> ' ') or F1^.Eof or (IOResult <> 0);
                  if not F1^.Eof then
                    begin
                      WasFilesInDIZ := True;
                      Writeln(F2.t, s);
                    end;
                  continue;
                end;

              Delete(s, 1, j-1);

              if ANewDescription <> nil
              then
                s := NewName+' '+ANewDescription^
              else
                s := NewName+' '+s;

            end;
        end;
      WasFilesInDIZ := True;
      Writeln(F2.t, s);
    until F1^.Eof or (IOResult <> 0);
    if not FNd then
      begin
        if ANewDescription <> nil
        then
          Writeln(F2.t, NewName+' '+ANewDescription^);
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

function GetDiz(const DPath: String; var Line: longInt; const Name:
    String): String; {DataCompBoy}
  var
    F1: PTextReader;
    s: String;
    zz: boolean;
    i, j: integer;
  begin
    GetDiz := '';
    Line := 0;
    F1 := New(PTextReader, Init(DPath));
    if F1 = nil then
      exit;
    repeat
      s := F1^.GetStr;
      Inc(Line);
      if s = '' then
        continue;
      if not (s[1] in [' ', #9]) then
        begin
          zz := False;
          for i := 1 to Length(s) do
            if (s[i] in [' ', #9]) and not zz
            then
              break
            else if s[i] = '"' then
              zz := not zz;
          if s[i] in [' ', #9] then
            Dec(i);
          j := i+1;
          while (s[j] in [' ', #9]) do
            Inc(j);

          if (UpStrg(Copy(s, 1, i)) = SquashesName(UpStrg(Name)))
          then
            begin
              Delete(s, 1, j-1);
              while s[1] in [' ', #9] do
                Delete(s, 1, 1); {DelFC(S);}
              GetDiz := s;
              break;
            end;
        end;
    until F1^.Eof or (IOResult <> 0);
    Dispose(F1, Done);
  end { GetDiz };
{-DataCompBoy-}
function CalcDPath(P: PDiz; Owen: PString): String;
  var
    i: integer;
    DPath: String;
    SR: lSearchRec;
  begin
    if (P = nil) or (P^.Owner = nil) then
      begin
        for i := 1 to 128 do
          begin
            DPath := GetPossibleDizOwner(i);
            if DPath = '' then
              exit;
            DPath := MakeNormName(CnvString(Owen), DPath);
            lFindFirst(DPath, Archive+ReadOnly+Hidden, SR);
            lFindClose(SR);
            if DOSError = 0 then
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
