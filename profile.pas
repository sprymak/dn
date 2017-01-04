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
unit Profile;

{ unit Profile, Version 1.01.001, Copyright 1994,1997 by Matthias K"oppe

  $Id: profile.pas 1.3 1999/02/09 11:15:29 mkoeppe Exp $

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$G+,X+}

interface

function GetPrivateProfileInt(ApplicationName, KeyName: PChar;
  Default: Integer; FileName: PChar): Word;
function GetPrivateProfileString(ApplicationName, KeyName: PChar;
  Default: PChar; ReturnedString: PChar; Size: Integer;
  FileName: PChar): Integer;
function WritePrivateProfileString(ApplicationName, KeyName, Str,
  FileName: PChar): Boolean;
procedure CloseProfile;

implementation

uses {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF}, Objects;

{ The most expensive operation with buffered streams is seeking --
  especially seeking relatively since both GetPos and Seek call the
  dos move function, which takes much time.
  SeekRel provides a buffered seeking operation, which calls no DOS
  function if in buffer and one DOS function if out of buffer.
}
type
  PModBufStream = ^TModBufStream;
  TModBufStream = object(TBufStream)
    procedure SeekRel(Delta: Integer);
  End;

procedure TModBufStream.SeekRel; begin Seek(GetPos+Delta); end; {DataCompBoy}

{ Current parameters
}
const
  CurFile: PModBufStream = nil;
  CurFileName: PChar = nil;
  CurApp: LongInt = 0;
  CurAppName: PChar = nil;

procedure CloseFile;
Begin
  If CurFile <> nil then Begin
    Dispose(CurFile, Done);
    CurFile := nil
  End;
  StrDispose(CurFileName);
  CurFileName := nil;
  CurApp := 0;
  StrDispose(CurAppName);
  CurAppName := nil
End;

function OpenFile(FileName: PChar): Boolean;
var
  Res: Boolean;
Begin
  OpenFile := false;
  If (CurFileName = nil) or (FileName = nil) or
     (StrIComp(CurFileName, FileName) <> 0) then Begin
    CloseFile;
    If FileName = nil then Exit;
    CurFileName := StrNew(FileName);
    CurFile := New(PModBufStream, Init(StrPas(FileName), stOpen, 4096));
    Res := (CurFile <> nil) and (CurFile^.Status = 0);
    If not Res then CloseFile;
    OpenFile := Res
  End
  else OpenFile := true
End;

function CreateFile(FileName: PChar): Boolean;
var
  Res: Boolean;
Begin
  Createfile := false;
  If FileName = nil then Exit;
  CurFileName := StrNew(FileName);
  CurFile := New(PModBufStream, Init(StrPas(FileName), stCreate, 4096));
  Res := (CurFile <> nil) and (CurFile^.Status = 0);
  If not Res then CloseFile;
  CreateFile := Res
End;

procedure ReadLine(Buf: PChar);
var
  c: Char;
  c2: Char; {DataCompBoy}
  Count: Word;
Begin
  Count := 0;
  with CurFile^ do Begin
    Repeat
      Read(Buf[0], 1);
      c := Buf[0];
      Inc(Buf);
      If Count < 256 then
        Inc(Count);
    Until (c in [#13,#10]) or (Status <> 0);
    {-DataCompBoy-}
    If Status = 0 then begin
      If (c in [#13,#10]) and (Status = 0) then Read(c2, 1);
      If not (c2 in [#13,#10]) or (c2=c) then SeekRel(-1)
    end;
    {-DataCompBoy-}
    (Buf-1)[0] := #0;
  End
End;

function IsAppLine(Buf: PChar): Boolean;
Begin
  IsAppLine := (Buf[0] = '[') and ((StrEnd(Buf)-1)[0] = ']')
End;

function FindApplication(AppName: PChar): Boolean;
var
  Buf: array[0..255] of Char;
Begin
  FindApplication := false;
  If AppName = nil then Exit;
  If (CurAppName <> nil) and (StrIComp(CurAppName, AppName) = 0)
  then Begin
    CurFile^.Seek(CurApp);
    FindApplication := true;
  End
  else Begin
    CurFile^.Seek(0);
    Repeat
      ReadLine(Buf);
      If IsAppLine(Buf) then Begin
        (StrEnd(Buf)-1)[0] := #0;
        StrDispose(CurAppName);
        CurAppName := StrNew(Buf+1);
        CurApp := CurFile^.GetPos;
        If (CurAppName <> nil) and (StrIComp(CurAppName, AppName) = 0)
        then Begin
          FindApplication := true;
          CurFile^.Reset;
          CurApp := CurFile^.GetPos;
          Exit
        End
      End
    Until CurFile^.Status <> 0;
    CurFile^.Reset;
  End
End;

procedure AddApplication(AppName: PChar);
const
  _L: array[0..2] of Char = #13#10'[';
  _R: array[0..2] of Char = ']'#13#10;
Begin
  with CurFile^ do Begin
    Seek(GetSize);
    Write(_L, 3);
    Write(AppName[0], StrLen(AppName));
    Write(_R, 3);
    StrDispose(CurAppName);
    CurAppName := StrNew(AppName);
    CurApp := CurFile^.GetPos
  end
End;

function FirstInsignificant(Str: PChar): PChar;
var
  P: PChar;
Begin
  P := StrEnd(Str);
  If P = Str
  then FirstInsignificant := Str
  else Begin
    repeat
      Dec(P);
    until P[0] > ' ';
    FirstInsignificant := P+1
  End
End;

function FindKey(KeyName: PChar; Dest: PChar): Boolean;
var
  Buf: array[0..255] of Char;
  P: PChar;
  pos: LongInt;
Begin
  FindKey := false;
  If KeyName = nil then Exit;
  Repeat
    pos := CurFile^.GetPos;
    ReadLine(Buf);
    P := StrScan(Buf, '=');
    If P <> nil then Begin
      P[0] := #0;
      FirstInsignificant(Buf)[0] := #0;
      If StrIComp(Buf, KeyName) = 0 then Begin
        CurFile^.Reset;
        If Dest = nil
          then CurFile^.Seek(pos)
          else StrCopy(Dest, P+1);
        FindKey := true;
        Exit
      End;
    End;
  Until IsAppLine(Buf) or (CurFile^.Status <> 0);
  If CurFile^.Status <> 0 then CurFile^.Reset;
end;

procedure DeleteBuf(Dest, Source: LongInt);
var
  p, Count: LongInt;
  Buf: array[0..255] of Char;
Begin
  p := Dest;
  repeat
    If CurFile^.GetSize - Source >= 256
      then Count := 256
      else Count := CurFile^.GetSize - Source;
    CurFile^.Seek(Source);
    CurFile^.Read(Buf, Count);
    CurFile^.Seek(Dest);
    CurFile^.Write(Buf, Count);
    Inc(Source, Count);
    Inc(Dest, Count);
  until Source = Curfile^.GetSize;
  CurFile^.Truncate;
  CurFile^.Seek(p)
End;

procedure DeleteLine;
var
  pos: LongInt;
  Buf: array[0..255] of Char;
Begin
  pos := CurFile^.GetPos;
  ReadLine(Buf);
  If CurFile^.Status <> 0 then CurFile^.reset;
  DeleteBuf(pos, CurFile^.GetPos);
End;

procedure InsertLine(Size: Word);
var
  pos, Count, Source, Dest: LongInt;
  Buf: array[0..255] of Char;
Begin
  pos := CurFile^.GetPos;
  Source := CurFile^.GetSize;
  Dest := Source + Size;
  repeat
    If Source - pos >= 256
      then Count := 256
      else Count := Source - pos;
    Dec(Source, Count);
    Dec(Dest, Count);
    CurFile^.Seek(Source);
    CurFile^.Read(Buf, Count);
    CurFile^.Seek(Dest);
    CurFile^.Write(Buf, Count);
  until Source = pos;
  CurFile^.Seek(pos)
End;

function InQuotes(Str: PChar): Boolean;
var
  P: PChar;
Begin
  P := StrEnd(Str) - 1;
  InQuotes :=
    ((Str[0] = '"') and (P[0] = '"')) or
    ((Str[0] = '''') and (P[0] = ''''))
End;

function GetPrivateProfileString;
var
  Buf: array[0..255] of Char;
  P, Copy: PChar;
  Res: Boolean;
Begin
  Copy := Default;
  If OpenFile(FileName) and
     FindApplication(ApplicationName) then
    If KeyName = nil
    then Begin
      { list all keys in section }
      Copy := ReturnedString;
      Repeat
        ReadLine(Buf);
        Res := IsAppLine(Buf);
        If not Res and (Buf[0] <> ';') then Begin
          P := StrScan(Buf, '=');
          If P <> nil then Begin
            P[0] := #0;
            FirstInsignificant(Buf)[0] := #0;
            Copy := StrEnd(StrLCopy(Copy, Buf, Size-(Copy-ReturnedString)-1)) + 1
          End
        End
      Until Res or (CurFile^.Status <> 0);
      If CurFile^.Status <> 0 then CurFile^.Reset;
      Copy[0] := #0;
      GetPrivateProfileString := Copy-ReturnedString-1;
      Exit
    End else
      if FindKey(KeyName, Buf) then
      If InQuotes(Buf)
      then Begin
        (StrEnd(Buf)-1)[0] := #0;
        Copy := Buf + 1
      End else
        Copy := @Buf;
  StrLCopy(ReturnedString, Copy, Size);
  GetPrivateProfileString := StrLen(ReturnedString)
End;

function GetInt(Str: PChar): Word;
var
  Res: Word;
  E: Integer;
Begin
  { auch Hex erkennen (C-Format) }
  Val(Str, Res, E);
  If E = 1 then Res := 0 else
  If E <> 0 then Begin
    Str[E-1] := #0;
    Val(Str, Res, E)
  End;
  GetInt := Res
End;

function GetPrivateProfileInt;
var
  Buf: array[0..255] of Char;
Begin
  GetPrivateProfileInt := Default;
  If OpenFile(FileName) and
     FindApplication(ApplicationName) and FindKey(KeyName, Buf)
  then GetPrivateProfileInt := GetInt(Buf);
End;

function WritePrivateProfileString;
var
  Buf: array[0..255] of Char;
  Res: Boolean;
  p: LongInt;
begin
  If (OpenFile(FileName) or CreateFile(FileName)) and (ApplicationName <> nil)
  then Begin
    If not FindApplication(ApplicationName)
    then AddApplication(ApplicationName);
    If KeyName = nil
    then Begin
      CurFile^.Seek(CurApp);
      repeat
        p := CurFile^.GetPos;
        ReadLine(Buf);
        Res := IsAppLine(Buf) or (CurFile^.Status <> 0);
        If not Res and (Buf[0] <> ';') then
          DeleteBuf(p, CurFile^.GetPos);
      until Res;
      If CurFile^.Status <> 0 then CurFile^.Reset;
    End
    else Begin
      If FindKey(KeyName, nil) then DeleteLine else CurFile^.Seek(CurApp);
      If Str <> nil then Begin
        StrLCopy(Buf, KeyName, 256);
        StrLCat(Buf, '=', 256);
        StrLCat(Buf, Str, 256);
        StrLCat(Buf, #13#10, 256);
        InsertLine(StrLen(Buf));
        CurFile^.Write(Buf, StrLen(Buf))
      End
    End
  End
end;

procedure CloseProfile;
begin CloseFile end;

end.
