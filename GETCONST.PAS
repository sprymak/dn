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

unit
GetConst;

interface

uses
  Lfn, {DataCompBoy}
  Collect,
  Objects;

type
  PValuesHolder = ^TValuesHolder;
  PTypeHolder = ^TTypeHolder;
  PLngWord = ^TLngWord;
  TTypeMode = (tmConst, tmEnum);

  { Collection of PTypeHolders }
  TValuesHolder = object(TCollection)
    function GetType(Id: String): PTypeHolder;
    function GetValue(s: String; var Complete: boolean): word;
    procedure Show;
    end;

  { Collection of PLngWords }
  TTypeHolder = object(TSortedCollection)
    TypeID: String[10];
    Mode: TTypeMode;
    Constructor Init(Id: String; AMode: TTypeMode);
    function Compare(P1, P2: Pointer): integer; virtual;
    procedure Show;
    end;

  TLngWord = object(TObject)
    Name: String[30];
    l: word;
    Mark: byte;
    Constructor Init(AL: word; const AName: String);
    end;

procedure ProcessFile(const FileName: String; Types: PValuesHolder);

implementation

uses
  Dos,
  advance,
  advance1,
  advance2,
  advance3,
  Commands;

procedure Error(const s: String);
  begin
    Writeln(s);
    Halt(2);
  end;

function TValuesHolder.GetType;
  function LookForType(P: PTypeHolder): boolean;
    begin
      LookForType := P^.TypeID = Id;
    end;
  begin
    GetType := FirstThat(@LookForType);
  end;

function TValuesHolder.GetValue;
  function DoScanValue(P: PTypeHolder): boolean;
    function DoScan(P: PLngWord): boolean;
      begin
        if P^.Name = s then
          begin
            GetValue := P^.l;
            DoScan := True;
          end
        else
          DoScan := False
      end;
    begin
      DoScanValue := P^.FirstThat(@DoScan) <> nil;
    end;
  begin
    UpStr(s);
    Complete := FirstThat(@DoScanValue) <> nil;
  end;

procedure TTypeHolder.Show;
  procedure DoScan(P: PLngWord);
    begin
      Writeln(P^.Name, '=', P^.l);
    end;
  begin
    ForEach(@DoScan);
  end;

function TTypeHolder.Compare;
  begin
    if PLngWord(P1)^.Name > PLngWord(P2)^.Name then
      Compare := 1
    else if PLngWord(P1)^.Name < PLngWord(P2)^.Name then
      Compare := -1
    else
      Compare := 0;
  end;


procedure TValuesHolder.Show;
  procedure DoScan(P: PTypeHolder);
    begin
      Writeln('TYPE ', P^.TypeID);
      Writeln(Strg(#196, 5+Length(P^.TypeID)));
      P^.Show;
    end;
  begin
    ForEach(@DoScan);
  end;

Constructor TTypeHolder.Init;
  begin
    inherited Init(50, 50);
    TypeID := Copy(Id, 1, 10);
    Mode := AMode;
  end;

Constructor TLngWord.Init;
  begin
    Name := UpStrg(Copy(AName, 1, 30));
    l := word(AL);
    Mark := 0;
  end;

const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  Digits = ['0'..'9'];
  Hexs = ['0'..'9', 'A'..'F', 'a'..'f'];
  Blank = [#0..#32];

  {-DataCompBoy-}
procedure ProcessFile;
  type
    TParseMode = (pmSkip, pmComment, pmConst, pmStartType,
      pmSeekType, pmType);
  var
    F: lText;
    US, s, Id: String;
    Eq: integer;
    P: PTypeHolder;
    CurrentType: PTypeHolder;
    TypeCount: word;
    CmtMode, Mode: TParseMode;
    CmtB, CmtE: integer;
    CmtS: String[2];

  function CalcValue(var s: String): word;
    var
      W: word;
      V: String;
      ParseMode: (pmNone, pmAlpha, pmWord, pmHex);
      i: integer;
      Complete: boolean;
    label Loop;
    begin
      i := 0;
      W := 0;
      ParseMode := pmNone;
      while i < Length(s) do
        begin
          Inc(i);
Loop:
          case ParseMode of
            pmNone:
              if s[i] in Blank then
                continue
              else if s[i] in Digits then
                begin
                  ParseMode := pmWord;
                  W := 0;
                  goto Loop
                end
              else if s[i] = '$' then
                begin
                  ParseMode := pmHex;
                  W := 0;
                end
              else if s[i] in Alpha then
                begin
                  ParseMode := pmAlpha;
                  V := s[i];
                end
              else if s[i] = '+' then
                begin
                  s := Copy(s, i+1, MaxStringLength);
                  W := W+CalcValue(s);
                  break;
                end
              else if s[i] = '-' then
                begin
                  s := Copy(s, i+1, MaxStringLength);
                  W := W-CalcValue(s);
                  break;
                end;
            pmWord:
              if s[i] in Digits
              then
                W := W*10+Ord(s[i])-Ord('0')
              else
                break;
            pmHex:
              if s[i] in Hexs
              then
                W := W*16+Ord(UpCase(s[i]))-Ord('0')-7*byte(s[i] >
                  '9')
              else
                break;
            pmAlpha:
              if s[i] in (Alpha+Digits)
              then
                V := V+s[i]
              else
                begin
                  W := Types^.GetValue(V, Complete);
                  if not Complete then
                    begin
                      Writeln('Undefined variable ', V);
                    end
                  else
                    break;
                end;
          end { case };
        end { while };
      s := Copy(s, i, MaxStringLength);
      CalcValue := W;
    end { CalcValue };

  function TruncSpace(s: String): String;
    var
      i: byte;
    begin
      i := Length(s);
      while s[i] = ' ' do
        Dec(i);
      SetLength(s, i);
      TruncSpace := s
    end;

  function RemoveLeadSpaces(const s: String): String;
    var
      i: integer;
    begin
      i := 1;
      while (i <= Length(s)) and (s[i] = ' ') do
        Inc(i);
      RemoveLeadSpaces := Copy(s, i, MaxStringLength);
    end;

  function Ident(n: integer): String;
    var
      R: String;
      i: integer;
    begin
      i := n;
      if not (s[i] in Alpha) then
        begin
          Ident := '';
          exit;
        end;
      while (i <= Length(s)) and (s[i] in (Alpha+Digits)) do
        Inc(i);
      Dec(i);
      Ident := Copy(s, n, i-n+1);
      s := RemoveLeadSpaces(Copy(s, i+1, MaxStringLength));
      US := UpStrg(s);
    end;

  function LookForType(P: PTypeHolder): boolean;
    begin
      LookForType := (Mode = pmConst) and (P^.Mode = tmConst) and
      (Pos(Copy(Id, 1, 2)+#0, P^.TypeID) > 0)
      or
      (Mode = pmStartType) and (P^.Mode = tmEnum) and
      (UpStrg(Id) = UpStrg(P^.TypeID));
    end;
  {DataCompBoy
  function DelDblSpc( S: String ): String;
  begin
    DelDoubles( '  ', S );
    DelDblSpc := S;
  end;
}
  label
    Reparse, StartType;
  begin { ProcessFile }
    Mode := pmSkip;
    lAssignText(F, FileName);
    lResetText(F);
    while not Eof(F.t) do
      begin
        readln(F.t, s);
Reparse:
        s := RemoveLeadSpaces(TruncSpace(s));
        DelDoubles('  ', s);
        if s = '' then
          continue;
        US := UpStrg(s);

        CmtB := Pos('{', s);
        if CmtB > 0 then
          begin
            CmtS := '}';
            CmtE := Pos('}', s);
            if CmtE > 0 then
              begin
                Delete(s, CmtB, CmtE-CmtB+1);
                goto Reparse;
              end
            else
              begin
                Delete(s, CmtB, 1);
                CmtMode := Mode;
                Mode := pmComment;
                goto Reparse;
              end;
          end;
        CmtB := Pos('(*', s);
        if CmtB > 0 then
          begin
            CmtS := '*)';
            CmtE := Pos('*)', s);
            if CmtE > 0 then
              begin
                Delete(s, CmtB, CmtE-CmtB+2);
                goto Reparse;
              end
            else
              begin
                Delete(s, CmtB, 2);
                CmtMode := Mode;
                Mode := pmComment;
                goto Reparse;
              end;
          end;

        case Mode of
          pmComment:
            begin
              CmtE := Pos(CmtS, s);
              if CmtE > 0 then
                begin
                  Delete(s, 1, CmtE+Length(CmtS));
                  Mode := CmtMode;
                  goto Reparse;
                end;
            end;
          pmSkip:
            begin
              if Pos('CONST', US) = 1 then
                begin
                  Mode := pmConst;
                  Delete(s, 1, 5);
                  goto Reparse;
                end
              else if Pos('TYPE', US) = 1
              then
                goto StartType;
            end;
          pmConst:
            if Pos('TYPE', US) = 1 then
StartType:
                begin
                  Mode := pmStartType;
                  Delete(s, 1, 4);
                  goto Reparse;
                end
              else
              begin
                if Pos('CONST', US) = 1 then
                  begin
                    Delete(s, 1, 5);
                    goto Reparse;
                  end;
                Eq := Pos('=', s);
                if Eq = 0 then
                  begin
                    Mode := pmSkip;
                    continue;
                  end;
                Id := Ident(1);
                P := Types^.FirstThat(@LookForType);
                if P = nil then
                  continue;
                Eq := Pos('=', s);
                s := RemoveLeadSpaces(Copy(s, Eq+1, MaxStringLength));
                US := RemoveLeadSpaces(Copy(US, Eq+1,
                  MaxStringLength));
                P^.Insert(New(PLngWord, Init(CalcValue(US), Id)));
                US := RemoveLeadSpaces(US);
                if US[1] = ';' then
                  US := Copy(US, 2, MaxStringLength);
                Delete(s, 1, Length(s)-Length(US));
                goto Reparse;
              end;
          pmStartType:
            begin
              Id := Ident(1);
              CurrentType := Types^.FirstThat(@LookForType);
              if CurrentType = nil then
                begin
                  if Pos(')', s) > 0 then
                    while not Eof(F.t) do
                      if Pos(')', s) > 0
                      then
                        break
                      else
                        readln(F.t, s);
                  Mode := pmSeekType;
                end
              else
                begin
                  TypeCount := 0;
                  Mode := pmType;
                  Delete(s, 1, Length(Id));
                  goto Reparse;
                end
            end;
          pmSeekType:
            if Pos('CONST', US) = 1 then
              begin
                Mode := pmSkip;
                goto Reparse;
              end
            else if Pos('TYPE', US) = 1 then
              begin
                Mode := pmSkip;
                goto Reparse;
              end
            else
              begin
                Mode := pmStartType;
                goto Reparse;
              end;
          pmType:
            begin
              Id := Ident(1);
              if Id <> '' then
                begin
                  CurrentType^.Insert(New(PLngWord, Init(TypeCount, Id))
                    );
                  Inc(TypeCount);
                  if s[1] = ',' then
                    begin
                      Delete(s, 1, 1);
                      goto Reparse;
                    end;
                end;
              if Pos(')', s) > 0
              then
                Mode := pmSeekType
              else
                goto Reparse;
            end
        end {case};
      end;
    Close(F.t);
  end { ProcessFile };
{-DataCompBoy-}

end.
