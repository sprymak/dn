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
unit DefColl;

interface

uses
  Collect, Lfn, advance1, advance, Objects;

type
  PDefCollection = ^TDefCollection;
  TDefCollection = object(TStringCollection)
    Constructor Init(ALimit, ADelta: longInt); {Initialization }
    procedure ProceedFile(FName: String); {Read defines   }
    {from pascal-   }
    {style defines  }
    {file           }
    function IsDef(Name: String): boolean; {Is Name defined}
    procedure Def(Name: String); {Define Name    }
    procedure UnDef(Name: String); {Undefine Name  }
    function ProceedStr(Str: String): String; {Proceed pascal-}
    {style string   }
    (*{$.....}' and *)
    {(*$.....*)     }
    function ProceedStr2(Str: String): String; {Proceed RCP-   }
    {style string   }
    {;$.....        }
    private
    InComm: byte;
    DefStack: array[0..20] of boolean;
    Sp: byte;
    procedure Push(const ADef: String; IfDef: boolean);
    procedure Pop(EndIf: boolean);
    function TextOk: boolean;
    end;

implementation

Constructor TDefCollection.Init(ALimit, ADelta: longInt);
  var
    i: longInt;
  begin
    inherited Init(ALimit, ADelta, False);
    InComm := 0;
    Sp := 0;
    DefStack[Sp] := True;
    Def('VER20');
    Def('VIRTUALPASCAL');

    {AK155  символы можно задавать в комстроке в параметрах, начиная
со второго. При этом второй параметр - целевая платформа.}
    if ParamStr(2) = '' then
      begin
        {$IFDEF OS2}Def('OS2'); {$ENDIF}
        {$IFDEF WIN32}Def('WIN32'); {$ENDIF}
        {$IFDEF DPMI32}Def('DPMI32'); {$ENDIF}
      end
    else
      begin
        for i := 2 to ParamCount do
          Def(ParamStr(i));
      end;
  end { TDefCollection.Init };

procedure TDefCollection.ProceedFile;
  var
    F: lText;
    s: String;
    Q: String;
    i, j: integer;
    B: boolean;
  begin
    lAssignText(F, FName);
    lResetText(F);
    if IOResult <> 0 then
      exit;
    while not Eof(F.t) do
      begin
        readln(F.t, s);
        s := ProceedStr(s);
        if s = '' then
          continue;
        repeat
          i := Pos('{$DEFINE', s);
          if i = 0 then
            i := Pos('(*$DEFINE', s);
          if i > 0 then
            begin
              B := (s[i] = '(');
              System.Delete(s, i, 9+byte(B));
              DelLeft(s);
              for i := 1 to Length(s) do
                if s[i] in BreakChars then
                  break;
              if (s[i] in BreakChars) then
                Dec(i);
              Q := Copy(s, 1, i);
              Def(Q);
              System.Delete(s, 1, i);
              i := 0;
              repeat
                Inc(i)
              until (i >= Length(s)) or
              (B and (s[i] = '*') and (i < Length(s)) and (s[i+1] =
                ')')) or
              (not B and (s[i] = '}')) or
              (i = Length(s));
              System.Delete(s, 1, i);
            end;

          i := Pos('{$UNDEF', s);
          if i = 0 then
            i := Pos('(*$UNDEF', s);
          if i > 0 then
            begin
              B := (s[i] = '(');
              System.Delete(s, i, 8+byte(B));
              DelLeft(s);
              for i := 1 to Length(s) do
                if s[i] in BreakChars then
                  break;
              if (s[i] in BreakChars) then
                Dec(i);
              Q := Copy(s, 1, i);
              UnDef(Q);
              System.Delete(s, 1, i);
              i := 0;
              repeat
                Inc(i)
              until (i >= Length(s)) or
              (B and (s[i] = '*') and (i < Length(s)) and (s[i+1] =
                ')')) or
              (not B and (s[i] = '}')) or
              (i = Length(s));
              System.Delete(s, 1, i);
            end;
        until i = 0;
      end;
    Close(F.t);
  end { TDefCollection.ProceedFile };

function TDefCollection.IsDef(Name: String): boolean;
  var
    i: CondInt;
  begin
    UpStr(Name);
    IsDef := Search(@Name, i);
  end;

procedure TDefCollection.Def(Name: String);
  var
    i: CondInt;
  begin
    UpStr(Name);
    if not Search(@Name, i) then
      AtInsert(i, NewStr(Name));
  end;

procedure TDefCollection.UnDef(Name: String);
  var
    i: CondInt;
  begin
    UpStr(Name);
    if Search(@Name, i) then
      AtFree(i);
  end;

function TDefCollection.ProceedStr(Str: String): String;
  var
    i: integer;
    j: integer;
    k: integer;
    B: boolean;
    s: String;
    Q: String;
    Z: String;
  begin
    s := '';
    i := 0;
    while i < Length(Str) do
      begin
        Inc(i);
        B := False;
        if InComm = 0 then
          begin
            if (Str[i] = '{') and ((i = Length(Str)) or (Str[i+1] <>
                '$'))
            then
              InComm := 1
            else if ((Str[i] = '(') and (i < Length(Str)) and (Str[i+1]
                <> '*')) and
              ((i = Length(Str)-1) or (Str[i+2] <> '$'))
            then
              InComm := 2
            else if Str[i] = '{' then
              begin
                k := i;
                Inc(i);
                Inc(i);
                j := i+1;
                while not ((j = Length(Str)) or (Str[j] in
                    BreakChars))
                do
                  Inc(j);
                if not (Str[j] in BreakChars) then
                  Inc(j);
                Q := UpStrg(Copy(Str, i, j-i));
                i := j;
                if (Q = 'IFDEF') or (Q = 'IFNDEF') then
                  begin
                    while (Str[i] in BreakChars) do
                      Inc(i);
                    j := i;
                    while not (Str[j] in BreakChars) do
                      Inc(j);
                    Z := Copy(Str, i, j-i);
                    i := j;
                    Push(Z, Q = 'IFDEF');
                  end
                else if Q = 'ELSE' then
                  Pop(False)
                else if Q = 'ENDIF' then
                  Pop(True)
                else
                  B := True;
                while (i <= Length(Str)) and (Str[i] <> '}') do
                  Inc(i);
                if B and TextOk then
                  s := s+Copy(Str, k, i-k+1);
                if Str[i] <> '}' then
                  InComm := 1;
              end
            else if (i < Length(Str)) and (Str[i] = '(') and (Str[i+1] =
                '*')
            then
              begin
                k := i;
                Inc(i);
                Inc(i);
                Inc(i);
                j := i+1;
                while not ((j = Length(Str)) or (Str[j] in
                    BreakChars))
                do
                  Inc(j);
                if not (Str[j] in BreakChars) then
                  Inc(j);
                Q := UpStrg(Copy(Str, i, j-i));
                i := j;
                if (Q = 'IFDEF') or (Q = 'IFNDEF') then
                  begin
                    while (Str[i] in BreakChars) do
                      Inc(i);
                    j := i;
                    while not (Str[j] in BreakChars) do
                      Inc(j);
                    Z := Copy(Str, i, j-i);
                    i := j;
                    Push(Z, Q = 'IFDEF');
                  end
                else if Q = 'ELSE' then
                  Pop(False)
                else if Q = 'ENDIF' then
                  Pop(True)
                else
                  B := True;
                while (i < Length(Str)) and (Str[i] <> '*') and (Str[i+1
                    ] <> ')')
                do
                  Inc(i);
                if B and TextOk then
                  s := s+Copy(Str, k, i-k+1);
                if (i = Length(Str)) or ((Str[i] <> '*') and (Str[i+1]
                    <> ')'))
                then
                  InComm := 2;
              end
            else if TextOk then
              s := s+Str[i];
          end
        else if InComm = 1 then
          if Str[i] = '}' then
            InComm := 0
          else
        else if InComm = 2 then
          if (i < Length(Str)) and (Str[i] = '*') and (Str[i+1] =
              ')')
          then
            InComm := 0;
      end;
    ProceedStr := s;
  end { TDefCollection.ProceedStr };

function TDefCollection.ProceedStr2(Str: String): String;
  var
    i: integer;
    j: integer;
    k: integer;
    s: String;
    Q: String;
    Z: String;
  begin
    ProceedStr2 := '';
    s := Str;
    while (Str <> '') and (Str[1] = ' ') do
      System.Delete(Str, 1, 1);
    if (Length(Str) >= 2) and (Str[1] = ';') and (Str[2] = '$') then
        begin
        i := 1;
        k := i;
        Inc(i);
        Inc(i);
        j := i+1;
        while not ((j = Length(Str)) or (Str[j] in BreakChars)) do
            Inc(j);
        if not (Str[j] in BreakChars) then
          Inc(j);
        Q := UpStrg(Copy(Str, i, j-i));
        i := j;
        if (Q = 'IFDEF') or (Q = 'IFNDEF') then
          begin
            while (Str[i] in BreakChars) do
              Inc(i);
            j := i;
            while not (Str[j] in BreakChars) do
              Inc(j);
            Z := Copy(Str, i, j-i);
            i := j;
            Push(Z, Q = 'IFDEF');
          end
        else if Q = 'ELSE' then
          Pop(False)
        else if Q = 'ENDIF' then
          Pop(True);
        s := '';
      end;
    if TextOk then
      ProceedStr2 := s;
  end { TDefCollection.ProceedStr2 };

procedure TDefCollection.Push(const ADef: String; IfDef: boolean);
  begin
    Inc(Sp);
    if DefStack[Sp-1]
    then
      DefStack[Sp] := not (IfDef xor IsDef(ADef))
    else
      DefStack[Sp] := False;
  end;

procedure TDefCollection.Pop(EndIf: boolean);
  begin
    if EndIf
    then
      if Sp > 0 then
        Dec(Sp)
      else
        DefStack[0] := True
    else
      DefStack[Sp] := not DefStack[Sp];
  end;

function TDefCollection.TextOk: boolean;
  begin
    TextOk := DefStack[Sp];
  end;

end.
