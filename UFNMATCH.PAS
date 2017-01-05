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
{       Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.     }

unit UFNMatch;

interface

function FnMatch(Pattern, Str: String): Boolean;

implementation

uses
  Commands {Cat}
  ;

function FnMatchFunc(Pattern, Str: String; AllLiters: Boolean): Boolean;
  var
    c, c1: Char;
    p, n: Byte;
    nt: Boolean;
    cstart, cend: Char;
  label Litera;
  begin
  p := 1;
  n := 1;
  while (p <= Length(Pattern)) do
    begin
    c := Pattern[p];
    Inc(p);
    if c = '"' then
      begin
      AllLiters := not AllLiters;
      continue;
      end;

    case (c) of
      '?':
        if  (n > Length(Str)) then
          begin
          FnMatchFunc := False;
          exit;
          end;
      {-------------------------------------------------------}
      '*':
        begin
        c := Pattern[p];
        Inc(p);
        while ((c = '?') or (c = '*')) and (p <= Length(Pattern)) do
          begin
          if c = '?' then
            Inc(n); {DataCompBoy: * may present 0 chars...}
          if  (c = '?') and (n > Length(Str)) then
            begin
            FnMatchFunc := False;
            exit;
            end;
          c := Pattern[p];
          Inc(p);
          end;

        if p-1 > Length(Pattern) then
          begin
          FnMatchFunc := True;
          exit;
          end;

        c1 := c;
        Dec(p);
        while n <= Length(Str) do
          begin
          if  ( ( ( (c = '[') and not AllLiters) or (Str[n] = c1)) and
                (FnMatchFunc(Copy(Pattern, p, MaxStringLength), Copy(Str,
                     n, MaxStringLength), AllLiters)))
          then
            begin
            FnMatchFunc := True;
            exit;
            end;
          Inc(n);
          end;
        FnMatchFunc := False;
        exit;
        end;
      {-------------------------------------------------------}
      '[':
        begin
        if AllLiters then
          goto Litera;
        if n > Length(Str) then
          begin
          FnMatchFunc := False;
          exit;
          end;
        nt := Pattern[p] = '^';
        if nt then
          Inc(p);
        c := Pattern[p];
        Inc(p);

        repeat
          cstart := c;
          cend := c;
          if p > Length(Pattern) then
            begin
            FnMatchFunc := False;
            exit;
            end;
          c := Pattern[p];
          Inc(p);
          if  (c = '-') and (Pattern[p] <> ']') then
            begin
            cend := Pattern[p];
            Inc(p);
            if p > Length(Pattern) then
              begin
              FnMatchFunc := False;
              exit;
              end;
            c := Pattern[p];
            Inc(p);
            end;
          if  (Str[n] >= cstart) and (Str[n] <= cend) then
            break;
        until c = ']';

        if not ((Str[n] >= cstart) and (Str[n] <= cend)) then
          begin
          if not nt then
            begin
            FnMatchFunc := False;
            exit;
            end;
          end
        else
          begin
          {Skip the rest of the [...] that already matched.}
          while c <> ']' do
            begin
            if p > Length(Pattern) then
              begin
              FnMatchFunc := False;
              exit;
              end;
            { [... (unterminated) loses.  }
            c := Pattern[p];
            Inc(p);
            end;
          if nt then
            begin
            FnMatchFunc := False;
            exit;
            end;
          end;
        end;
      {-------------------------------------------------------}
      else {case}
Litera:
          if  (c <> Str[n]) or (n > Length(Str))
          then
            begin
            FnMatchFunc := False;
            exit;
            end
          else if Copy(Pattern, p, 255) = '*.*'
          then
            begin
            FnMatchFunc := True;
            exit;
            end;
      {checks only for '*.*', because '*' checked under Case (c) of '*'}
    end {case};

    if  (n = Length(Str)) and (Copy(Pattern, p, 255) = '.*')
    then
      begin
      FnMatchFunc := True;
      exit;
      end;
    Inc(n);
    end;

  if n > Length(Str) then
    begin
    FnMatchFunc := True;
    exit;
    end;

  FnMatchFunc := False;
  end { FnMatchFunc };

function FnMatch(Pattern, Str: String): Boolean;
  var
    k, l: Byte;
    j: String;
  begin
  k := 1;
  l := 0;
  j := '';
  while k <= Length(Pattern) do
    begin
    if Pattern[k] = '?' then
      begin
      j := '?'+j;
      if l = 0 then
        l := k;
      Inc(k);
      end
    else if Pattern[k] = '*' then
      begin
      if j[Length(j)] <> '*' then
        j := j+'*';
      if l = 0 then
        l := k;
      Inc(k);
      end
    else
      begin
      Inc(k);
      if l > 0 then
        begin
        Delete(Pattern, l, k-l-1);
        Insert(j, Pattern, l);
        k := l+Length(j);
        j := '';
        l := 0;
        end;
      end;
    end;
  if l > 0 then
    begin
    Delete(Pattern, l, k-l);
    Insert(j, Pattern, l);
    end;
  FnMatch := FnMatchFunc(Pattern, Str, False);
  end { FnMatch };

end.
