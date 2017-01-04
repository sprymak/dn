{                   ÚÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂ¿                 }
{                   ÃÅÅ‘¯®pâ¨p®¢ « ­  ¯ áª «ì ¯®¤ ¤®áÅÅ´                 }
{                   ÃÅÅ €­â®­ ”¥¤®p®¢ aka DataCompBoyÅÅ´                 }
{                   ÃÅÅ ‚ ç¥áâì Œ â pëª¨­®© “«ìï­ë...ÅÅ´                 }
{                   ÀÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÙ                 }

{       Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.

     This  library  is free software; you can redistribute it and/or modify it
under  the terms of the GNU Library General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your option)
any later version.
     This  library  is  distributed  in  the  hope that it will be useful, but
WITHOUT  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS  FOR  A PARTICULAR PURPOSE. See the GNU Library General Public License
for more details.
     You should have received a copy of the GNU Library General Public License
along  with  this library; see the file COPYING.LIB. If not, write to the Free
Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. }

UNIT uFnMatch;

INTERFACE

Function FnMatch (Pattern, Str: String): Boolean;

IMPLEMENTATION

Function FnMatchFunc (Pattern, Str: String): Boolean;
Var c, c1: Char;
    p, n : Byte;
    nt: Boolean;
    cstart, cend: Char;
Begin
  p:=1; n:=1;
  While (p<=Length(Pattern)) do
    Begin
      c := Pattern[p]; Inc(p);

      Case (c) Of
       '?': If (n>Length(Str)) Then Begin FnMatchFunc:=False; Exit; End;
       {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
       '*': Begin
             c := Pattern[p]; Inc(p);
             While ((c='?') Or (c='*')) and (p<=Length(Pattern)) Do
              Begin
               Inc(n);
               If (c='?') And (n>Length(Str)) Then Begin FnMatchFunc:=False; Exit; End;
               c := Pattern[p]; Inc(p);
              End;

             If p-1>Length(Pattern) Then Begin FnMatchFunc:=True; Exit; End;

             c1:=c;
             Dec(p);
             While n<=Length(Str) do
              Begin
               If (((c='[') Or (Str[n]=c1)) And (FnMatchFunc(Copy(Pattern, p, 255), Copy(Str, n, 255))))
                Then Begin FnMatchFunc:=True; Exit; End;
               Inc(n);
              End;
             FnMatchFunc:=False;
             Exit;
            End;
       {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
       '[': Begin
             If n>Length(Str) Then Begin FnMatchFunc:=False; Exit; End;
             nt := Pattern[p] In ['!', '^'];
             If nt Then Inc(p);
             c:=Pattern[p]; Inc(p);

             Repeat
              cstart:=c;
              cend:=c;
              If p>Length(Pattern) Then Begin FnMatchFunc:=False; Exit; End;
              c:=Pattern[p]; Inc(p);
              If (c='-') And (Pattern[p]<>']') Then
               Begin
                cend:=Pattern[p]; Inc(p);
                If p>Length(Pattern) Then Begin FnMatchFunc:=False; Exit; End;
                c:=Pattern[p]; Inc(p);
               End;
              If (Str[n]>=cstart) And (Str[n]<=cend) Then Break;
              If c=']' then Break;
             Until True;

             If Not ((Str[n]>=cstart) And (Str[n]<=cend)) Then
              Begin
               If Not nt Then Begin FnMatchFunc:=False; Exit; End;
              End
             Else
              Begin
               {Skip the rest of the [...] that already matched.}
               While c<>']' Do
                Begin
                 If p>Length(Pattern) Then Begin FnMatchFunc:=False; Exit; End;
                   { [... (unterminated) loses.  }
                 c:=Pattern[p]; Inc(p);
                End;
               If nt Then Begin FnMatchFunc:=False; Exit; End;
              End;
            End;
       {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}
       Else If c<>Str[n] Then Begin FnMatchFunc:=False; Exit; End;
      End;

      Inc(n);
    End;

  If n>Length(Str) Then Begin FnMatchFunc:=True; Exit; End;

  FnMatchFunc:=False;
 End;

Function FnMatch (Pattern, Str: String): Boolean;
 var k, l: byte;
     j: string;
 Begin
  k:=1; l:=0; j:='';
  while k<=length(Pattern) do
   Begin
    if Pattern[k]='?' then
     begin j:='?'+j; if l=0 then l:=k; inc(k); end
    else
     if Pattern[k]='*' then
      begin if j[length(j)]<>'*' then j:=j+'*'; if l=0 then l:=k; inc(k); end
     else
      begin
       inc(k);
       if l>0 then
        begin
         delete(Pattern, l, k-l-1);
         insert(j, Pattern, l);
         k:=l+length(j);
         j:='';
         l:=0;
        end;
      end;
   end;
  if l>0 then
   begin
    delete(Pattern, l, k-l);
    insert(j, Pattern, l);
   end;
  FnMatch:= FnMatchFunc(Pattern, Str);
 End;

END.        