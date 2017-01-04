{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source
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
//////////////////////////////////////////////////////////////////////////
//
//  Version history:
//
//  1.6.RC1
//  dn270-IsFilenameMatchesMask_fix_for_DOS_styled_anyfile_mask.patch
//
//  3.7.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

{       Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.     }

UNIT uFnMatch;

INTERFACE

Function FnMatch (Pattern, Str: String): Boolean;

IMPLEMENTATION

Function FnMatchFunc (Pattern, Str: String; AllLiters: boolean): Boolean;
Var c, c1: Char;
    p, n : Byte;
    nt: Boolean;
    cstart, cend: Char;
label Litera;
Begin
  p:=1; n:=1;
  While (p<=Length(Pattern)) do
    Begin
      c := Pattern[p]; Inc(p);
      if c='"' then begin
       AllLiters:=not AllLiters;
       continue;
      end;

      Case (c) Of
       '?': If (n>Length(Str)) Then Begin FnMatchFunc:=False; Exit; End;
       {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
       '*': Begin
             c := Pattern[p]; Inc(p);
             While ((c='?') Or (c='*')) and (p<=Length(Pattern)) Do
              Begin
               if c='?' then Inc(n);{DataCompBoy: * may present 0 chars...}
               If (c='?') And (n>Length(Str)) Then Begin FnMatchFunc:=False; Exit; End;
               c := Pattern[p]; Inc(p);
              End;

             If p-1>Length(Pattern) Then Begin FnMatchFunc:=True; Exit; End;

             c1:=c;
             Dec(p);
             While n<=Length(Str) do
              Begin
               If ((((c='[') and not AllLiters) Or (Str[n]=c1)) And
                  (FnMatchFunc(Copy(Pattern, p, 255), Copy(Str, n, 255), AllLiters)))
                Then Begin FnMatchFunc:=True; Exit; End;
               Inc(n);
              End;
             FnMatchFunc:=False;
             Exit;
            End;
       {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
       '[': Begin
             if AllLiters then Goto Litera;
             If n>Length(Str) Then Begin FnMatchFunc:=False; Exit; End;
             nt := Pattern[p]='^';
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
             Until c=']';

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
       {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
       Else
Litera:  If (c<>Str[n]) or (N>length(Str))
           Then Begin FnMatchFunc:=False; Exit; End
           Else if Copy(Pattern, p, 255) = '*.*'
                then begin FnMatchFunc:=True; Exit; end;
           {checks only for '*.*', because '*' checked under Case (c) of '*'}
      End;

      if (n = length(Str)) and (Copy(Pattern, p, 255) = '.*')
        then begin FnMatchFunc:=True; Exit; end;
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
  FnMatch:= FnMatchFunc(Pattern, Str, False);
 End;

END.
