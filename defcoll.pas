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
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
UNIT DefColl;

INTERFACE
uses Collect, LFN, Advance1, Advance;

Type
 PDefCollection = ^TDefCollection;
 TDefCollection = Object(TStringCollection)
                   Constructor Init(ALimit, ADelta: LongInt); {Initialization }
                   procedure ProceedFile(FName: String);      {Read defines   }
                                                              {from pascal-   }
                                                              {style defines  }
                                                              {file           }
                   function  IsDef(Name: String): boolean;    {Is Name defined}
                   procedure Def  (Name: String);             {Define Name    }
                   procedure UnDef(Name: String);             {Undefine Name  }
                   function  ProceedStr (Str: String): string;{Proceed pascal-}
                                                              {style string   }
                                                             (*{$.....}' and *)
                                                              {(*$.....*)     }
                   function  ProceedStr2(Str: String): string;{Proceed RCP-   }
                                                              {style string   }
                                                              {;$.....        }
                  private
                   InComm: byte;
                   DefStack: Array[0..20] of boolean;
                   SP: byte;
                   procedure Push (const ADef: String; IfDef: boolean);
                   procedure Pop  (EndIf: boolean);
                   Function  TextOk: boolean;
                  end;

IMPLEMENTATION

Constructor TDefCollection.Init(ALimit, ADelta: LongInt);
begin
 Inherited Init(ALimit, ADelta);
 InComm:=0;
 SP:=0;
 DefStack[SP]:=true;
{$IFDEF VER70} Def('VER70'); {$ENDIF}
{$IFDEF NONBP} Def('VER20'); {$ENDIF}
{$IFDEF DPMI}  Def('DPMI');  {$ENDIF}
{$IFDEF VIRTUALPASCAL}Def('VIRTUALPASCAL');  {$ENDIF}
{$IFDEF WIN32} Def('WIN32');  {$ENDIF}
{$IFDEF OS2}   Def('OS2');    {$ENDIF}
end;

procedure TDefCollection.ProceedFile;
var F: lText;
    S: String;
    Q: String;
    I,j: integer;
    B: boolean;
begin
 lAssignText(F, FName);
 lResetText(F);
 if IOResult<>0 then exit;
 while not EOF(F.T) do begin
  Readln(F.T, S);
  S:=ProceedStr(S);
  if S='' then continue;
  Repeat
   I:=Pos('{$DEFINE',S);
   If I=0 then I:=Pos('(*$DEFINE',S);
   If I>0 then begin
    B:=(S[i]='(');
    System.Delete(S, I, 9+Byte(B));
    DelLeft(S);
    For I:=1 to length(S) do if S[I] in BreakChars then break;
    if (S[I] in BreakChars) then dec(I);
    Q:=Copy(S, 1, I);
    Def(Q);
    System.Delete(S, 1, I); I:=0;
    Repeat Inc(I)
    Until (i>=length(S)) or
          (B and (S[i]='*') and (i<length(s)) and (S[i+1]=')')) or
          (not B and (S[i]='}')) or
          (i=length(S));
    System.Delete(S, 1, I);
   end;

   I:=Pos('{$UNDEF',S);
   If I=0 then I:=Pos('(*$UNDEF',S);
   If I>0 then begin
    B:=(S[i]='(');
    System.Delete(S, I, 8+Byte(B));
    DelLeft(S);
    For I:=1 to length(S) do if S[I] in BreakChars then break;
    if (S[I] in BreakChars) then dec(I);
    Q:=Copy(S, 1, I);
    UnDef(Q);
    System.Delete(S, 1, I); I:=0;
    Repeat Inc(I)
    Until (i>=length(S)) or
          (B and (S[i]='*') and (i<length(s)) and (S[i+1]=')')) or
          (not B and (S[i]='}')) or
          (i=length(S));
    System.Delete(S, 1, I);
   end;
  until I=0;
 end;
 Close(F.T);
end;

function  TDefCollection.IsDef(Name: String): boolean;
var I: LongInt;
begin
 UpStr(Name);
 IsDef:=Search(@Name, I);
end;

procedure TDefCollection.Def  (Name: String);
var I: LongInt;
begin
 UpStr(Name);
 if not Search(@Name, I) then
  AtInsert(I, NewStr(Name));
end;

procedure TDefCollection.UnDef(Name: String);
var I: LongInt;
begin
 UpStr(Name);
 If Search(@Name, I) then AtFree(I);
end;


function  TDefCollection.ProceedStr(Str: String): string;
var i: integer;
    j: integer;
    k: integer;
    b: boolean;
    S: String;
    Q: string;
    Z: string;
begin
 S:='';
 I:=0;
 while i<length(Str) Do begin
  inc(I);
  b:=false;
  if InComm=0 then begin
   if (Str[i]='{') and ((i=length(Str)) or (Str[i+1]<>'$')) then InComm:=1 else
   if ((Str[i]='(') and (i<Length(Str)) and (Str[I+1]<>'*')) and
       ((i=length(Str)-1) or (Str[i+2]<>'$')) then InComm:=2 else
   if Str[i]='{' then begin
    K:=I;
    Inc(I); Inc(I);
    J:=I+1; while not ((j=length(Str)) or (Str[j] in BreakChars)) do inc(j);
    if not (Str[j] in BreakChars) then inc(j);
    Q:=UpStrg(Copy(Str, I, J-I));
    I:=J;
    if (Q='IFDEF') or (Q='IFNDEF') then begin
     while (Str[I] in BreakChars) do inc(i);
     j:=i;
     while not (Str[J] in BreakChars) do inc(J);
     Z:=Copy(Str, I, J-I);
     I:=J;
     Push(Z, Q='IFDEF');
    end else
    if Q='ELSE' then Pop(False) else
    If Q='ENDIF' then Pop(True) else
    b:=true;
    while (I<=Length(Str)) and (Str[I] <> '}') do inc(I);
    if b and TextOk then S:=S+Copy(Str, K, I-K+1);
    if Str[I]<>'}' then InComm:=1;
   end else
   if (I<length(Str)) and (Str[i]='(') and (Str[i+1]='*') then begin
    K:=I;
    Inc(I); Inc(I); Inc(I);
    J:=I+1; while not ((j=length(Str)) or (Str[j] in BreakChars)) do inc(j);
    if not (Str[j] in BreakChars) then inc(j);
    Q:=UpStrg(Copy(Str, I, J-I));
    I:=J;
    if (Q='IFDEF') or (Q='IFNDEF') then begin
     while (Str[I] in BreakChars) do inc(i);
     j:=i;
     while not (Str[J] in BreakChars) do inc(J);
     Z:=Copy(Str, I, J-I);
     I:=J;
     Push(Z, Q='IFDEF');
    end else
    if Q='ELSE' then Pop(False) else
    If Q='ENDIF' then Pop(True) else
    b:=true;
    while (I<Length(Str)) and (Str[I] <> '*') and (Str[I+1]<>')')  do inc(I);
    if b and TextOk then S:=S+Copy(Str, K, I-K+1);
    if (I=Length(Str)) or ((Str[I]<>'*') and (Str[I+1]<>')')) then InComm:=2;
   end else
   If TextOk then S:=S+Str[i];
  end else
  if InComm=1 then if Str[i]='}' then InComm:=0 else
  else
  if InComm=2 then if (i<length(str)) and (Str[i]='*') and (Str[I+1]=')') then InComm:=0;
 end;
 ProceedStr:=S;
end;

function  TDefCollection.ProceedStr2(Str: String): string;
var i: integer;
    j: integer;
    k: integer;
    S: String;
    Q: string;
    Z: string;
begin
 ProceedStr2:='';
 S:=Str;
 While (Str[1] = ' ') and (Str <> '') do System.Delete(Str, 1, 1);
 If (Str <> '') and (Str[1]=';') and (Str[2]='$') then begin
    I:=1;
    K:=I;
    Inc(I); Inc(I);
    J:=I+1; while not ((j=length(Str)) or (Str[j] in BreakChars)) do inc(j);
    if not (Str[j] in BreakChars) then inc(j);
    Q:=UpStrg(Copy(Str, I, J-I));
    I:=J;
    if (Q='IFDEF') or (Q='IFNDEF') then begin
     while (Str[I] in BreakChars) do inc(i);
     j:=i;
     while not (Str[J] in BreakChars) do inc(J);
     Z:=Copy(Str, I, J-I);
     I:=J;
     Push(Z, Q='IFDEF');
    end else
    if Q='ELSE' then Pop(False) else
    If Q='ENDIF' then Pop(True);
    S:='';
 end;
 If TextOk then ProceedStr2:=S;
end;

procedure TDefCollection.Push (const ADef: String; IfDef: boolean);
begin
 Inc(SP);
 if DefStack[SP-1]
  then DefStack[SP]:=not (IfDef xor IsDef(ADef))
  else DefStack[SP]:=false;
end;

procedure TDefCollection.Pop  (EndIf: boolean);
begin
 if EndIf
  then if SP>0 then Dec(SP) else DefStack[0]:=true
  else DefStack[SP]:=not DefStack[SP];
end;

Function  TDefCollection.TextOk: boolean; begin TextOk:=DefStack[SP]; end;

END.
