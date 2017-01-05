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

unit U_SrchF;

interface

uses
  Objects, Collect, Gauge,
  Lfn, {DataCompBoy}
  Commands, {DataCompBoy}
  DNApp; {DataCompBoy}

function FindFileWithSPF(pFileName: String; Info: PWhileView):
  String;

implementation

uses
  Dos, advance, advance1, advance2, advance3, Messages, DnIni;

{-DataCompBoy-}
function RecursiveCheck(Name, pPath: String; DoMore: boolean; Info:
    PWhileView): String;
  var
    SR: lSearchRec;
    Sc: TStringCollection;
  label Q;
  begin
    if Abort then
      exit;
    DispatchEvents(Info, Abort);
    if Abort then
      exit;
    Sc.Init($10, $10, False);
    RecursiveCheck := '';
    Sc.AtInsert(0, NewStr(pPath));

    repeat
      DispatchEvents(Info, Abort);
      if Abort then
        goto Q;
      pPath := CnvString(Sc.At(0));
      Sc.AtFree(0);

      if ExistFile(MakeNormName(pPath, Name)) then
        begin
          RecursiveCheck := MakeNormName(pPath, Name);
          lFindClose(SR);
          goto Q;
        end;

      lFindFirst(MakeNormName(pPath, x_x), AnyFile, SR);
      while DOSError = 0 do
        begin
          DispatchEvents(Info, Abort);
          if Abort then
            begin
              lFindClose(SR);
              goto Q
            end;
          if (SR.SR.Name <> '.') and (SR.SR.Name <> '..') and
            (SR.SR.Attr and Directory <> 0)
          then
            begin
              if DoMore
              then
                Sc.Insert(NewStr(MakeNormName(pPath, SR.FullName)))
              else if ExistFile(MakeNormName(MakeNormName(pPath, SR.
                  FullName), Name))
              then
                begin
                  RecursiveCheck := MakeNormName(MakeNormName(pPath, SR.
                    FullName), Name);
                  lFindClose(SR);
                  goto Q;
                end;
            end;
          lFindNext(SR);
        end;
      lFindClose(SR);
    until Sc.Count = 0;
Q:
    Sc.Done;
  end { RecursiveCheck };
{-DataCompBoy-}

{-DataCompBoy-}
function FindFileWithSPF(pFileName: String; Info: PWhileView):
    String;
  var
    F: lText;
    s, s2: String;
    IsSec, Found, InSq: boolean;
    P, i: integer;
    D: TMaskData;
  label ex;
  begin
    FillChar(D, SizeOf(D), 0);
    FindFileWithSPF := '';
    if ExistFile(pFileName) then
      begin
        FindFileWithSPF := pFileName;
        exit;
      end;
    if CharCount('.', pFileName) = 0 then
      pFileName := pFileName+'.';
    lAssignText(F, SourceDir+'DN.SPF');
    lResetText(F);
    if IOResult <> 0 then
      begin
        lRewriteText(F);
        if IOResult <> 0 then
          exit;
        Writeln(F.t, '; Dos Navigator Search Paths file');
        Writeln(F.t);
        Writeln(F.t, GetString(dlSPF01));
        Writeln(F.t, GetString(dlSPF02));
        Writeln(F.t, GetString(dlSPF03));
        Writeln(F.t, GetString(dlSPF04));
        Writeln(F.t, GetString(dlSPF05));
        Writeln(F.t, GetString(dlSPF06));
        Writeln(F.t, GetString(dlSPF07));
        Writeln(F.t, GetString(dlSPF08));
        Writeln(F.t, GetString(dlSPF09));
        Writeln(F.t, GetString(dlSPF10));
        Writeln(F.t, GetString(dlSPF11));
        Writeln(F.t, GetString(dlSPF12));
        Writeln(F.t, GetString(dlSPF13));
        Writeln(F.t, GetString(dlSPF14));
        Writeln(F.t, GetString(dlSPF15));
        Writeln(F.t, GetString(dlSPF16));
        Writeln(F.t, GetString(dlSPF17));
        Writeln(F.t, GetString(dlSPF18));
        Writeln(F.t, GetString(dlSPF19));
        Writeln(F.t, GetString(dlSPF20));
        Writeln(F.t, GetString(dlSPF21));
        Writeln(F.t, GetString(dlSPF22));
        Writeln(F.t, GetString(dlSPF23));
        Writeln(F.t, GetString(dlSPF24));
        Writeln(F.t, GetString(dlSPF25));
        Close(F.t);
        Info^.Hide; {Cat}
        MessageBox(GetString(dlSPFht), nil, mfOKButton);
        exit;
      end;
    IsSec := False;
    Found := False;
    while (not Eof(F.t)) and (not Found) do
      begin
        if Abort then
          goto ex;
        readln(F.t, s);
        s := fDelLeft(fDelRight(s));
        if (s = '') or (Copy(s, 1, 1) = ';') then
          continue;
        if (s[1] = '[') then
          begin
            IsSec := False;
            P := Pos(']', s);
            if P > 0 then
              s := Copy(s, 2, P-2)
            else
              Delete(s, 1, 1);
            D.Filter := s;
            MakeTMaskData(D);
            IsSec := InExtFilter(pFileName, D);
          end
        else if IsSec then
          begin
            DelLeft(s);
            InSq := False;
            P := 1;
            for i := 1 to Length(s)+1 do
              if ((i > Length(s)) and (P < Length(s))) or
                ((s[i] in [';', ',']) and (not InSq))
              then
                begin
                  s2 := Copy(s, P, i-P);
                  if s2[1] = '*' then
                    s2 := RecursiveCheck(pFileName, Copy(s2, 2,
                      MaxStringLength), True, Info)
                  else if s2[1] = '+' then
                    s2 := RecursiveCheck(pFileName, Copy(s2, 2,
                      MaxStringLength), False, Info)
                  else if ExistFile(MakeNormName(Copy(s2, 2,
                      MaxStringLength), pFileName))
                  then
                    s2 := MakeNormName(Copy(s2, 2, MaxStringLength),
                      pFileName)
                  else
                    s2 := '';
                  if Abort then
                    goto ex;
                  P := i+1;
                  if s2 <> '' then
                    begin
                      FindFileWithSPF := s2;
                      Found := True;
                    end
                end
              else if s[i] = '"' then
                InSq := not InSq;
          end;
      end;
ex:
    Close(F.t);
    {D.Filter := ''; MakeTMaskData(D);}
    FreeTMaskData(D); {Cat}
  end { FindFileWithSPF };
{-DataCompBoy-}

end.
