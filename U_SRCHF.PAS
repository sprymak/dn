{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.04/DOS
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
//     "Based on TinyWeb Server by RIT Research Labs."
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

unit u_srchf;
interface
uses Objects;

function FindFileWithSPF(pFileName: String): String;

implementation

uses u_crt, dos, advance, u_dos, messages, format, dnini;

procedure RecursiveAdd(SC: PStringCollection; pPath: String; DoMore: Boolean); far;
var SR: SearchRec;
begin
  pPath := u_crt.Replace(pPath + '\', '\\', '\');
  SC^.Insert(NewStr(UpperCase(pPath)));
  FindFirst(pPath + '*.*', AnyFile, SR);
  while DOSError = 0 do
  begin
    if (SR.Name <> '.') and (SR.Name <> '..') and
       (SR.Attr and Directory <> 0) then
    begin
      if DoMore then RecursiveAdd(SC, pPath + SR.Name, DoMore) else
        SC^.Insert(NewStr(UpperCase(pPath + SR.Name)));
    end;
    FindNext(SR);
  end;
end;

function FindFileWithSPF(pFileName: String): String;
var SPF: PStringCollection;
    F: Text;
    Ext, S, S2: String;
    IsSec, More1, Found: Boolean;
    P, I: Integer;
    Hour, Min, Sec, Sec100: Word;
begin
  FindFileWithSPF := '';
  pFileName := UpperCase(pFileName);
  if FileExists(pFileName) then
  begin
    FindFileWithSPF := pFileName;
    Exit;
  end;
  Ext := CutExt(pFileName);
  SPF := New(PStringCollection, Init(20, 10));
  Assign(F, SourceDir + 'DN.SPF');
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then
  begin
    Dispose(SPF, Done);
    {$I-}
    Rewrite(F);
    {$I+}
    if IOResult <> 0 then Exit;
    Writeln(F, '; Dos Navigator Search Paths file');
    Writeln(F);
    Writeln(F);
    Writeln(F);
    Writeln(F, ';--------------------------------------------------------------------------');
    Writeln(F, '; Формат:' );
    Writeln(F, '; ~~~~~~~' );
    Writeln(F, '; <Формат строки> ::= <Комментарий> | <Секция> | <Путь>');
    Writeln(F, '; <Комментарий> ::= <;> Любой текст');
    Writeln(F, '; <Секция> ::= <[> <Расширение> { <,> <Расширение> } <]>');
    Writeln(F, '; <Путь> ::= <Каталог> { <;> <Каталог> }');
    Writeln(F, '; <Каталог> ::= [ <*> | <+> ] <Каталог MS-DOS>');
    Writeln(F, '; ');
    Writeln(F, '; Примечания:');
    Writeln(F, '; ~~~~~~~~~~~' );
    Writeln(F, '; {} - параметр может повтарятся 0 и более раз');
    Writeln(F, '; [] - параметр может быть опущен');
    Writeln(F, ';  * - в первом символе имени каталога означает рекурсивный');
    Writeln(F, ';      поиск в каталоге по всем подкаталогам');
    Writeln(F, ';  + - в первом символе имени каталога означает рекурсивный');
    Writeln(F, ';      поиск по всем подкаталогам одного уровня вложенности');
    Writeln(F, '; Для отладки используйте ключ Kernel/DebugMode в dn.ini');
    Writeln(F, '; ');
    Writeln(F, '; Пример:');
    Writeln(F, '; ~~~~~~' );
    Writeln(F, '; [pas,atl]');
    Writeln(F, '; *c:\bp\rtl\;c:\my\units');
    Writeln(F, '; i:\vex\atl\');
    Writeln(F, ';------------------------------------------------------------- (c) Viv''99 -');
    Close(F);
    MessageBox('Для поиска фалов необходимо настроить файл DN.SPF. Образец файла DN.SPF был успешно создан.', nil, mfOkButton);
    Exit;
  end;
  IsSec := False;
  if DebugMode then InitTimer;
  while not EOF(F) do
  begin
    Readln(F, S); S := Trim(S);
    if (S = '') or (Copy(S, 1, 1) = ';') then Continue;
    if (S[1] = '[') then
    begin
      IsSec := False;
      P := Pos(']', S);
      if P > 0 then S := Copy(S, 2, P - 2) else Delete(S, 1, 1);
      S := UpperCase(S);
      repeat
        S := Trim(S);
        P := Pos(',', S);
        if P > 0 then
        begin
          S2 := Copy(S, 1, P - 1);
          Delete(S, 1, P);
        end else
        begin
          S2 := S;
          S := '';
        end;
        S2 := Trim(S2);
        if S2 <> '' then
          if (S2 = '*') or (S2 = Ext) then IsSec := True;
      until (S = '') or IsSec;
    end else if IsSec then
    begin
      repeat
        S := Trim(S);
        P := Pos(';', S);
        if P > 0 then
        begin
          S2 := Copy(S, 1, P - 1);
          Delete(S, 1, P);
        end else
        begin
          S2 := S;
          S := '';
        end;
        S2 := Trim(S2);
        if S2 <> '' then
        begin
          if S2[1] in ['*', '+'] then
          begin
            More1 := (S2[1] = '*');
            Delete(S2, 1, 1); S2 := Trim(S2);
            RecursiveAdd(SPF, S2, More1);
          end else SPF^.Insert(NewStr(UpperCase(S2)));
        end;
      until (S = '');
    end;
  end;
  Close(F);
  Found := False;
  for I := 0 to SPF^.Count - 1 do
  begin
    S := u_crt.Replace(PString(SPF^.At(I))^ + '\' + pFileName, '\\', '\');
    if FileExists(S) then
    begin
      FindFileWithSPF := S;
      Found := True;
      if DebugMode then
      begin
        S2 := PString(SPF^.At(I))^; {-$VOL}
        SPF^.AtReplace(I, NewStr(S2 + ' <--- найден здесь')); {-$VOL}
      end;
      Break;
    end;
  end;

  {DEBUF}
  if DebugMode then
  begin
    GetTimerFull(Hour, Min, Sec, Sec100);
    Assign(f, SourceDir + 'SPF.DBG');
    Rewrite(f);
    Writeln(f, ';----------------------------------------------------------');
    Writeln(f, '; Отчет о поиске файла: ', pFileName);
    if Found then Writeln(f, '; Найден как файл: ', S) else
                  Writeln(f, '; Файл не найден !');
    Writeln(f, '; Всего каталогов в списке: ', SPF^.Count);
    Writeln(f, '; Общее время поиска: ', Hour, ':', Min, ':', Sec);
    Writeln(f, ';----------------------------------------------------------');
    for I := 0 to SPF^.Count - 1 do Writeln(f, PString(SPF^.At(I))^);
    Close(f);
    MessageBox('DEBUG: Файл ' + SourceDir + 'SPF.DBG сформирован.', nil, mfOkButton);
  end;

  Dispose(SPF, Done);
end;

end.
