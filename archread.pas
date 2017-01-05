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
unit Archread;

interface

uses
  Archiver, FStorage, Dos, ArcView, advance, advance1, advance2,
    Messages,
  DNApp, Commands, Drivers, Lfn, Objects, Views;

procedure ReadArcList;

implementation

{-DataCompBoy-}
procedure ReadArcList; {changed & AIN added by piwamoto}
  const
    FuckName = 'U$~RESLT.OK';
  var
    s, CurDir, Id: String;
    P: PArcFile;
    PC: PDirStorage;
    F: PTextReader;
    DT: DateTime;
    i, j: integer;
    Drv: PArcDrive;
  label 1, 2;
  begin
    s := TempFile;
    Id := Copy(s, 1, 4);
    Delete(s, 1, 4);
    i := PosChar(']', s);
    ArcFileName := Copy(s, i+1, MaxStringLength);
    SetLength(s, i-1);
    ClrIO;
    TempFile := '';
    if (Pos(Id, 'UC2:AIN:')+3) mod 4 = 0 then
      begin
        if Id = 'UC2:' then
          begin
            if not ExistFile(FuckName) then
              begin
                MessageBox(GetString(dlArcMsg6), nil, mfOKButton or
                  mfError);
                exit;
              end;
            EraseFile(FuckName);
            lGetDir(0, CurDir); {GetDir(0, CurDir);} {Cat}
            GlobalMessage(evCommand, cmRereadDir, @CurDir);
            F := New(PTextReader, Init(s));
            if F = nil then
              exit;
            P := nil;
            New(PC, Init);
            while not F^.Eof do
              begin
                s := F^.GetStr;
                DelLeft(s);
                DelRight(s);
                Id := Copy(s, 1, 4);
                if Id = 'LIST' then
                  begin
                    i := PosChar('[', s);
                    if i = 0 then
                      continue;
                    Delete(s, 1, i);
                    i := PosChar(']', s);
                    if i < 2 then
                      continue;
                    SetLength(s, i-1);
                    CurDir := s;
                  end
                else if (Id = 'FILE') or (Id = 'DIR') then
                  begin
                    if P <> nil then
                      begin
                        PackTime(DT, P^.Date);
                        PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.
                          Date, P^.Attr);
                        DisposeStr(P^.FName);
                        Dispose(P);
                      end;
                    New(P);
                    P^.FName := nil;
                    if Id = 'DIR' then
                      begin
                        P^.Attr := Directory;
                        P^.PSize := 0;
                        P^.USize := 0
                      end
                    else
                      P^.Attr := 0;
                  end
                else if Id = 'END' then
                  begin
                    if P <> nil then
                      begin
                        PackTime(DT, P^.Date);
                        PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.
                          Date, P^.Attr);
                        DisposeStr(P^.FName);
                        Dispose(P);
                      end;
                    break
                  end
                else
                  begin
                    i := PosChar('=', s);
                    if i = 0 then
                      goto 1;
                    Delete(s, 1, i);
                    if Id = 'NAME' then
                      begin
                        i := PosChar('[', s);
                        if i = 0 then
                          continue;
                        Delete(s, 1, i);
                        i := PosChar(']', s);
                        if i < 2 then
                          continue;
                        SetLength(s, i-1);
                        if P^.Attr = Directory then
                          s := s+'\';
                        P^.Attr := 0;
                        P^.FName := NewStr(CurDir+s);
                      end
                    else if Id = 'DATE' then
                      begin
                        DT.Month := StoI(Copy(s, 1, 2));
                        DT.Day := StoI(Copy(s, 4, 2));
                        DT.Year := StoI(Copy(s, 7, 4));
                      end
                    else if Id = 'TIME' then
                      begin
                        DT.Hour := StoI(Copy(s, 1, 2));
                        DT.Min := StoI(Copy(s, 4, 2));
                        DT.Sec := StoI(Copy(s, 7, 4));
                      end
                    else if Id = 'ATTR' then
                      begin
                      end
                    else if Id = 'SIZE' then
                      begin
                        P^.USize := StoI(s);
                        P^.PSize := StoI(s);
                      end
                    else if Id = 'VERS' then
                      begin
                      end;
                  end;
              end;
1:
            Id := 'UC2:';
          end;
        if Id = 'AIN:' then
          begin
            F := New(PTextReader, Init(s));
            if F = nil then
              exit;
            P := nil;
            New(PC, Init);
            repeat
              s := F^.GetStr;
              Id := Copy(s, 9, 2);
              i := PosChar('%', s);
            until ((Id = ': ') and (i > 50)) or (F^.Eof);
            i := PosChar(',', s);
            if (i < 12) or (F^.Eof) then
              goto 2;
            Id := Copy(s, 11, i-11);
            Val(Id, j, i);
            for i := 1 to 4 do
              s := F^.GetStr;
            while (j <> 0) and (not F^.Eof) do
              begin
                s := F^.GetStr;
                DelLeft(s);
                DelRight(s);
                New(P);
                P^.Attr := 0;
                i := PosChar(' ', s);
                if i = 0 then
                  begin
                    P^.FName := NewStr('\'+s);
                    s := F^.GetStr;
                    DelLeft(s);
                    DelRight(s);
                  end
                else
                  begin
                    P^.FName := NewStr('\'+Copy(s, 1, i-1));
                    Delete(s, 1, i);
                    DelLeft(s);
                  end;
                i := PosChar(' ', s);
                P^.USize := StoI(Copy(s, 1, i-1));
                P^.PSize := StoI(Copy(s, 1, i-1));
                Delete(s, 1, i);
                DelLeft(s);
                DT.Month := StoI(Copy(s, 1, 2));
                DT.Day := StoI(Copy(s, 4, 2));
                i := StoI('19'+fDelRight(Copy(s, 7, 3)));
                if i < 1980 then
                  i := i+100;
                DT.Year := i;
                i := PosChar(' ', s);
                Delete(s, 1, i);
                DelLeft(s);
                DT.Hour := StoI(Copy(s, 1, 2));
                DT.Min := StoI(Copy(s, 4, 2));
                DT.Sec := StoI(Copy(s, 7, 2));
                PackTime(DT, P^.Date);
                PC^.AddFile(P^.FName^, P^.USize, P^.PSize, P^.Date, P^.
                  Attr);
                DisposeStr(P^.FName);
                Dispose(P);
                P := nil;
                j := j-1;
              end;
2:
            Id := 'AIN:';
          end;
        {next archive}

        s := F^.FileName;
        Dispose(F, Done);
        EraseFile(s);
        GlobalMessage(evCommand, cmRereadDir, @TempDir);
        if PC^.Files = 0 then
          Dispose(PC, Done)
        else
          begin
            New(Drv, InitCol(PC, ArcFileName, VArcFileName));
            if Message(Application, evBroadcast, cmFindForced, Drv) =
                nil
            then
              if Message(Application, evCommand, cmInsertDrive, Drv) =
                  nil
              then
                begin
                end;
          end;
      end;
  end { ReadArcList };
{-DataCompBoy-}

end.
