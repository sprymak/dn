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
unit ArvidTdr;

interface

uses
  Arvid, Objects, advance1, Messages, DNApp, Commands, Collect,
  Views, Drivers, Startup, U_KeyMap, advance, Lfn, Files, Dos, Tree,
  FilesCol, advance2, Drives, FlPanel, Memory;

procedure TdrSeekDirectory(AvtDr: PArvidDrive);
procedure TdrGetDirectory(AvtDr: PArvidDrive; var ALocation: longInt;
  var FC: PFilesCollection; const FileMask: String);
procedure TdrEditDescription(AvtDr: PArvidDrive; var s, Nam: String;
    var PF: PFileRec);
procedure TdrCalcTotal(AvtDr: PArvidDrive; const Offset: longInt;
    var LL: TSize);
function TdrInit(AvtDr: PArvidDrive): boolean;
function TdrMakeFileName(s: String): String; {JO}

implementation
uses
  Archiver {TdrInit:_Cardinal} {piwamoto}
  ;

function TdrMakeFileName(s: String): String;
  var
    i: integer;
  begin
    s[9] := '.';
    i := 8;
    while (i > 0) and (s[i] = ' ') do
      begin
        Delete(s, i, 1);
        Dec(i);
      end;
    while s[Length(s)] = ' ' do
      SetLength(s, Length(s)-1);
    if s[Length(s)] = '.' then
      SetLength(s, Length(s)-1);
    Result := s;
  end;

procedure TdrSeekDirectory;
  var
    i, j: longInt;
    Lv: integer;
    DD: TTdrDirCell;
    SS: String[12];
    s: String;
  begin
    with AvtDr^ do
      begin
        Stream^.Status := stOK;
        Stream^.Seek(D.DirTableOfs);
        Stream^.Read(DD, SizeOf(DD));
        CurDirPos := Stream^.GetPos;
        s := CurDir;
        CurLevel := 0;
        CurDir := '';
        Lv := 1;
        if s[1] = '\' then
          Delete(s, 1, 1); {DelFC(S);}
        while s <> '' do
          begin
            SS := '';
            while (s[1] <> '\') and (s <> '') do
              begin
                SS := SS+s[1]; {AddStr(SS, S[1]);}
                Delete(s, 1, 1); {DelFC(S);}
                if Length(SS) = 12 then
                  break;
              end;
            Delete(s, 1, 1); {DelFC(S);}
            SS := Norm12(SS);
            UpStr(SS); {JO ??? for OS/2}
            {AK155: нельзя убирать! При чем тут OS/2 к Арвиду? }
            Delete(SS, 9, 1);
            repeat
              Stream^.Read(DD, SizeOf(DD));
            until (UpStrg(Copy(DD.Name, 1, 11)) = SS) and (DD.Level = Lv
              ) or (DD.Level < Lv);
            if (DD.Level < Lv) or (DD.Level = 0) then
              break;
            Insert('.', SS, 9);
            if (CurDir[Length(CurDir)] <> '\') and
              (CurDir <> '')
            then
              AddStr(CurDir, '\');
            CurDir := CurDir+TdrMakeFileName(SS);
            CurDirPos := Stream^.GetPos;
            CurLevel := Lv;
            Inc(Lv);
          end;

        CurDate := DD.Time;
        CurFile := DD.Files;
        CurFileNum := DD.NumFiles;
      end
  end { TdrSeekDirectory };

procedure TdrGetDirectory(AvtDr: PArvidDrive; var ALocation: longInt;
  var FC: PFilesCollection; const FileMask: String);
  var
    FF: TTdrFileCell;
    DD: TTdrDirCell;
    i, j, SeekPos: longInt;
    F: PFileRec;

  procedure AddFile;
    var
      s: String;
      B: byte;
    begin
      with AvtDr^ do
        begin
          s := FF.Name;
          Insert('.', s, 9);
          {    if (s[01] in [#0..#31]) or
       (s[02] in [#0..#31]) or
       (s[03] in [#0..#31]) or
       (s[04] in [#0..#31]) or
       (s[05] in [#0..#31]) or
       (s[06] in [#0..#31]) or
       (s[07] in [#0..#31]) or
       (s[08] in [#0..#31]) or
       (s[09] in [#0..#31]) or
       (s[10] in [#0..#31]) or
       (s[11] in [#0..#31]) or
       (s[12] in [#0..#31]) then exit; }
          {AK155: в ритлабовском этого не было }

          if
            not (ArvidWithDN and Security and (FF.Attr and (Hidden+
              SysFile) <> 0))
            and (AllFiles or (FF.Attr and Directory <> 0) or
              InFilter(s, FileMask))
          then
            begin
              {$IFNDEF OS2}
              F := NewFileRec(TdrMakeFileName(s), s, FF.Size, FF.
                Time, 0, 0, FF.Attr, @CurDir);
              {$ELSE}
              F := NewFileRec(TdrMakeFileName(s), FF.Size, FF.Time, 0, 0
                , FF.Attr, @CurDir);
              {$ENDIF}
              New(F^.DIZ);
              F^.DIZ^.Owner := nil;
              F^.DIZ^.isDisposable := True;
              F^.DIZ^.Line := SeekPos;
              if FF.Description <> 0 then
                begin
                  j := Stream^.GetPos;
                  Stream^.Seek(D.DescTableOfs+FF.Description-1);
                  {Cat:warn AnsiString}
                  Stream^.Read(FreeStr, 2);
                  Stream^.Read(FreeStr[1], Length(FreeStr));
                  Stream^.Seek(j);
                  F^.DIZ^.DIZ := NewStr(FreeStr);
                end
              else
                F^.DIZ^.DIZ := nil;

              if FF.Attr and Directory = 0 then
                begin
                  Inc(TotFiles);
                  TotLen := TotLen+FF.Size;
                end;

              FC^.Insert(F);
            end;
        end
    end { AddFile };

  begin { TdrGetDirectory }
    with AvtDr^ do
      begin
        Stream^.Seek(CurDirPos);
        repeat
          SeekPos := Stream^.GetPos+2;
          Stream^.Read(DD, SizeOf(DD));
          if DD.Level = CurLevel+1 then
            begin
              Move(DD.Name, FF, SizeOf(FF));
              FF.Attr := FF.Attr or Directory;
              AddFile;
            end;
        until (DD.Level = 0) or (DD.Level <= CurLevel);

        Stream^.Status := stOK;
        Stream^.Seek(D.FileTableOfs+CurFile*SizeOf(TTdrFileCell));

        FC^.SetLimit(FC^.Count+CurFileNum);

        for i := 1 to CurFileNum do
          begin
            SeekPos := Stream^.GetPos;
            Stream^.Read(FF, SizeOf(FF));
            AddFile;
          end;

      end
  end { TdrGetDirectory };

procedure TdrEditDescription;
  var
    FF: TTdrFileCell;
    i, j: longInt;
  procedure ExpandStream;
    var
      i, j: longInt;
      l: word;
      B: array[1..512] of byte;
    begin
      with AvtDr^ do
        begin
          j := Stream^.GetSize;
          repeat
            i := j-512;
            if i < D.PosTableOfs then
              i := D.PosTableOfs;
            l := j-i;
            Stream^.Seek(i);
            Stream^.Read(B, l);
            Stream^.Seek(i+512);
            Stream^.Write(B, l);
            j := i;
          until j <= D.PosTableOfs;
          FillChar(B, 512, 0);
          Stream^.Seek(i);
          Stream^.Write(B, 512);
          Inc(D.PosTableOfs, 512);
        end
    end { ExpandStream };

  begin { TdrEditDescription }
    with AvtDr^ do
      begin
        Stream^.Read(FF, SizeOf(FF));
        if Length(s) <= Length(CnvString(PF^.DIZ^.DIZ)) then
          begin
            if s = '' then
              begin
                FF.Description := 0;
                Stream^.Seek(PF^.DIZ^.Line);
                Stream^.Write(FF, SizeOf(FF));
              end
            else
              begin
                Stream^.Seek(D.DescTableOfs+FF.Description-1);
                i := Length(s);
                Stream^.Write(i, 2);
                Stream^.Write(s[1], i);
              end;
          end
        else
          begin
            if D.PosTableOfs-D.DescTableOfs-D.DescTableLen < 2+
                Length(s)
            then
              ExpandStream;
            FF.Description := D.DescTableLen+1;
              {+1 by piwamoto:new desc creation fix}
            Stream^.Seek(PF^.DIZ^.Line);
            Stream^.Write(FF, SizeOf(FF));
            Inc(D.DescTableLen, Length(s)+2);
            Stream^.Seek(D.DescTableOfs+FF.Description-1);
            i := Length(s);
            Stream^.Write(i, 2);
            Stream^.Write(s[1], i);
          end;
      end
  end { TdrEditDescription };

procedure TdrCalcTotal;
  var
    DD: TTdrDirCell;
    SPos: longInt;
  procedure CountDirectory(DD, Num: longInt);
    var
      FF: TTdrFileCell;
      i: integer;
    begin
      with AvtDr^ do
        begin
          Stream^.Status := stOK;
          Stream^.Seek(D.FileTableOfs+DD*SizeOf(TTdrFileCell));
          for i := 1 to Num do
            begin
              Stream^.Read(FF, SizeOf(FF));
              LL := LL+FF.Size;
            end;
        end
    end;
  begin { TdrCalcTotal }
    with AvtDr^ do
      begin
        SPos := CurDirPos;
        repeat
          Stream^.Seek(SPos);
          Stream^.Read(DD, SizeOf(DD));
          SPos := Stream^.GetPos;
          if DD.Level > CurLevel then
            CountDirectory(DD.Files, DD.NumFiles);
        until (DD.Level = 0) or (DD.Level <= CurLevel);
        CountDirectory(CurFile, CurFileNum);
      end
  end { TdrCalcTotal };

function TdrInit;
  var
    j: word;
  begin
    with AvtDr^ do
      begin
        TdrInit := False;
        filetype := avdTdr;
        Stream^.Seek(0);
        Stream^.Read(D, SizeOf(D));
        if (Stream^.Status <> stOK) or
          (_Cardinal(D.PosTableOfs) > Stream^.GetSize)
        then
          exit; {piwamoto: reject invalid TDRs}
        TapeFmt := D.TapeFmt;
        TapeTotalTime := D.TapeLen;
        PosTableOfs := D.PosTableOfs;
        Stream^.Seek(D.PosTableOfs);
        Stream^.Read(j, SizeOf(j));
        {  if Stream^.Status <> stOK then exit;}
          {commented by piwamoto}
        TapeRecordedTime := j*8;
        TdrInit := True;
      end
  end { TdrInit };

end.
