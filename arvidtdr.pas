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
//  dn3331-Arvid_bugfix_and_TDR_detection.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
UNIT ArvidTdr;

INTERFACE
uses Arvid, Objects, Advance1,   Messages, DnApp, Commands, Collect,
     Views, Drivers, Startup,  U_KeyMap, Advance, Dos, Lfn, LfnCol, Tree,
     FilesCol, Advance2, Drives, FlPanel, Memory;

procedure TdrSeekDirectory(AvtDr: PArvidDrive);
procedure TdrGetDirectory(AvtDr:PArvidDrive; var ALocation: LongInt;
                          var FC: PFilesCollection; const FileMask: string);
procedure TdrEditDescription(AvtDr: PArvidDrive; var S, Nam: String; var PF: PFileRec);
procedure TdrCalcTotal(AvtDr: PArvidDrive; const Offset: LongInt; var LL: TSize);
Function  TdrInit(AvtDr: PArvidDrive): boolean;

IMPLEMENTATION
uses Archiver{TdrInit:_Cardinal}{piwamoto}
     ;

{$IFDEF OS2}
FUNCTION  Norm12(const s:string):Str12;
var R: string[12]; I: Byte; L: Byte absolute S;
begin
  {if S[0]=#12 then begin Norm12:=S; Exit end;}
  System.FillChar(R[1],12,' '); R[0]:=#12;
  if s[1]='.' then begin Norm12:=AddSpace(s,12); Exit end;
  R[9]:='.'; i:=PosChar('.',s);
  if i=0 then i:=succ(l) else move(s[succ(i)],r[10],Min(l-i,3));
  if i>8 then i:=8 else dec(i);
  move(s[1],r[1],i);
  i:=1;
  while i<=12 do
    if r[i]='*'
      then while (i<>9) and (i<=12) do begin
        r[i]:='?';
        inc(i)
      end else inc(i);
  Norm12:=R
end;
{$ENDIF}

procedure TdrSeekDirectory;
var
      I,J: LongInt;
      Lv: Integer;
      DD: TTdrDirCell;
      SS: String[12];
      S: String;
begin with AvtDr^ do begin
  Stream^.Status := stOK;
  Stream^.Seek(D.DirTableOfs);
  Stream^.Read(DD, SizeOf(DD));
  CurDirPos := Stream^.GetPos;
  S := CurDir; CurLevel := 0; CurDir := ''; Lv := 1;
  if S[1] = '\' then DelFC(S);
  while S <> '' do
    begin
      SS := '';
      while (S[1] <> '\') and (S <> '') do
        begin
          AddStr(SS, S[1]);
          DelFC(S);
          if SS[0] = #12 then Break;
        end;
      DelFC(S);
      SS := Norm12(SS); UpStr(SS);
      Delete(SS, 9, 1);
      repeat
        Stream^.Read(DD, SizeOf(DD));
      until (UpStrg(Copy(DD.Name, 1, 11)) = SS) and (DD.Level = Lv) or (DD.Level < Lv);
      if (DD.Level < Lv) or (DD.Level = 0) then Break;
      Insert('.', SS, 9);
      if (CurDir[Length(CurDir)] <> '\') and
         (CurDir <> '') then AddStr(CurDir, '\');
      CurDir := CurDir + MakeFileName(SS);
      CurDirPos := Stream^.GetPos;
      CurLevel := Lv;
      Inc(Lv);
    end;

  CurDate := DD.Time;
  CurFile := DD.Files;
  CurFileNum := DD.NumFiles;
end end;

procedure TdrGetDirectory(AvtDr:PArvidDrive; var ALocation: LongInt;
                          var FC: PFilesCollection; const FileMask: string);
var
  FF: TTdrFileCell;
  DD: TTdrDirCell;
  I, J, SeekPos: LongInt;
  F: PFileRec;

  procedure AddFile;
  var
    S: Str12;
  begin with AvtDr^ do begin
    S := FF.Name;
    Insert('.', S, 9);
    if (s[01] in [#0..#31]) or
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
       (s[12] in [#0..#31]) then exit;
    if
    not (ArvidWithDN and Security and (FF.Attr and (Hidden+SysFile) <> 0))
    and (AllFiles or (FF.Attr and Directory <> 0) or InFilter(S, FileMask)) then
        begin
          F := NewFileRec(MakeFileName(S), {$IFNDEF OS2}MakeFileName(S),{$ENDIF} FF.Size, FF.Time, FF.Attr, @CurDir);
          New(F^.DIZ);
          F^.DIZ^.Owner := nil;
          F^.DIZ^.isDisposable := True;
          F^.DIZ^.Line := SeekPos;
          if FF.Description <> 0 then
            begin
              J := Stream^.GetPos;
              Stream^.Seek(D.DescTableOfs+FF.Description-1);
              Stream^.Read(FreeStr, 2);
              Stream^.Read(FreeStr[1], Length(FreeStr));
              Stream^.Seek(J);
              F^.DIZ^.DIZ := NewStr(FreeStr);
            end else F^.DIZ^.DIZ := nil;

          if FF.Attr and Directory = 0 then
            begin
              Inc(TotFiles);
              TotLen := TotLen + FF.Size;
            end;

          FC^.Insert(F);
        end;
  end end;

begin with AvtDr^ do begin
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
  Stream^.Seek(D.FileTableOfs + CurFile*SizeOf(TTdrFileCell));

  FC^.SetLimit(FC^.Count + CurFileNum);

  for I := 1 to CurFileNum do
    begin
      SeekPos := Stream^.GetPos;
      Stream^.Read(FF, SizeOf(FF));
      AddFile;
    end;

end end;

procedure TdrEditDescription;
var FF: TTdrFileCell;
    I, J: LongInt;
   procedure ExpandStream;
     var
         I,J: LongInt;
         L: Word;
         B: Array [1..512] of Byte;
   begin with AvtDr^ do begin
     J:=Stream^.GetSize;
     repeat
       I:=J-512;
       if I < D.PosTableOfs then I:=D.PosTableOfs;
       L:=J - I;
       Stream^.Seek(I);
       Stream^.Read(B, L);
       Stream^.Seek(I+512);
       Stream^.Write(B, L);
       J:=I;
     until J <= D.PosTableOfs;
     FillChar(B, 512, 0);
     Stream^.Seek(I);
     Stream^.Write(B, 512);
     Inc(D.PosTableOfs, 512);
   end end;

begin with AvtDr^ do begin
  Stream^.Read(FF, SizeOf(FF));
  if Length(S) <= Length(CnvString(PF^.DIZ^.DIZ)) then
    begin
      if S = '' then
        begin
          FF.Description:=0;
          Stream^.Seek(PF^.DIZ^.Line);
          Stream^.Write(FF, SizeOf(FF));
        end else
        begin
          Stream^.Seek(D.DescTableOfs+FF.Description-1);
          I := Length(S);
          Stream^.Write(I, 2);
          Stream^.Write(S[1], I);
        end;
    end else
    begin
      if D.PosTableOfs - D.DescTableOfs - D.DescTableLen < 2 + Length(S)
         then ExpandStream;
      FF.Description:=D.DescTableLen+1;{+1 by piwamoto:new desc creation fix}
      Stream^.Seek(PF^.DIZ^.Line);
      Stream^.Write(FF, SizeOf(FF));
      Inc(D.DescTableLen, Length(S)+2);
      Stream^.Seek(D.DescTableOfs+FF.Description-1);
      I := Length(S);
      Stream^.Write(I, 2);
      Stream^.Write(S[1], I);
    end;
end end;

procedure TdrCalcTotal;
var
  DD: TTdrDirCell;
  SPos: LongInt;
  procedure CountDirectory(DD, Num: LongInt);
   var FF: TTdrFileCell;
       I: Integer;
  begin with AvtDr^ do begin
    Stream^.Status:=stOK;
    Stream^.Seek(D.FileTableOfs + DD*SizeOf(TTdrFileCell));
    for I:=1 to Num do
      begin
        Stream^.Read(FF, SizeOf(FF));
        LL:=LL + FF.Size;
      end;
  end end;
begin with AvtDr^ do begin
  SPos:=CurDirPos;
  repeat
    Stream^.Seek(SPos);
    Stream^.Read(DD, SizeOf(DD));
    SPos:=Stream^.GetPos;
    if DD.Level > CurLevel then CountDirectory(DD.Files, DD.NumFiles);
  until (DD.Level = 0) or (DD.Level <= CurLevel);
  CountDirectory(CurFile, CurFileNum);
end end;

Function TdrInit;
var J: Word;
begin with AvtDr^ do begin
  TdrInit := false;
  FileType := avdTdr;
  Stream^.Seek(0);
  Stream^.Read(D, SizeOf(D));
  if (Stream^.Status <> stOK) or
     (_Cardinal(D.PosTableOfs) > Stream^.GetSize) then Exit;{piwamoto: reject invalid TDRs}
  TapeFmt:=D.TapeFmt;
  TapeTotalTime:=D.TapeLen;
  PosTableOfs:=D.PosTableOfs;
  Stream^.Seek(D.PosTableOfs);
  Stream^.Read(J, SizeOf(J));
{  if Stream^.Status <> stOK then exit;}{commented by piwamoto}
  TapeRecordedTime:=J*8;
  TdrInit:=True;
end end;

END.
