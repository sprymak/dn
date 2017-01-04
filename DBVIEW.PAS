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
//  dn16rc1-deleting_temporary_files_safety_fix.patch
//  dn16rc1-dbf_scrollbar_colors_fix.patch
//  dn16rc1-vp_noasm_compatible.patch
//  dn16rc1-edit_long_db_field.patch
//
//  2.0.0
//  dn200-delete_file_from_temp_fix-diff161byMV.patch
//  dn200-dbf_wkz-history.patch
//  dn200-dbf_wkz_hscroll_bar.patch
//  dn200-empty_dbf.patch
//  dn223-dbf_indicator_and_hscrollbar.patch
//  dn2215-windows_manager_changes_2.patch
//
//  2.3.0
//  dn247-text_and_file_find_improve.patch
//  dn230-dbf_history_improve.patch
//  dn230-dbf_view_long_field_improve.patch
//  dn230-dbf_viewer_scrollbars_fix.patch
//
//  2.7.0
//  dn270-dbf_window_activate_fix.patch
//  dn270_dbf_goto_row_column_added.patch
//  dn281-dbf_wkz_view_from_archive_fix.patch
//  dn281_dbf_detect_fix.patch
//  dn281-dbf_view_Clipper_memo_fix.patch
//  dn2911-dbf_search_in_current_line_fix.patch
//  dn2106-dbf_viewer_search_date_fix.patch
//  dn2106-dbf_update_after_country_settings_changed.patch
//  dn21013-dbf_search_fix.patch
//  dn21029-dbf_find_empty_fields.patch
//  dn3216-DBF_Viewer(fn)-change_code_page_ability_added.patch
//  dn3315-DBF_Viewer(i)-indicator_and_code_page_improve.patch
//  dn3421-DBF_WKZ(f)-find_settings_store_fix.patch
//
//  3.7.0
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

{$ifndef db}
unit DBView;

interface
uses LFN,   DBWatch, Objects, Views, Drivers, Dos, Memory, Dialogs,
     RStrings, FViewer, Commands, U_KeyMap, DNStdDlg;
{$else}

uses LFN, DBWatch, Objects, Views, Drivers, Dos, Memory, Menus, Messages,
     advance, advance1, advance2, advance3, Startup, FViewer, Dialogs,
     Commands, DNApp, DNStdDlg, U_KeyMap;

{$endif}

type
{$ifdef db}
    TMyApp = object(TApplication)
     procedure HandleEvent(var Event: TEvent); virtual;
     procedure InitStatusLine; virtual;
    end;
{$endif}

    TDBPoint = record X: Integer; Y: LongInt; end;

    PDBIndicator = ^TDBIndicator;
    PDBViewer = ^TDBViewer;

{ -------- Eugeny Zvyagintzev ---------}
    PDBScrollBar = ^TDBScrollBar;
    TDBScrollBar = Object(TScrollBar)
     ScrollBarType: Integer;
     DBViewer: PDBViewer;
     Procedure HandleEvent(var Event: TEvent); Virtual;
     Function GetPalette: PPalette; Virtual;
    End;
{ -------- Eugeny Zvyagintzev ---------}

    TDBViewer = object(TView)
     FileName: PString;
     DBFile: PDBFile;
     IsValid, KillAfterUse: Boolean;
     Buf: Pointer;
     SearchString: PString;
     StartRec: LongInt;
     NumRec: Word;
     Indicator: PDBIndicator;
     Delta, Pos: TDBPoint;
     VerticalScrollBar  : PDBScrollBar;
     HorizontalScrollBar: PDBScrollBar;
     XLAT: TXLAT;      {John_SW 18-03-2003}
     UseXLAT: Boolean; {John_SW 18-03-2003}
     XLatFile: PString;{John_SW 18-03-2003}
     KeyMap: TKeyMap;  {John_SW 14-03-2003}
     constructor Init(R: TRect; FName: String; Var FileIsDBF: Boolean); {DataCompBoy}
     function Valid(Command: Word): Boolean; virtual;
     function GetRecord(N: LongInt): Pointer;
     procedure GetInfo(StrIdx: TStrIdx);
     procedure HandleEvent(var Event: TEvent); virtual;
     destructor Done; virtual;
     procedure Draw; virtual;
     function GetPalette: PPalette; virtual;
     Constructor Load(var S : Tstream);
     Procedure Store(var S : TStream);
     function Failed(Var FileIsDBF: Boolean): Boolean;
     function KeyMapConvertStr(S: String; toAscii: Boolean): String; {-$VIV} {John_SW 14-03-2003}
     procedure SetXlatFile(const aFName: String; DoRedraw: Boolean); {John_SW 18-03-2003}
    end;

    PMemoStream = ^TMemoStream;
    TMemoStream = object(TStream)
     StartPos: LongInt;
     Length: LongInt;
     BaseStream: PStream;
     constructor Init(AStream: PStream; Start, Len: LongInt);
     destructor Done; virtual;
     function GetPos: Longint; virtual;
     function GetSize: Longint; virtual;
     procedure Read(var Buf; Count: Word); virtual;
     procedure Seek(Pos: Longint); virtual;
     procedure Truncate; virtual;
     procedure Write(const Buf; Count: Word); virtual;
    end;

    TDBIndicator = object(TView)
     DBViewer: PDBViewer;
     procedure Draw; virtual;
     {Constructor Load(var S : Tstream);
       Procedure Store(var S : TStream);}
    end;

    PFieldListBox = ^TFieldListBox;
    TFieldListBox = object(TListBox)
     function GetText(Item: LongInt; MaxLen: Integer): String; virtual;
    end;

    PDBWindow = ^TDBWindow;
    TDBWindow = object(TFileWindow)
       P: PDBViewer;
      P1: PDBIndicator;
      VSB: PDBScrollBar;
      HSB: PDBScrollBar;
     RealName: String;
{--- start -------- Eugeny Zvyagintzev ---------}
     constructor Init(FName: String; Var FileIsDBF: Boolean);
     Procedure ChangeBounds(Var R: TRect); Virtual;
{--- finish -------- Eugeny Zvyagintzev ---------}
     function GetPalette: PPalette; virtual;
     Constructor Load(var S : Tstream);
     Procedure Store(var S : TStream);
     procedure SetState(AState: Word; Enable: Boolean); virtual; {John_SW 14-03-2003}
     destructor Done; virtual;
    end;


 const CDBWindow = #166#167#168#169#170#171#212#213;
       CDBViewer = #4#5#6;
       CDBScrollBar = #7#8#8;

 const SearchData : record
    S: String[250]; {Inputline}
    Opt: Word; {Checkbox}
    Scope: Word;  {Radiobuttons}
    Direction: Integer;  {Radiobuttons}
   end = (S:'';Opt:0;Scope:0;Direction:0);

{$ifndef db}
implementation
uses Advance, Advance1, Advance2, Advance3, Startup, DNHelp, DNApp, Messages,
     Histries, Gauge, xTime, DNIni;
{$endif}

function MaxAvail: LongInt;
begin
  MaxAvail := MemAdjust(System.MaxAvail);
end;

function TFieldListBox.GetText;
 var P: PFieldRec;
     S: String;
     S1, S2, S3, S4: String[20];
     M: Array [0..4] of Pointer;
begin
 P := List^.At(Item);
 LongInt(M[0]) := Item+1;
 M[1] := @P^.Name;
 M[2] := @S1;
 M[3] := @S2;
 M[4] := @S3;
 case P^.Who of
  'N': S1 := GetString(dlDBNumeric);
  'C': S1 := GetString(dlDBCharacter);
  'M': S1 := GetString(dlDBMemo);
  'L': S1 := GetString(dlDBLogical);
  'D': S1 := GetString(dlDBDate);
  'F': S1 := GetString(dlDBFloat);
  'P': S1 := GetString(dlDBPicture);
 end;
 S2 := ItoS(P^.Len);
 if (P^.Who = 'N') or (P^.Who = 'F') then S3 := ItoS(P^.Dec) else S3 := '';
 if (P^.Who = 'C') and (P^.Dec <> 0) then S3 := ItoS(P^.Dec); {JOHN_SW}
 FormatStr(S, ' %-4d%-12s%-13s%-11s%-8s', M);
 GetText := S;
end;

{ -------- Eugeny Zvyagintzev ---------}
Function TDBScrollBar.GetPalette: PPalette;
 Const
       S: String[Length(CDBScrollBar)] = CDBScrollBar;
Begin
 GetPalette := @S;
End;

Procedure TDBScrollBar.HandleEvent(var Event: TEvent);
Begin
 Inherited HandleEvent(Event);
 case ScrollBarType of
  sbVertical:  If DBViewer^.Delta.Y <> Value Then Begin
                DBViewer^.Delta.Y:=Value;
                DBViewer^.DrawView;
                DBViewer^.Indicator^.Draw;
               End;
  sbHorizontal:If DBViewer^.Delta.X <> Value Then Begin
                DBViewer^.Delta.X:=Value;
                DBViewer^.DrawView;
                DBViewer^.Indicator^.Draw;
               End;
 end;
End;
{ -------- Eugeny Zvyagintzev ---------}

constructor TMemoStream.Init;
begin
 TObject.Init;
 StartPos := Start;
 Length := Len;
 BaseStream := AStream;
 if AStream^.Status = stOK then AStream^.Seek(StartPos);
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;
end;

destructor TMemoStream.Done;
begin
 Dispose(BaseStream,Done); BaseStream:=nil;
 TObject.Done;
end;

function TMemoStream.GetPos: Longint;
 var L: LongInt;
begin
 L := BaseStream^.GetPos - StartPos;
 if L < 0 then begin BaseStream^.Seek(StartPos); L := 0; end;
 GetPos := L;
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;
end;

function TMemoStream.GetSize: Longint;
begin
 GetSize := Length;
end;

procedure TMemoStream.Read(var Buf; Count: Word);
begin
 BaseStream^.Read(Buf, Count);
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;
end;

procedure TMemoStream.Seek(Pos: Longint);
 var L: LongInt;
begin
 L := Pos + StartPos;
 BaseStream^.Seek(L);
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;
end;

procedure TMemoStream.Truncate;
begin
 BaseStream^.Truncate;
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;
end;

procedure TMemoStream.Write(const Buf; Count: Word);
begin
 BaseStream^.Write(Buf, Count);
 ErrorInfo := BaseStream^.ErrorInfo;
 Status := BaseStream^.Status;

 {Move(AStream^, Self, sizeof(TStream));}
end;

        {-DataCompBoy-}
constructor TDBViewer.Init;
begin
  inherited Init(R);
  KillAfterUse := TempFile <> '';
  Options := Options or ofSelectable;
  GrowMode := gfGrowHiX + gfGrowHiY;
  EventMask := $FFFF;
  SearchString := nil;
  HelpCtx := hcDBView;
  Buf := nil; FileName := NewStr(lFExpand(FName));
  if Failed(FileIsDBF) then isValid:=false else if isValid then TempFile := '';
  KeyMap:=kmAscii; {John_SW 14-03-2003}
  XlatFile := nil;              {John_SW 18-03-2003}
  NullXLAT(XLat); UseXLat:=False; {John_SW 18-03-2003}
end;
        {-DataCompBoy-}

Procedure TDBViewer.Store ;
 begin
  Inherited Store( S );
  S.Write( KillAfterUse, SizeOf ( KillAfterUse ) );
  S.WriteStr( SearchString );
  S.WriteStr( FileName );
  S.Write( Delta , SizeOf(Delta));
  S.Write( Pos , SizeOf( Pos ));
  S.Write(KeyMap, SizeOf(KeyMap));  {John_SW 14-03-2003}
  S.WriteStr(XlatFile);             {John_SW 18-03-2003}
  S.Write(XLAT, SizeOf(XLAT));      {John_SW 18-03-2003}
  S.Write(UseXLAT, SizeOf(UseXLAT));{John_SW 18-03-2003}
 end;


function DateToHuman(const S: String): String;
  var I: Integer;
      C: Char;
      BB: Byte;
      D, M: String[2];
      Y: String[4];
begin
 Y:=Copy(S, 1, 4);
 M:=Copy(S, 5, 2);
 D:=Copy(S, 7, 2);
 case CountryInfo.DateFmt of
  dfMDY: DateToHuman := M + CountryInfo.DateSep + D + CountryInfo.DateSep + Y;
  dfDMY: DateToHuman := D + CountryInfo.DateSep + M + CountryInfo.DateSep + Y;
  dfYMD: DateToHuman := Y + CountryInfo.DateSep + M + CountryInfo.DateSep + D;
 end;
end;


function TDBViewer.Failed(Var FileIsDBF: Boolean): Boolean;
var I : LongInt ;
begin
  Failed := False;
  New(DBFile, Init(CnvString(FileName)));

  if DBFile <> nil then
  begin
    I := 65520;
    if DBFile^.NumRec = 0 then
    begin
      GetInfo(dlDBEmptyStruc);
{     DisposeStr(SearchString);
      Dispose(DBFile,Done); DBFile:=nil;
}     TempFile := '';
      if KillAfterUse then EraseTempFile(CnvString(FileName));
{     DisposeStr(FileName);}
      Failed := True;
      FileIsDBF:=True;
      Exit;
    end;
    if MaxAvail < I then I := MaxAvail;
    if LongInt(I) > DBFile^.NumRec*LongInt(DBFile^.RecLen) then
    I := DBFile^.NumRec*LongInt(DBFile^.RecLen);
    NumRec := I div DBFile^.RecLen;
    I := NumRec * DBFile^.RecLen;
    StartRec := 0;
    if I <> 0 then GetMem(Buf, I);
  end;
  IsValid := (DBFile <> nil) and (Buf <> nil);
  if IsValid then
  begin
    DBFile^.Seek(0);
    DBFile^.Read(Buf^, NumRec);
  end;
 FileIsDBF:=IsValid;
end;

Constructor TDBViewer.Load ;
Var
   FileIsDBF: Boolean;
begin
  inherited Load( S );
  S.Read( KillAfterUse, SizeOf ( KillAfterUse ) );
  SearchString := S.ReadStr;
  FileName := S.ReadStr;

  if Failed(FileIsDBF) then Fail;

  S.Read( Delta , SizeOf(Delta));
  S.Read( Pos , SizeOf( Pos ));

  S.Read(KeyMap, SizeOf(KeyMap));    {John_SW 14-03-2003}
  XlatFile:=S.ReadStr;
  S.Read(XLAT, SizeOf(XLAT));        {John_SW 18-03-2003}
  S.Read(UseXLAT, SizeOf(UseXLAT));  {John_SW 18-03-2003}
end;

destructor TDBViewer.Done;
begin
 DisposeStr(SearchString);
 if Buf <> nil then FreeMem(Buf, NumRec*DBFile^.RecLen);
 if DBFile <> nil then Dispose(DBFile,Done); DBFile:=nil;
{--- start -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
 if not (TottalExit or Exiting) and KillAfterUse then
  begin
   EraseTempFile(FileName^);
   KillAfterUse:=False;
  end;
{--- finish -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
 DisposeStr(FileName);
 inherited Done;
end;

function TDBViewer.GetPalette;
 const S: String[Length(CDBviewer)] = CDBViewer;
begin
 GetPalette := @S;
end;

function TDBWindow.GetPalette;
 const S: String[Length(CDBWindow)] = CDBWindow;
begin
 GetPalette := @S;
end;

function TDBViewer.GetRecord;
 var
     I: Integer;
begin
 GetRecord := nil;
 if N < 0 then N := 0;
 if N >= DBFile^.NumRec then Exit;
 if (N < StartRec) or (N >= StartRec + NumRec) then
  begin
   StartRec := N - (NumRec div 2);
   if StartRec < 0 then StartRec := 0;
   DBFile^.Seek(StartRec);
   DBFile^.Read(Buf^, NumRec);
  end;
 GetRecord := @PByteArray(Buf)^[(N - StartRec)*DBFile^.RecLen];
end;

function TDBViewer.Valid;
begin
 Valid := inherited Valid(Command) and IsValid;
end;

procedure TDBViewer.GetInfo;
 var R: TRect;
     D: PDialog;
     P: PView;
begin
 R.Assign(1,1,50,18);
 D := New(PDialog, Init(R, GetString(StrIdx)+Cut(FileName^, 20)));
 D^.Options := D^.Options or ofCentered;

 R.Assign(2,2,48,3);
 P := New(PStaticText, Init(R, GetString(dlDBViewInfoString)));
 D^.Insert(P);

 R.Assign(46,3,47,13);
 P := New(PScrollBar, Init(R));
 P^.Options := P^.Options or ofPostProcess;
 D^.Insert(P);

 R.Assign(2,3,46,13);
 P := New(PFieldListBox, Init(R, 1, PScrollBar(P)));
 PListBox(P)^.NewList(DBFile^.Fields);
 D^.Insert(P);

 R.Assign(30, 14, 40, 16);
 P := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
 P^.Options := P^.Options or ofCenterX;
 D^.Insert(P);

 D^.SelectNext(False);

 D := PDialog(Application^.ValidView(D));
 if D = nil then Exit;
 Desktop^.ExecView(D);
 Dispose(D,Done);
end;

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
function TDBViewer.KeyMapConvertStr(S: String; toAscii: Boolean): String; {-$VIV}
begin
 If Not UseXlat Then
  case KeyMap of
    kmAnsi:
      if toAscii then
        KeyMapConvertStr := ansi_ascii(S) else
        KeyMapConvertStr := ascii_ansi(S);
    kmKoi8r:
      if toAscii then
        KeyMapConvertStr := koi8r_ascii(S) else
        KeyMapConvertStr := ascii_koi8r(S);
  else
   KeyMapConvertStr := S;
  end
 Else
  If toAscii Then
   Begin
    XLatBuf(S[1],Length(S),XLat);
    KeyMapConvertStr := S;
   End
  Else
   Begin
    CharsConvertBufBack(S[1], Length(S), String(XLat));
    KeyMapConvertStr := S;
   End;
end; {-$VIV}

procedure TDBViewer.SetXlatFile;
  var Dr: String;
      Nm: String;
      Xt: String;
      S: TDosStream;
      Table: Array [1..256] of Record A, B: Char end;
      C, CC: Char;
      I,J: Integer;
{      PC: PChar;}
begin
  if aFName <> '' then
  begin
    lFSplit(aFName, Dr, Nm, Xt);
    if Dr = '' then begin Dr:=SourceDir+'XLT\'; Xt:='.xlt'; end;
    S.Init(Dr+Nm+Xt, stOpenRead);
    if (S.Status = stOK) and (S.GetSize >= 2) then
    begin
      J:=Min(256, S.GetSize shr 1);
      S.Read(Table, J * 2);
      NullXLAT(XLAT);
      for I:=1 to J do XLat[Table[I].A]:=Table[I].B;
      UseXlat:=S.Status = stOK;
      if XlatFile<>nil then DisposeStr(XlatFile); XlatFile:=nil;
      if UseXlat then XlatFile:=NewStr(Nm);
    end;
    S.Done;
  end;
  if aFName='' then begin
    UseXLat:=False;
    if not GetToAsciiRecodeTable ( KeyMap, XLat ) then
      NullXLAT(XLat);
  end;
  if DoRedraw then
   Begin
    DrawView;
    If Indicator <> Nil Then Indicator^.Draw;
   End;
end;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

procedure TDBViewer.HandleEvent;

{--- start -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}
 Procedure GotoRecFld;
 Const
      GotoRec: Record
       S1: String[10]; {Line Number}
       S2: String[10]; {Field Name}
      End = (S1:'';S2:'');
 Var
   FldCnt: Integer;
   Done  : Boolean;
   PFR   : PFieldRec;
 Begin
  PFR:=DBFile^.GetFieldRec(Delta.X);
  GotoRec.S1:=ItoS(Delta.Y+1);
  GotoRec.S2:=PFR^.Name;
  if (ExecResource(dlgDBFGoto, GotoRec) <> cmOK) Or
     ((GotoRec.S1 = ItoS(Delta.Y+1)) And (GotoRec.S2 = PFR^.Name)) then Exit;
  FldCnt:=0;
  GotoRec.S2:=UpStrg(DelSpaces(GotoRec.S2));
  Done:=False;
  If PFR^.Name <> GotoRec.S2 Then
   While (Not Done) And (FldCnt < dbFile^.NumFields) Do
    Begin
     PFR:=DBFile^.GetFieldRec(FldCnt);
     If PFR^.Name = GotoRec.S2 Then Done:=True Else Inc(FldCnt);
    End;
  If Done Then Delta.X:=FldCnt;
  If GotoRec.S1 <> ItoS(Delta.Y+1) Then
   Begin
    Done:=True;
    Delta.Y:=StoI(GotoRec.S1)-1;
   End;
  If Done Then DrawView;
 End;
{--- finish -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
 procedure LoadXlatTable;  {JO}
   var FN: String;
       More: Boolean;
       None: Boolean;
       OldFN: String; {JOHN_SW}
 begin
   FN := CnvString(XlatFile);
   OldFN:=FN; {JOHN_SW}
   if FN <> '' then FN := FN + '.xlt';
   More := True;
   None := UseXlat;
   FN := GetFileNameMenu(SourceDir+'XLT\','*.XLT', FN, True, More, None);
   if None then begin
    KeyMap:=kmAscii;
    SetXlatFile('', True);
    UseXlat := False;
    DisposeStr(XlatFile); XlatFile:=nil;
    Exit;
   end;
   if More then FN := GetFileNameDialog(SourceDir+'XLT\*.XLT',
                                        GetString(dlSelectXLT),
                                        GetString(dlOpenFileName),
                                        fdOKButton + fdHelpButton,
                                        hsOpenXLT);
   DisposeStr(XlatFile); XlatFile:=nil;
   if FN <> '' then SetXlatFile(FN, True)
   else if UseXlat then SetXlatFile(OldFN, True); {JOHN_SW}
 end; {JO}

 procedure NextXLat;
 begin
  case KeyMap of
    kmAscii: KeyMap := kmAnsi;
    kmAnsi: if Koi8rKeyMap then KeyMap := kmKoi8r
             else KeyMap := kmAscii;
    kmKoi8r: KeyMap := kmAscii;
    else     KeyMap := kmAscii;
  end;
  SetXlatFile('', True); {John_SW 18-03-2003}
 end;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

        {-DataCompBoy-}
 procedure ViewMemo;
  var P: PByteArray;
      PFR: PFieldRec;
      S: String;
      L, ML: LongInt;
      I: Integer;
      D: PDialog;
      R: TRect;
      PV: PView;
      MemoStream: PStream;
      Dr: String;
      Nm: String;
      Xt: String;
 begin
  P := GetRecord(Delta.Y); if P = nil then Exit;
  PFR := DBFile^.GetFieldRec(Delta.X); if PFR = nil then Exit;
  if (PFR^.Who <> 'M') then Exit;
  Move(P^[PFR^.Pos], S[1], PFR^.Len); S[0] := Char(PFR^.Len);
  Val(DelSpaces(S), L, I);
  ML:=0;
  lFSplit(FileName^, Dr, Nm, Xt);
  if (XT[0] = #4) and (UpStrg(XT) <> '.DBF') then XT[4] := 'T' else XT := '.FPT';
  MemoStream := New(PDosStream, Init(Dr+Nm+XT, stOpenRead));
  if (MemoStream = nil) or (MemoStream^.Status <> stOK) then
   begin
    if MemoStream <> nil then Dispose(MemoStream,Done);
    MemoStream := New(PBufStream, Init(Dr+Nm+'.dbt', stOpenRead, 512));
    If (MemoStream = nil) or (MemoStream^.Status <> stOK) then begin
     if MemoStream <> nil then Dispose(MemoStream, Done); MemoStream := nil;
     exit;
    end;
    If L <> 0 Then {JOHN_SW}
     begin
      L := L * 512;
      MemoStream^.Seek(L);
      repeat MemoStream^.Read(Nm[1], 1); if Nm[1] <> #$1A then Inc(ML) else Break
      until MemoStream^.Status <> stOK;
      if (MemoStream = nil) or (MemoStream^.Status <> stOK) then
       begin
        ErrMsg(dlDBViewNoMemo);
        if MemoStream <> nil then Dispose(MemoStream,Done); MemoStream:=nil;
        Exit;
       end;
     end;
   end else
   begin
    If L <> 0 Then {JOHN_SW}
     begin
      MemoStream^.Seek(4);
      MemoStream^.Read(ML, 4);
      asm
       mov ax, word ptr ML
       mov bx, word ptr ML+2
       xchg al, ah
       xchg bl, bh
       mov word ptr ML, bx
       mov word ptr ML+2, ax
      end;

      L := L * ML;
      MemoStream^.Seek(L+4);
      ML := MemoStream^.GetPos;
      ML := 0;
      MemoStream^.Read(ML, 4);
      asm
       mov ax, word ptr ML
       mov bx, word ptr ML+2
       xchg al, ah
       xchg bl, bh
       mov word ptr ML, bx
       mov word ptr ML+2, ax
      end;
      Inc(L, 8);
    end;
   end;

  MemoStream := New(PMemoStream, Init(MemoStream, L, ML));
  if MemoStream^.Status <> stOK then
   begin Dispose(MemoStream,Done); MemoStream:=nil; Exit end;
  Desktop^.GetExtent(R);
  R.Grow(-2,-2);
  R.Assign(1,1,70,20);
  D := New(PDialog, Init(R, GetString(dlDBViewViewMemo)));
  D^.Options := D^.Options or ofCentered;

  R.Assign(D^.Size.X-2, 2, D^.Size.X-1, D^.Size.Y - 4);
  PV := New(PViewScroll, Init(R));
  D^.Insert(PV);
  PV^.GetPalette^ := CScrollBar;

  R.Assign(2, 2, D^.Size.X-2, D^.Size.Y-4);
  PV := New(PNFileViewer, Init(R, MemoStream, '', '', PV, False, False));

  PNFileViewer(PV)^.KeyMap := KeyMap;      {John_SW 14-03-2003}
  PNFileViewer(PV)^.UseXLat := UseXLat;    {John_SW 18-03-2003}
  If Not UseXLat Then
   PNFileViewer(PV)^.SetXlatFile('', True) {John_SW 14-03-2003}
  Else
   PNFileViewer(PV)^.SetXlatFile(CnvString(XlatFile), True); {John_SW 18-03-2003}

  D^.Insert(PV);

  R.Assign(30, D^.Size.Y - 3, 40, D^.Size.Y - 1);
  PV := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
  PV^.Options := PV^.Options or ofCenterX;
  D^.Insert(PV);

  D^.SelectNext(False);

  D := PDialog(Application^.ValidView(D));
  if D = nil then Exit;
  Desktop^.ExecView(D);
  Dispose(D,Done);
 end;
        {-DataCompBoy-}

 procedure ContSearch(StartSearch: Boolean);
  var FirstFld,LastFld,CurFld,CurRow: LongInt;
      PFR: PFieldRec;
      S, SS: String;
      P: PByteArray;
      V: PWhileView;
      R: TRect;
      T: TEventTimer;
 begin
  SS := SearchData.S;
  if SearchData.Opt and 1 = 0 then UpStr(SS);
{--- start -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}
  If (SearchData.Direction = 2) And (StartSearch) Then
   CurRow := 0
  Else
   CurRow := Delta.Y;

  If SearchData.Scope = 0 Then Begin FirstFld:=Delta.X; LastFld:=Delta.X End
  Else Begin FirstFld:=0; LastFld:=dbFile^.NumFields-1; End;
  CurFld:=Delta.X;
  If Not StartSearch Then
   If SearchData.Scope = 0 Then
    If SearchData.Direction = 1 Then Dec(CurRow)
    Else Inc(CurRow)
   Else
    If SearchData.Direction = 1 Then Dec(CurFld)
    Else Inc(CurFld);
{--- finish -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}

  R.Assign(1,1,30,9);
  New(V, Init(R));
  V^.Top := GetString(dlSearching) + '"' + Cut(SearchData.S, 40) + '"' ;
  V^.Write(1, GetString(dlPercentComplete));
  Desktop^.Insert(V);
  Abort:=False;
  NewTimer(T, 1);
  while ((CurRow < DBFile^.NumRec) and (SearchData.Direction <> 1)) or
        ((SearchData.Direction = 1) and (CurRow >= 0)) do
   begin
    if TimerExpired(T) then begin
     DispatchEvents(V, Abort);
     if Abort then break;
     V^.Write(1, StrGrd(DBFile^.NumRec, CurRow, 30, False));
     V^.Write(2, ZtoS(Percent(DBFile^.NumRec,CurRow))+'%');
     NewTimer(T, 1);
    end;
    P := GetRecord(CurRow);
    while ((CurFld <= LastFld) and (SearchData.Direction <> 1)) or
          ((SearchData.Direction = 1) and (CurFld >= FirstFld)) do
      begin
        If Abort then Break;
        PFR := DBFile^.GetFieldRec(CurFld);
        Move(P^[PFR^.Pos], S[1], PFR^.Len);
        S[0] := Char(PFR^.Len);

        If PFR^.Who = 'D' Then {John_SW  10-10-2002}
         S:=DateToHuman(S);    {We have to convert date field}

        If SS = '' Then        {John_SW  14-11-2002}
         S:=DelSpaces(S);      {String to find is empty-searching empty field}

        S:=KeyMapConvertStr(S, True);    {John_SW 14-03-2003}
        if SearchData.Opt and 1 = 0 then UpStr(S);
        if (System.Pos(SS, S) > 0) Or (SS = S) then
         begin
          Delta.Y := CurRow;
          Delta.X := CurFld;
          DrawView;
          V^.Free;
          Exit;
         end;
       if SearchData.Direction = 1 then Dec(CurFld) else Inc(CurFld);
      end;
    If SearchData.Direction = 1 Then CurFld := LastFld   {John_SW 20-10-2002}
    Else CurFld := FirstFld;                            {John_SW 20-10-2002}
    if SearchData.Direction = 1 then Dec(CurRow) else Inc(CurRow);
   end;
  V^.Free;
  if not Abort then ErrMsg(dlDBViewSearchNot);
 end;

 procedure StartSearch;
 begin
  if ExecResource(dlgDbFind, SearchData) <> cmOK then Exit;
  ContSearch(True);
 end;

{ -------- Eugeny Zvyagintzev ---------}

 Procedure DeleteRecord;
  Var
     p: PByteArray;
  Begin
   Repeat
    Case DBFile^.WriteMode Of
     0 : Begin
          DBFile^.BaseFile.Done;
          DBFile^.OpenMode(stOpen);
          Inc(DBFile^.WriteMode);
          If DBFile^.BaseFile.Status = stOK Then Inc(DBFile^.WriteMode) Else DBFile^.OpenMode(stOpenRead);
         End;
     1 : Begin ErrMsg(dlDBCantEdit); Exit End;
     2 : Break;
    End;
   Until False;
   P := GetRecord(Delta.Y);
   If P^[0] = $2A Then
    P^[0]:=$20
   Else
    P^[0]:=$2A;
   DBFile^.Seek(Delta.Y);
   DBFile^.BaseFile.Write(P^[0],DBFile^.RecLen);
  End;

{ -------- Eugeny Zvyagintzev ---------}

 procedure EditField;
   var P: PChar;
       PFR: PFieldRec;
       S: String;
       R: Real;
       I: Integer;
       MM, DD, YY: Word;
       II, N1, N2, N3: Word;
       LL: Array [0..3] of LongInt;
       FldLen : AWord; { Kirill }

   procedure StoreField;
   begin
     S := AddSpace(S, FldLen);
     S:=KeyMapConvertStr(S, False);    {John_SW 14-03-2003}
     Move(S[1], P[PFR^.Pos], FldLen);
     DBFile^.Seek(Delta.Y);
     DBFile^.BaseFile.Seek(DBFile^.BaseFile.GetPos + PFR^.Pos);
     DBFile^.BaseFile.Write(S[1], FldLen);
   end;

   function GetDig: Word;
     var D: String;
   begin
     D := '';
     while (II <= Length(S)) and ((S[II] < '0') or (S[II] > '9')) do Inc(II);
     while (II <= Length(S)) and ((S[II] >= '0') and (S[II] <= '9')) do
       begin AddStr(D, S[II]); Inc(II); end;
     GetDig := StoI(D);
   end;

 label redat;
 begin
   repeat
     case DBFile^.WriteMode of
       0 : begin
             DBFile^.BaseFile.Done;
             DBFile^.OpenMode(stOpen);
             Inc(DBFile^.WriteMode);
             if DBFile^.BaseFile.Status = stOK then Inc(DBFile^.WriteMode) else DBFile^.OpenMode(stOpenRead);
           end;
       1 : begin ErrMsg(dlDBCantEdit); Exit end;
       2 : Break;
     end;
   until False;
   PFR := DBFile^.GetFieldRec(Delta.X);
   if PFR^.Who = 'M' then Exit;
   P := GetRecord(Delta.Y);
   FldLen:=PFR^.Len;
   if FldLen > 252 then { Больше чем 252 нельзя, иначе будет вылетать  }
     FldLen:=252; { Kirill } { Здесь необходимо вывести предупреждение }
                             { о том, что будут отредактированы только }
                             { первые 252 символа длинного поля.       }
   Move(P[pfr^.pos], S[1], FldLen); S[0] := Char(FldLen); { Kirill }
   if PFR^.Who <> 'D' then begin
    if PFR^.Who <> 'C' then DelLeft(S);
    DelRight(S); {Oleg Redut}
   end;
   S:=KeyMapConvertStr(S, True);    {John_SW 14-03-2003}
   case PFR^.Who of
     'C','N': if InputBox(GetString(dlDBEditField), GetString(dlDBValue), S, FldLen, hsEditDBF) = cmOK then
                begin
                  if PFR^.Who = 'N' then
                    begin
                      DelSpace(S);
                      Val(S, R, I);
                      Str(R:FldLen:PFR^.Dec, S);
                      S := PredSpace(S, FldLen);
                    end;
                  StoreField;
                end;
         'D': begin
                S := DateToHuman(S);{!}
                redat:
                if InputBox(GetString(dlDBEditField), GetString(dlDBValue), S, 10, hsEditDBF) = cmOK then
                  begin
                    II := 1;
                    N1 := GetDig; N2 := GetDig; N3 := GetDig;
                    GetDate(II, MM, DD, YY);
                    case CountryInfo.DateFmt of
                      dfMDY: begin MM := N1; DD := N2; YY := N3; end;
                      dfDMY: begin MM := N2; DD := N1; YY := N3; end;
                      dfYMD: begin MM := N2; DD := N3; YY := N1; end;
                    end;
                    if ((MM=0) and (DD=0) and (YY=0)) then
                     S:=' '+' '+' '+' '+' '+' '+' '+' '{8 spaces!}
                    else begin
                     if YY < 100 then YY := (II div 100) * 100 + YY;
                     if (MM<1) or (MM>12) or (DD<1) or
                        (DD>31) or ((DD=31) and (MM in [4, 6, 9, 11])) or
                        ((MM=2) and ((DD=30) or ((DD=29) and (YY mod 4<>0))))
                      then goto redat;
                     LL[0] := YY mod 10000; LL[1] := MM mod 100; LL[2] := DD mod 100;
                     FormatStr(S, '%04d%02d%02d', LL);
                    end;
                    StoreField;
                  end;
              end;
         'L': begin
                if UpStrg(S) = 'T' then S := 'F' else S := 'T';
                StoreField;
              end;
   end;
 end;

 procedure CE; begin ClearEvent(Event) end;
 procedure CED; begin CE; DrawView end;

 var F: lFile; {DataCompBoy}
     P: TPoint;
     RD: Word;

begin
 inherited HandleEvent(Event);
 case Event.What of
  evBroadcast: case Event.Command of
              cmFindView:
                if PString(Event.InfoPtr)^=FileName^ then begin
                  if Owner<>nil then begin
                    Owner^.Select; { Flash }
                    ClearEvent(Event);
                  end;
                end;
             end;
  evCommand: case Event.Command of
              cmReboundPanel: DrawView; {John_SW 10-10-2002 after country settings changed}
              cmClose, cmKillUsed: if KillAfterUse then
                                    begin
                                      if DBFile <> nil then
                                        begin
                                          if Buf <> nil then FreeMem(Buf, NumRec*DBFile^.RecLen);
                                          Dispose(DBFile,Done);
                                          Buf := nil;
                                          dbFile := nil;
                                        end;
                                      EraseTempFile(FileName^);
                                      KillAfterUse := False;
                                      if Event.Command = cmClose then GlobalMessage(evCommand, cmPanelReread, nil);
                                    end;
              cmGetName: PString(Event.InfoPtr)^ := GetString(dlDBViewName) + ' - '
                         + Cut(FileName^, 49 - Length(GetString(dlDBViewName)));
              cmShowFields: begin GetInfo(dlDBViewInfo); CE end;
              cmShowMemo: begin ViewMemo; CE end;
              cmDBFGoto: begin GotoRecFld; CE; end;
              cmSearchFor: begin StartSearch; CE; end;
              cmContinueSearch: begin ContSearch(False); CE; end;
              cmBackSearch: begin
                             if SearchData.Direction = 1 then begin
                              SearchData.Direction := 0;
                              ContSearch(False);
                              SearchData.Direction := 1;
                             end else begin
                              RD := SearchData.Direction;
                              SearchData.Direction := 1;
                              ContSearch(False);
                              SearchData.Direction := RD;
                             end;
                             CE;
                            end;
              cmEditDBField: begin EditField; CED end;
              cmDeleteDBRec: begin DeleteRecord; CED; end;
              cmSwitchKeyMapping: begin NextXLat; CED; end; {John_SW 14-03-2003}
              cmLoadXlatTable: begin LoadXlatTable; CED; end; {John_SW 18-03-2003}
             end;
  evKeyDown: case Event.KeyCode of
              kbEnter: begin if Delta.X < DBFile^.NumFields - 1 then Inc(Delta.X)
                                else begin Inc(Delta.Y); Delta.X := 0; end;
                             CED;
                       end;
              kbLeft: begin if Delta.X > 0 then Dec(Delta.X); CED; end;
              kbRight: begin if Delta.X < DBFile^.NumFields - 1 then Inc(Delta.X); CED; end;
              kbHome: begin Delta.X := 0; CED; end;
              kbEnd: begin Delta.X := DBFile^.NumFields - 1; CED; end;
              kbUp: begin Dec(Delta.Y); CED; end;
              kbDown: begin Inc(Delta.Y); CED; end;
              kbPgUp: begin Dec(Delta.Y, Size.Y - 2); CED; end;
              kbPgDn: begin Inc(Delta.Y, Size.Y - 2); CED; end;
              kbCtrlPgUp: begin Delta.Y := 0; CED; end;
              kbCtrlPgDn: begin Delta.Y := DBFile^.NumRec - 1; CED; end;
{              kbCtrlD: begin DeleteRecord; CED; end;}
              kbESC: begin Message(Owner, evCommand, cmClose, nil); CE end;
             end;
  evMouseDown: begin
                RD := RepeatDelay; RepeatDelay := 0;
                repeat
                 MakeLocal(Event.Where, P);
                 if MouseInView(Event.Where) then
                  if P.X < Size.X div 4 then Message(@Self, evKeyDown, kbLeft, nil) else
                   if P.X >= (Size.X*3) div 4 then Message(@Self, evKeyDown, kbRight, nil) else
                    if P.Y < Size.Y div 2 then Message(@Self, evKeyDown, kbUp, nil) else
                     Message(@Self, evKeyDown, kbDown, nil)
                until not MouseEvent(Event, evMouseMove+evMouseAuto);
                RepeatDelay := RD; CE
               end;
 end;
end;


procedure TDBViewer.Draw;
 var B: Array [0..1024] of record C: Char; A: Byte; end;
     FN: Array [0..132] of PFieldRec;
     C1, C2 ,C3: Word;
     C: Byte;
     I, J, K, NFN: Integer;
     FldLen : Word; { Kirill }
     FldPart: Byte;
     Idx: LongInt;
     PFR: PFieldRec;
     P: PByteArray;
     S: String;
begin
 C1 := GetColor(1); C2 := GetColor(2); C3 := GetColor(3);
 if Delta.Y < 0 then Delta.Y := 0;
 if Delta.Y >= DBFile^.NumRec then Delta.Y := DBFile^.NumRec - 1;
 if Pos.Y > Delta.Y then Pos.Y := Delta.Y;
 if Pos.Y + Size.Y - 2 < Delta.Y then Pos.Y := Delta.Y - Size.Y + 2;
 if Pos.Y < 0 then Pos.Y := 0;
 if Delta.X > DBFile^.NumFields-1 then Delta.X := DBFile^.NumFields-1;
 if Pos.X > Delta.X then Pos.X := Delta.X;
 {if Indicator <> nil then Indicator^.Draw;}
{ -------- Eugeny Zvyagintzev ---------}
 If (VerticalScrollBar <> Nil) And (VerticalScrollBar^.Value <> Delta.Y) Then
  Begin
   VerticalScrollBar^.SetValue(Delta.Y);
   If Indicator <> Nil Then Indicator^.Draw;
  End;
 If (HorizontalScrollBar <> Nil) And (HorizontalScrollBar^.Value <> Delta.X) Then
  Begin
   HorizontalScrollBar^.SetValue(Delta.X);
   If Indicator <> Nil Then Indicator^.Draw;
  End;
{ -------- Eugeny Zvyagintzev ---------}
 repeat
  MoveChar(B, ' ', C1, Size.X);
  J := Pos.X; NFN := 0; I := 1;
  FldPart:=0;
  While (I < Size.X) and (J < DBFile^.NumFields) do
   begin
    PFR := DBFile^.GetFieldRec(J);
    FN[NFN] := PFR;
    if PFR <> nil then
     begin
      K:=PFR^.Ln; if K > Size.X-I then K:=Size.X-I; { Kirill }
      MoveStr(B[I], CenterStr(PFR^.Name, {PFR^.Ln} K), C1);
      Inc(I, PFR^.Ln+1);
     end;
    if I < Size.X then Inc(NFN) Else FldPart:=1;
    Inc(J);
   end;
  if (Delta.X > Pos.X + NFN - 1) and (Pos.X <> Delta.X) then Inc(Pos.X) else Break;
 until False;

 WriteLine(0,0, Size.X, 1, B);
 for K := 1 to Size.Y - 1 do
  begin
   MoveChar(B, ' ', C2, Size.X);
   if DBFile^.NumRec > 0 then begin
    J := Delta.X; I := 1; Idx := Pos.Y+LongInt(K)-1;
    P := GetRecord(Idx);
    {if (NFN = 0) and (P <> nil) then NFN := 1;}
    if P <> nil then B[0].C := Char(P^[0]);
    if P <> nil then

    for J := 0 to NFN+FldPart - 1 do
     begin
      PFR := FN[J];
      if (Delta.Y = Idx) and (Pos.X + J = Delta.X) then C := C3 else C := C2;
      if PFR <> nil then
      if PFR^.Who = 'M' then
      begin
       Move(P^[PFR^.Pos], S[1], PFR^.Len);
       S[0] := Char(PFR^.Len);
       if StoI(S) > 0 then S := '  Memo  ' else S := '  memo  ';
       MoveStr(B[I], S, C);
       Inc(I, PFR^.Ln + 1);
      end else
      if PFR^.Who = 'D' then
      begin
        Move(P^[PFR^.Pos], S[1], PFR^.Len); S[0] := Char(PFR^.Len);
        MoveStr(B[I], DateToHuman(S), C);
        Inc(I, PFR^.Ln + 1);
      end else
      begin { Kirill }
        FldLen:=PFR^.Len;
        if FldLen > Size.X-I then FldLen:=Size.X-I;
        Move(P^[PFR^.Pos], S[1], FldLen); S[0] := Char(FldLen);
        S:=KeyMapConvertStr(S, True);    {John_SW 14-03-2003}
        MoveStr(B[I], S, C);
        Inc(I, PFR^.Ln + 1);
      end;
     end;
    end;
   WriteLine(0,K,Size.X,1,B);
  end;
end;

procedure TDBIndicator.Draw;
 var B: TDrawBuffer;
     S,S1: String; {John_SW 18-03-2003}
     C: Byte;
     PFR: PFieldRec;
begin
 {10171114/10171114═══[C 10,10]═WIN═}
 If (DBViewer = Nil) Or (DBViewer^.DBFile = Nil) Then Exit;
 if DBViewer^.Delta.Y < 0 then DBViewer^.Delta.Y := 0;
 if DBViewer^.Delta.X > DBViewer^.DBFile^.NumFields-1 then DBViewer^.Delta.X := DBViewer^.DBFile^.NumFields-1;
 if Origin.Y <> Owner^.Size.Y - 1 then MoveTo(Origin.X, Owner^.Size.Y - 1);
 PFR:=DBViewer^.DBFile^.GetFieldRec(DBViewer^.Delta.X);
 if (State and sfDragging <> 0) Or (State and sfActive = 0) then C := 196
 Else C:=205;
 S:=AddSpace(ItoS(DBViewer^.Delta.Y+1)+'/'+Itos(DBViewer^.DBFile^.NumRec),10);
 Replace(' ',Char(C),S);
 S1:=AddSpace(Itos(PFR^.Len)+','+Itos(PFR^.Dec)+']',7);
 Replace(' ',Char(C),S1);
 S:=S+Char(C)+'['+PFR^.Who+' '+S1;
{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
 if (DBViewer^.UseXlat) And (DBViewer^.XLatFile <> Nil) then
  S:=S+Char(C)+Cut(DBViewer^.XLatFile^,8)
 Else
 case DBViewer^.KeyMap of
  kmAscii: S:=S+Char(C)+'DOS';
  kmAnsi:  S:=S+Char(C)+'WIN';
  kmKoi8r: S:=S+Char(C)+'KOI';
 else
  S:=S+Char(C)+'XXX';
 end;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
 S:=Copy(S,1,Owner^.Size.X-4);
 if Byte(S[0]) <> Size.X then begin GrowTo(Byte(S[0]), 1); Exit; end;
{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
 if State and sfDragging <> 0 then C := PWindow(Owner)^.Frame^.GetColor($05)
 else if State and sfActive = 0 then C := PWindow(Owner)^.Frame^.GetColor($01)
 else C := PWindow(Owner)^.Frame^.GetColor($03);
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
 MoveStr(B, S, C);
 WriteLine(0,0,Size.X,1,B);
end;

constructor TDBWindow.Init;
 var R  : TRect;
     I  : Integer;
     s  : String;
begin
 DeskTop^.GetExtent(R);
 I:=PosChar('|', FName);
 If I > 0 Then
  begin
   S:=Copy(FName, 1, I-1);
   FName:=Copy(FName, I+1, 255);
  end;
 FName := lFExpand(FName);
 If I <= 0 Then
  S:=FName;
 TWindow.Init(R, S, 0);
 RealName:=S;
 Options := Options or ofTileable;
 Flags := Flags or wfMaxi;
 GetExtent(R); R.Grow(-1, -1);
 P := New(PDBViewer, Init(R, FName,FileIsDBF));
 if P = nil then Fail;
 Insert(P);
 if not P^.isValid then begin
  Done;
  Fail;
 end;
{ -------- Eugeny Zvyagintzev ---------}
 R.A.X:=R.B.X;
 R.B.X:=R.B.X+1;
 VSB:=New(PDBScrollBar,Init(R));
 VSB^.Max:=P^.DBFile^.NumRec-1;
 VSB^.GrowMode:=gfGrowLoX + gfGrowHiX + gfGrowHiY;
 VSB^.DBViewer:=P;
 VSB^.SetStep(Size.Y-4,1);
 VSB^.ScrollBarType:=sbVertical;
 Insert(VSB);
 P^.VerticalScrollBar:=VSB;
 GetBounds(R);
 R.A.X:=R.A.X+49;
 R.B.X:=R.B.X-2;
 R.A.Y:=R.B.Y-1;
 HSB:=New(PDBScrollBar,Init(R));
 HSB^.Max:=P^.DBFile^.NumFields-1;
 HSB^.GrowMode:=gfGrowLoY + gfGrowHiX + gfGrowHiY;
 HSB^.DBViewer:=P;
 HSB^.ScrollBarType:=sbHorizontal;
 Insert(HSB);
 P^.HorizontalScrollBar:=HSB;
{ -------- Eugeny Zvyagintzev ---------}
 R.A.X := 2; R.B.X := 34; R.A.Y := Size.Y - 1; R.B.Y := Size.Y;
 P1 := New(PDBIndicator, Init(R));
 P1^.DBViewer := P;
 P1^.GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;
 Insert(P1);
 P^.Indicator := P1;
end;

{--- start -------- Eugeny Zvyagintzev ---------}
{Now horizontal scrollbar can't hide bottom-right resize corner}
Procedure TDBWindow.ChangeBounds;
Var
   R1: TRect;
Begin
 Inherited ChangeBounds(R);
 P^.HorizontalScrollBar^.GetBounds(R1);
 R1.B.X:=Size.X-2;
 P^.HorizontalScrollBar^.SetBounds(R1);
End;
{--- finish -------- Eugeny Zvyagintzev ---------}

constructor TDBWindow.Load;
 begin
  inherited Load( S );
   GetSubViewPtr( S, P );
   GetSubViewPtr( S, P1 );
   P1^.DBViewer := P;
   P^.Indicator := P1;
   GetSubViewPtr( S, VSB );
   VSB^.DBViewer := P;
   VSB^.ScrollBarType:=sbVertical;
   P^.VerticalScrollBar:=VSB;
   GetSubViewPtr( S, HSB );
   HSB^.DBViewer := P;
   HSB^.ScrollBarType:=sbHorizontal;
   P^.HorizontalScrollBar:=HSB;

   Redraw ;
 end;

Procedure TDBWindow.Store;
 begin
   inherited Store( S );
     PutSubViewPtr( S, P );
     PutSubViewPtr( S, P1 );
     PutSubViewPtr( S, VSB );
     PutSubViewPtr( S, HSB );
   end;

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
procedure TDBWindow.SetState(AState: Word; Enable: Boolean);
Begin
 inherited SetState(AState, Enable);
 If P1 <> nil Then
  Begin
   P1^.SetState(AState,Enable);
   P1^.Draw;
  End;
End;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

Destructor TDBWindow.Done;
begin
 StoreViewInfo(@Self);
 Inherited Done;
end;

{$ifdef db}
const
     cmOpenDBF = 1000;


procedure TMyApp.InitStatusLine;
 var R: TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R,
    NewStatusDef(0, $FFFF,
      NewStatusKey('~F3~ Open', kbF3, cmOpenDBF,
      NewStatusKey('~Alt-X~ Exit', kbAltX, cmQuit, nil)), nil)));
end;

procedure TMyApp.HandleEvent;
 var S: String;
     D: PFileDialog;
begin
 inherited HandleEvent(Event);
 if (Event.What = evCommand) and (Event.Command = cmOpenDBF) then
  begin
   D := PFileDialog(Application^.ValidView(New(PFileDialog,
         Init('*.dbf', '', '', fdOpenButton, 1))));
   if D = nil then Exit;
   if Desktop^.ExecView(D) <> cmCancel then D^.GetFileName(S);
   Dispose(D,Done);
   if S = '' then Exit;
   {$IFNDEF OS2}
   S:=lfGetLongFileName(S);
   {$ENDIF}
   InsertWindow(New(PDBWindow, Init(S)));
  end;
end;

var MyApp: TMyApp;

begin
 Inc(SkyEnabled);
 MyApp.Init;
 MyApp.Run;
 MyApp.Done;
{$endif}
end.
