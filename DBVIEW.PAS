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
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit DBView;

interface
uses
  Lfn, DBWatch, Objects, Views, Drivers, Dos, Memory, Dialogs,
  RStrings, FViewer, Commands, U_KeyMap, DNStdDlg;


type

  TDBPoint = record
    X: integer;
    Y: longInt;
    end;

  PDBIndicator = ^TDBIndicator;
  PDBViewer = ^TDBViewer;

  { -------- Eugeny Zvyagintzev ---------}
  PDBScrollBar = ^TDBScrollBar;
  TDBScrollBar = object(TScrollBar)
    ScrollBarType: integer;
    DBViewer: PDBViewer;
    procedure HandleEvent(var Event: TEvent); virtual;
    function GetPalette: PPalette; virtual;
    end;
  { -------- Eugeny Zvyagintzev ---------}

  TDBViewer = object(TView)
    FileName: PString;
    DBFile: PDBFile;
    isValid, KillAfterUse: boolean;
    Buf: Pointer;
    SearchString: PString;
    StartRec: longInt;
    NumRec: word;
    Indicator: PDBIndicator;
    Delta, Pos: TDBPoint;
    VerticalScrollBar: PDBScrollBar;
    HorizontalScrollBar: PDBScrollBar;
    XLAT: TXlat; {John_SW 18-03-2003}
    UseXLat: boolean; {John_SW 18-03-2003}
    XLatFile: PString; {John_SW 18-03-2003}
    KeyMap: TKeyMap; {John_SW 14-03-2003}
    Constructor Init(R: TRect; FName: String; var FileIsDBF: boolean);
      {DataCompBoy}
    function Valid(Command: word): boolean; virtual;
    function GetRecord(n: longInt): Pointer;
    procedure GetInfo(StrIdx: TStrIdx);
    procedure HandleEvent(var Event: TEvent); virtual;
    destructor Done; virtual;
    procedure Draw; virtual;
    function GetPalette: PPalette; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    function Failed(var FileIsDBF: boolean): boolean;
    function KeyMapConvertStr(s: String; toAscii: boolean): String;
      {-$VIV} {John_SW 14-03-2003}
    procedure SetXlatFile(const aFName: String; DoRedraw: boolean);
      {John_SW 18-03-2003}
    end;

  PMemoStream = ^TMemoStream;
  TMemoStream = object(TStream)
    StartPos: longInt;
    Length: longInt;
    BaseStream: PStream;
    Constructor Init(AStream: PStream; Start, len: longInt);
    destructor Done; virtual;
    function GetPos: longInt; virtual;
    function GetSize: longInt; virtual;
    procedure Read(var Buf; Count: word); virtual;
    procedure Seek(Pos: longInt); virtual;
    procedure Truncate; virtual;
    procedure Write(const Buf; Count: word); virtual;
    end;

  TDBIndicator = object(TView)
    DBViewer: PDBViewer;
    procedure Draw; virtual;
    {Constructor Load(var S : Tstream);
       Procedure Store(var S : TStream);}
    end;

  PFieldListBox = ^TFieldListBox;
  TFieldListBox = object(TListBox)
    function GetText(Item: longInt; MaxLen: integer): String;
      virtual;
    end;

  PDBWindow = ^TDBWindow;
  TDBWindow = object(TFileWindow)
    P: PDBViewer;
    P1: PDBIndicator;
    VSB: PDBScrollBar;
    HSB: PDBScrollBar;
    RealName: String;
    {--- start -------- Eugeny Zvyagintzev ---------}
    Constructor Init(FName: String; var FileIsDBF: boolean);
    {--- finish -------- Eugeny Zvyagintzev ---------}
    function GetPalette: PPalette; virtual;
    Constructor Load(var s: TStream);
    procedure Store(var s: TStream);
    procedure SetState(AState: word; Enable: boolean); virtual;
      {John_SW 14-03-2003}
    destructor Done; virtual;
    end;

const
  CDBWindow = #166#167#168#169#170#171#212#213;
  CDBViewer = #4#5#6;
  CDBScrollBar = #7#8#8;

const
  SearchData: record
    s: String[250]; {Inputline}
    OPT: word; {Checkbox}
    Scope: word; {Radiobuttons}
    Direction: integer; {Radiobuttons}
    end = (s: ''; OPT: 0; Scope: 0; Direction: 0);

implementation
uses
  advance, advance1, advance2, advance3, Startup, DNHelp, DNApp,
    Messages,
  Histries, Gauge, xTime, DnIni;

function MaxAvail: longInt;
  begin
    MaxAvail := MemAdjust(System.MaxAvail);
  end;

function TFieldListBox.GetText;
  var
    P: PFieldRec;
    s: String;
    s1, s2, S3, S4: String[20];
    M: array[0..4] of Pointer;
  begin
    P := List^.At(Item);
    longInt(M[0]) := Item+1;
    M[1] := @P^.Name;
    M[2] := @S1;
    M[3] := @S2;
    M[4] := @S3;
    case P^.Who of
      'N':
        s1 := GetString(dlDBNumeric);
      'C':
        s1 := GetString(dlDBCharacter);
      'M':
        s1 := GetString(dlDBMemo);
      'L':
        s1 := GetString(dlDBLogical);
      'D':
        s1 := GetString(dlDBDate);
      'F':
        s1 := GetString(dlDBFloat);
      'P':
        s1 := GetString(dlDBPicture);
    end {case};
    s2 := ItoS(P^.len);
    if (P^.Who = 'N') or (P^.Who = 'F') then
      S3 := ItoS(P^.Dec)
    else
      S3 := '';
    if (P^.Who = 'C') and (P^.Dec <> 0) then
      S3 := ItoS(P^.Dec); {JOHN_SW}
    FormatStr(s, ' %-4d%-12s%-13s%-11s%-8s', M);
    GetText := s;
  end { TFieldListBox.GetText };

{ -------- Eugeny Zvyagintzev ---------}
function TDBScrollBar.GetPalette: PPalette;
  const
    s: String[Length(CDBScrollBar)] = CDBScrollBar;
  begin
    GetPalette := @S;
  end;

procedure TDBScrollBar.HandleEvent(var Event: TEvent);
  begin
    inherited HandleEvent(Event);
    case ScrollBarType of
      sbVertical:
        if DBViewer^.Delta.Y <> Value then
          begin
            DBViewer^.Delta.Y := Value;
            DBViewer^.DrawView;
            DBViewer^.Indicator^.Draw;
          end;
      sbHorizontal:
        if DBViewer^.Delta.X <> Value then
          begin
            DBViewer^.Delta.X := Value;
            DBViewer^.DrawView;
            DBViewer^.Indicator^.Draw;
          end;
    end {case};
  end;
{ -------- Eugeny Zvyagintzev ---------}

Constructor TMemoStream.Init;
  begin
    TObject.Init;
    StartPos := Start;
    Length := len;
    BaseStream := AStream;
    if AStream^.Status = stOK then
      AStream^.Seek(StartPos);
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;
  end;

destructor TMemoStream.Done;
  begin
    Dispose(BaseStream, Done);
    BaseStream := nil;
    TObject.Done;
  end;

function TMemoStream.GetPos: longInt;
  var
    l: longInt;
  begin
    l := BaseStream^.GetPos-StartPos;
    if l < 0 then
      begin
        BaseStream^.Seek(StartPos);
        l := 0;
      end;
    GetPos := l;
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;
  end;

function TMemoStream.GetSize: longInt;
  begin
    GetSize := Length;
  end;

procedure TMemoStream.Read(var Buf; Count: word);
  begin
    BaseStream^.Read(Buf, Count);
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;
  end;

procedure TMemoStream.Seek(Pos: longInt);
  var
    l: longInt;
  begin
    l := Pos+StartPos;
    BaseStream^.Seek(l);
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;
  end;

procedure TMemoStream.Truncate;
  begin
    BaseStream^.Truncate;
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;
  end;

procedure TMemoStream.Write(const Buf; Count: word);
  begin
    BaseStream^.Write(Buf, Count);
    ErrorInfo := BaseStream^.ErrorInfo;
    Status := BaseStream^.Status;

    {Move(AStream^, Self, sizeof(TStream));}
  end;

{-DataCompBoy-}
Constructor TDBViewer.Init;
  begin
    inherited Init(R);
    KillAfterUse := TempFile <> '';
    Options := Options or ofSelectable;
    GrowMode := gfGrowHiX+gfGrowHiY;
    EventMask := $FFFF;
    SearchString := nil;
    HelpCtx := hcDBView;
    Buf := nil;
    FileName := NewStr(lFExpand(FName));
    if Failed(FileIsDBF) then
      isValid := False
    else if isValid then
      TempFile := '';
    KeyMap := kmAscii; {John_SW 14-03-2003}
    XLatFile := nil; {John_SW 18-03-2003}
    NullXLAT(XLAT);
    UseXLat := False; {John_SW 18-03-2003}
  end;
{-DataCompBoy-}

procedure TDBViewer.Store;
  begin
    inherited Store(s);
    s.Write(KillAfterUse, SizeOf(KillAfterUse));
    s.WriteStr(SearchString);
    s.WriteStr(FileName);
    s.Write(Delta, SizeOf(Delta));
    s.Write(Pos, SizeOf(Pos));
    s.Write(KeyMap, SizeOf(KeyMap)); {John_SW 14-03-2003}
    s.WriteStr(XLatFile); {John_SW 18-03-2003}
    s.Write(XLAT, SizeOf(XLAT)); {John_SW 18-03-2003}
    s.Write(UseXLat, SizeOf(UseXLat)); {John_SW 18-03-2003}
  end;

function DateToHuman(const s: String): String;
  var
    i: integer;
    C: Char;
    BB: byte;
    D, M: String[2];
    Y: String[4];
  begin
    Y := Copy(s, 1, 4);
    M := Copy(s, 5, 2);
    D := Copy(s, 7, 2);
    case CountryInfo.DateFmt of
      dfMDY:
        DateToHuman := M+CountryInfo.DateSep+D+CountryInfo.DateSep+Y;
      dfDMY:
        DateToHuman := D+CountryInfo.DateSep+M+CountryInfo.DateSep+Y;
      dfYMD:
        DateToHuman := Y+CountryInfo.DateSep+M+CountryInfo.DateSep+D;
    end {case};
  end;

function TDBViewer.Failed(var FileIsDBF: boolean): boolean;
  var
    i: longInt;
  begin
    Failed := False;
    New(DBFile, Init(CnvString(FileName)));

    if DBFile <> nil then
      begin
        i := 65520;
        if DBFile^.NumRec = 0 then
          begin
            GetInfo(dlDBEmptyStruc);
            {     DisposeStr(SearchString);
      Dispose(DBFile,Done); DBFile:=nil;
}TempFile := '';
            if KillAfterUse then
              EraseTempFile(CnvString(FileName));
            {     DisposeStr(FileName);}
            Failed := True;
            FileIsDBF := True;
            exit;
          end;
        if MaxAvail < i then
          i := MaxAvail;
        if longInt(i) > DBFile^.NumRec*longInt(DBFile^.RecLen) then
          i := DBFile^.NumRec*longInt(DBFile^.RecLen);
        NumRec := i div DBFile^.RecLen;
        i := NumRec*DBFile^.RecLen;
        StartRec := 0;
        if i <> 0 then
          GetMem(Buf, i);
      end;
    isValid := (DBFile <> nil) and (Buf <> nil);
    if isValid then
      begin
        DBFile^.Seek(0);
        DBFile^.Read(Buf^, NumRec);
      end;
    FileIsDBF := isValid;
  end { TDBViewer.Failed };

Constructor TDBViewer.Load;
  var
    FileIsDBF: boolean;
  begin
    inherited Load(s);
    s.Read(KillAfterUse, SizeOf(KillAfterUse));
    SearchString := s.ReadStr;
    FileName := s.ReadStr;

    if Failed(FileIsDBF) then
      Fail;

    s.Read(Delta, SizeOf(Delta));
    s.Read(Pos, SizeOf(Pos));

    s.Read(KeyMap, SizeOf(KeyMap)); {John_SW 14-03-2003}
    XLatFile := s.ReadStr;
    s.Read(XLAT, SizeOf(XLAT)); {John_SW 18-03-2003}
    s.Read(UseXLat, SizeOf(UseXLat)); {John_SW 18-03-2003}
  end;

destructor TDBViewer.Done;
  begin
    DisposeStr(SearchString);
    if Buf <> nil then
      FreeMem(Buf, NumRec*DBFile^.RecLen);
    if DBFile <> nil then
      Dispose(DBFile, Done);
    DBFile := nil;
    {--- start -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
    if not (TottalExit or Exiting) and KillAfterUse then
      begin
        EraseTempFile(FileName^);
        KillAfterUse := False;
      end;
    {--- finish -------- Eugeny Zvyagintzev ---- 07-08-2002 -----}
    DisposeStr(FileName);
    inherited Done;
  end;

function TDBViewer.GetPalette;
  const
    s: String[Length(CDBViewer)] = CDBViewer;
  begin
    GetPalette := @S;
  end;

function TDBWindow.GetPalette;
  const
    s: String[Length(CDBWindow)] = CDBWindow;
  begin
    GetPalette := @S;
  end;

function TDBViewer.GetRecord;
  var
    i: integer;
  begin
    GetRecord := nil;
    if n < 0 then
      n := 0;
    if n >= DBFile^.NumRec then
      exit;
    if (n < StartRec) or (n >= StartRec+NumRec) then
      begin
        StartRec := n-(NumRec div 2);
        if StartRec < 0 then
          StartRec := 0;
        DBFile^.Seek(StartRec);
        DBFile^.Read(Buf^, NumRec);
      end;
    GetRecord := @PByteArray(Buf)^[(n-StartRec)*DBFile^.RecLen];
  end;

function TDBViewer.Valid;
  begin
    Valid := inherited Valid(Command) and isValid;
  end;

procedure TDBViewer.GetInfo;
  var
    R: TRect;
    D: PDialog;
    P: PView;
    Data: record
      List: Pointer {PCollection};
      Focused: integer;
      end;
  begin
    R.Assign(1, 1, 50, 18);
    D := New(PDialog, Init(R, GetString(StrIdx)+Cut(FileName^, 20)));
    D^.Options := D^.Options or ofCentered;

    R.Assign(2, 2, 48, 3);
    P := New(PStaticText, Init(R, GetString(dlDBViewInfoString)));
    D^.Insert(P);

    R.Assign(46, 3, 47, 13);
    P := New(PScrollBar, Init(R));
    P^.Options := P^.Options or ofPostProcess;
    D^.Insert(P);

    R.Assign(2, 3, 46, 13);
    P := New(PFieldListBox, Init(R, 1, PScrollBar(P)));
    D^.Insert(P);

    R.Assign(30, 14, 40, 16);
    P := New(PButton, Init(R, GetString(dlOKButton), cmOK, bfDefault));
    P^.Options := P^.Options or ofCenterX;
    D^.Insert(P);

    D^.SelectNext(False);

    D := PDialog(Application^.ValidView(D));
    if D = nil then
      exit;

    Data.List := DBFile^.Fields;
    Data.Focused := Delta.X;
    D^.SetData(Data);
    if Desktop^.ExecView(D) = cmOK then
      begin
        D^.GetData(Data);
        if Delta.X <> Data.Focused then
          begin
            Delta.X := Data.Focused;
            DrawView;
          end;
      end;
    Dispose(D, Done);
  end { TDBViewer.GetInfo };

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
function TDBViewer.KeyMapConvertStr(s: String; toAscii: boolean):
    String; {-$VIV}
  begin
    if not UseXLat then
      case KeyMap of
        kmAnsi:
          if toAscii then
            KeyMapConvertStr := ansi_ascii(s)
          else
            KeyMapConvertStr := ascii_ansi(s);
        kmKoi8r:
          if toAscii then
            KeyMapConvertStr := koi8r_ascii(s)
          else
            KeyMapConvertStr := ascii_koi8r(s);
        else
          KeyMapConvertStr := s;
      end
    else if toAscii then
      begin
        XLatBuf(s[1], Length(s), XLAT);
        KeyMapConvertStr := s;
      end
    else
      begin
        CharsConvertBufBack(s[1], Length(s), String(XLAT));
        KeyMapConvertStr := s;
      end;
  end { TDBViewer.KeyMapConvertStr }; {-$VIV}

procedure TDBViewer.SetXlatFile;
  var
    dr: String;
    Nm: String;
    XT: String;
    s: TDOSStream;
    Table: array[1..256] of record
      A, B: Char
      end;
    C, CC: Char;
    i, j: integer;
    {      PC: PChar;}
  begin
    if aFName <> '' then
      begin
        lFSplit(aFName, dr, Nm, XT);
        if dr = '' then
          begin
            dr := SourceDir+'XLT\';
            XT := '.xlt';
          end;
        s.Init(dr+Nm+XT, stOpenRead);
        if (s.Status = stOK) and (s.GetSize >= 2) then
          begin
            j := Min(256, s.GetSize shr 1);
            s.Read(Table, j*2);
            NullXLAT(XLAT);
            for i := 1 to j do
              XLAT[Table[i].A] := Table[i].B;
            UseXLat := s.Status = stOK;
            if XLatFile <> nil then
              DisposeStr(XLatFile);
            XLatFile := nil;
            if UseXLat then
              XLatFile := NewStr(Nm);
          end;
        s.Done;
      end;
    if aFName = '' then
      begin
        UseXLat := False;
        if not GetToAsciiRecodeTable(KeyMap, XLAT) then
          NullXLAT(XLAT);
      end;
    if DoRedraw then
      begin
        DrawView;
        if Indicator <> nil then
          Indicator^.Draw;
      end;
  end { TDBViewer.SetXlatFile };
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

procedure TDBViewer.HandleEvent;

  {--- start -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}
  procedure GotoRecFld;
    const
      GotoRec: record
        s1: String[10]; {Line Number}
        s2: String[10]; {Field Name}
        end = (s1: ''; s2: '');
    var
      FldCnt: integer;
      Done: boolean;
      PFR: PFieldRec;
    begin
      PFR := DBFile^.GetFieldRec(Delta.X);
      GotoRec.s1 := ItoS(Delta.Y+1);
      GotoRec.s2 := PFR^.Name;
      if (ExecResource(dlgDBFGoto, GotoRec) <> cmOK) or
        ((GotoRec.s1 = ItoS(Delta.Y+1)) and (GotoRec.s2 = PFR^.Name))
      then
        exit;
      FldCnt := 0;
      GotoRec.s2 := UpStrg(DelSpaces(GotoRec.s2));
      Done := False;
      if PFR^.Name <> GotoRec.s2 then
        while (not Done) and (FldCnt < DBFile^.NumFields) do
          begin
            PFR := DBFile^.GetFieldRec(FldCnt);
            if PFR^.Name = GotoRec.s2 then
              Done := True
            else
              Inc(FldCnt);
          end;
      if Done then
        Delta.X := FldCnt;
      if GotoRec.s1 <> ItoS(Delta.Y+1) then
        begin
          Done := True;
          Delta.Y := StoI(GotoRec.s1)-1;
        end;
      if Done then
        DrawView;
    end { GotoRecFld };
  {--- finish -------- Eugeny Zvyagintzev ---- 31-07-2002 ----}

  {--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
  procedure LoadXlatTable; {JO}
    var
      FN: String;
      More: boolean;
      None: boolean;
      OldFN: String; {JOHN_SW}
    begin
      FN := CnvString(XLatFile);
      OldFN := FN; {JOHN_SW}
      if FN <> '' then
        FN := FN+'.xlt';
      More := True;
      None := UseXLat;
      FN := GetFileNameMenu(SourceDir+'XLT\', '*.XLT', FN, True,
        More, None);
      if None then
        begin
          KeyMap := kmAscii;
          SetXlatFile('', True);
          UseXLat := False;
          DisposeStr(XLatFile);
          XLatFile := nil;
          exit;
        end;
      if More then
        FN := GetFileNameDialog(SourceDir+'XLT\*.XLT',
        GetString(dlSelectXLT),
        GetString(dlOpenFileName),
        fdOKButton+fdHelpButton,
        hsOpenXLT);
      DisposeStr(XLatFile);
      XLatFile := nil;
      if FN <> '' then
        SetXlatFile(FN, True)
      else if UseXLat then
        SetXlatFile(OldFN, True); {JOHN_SW}
    end { LoadXlatTable }; {JO}

  procedure NextXLat;
    begin
      case KeyMap of
        kmAscii:
          KeyMap := kmAnsi;
        kmAnsi:
          if Koi8rKeyMap then
            KeyMap := kmKoi8r
          else
            KeyMap := kmAscii;
        kmKoi8r:
          KeyMap := kmAscii;
        else
          KeyMap := kmAscii;
      end {case};
      SetXlatFile('', True); {John_SW 18-03-2003}
    end;
  {--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

  {-DataCompBoy-}
  procedure ViewMemo;
    var
      P: PByteArray;
      PFR: PFieldRec;
      s: String;
      l, ML: longInt;
      i: integer;
      D: PDialog;
      R: TRect;
      PV: PView;
      MemoStream: PStream;
      dr: String;
      Nm: String;
      XT: String;
    begin
      P := GetRecord(Delta.Y);
      if P = nil then
        exit;
      PFR := DBFile^.GetFieldRec(Delta.X);
      if PFR = nil then
        exit;
      if (PFR^.Who <> 'M') then
        exit;
      Move(P^[PFR^.Pos], s[1], PFR^.len);
      s[0] := Char(PFR^.len);
      Val(DelSpaces(s), l, i);
      ML := 0;
      lFSplit(FileName^, dr, Nm, XT);
      if (XT[0] = #4) and (UpStrg(XT) <> '.DBF') then
        XT[4] := 'T'
      else
        XT := '.FPT';
      MemoStream := New(PDosStream, Init(dr+Nm+XT, stOpenRead));
      if (MemoStream = nil) or (MemoStream^.Status <> stOK) then
        begin
          if MemoStream <> nil then
            Dispose(MemoStream, Done);
          MemoStream := New(PBufStream, Init(dr+Nm+'.dbt',
            stOpenRead, 512));
          if (MemoStream = nil) or (MemoStream^.Status <> stOK) then
              begin
              if MemoStream <> nil then
                Dispose(MemoStream, Done);
              MemoStream := nil;
              exit;
            end;
          if l <> 0 then{JOHN_SW}
            begin
              l := l*512;
              MemoStream^.Seek(l);
              repeat
                MemoStream^.Read(Nm[1], 1);
                if Nm[1] <> #$1A then
                  Inc(ML)
                else
                  break
              until MemoStream^.Status <> stOK;
              if (MemoStream = nil) or (MemoStream^.Status <> stOK)
              then
                begin
                  ErrMsg(dlDBViewNoMemo);
                  if MemoStream <> nil then
                    Dispose(MemoStream, Done);
                  MemoStream := nil;
                  exit;
                end;
            end;
        end
      else
        begin
          if l <> 0 then{JOHN_SW}
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
      end
                ;

              l := l*ML;
              MemoStream^.Seek(l+4);
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
      end
                ;
              Inc(l, 8);
            end;
        end;

      MemoStream := New(PMemoStream, Init(MemoStream, l, ML));
      if MemoStream^.Status <> stOK then
        begin
          Dispose(MemoStream, Done);
          MemoStream := nil;
          exit
        end;
      Desktop^.GetExtent(R);
      R.Grow(-2, -2);
      R.Assign(1, 1, 70, 20);
      D := New(PDialog, Init(R, GetString(dlDBViewViewMemo)));
      D^.Options := D^.Options or ofCentered;

      R.Assign(D^.Size.X-2, 2, D^.Size.X-1, D^.Size.Y-4);
      PV := New(PViewScroll, Init(R));
      D^.Insert(PV);
      PV^.GetPalette^:= CScrollBar;

      R.Assign(2, 2, D^.Size.X-2, D^.Size.Y-4);
      PV := New(PNFileViewer, Init(R, MemoStream, '', '', PV, False,
        False));

      PNFileViewer(PV)^.KeyMap := KeyMap; {John_SW 14-03-2003}
      PNFileViewer(PV)^.UseXLat := UseXLat; {John_SW 18-03-2003}
      if not UseXLat then
        PNFileViewer(PV)^.SetXlatFile('', True) {John_SW 14-03-2003}
      else
        PNFileViewer(PV)^.SetXlatFile(CnvString(XLatFile), True);
        {John_SW 18-03-2003}

      D^.Insert(PV);

      R.Assign(30, D^.Size.Y-3, 40, D^.Size.Y-1);
      PV := New(PButton, Init(R, GetString(dlOKButton), cmOK,
        bfDefault));
      PV^.Options := PV^.Options or ofCenterX;
      D^.Insert(PV);

      D^.SelectNext(False);

      D := PDialog(Application^.ValidView(D));
      if D = nil then
        exit;
      Desktop^.ExecView(D);
      Dispose(D, Done);
    end { ViewMemo };
  {-DataCompBoy-}

  procedure ContSearch(StartSearch: boolean);
    var
      FirstFld, LastFld, CurFld, CurRow: longInt;
      PFR: PFieldRec;
      s, SS: String;
      P: PByteArray;
      V: PWhileView;
      R: TRect;
      t: TEventTimer;
    begin
      SS := SearchData.s;
      if SearchData.OPT and 1 = 0 then
        UpStr(SS);
      {--- start -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}
      if (SearchData.Direction = 2) and (StartSearch) then
        CurRow := 0
      else
        CurRow := Delta.Y;

      if SearchData.Scope = 0 then
        begin
          FirstFld := Delta.X;
          LastFld := Delta.X
        end
      else
        begin
          FirstFld := 0;
          LastFld := DBFile^.NumFields-1;
        end;
      CurFld := Delta.X;
      if not StartSearch then
        if SearchData.Scope = 0 then
          if SearchData.Direction = 1 then
            Dec(CurRow)
          else
            Inc(CurRow)
        else if SearchData.Direction = 1 then
          Dec(CurFld)
        else
          Inc(CurFld);
      {--- finish -------- Eugeny Zvyagintzev ---- 19-10-2002 ----}

      R.Assign(1, 1, 30, 9);
      New(V, Init(R));
      V^.Top := GetString(dlSearching)+'"'+Cut(SearchData.s, 40)+'"';
      V^.Write(1, GetString(dlPercentComplete));
      Desktop^.Insert(V);
      Abort := False;
      NewTimer(t, 1);
      while ((CurRow < DBFile^.NumRec) and (SearchData.Direction <> 1))
          or
        ((SearchData.Direction = 1) and (CurRow >= 0))
      do
        begin
          if TimerExpired(t) then
            begin
              DispatchEvents(V, Abort);
              if Abort then
                break;
              V^.Write(1, StrGrd(DBFile^.NumRec, CurRow, 30, False));
              V^.Write(2, ZtoS(Percent(DBFile^.NumRec, CurRow))+'%');
              NewTimer(t, 1);
            end;
          P := GetRecord(CurRow);
          while ((CurFld <= LastFld) and (SearchData.Direction <> 1)) or
            ((SearchData.Direction = 1) and (CurFld >= FirstFld))
          do
            begin
              if Abort then
                break;
              PFR := DBFile^.GetFieldRec(CurFld);
              Move(P^[PFR^.Pos], s[1], PFR^.len);
              s[0] := Char(PFR^.len);

              if PFR^.Who = 'D' then{John_SW  10-10-2002}
                s := DateToHuman(s); {We have to convert date field}

              if SS = '' then{John_SW  14-11-2002}
                s := DelSpaces(s);
                {String to find is empty-searching empty field}

              s := KeyMapConvertStr(s, True); {John_SW 14-03-2003}
              if SearchData.OPT and 1 = 0 then
                UpStr(s);
              if (System.Pos(SS, s) > 0) or (SS = s) then
                begin
                  Delta.Y := CurRow;
                  Delta.X := CurFld;
                  DrawView;
                  V^.Free;
                  exit;
                end;
              if SearchData.Direction = 1 then
                Dec(CurFld)
              else
                Inc(CurFld);
            end;
          if SearchData.Direction = 1 then
            CurFld := LastFld {John_SW 20-10-2002}
          else
            CurFld := FirstFld; {John_SW 20-10-2002}
          if SearchData.Direction = 1 then
            Dec(CurRow)
          else
            Inc(CurRow);
        end;
      V^.Free;
      if not Abort then
        ErrMsg(dlDBViewSearchNot);
    end { ContSearch };

  procedure StartSearch;
    begin
      if ExecResource(dlgDbFind, SearchData) <> cmOK then
        exit;
      ContSearch(True);
    end;

  { -------- Eugeny Zvyagintzev ---------}

  procedure DeleteRecord;
    var
      P: PByteArray;
    begin
      repeat
        case DBFile^.WriteMode of
          0:
            begin
              DBFile^.BaseFile.Done;
              DBFile^.OpenMode(stOpen);
              Inc(DBFile^.WriteMode);
              if DBFile^.BaseFile.Status = stOK then
                Inc(DBFile^.WriteMode)
              else
                DBFile^.OpenMode(stOpenRead);
            end;
          1:
            begin
              ErrMsg(dlDBCantEdit);
              exit
            end;
          2:
            break;
        end {case};
      until False;
      P := GetRecord(Delta.Y);
      if P^[0] = $2A then
        P^[0] := $20
      else
        P^[0] := $2A;
      DBFile^.Seek(Delta.Y);
      DBFile^.BaseFile.Write(P^[0], DBFile^.RecLen);
    end { DeleteRecord };

  { -------- Eugeny Zvyagintzev ---------}

  procedure EditField;
    var
      P: PChar;
      PFR: PFieldRec;
      s: String;
      R: real;
      i: integer;
      MM, DD, YY: word;
      II, N1, N2, N3: word;
      LL: array[0..3] of longInt;
      FldLen: AWord; { Kirill }

    procedure StoreField;
      begin
        s := AddSpace(s, FldLen);
        s := KeyMapConvertStr(s, False); {John_SW 14-03-2003}
        Move(s[1], P[PFR^.Pos], FldLen);
        DBFile^.Seek(Delta.Y);
        DBFile^.BaseFile.Seek(DBFile^.BaseFile.GetPos+PFR^.Pos);
        DBFile^.BaseFile.Write(s[1], FldLen);
      end;

    function GetDig: word;
      var
        D: String;
      begin
        D := '';
        while (II <= Length(s)) and ((s[II] < '0') or (s[II] > '9')) do
            Inc(II);
        while (II <= Length(s)) and ((s[II] >= '0') and (s[II] <=
            '9'))
        do
          begin
            AddStr(D, s[II]);
            Inc(II);
          end;
        GetDig := StoI(D);
      end;

    label redat;
    begin { EditField }
      repeat
        case DBFile^.WriteMode of
          0:
            begin
              DBFile^.BaseFile.Done;
              DBFile^.OpenMode(stOpen);
              Inc(DBFile^.WriteMode);
              if DBFile^.BaseFile.Status = stOK then
                Inc(DBFile^.WriteMode)
              else
                DBFile^.OpenMode(stOpenRead);
            end;
          1:
            begin
              ErrMsg(dlDBCantEdit);
              exit
            end;
          2:
            break;
        end {case};
      until False;
      PFR := DBFile^.GetFieldRec(Delta.X);
      if PFR^.Who = 'M' then
        exit;
      P := GetRecord(Delta.Y);
      FldLen := PFR^.len;
      if FldLen > 252 then
          { Больше чем 252 нельзя, иначе будет вылетать  }
        FldLen := 252; { Kirill }
        { Здесь необходимо вывести предупреждение }
      { о том, что будут отредактированы только }
      { первые 252 символа длинного поля.       }
      Move(P[PFR^.Pos], s[1], FldLen);
      s[0] := Char(FldLen); { Kirill }
      if PFR^.Who <> 'D' then
        begin
          if PFR^.Who <> 'C' then
            DelLeft(s);
          DelRight(s); {Oleg Redut}
        end;
      s := KeyMapConvertStr(s, True); {John_SW 14-03-2003}
      case PFR^.Who of
        'C', 'N':
          if InputBox(GetString(dlDBEditField), GetString(dlDBValue), s,
              FldLen, hsEditDBF) = cmOK
          then
            begin
              if PFR^.Who = 'N' then
                begin
                  DelSpace(s);
                  Val(s, R, i);
                  Str(R:
                  FldLen:
                  PFR^.Dec, s);
                  s := PredSpace(s, FldLen);
                end;
              StoreField;
            end;
        'D':
          begin
            s := DateToHuman(s); {!}
redat:
            if InputBox(GetString(dlDBEditField), GetString(
                dlDBValue), s, 10, hsEditDBF) = cmOK
            then
              begin
                II := 1;
                N1 := GetDig;
                N2 := GetDig;
                N3 := GetDig;
                GetDate(II, MM, DD, YY);
                case CountryInfo.DateFmt of
                  dfMDY:
                    begin
                      MM := N1;
                      DD := N2;
                      YY := N3;
                    end;
                  dfDMY:
                    begin
                      MM := N2;
                      DD := N1;
                      YY := N3;
                    end;
                  dfYMD:
                    begin
                      MM := N2;
                      DD := N3;
                      YY := N1;
                    end;
                end {case};
                if ((MM = 0) and (DD = 0) and (YY = 0)) then
                  s := ' '+' '+' '+' '+' '+' '+' '+' ' {8 spaces!}
                else
                  begin
                    if YY < 100 then
                      YY := (II div 100)*100+YY;
                    if (MM < 1) or (MM > 12) or (DD < 1) or
                      (DD > 31) or ((DD = 31) and (MM in [4, 6, 9, 11]))
                        or
                      ((MM = 2) and ((DD = 30) or ((DD = 29) and (YY
                        mod 4 <> 0))))
                    then
                      goto redat;
                    LL[0] := YY mod 10000;
                    LL[1] := MM mod 100;
                    LL[2] := DD mod 100;
                    FormatStr(s, '%04d%02d%02d', LL);
                  end;
                StoreField;
              end;
          end;
        'L':
          begin
            if UpStrg(s) = 'T' then
              s := 'F'
            else
              s := 'T';
            StoreField;
          end;
      end {case};
    end { EditField };

  procedure CE;
    begin
      ClearEvent(Event)
    end;
  procedure CED;
    begin
      CE;
      DrawView
    end;

  var
    F: lFile; {DataCompBoy}
    P: TPoint;
    RD: word;

  begin { TDBViewer.HandleEvent }
    inherited HandleEvent(Event);
    case Event.What of
      evBroadcast:
        case Event.Command of
          cmFindView:
            if PString(Event.InfoPtr)^ = FileName^ then
              begin
                if Owner <> nil then
                  begin
                    Owner^.Select; { Flash }
                    ClearEvent(Event);
                  end;
              end;
        end {case};
      evCommand:
        case Event.Command of
          cmReboundPanel:
            DrawView;
            {John_SW 10-10-2002 after country settings changed}
          cmClose, cmKillUsed:
            if KillAfterUse then
              begin
                if DBFile <> nil then
                  begin
                    if Buf <> nil then
                      FreeMem(Buf, NumRec*DBFile^.RecLen);
                    Dispose(DBFile, Done);
                    Buf := nil;
                    DBFile := nil;
                  end;
                EraseTempFile(FileName^);
                KillAfterUse := False;
                if Event.Command = cmClose then
                  GlobalMessage(evCommand, cmPanelReread, nil);
              end;
          cmGetName:
            PString(Event.InfoPtr)^:= GetString(dlDBViewName)+' - '
            +Cut(FileName^, 49-Length(GetString(dlDBViewName)));
          cmShowFields:
            begin
              GetInfo(dlDBViewInfo);
              CE
            end;
          cmShowMemo:
            begin
              ViewMemo;
              CE
            end;
          cmDBFGoto:
            begin
              GotoRecFld;
              CE;
            end;
          cmSearchFor:
            begin
              StartSearch;
              CE;
            end;
          cmContinueSearch:
            begin
              ContSearch(False);
              CE;
            end;
          cmBackSearch:
            begin
              if SearchData.Direction = 1 then
                begin
                  SearchData.Direction := 0;
                  ContSearch(False);
                  SearchData.Direction := 1;
                end
              else
                begin
                  RD := SearchData.Direction;
                  SearchData.Direction := 1;
                  ContSearch(False);
                  SearchData.Direction := RD;
                end;
              CE;
            end;
          cmEditDBField:
            begin
              EditField;
              CED
            end;
          cmDeleteDBRec:
            begin
              DeleteRecord;
              CED;
            end;
          cmSwitchKeyMapping:
            begin
              NextXLat;
              CED;
            end; {John_SW 14-03-2003}
          cmLoadXlatTable:
            begin
              LoadXlatTable;
              CED;
            end; {John_SW 18-03-2003}
        end {case};
      evKeyDown:
        case Event.KeyCode of
          kbEnter:
            begin
              if Delta.X < DBFile^.NumFields-1 then
                Inc(Delta.X)
              else
                begin
                  Inc(Delta.Y);
                  Delta.X := 0;
                end;
              CED;
            end;
          kbLeft:
            begin
              if Delta.X > 0 then
                Dec(Delta.X);
              CED;
            end;
          kbRight:
            begin
              if Delta.X < DBFile^.NumFields-1 then
                Inc(Delta.X);
              CED;
            end;
          kbHome:
            begin
              Delta.X := 0;
              CED;
            end;
          kbEnd:
            begin
              Delta.X := DBFile^.NumFields-1;
              CED;
            end;
          kbUp:
            begin
              Dec(Delta.Y);
              CED;
            end;
          kbDown:
            begin
              Inc(Delta.Y);
              CED;
            end;
          kbPgUp:
            begin
              Dec(Delta.Y, Size.Y-2);
              CED;
            end;
          kbPgDn:
            begin
              Inc(Delta.Y, Size.Y-2);
              CED;
            end;
          kbCtrlPgUp:
            begin
              Delta.Y := 0;
              CED;
            end;
          kbCtrlPgDn:
            begin
              Delta.Y := DBFile^.NumRec-1;
              CED;
            end;
          {              kbCtrlD: begin DeleteRecord; CED; end;}
          kbESC:
            begin
              Message(Owner, evCommand, cmClose, nil);
              CE
            end;
        end {case};
      evMouseDown:
        begin
          RD := RepeatDelay;
          RepeatDelay := 0;
          repeat
            MakeLocal(Event.Where, P);
            if MouseInView(Event.Where) then
              if P.X < Size.X div 4 then
                Message(@Self, evKeyDown, kbLeft, nil)
              else if P.X >= (Size.X*3) div 4 then
                Message(@Self, evKeyDown, kbRight, nil)
              else if P.Y < Size.Y div 2 then
                Message(@Self, evKeyDown, kbUp, nil)
              else
                Message(@Self, evKeyDown, kbDown, nil)
          until not MouseEvent(Event, evMouseMove+evMouseAuto);
          RepeatDelay := RD;
          CE
        end;
    end {case};
  end { TDBViewer.HandleEvent };

procedure TDBViewer.Draw;
  var
    B: array[0..1024] of record
      C: Char;
      A: byte;
      end;
    FN: array[0..132] of PFieldRec;
    C1, C2, C3: word;
    C: byte;
    i, j, k, NFN: integer;
    FldLen: word; { Kirill }
    FldPart: byte;
    Idx: longInt;
    PFR: PFieldRec;
    P: PByteArray;
    s: String;
  begin
    C1 := GetColor(1);
    C2 := GetColor(2);
    C3 := GetColor(3);
    if Delta.Y < 0 then
      Delta.Y := 0;
    if Delta.Y >= DBFile^.NumRec then
      Delta.Y := DBFile^.NumRec-1;
    if Pos.Y > Delta.Y then
      Pos.Y := Delta.Y;
    if Pos.Y+Size.Y-2 < Delta.Y then
      Pos.Y := Delta.Y-Size.Y+2;
    if Pos.Y < 0 then
      Pos.Y := 0;
    if Delta.X > DBFile^.NumFields-1 then
      Delta.X := DBFile^.NumFields-1;
    if Pos.X > Delta.X then
      Pos.X := Delta.X;
    {if Indicator <> nil then Indicator^.Draw;}
    { -------- Eugeny Zvyagintzev ---------}
    if (VerticalScrollBar <> nil) and (VerticalScrollBar^.Value <>
        Delta.Y)
    then
      begin
        VerticalScrollBar^.SetValue(Delta.Y);
        if Indicator <> nil then
          Indicator^.Draw;
      end;
    if (HorizontalScrollBar <> nil) and (HorizontalScrollBar^.Value <>
        Delta.X)
    then
      begin
        HorizontalScrollBar^.SetValue(Delta.X);
        if Indicator <> nil then
          Indicator^.Draw;
      end;
    { -------- Eugeny Zvyagintzev ---------}
    repeat
      MoveChar(B, ' ', C1, Size.X);
      j := Pos.X;
      NFN := 0;
      i := 1;
      FldPart := 0;
      while (i < Size.X) and (j < DBFile^.NumFields) do
        begin
          PFR := DBFile^.GetFieldRec(j);
          FN[NFN] := PFR;
          if PFR <> nil then
            begin
              k := PFR^.ln;
              if k > Size.X-i then
                k := Size.X-i; { Kirill }
              MoveStr(B[i], CenterStr(PFR^.Name, {PFR^.Ln}k), C1);
              Inc(i, PFR^.ln+1);
            end;
          if i < Size.X then
            Inc(NFN)
          else
            FldPart := 1;
          Inc(j);
        end;
      if (Delta.X > Pos.X+NFN-1) and (Pos.X <> Delta.X) then
        Inc(Pos.X)
      else
        break;
    until False;

    WriteLine(0, 0, Size.X, 1, B);
    for k := 1 to Size.Y-1 do
      begin
        MoveChar(B, ' ', C2, Size.X);
        if DBFile^.NumRec > 0 then
          begin
            j := Delta.X;
            i := 1;
            Idx := Pos.Y+longInt(k)-1;
            P := GetRecord(Idx);
            {if (NFN = 0) and (P <> nil) then NFN := 1;}
            if P <> nil then
              B[0].C := Char(P^[0]);
            if P <> nil then

              for j := 0 to NFN+FldPart-1 do
                begin
                  PFR := FN[j];
                  if (Delta.Y = Idx) and (Pos.X+j = Delta.X) then
                    C := C3
                  else
                    C := C2;
                  if PFR <> nil then
                    if PFR^.Who = 'M' then
                      begin
                        Move(P^[PFR^.Pos], s[1], PFR^.len);
                        s[0] := Char(PFR^.len);
                        if StoI(s) > 0 then
                          s := '  Memo  '
                        else
                          s := '  memo  ';
                        MoveStr(B[i], s, C);
                        Inc(i, PFR^.ln+1);
                      end
                    else if PFR^.Who = 'D' then
                      begin
                        Move(P^[PFR^.Pos], s[1], PFR^.len);
                        s[0] := Char(PFR^.len);
                        MoveStr(B[i], DateToHuman(s), C);
                        Inc(i, PFR^.ln+1);
                      end
                    else
                      begin{ Kirill }
                        FldLen := PFR^.len;
                        if FldLen > Size.X-i then
                          FldLen := Size.X-i;
                        Move(P^[PFR^.Pos], s[1], FldLen);
                        s[0] := Char(FldLen);
                        s := KeyMapConvertStr(s, True);
                          {John_SW 14-03-2003}
                        MoveStr(B[i], s, C);
                        Inc(i, PFR^.ln+1);
                      end;
                end;
          end;
        WriteLine(0, k, Size.X, 1, B);
      end;
  end { TDBViewer.Draw };

procedure TDBIndicator.Draw;
  var
    B: TDrawBuffer;
    s, s1: String; {John_SW 18-03-2003}
    C: byte;
    PFR: PFieldRec;
  begin
    {10171114/10171114═══[C 10,10]═WIN═}
    if (DBViewer = nil) or (DBViewer^.DBFile = nil) then
      exit;
    if DBViewer^.Delta.Y < 0 then
      DBViewer^.Delta.Y := 0;
    if DBViewer^.Delta.X > DBViewer^.DBFile^.NumFields-1 then
        DBViewer^.Delta.X := DBViewer^.DBFile^.NumFields-1;
    if Origin.Y <> Owner^.Size.Y-1 then
      MoveTo(Origin.X, Owner^.Size.Y-1);
    PFR := DBViewer^.DBFile^.GetFieldRec(DBViewer^.Delta.X);
    if (State and sfDragging <> 0) or (State and sfActive = 0) then
      C := 196
    else
      C := 205;
    s := AddSpace(ItoS(DBViewer^.Delta.Y+1)+'/'+ItoS(DBViewer^.
      DBFile^.NumRec), 10);
    Replace(' ', Char(C), s);
    s1 := AddSpace(ItoS(PFR^.len)+','+ItoS(PFR^.Dec)+']', 7);
    Replace(' ', Char(C), s1);
    s := s+Char(C)+'['+PFR^.Who+' '+s1;
    {--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
    if (DBViewer^.UseXLat) and (DBViewer^.XLatFile <> nil) then
      s := s+Char(C)+Cut(DBViewer^.XLatFile^, 8)
    else
      case DBViewer^.KeyMap of
        kmAscii:
          s := s+Char(C)+'DOS';
        kmAnsi:
          s := s+Char(C)+'WIN';
        kmKoi8r:
          s := s+Char(C)+'KOI';
        else
          s := s+Char(C)+'XXX';
      end {case};
    {--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
    s := Copy(s, 1, Owner^.Size.X-4);
    if byte(s[0]) <> Size.X then
      begin
        GrowTo(byte(s[0]), 1);
        exit;
      end;
    {--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
    if State and sfDragging <> 0 then
      C := PWindow(Owner)^.Frame^.GetColor($05)
    else if State and sfActive = 0 then
      C := PWindow(Owner)^.Frame^.GetColor($01)
    else
      C := PWindow(Owner)^.Frame^.GetColor($03);
    {--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 -----}
    MoveStr(B, s, C);
    WriteLine(0, 0, Size.X, 1, B);
  end { TDBIndicator.Draw };

Constructor TDBWindow.Init;
  var
    R: TRect;
    i: integer;
    s: String;
  begin
    Desktop^.GetExtent(R);
    i := PosChar('|', FName);
    if i > 0 then
      begin
        s := Copy(FName, 1, i-1);
        FName := Copy(FName, i+1, 255);
      end;
    FName := lFExpand(FName);
    if i <= 0 then
      s := FName;
    TWindow.Init(R, s, 0);
    RealName := s;
    Options := Options or ofTileable;
    Flags := Flags or wfMaxi;
    GetExtent(R);
    R.Grow(-1, -1);
    P := New(PDBViewer, Init(R, FName, FileIsDBF));
    if P = nil then
      Fail;
    Insert(P);
    if not P^.isValid then
      begin
        Done;
        Fail;
      end;
    { -------- Eugeny Zvyagintzev ---------}
    R.A.X := R.B.X;
    R.B.X := R.B.X+1;
    VSB := New(PDBScrollBar, Init(R));
    VSB^.Max := P^.DBFile^.NumRec-1;
    VSB^.GrowMode := gfGrowLoX+gfGrowHiX+gfGrowHiY;
    VSB^.DBViewer := P;
    VSB^.SetStep(Size.Y-4, 1);
    VSB^.ScrollBarType := sbVertical;
    Insert(VSB);
    P^.VerticalScrollBar := VSB;
    GetBounds(R);
    R.A.X := R.A.X+49;
    R.B.X := R.B.X-2;
    R.A.Y := R.B.Y-1;
    HSB := New(PDBScrollBar, Init(R));
    HSB^.Max := P^.DBFile^.NumFields-1;
    HSB^.GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
    HSB^.DBViewer := P;
    HSB^.ScrollBarType := sbHorizontal;
    Insert(HSB);
    P^.HorizontalScrollBar := HSB;
    { -------- Eugeny Zvyagintzev ---------}
    R.A.X := 2;
    R.B.X := 34;
    R.A.Y := Size.Y-1;
    R.B.Y := Size.Y;
    P1 := New(PDBIndicator, Init(R));
    P1^.DBViewer := P;
    P1^.GrowMode := gfGrowLoY+gfGrowHiX+gfGrowHiY;
    Insert(P1);
    P^.Indicator := P1;
  end { TDBWindow.Init };

Constructor TDBWindow.Load;
  begin
    inherited Load(s);
    GetSubViewPtr(s, P);
    GetSubViewPtr(s, P1);
    P1^.DBViewer := P;
    P^.Indicator := P1;
    GetSubViewPtr(s, VSB);
    VSB^.DBViewer := P;
    VSB^.ScrollBarType := sbVertical;
    P^.VerticalScrollBar := VSB;
    GetSubViewPtr(s, HSB);
    HSB^.DBViewer := P;
    HSB^.ScrollBarType := sbHorizontal;
    P^.HorizontalScrollBar := HSB;

    Redraw;
  end;

procedure TDBWindow.Store;
  begin
    inherited Store(s);
    PutSubViewPtr(s, P);
    PutSubViewPtr(s, P1);
    PutSubViewPtr(s, VSB);
    PutSubViewPtr(s, HSB);
  end;

{--- start -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}
procedure TDBWindow.SetState(AState: word; Enable: boolean);
  begin
    inherited SetState(AState, Enable);
    if P1 <> nil then
      begin
        P1^.SetState(AState, Enable);
        P1^.Draw;
      end;
  end;
{--- finish -------- Eugeny Zvyagintzev ---- 14-03-2003 ----}

destructor TDBWindow.Done;
  begin
    StoreViewInfo(@Self);
    inherited Done;
  end;



end.
