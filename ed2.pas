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
unit ed2;

interface

uses
  Commands, advance1, U_KeyMap, Collect, Views, Drivers, Objects,
  Lfn {, SBlocks};

{ TDoCollection }
const
  maxUndo = 100;

  udDelChar = 1;
  udInsChar = 2;
  udDelLine = 3;
  udInsLine = 4;
  udDelBlock = 5;
  udInsBlock = 6;
  udBackDel = 7;
  udSubDel = 8;
  udSubDelLine = 9;
  udIndentBlock = 10;
  udUnindentBlock = 11;
  udInsVertBlock = 12;
  udReplace = 13;
  udReplaceChar = 14;
  udReplaceBlock = 15;
  udClearBlock = 16;
  udStrModified = 17;
  udDupeLine = 18;
  udFormatBlock = 19;
  udReplaceAll = 20; {-$VOL}

type
  TDoKind = (dkRedo, dkUndo); {-$VOL}

  PUndoRec = ^TUndoRec;
  TUndoRec = record
    What: word;
    Where: TPoint;
    KeyMap: TKeyMap; {-$VIV}
    case word of
      udDelChar: (Str: PLongString);
      udInsChar: (Count, Width: integer; Block: TRect);
      udDelLine: (Lines: PCollection; Vertical, InsM: boolean);
  end;

PDoCollection = ^TDoCollection;
TDoCollection = object(TCollection)
  DoKind: TDoKind; {-$VOL}
  procedure FreeItem(P: Pointer); virtual;
  Constructor Init(ReOrUn_do: TDoKind); {-$VOL}
  end;

{ TInfoLine }

PInfoLine = ^TInfoLine;
TInfoLine = object(TView)
  Constructor Init(var R: TRect);
  procedure Draw; virtual;
  procedure HandleEvent(var Event: TEvent); virtual;
  end;

{ TBookmarkLine }

PBookmarkLine = ^TBookmarkLine;
TBookmarkLine = object(TView)
  procedure Draw; virtual;
  end;

{ TAttrBufStream }

PAttrBufStream = ^TAttrBufStream;
TAttrBufStream = object(TBufStream)
  OldAttr: word;
  F: lFile;
  Constructor Init(FileName: String; Mode, Size: word);
  destructor Done; virtual;
  end;

function CheckForOver(Name: String): PStream;
procedure WriteBlock(Hint: String; s: PStream; C: PLineCollection
    {PCollector}; ForcedCRLF: TCRLF; AOptimalFill: boolean);
  {-SBlocks}

implementation
uses
  advance, advance2, Messages, Dos, DNApp, Microed, Startup, DnIni,
    EdWin;

{ TDoCollection }
procedure TDoCollection.FreeItem(P: Pointer);
  var
    t: PUndoRec;
  begin
    t := P;
    if P = nil then
      exit;
    case DoKind of
      dkUndo:
        case t^.What of
          udDelChar, udInsLine, udSubDelLine, udReplace,
            udReplaceChar, udInsBlock,
          udFormatBlock, udStrModified, udSubDel, udBackDel,
            udReplaceAll:
            DisposeLongStr(t^.Str);
          udDelLine, udDelBlock, udReplaceBlock,
          udClearBlock:
            begin
              Dispose(t^.Lines, Done);
              t^.Lines := nil
            end;
          udInsChar:
            begin
            end;
        end {case};
      dkRedo:
        case t^.What of
          udInsChar, udSubDel, udReplaceChar, udSubDelLine,
            udStrModified, udReplace,
          udReplaceAll:
            DisposeLongStr(t^.Str);
          udInsLine, udInsBlock, udFormatBlock, udDelBlock,
            udInsVertBlock,
          udReplaceBlock, udClearBlock:
            begin
              Dispose(t^.Lines, Done);
              t^.Lines := nil
            end;
          udDelChar, udDelLine, udBackDel:
            begin
            end;
        end {case};
    end {case};
    Dispose(t);
  end { TDoCollection.FreeItem };

Constructor TDoCollection.Init(ReOrUn_do: TDoKind); {-$VOL}
  begin
    inherited Init($100, $80);
    DoKind := ReOrUn_do;
  end; {-$VOL}

{ TInfoLine }

Constructor TInfoLine.Init;
  begin
    inherited Init(R);
    EventMask := evMouseDown;
    GrowMode := gfGrowHiX+gfGrowHiY+gfGrowLoY;
  end;

procedure TInfoLine.HandleEvent;
  var
    t: TPoint;
    { lS: byte; }
    P: PFileEditor;
    EV: TEvent;
    BookMark: byte;
  begin
    inherited HandleEvent(Event);
    if Event.What = evMouseDown then
      begin
        Owner^.MakeLocal(Event.Where, t);
        if t.X >= Owner^.Size.X-2 then
          begin
            PWindow(Owner)^.Frame^.HandleEvent(Event);
            exit;
          end;
        MakeLocal(Event.Where, t);
        Event.What := evCommand;
        {==0000000:000=[000 00]=(-)==CrLf=DOS==<1........>}
        {00000000001111111111222222222233333333334444444444}
        {01234567890123456789012345678901234567890123456789}
        P := PFileEditor(PEditWindow(Owner)^.Intern);
        if (t.X > 1) and (t.X < 12) then
          Event.Command := cmGotoLineNumber
        else if (t.X > 12) and (t.X < 21) then
          Event.Command := cmSpecChar
        else if (t.X > 21) and (t.X < 25) then
          Event.Command := cmSwitchBlock
        else if (t.X > 26) and (t.X < 31) then
          case P^.EdOpt.ForcedCRLF of
            cfCRLF:
              Event.Command := cmEditLfMode;
            cfLF:
              Event.Command := cmEditCrMode;
            cfCR:
              Event.Command := cmEditCrLfMode;
            else
              Event.Command := cmEditCrLfMode;
          end
        else if (t.X > 31) and (t.X < 35) then
          Event.Command := cmSwitchKeyMapping
        else if (t.X = 37) then
          PFileEditor(Owner^.Current)^.ScrollTo(0, 0) {AK155}
        else if FastBookmark and ((t.X > 37) and (t.X < 47)) then
          begin
            BookMark := t.X-38;
            if (Event.Buttons and mbRightButton <> 0) or
              (not P^.MarkPos[Event.InfoByte].EqualsXY(-1, -1))
            then
              with Event do
                begin
                  What := evCommand;
                  if (Event.Buttons and mbRightButton <> 0)
                  then
                    Command := cmPlaceMarker1+BookMark
                  else
                    Command := cmGoToMarker1+BookMark;
                end
              else
              Event.What := evNothing;
          end
        else if (t.X = 47) then
          with PFileEditor(Owner^.Current) do
            ScrollTo(0, FileLines^.Count) {AK155}
          else
          Event.What := evNothing;
        if Event.What <> evNothing then
          PutEvent(Event);
        ClearEvent(Event);
      end;
  end { TInfoLine.HandleEvent };

procedure TInfoLine.Draw;
  var
    P: PFileEditor;
    X, Y: longInt;
    C: String[1];
    s: String;
    CharNum: byte; {-$VIV}
    Ch: Char;
    Ch2: Char;
    Color: byte;
    qwe: byte;
    B: TDrawBuffer;
  begin
    P := PFileEditor(PEditWindow(Owner)^.Intern);
    if Owner^.GetState(sfDragging) or not Owner^.GetState(sfActive)
    then
      begin
        if Owner^.GetState(sfDragging)
        then
          Color := PWindow(Owner)^.Frame^.GetColor(5)
        else
          Color := PWindow(Owner)^.Frame^.GetColor(2);
        Ch2 := #196;
      end
    else
      begin
        Color := PWindow(Owner)^.Frame^.GetColor(3);
        Ch2 := #205;
      end;
    if not Owner^.GetState(sfActive) then
      SetLength(s, 0)
    else
      begin
        with P^ do
          begin
            X := Delta.X+1;
            Y := Delta.Y+1;
            if X <= Length(WorkString) then
              C := WorkString[X]
            else
              C := #0;
          end;
        C := P^.KeyMapConvertStr(C, False); {-$VIV start}
        CharNum := byte(C[1]); {-$VIV e.nd}
        if P^.Modified then
          s := #15+Ch2
        else
          s := Ch2+Ch2;
        s := s+SStr(Y, 5, Ch2)+':'+SSt2(X, 4, Ch2)+Ch2+'['+SStr(
          CharNum, 3, '0')+'·'+Hex2(CharNum)+']'+Ch2; {-$VIV}
        if P^.DrawMode = 1 then
          s := s+'{┼'
        else if P^.DrawMode = 2 then
          s := s+'{╬'
        else if P^.VertBlock then
          s := s+'('#18
        else
          s := s+'('#29;
        if P^.DrawMode = 0 then
          if P^.OptimalFill then
            s := s+'F)'
          else
            s := s+')═'
        else if P^.OptimalFill then
          s := s+'F}'
        else
          s := s+'}═';

        if P^.EdOpt.ForcedCRLF = cfNone then
          begin
            P^.EdOpt.ForcedCRLF := cfNone;
            for qwe := 0 to EditorDefaults.NewLine do
              P^.EdOpt.ForcedCRLF := succ(P^.EdOpt.ForcedCRLF);
          end;
        if P^.EdOpt.ForcedCRLF = cfCR then
          s := s+Ch2+Ch2+'Cr'+Ch2
        else if P^.EdOpt.ForcedCRLF = cfLF then
          s := s+Ch2+Ch2+'Lf'+Ch2
        else if P^.EdOpt.ForcedCRLF = cfCRLF then
          s := s+Ch2+'CrLf'
        else
          s := s+Ch2+'::::';
        if CapitalCodePageName then
          case P^.KeyMap of{-$VIV--}
            kmAscii:
              s := s+Ch2+'Dos'+Ch2;
            kmAnsi:
              s := s+Ch2+'Win'+Ch2;
            kmKoi8r:
              s := s+Ch2+'Koi'+Ch2;
          end
        else
          case P^.KeyMap of{-$VIV--}
            kmAscii:
              s := s+Ch2+'DOS'+Ch2;
            kmAnsi:
              s := s+Ch2+'WIN'+Ch2;
            kmKoi8r:
              s := s+Ch2+'KOI'+Ch2;
          end {case};
        if FastBookmark then
          begin
            s := s+Ch2+'<'; {-$VIV 20.05.99--}
            for X := 1 to 9 do
              if not P^.MarkPos[X].EqualsXY(-1, -1)
              then
                s := s+Char(X+48)
              else
                s := s+#250;
            s := s+'>'; {-$VIV::}
          end;
      end;
    MoveChar(B, Ch2, Color, Size.X);
    MoveStr(B, s, Color);
    WriteLine(0, 0, Size.X, 1, B);
  end { TInfoLine.Draw };

{TBookmarkLine}
procedure TBookmarkLine.Draw;
  var
    P: PFileEditor;
    Col: byte;
    i: integer;
    Mrk: Char;
    Ch: Char;
    B: array[0..20] of AWord;

  function IsMarker(pLine: longInt): Char;
    var
      i: byte;
    begin
      IsMarker := #0;
      for i := 1 to 9 do
        if P^.MarkPos[i].Y = pLine then
          begin
            IsMarker := Char(i+48);
            break;
          end;
    end;

  function SwitchHalfs(B: byte): byte;
    begin
      SwitchHalfs := ((B and $0F) shl 4) or ((B and $F0) shr 4);
    end;

  begin { TBookmarkLine.Draw }
    P := PFileEditor(PEditWindow(Owner)^.Intern);
    if Owner^.GetState(sfDragging) or not Owner^.GetState(sfActive)
    then
      begin
        if Owner^.GetState(sfDragging)
        then
          Col := PWindow(Owner)^.Frame^.GetColor(5)
        else
          Col := PWindow(Owner)^.Frame^.GetColor(2);
        Ch := #179;
      end
    else
      begin
        Col := PWindow(Owner)^.Frame^.GetColor(3);
        Ch := #186;
      end;
    if not ShowBookmarks then
      begin
        MoveChar(B, Ch, Col, 1); {SYR}
        WriteLine(0, 0, Size.X, Size.Y, B);
        exit;
      end;
    for i := 0 to Size.Y do
      begin
        Mrk := IsMarker(P^.Pos.Y+i);
        if Mrk = #0 then
          MoveChar(B, Ch, Col, 1) {SYR}
        else
          MoveChar(B, Mrk, SwitchHalfs(Col), 1);
        WriteLine(0, i, Size.X, 1, B);
      end;
  end { TBookmarkLine.Draw };

Constructor TAttrBufStream.Init(FileName: String; Mode, Size: word);
  begin
    inherited Init(FileName, Mode, Size);
    OldAttr := $FFFF;
  end;

destructor TAttrBufStream.Done;
  begin
    inherited Done;
    if OldAttr <> $FFFF then
      lSetFAttr(F, OldAttr);
  end;

{-DataCompBoy-}
function CheckForOver(Name: String): PStream;
  var
    s: PAttrBufStream;
    F: lFile;
    W: word;
    l: array[0..0] of longInt;
    Attr: word;

  procedure CreateBackup;
    var
      dr: String;
      Nm: String;
      XT: String;
    begin
      lFSplit(Name, dr, Nm, XT);
      ClrIO;
      EraseFile(dr+Nm+'.BAK');
      lChangeFileName(Name, dr+Nm+'.BAK');
      ClrIO;
    end;

  procedure OverQuery;
    var
      P: PString;
      dr: String[30];
    begin
      P := @Dr;
      dr := Cut(Name, 30);
      W := Msg(dlED_OverQuery, @P, mfYesButton+mfCancelButton+
        mfAppendButton+mfWarning);
    end;

  begin { CheckForOver }
    CheckForOver := nil;
    Abort := False;
    s := nil;
    lAssignFile(F, Name);
    ClrIO;
    lGetFAttr(F, Attr);
    if Abort then
      exit;
    if (DOSError = 0) and (Attr and ReadOnly <> 0) then
      begin
        OverQuery;
        case W of
          cmYes, cmOK:
            ;
          else
            exit
        end {case};
        Pointer(l[0]) := @Name;
        if Msg(dlED_ModifyRO, @L, mfConfirmation+mfOKCancel) <> cmOK
        then
          exit;
        lSetFAttr(F, Archive);
        if Abort or (DOSError <> 0) then
          begin
            CantWrite(Name);
            exit;
          end;
      end
    else
      begin
        W := $FFFF;
        Attr := Archive
      end;
    New(s, Init(Name, stOpen, 4096));
    if s = nil then
      exit;
    if Abort or (s^.Status = stOK) then
      begin
        if W = $FFFF then
          OverQuery;
        case W of
          cmYes, cmOK:
            begin
              if Attr and ReadOnly <> 0 then
                begin
                  s^.OldAttr := Attr;
                  lAssignFile(s^.F, lFileNameOf(F));
                end;
              case W of
                cmYes:
                  s^.Truncate;
                cmOK:
                  s^.Seek(s^.GetSize);
              end {case};
              CheckForOver := s;
            end;
          else
            begin
              Dispose(s, Done);
              s := nil;
            end;
        end {case};
        exit;
      end;
    Dispose(s, Done);
    s := nil;
    if Abort then
      exit;
    if EditorDefaults.EdOpt and ebfCBF <> 0 then
      CreateBackup;
    New(s, Init(Name, stCreate, 4096));
    if Abort or (s = nil) or (s^.Status <> stOK) then
      begin
        CantWrite(Name);
        Dispose(s, Done);
        s := nil;
        exit
      end;
    CheckForOver := s;
  end { CheckForOver };
{-DataCompBoy-}

procedure WriteBlock(Hint: String; s: PStream; C: PLineCollection
    {PCollector}; ForcedCRLF: TCRLF; AOptimalFill: boolean);
    {-SBlocks}
  var
    i: longInt;
    M: longInt;
    SST: LongString;
    P: PLongString;

    {Cat: эта процедура теперь умеет работать с длинными строками
      и находится в модуле Advance1}
    (*
  procedure CompressString; {та, кот. при сохранении файла}
  var PP: Pointer;
      TSt: Integer;
  begin
   PP := @SST;
   TSt := StoI(EditorDefaults.TabSize);
   if TSt = 0 then TSt := 8;
 {$IFNDEF BIT_32}
   asm
      les bx, PP
      mov cl, es:[bx]
      inc bx
      xor ch, ch
      jcxz @@Ex
      xor di, di
      xor si, si
      mov byte ptr es:[bx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor dx, dx
    @@2:
      mov al, es:[bx][si]
      mov es:[bx][di], al
      inc si
      cmp si, cx
      ja  @@Ex
      inc di
      inc byte ptr es:[bx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor Dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub di, dx
      sub byte ptr es:[bx-1], dl
      mov al, 9
      mov es:[bx][di-1], al
     @@5:
      jmp @@1
    @@Ex:
   end;
 {$ELSE}
   asm
      push ebx
      push edx
      push edi
      push esi
      mov ebx, PP
      xor ecx, ecx
      mov cl, [ebx]
      inc ebx
      jcxz @@Ex
      xor edi, edi
      xor esi, esi
      mov byte ptr [ebx-1], ch
    @@1:
      mov ah, byte ptr TSt
      xor edx, edx
    @@2:
      mov al, [ebx+esi]
      mov [ebx+edi], al
      inc esi
      cmp esi, ecx
      ja  @@Ex
      inc edi
      inc byte ptr [ebx-1]
      cmp al, ' '
      jne @@3
      inc dl
      jmp @@4
     @@3:
      xor dl, dl
     @@4:
      dec ah
      jnz @@2
      or  dl, dl
      jz @@5
      dec dl
      jz @@5
      sub edi, edx
      sub byte ptr [ebx-1], dl
      mov al, 9
      mov [ebx+edi-1], al
     @@5:
      jmp @@1
    @@Ex:
      pop esi
      pop edi
      pop edx
      pop ebx
   end;
 {$ENDIF}
  end;
*)

  var
    PP: PView;
    CrLf: String[2];
    qwe: byte;
  begin

    if ForcedCRLF = cfNone then
      begin
        ForcedCRLF := cfNone;
        for qwe := 0 to EditorDefaults.NewLine do
          ForcedCRLF := succ(ForcedCRLF);
      end;

    if ForcedCRLF = cfCR then
      CrLf := #13
    else if ForcedCRLF = cfLF then
      CrLf := #10
    else if ForcedCRLF = cfCRLF then
      CrLf := #13#10;

    i := 1;
    if (s = nil) or (C = nil) then
      exit;
    PP := WriteMsg(^M^M^C+GetString(dlWritingFile));
    while not Abort and (s^.Status = stOK) and (i < C^.Count) do
      begin
        UpdateWriteView(PP);
        P := C^.At(i-1);
        if P <> nil then{JO}{!!!}
          SST := P^+CrLf
        else
          SST := CrLf; {JO}
        if AOptimalFill then
          CompressString(SST);
        s^.Write(SST[1], Length(SST));
        Inc(i);
      end;
    {HintString := '';}Application^.Idle;
    P := C^.At(i-1);
    if P <> nil then
      SST := P^
    else
      SST := '';
    if AOptimalFill then
      CompressString(SST);
      {Cat: про последнюю строку тоже не забываем}
    s^.Write(SST[1], Length(SST));
    if PP <> nil then
      PP^.Free;
  end { WriteBlock };

end.
