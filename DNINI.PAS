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
//  Ini Configuration by VIV (C) 1999
//
//////////////////////////////////////////////////////////////////////////}

unit dnini;
interface

type TBlockMarker=record end;

const

  INIstoredsize:longint = 0;
  INIstoredtime:longint = 0;

  { DO NOT CHANGE THIS LINE } iniparamblock_START:TBlockMarker=();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

  {Interface}
  CutDriveInfo: Boolean = True;
  HideLocalCurDir: Boolean = False;
  HideNetworkCurDir: Boolean = True;
  WinManagerSelectNext: Boolean = True;
  DriveSelectVCenter: Boolean = False; {-$X-Man}
  SystemMenuChar: Longint = 4;

  {Clock}
  ShowSeconds: Boolean = True;
  ShowCentury: Boolean = True;
  ShowDayOfWeek: Boolean = True;
  DaysOfWeek: String = '(use language default)';

  {SmartPad}
  SPInsertDate: Boolean = True;
  SPLineChar: Longint = 196; {X-Man} {SYR}

  {Game}
  EnableGame: Boolean = True;

  {ClipBoard}
  CBSize: Longint = 4096; {-$VOL}
  CBAutoSave: Boolean = True; {-$VOL}

  {Kernel}
  AutoSave: Boolean = True;
  DebugMode: Boolean = False;
  ShowKeyCode: Boolean = False;
  CopyLimit8M: Boolean = True; {piwamoto}

  {Editor}
  UnlimitUnindent: Boolean = False;
  Koi8rKeyMap: Boolean = True;
  DrawRShift: Boolean = True;
  AutoScopeDetect: Boolean = True;
  ShowBookmarks: Boolean = True;
  FastBookmark: Boolean = True;
  DefCodePage: String = 'Dos';
  CapitalCodePageName: Boolean = False;
  FastSearchDeep: Longint = 0;
  WinManagerPosToEdit: Boolean = True;
  AutoBracketPairs: String = '()[]{}<>';

  {FilePanels}
  ShowFileMask: Boolean = True;
  ShowLongName: Boolean = True;

  {NetInfo}
  NoLevelsInfo: Boolean = False;

  {Language}
  ActiveLanguage: String = '';
  HelpLanguageOverride: String = '';
  ShowLanguageMenu: Boolean = False;

  { ----------------------------- NO INI VARIABLES BEYOND HERE }
  { DO NOT CHANGE THIS LINE } iniparamblock_END:TBlockMarker=();

procedure InitIniEngine;
procedure DoneIniEngine;

procedure LoadDnIniSettings;
procedure SaveDnIniSettings;

function ProbeINI(var INItime,INIsize:longint):boolean;

implementation
uses u_ini, advance, objects, dos;

type
  TIniItemKind = (ikBoolean, ikString, ikLongint, ikChar{SYR});

  PIniItem = ^TIniItem;
  TIniItem = record
    Section: PString;
    Name: PString;
    Kind: TIniItemKind;
    VarPtr: Pointer;
  end;

  PIniCollection = ^TIniCollection;
  TIniCollection = object(TCollection)
    procedure FreeItem(P: Pointer); virtual;
  end;

procedure TIniCollection.FreeItem(P: Pointer);
var iP: PIniItem;
begin
  if P = nil then Exit;
  iP := PIniItem(P);
  DisposeStr(iP^.Section);
  DisposeStr(iP^.Name);
  Dispose(iP);
end;

var IniCol: PIniCollection;
    IniF: PIniFile;

procedure LoadDnIniSettings;

  procedure Loader(iP: PIniItem); far;
  begin
    with iP^ do
      case Kind of
        ikBoolean: Boolean(VarPtr^) := IniF^.ReadBool(Section^, Name^, Boolean(VarPtr^));
        ikString: String(VarPtr^) := IniF^.ReadStr(Section^, Name^, String(VarPtr^));
        ikLongint: Longint(VarPtr^) := IniF^.ReadInt(Section^, Name^, Longint(VarPtr^));
        ikChar: Char(VarPtr^) := IniF^.ReadChar(Section^, Name^, Char(VarPtr^)); {SYR}
      end;
  end;

begin
  if IniCol = nil then Exit;
  IniF := New(PIniFile, Init(SourceDir+'DN.INI'));
  IniCol^.ForEach(@Loader);
  Dispose(IniF, Done(False));
end;

procedure SaveDnIniSettings;

  procedure Saver(iP: PIniItem); far;
  begin
    with iP^ do
      case Kind of
        ikBoolean: IniF^.WriteBool(Section^, Name^, Boolean(VarPtr^));
        ikString: IniF^.WriteStr(Section^, Name^, String(VarPtr^));
        ikLongint: IniF^.WriteInt(Section^, Name^, Longint(VarPtr^));
        ikChar: IniF^.WriteChar(Section^, Name^, Char(VarPtr^)); {SYR}
      end;
  end;

begin
  if IniCol = nil then Exit;
  IniF := New(PIniFile, Init(SourceDir+'DN.INI'));
  IniCol^.ForEach(@Saver);
  Dispose(IniF, Done(True));
end;

procedure RegisterVar(pSection, pName: String; pKind: TIniItemKind; pVarPtr: Pointer);
var iP: PIniItem;
begin
  New(iP);
  with iP^ do
  begin
     Section := NewStr(pSection);
     Name := NewStr(pName);
     Kind := pKind;
     VarPtr := pVarPtr;
  end;
  IniCol^.Insert(iP);
end;

const
  CSInterface = 'Interface';
  CSClock = 'Clock';
  CSSmartPad = 'SmartPad';
  CSGame = 'Game';
  CSClipBoard = 'ClipBoard';
  CSKernel = 'Kernel';
  CSEditor = 'Editor';
  CSFilePanels = 'FilePanels';
  CSNetInfo = 'NetInfo';
  CSLanguage = 'Language';

procedure FillVarList;
begin
  {Interface}
  RegisterVar(CSInterface, 'CutDriveInfo', ikBoolean, @CutDriveInfo);
  RegisterVar(CSInterface, 'HideLocalCurDir', ikBoolean, @HideLocalCurDir);
  RegisterVar(CSInterface, 'HideNetworkCurDir', ikBoolean, @HideNetworkCurDir);
  RegisterVar(CSInterface, 'WinManagerSelectNext', ikBoolean, @WinManagerSelectNext);
  RegisterVar(CSInterface, 'DriveSelectVCenter', ikBoolean, @DriveSelectVCenter); {-$X-Man}
  RegisterVar(CSInterface, 'SystemMenuChar', ikLongint, @SystemMenuChar);
  {Clock}
  RegisterVar(CSClock, 'ShowSeconds', ikBoolean, @ShowSeconds);
  RegisterVar(CSClock, 'ShowCentury', ikBoolean, @ShowCentury);
  RegisterVar(CSClock, 'ShowDayOfWeek', ikboolean, @ShowDayOfWeek);
  RegisterVar(CSClock, 'DaysOfWeek', ikString, @DaysOfWeek);
  {SmartPad}
  RegisterVar(CSSmartPad, 'InsertDate', ikBoolean, @SPInsertDate);
  RegisterVar(CSSmartPad, 'LineChar', ikLongint, @SPLineChar); {SYR}
  {Game}
  RegisterVar(CSGame, 'EnableGame', ikBoolean, @EnableGame);
  {ClipBoard}
  RegisterVar(CSClipBoard, 'SaveClipBoardOnExit', ikBoolean, @CBAutoSave);
  RegisterVar(CSClipBoard, 'MaxClipBoardSize', ikLongint, @CBSize);
  {Kernel}
  RegisterVar(CSKernel, 'AutoSave', ikBoolean, @AutoSave);
  RegisterVar(CSKernel, 'DebugMode', ikBoolean, @DebugMode);
  RegisterVar(CSKernel, 'ShowKeyCode', ikBoolean, @ShowKeyCode);
  RegisterVar(CSKernel, 'CopyLimit8M', ikBoolean, @CopyLimit8M);
  {Editor}
  RegisterVar(CSEditor, 'UnlimitUnindent', ikBoolean, @UnlimitUnindent);
  RegisterVar(CSEditor, 'Koi8rKeyMap', ikBoolean, @Koi8rKeyMap);
  RegisterVar(CSEditor, 'DrawRShift', ikBoolean, @DrawRShift);
  RegisterVar(CSEditor, 'AutoScopeDetect', ikBoolean, @AutoScopeDetect);
  RegisterVar(CSEditor, 'ShowBookmarks', ikBoolean, @ShowBookmarks);
  RegisterVar(CSEditor, 'FastBookmark', ikBoolean, @FastBookmark);
  RegisterVar(CSEditor, 'DefCodePage', ikString, @DefCodePage);
  RegisterVar(CSEditor, 'CapitalCodePageName', ikBoolean, @CapitalCodePageName);
  RegisterVar(CSEditor, 'FastSearchDeep', ikLongint, @FastSearchDeep);
  RegisterVar(CSEditor, 'WinManagerPosToEdit', ikBoolean, @WinManagerPosToEdit);
  RegisterVar(CSEditor, 'AutoBracketPairs', ikString, @AutoBracketPairs);
  {FilePanels}
  RegisterVar(CSFilePanels, 'ShowFileMask', ikBoolean, @ShowFileMask);
  RegisterVar(CSFilePanels, 'ShowLongName', ikBoolean, @ShowLongName);
  {NetInfo}
  RegisterVar(CSNetInfo, 'NoLevelsInfo', ikBoolean, @NoLevelsInfo);
  {Language}
  RegisterVar(CSLanguage, 'ActiveLanguage', ikString, @ActiveLanguage);
  RegisterVar(CSLanguage, 'HelpLanguageOverride', ikString, @HelpLanguageOverride);
  RegisterVar(CSLanguage, 'ShowLanguageMenu', ikBoolean, @ShowLanguageMenu);
end;

procedure InitIniEngine;
begin
  if IniCol <> nil then Exit;
  IniCol := New(PIniCollection, Init(20, 20));
  FillVarList;
end;

procedure DoneIniEngine;
begin
  if IniCol = nil then Exit;
  Dispose(IniCol, Done);
  IniCol := nil;
end;

function ProbeINI(var INItime,INIsize:longint):boolean;
var F:file; SR:SearchRec;
begin
    INItime:=0;
    INIsize:=0;
    FindFirst(SourceDir+'DN.INI',AnyFile,SR);
    if (DosError=0) and ((SR.Attr and Directory)=0) then begin
        ProbeINI:=True;
        INItime:=SR.Time;
        INIsize:=SR.Size
    end else ProbeINI:=False
end;

begin
  IniCol := nil;
end.
