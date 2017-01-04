{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.05/DOS
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
//////////////////////////////////////////////////////////////////////////
//
//      LFNизация выполнена Антоном Федоpовым aka DataCompBoy'ем.
//                  В честь Матаpыкиной Ульяны...
//////////////////////////////////////////////////////////////////////////}

unit dnini;
interface
uses Strings;


type TBlockMarker=record end;

const

  INIstoredsize:longint = 0;
  INIstoredtime:longint = 0;

  { DO NOT CHANGE THIS LINE } iniparamblock_START:TBlockMarker=();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

  {Interface}
  CutDriveInfo: Boolean = True;
  DriveInfoType: Longint = 2; {DataCompBoy} {DataCompBoy}
  WinManagerSelectNext: Boolean = True;
  DriveSelectVCenter: Boolean = False; {-$X-Man}
  SystemMenuChar: Longint = 4;
  HorizScrollBarChars: string = #17#16#177#254#178; {DataCompBoy}
  VertScrollBarChars:  string = #30#31#177#254#178; {DataCompBoy}
  FadeDelay: Longint = 0; {Knave}

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
  CanUseLFN: Boolean = True;
  AutoSave: Boolean = True;
  DebugMode: Boolean = False;
  ShowKeyCode: Longint = 0;
  CopyLimit: LongInt = 8192; {DataCompBoy}{piwamoto}
  HandleChDirCommand: Boolean = True; {DataCompBoy}

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
uses dos, LFN, {DataCompBoy}
     u_ini, advance, advance1, advance2, advance3, objects;

type
  TIniItemKind = (ikBoolean, ikString, ikLongint, ikChar{SYR});

  PIniItem = ^TIniItem;
  TIniItem = record
    Section: PChar;
    Name: PChar;
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
  Dispose(iP);
end;

var IniCol: PIniCollection;
    IniF: PIniFile;

procedure LoadDnIniSettings;

  procedure Loader(iP: PIniItem); far;
  begin
    with iP^ do
      case Kind of
        ikBoolean: Boolean(VarPtr^) := IniF^.ReadBool(StrPas(Section), StrPas(Name), Boolean(VarPtr^));
        ikString:  String (VarPtr^) := IniF^.ReadStr (StrPas(Section), StrPas(Name), String (VarPtr^));
        ikLongint: Longint(VarPtr^) := IniF^.ReadInt (StrPas(Section), StrPas(Name), Longint(VarPtr^));
        ikChar:    Char   (VarPtr^) := IniF^.ReadChar(StrPas(Section), StrPas(Name), Char   (VarPtr^)); {SYR}
      end;
  end;

begin
  if IniCol = nil then Exit;
  IniF := New(PIniFile, Init(SourceDir+'DN.INI')); {DataCompBoy}
  IniCol^.ForEach(@Loader);
  Dispose(IniF, Done(False));
end;

procedure SaveDnIniSettings;

  procedure Saver(iP: PIniItem); far;
  begin
    with iP^ do
      case Kind of
        ikBoolean: IniF^.WriteBool(StrPas(Section), StrPas(Name), Boolean  (VarPtr^));
        ikString:  IniF^.WriteStr (StrPas(Section), StrPas(Name), CnvString(VarPtr));
        ikLongint: IniF^.WriteInt (StrPas(Section), StrPas(Name), Longint  (VarPtr^));
        ikChar:    IniF^.WriteChar(StrPas(Section), StrPas(Name), Char     (VarPtr^)); {SYR}
      end;
  end;

begin
  if IniCol = nil then Exit;
  IniF := New(PIniFile, Init(SourceDir+'DN.INI'));
  IniCol^.ForEach(@Saver);
  Dispose(IniF, Done(True));
end;

procedure RegisterVar(pSection, pName: PChar; pKind: TIniItemKind; pVarPtr: Pointer);
var iP: PIniItem;
begin
  New(iP);
  with iP^ do
  begin
     Section := pSection;
     Name := pName;
     Kind := pKind;
     VarPtr := pVarPtr;
  end;
  IniCol^.Insert(iP);
end;

const
{{{{{{{{{{{{}
 CSInterface            :PChar = 'Interface';
{{{{{{{{{{{{}
 KCutDriveInfo          :PChar = 'CutDriveInfo';
 KDriveInfoType         :PChar = 'DriveInfoType';
 KWinManagerSelectNext  :PChar = 'WinManagerSelectNext';
 KDriveSelectVCenter    :PChar = 'DriveSelectVCenter';
 KSystemMenuChar        :PChar = 'SystemMenuChar';
 KHorizScrollBarChars   :PChar = 'HorizScrollBarChars';
 KVertScrollBarChars    :PChar = 'VertScrollBarChars';
 KFadeDelay             :PChar = 'FadeDelay';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSClock                :PChar = 'Clock';
{{{{{{{{{{{{}
 KShowSeconds           :PChar = 'ShowSeconds';
 KShowCentury           :PChar = 'ShowCentury';
 KShowDayOfWeek         :PChar = 'ShowDayOfWeek';
 KDaysOfWeek            :PChar = 'DaysOfWeek';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSSmartPad             :PChar = 'SmartPad';
{{{{{{{{{{{{}
 KInsertDate            :PChar = 'InsertDate';
 KLineChar              :PChar = 'LineChar';
{{{{{{{{{{{{}


{{{{{{{{{{{{}
 CSGame                 :PChar = 'Game';
{{{{{{{{{{{{}
 KEnableGame            :PChar = 'EnableGame';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSClipBoard            :PChar = 'ClipBoard';
{{{{{{{{{{{{}
 KSaveClipBoardOnExit   :PChar = 'SaveClipBoardOnExit';
 KMaxClipBoardSize      :PChar = 'MaxClipBoardSize';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSKernel               :PChar = 'Kernel';
{{{{{{{{{{{{}
 KUseLFN                :PChar = 'UseLFN';
 KAutoSave              :PChar = 'AutoSave';
 KDebugMode             :PChar = 'DebugMode';
 KShowKeyCode           :PChar = 'ShowKeyCode';
 KCopyLimit             :PChar = 'CopyLimit';
 KHandleChDirCommand    :PChar = 'HandleChDirCommand';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSEditor               :PChar = 'Editor';
{{{{{{{{{{{{}
 KUnlimitUnindent       :PChar = 'UnlimitUnindent';
 KKoi8rKeyMap           :PChar = 'Koi8rKeyMap';
 KDrawRShift            :PChar = 'DrawRShift';
 KAutoScopeDetect       :PChar = 'AutoScopeDetect';
 KShowBookmarks         :PChar = 'ShowBookmarks';
 KFastBookmark          :PChar = 'FastBookmark';
 KDefCodePage           :PChar = 'DefCodePage';
 KCapitalCodePageName   :PChar = 'CapitalCodePageName';
 KFastSearchDeep        :PChar = 'FastSearchDeep';
 KWinManagerPosToEdit   :PChar = 'WinManagerPosToEdit';
 KAutoBracketPairs      :PChar = 'AutoBracketPairs';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSFilePanels           :PChar = 'FilePanels';
{{{{{{{{{{{{}
 KShowFileMask          :PChar = 'ShowFileMask';
 KShowLongName          :PChar = 'ShowLongName';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSNetInfo              :PChar = 'NetInfo';
{{{{{{{{{{{{}
 KNoLevelsInfo          :PChar = 'NoLevelsInfo';
{{{{{{{{{{{{}

{{{{{{{{{{{{}
 CSLanguage             :PChar = 'Language';
{{{{{{{{{{{{}
 KActiveLanguage        :PChar = 'ActiveLanguage';
 KHelpLanguageOverride  :PChar = 'HelpLanguageOverride';
 KShowLanguageMenu      :PChar = 'ShowLanguageMenu';
{{{{{{{{{{{{}

procedure FillVarList;
begin
  {Interface}
  RegisterVar(CSInterface, KCutDriveInfo,         ikBoolean, @CutDriveInfo);
  RegisterVar(CSInterface, KDriveInfoType,        ikLongint, @DriveInfoType);
  RegisterVar(CSInterface, KWinManagerSelectNext, ikBoolean, @WinManagerSelectNext);
  RegisterVar(CSInterface, KDriveSelectVCenter,   ikBoolean, @DriveSelectVCenter); {-$X-Man}
  RegisterVar(CSInterface, KSystemMenuChar,       ikLongint, @SystemMenuChar);
  RegisterVar(CSInterface, KHorizScrollBarChars,  ikString,  @HorizScrollBarChars);
  RegisterVar(CSInterface, KVertScrollBarChars,   ikString,  @VertScrollBarChars);
  RegisterVar(CSInterface, KFadeDelay,            ikLongInt, @FadeDelay);
  {Clock}
  RegisterVar(CSClock, KShowSeconds,   ikBoolean, @ShowSeconds);
  RegisterVar(CSClock, KShowCentury,   ikBoolean, @ShowCentury);
  RegisterVar(CSClock, KShowDayOfWeek, ikboolean, @ShowDayOfWeek);
  RegisterVar(CSClock, KDaysOfWeek,    ikString,  @DaysOfWeek);
  {SmartPad}
  RegisterVar(CSSmartPad, KInsertDate, ikBoolean, @SPInsertDate);
  RegisterVar(CSSmartPad, KLineChar,   ikLongint, @SPLineChar); {SYR}
  {Game}
  RegisterVar(CSGame, KEnableGame, ikBoolean, @EnableGame);
  {ClipBoard}
  RegisterVar(CSClipBoard, KSaveClipBoardOnExit, ikBoolean, @CBAutoSave);
  RegisterVar(CSClipBoard, KMaxClipBoardSize,    ikLongint, @CBSize);
  {Kernel}
  RegisterVar(CSKernel, KUseLFN,              ikBoolean, @CanUseLFN);
  RegisterVar(CSKernel, KAutoSave,            ikBoolean, @AutoSave);
  RegisterVar(CSKernel, KDebugMode,           ikBoolean, @DebugMode);
  RegisterVar(CSKernel, KShowKeyCode,         ikLongInt, @ShowKeyCode);
  RegisterVar(CSKernel, KCopyLimit,           ikLongInt, @CopyLimit);
  RegisterVar(CSKernel, KHandleChDirCommand,  ikBoolean, @HandleChDirCommand); {DataCompBoy}
  {Editor}
  RegisterVar(CSEditor, KUnlimitUnindent,     ikBoolean, @UnlimitUnindent);
  RegisterVar(CSEditor, KKoi8rKeyMap,         ikBoolean, @Koi8rKeyMap);
  RegisterVar(CSEditor, KDrawRShift,          ikBoolean, @DrawRShift);
  RegisterVar(CSEditor, KAutoScopeDetect,     ikBoolean, @AutoScopeDetect);
  RegisterVar(CSEditor, KShowBookmarks,       ikBoolean, @ShowBookmarks);
  RegisterVar(CSEditor, KFastBookmark,        ikBoolean, @FastBookmark);
  RegisterVar(CSEditor, KDefCodePage,         ikString,  @DefCodePage);
  RegisterVar(CSEditor, KCapitalCodePageName, ikBoolean, @CapitalCodePageName);
  RegisterVar(CSEditor, KFastSearchDeep,      ikLongint, @FastSearchDeep);
  RegisterVar(CSEditor, KWinManagerPosToEdit, ikBoolean, @WinManagerPosToEdit);
  RegisterVar(CSEditor, KAutoBracketPairs,    ikString,  @AutoBracketPairs);
  {FilePanels}
  RegisterVar(CSFilePanels, KShowFileMask, ikBoolean, @ShowFileMask);
  RegisterVar(CSFilePanels, KShowLongName, ikBoolean, @ShowLongName);
  {NetInfo}
  RegisterVar(CSNetInfo, KNoLevelsInfo, ikBoolean, @NoLevelsInfo);
  {Language}
  RegisterVar(CSLanguage, KActiveLanguage,       ikString,  @ActiveLanguage);
  RegisterVar(CSLanguage, KHelpLanguageOverride, ikString,  @HelpLanguageOverride);
  RegisterVar(CSLanguage, KShowLanguageMenu,     ikBoolean, @ShowLanguageMenu);
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

        {-DataCompBoy-}
function ProbeINI(var INItime,INIsize:longint):boolean;
var SR:lSearchRec;
begin
    INItime:=0;
    INIsize:=0;
    lFindFirst(SourceDir+'DN.INI',AnyFile,SR);
    lFindClose(SR);
    if (DosError=0) and ((SR.SR.Attr and Directory)=0) then begin
        ProbeINI:=True;
        INItime:=SR.SR.Time;
        INIsize:=SR.SR.Size
    end else ProbeINI:=False
end;
        {-DataCompBoy-}

begin
  IniCol := nil;
end.