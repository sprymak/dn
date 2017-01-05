{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.6.RC1
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
{$I STDEFINE.INC}
unit DnIni;

interface

uses
  Commands
  ;

type
  TBlockMarker = record
    end;

const

  INIstoredsize: LongInt = 0;
  INIstoredtime: LongInt = 0;
  INIstoredcrc: LongInt = 0; {DataCompBoy}

  { DO NOT CHANGE THIS LINE }iniparamblock_START: TBlockMarker = ();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

  {Cat: Внимание!!!
      При добавлении новых переменных необходимо соответствующим
      образом изменять структуру TIniVars в модуле Vars
      Добавлять новые переменные обязательно в конец - для
      совместимости со старыми версиями плагинов}

  { NOTE! }
  { To declare a short string that must have fixed length add at least }
  { one extra character to its length for better error checking.       }

  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {; Variable name           ; Type       ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Interface}
  {}CutDriveInfo: Boolean = True;
  {}WinManagerSelectNext: Boolean = True;
  {}DriveSelectVCenter: Boolean = False; {-$X-Man}
  {}SystemMenuChar: Byte = 4;
  {}HorizScrollBarChars: String[6] = #17#16#177#254#178; {DataCompBoy}
  {}VertScrollBarChars: String[6] = #30#31#177#254#178; {DataCompBoy}
  {}ReflectCopyDirection: Boolean = False;
  {}ReuseViewers: Byte = 0; { 0 - always open new}
  {}ReuseEditors: Byte = 0; { 1 - prompt for open}
  {} { 2 - do not open new}
  {}HistoryErrorBeep: Boolean = True; {DataCompBoy}
  {}PreserveMenuPositions: Boolean = False; {DataCompBoy}
  {}LFNinBottom: Boolean = False; {JO}
  {}PanelDescrArvid: String = ''; {JO}
  {}PanelDescrArc: String = ''; {JO}
  {}PanelDescrTemp: String = ''; {JO}
  {}PanelDescrFind: String = ''; {JO}
  {}PanelDescrDrive: String = ''; {JO}
  {}UseEnterInViewer: Byte = 0; {JO}
  {}SkipXLatMenu: Boolean = False; {JO}
  {}EscForOutputWindow: Boolean = False; {JO}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Clock}
  {}ShowSeconds: Boolean = True;
  {}BlinkSeparator: Boolean = True; {JO}
  {}ShowCentury: Boolean = True;
  {}ShowDayOfWeek: Boolean = True;
  {}DaysOfWeek: String[23] = '(use language default)';
  {}RightAlignClock: Boolean = False; {FY 13-03-2000}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {SmartPad}
  {}SPInsertDate: Boolean = True;
  {}SPLineChar: Byte = 196; {X-Man}
  {SYR}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Game}
  {}EnableGame: Boolean = True;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Clipboard}
  {}cbSize: LongInt = 4096; {-$VOL}
  {}cbAutoSave: Boolean = False; {-$VOL}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Kernel}
  //CanUseLFN                : Boolean    = True;
  {}AutoSave: Boolean = True;
  {}ShowKeyCode: Byte = 0;
  {}CopyLimit: LongInt = 8192; {DataCompBoy}
  {piwamoto}
  {}StoreVideoMode: Byte = 0; {PZ}
  {DataCompBoy}
  {}SmartWindowsBoxClose: LongInt = 0; {DataCompBoy, AK155}
  {}CtrlBreakKill: Boolean = False; {AK155}
  //DoVESATest               : Boolean    = False;                      {DataCompBoy}
  {}ForceDefaultArchiver: String[3] = ''; {JO}
  {$IFDEF WIN32}
  {}AltGrAsAlt               : Boolean    = True; {JO} {JO: эта переменная дублируется в VpKbdW32}
  {$ENDIF}
  {$IFNDEF OS2}
  {}DescrByShortNames: Boolean = True; {JO}
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Editor}
  {}UnlimitUnindent: Boolean = False;
  {}Koi8rKeyMap: Boolean = True;
  {}DrawRShift: Boolean = True;
  {}AutoScopeDetect: Boolean = True;
  {}ShowBookmarks: Boolean = True;
  {}FastBookmark: Boolean = True;
  {}DefCodePageView: String[9] = 'Dos';
  {}DefCodePage: String[9] = 'Dos';
  {}CapitalCodePageName: Boolean = False;
  {}FastSearchDeep: LongInt = 0;
  {}WinManagerPosToEdit: Boolean = True;
  {}AutoBracketPairs: String = '()[]{}<>';
  {}F6_DuplicatesLine: Boolean = False; {JO}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {FilePanels}
  {}ShowFileMask: Boolean = True;
  {}ShowLongName: Boolean = True;
  {}QuickRenameInDialog: Boolean = False;
  {}UpperCaseSorting: Boolean = True; {JO}
  {}AutoRefreshDriveLine: Boolean = False; {Cat}
  {}AutoRefreshPanels: Boolean = False; {Cat}
  {}QuickSearchType: Byte = 0; {Cat}
  {}BriefSelectedInfo: Boolean = True; {AK155}
  {}BriefTotalInfo: Boolean = False; {AK155}
  {}PackingInfoInBottom: Boolean = True; {AK155}
  {}PathInfoInBottom: Boolean = True; {AK155}
  {$IFDEF OS2}
  {}ExecWin32GUI: String[30] = 'start /n /f pe';
  {}ExecWin32CUI: String[30] = 'pec';
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {NetInfo}
  {}NoLevelsInfo: Boolean = False;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {TaskList}
  {}KillAllowed: Boolean = False; {JO}
  {}ShowExePaths: Boolean = False; {JO}
  {}FilteredList: Boolean = True; {JO}
  {$IFDEF OS2}
  {}UserTaskFilter: String = 'pmshell.exe#filebar.exe#clipvdm.exe';
  {JO}
  {$ENDIF}
  {$IFDEF Win32}
  {}UserTaskFilter: String =
  'msgsrv32.exe#mmtask.tsk#mstask.exe#systray.exe#internat.exe#winoa386.mod'
  ; {JO}
  {$ENDIF}
  {$IFDEF DPMI32}
  {}UserTaskFilter: String = ''; {JO}
  {$ENDIF}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {Language}
  {}ActiveLanguage: String[9] = '';
  {}HelpLanguageOverride: String[9] = '';
  {}ShowLanguageMenu: Boolean = False;
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type     ; Default value      ;          Comments ;}
  {`.________________________;____________;____________________;___________________'}
  {} {RegExp}
  {}RegExpStr0: String = ''; {PZ}
  {}RegExpStr1: String = ''; {PZ}
  {}RegExpStr2: String = ''; {PZ}
  {}RegExpStr3: String = ''; {PZ}
  {}RegExpStr4: String = ''; {PZ}
  {}RegExpStr5: String = ''; {PZ}
  {}RegExpStr6: String = ''; {PZ}
  {}RegExpStr7: String = ''; {PZ}
  {}RegExpStr8: String = ''; {PZ}
  {}RegExpStr9: String = ''; {PZ}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type  ; Default value  ;          Comments ;}
  {`.________________________;_________;________________;___________________'}
  {} {QuickDirs}
  {}QDirs1: String = ''; {Quick Dirs - 1}
  {}QDirs2: String = ''; {Quick Dirs - 2}
  {}QDirs3: String = ''; {Quick Dirs - 3}
  {}QDirs4: String = ''; {Quick Dirs - 4}
  {}QDirs5: String = ''; {Quick Dirs - 5}
  {}QDirs6: String = ''; {Quick Dirs - 6}
  {}QDirs7: String = ''; {Quick Dirs - 7}
  {}QDirs8: String = ''; {Quick Dirs - 8}
  {}QDirs9: String = ''; {Quick Dirs - 9}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
  {;      Variable name      ;   Type  ; Default value  ;          Comments ;}
  {`.________________________;_________;________________;___________________'}

  {} {SetupStorage}
  {}SystemDataOpt: LongInt = 75; {SystemData.Options}
  {}InterfaceDataOpt: LongInt = 8201; {InterfaceData.Options}
  {}DriveInfoType: LongInt = 0; {InterfaceData.DrvInfType}
  {}FMSetupOpt: LongInt = 11358; {FMSetup.Options}
  {}EditorDefaultsOpt: LongInt = 8194; {EditorDefaults.EdOpt}
  {}EditorDefaultsOpt2: LongInt = 3; {EditorDefaults.EdOpt2}
  {}ViewerOpt: LongInt = 16; {EditorDefaults.ViOpt}
  {}StartupDataLoad: LongInt = 2; {StartupData.Load}
  {}StartupDataUnload: LongInt = 0; {StartupData.Unload}
  {}StartupDataSlice2: LongInt = 1; {StartupData.Slice2}
  {}ConfirmsOpt: LongInt =
   cfSingleErase+cfMultiErase+cfEraseReadonly+cfEraseSubDir
  +cfMouseConfirm+cfExitConfirm+cfChaneLngWarning; {Confirms}
  {}NonVIOScreenMode: LongInt = 65535; {JO}
  {последний видеорежим в полноэкранной сессии}
  {}VIOScreenMode: LongInt = 65535; {JO}
  {последний видеорежим в оконной сессии}
  {,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~~`,}
  {;                   This is end of parameters definitions                       ;}
  {`.______________________________________________________________________________'}

  { ----------------------------- NO INI VARIABLES BEYOND HERE }
  { DO NOT CHANGE THIS LINE }iniparamblock_END: TBlockMarker = ();

function DnIniFileName: String;

procedure LoadDnIniSettings;
procedure SaveDnIniSettings(PVar: Pointer);
procedure DoneIniEngine;

function ProbeINI(var INItime, INIsize, INIcrc: LongInt): Boolean;

procedure ShowIniErrors;
procedure ClearIniErrors;

implementation
uses
  Dos, Lfn, profile, advance, advance1, Collect, Messages, DNApp,
  {$IFDEF WIN32}VpSysLow, {$ENDIF}
  Strings
  ;

type

  TSInt8 = ShortInt;
  TSInt16 = SmallInt;
  TSInt32 = LongInt;
  TUInt8 = Byte;
  TUInt16 = word;

  TIniItemKind = (
    ikBool,
    ikEnum,
    ikStr,
    ikSInt,
    ikUInt,
    ikChar
    );

  TDoProc = procedure (Group, Parameter: PChar; Kind: TIniItemKind;
     Size: word; PVar: Pointer);

const
  {Group names}
  CSInterface: PChar = 'Interface';
  CSClock: PChar = 'Clock';
  CSSmartPad: PChar = 'SmartPad';
  CSGame: PChar = 'Game';
  CSClipboard: PChar = 'Clipboard';
  CSKernel: PChar = 'Kernel';
  CSEditor: PChar = 'Editor';
  CSFilePanels: PChar = 'FilePanels';
  CSNetInfo: PChar = 'NetInfo';
  CSTaskList: PChar = 'TaskList';
  CSLanguage: PChar = 'Language';
  CSRegExp: PChar = 'RegExp';
  CSQuickDirs: PChar = 'QuickDirs';
  CSSetupStorage: PChar = 'SetupStorage';

type
  PIniError = ^TIniError;
  TIniError = record
    Size: Byte;
    Group: PChar;
    Parameter: PChar;
    end;

  PIniErrors = ^TIniErrors;
  TIniErrors = object(TCollection)
    procedure FreeItem(Item: Pointer); virtual;
    end;

const
  IniErrors: PIniErrors = nil;

procedure TIniErrors.FreeItem(Item: Pointer);
  var
    P: PIniError absolute Item;
  begin
  Dispose(P);
  end;

procedure AddIniError(Size: Byte; Group: PChar; Parameter: PChar);
  var
    p: PIniError;
  begin
  if IniErrors = nil then
    IniErrors := New(PIniErrors, Init(1, 1));
  New(p);
  p^.Size := Size;
  p^.Group := Group;
  p^.Parameter := Parameter;
  IniErrors^.Insert(p);
  end;

procedure ShowIniErrors;

  procedure Show(P: Pointer);
    type
      TParams = record
        Size: LongInt;
        Group: PChar;
        Parameter: PChar;
        end;
    var
      I: PIniError absolute P;
      sg: String[10];
      sp: String[21];
      id: TStrIdx;
      flg: word;
      q: Pointer;
      par: TParams;
    begin
    if P = nil then
      exit;
    sg := StrPas(I^.Group);
    sp := StrPas(I^.Parameter);
    flg := mfOKButton;
    par.Group := @sg;
    par.Parameter := @sp;
    if I^.Size > 0 then
      begin
      par.Size := I^.Size;
      Inc(flg, mfWarning);
      id := dlIniDataTooLong;
      q := @par.Size;
      end
    else
      begin
      Inc(flg, mfError);
      id := dlIniDataReadError;
      q := @par.Group;
      end;
    Msg(id, q, flg);
    end { Show };

  begin { ShowIniErrors }
  if IniErrors <> nil then
    begin
    IniErrors^.ForEach(@Show);
    ClearIniErrors;
    end;
  end { ShowIniErrors };

procedure ClearIniErrors;
  begin
  if IniErrors <> nil then
    begin
    IniErrors^.FreeAll;
    Dispose(IniErrors, Done);
    IniErrors := nil;
    end;
  end;

function DnIniFileName: String;
  begin
  DnIniFileName := SourceDir+'DN.INI';
  end;

procedure Proceed(RegisterVar: TDoProc);
  begin
  {Interface}
  RegisterVar(CSInterface, 'CutDriveInfo', ikBool, SizeOf(CutDriveInfo),
     @CutDriveInfo);
  RegisterVar(CSInterface, 'WinManagerSelectNext', ikBool,
       SizeOf(WinManagerSelectNext), @WinManagerSelectNext);
  RegisterVar(CSInterface, 'DriveSelectVCenter', ikBool,
       SizeOf(DriveSelectVCenter), @DriveSelectVCenter); {-$X-Man}
  RegisterVar(CSInterface, 'SystemMenuChar', ikUInt,
       SizeOf(SystemMenuChar), @SystemMenuChar);
  RegisterVar(CSInterface, 'HorizScrollBarChars', ikStr,
       SizeOf(HorizScrollBarChars), @HorizScrollBarChars);
  RegisterVar(CSInterface, 'VertScrollBarChars', ikStr,
       SizeOf(VertScrollBarChars), @VertScrollBarChars);
  RegisterVar(CSInterface, 'ReflectCopyDirection', ikBool,
       SizeOf(ReflectCopyDirection), @ReflectCopyDirection);
  RegisterVar(CSInterface, 'ReuseViewers', ikUInt, SizeOf(ReuseViewers),
     @ReuseViewers);
  RegisterVar(CSInterface, 'ReuseEditors', ikUInt, SizeOf(ReuseEditors),
     @ReuseEditors);
  RegisterVar(CSInterface, 'HistoryErrorBeep', ikBool,
       SizeOf(HistoryErrorBeep), @HistoryErrorBeep);
  RegisterVar(CSInterface, 'PreserveMenuPositions', ikBool,
       SizeOf(PreserveMenuPositions), @PreserveMenuPositions);
  RegisterVar(CSInterface, 'LFNinBottom', ikBool, SizeOf(LFNinBottom),
     @LFNinBottom); {JO}
  //  RegisterVar(CSInterface, 'DescriptionInBottom',  ikBool, SizeOf(DescriptionInBottom),  @DescriptionInBottom);{JO}
  RegisterVar(CSInterface, 'PanelDescrArvid', ikStr,
       SizeOf(PanelDescrArvid), @PanelDescrArvid); {JO}
  RegisterVar(CSInterface, 'PanelDescrArc', ikStr,
     SizeOf(PanelDescrArc), @PanelDescrArc); {JO}
  RegisterVar(CSInterface, 'PanelDescrTemp', ikStr,
     SizeOf(PanelDescrTemp), @PanelDescrTemp); {JO}
  RegisterVar(CSInterface, 'PanelDescrFind', ikStr,
     SizeOf(PanelDescrFind), @PanelDescrFind); {JO}
  RegisterVar(CSInterface, 'PanelDescrDrive', ikStr,
       SizeOf(PanelDescrDrive), @PanelDescrDrive); {JO}
  RegisterVar(CSInterface, 'UseEnterInViewer', ikUInt,
       SizeOf(UseEnterInViewer), @UseEnterInViewer); {JO}
  RegisterVar(CSInterface, 'SkipXLatMenu', ikBool, SizeOf(SkipXLatMenu),
     @SkipXLatMenu); {JO}
  RegisterVar(CSInterface, 'EscForOutputWindow', ikBool,
       SizeOf(EscForOutputWindow), @EscForOutputWindow); {JO}
  {Clock}
  RegisterVar(CSClock, 'ShowSeconds', ikBool, SizeOf(ShowSeconds),
     @ShowSeconds);
  RegisterVar(CSClock, 'BlinkSeparator', ikBool, SizeOf(BlinkSeparator),
     @BlinkSeparator);
  RegisterVar(CSClock, 'ShowCentury', ikBool, SizeOf(ShowCentury),
     @ShowCentury);
  RegisterVar(CSClock, 'ShowDayOfWeek', ikBool, SizeOf(ShowDayOfWeek),
     @ShowDayOfWeek);
  RegisterVar(CSClock, 'DaysOfWeek', ikStr, SizeOf(DaysOfWeek),
     @DaysOfWeek);
  RegisterVar(CSClock, 'RightAlignClock', ikBool,
     SizeOf(RightAlignClock), @RightAlignClock); {FY 13-03-2000}
  {SmartPad}
  RegisterVar(CSSmartPad, 'InsertDate', ikBool, SizeOf(SPInsertDate),
     @SPInsertDate);
  RegisterVar(CSSmartPad, 'LineChar', ikUInt, SizeOf(SPLineChar),
     @SPLineChar); {SYR}
  {Game}
  RegisterVar(CSGame, 'EnableGame', ikBool, SizeOf(EnableGame),
     @EnableGame);
  {Clipboard}
  RegisterVar(CSClipboard, 'SaveClipboardOnExit', ikBool,
       SizeOf(cbAutoSave), @CBAutoSave);
  RegisterVar(CSClipboard, 'MaxClipboardSize', ikSInt, SizeOf(cbSize),
     @CBSize);
  {Kernel}
  //RegisterVar(CSKernel,    'UseLFN',               ikBool, SizeOf(CanUseLFN),            @CanUseLFN);
  RegisterVar(CSKernel, 'AutoSave', ikBool, SizeOf(AutoSave), @AutoSave);
  RegisterVar(CSKernel, 'ShowKeyCode', ikUInt, SizeOf(ShowKeyCode),
     @ShowKeyCode);
  RegisterVar(CSKernel, 'CopyLimit', ikSInt, SizeOf(CopyLimit),
     @CopyLimit);
  RegisterVar(CSKernel, 'StoreVideoMode', ikUInt,
     SizeOf(StoreVideoMode), @StoreVideoMode); {PZ}
  {DataCompBoy}
  RegisterVar(CSKernel, 'SmartWindowsBoxClose', ikSInt,
       SizeOf(SmartWindowsBoxClose), @SmartWindowsBoxClose);
  {DataCompBoy}
  RegisterVar(CSKernel, 'CtrlBreakKill', ikBool, SizeOf(CtrlBreakKill),
     @CtrlBreakKill); {AK155}
  //RegisterVar(CSKernel,    'DoVESATest',           ikBool, SizeOf(DoVESATest),           @DoVESATest);           {DataCompBoy}
  RegisterVar(CSKernel, 'ForceDefaultArchiver', ikStr,
       SizeOf(ForceDefaultArchiver), @ForceDefaultArchiver); {JO}
  {$IFDEF WIN32}
  RegisterVar(CSKernel, 'AltGrAsAlt', ikBool,
      SizeOf(AltGrAsAlt), @AltGrAsAlt); {JO}
  {$ENDIF}
  {$IFNDEF OS2}
  RegisterVar(CSKernel, 'DescrByShortNames', ikBool,
       SizeOf(DescrByShortNames), @DescrByShortNames); {JO}
  {$ENDIF}
  {Editor}
  RegisterVar(CSEditor, 'UnlimitUnindent', ikBool,
     SizeOf(UnlimitUnindent), @UnlimitUnindent);
  RegisterVar(CSEditor, 'Koi8rKeyMap', ikBool, SizeOf(Koi8rKeyMap),
     @Koi8rKeyMap);
  RegisterVar(CSEditor, 'DrawRShift', ikBool, SizeOf(DrawRShift),
     @DrawRShift);
  RegisterVar(CSEditor, 'AutoScopeDetect', ikBool,
     SizeOf(AutoScopeDetect), @AutoScopeDetect);
  RegisterVar(CSEditor, 'ShowBookmarks', ikBool, SizeOf(ShowBookmarks),
     @ShowBookmarks);
  RegisterVar(CSEditor, 'FastBookmark', ikBool, SizeOf(FastBookmark),
     @FastBookmark);
  RegisterVar(CSEditor, 'DefCodePageView', ikStr,
   SizeOf(DefCodePageView), @DefCodePageView);
  RegisterVar(CSEditor, 'DefCodePage', ikStr, SizeOf(DefCodePage),
     @DefCodePage);
  RegisterVar(CSEditor, 'CapitalCodePageName', ikBool,
       SizeOf(CapitalCodePageName), @CapitalCodePageName);
  RegisterVar(CSEditor, 'FastSearchDeep', ikSInt,
     SizeOf(FastSearchDeep), @FastSearchDeep);
  RegisterVar(CSEditor, 'WinManagerPosToEdit', ikBool,
       SizeOf(WinManagerPosToEdit), @WinManagerPosToEdit);
  RegisterVar(CSEditor, 'AutoBracketPairs', ikStr,
       SizeOf(AutoBracketPairs), @AutoBracketPairs);
  RegisterVar(CSEditor, 'F6_DuplicatesLine', ikBool,
       SizeOf(F6_DuplicatesLine), @F6_DuplicatesLine); {JO}
  {FilePanels}
  RegisterVar(CSFilePanels, 'ShowFileMask', ikBool,
     SizeOf(ShowFileMask), @ShowFileMask);
  RegisterVar(CSFilePanels, 'ShowLongName', ikBool,
     SizeOf(ShowLongName), @ShowLongName);
  RegisterVar(CSFilePanels, 'QuickRenameInDialog', ikBool,
       SizeOf(QuickRenameInDialog), @QuickRenameInDialog);
  RegisterVar(CSFilePanels, 'UpperCaseSorting', ikBool,
       SizeOf(UpperCaseSorting), @UpperCaseSorting); {JO}
  RegisterVar(CSFilePanels, 'AutoRefreshDriveLine', ikBool,
       SizeOf(AutoRefreshDriveLine), @AutoRefreshDriveLine); {Cat}
  RegisterVar(CSFilePanels, 'AutoRefreshPanels', ikBool,
       SizeOf(AutoRefreshPanels), @AutoRefreshPanels); {Cat}
  RegisterVar(CSFilePanels, 'QuickSearchType', ikUInt,
       SizeOf(QuickSearchType), @QuickSearchType); {Cat}
  RegisterVar(CSFilePanels, 'BriefSelectedInfo', ikBool,
       SizeOf(BriefSelectedInfo), @BriefSelectedInfo); {AK155}
  RegisterVar(CSFilePanels, 'BriefTotalInfo', ikBool,
       SizeOf(BriefTotalInfo), @BriefTotalInfo); {AK155}
  RegisterVar(CSFilePanels, 'PackingInfoInBottom', ikBool,
       SizeOf(PackingInfoInBottom), @PackingInfoInBottom); {AK155}
  RegisterVar(CSFilePanels, 'PathInfoInBottom', ikBool,
       SizeOf(PathInfoInBottom), @PathInfoInBottom); {AK155}
  {$IFDEF OS2}
  RegisterVar(CSFilePanels, 'ExecWin32GUI', ikStr, SizeOf(ExecWin32GUI),
     @ExecWin32GUI);
  RegisterVar(CSFilePanels, 'ExecWin32CUI', ikStr, SizeOf(ExecWin32CUI),
     @ExecWin32CUI);
  {$ENDIF}
  {NetInfo}
  RegisterVar(CSNetInfo, 'NoLevelsInfo', ikBool, SizeOf(NoLevelsInfo),
     @NoLevelsInfo);
  {TaskList}
  RegisterVar(CSTaskList, 'KillAllowed', ikBool, SizeOf(KillAllowed),
     @KillAllowed); {JO}
  RegisterVar(CSTaskList, 'ShowExePaths', ikBool, SizeOf(ShowExePaths),
     @ShowExePaths); {JO}
  RegisterVar(CSTaskList, 'FilteredList', ikBool, SizeOf(FilteredList),
     @FilteredList); {JO}
  RegisterVar(CSTaskList, 'UserTaskFilter', ikStr,
     SizeOf(UserTaskFilter), @UserTaskFilter); {JO}
  {Language}
  RegisterVar(CSLanguage, 'ActiveLanguage', ikStr,
     SizeOf(ActiveLanguage), @ActiveLanguage);
  RegisterVar(CSLanguage, 'HelpLanguageOverride', ikStr,
       SizeOf(HelpLanguageOverride), @HelpLanguageOverride);
  RegisterVar(CSLanguage, 'ShowLanguageMenu', ikBool,
       SizeOf(ShowLanguageMenu), @ShowLanguageMenu);
  {$IFDEF REGEXP}
  {RegExp}
  RegisterVar(CSRegExp, 'RegExpStr0', ikStr, SizeOf(RegExpStr0),
     @RegExpStr0); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr1', ikStr, SizeOf(RegExpStr1),
     @RegExpStr1); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr2', ikStr, SizeOf(RegExpStr2),
     @RegExpStr2); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr3', ikStr, SizeOf(RegExpStr3),
     @RegExpStr3); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr4', ikStr, SizeOf(RegExpStr4),
     @RegExpStr4); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr5', ikStr, SizeOf(RegExpStr5),
     @RegExpStr5); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr6', ikStr, SizeOf(RegExpStr6),
     @RegExpStr6); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr7', ikStr, SizeOf(RegExpStr7),
     @RegExpStr7); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr8', ikStr, SizeOf(RegExpStr8),
     @RegExpStr8); {PZ}
  RegisterVar(CSRegExp, 'RegExpStr9', ikStr, SizeOf(RegExpStr9),
     @RegExpStr9); {PZ}
  {$ENDIF}
  {QuickDirs}
  RegisterVar(CSQuickDirs, 'QDirs1', ikStr, SizeOf(QDirs1), @QDirs1);
  RegisterVar(CSQuickDirs, 'QDirs2', ikStr, SizeOf(QDirs2), @QDirs2);
  RegisterVar(CSQuickDirs, 'QDirs3', ikStr, SizeOf(QDirs3), @QDirs3);
  RegisterVar(CSQuickDirs, 'QDirs4', ikStr, SizeOf(QDirs4), @QDirs4);
  RegisterVar(CSQuickDirs, 'QDirs5', ikStr, SizeOf(QDirs5), @QDirs5);
  RegisterVar(CSQuickDirs, 'QDirs6', ikStr, SizeOf(QDirs6), @QDirs6);
  RegisterVar(CSQuickDirs, 'QDirs7', ikStr, SizeOf(QDirs7), @QDirs7);
  RegisterVar(CSQuickDirs, 'QDirs8', ikStr, SizeOf(QDirs8), @QDirs8);
  RegisterVar(CSQuickDirs, 'QDirs9', ikStr, SizeOf(QDirs9), @QDirs9);
  {SetupStorage}
  RegisterVar(CSSetupStorage, 'SystemDataOpt', ikSInt,
       SizeOf(SystemDataOpt), @SystemDataOpt);
  RegisterVar(CSSetupStorage, 'InterfaceDataOpt', ikSInt,
       SizeOf(InterfaceDataOpt), @InterfaceDataOpt);
  RegisterVar(CSSetupStorage, 'DriveInfoType', ikSInt,
       SizeOf(DriveInfoType), @DriveInfoType);
  RegisterVar(CSSetupStorage, 'FMSetupOpt', ikSInt, SizeOf(FMSetupOpt),
     @FMSetupOpt);
  RegisterVar(CSSetupStorage, 'EditorDefaultsOpt', ikSInt,
       SizeOf(EditorDefaultsOpt), @EditorDefaultsOpt);
  RegisterVar(CSSetupStorage, 'EditorDefaultsOpt2', ikSInt,
       SizeOf(EditorDefaultsOpt2), @EditorDefaultsOpt2);
  RegisterVar(CSSetupStorage, 'ViewerOpt', ikSInt, SizeOf(ViewerOpt),
     @ViewerOpt);
  RegisterVar(CSSetupStorage, 'StartupDataLoad', ikSInt,
       SizeOf(StartupDataLoad), @StartupDataLoad);
  RegisterVar(CSSetupStorage, 'StartupDataUnload', ikSInt,
       SizeOf(StartupDataUnload), @StartupDataUnload);
  RegisterVar(CSSetupStorage, 'StartupDataSlice2', ikSInt,
       SizeOf(StartupDataSlice2), @StartupDataSlice2);
  RegisterVar(CSSetupStorage, 'ConfirmsOpt', ikSInt,
     SizeOf(ConfirmsOpt), @ConfirmsOpt);
  RegisterVar(CSSetupStorage, 'NonVIOScreenMode', ikSInt,
       SizeOf(NonVIOScreenMode), @NonVIOScreenMode);
  RegisterVar(CSSetupStorage, 'VIOScreenMode', ikSInt,
       SizeOf(VIOScreenMode), @VIOScreenMode);
  end { Proceed };

function IniVarToStr(Kind: TIniItemKind; Size: word; PVar: Pointer)
  : String;
  var
    b: Boolean;
    i: integer;
  begin
  Result := '';

  case Kind of

    ikBool:
      begin
      case Size of
        1:
          b := Boolean(PVar^);
        2:
          b := Wordbool(PVar^);
        4:
          b := Longbool(PVar^);
        else {case}
          exit;
      end {case};
      if b then
        Result := '1'
      else
        Result := '0';
      end;

    ikEnum:
      begin
      if Size <= High(TUInt8) then
        Str(TUInt8(PVar^), Result)
      else
        Str(TUInt16(PVar^), Result);
      end;

    ikStr:
      begin
      Result := Copy(String(PVar^), 1, Size-1);
      end;

    ikSInt:
      begin
      case Size of
        1:
          Str(TSInt8(PVar^), Result);
        2:
          Str(TSInt16(PVar^), Result);
        4:
          Str(TSInt32(PVar^), Result);
      end {case};
      end;

    ikUInt:
      begin
      case Size of
        1:
          Str(TUInt8(PVar^), Result);
        2:
          Str(TUInt16(PVar^), Result);
      end {case};
      end;

    ikChar:
      begin
      Move(PVar^, Result[1], Size);
      SetLength(Result, Size) {result[0] := Chr(size);}
      end;

  end {case};
  end { IniVarToStr };

function IniStrToVar(Text: String; Kind: TIniItemKind; Size: word;
     PVar: Pointer): Boolean;
  var
    i: integer;
    buffer: array[0..3] of Byte;
    b: Boolean absolute buffer;
    i32: TSInt32 absolute buffer;
    i16: TSInt16 absolute buffer;
    i8: TSInt8 absolute buffer;
    u16: TUInt16 absolute buffer;
    u8: TUInt8 absolute buffer;
  begin
  IniStrToVar := False;
  case Kind of

    ikBool:
      begin
      UpStr(Text);
      if  (Text = '1') or (Text = 'TRUE') or (Text = 'YES')
           or (Text = 'ON')
      then
        b := True
      else if (Text = '0') or (Text = 'FALSE') or (Text = 'NO')
           or (Text = 'OFF')
      then
        b := False
      else
        exit;
      IniStrToVar := True;
      case Size of
        1:
          Boolean(PVar^) := b;
        2:
          Wordbool(PVar^) := b;
        4:
          Longbool(PVar^) := b;
      end {case};
      end;

    ikEnum:
      begin
      if Size <= High(u8) then
        begin
        Val(Text, u8, i);
        if  (i = 0) and (u8 <= Size) then
          begin
          IniStrToVar := True;
          Move(buffer, PVar^, 1);
          end;
        end
      else
        begin
        Val(Text, u16, i);
        if  (i = 0) and (u16 <= Size) then
          begin
          IniStrToVar := True;
          Move(buffer, PVar^, 2);
          end;
        end;
      end;

    ikStr:
      begin
      String(PVar^) := Copy(Text, 1, Size-1);
      IniStrToVar := Length(Text) < Size;
      end;

    ikSInt:
      begin
      case Size of
        1:
          Val(Text, i8, i);
        2:
          Val(Text, i16, i);
        4:
          Val(Text, i32, i);
      end {case};
      if i = 0 then
        begin
        IniStrToVar := True;
        Move(buffer, PVar^, Size);
        end;
      end;

    ikUInt:
      begin
      case Size of
        1:
          Val(Text, u8, i);
        2:
          Val(Text, u16, i);
      end {case};
      if i = 0 then
        begin
        IniStrToVar := True;
        Move(buffer, PVar^, Size);
        end;
      end;

    ikChar:
      begin
      if Text = '' then
        exit;
      i := Size;
      if Length(Text) < Size then
        begin
        IniStrToVar := True;
        i := Length(Text);
        end;
      Move(Text[1], PVar^, i);
      if i < Size then
        FillChar(PChar(PVar)[i], Size-i, 0);
      end;

  end {case};
  end { IniStrToVar };

procedure Loader(Group, Parameter: PChar; Kind: TIniItemKind;
     Size: word; PVar: Pointer);
  var
    s: String;
    b: Byte;
  begin
  s := IniVarToStr(Kind, Size, PVar)+#0;
  s[0] := Chr(GetPrivateProfileString(
        Group,
        Parameter,
        @s[1],
        @s[1],
        255,
        @FreeStr[1]
        ));
  if not IniStrToVar(s, Kind, Size, PVar) then
    begin
    b := 0;
    if  (Kind = ikStr) or (Kind = ikChar) then
      b := Length(s)+1-Size;
    AddIniError(b, Group, Parameter);
    end;
  end { Loader };

const
  SaveVar: Pointer = nil;

procedure Saver(Group, Parameter: PChar; Kind: TIniItemKind; Size: word;
     PVar: Pointer);
  var
    s: String;
    b: array[0..255] of Char;
  begin
  if  (SaveVar <> nil) and (SaveVar <> PVar) then
    exit;
  s := IniVarToStr(Kind, Size, PVar)+#0;
  { This code is provided only to not cut longer string if only a part }
  { was read in Loader procedure.                                      }
  if  ( (Kind = ikStr) and (Length(s) = (Size-1)))
    or ((Kind = ikChar) and (Length(s) = Size))
  then
    begin
    FillChar(b, SizeOf(b), 0);
    GetPrivateProfileString(
      Group,
      Parameter,
      @b[255], { default = empty string (null-terminated) }
      @b[0],
      255,
      @FreeStr[1]
      );
    if StrLComp(@b, @s[1], Length(s)) = 0 then
      exit;
    end;
  StrPCopy(b, s);
  WritePrivateProfileString(
    Group,
    Parameter,
    @b[0],
    @FreeStr[1]
    );
  end { Saver };

procedure LoadDnIniSettings;
  begin
  FreeStr := DnIniFileName+#0;
  Proceed(Loader);
  CloseProfile;
  end;

procedure SaveDnIniSettings(PVar: Pointer);
  begin
  SaveVar := PVar;
  FreeStr := DnIniFileName+#0;
  Proceed(Saver);
  CloseProfile;
  end;

procedure DoneIniEngine;
  begin
  CloseProfile;
  end;

{-DataCompBoy-}
function ProbeINI(var INItime, INIsize, INIcrc: LongInt): Boolean;
  var
    SR: lSearchRec;
    {F: TDosStream;}
    {B: PByteArray;}
    Pos: LongInt;
    S: word;
  begin
  INItime := 0;
  INIsize := 0;
  INIcrc := 0;
  lFindFirst(DnIniFileName, AnyFile, SR);
  if  (DosError = 0) and ((SR.SR.Attr and Directory) = 0) then
    begin
    ProbeINI := True;
    INItime := SR.SR.Time;
    INIsize := 0; {AK155 на случай DPMI, где Size: longint}
    Move(SR.SR.Size, INIsize, SizeOf(SR.SR.Size)); {AK155 Size: comp}
    {F.Init(SourceDir+'DN.INI', stOpenRead);
        S := Min(INIsize, 32767);
        if S>0 then begin
         GetMem(B, S);
         F.Read(B^, S);
         INIcrc := GetCrc(0, B^, S);
         FreeMem(B, S);
        end;
        F.Done;}
    end
  else
    ProbeINI := False;
  lFindClose(SR);
  end { ProbeINI };
{-DataCompBoy-}

end.
