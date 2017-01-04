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
//  dn15112-enhanced_calendar.patch
//  dn15112-history_button.patch
//  dn16rc1-Calendar_improvement_diff129byMV.patch
//  dn16rc1-piwcal22.patch
//
//  2.0.0
//  dn200-dnini_string_parametrs_fix.patch
//
//  2.3.0
//  dn230-remove_useless_CapitalCodePageName_option.patch
//
//  2.7.0
//  dn21128-show_LFN_and_desc_in_bottom.patch
//  dn21225-FilePanel(fi)-lfn_and_description_show_on_divider.patch
//  dn3315-CopyFile(n)-show_time_and_speed_added.patch
//  dn3421-Kernel(i)_AltGr_works_like_Alt.patch
//
//  3.7.0
//  dn370-clock(i)-disable_seconds_in_winnt.patch
//  dn31029-Compile_by_VP.patch
//  dn31220-built-in_result_variable.patch
//  dn40205-FViewer(f)-streams_auto_detect_code_page_fix.patch
//  dn40307-define_DN_Micro.patch
//
//  4.9.0
//  dn50202-viewer(if)-DefCodePageView.patch
//  dn50208-cleanup.patch
//
//  5.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit dnini;

interface

type TBlockMarker=record end;

const

  INIstoredsize:longint = 0;
  INIstoredtime:longint = 0;
{  INIstoredcrc :longint = 0;}{DataCompBoy}

  { DO NOT CHANGE THIS LINE } iniparamblock_START:TBlockMarker=();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

{ NOTE! }
{ To declare a short string that must have fixed length add at least }
{ one extra character to its length for better error checking.       }

{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{; Variable name           ; Type       ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Interface}
{}CutDriveInfo             : Boolean    = True;
{}DriveInfoType            : Byte       = 2;                          {DataCompBoy}
{}WinManagerSelectNext     : Boolean    = True;
{}DriveSelectVCenter       : Boolean    = False;                          {-$X-Man}
{}SystemMenuChar           : Byte       = 4;
{}HorizScrollBarChars      : string[6]  = #17#16#177#254#178;         {DataCompBoy}
{}VertScrollBarChars       : string[6]  = #30#31#177#254#178;         {DataCompBoy}
{}FadeDelay                : Byte       = 0;                                {Knave}
{}ReflectCopyDirection     : Boolean    = False;
{}ReuseViewers             : Byte       = 0;                 { 0 - always open new}
{}ReuseEditors             : Byte       = 0;                 { 1 - prompt for open}
{}                                                           { 2 - do not open new}
{}HistoryErrorBeep         : Boolean    = True;                       {DataCompBoy}
{}PreserveMenuPositions    : Boolean    = False;                      {DataCompBoy}
{$IFDEF COLUMNSMENU}
{}PanelDescrArvid          : string     = '';                         {JO}
{}PanelDescrArc            : string     = '';                         {JO}
{}PanelDescrTemp           : string     = '';                         {JO}
{}PanelDescrFind           : string     = '';                         {JO}
{}PanelDescrDrive          : string     = '';                         {JO}
{$ENDIF}
{}UseEnterInViewer         : Byte       = 0;                          {JO}
{}UseF3InViewer            : Byte       = 0;                   (* X-Man *)
{}PoorFrames               : Boolean    = False;                      {PZ}
{}HistoryButtonChars       : string[4]  = #222#25#221;                {PZ}
{}ShowCopyTime             : Boolean    = False;                      {John_SW 22-03-2003}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Clock}
{}AutoDisableSecondsWNT    : Boolean    = True;
{}ShowSeconds              : Boolean    = True;
{}BlinkSeparator           : Boolean    = True; {JO}
{}ShowCentury              : Boolean    = True;
{}ShowDayOfWeek            : Boolean    = True;
{}DaysOfWeek               : string[23] = '(use language default)';
{}RightAlignClock          : Boolean    = False;                    {FY 13-03-2000}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{SmartPad}
{}SPInsertDate             : Boolean    = True;
{}SPLineChar               : Byte       = 196;                        {X-Man} {SYR}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Game}
{$IFDEF GAME}
{}EnableGame               : Boolean    = True;
{$ENDIF}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Clipboard}
{}CBSize                   : Longint    = 4096;                             {-$VOL}
{}CBAutoSave               : Boolean    = True;                             {-$VOL}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Kernel}
{}CanUseLFN                : Boolean    = True;
{}AutoSave                 : Boolean    = True;
{}ShowKeyCode              : Byte       = 0;
{}CopyLimit                : LongInt    = 8192;             {DataCompBoy}{piwamoto}
{}HandleChDirCommand       : Boolean    = True;                       {DataCompBoy}
{}StoreVideoMode           : Byte       = 0;                      {PZ}{DataCompBoy}
{}SmartWindowsBoxClose     : Boolean    = False;                      {DataCompBoy}
{}DoVESATest               : Boolean    = False;                      {DataCompBoy}
{}AltGrAsAlt               : Boolean    = True;                       {John_SW  01-05-2003}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Editor}
{}UnlimitUnindent          : Boolean    = False;
{}Koi8rKeyMap              : Boolean    = True;
{}DrawRShift               : Boolean    = False;
{}AutoScopeDetect          : Boolean    = True;
{}ShowBookmarks            : Boolean    = True;
{}FastBookmark             : Boolean    = True;
{}DefCodePageView          : string[5]  = 'AUTO';
{}DefCodePage              : string[5]  = 'AUTO';
{}FastSearchDeep           : Longint    = 0;
{}WinManagerPosToEdit      : Boolean    = True;
{}AutoBracketPairs         : string     = '()[]{}<>';
{}RecombineLongLines       : Boolean    = False; {JO}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{FilePanels}
{}ShowFileMask             : Boolean    = True;
{}ShowLongName             : Byte       = 1;                  {John_SW 18-01-2003}
{}AlignLFNInBottom         : Byte       = 1;                  {John_SW 18-01-2003}
{}LFNCutSide               : Byte       = 1;                  {John_SW 18-01-2003}
{}QuickRenameInDialog      : Boolean    = False;
{}QuickSearchType          : Boolean    = False; {Cat}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{NetInfo}
{$IFDEF NETINFO}
{}NoLevelsInfo             : Boolean    = False;
{$ENDIF}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Language}
{}ActiveLanguage           : string[9]  = '';
{}HelpLanguageOverride     : string[9]  = '';
{}ShowLanguageMenu         : Boolean    = False;
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type     ; Default value      ;          Comments ;}
{`.________________________;____________;____________________;___________________'}
{}{Calendar}
{$IFDEF CALENDAR}
{}CalShowOnStartup         : Boolean    = False;                               {PZ}
{}CalSundayFirst           : Byte       = 2;                         {piwamoto}{PZ}
{}CalDefaultInfo           : string[30] = '';                                  {PZ}
{}CalOptionFlags           : byte       = 0;                                   {PZ}
{$ENDIF}
{,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~~`,}
{;                   This is end of parameters definitions                       ;}
{`.______________________________________________________________________________'}


  { ----------------------------- NO INI VARIABLES BEYOND HERE }
  { DO NOT CHANGE THIS LINE } iniparamblock_END:TBlockMarker=();

function DnIniFileName : string;

procedure LoadDnIniSettings;
procedure SaveDnIniSettings ( PVar : Pointer );
procedure DoneIniEngine;

function ProbeINI(var INItime,INIsize{,INIcrc}:longint):boolean;

procedure ShowIniErrors;
procedure ClearIniErrors;

implementation
uses dos, LFN, profile, advance, advance1, Commands, Collect, Messages, DnApp,
     {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF};

type

{$IFNDEF NONBP}
  SmallInt = Integer;
{$ENDIF}
  TSInt8  = ShortInt;
  TSInt16 = SmallInt;
  TSInt32 = LongInt;
  TUInt8  = Byte;
  TUInt16 = Word;

  TIniItemKind = (
    ikBool,
    ikEnum,
    ikStr,
    ikSInt,
    ikUInt,
    ikChar
  );

  TDoProc = procedure ( Group, Parameter : PChar; Kind: TIniItemKind; Size : Word; PVar: Pointer );

const
{Group names}
 CSInterface            :PChar = 'Interface';
 CSClock                :PChar = 'Clock';
 CSSmartPad             :PChar = 'SmartPad';
 CSGame                 :PChar = 'Game';
 CSClipboard            :PChar = 'Clipboard';
 CSKernel               :PChar = 'Kernel';
 CSEditor               :PChar = 'Editor';
 CSFilePanels           :PChar = 'FilePanels';
{$IFDEF NETINFO}
 CSNetInfo              :PChar = 'NetInfo';
{$ENDIF}
 CSLanguage             :PChar = 'Language';
{$IFDEF CALENDAR}
 CSCalendar             :PChar = 'Calendar';
{$ENDIF}

type
  PIniError = ^TIniError;
  TIniError = record
    Size      : Byte;
    Group     : PChar;
    Parameter : PChar;
  end;

  PIniErrors = ^TIniErrors;
  TIniErrors = object (TCollection)
    procedure FreeItem ( Item : Pointer ); virtual;
  end;

const
  IniErrors : PIniErrors = nil;

procedure TIniErrors.FreeItem ( Item : Pointer );
var
  P : PIniError absolute Item;
begin
  Dispose ( P );
end;

procedure AddIniError ( Size : Byte; Group : PChar; Parameter : PChar );
var
  p : PIniError;
begin
  if IniErrors = nil then
    IniErrors := New ( PIniErrors, Init ( 1, 1 ) );
  New ( p );
  p^.Size      := Size;
  p^.Group     := Group;
  p^.Parameter := Parameter;
  IniErrors^.Insert ( p );
end;

procedure ShowIniErrors;

  procedure Show ( P : Pointer );
  type
    TParams = record
      Size      : Longint;
      Group     : PChar;
      Parameter : PChar;
    end;
  var
    I   : PIniError absolute P;
    sg  : string[10];
    sp  : string[21];
    id  : TStrIdx;
    flg : Word;
    q   : Pointer;
    par : TParams;
  begin
    if P = nil then
      Exit;
    sg            := StrPas ( I^.Group );
    sp            := StrPas ( I^.Parameter );
    flg           := mfOKButton;
    par.Group     := @sg;
    par.Parameter := @sp;
    if I^.Size > 0 then begin
      par.Size := I^.Size;
      Inc ( flg, mfWarning );
      id := dlIniDataTooLong;
      q  := @par.Size;
    end else begin
      Inc ( flg, mfError );
      id := dlIniDataReadError;
      q  := @par.Group;
    end;
    Msg ( id, q, flg );
  end;

begin
  if IniErrors <> nil then begin
    IniErrors^.ForEach ( @Show );
    ClearIniErrors;
  end;
end;

procedure ClearIniErrors;
begin
  if IniErrors <> nil then begin
    IniErrors^.FreeAll;
    Dispose ( IniErrors, Done );
    IniErrors := nil;
  end;
end;

function DnIniFileName : string;
begin
  DnIniFileName := SourceDir + 'DN.INI';
end;

procedure Proceed(RV: TDoProc);
begin
  {Interface}
  RV(CSInterface, 'CutDriveInfo',         ikBool, SizeOf(CutDriveInfo),         @CutDriveInfo);
  RV(CSInterface, 'DriveInfoType',        ikUInt, SizeOf(DriveInfoType),        @DriveInfoType);
  RV(CSInterface, 'WinManagerSelectNext', ikBool, SizeOf(WinManagerSelectNext), @WinManagerSelectNext);
  RV(CSInterface, 'DriveSelectVCenter',   ikBool, SizeOf(DriveSelectVCenter),   @DriveSelectVCenter); {-$X-Man}
  RV(CSInterface, 'SystemMenuChar',       ikUInt, SizeOf(SystemMenuChar),       @SystemMenuChar);
  RV(CSInterface, 'HorizScrollBarChars',  ikStr,  SizeOf(HorizScrollBarChars),  @HorizScrollBarChars);
  RV(CSInterface, 'VertScrollBarChars',   ikStr,  SizeOf(VertScrollBarChars),   @VertScrollBarChars);
  RV(CSInterface, 'FadeDelay',            ikUInt, SizeOf(FadeDelay),            @FadeDelay);
  RV(CSInterface, 'ReflectCopyDirection', ikBool, SizeOf(ReflectCopyDirection), @ReflectCopyDirection);
  RV(CSInterface, 'ReuseViewers',         ikUInt, SizeOf(ReuseViewers),         @ReuseViewers);
  RV(CSInterface, 'ReuseEditors',         ikUInt, SizeOf(ReuseEditors),         @ReuseEditors);
  RV(CSInterface, 'HistoryErrorBeep',     ikBool, SizeOf(HistoryErrorBeep),     @HistoryErrorBeep);
  RV(CSInterface, 'PreserveMenuPositions',ikBool, SizeOf(PreserveMenuPositions),@PreserveMenuPositions);
{$IFDEF COLUMNSMENU}
  RV(CSInterface, 'PanelDescrArvid',      ikStr,  SizeOf(PanelDescrArvid),      @PanelDescrArvid);    {JO}
  RV(CSInterface, 'PanelDescrArc',        ikStr,  SizeOf(PanelDescrArc),        @PanelDescrArc);      {JO}
  RV(CSInterface, 'PanelDescrTemp',       ikStr,  SizeOf(PanelDescrTemp),       @PanelDescrTemp);     {JO}
  RV(CSInterface, 'PanelDescrFind',       ikStr,  SizeOf(PanelDescrFind),       @PanelDescrFind);     {JO}
  RV(CSInterface, 'PanelDescrDrive',      ikStr,  SizeOf(PanelDescrDrive),      @PanelDescrDrive);    {JO}
{$ENDIF}
  RV(CSInterface, 'UseEnterInViewer',     ikUInt, SizeOf(UseEnterInViewer),     @UseEnterInViewer);   {JO}
  RV(CSInterface, 'UseF3InViewer',        ikUInt, SizeOf(UseF3InViewer),        @UseF3InViewer); (* X-Man *)
  RV(CSInterface, 'PoorFrames',           ikBool, SizeOf(PoorFrames),           @PoorFrames);         {PZ}
  RV(CSInterface, 'HistoryButtonChars',   ikStr,  SizeOf(HistoryButtonChars),   @HistoryButtonChars); {PZ}
  RV(CSInterface, 'ShowCopyTime',         ikBool, SizeOf(ShowCopyTime),         @ShowCopyTime);       {John_SW 22-03-2003}
  {Clock}
  RV(CSClock,     'AutoDisableSecondsWNT',ikBool, SizeOf(AutoDisableSecondsWNT),@AutoDisableSecondsWNT); { Flash 14-07-2003 }
  RV(CSClock,     'ShowSeconds',          ikBool, SizeOf(ShowSeconds),          @ShowSeconds);
  RV(CSClock,     'BlinkSeparator',       ikBool, SizeOf(BlinkSeparator),       @BlinkSeparator);
  RV(CSClock,     'ShowCentury',          ikBool, SizeOf(ShowCentury),          @ShowCentury);
  RV(CSClock,     'ShowDayOfWeek',        ikBool, SizeOf(ShowDayOfWeek),        @ShowDayOfWeek);
  RV(CSClock,     'DaysOfWeek',           ikStr,  SizeOf(DaysOfWeek),           @DaysOfWeek);
  RV(CSClock,     'RightAlignClock',      ikBool, SizeOf(RightAlignClock),      @RightAlignClock); {FY 13-03-2000}
  {SmartPad}
  RV(CSSmartPad,  'InsertDate',           ikBool, SizeOf(SPInsertDate),         @SPInsertDate);
  RV(CSSmartPad,  'LineChar',             ikUInt, SizeOf(SPLineChar),           @SPLineChar); {SYR}
{$IFDEF GAME}
  {Game}
  RV(CSGame,      'EnableGame',           ikBool, SizeOf(EnableGame),           @EnableGame);
{$ENDIF}
  {Clipboard}
  RV(CSClipboard, 'SaveClipboardOnExit',  ikBool, SizeOf(CBAutoSave),           @CBAutoSave);
  RV(CSClipboard, 'MaxClipboardSize',     ikSInt, SizeOf(CBSize),               @CBSize);
  {Kernel}
  RV(CSKernel,    'UseLFN',               ikBool, SizeOf(CanUseLFN),            @CanUseLFN);
  RV(CSKernel,    'AutoSave',             ikBool, SizeOf(AutoSave),             @AutoSave);
  RV(CSKernel,    'ShowKeyCode',          ikUInt, SizeOf(ShowKeyCode),          @ShowKeyCode);
  RV(CSKernel,    'CopyLimit',            ikSInt, SizeOf(CopyLimit),            @CopyLimit);
  RV(CSKernel,    'HandleChDirCommand',   ikBool, SizeOf(HandleChDirCommand),   @HandleChDirCommand);   {DataCompBoy}
  RV(CSKernel,    'StoreVideoMode',       ikUInt, SizeOf(StoreVideoMode),       @StoreVideoMode);       {PZ}{DataCompBoy}
  RV(CSKernel,    'SmartWindowsBoxClose', ikBool, SizeOf(SmartWindowsBoxClose), @SmartWindowsBoxClose); {DataCompBoy}
  RV(CSKernel,    'DoVESATest',           ikBool, SizeOf(DoVESATest),           @DoVESATest);           {DataCompBoy}
  RV(CSKernel,    'AltGrAsAlt',           ikBool, SizeOf(AltGrAsAlt),           @AltGrAsAlt);           {John_SW  01-05-2003}
  {Editor}
  RV(CSEditor,    'UnlimitUnindent',      ikBool, SizeOf(UnlimitUnindent),      @UnlimitUnindent);
  RV(CSEditor,    'Koi8rKeyMap',          ikBool, SizeOf(Koi8rKeyMap),          @Koi8rKeyMap);
  RV(CSEditor,    'DrawRShift',           ikBool, SizeOf(DrawRShift),           @DrawRShift);
  RV(CSEditor,    'AutoScopeDetect',      ikBool, SizeOf(AutoScopeDetect),      @AutoScopeDetect);
  RV(CSEditor,    'ShowBookmarks',        ikBool, SizeOf(ShowBookmarks),        @ShowBookmarks);
  RV(CSEditor,    'FastBookmark',         ikBool, SizeOf(FastBookmark),         @FastBookmark);
  RV(CSEditor,    'DefCodePageView',      ikStr,  SizeOf(DefCodePageView),      @DefCodePageView);
  RV(CSEditor,    'DefCodePage',          ikStr,  SizeOf(DefCodePage),          @DefCodePage);
  RV(CSEditor,    'FastSearchDeep',       ikSInt, SizeOf(FastSearchDeep),       @FastSearchDeep);
  RV(CSEditor,    'WinManagerPosToEdit',  ikBool, SizeOf(WinManagerPosToEdit),  @WinManagerPosToEdit);
  RV(CSEditor,    'AutoBracketPairs',     ikStr,  SizeOf(AutoBracketPairs),     @AutoBracketPairs);
  RV(CSEditor,    'RecombineLongLines',   ikBool, SizeOf(RecombineLongLines),   @RecombineLongLines); {JO}
  {FilePanels}
  RV(CSFilePanels,'ShowFileMask',         ikBool, SizeOf(ShowFileMask),         @ShowFileMask);
  RV(CSFilePanels,'ShowLongName',         ikUInt, SizeOf(ShowLongName),         @ShowLongName);       {John_SW 18-01-2003}
  RV(CSFilePanels,'AlignLFNInBottom',     ikUInt, SizeOf(AlignLFNInBottom),     @AlignLFNInBottom);   {John_SW 18-01-2003}
  RV(CSFilePanels,'LFNCutSide',           ikUInt, SizeOf(LFNCutSide),           @LFNCutSide);         {John_SW 18-01-2003}
  RV(CSFilePanels,'QuickRenameInDialog',  ikBool, SizeOf(QuickRenameInDialog),  @QuickRenameInDialog);
  RV(CSFilePanels,'QuickSearchType',      ikBool, SizeOf(QuickSearchType),      @QuickSearchType);
{$IFDEF NETINFO}
  {NetInfo}
  RV(CSNetInfo,   'NoLevelsInfo',         ikBool, SizeOf(NoLevelsInfo),         @NoLevelsInfo);
{$ENDIF}
  {Language}
  RV(CSLanguage,  'ActiveLanguage',       ikStr,  SizeOf(ActiveLanguage),       @ActiveLanguage);
  RV(CSLanguage,  'HelpLanguageOverride', ikStr,  SizeOf(HelpLanguageOverride), @HelpLanguageOverride);
  RV(CSLanguage,  'ShowLanguageMenu',     ikBool, SizeOf(ShowLanguageMenu),     @ShowLanguageMenu);
{$IFDEF CALENDAR}
  {Calendar}
  RV(CSCalendar,  'ShowOnStartup',        ikBool, SizeOf(CalShowOnStartup),     @CalShowOnStartup);  {PZ}
  RV(CSCalendar,  'SundayFirst',          ikUInt, SizeOf(CalSundayFirst),       @CalSundayFirst);    {PZ}
  RV(CSCalendar,  'DefaultInfo',          ikStr,  SizeOf(CalDefaultInfo),       @CalDefaultInfo);    {PZ}
  RV(CSCalendar,  'OptionFlags',          ikUInt, SizeOf(CalOptionFlags),       @CalOptionFlags);    {PZ}
{$ENDIF}
end;

function IniVarToStr ( Kind : TIniItemKind; Size : Word; PVar : Pointer ) : string;
var
  b      : Boolean;
{$IFNDEF RESULT}Var Result: String;{$ENDIF}
begin
  result := '';

  case Kind of

    ikBool : begin
      case Size of
        1: b := Boolean(pvar^);
        2: b := Wordbool(pvar^);
        4: b := Longbool(pvar^);
      else Exit;
      end;
      if b then
        result := '1'
      else
        result := '0';
    end;

    ikEnum : begin
      if Size <= High(TUint8) then
        Str ( TUInt8(pvar^), result )
      else
        Str ( TUInt16(pvar^), result );
    end;

    ikStr : begin
      result := Copy ( string(pvar^), 1, Size-1 );
    end;

    ikSInt : begin
      case Size of
        1: Str ( TSInt8(pvar^), result );
        2: Str ( TSInt16(pvar^), result );
        4: Str ( TSInt32(pvar^), result );
      end;
    end;

    ikUInt : begin
      case Size of
        1: Str ( TUInt8(pvar^), result );
        2: Str ( TUInt16(pvar^), result );
      end;
    end;

    ikChar : begin
      Move ( pvar^, result[1], Size );
      result[0] := Chr(size);
    end;

  end;
{$IFNDEF RESULT}IniVarToStr:=Result;{$ENDIF}
end;

function IniStrToVar ( Text : string; Kind : TIniItemKind; Size : Word; PVar : Pointer ) : Boolean;
var
  i      : Integer;
  buffer : array [0..3] of Byte;
  b      : Boolean absolute buffer;
  i32    : TSInt32 absolute buffer;
  i16    : TSInt16 absolute buffer;
  i8     : TSInt8  absolute buffer;
  u16    : TUInt16 absolute buffer;
  u8     : TUInt8  absolute buffer;
begin
  IniStrToVar := False;
  case Kind of

    ikBool : begin
      UpStr ( Text );
      if (Text = '1') or (Text = 'TRUE') or (Text = 'YES') or (Text = 'ON') then
        b := True
      else if (Text = '0') or (Text = 'FALSE') or (Text = 'NO') or (Text = 'OFF') then
        b := False
      else
        Exit;
      IniStrToVar := True;
      case Size of
        1: Boolean(pvar^)  := b;
        2: Wordbool(pvar^) := b;
        4: Longbool(pvar^) := b;
      end;
    end;

    ikEnum : begin
      if Size <= High(u8) then begin
        Val ( Text, u8, i );
        if (i = 0) and (u8 <= Size) then begin
          IniStrToVar := True;
          Move ( buffer, pvar^, 1 );
        end;
      end else begin
        Val ( Text, u16, i );
        if (i = 0) and (u16 <= Size) then begin
          IniStrToVar := True;
          Move ( buffer, pvar^, 2 );
        end;
      end;
    end;

    ikStr : begin
      string(pvar^) := Copy ( Text, 1, Size-1 );
      IniStrToVar := Length(Text) < Size;
    end;

    ikSInt : begin
      case Size of
        1: Val ( Text, i8, i );
        2: Val ( Text, i16, i );
        4: Val ( Text, i32, i );
      end;
      if i = 0 then begin
        IniStrToVar := True;
        Move ( buffer, pvar^, Size );
      end;
    end;

    ikUInt : begin
      case Size of
        1: Val ( Text, u8, i );
        2: Val ( Text, u16, i );
      end;
      if i = 0 then begin
        IniStrToVar := True;
        Move ( buffer, pvar^, Size );
      end;
    end;

    ikChar : begin
      if Text = '' then
        Exit;
      i := size;
      if Length(Text) < Size then begin
        IniStrToVar := True;
        i := Length(Text);
      end;
      Move ( Text[1], pvar^, i );
      if i < size then
        FillChar ( PChar(pvar)[i], Size-i, 0 );
    end;

  end;
end;

procedure Loader ( Group, Parameter: PChar; Kind: TIniItemKind; Size : Word; PVar: Pointer );
var
  b : Byte;
  s : string;
begin
  s    := IniVarToStr ( Kind, Size, PVar ) + #0;
  s[0] := Chr ( GetPrivateProfileString (
            Group,
            Parameter,
            @s[1],
            @s[1],
            255,
            @FreeStr[1]
          ));
  if not IniStrToVar ( s, Kind, Size, PVar ) then begin
    b := 0;
    if (Kind = ikStr) or (Kind = ikChar) then
      b := Length(s)+1 - Size;
    AddIniError ( b, Group, Parameter );
  end;
end;

const
  SaveVar : Pointer = nil;

procedure Saver ( Group, Parameter: PChar; Kind: TIniItemKind; Size : Word; PVar: Pointer );
var
  b : array [0..255] of Char;
  s : string;
begin
  if (SaveVar <> nil) and (SaveVar <> PVar) then
    Exit;
  s := IniVarToStr ( Kind, Size, PVar );
  { This code is provided only to not cut longer string if only a part }
  { was read in Loader procedure.                                      }
  if ((Kind = ikStr) and (Length(s) = (Size-1)))
  or ((Kind = ikChar) and (Length(s) = Size)) then begin
    FillChar ( b, SizeOf(b), 0 );
    GetPrivateProfileString (
      Group,
      Parameter,
      @b[255],   { default = empty string (null-terminated) }
      @b[0],
      255,
      @FreeStr[1]
    );
    if StrLComp ( @b, @s[1], Length(s) ) = 0 then
      Exit;
  end;
  StrPCopy ( b, s );
  WritePrivateProfileString (
    Group,
    Parameter,
    @b[0],
    @FreeStr[1]
  );
end;

procedure LoadDnIniSettings;
begin
  FreeStr := DnIniFileName + #0;
  Proceed ( Loader );
  CloseProfile;
end;

procedure SaveDnIniSettings ( PVar : Pointer );
begin
  SaveVar := PVar;
  FreeStr := DnIniFileName + #0;
  Proceed ( Saver );
  CloseProfile;
end;

procedure DoneIniEngine;
begin
  CloseProfile;
end;

        {-DataCompBoy-}
function ProbeINI(var INItime,INIsize{,INIcrc}:longint):boolean;
var
    Pos: Longint;
    S: Word;
    SR:lSearchRec;
    {F: TDosStream;}
    {B: PByteArray;}
begin
    INItime:=0;
    INIsize:=0;
{    INIcrc :=0;}
    lFindFirst(DnIniFileName,AnyFile,SR);
    if (DosError=0) and ((SR.SR.Attr and Directory)=0) then begin
        ProbeINI:=True;
        INItime:=SR.SR.Time;
        INIsize:=SR.SR.Size;
{
        INIcrc :=0;
        F.Init(SourceDir+'DN.INI', stOpenRead);
        S := Min(INIsize, 32767);
        if S>0 then begin
         GetMem(B, S);
         F.Read(B^, S);
         INIcrc := GetCrc(0, B^, S);
         FreeMem(B, S);
        end;
        F.Done;
}
    end else ProbeINI:=False;
    lFindClose(SR);
end;
        {-DataCompBoy-}

end.
