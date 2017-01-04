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
//////////////////////////////////////////////////////////////////////////
//
//  Ini Configuration by VIV (C) 1999
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit dnini;
interface
uses {$IFNDEF NONBP}BStrings{$ELSE}Strings{$ENDIF};


type TBlockMarker=record end;

const

  INIstoredsize:longint = 0;
  INIstoredtime:longint = 0;
  INIstoredcrc :longint = 0;{DataCompBoy}

  { DO NOT CHANGE THIS LINE } iniparamblock_START:TBlockMarker=();
  { ------------------------------ PLACE ALL INI VARIABLES BELOW }

{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{; Variable name           ; Type    ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Interface}
{}CutDriveInfo             : Boolean = True;
{}DriveInfoType            : Longint = 2;                          {DataCompBoy}
{}WinManagerSelectNext     : Boolean = True;
{}DriveSelectVCenter       : Boolean = False;                          {-$X-Man}
{}SystemMenuChar           : Longint = 4;
{}HorizScrollBarChars      : String  = #17#16#177#254#178;         {DataCompBoy}
{}VertScrollBarChars       : String  = #30#31#177#254#178;         {DataCompBoy}
{}FadeDelay                : Longint = 0;                                {Knave}
{}ReflectCopyDirection     : Boolean = False;
{}ReuseViewers             : Longint = 0;                 { 0 - always open new}
{}ReuseEditors             : Longint = 0;                 { 1 - prompt for open}
{}                                                        { 2 - do not open new}
{}HistoryErrorBeep         : Boolean = True;                       {DataCompBoy}
{}PreserveMenuPositions    : Boolean = False;                      {DataCompBoy}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Clock}
{}ShowSeconds              : Boolean = True;
{}ShowCentury              : Boolean = True;
{}ShowDayOfWeek            : Boolean = True;
{}DaysOfWeek               : String  = '(use language default)';
{}RightAlignClock          : Boolean = False;                    {FY 13-03-2000}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{SmartPad}
{}SPInsertDate             : Boolean = True;
{}SPLineChar               : Longint = 196;                        {X-Man} {SYR}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Game}
{}EnableGame               : Boolean = True;
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Clipboard}
{}CBSize                   : Longint = 4096;                             {-$VOL}
{}CBAutoSave               : Boolean = True;                             {-$VOL}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Kernel}
{}CanUseLFN                : Boolean = True;
{}AutoSave                 : Boolean = True;
{}ShowKeyCode              : Longint = 0;
{}CopyLimit                : LongInt = 8192;             {DataCompBoy}{piwamoto}
{}HandleChDirCommand       : Boolean = True;                       {DataCompBoy}
{}StoreVideoMode           : Longint = 0;                      {PZ}{DataCompBoy}
{}SmartWindowsBoxClose     : Boolean = False;                      {DataCompBoy}
{}DoVESATest               : Boolean = False;                      {DataCompBoy}
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Editor}
{}UnlimitUnindent          : Boolean = False;
{}Koi8rKeyMap              : Boolean = True;
{}DrawRShift               : Boolean = True;
{}AutoScopeDetect          : Boolean = True;
{}ShowBookmarks            : Boolean = True;
{}FastBookmark             : Boolean = True;
{}DefCodePage              : String  = 'Dos';
{}CapitalCodePageName      : Boolean = False;
{}FastSearchDeep           : Longint = 0;
{}WinManagerPosToEdit      : Boolean = True;
{}AutoBracketPairs         : String  = '()[]{}<>';
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{FilePanels}
{}ShowFileMask             : Boolean = True;
{}ShowLongName             : Boolean = True;
{}QuickRenameInDialog      : Boolean = False;
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{NetInfo}
{}NoLevelsInfo             : Boolean = False;
{,'~~~~~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~;~~~~~~~~~~~~~~~~~~~~;~~~~~~~~~~~~~~~~~~`,}
{;      Variable name      ;   Type  ; Default value      ;          Comments ;}
{`.________________________;_________;____________________;___________________'}
{}{Language}
{}ActiveLanguage           : String  = '';
{}HelpLanguageOverride     : String  = '';
{}ShowLanguageMenu         : Boolean = False;
{,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`~~~~~~~~~~~~`,}
{;                   This is end of parameters definitions                    ;}
{`.___________________________________________________________________________'}


  { ----------------------------- NO INI VARIABLES BEYOND HERE }
  { DO NOT CHANGE THIS LINE } iniparamblock_END:TBlockMarker=();

procedure LoadDnIniSettings;
procedure SaveDnIniSettings;
procedure DoneIniEngine;

function ProbeINI(var INItime,INIsize,INIcrc:longint):boolean;

implementation
uses dos, LFN, profile, advance, advance1, advance3, objects;

type
  TIniItemKind = (ikBoolean, ikString, ikLongint, ikChar);
  TDoProc = Procedure (const Group, Parameter: PChar; ItemKind: TIniItemKind; PVar: Pointer);

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
 CSNetInfo              :PChar = 'NetInfo';
 CSLanguage             :PChar = 'Language';

procedure Proceed(RegisterVar: TDoProc);
begin
  {Interface}
  RegisterVar(CSInterface, 'CutDriveInfo',         ikBoolean, @CutDriveInfo);
  RegisterVar(CSInterface, 'DriveInfoType',        ikLongint, @DriveInfoType);
  RegisterVar(CSInterface, 'WinManagerSelectNext', ikBoolean, @WinManagerSelectNext);
  RegisterVar(CSInterface, 'DriveSelectVCenter',   ikBoolean, @DriveSelectVCenter); {-$X-Man}
  RegisterVar(CSInterface, 'SystemMenuChar',       ikLongint, @SystemMenuChar);
  RegisterVar(CSInterface, 'HorizScrollBarChars',  ikString,  @HorizScrollBarChars);
  RegisterVar(CSInterface, 'VertScrollBarChars',   ikString,  @VertScrollBarChars);
  RegisterVar(CSInterface, 'FadeDelay',            ikLongInt, @FadeDelay);
  RegisterVar(CSInterface, 'ReflectCopyDirection', ikBoolean, @ReflectCopyDirection);
  RegisterVar(CSInterface, 'ReuseViewers',         ikLongInt, @ReuseViewers);
  RegisterVar(CSInterface, 'ReuseEditors',         ikLongInt, @ReuseEditors);
  RegisterVar(CSInterface, 'HistoryErrorBeep',     ikBoolean, @HistoryErrorBeep);
  RegisterVar(CSInterface, 'PreserveMenuPositions',ikBoolean, @PreserveMenuPositions);
  {Clocl}
  RegisterVar(CSClock, 'ShowSeconds',     ikBoolean, @ShowSeconds);
  RegisterVar(CSClock, 'ShowCentury',     ikBoolean, @ShowCentury);
  RegisterVar(CSClock, 'ShowDayOfWeek',   ikboolean, @ShowDayOfWeek);
  RegisterVar(CSClock, 'DaysOfWeek',      ikString,  @DaysOfWeek);
  RegisterVar(CSClock, 'RightAlignClock', ikBoolean, @RightAlignClock); {FY 13-03-2000}
  {SmartPad}
  RegisterVar(CSSmartPad, 'InsertDate', ikBoolean, @SPInsertDate);
  RegisterVar(CSSmartPad, 'LineChar',   ikLongint, @SPLineChar); {SYR}
  {Game}
  RegisterVar(CSGame, 'EnableGame', ikBoolean, @EnableGame);
  {Clipboard}
  RegisterVar(CSClipboard, 'SaveClipboardOnExit', ikBoolean, @CBAutoSave);
  RegisterVar(CSClipboard, 'MaxClipboardSize',    ikLongint, @CBSize);
  {Kernel}
  RegisterVar(CSKernel, 'UseLFN',                ikBoolean, @CanUseLFN);
  RegisterVar(CSKernel, 'AutoSave',              ikBoolean, @AutoSave);
  RegisterVar(CSKernel, 'ShowKeyCode',           ikLongInt, @ShowKeyCode);
  RegisterVar(CSKernel, 'CopyLimit',             ikLongInt, @CopyLimit);
  RegisterVar(CSKernel, 'HandleChDirCommand',    ikBoolean, @HandleChDirCommand); {DataCompBoy}
  RegisterVar(CSKernel, 'StoreVideoMode',        ikLongint, @StoreVideoMode); {PZ}{DataCompBoy}
  RegisterVar(CSKernel, 'SmartWindowsBoxClose',  ikBoolean, @SmartWindowsBoxClose); {DataCompBoy}
  RegisterVar(CSKernel, 'DoVESATest',            ikBoolean, @DoVESATest); {DataCompBoy}
  {Editor}
  RegisterVar(CSEditor, 'UnlimitUnindent',     ikBoolean, @UnlimitUnindent);
  RegisterVar(CSEditor, 'Koi8rKeyMap',         ikBoolean, @Koi8rKeyMap);
  RegisterVar(CSEditor, 'DrawRShift',          ikBoolean, @DrawRShift);
  RegisterVar(CSEditor, 'AutoScopeDetect',     ikBoolean, @AutoScopeDetect);
  RegisterVar(CSEditor, 'ShowBookmarks',       ikBoolean, @ShowBookmarks);
  RegisterVar(CSEditor, 'FastBookmark',        ikBoolean, @FastBookmark);
  RegisterVar(CSEditor, 'DefCodePage',         ikString,  @DefCodePage);
  RegisterVar(CSEditor, 'CapitalCodePageName', ikBoolean, @CapitalCodePageName);
  RegisterVar(CSEditor, 'FastSearchDeep',      ikLongint, @FastSearchDeep);
  RegisterVar(CSEditor, 'WinManagerPosToEdit', ikBoolean, @WinManagerPosToEdit);
  RegisterVar(CSEditor, 'AutoBracketPairs',    ikString,  @AutoBracketPairs);
  {FilePanels}
  RegisterVar(CSFilePanels, 'ShowFileMask',        ikBoolean, @ShowFileMask);
  RegisterVar(CSFilePanels, 'ShowLongName',        ikBoolean, @ShowLongName);
  RegisterVar(CSFilePanels, 'QuickRenameInDialog', ikBoolean, @QuickRenameInDialog);
  {NetInfo}
  RegisterVar(CSNetInfo, 'NoLevelsInfo', ikBoolean, @NoLevelsInfo);
  {Language}
  RegisterVar(CSLanguage, 'ActiveLanguage',       ikString,  @ActiveLanguage);
  RegisterVar(CSLanguage, 'HelpLanguageOverride', ikString,  @HelpLanguageOverride);
  RegisterVar(CSLanguage, 'ShowLanguageMenu',     ikBoolean, @ShowLanguageMenu);
end;


procedure Loader(const Group, Parameter: PChar; ItemKind: TIniItemKind; PVar: Pointer);
var S: String;
    I: integer;
    L: longint;
begin
 FreeStr:=SourceDir+'DN.INI'#0;
 case ItemKind of
  ikBoolean: if Boolean((PVar^)) then S:='1' else S:='0';
  ikString: S:=String((PVar^));
  ikLongint: Str(LongInt((PVar^)), S);
  ikChar: S:=Char((PVar^));
 end;
 S:=S+#0;
 S[0] := Char(GetPrivateProfileString(Group, Parameter, @S[1], @S[1], 255, @FreeStr[1]));
 case ItemKind of
  ikBoolean: begin
              UpStr(S);
              if (S='1') or (S='TRUE') or (S='YES') then Boolean((PVar^)):=true else
              if (S='0') or (S='False') or (S='NO') then Boolean((PVar^)):=false;
             end;
  ikString: String((PVar^)):=S;
  ikLongint: begin
              Val(S, L, I);
              if I=0 then LongInt((PVar^)):=L;
             end;
  ikChar: begin
           if (S<>'') then Char((PVar^)):=S[1];
          end;
 end;
end;

procedure Saver(const Group, Parameter: PChar; ItemKind: TIniItemKind; PVar: Pointer);
var S: string;
begin
 FreeStr:=SourceDir+'DN.INI'#0;
 case ItemKind of
  ikBoolean: if Boolean((PVar^)) then S:='1' else S:='0';
  ikString: S:=String((PVar^));
  ikLongint: Str(LongInt((PVar^)), S);
  ikChar: S:=Char((PVar^));
 end;
 S:=S+#0;
 WritePrivateProfileString(Group, Parameter, @S[1], @FreeStr[1]);
end;

procedure LoadDnIniSettings; begin Proceed(Loader); end;
procedure SaveDnIniSettings; begin Proceed(Saver);  end;
procedure DoneIniEngine;     begin CloseProfile;    end;

        {-DataCompBoy-}
function ProbeINI(var INItime,INIsize,INIcrc:longint):boolean;
var SR:lSearchRec;
    {F: TDosStream;}
    {B: PByteArray;}
    Pos: Longint;
    S: Word;
begin
    INItime:=0;
    INIsize:=0;
    INIcrc :=0;
    lFindFirst(SourceDir+'DN.INI',AnyFile,SR);
    lFindClose(SR);
    if (DosError=0) and ((SR.SR.Attr and Directory)=0) then begin
        ProbeINI:=True;
        INItime:=SR.SR.Time;
        INIsize:=SR.SR.Size;
        INIcrc :=0;
        {F.Init(SourceDir+'DN.INI', stOpenRead);
        S := Min(INIsize, 32767);
        if S>0 then begin
         GetMem(B, S);
         F.Read(B^, S);
         INIcrc := GetCrc(B^, S);
         FreeMem(B, S);
        end;
        F.Done;}
    end else ProbeINI:=False
end;
        {-DataCompBoy-}

end.
