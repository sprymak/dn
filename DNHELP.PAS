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
//     "Based on TinyWeb Server by RIT Research Labs."
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

unit DNHELP;

interface

const

  hcAboutDialog          = 16553;
  hcAdvancedFilter       = 48029;
  hcAdvancedFloppyFormat = 48042;
  hcAdvanceSearch        = 21002;
  hcAdvPortSetup         = 11013;
  hcAllContainingDialog  = 48000;
  hcAppendFiles          = 31;
  hcAppendPhoneDirectory = 16572;
  hcAppendPhoneNumber    = 16570;
  hcArchiveFiles         = 48031;
  hcArchives             = 11014;
  hcArcSetup             = 11019;
  hcArvidFindResults     = 48076;
  hcAsciiChart           = 16595;
  hcBlockCommands        = 10002;
  hcCalculator           = 11016;
  hcCDplayer             = 16555;
  hcCDplayerOptions      = 16556;
  hcCDplayerTitle        = 16557;
  hcChLngId              = 48077;
  hcClipboard            = 11023;
  hcCmdHistory           = 16586;
  hcColorDialog          = 48045;
  hcColumnsDefaults      = 16566;
  hcColumnsSetup         = 48034;
  hcCombineDialog        = 27;
  hcCommonInfo           = 4;
  hcCommunication        = 11012;
  hcCompareDirs          = 48027;
  hcConfiguration        = 48009;
  hcConfirmationDialog   = 48006;
  hcConfirmations        = 16563;
  hcCopyDialog           = 30;
  hcCopyFiles            = 29;
  hcCountrySetup         = 16564;
  hcCustomVideo          = 48059;
  hcDBSearch             = 16576;
  hcDBView               = 48048;
  hcDeleteFiles          = 25;
  hcDesktop              = 48050;
  hcDialogButton         = 48054;
  hcDialogCase           = 48055;
  hcDialogHistory        = 48053;
  hcDialogInput          = 48052;
  hcDialogMessage        = 48056;
  hcDialogWindow         = 48051;
  hcDirectAccessDenied   = 16594;
  hcDirectDiskAccess     = 16593;
  hcDirHistory           = 48073;
  hcDirTree              = 11007;
  hcDiskEditor           = 11011;
  hcDiskError            = 16579;
  hcDiskInfo             = 16561;
  hcDiskMenu             = 14;
  hcDiskOperations       = 10006;
  hcDriveInfoSetup       = 11004;
  hcDrivesSetup          = 48035;
  hcedEdit               = 48021;
  hcedFile               = 48020;
  hcEditHistory          = 48074;
  hcEditor               = 1102;
  hcEditorCommands       = 10001;
  hcEditorDefaults       = 48040;
  hcEditorFeature        = 8;
  hcEditorFind           = 48071;
  hcEditorHilite         = 48044;
  hcEditorReplace        = 48072;
  hcEditorWindow         = 10000;
  hcEditPhoneDirectory   = 16573;
  hcEditPhoneNumber      = 16571;
  hcedOptions            = 48024;
  hcedParagraph          = 48023;
  hcedSearch             = 48022;
  hcEnvEditor            = 16559;
  hcErrorDialog          = 48004;
  hcExtEditors           = 48033;
  hcExtendedFileMask     = 16588;
  hcExtendedVideo        = 11;
  hcExtFile              = 11005;
  hcExtMacros            = 16552;
  hcExtract              = 48030;
  hcExtViewers           = 48032;
  hcFeaturesOverview     = 5;
  hcFileAttr             = 16583;
  hcFileFind             = 21003;
  hcFileManager          = 20;
  hcFileMask             = 16587;
  hcFileMenu             = 12;
  hcFileMgrSetup         = 48036;
  hcFilePanel            = 1100;
  hcFiles                = 24;
  hcFilesAttr            = 16584;
  hcFilesBBS             = 48061;
  hcFindCell             = 48067;
  hcFloppyFormat         = 48041;
  hcFormat               = 11009;
  hcFormatMargins        = 48070;
  hcGame                 = 21005;
  hcGameSetup            = 16575;
  hcGotoAddress          = 16578;
  hcGotoCellNumber       = 48068;
  hcGotoLine             = 16577;
  hcGUserMenu            = 16547;
  hcHelp                 = 48049;
  hcHelpOnHelp           = 3;
  hcHighlight            = 11022;
  hcHotOverview          = 7;
  hcIndex                = 2;
  hcInformationDialog    = 48005;
  hcInputBox             = 48008;
  hcInsDelCommands       = 10004;
  hcInterface            = 11006;
  hcInterfaceSetup       = 16568;
  hcLinkPanel            = 1101;
  hcLinkStatusWnd        = 48062;
  hcLUserMenu            = 16546;
  hcMainMenu             = 9;
  hcMakeListFile         = 48028;
  hcManagerCommands      = 21;
  hcManualDial           = 48014;
  hcMemoryInfo           = 16591;
  hcMenuBox              = 10;
  hcMenuEdit             = 16550;
  hcMenuExample          = 16549;
  hcMenuFormat           = 16551;
  hcMenuTips             = 16548;
  hcMiscCommands         = 10005;
  hcMkDir                = 48017;
  hcMouseSetup           = 16567;
  hcMovementCommands     = 10003;
  hcNavigatorsWindows    = 19;
  hcNavyLink             = 48063;
  hcNetInfo              = 16560;
  hcNoContext            = 0;
  hcOpenFileDialog       = 48012;
  hcOptionsMenu          = 18;
  hcOptionsStartup       = 48038;
  hcOS2Support           = 48060;
  hcOverwriteQuery       = 48046;
  hcPanelDefaults        = 11002;
  hcPanelHotKeys         = 11003;
  hcPanelMenu            = 16;
  hcPanelSetup           = 11001;
  hcPanelWide            = 11000;
  hcPentix               = 16544;
  hcPhone                = 11020;
  hcPrinterSetup         = 16574;
  hcPrintManager         = 16562;
  hcQueryDialog          = 48007;
  hcQuickDirs            = 48019;
  hcQuickRun             = 17;
  hcReanimator           = 11008;
  hcReenterPassword      = 16565;
  hcRenameMove           = 13;
  hcReplaceCell          = 48066;
  hcSavers               = 48039;
  hcScreenGrabber        = 11024;
  hcScrollBack           = 48043;
  hcSelect               = 16589;
  hcSelectDrive          = 21000;
  hcSelectDriveExtended  = 21001;
  hcSelectFileDialog     = 48013;
  hcSerialInterface      = 48064;
  hcSetCellFormat        = 48069;
  hcSetPassword          = 16585;
  hcSetupArcColumns      = 16581;
  hcSetupDiskColumns     = 16580;
  hcSetupFindColumns     = 16582;
  hcSetupTerminal        = 16569;
  hcSortBy               = 48018;
  hcSplitDialog          = 28;
  hcSplitFiles           = 26;
  hcSpreadSheet          = 11015;
  hcSubMenuConfig        = 48010;
  hcSubMenuFileMgr       = 48011;
  hcSystemInfo           = 16592;
  hcSystemSetup          = 48037;
  hcTeam                 = 16554;
  hcTempList             = 11017;
  hcTerminal             = 11018;
  hcTetris               = 16543;
  hcTetrisScore          = 48015;
  hcTetrisWinner         = 48016;
  hcTreeDialog           = 16558;
  hcUnpackDiskImg        = 48065;
  hcUnselect             = 16590;
  hcUserMenu             = 16545;
  hcUtilMenu             = 15;
  hcUUDecode             = 48026;
  hcUUEncode             = 48025;
  hcView                 = 11021;
  hcViewerFind           = 48047;
  hcViewFile             = 48057;
  hcViewHistory          = 48075;
  hcVolLabel             = 11010;
  hcWarningDialog        = 48003;
  hcWildcards            = 48058;
  hcWindows              = 22;
  hcWindowsFeature       = 6;
  hcWindowsType          = 23;
  hcWinMan               = 21004;
  hcYesNoCancelDialog    = 48002;
  hcYesNoDialog          = 48001;
  hc_                    = 65535;

implementation

end.
