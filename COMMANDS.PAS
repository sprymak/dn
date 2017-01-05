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

UNIT Commands;

INTERFACE

CONST
  kbEsc       = $011B;  kbAltSpace  = $C200;  kbCtrlIns    = $9200;
  kbShiftIns  = $C300;  kbCtrlDel   = $9300;  kbShiftDel   = $C400;
  kbBack      = $0E08;  kbCtrlBack  = $0E7F;  kbShiftTab   = $0F00;
  kbTab       = $0F09;  kbAltQ      = $1000;  kbAltW       = $1100;
  kbAltE      = $1200;  kbAltR      = $1300;  kbAltT       = $1400;
  kbAltY      = $1500;  kbAltU      = $1600;  kbAltI       = $1700;
  kbAltO      = $1800;  kbAltP      = $1900;  kbCtrlEnter  = $1C0A;
  kbEnter     = $1C0D;  kbAltA      = $1E00;  kbAltS       = $1F00;
  kbAltD      = $2000;  kbAltF      = $2100;  kbAltG       = $2200;
  kbAltH      = $2300;  kbAltJ      = $2400;  kbAltK       = $2500;
  kbAltL      = $2600;  kbAltZ      = $2C00;  kbAltX       = $2D00;
  kbAltC      = $2E00;  kbAltV      = $2F00;  kbAltB       = $3000;
  kbAltN      = $3100;  kbAltM      = $3200;  kbF1         = $3B00;
  kbF2        = $3C00;  kbF3        = $3D00;  kbF4         = $3E00;
  kbF5        = $3F00;  kbF6        = $4000;  kbF7         = $4100;
  kbF8        = $4200;  kbF9        = $4300;  kbF10        = $4400;
  kbHome      = $4700;  kbUp        = $4800;  kbPgUp       = $4900;
  kbGrayMinus = $4A2D;  kbLeft      = $4B00;  kbRight      = $4D00;
  kbGrayPlus  = $4E2B;  kbEnd       = $4F00;  kbDown       = $5000;
  kbPgDn      = $5100;  kbIns       = $5200;  kbDel        = $5300;
  kbShiftF1   = $5400;  kbShiftF2   = $5500;  kbShiftF3    = $5600;
  kbShiftF4   = $5700;  kbShiftF5   = $5800;  kbShiftF6    = $5900;
  kbShiftF7   = $5A00;  kbShiftF8   = $5B00;  kbShiftF9    = $5C00;
  kbShiftF10  = $5D00;  kbCtrlF1    = $5E00;  kbCtrlF2     = $5F00;
  kbCtrlF3    = $6000;  kbCtrlF4    = $6100;  kbCtrlF5     = $6200;
  kbCtrlF6    = $6300;  kbCtrlF7    = $6400;  kbCtrlF8     = $6500;
  kbCtrlF9    = $6600;  kbCtrlF10   = $6700;  kbAltF1      = $6800;
  kbAltF2     = $6900;  kbAltF3     = $6A00;  kbAltF4      = $6B00;
  kbAltF5     = $6C00;  kbAltF6     = $6D00;  kbAltF7      = $6E00;
  kbAltF8     = $6F00;  kbAltF9     = $7000;  kbAltF10     = $7100;
  kbCtrlPrtSc = $7200;  kbCtrlLeft  = $7300;  kbCtrlRight  = $7400;
  kbCtrlEnd   = $7500;  kbCtrlPgDn  = $7600;  kbCtrlHome   = $7700;
  kbAlt1      = $7800;  kbAlt2      = $7900;  kbAlt3       = $7A00;
  kbAlt4      = $7B00;  kbAlt5      = $7C00;  kbAlt6       = $7D00;
  kbAlt7      = $7E00;  kbAlt8      = $7F00;  kbAlt9       = $8000;
  kbAlt0      = $8100;  kbAltMinus  = $8200;  kbAltEqual   = $8300;
  kbCtrlPgUp  = $8400;  kbAltBack   = $0E00;  kbNoKey      = $0000;
  kbDoubleAlt = $3800;  kbAltLeft   = $9B00;  kbAltRight   = $9D00;
                        kbAltUp     = $9800;  kbAltDown    = $A000; {-$VIV}
  kbAltTAB    = $A500;  kbCtrlTAB   = $9400;  kbCtrlESC    = $0E00;
  kbAltEnter  = $1C00;  kbDoubleCtrl = $1D00; kbGrayAst    = $372A;
  kbSpace     = $3920;  kbCtrlGPlus  = $9000; kbCtrlGMinus = $8E00;
  kbCtrlGAst  = $9600;  kbAltSlash   = $A400; kbAltIns     = $A200;
  kbShortCut  = $FFFF;  kbAltDel     = $A300; kbCtrlSlash  = $9500;
  kbAltGPlus  = $4E00;  kbAltGMinus  = $4A00; kbAltPgUp    = $9900;
  kbAltPgDn   = $A100;  kbF11        = $8500; kbF12        = $8600;
  kbShiftF11  = $8700;  kbShiftF12   = $8800; kbAltHome    = $9900;
  kbCtrlF11   = $8900;  kbCtrlF12    = $8A00; kbAltEnd     = $9F00;
  kbAltF11    = $8B00;  kbAltF12     = $8C00; kbCtrlBSlash = $2B1C;
  kbCtrlUp    = $8D00;  kbCtrlDown   = $9100; kbCtrlSTab   = kbAltTab;

  kbCtrlA = $1E01;  kbCtrlB = $3002;  kbCtrlC = $2E03;  kbCtrlD = $2004;
  kbCtrlE = $1205;  kbCtrlF = $2106;  kbCtrlG = $2207;  kbCtrlH = $2308;
  kbCtrlI = $1709;  kbCtrlJ = $240A;  kbCtrlK = $250B;  kbCtrlL = $260C;
  kbCtrlM = $320D;  kbCtrlN = $310E;  kbCtrlO = $180F;  kbCtrlP = $1910;
  kbCtrlQ = $1011;  kbCtrlR = $1312;  kbCtrlS = $1F13;  kbCtrlT = $1414;
  kbCtrlU = $1615;  kbCtrlV = $2F16;  kbCtrlW = $1117;  kbCtrlX = $2D18;
  kbCtrlY = $1519;  kbCtrlZ = $2C1A;  kbAltTilde = $2900;

        {-DataCompBoy-}
  kbCtrlAltX = $2D0D; kbCtrlAltZ = $2C0C;
  kbCtrlAltF1 = $F100; kbCtrlAltF2 = $F200; kbCtrlAltF3 = $F300;
  kbCtrlAltF4 = $F400; kbCtrlAltF5 = $F500; kbCtrlAltF6 = $F600;
  kbCtrlAltF7 = $F700; kbCtrlAltF8 = $F800; kbCtrlAltF9 = $F900;
  kbCtrlAltF10= $FA00; kbCtrlAltF11= $FB00; kbCtrlAltF12= $FC00;
  kbCtrlShift1 = $E100; kbCtrlShift2 = $E200; kbCtrlShift3 = $E300;
  kbCtrlShift4 = $E400; kbCtrlShift5 = $E500; kbCtrlShift6 = $E600;
  kbCtrlShift7 = $E700; kbCtrlShift8 = $E800; kbCtrlShift9 = $E900;
  kbCtrl1=$D100; kbCtrl2=$D200; kbCtrl3=$D300; kbCtrl4=$D400; kbCtrl5=$D500;
  kbCtrl6=$D600; kbCtrl7=$D700; kbCtrl8=$D800; kbCtrl9=$D900; kbCtrl0=$DA00;
  kbCtrlShiftTilde = $0FE00;    kbBackUp=$FD00; kbDownUp=$D000; kbUpUp=$C800;
  kbAltCtrlSqBracketL=$1A1A;    kbAltCtrlSqBracketR=$1B1B;
  kbCtrlSqBracketL=$1A1B;       kbCtrlSqBracketR=$1B1D;
  kbAltMore=$3400;              kbAltLess=$3300;
  kbAltShiftBack=$9700;
  kbShiftGrayPlus=$CE00;        kbShiftGrayMinus=$CA00;
  kbAltShiftIns=$A20A; {for VP-version}

        {-DataCompBoy-}

  kbAltQuote = $2800;
  kbAltSemicolon = $2700; {JO}

const

{ Event codes }

  evMouseDown = $0001;
  evMouseUp   = $0002;
  evMouseMove = $0004;
  evMouseAuto = $0008;
  evKeyDown   = $0010;
  evCommand   = $0100;
  evBroadcast = $0200;

{ Event masks }

  evNothing   = $0000;
  evMouse     = $000F;
  evKeyboard  = $0010;
  evMessage   = $FF00;


const
  kbRightShift  = $0001;
  kbLeftShift   = $0002;
  kbCtrlShift   = $0004;
  kbAltShift    = $0008;
  kbScrollState = $0010;
  kbNumState    = $0020;
  kbCapsState   = $0040;
  kbInsState    = $0080;

  kbAnyShift    = $000F;

{ ViewMode constants }
  vmText      =   0;
  vmHex       =   1;
  vmDump      =   2;
  vmAsm       =   3;
  vmInternal  =  16;
  vmExternal  =  32;
  vmDB        = 100;
  vmSpread    = 101;
  vmPKT       = 102;
  vmMSG       = 103;

  ebfCBF = 1 shl  0;
  ebfBSU = 1 shl  1;
  ebfABr = 1 shl  2;
  ebfAId = 1 shl  3;
  ebfAwr = 1 shl  4;
  ebfJwr = 1 shl  5;
  ebfVBl = 1 shl  6;
  ebfOfl = 1 shl  7;
  ebfHLn = 1 shl  8;
  ebfHCl = 1 shl  9;
  ebfPBl = 1 shl 10;
  ebfObl = 1 shl 11;
  ebfLck = 1 shl 12;
  ebfEMS = 1 shl 13;
  ebfXMS = 1 shl 14;
  ebfTRp = 1 shl 15; {-$VOL} {TabReplace}
  ebfHlt = 1 shl  0; {Highlight}
  ebfSmt = 1 shl  1; {SmartTab}

  vbfHex               = 1 shl 0;
  vbfWrap              = 1 shl 1;
  vbfWordWrap          = 1 shl 2;
  vbfHlt               = 1 shl 3;
  vbfScrollAfterEOF    = 1 shl 4;
  vbfAutoscroll        = 1 shl 5;
  vbfDisAsm            = 1 shl 6;

  ouiClock               = $0001;
  ouiHideMenu            = $0002;
  ouiHideStatus          = $0004;
  ouiEsc                 = $0008;
  ouiHideCmdline         = $0010;
  ouiAutoCmdLine         = $0020;
  ouiBlockInsertCursor   = $0040;
  ouiStoreEditorPosition = $0080;
  ouiStoreViewerPosition = $0100;
  ouiTrackEditors        = $0200;
  ouiTrackViewers        = $0400;
  ouiTrackDirs           = $0800;
  ouiHideDriveInfo       = $1000; {-$VIV}
  ouiShowScrollBar       = $2000;

  omsReverse = $01;
  omsCursor  = $02;

  osuAutoMenu       = $01;
  osuRestoreScrMode = $02;
  osuKillHistory    = $04;
{ osuOverlayXMS     = $08;}
{ osuOverlayEMS     = $10;}
  osuResetPalette   = $08;

  osuInactivityExit = $01;
  osuAutosave       = $02;
  osuBlinking       = $04;
  osuPreserveDir    = $08;
  osuRestorePal     = $10;

  osuInt28          = $01;
  osuSleep          = $02;
  osuInt2F          = $04;
  osuInt15          = $08;

  osuOnceRelease    = $01;
  osuReleaseOpen    = $02;
  osuReleaseCopy    = $04;
  osuReleaseArch    = $08;
  osuReleaseFind    = $10;
 {osuReleaseDrag    = $20;}

  ossEditor      =  $01;
  ossUseSysClip  =  $02;
  ossShowHidden  =  $04;
  ossFastExec    =  $08;
  ossDisableXMS  =  $10;
  ossCheckDDA    =  $20;
  ossRemoveCD_RO =  $40;
  ossAdvCopy     =  $80;
  ossFlushDsk    = $100;
  ossHandleChDirCommand = $200;

  ossDirectAccess   =$01;
  ossTree           =$02;
  ossVerify         =$04;
  ossCheckFreeSpace =$08; {DataCompBoy}
  ossNoLFN          =$10; {DataCompBoy}

  cdnHideCmdLine    = $0001;  { DN Misc Information }

        {-DataCompBoy-}
  ditMediaType      = $01;
  ditLocalDir       = $02;
  ditCDdir          = $04;
  ditNetDir         = $08;
  ditIncludeQick    = $10;
  ditIncludeTempInfo= $20;
  ditShowVolume     = $40;
  ditSkipFloppy     = $80;

  dfMDY             = 00;
  dfDMY             = 01;
  dfYMD             = 02;
        {-DataCompBoy-}

{ Editor History }

  hsFindMask        = 109;
  hsFindText        = 110;
  hsSelectBox       = 111;
  hsFileCopyName    = 112;
  hsLongCopy        = 113;
  hsEditOpen        = 114;
  hsEditSave        = 115;
  hsEditPasteFrom   = 116;
  hsFileMask        = 117;
  hsFindWhat        = 118;
  hsViewFind        = 110;

  hsSaveSheetAs     = 120;
  hsLoadSheet       = 121;
  hsGotoCell        = 122;
  hsFindCellPattern = 123;
  hsReplacePattern  = 124;
  hsColors          = 125;
  hsExtract         = 126;
  hsFBBCopy         = 127;
  hsCustoms         = 128;
  hsdbSearch        = 129;
  hsImportPhones    = 130;
  hsMakeDir         = 131;
  hsInputParams     = 132;
  hsPrintOut        = 133;
  hsTetris          = 134;
  hsManualPhone     = 135;
  hsCalcLine        = 136;
  hsArcFiles        = 137;
  hsEditDesc        = 138;
  hsFindAfter       = 139;
  hsFindBefore      = 140;
  hsNewVariable     = 141;
  hsExecDOSCmd      = 142;
  hsMakeList        = 143;
  hsEditHistory     = 144;
  hsOpenXLT         = 145;
  hsEditDBF         = 146;
  hsUUEncode        = 147;
  hsUUDecode        = 148;
  hsUULines         = 149;
  hsMakeListHeader  = 150;
  hsMakeListFooter  = 151;
  hsFmtVolumeL      = 152;
  hsAddOptions      = 153;
  hsRenameFil       = 154;
  hsFBBCopySize     = 155;
  hsFBBCopyNum      = 156;
  hsEditEALongname  = 157;
  hsCreateWPSObject = 158;

{UU Encode options}

  ckNone          = 0;
  ckEntire        = 1;
  ckStd           = 2;
  ckEach          = 3;
  ck64            = 4;

  ckFileTime      = 1;
  ckMapTable      = 2;
  ckStatistic     = 4;

{ QuickSearch key }
  pqsAlt  = 0;
  pqsCtrl = 1;
  pqsCaps = 2;

{ Default interior for panels, may differs in instances }
  fmiDirLen          = $0001;
  fmiCurrent         = $0002;
  fmiSelected        = $0004;
  fmiTotals          = $0008;
  fmiFree            = $0010;
  fmiHiliteFiles     = $0020;
  fmiExeFirst        = $0040;
  fmiArchivesFirst   = $0080;
  fmiExeBeforeArc    = $0100;

{ Common panel interior, same for all instances }
  fmsColumnTitles    = $0001;
  fmsDriveLine       = $0002;
  fmsDivider         = $0004;
  fmsTagChar         = $0008;

{ Options for panels }
  fmoAutoChangeDir   = $0001;
  fmoDragAndDrop     = $0002;
  fmoBeep            = $0004;
  fmoEnterArchives   = $0008;
  fmoSpaceToggle     = $0010;
  fmoDelErase        = $0020;
  fmoUseArrows       = $0040;
  fmoAltDifference   = $0080;
  fmoCtrlDifference  = $0100;
  fmoAlternCtrlDiff  = $0200;
  fmoBackGoesBack    = $0400;
  fmoPreserveDesc    = $0800;
  fmoKillContainer   = $1000;
  fmoAlwaysCopyDesc  = $2000;

{ New manager panel types }
  fdoDriveDrive      = 0;
  fdoInfoDrive       = 1;
  fdoTreeFrive       = 2;
  fdoRightOnly       = 3;

{ DriveInfo data items }
  fdiTitle           = $0001;
  fdiTotals          = $0002;
  fdiVolumeSize      = $0004;
  fdiVolumeFree      = $0008;
  fdiVolumeLabel     = $0010;
  fdiLowMemory       = $0020;
  fdiAvailLowMemory  = $0040;
  fdiMemAvail        = $0080;
  fdiEMSFree         = $0100;
  fdiXMSFree         = $0200;
  fdiDIZ             = $0400;
  fdiSerialNo        = $0800; { Rainbow }
  fdiFileSys         = $1000; { Rainbow }

{ Confirmations }
  cfSingleErase      = $0001;
  cfMultiErase       = $0002;
  cfEraseSubdir      = $0004;
  cfEraseReadonly    = $0008;
  cfCreateSubdir     = $0010;
  cfExitCDconfirm    = $0020;
  cfMouseConfirm     = $0040;
  cfExitConfirm      = $0080;
  cfFmtOs2Warning    = $0100;
  cfChaneLngWarning  = $0100;

{ Column data items }
  psShowLongName = $0001;
  psShowSize     = $0002;
  psShowDate     = $0004;
  psShowTime     = $0008;
  psShowPacked   = $0010;
  psShowRatio    = $0020;
  psShowCrDate   = $0010;
  psShowCrTime   = $0020;
  psShowLADate   = $0040;
  psShowLATime   = $0080;
  psShowDir      = $0100;
  psShowDescript = $0100;
  psShowLongDesc = $0200;


{ Terminal emulation }
  emTTY     = 0;
  emANSIBBS = 1;
  emANSI    = 2;
  emVT52    = 3;
  emVT102   = 4;

{ Terminal Options }
  toENTERLF      = $0001;
  toCTRLCLS      = $0002;
  toLocalEcho    = $0004;
  toAutoZModem   = $0008;
  toInsertMode   = $0010;
  toCheckDCD     = $0020;
  toConnectSnd   = $0040;
  toCaptureCtrls = $0080;

  ttDirectory    = 1;
  ttExec         = 2;
  ttArc          = 3;
  ttCust1        = 4;
  ttCust2        = 5;
  ttCust3        = 6;
  ttCust4        = 7;
  ttCust5        = 8;
  ttCust6        = 9;  {JO}
  ttCust7        = 10;
  ttCust8        = 11;
  ttCust9        = 12;
  ttCust10       = 13; {JO}

{Phones}
  cmHold    = 11304;
  cmUnhold  = 11005;

{ Standard command codes }

  cmValid   = 0;
  cmQuit    = 1;
  cmError   = 2;
  cmMenu    = 3;
  cmClose   = 4;
  cmZoom    = 5;
  cmMaxi    = 45;
  cmMinimize= 46;
  cmResize  = 6;
  cmNext    = 7;
  cmPrev    = 8;
  cmHelp    = 9;
  cmHelp2   = $DCB;

{ Application command codes }

  cmCut     = 20;
  cmCopy    = 21;
  cmPaste   = 22;
  cmUndo    = 23;
  cmRedo    = 30; {-$VOL}
  cmClear   = 24;
  cmTile    = 25;
  cmCascade = 26;

{ TDialog standard commands }

  cmOK      = 10;
  cmCancel  = 11;
  cmYes     = 12;
  cmNo      = 13;
  cmDefault = 14;
  cmSkip    = 15;
  cmTest    = 16;

{ Dialog broadcast commands }

  cmRecordHistory = 60;

{ Standard messages }

  cmFind              = 47;
  cmSelectAll         = 48; {+CN}
  cmIncSearch         = 49;

  cmReceivedFocus     = 50;
  cmReleasedFocus     = 51;
  cmCommandSetChanged = 52;

{ TScrollBar messages }

  cmScrollBarChanged  = 53;
  cmScrollBarClicked  = 54;

{ TWindow select messages }

  cmSelectWindowNum   = 55;

{ TListViewer messages }

  cmListItemSelected  = 56;

  cmSaveText           = 12101;
  cmLoadText           = 12102;
  cmSaveTextAs         = 12103;
  cmSwitchBlock        = 12104;
  cmSwitchIndent       = 12105;
  cmSwitchFill         = 12106;
  cmSwitchBack         = 12107;
  cmSwitchSave         = 12108;

  cmWordLeft           = 12110;
  cmWordRight          = 12111;
  cmDelWordRight       = 12112;
  cmDelWordLeft        = 12113;
  cmScrollUp           = 12114;
  cmScrollDn           = 12115;
  cmCenter             = 12116;
  cmStartSearch        = 12117;
  cmContSearch         = 12118;
  cmDelBackChar        = 12119;
  cmDelChar            = 12120;
  cmSwitchIns          = 12121;
  cmPgUp               = 12122;
  cmPgDn               = 12123;
  cmMoveUp             = 12124;
  cmMoveLeft           = 12125;
  cmMoveRight          = 12126;
  cmMoveDown           = 12127;
  cmBlockStart         = 12128;
  cmBlockEnd           = 12129;
  cmMarkWord           = 12130;
  cmMarkLine           = 12131;
  cmTab                = 12135;
  cmSpecChar           = 12138;
  cmReplace            = 12139;
  cmEnd                = 12140;
  cmEnter              = 12141;
  cmInsLine            = 12142;
  cmLJustify           = 12147;
  cmLCenter            = 12148;
  cmLLeft              = 12149;
  cmLRight             = 12150;
  cmDeltoEOLN          = 12151;
  cmSetMargins         = 12152;
  cmCtrlHome           = 12153;
  cmCtrlEnd            = 12154;
  cmSwitchHighLight    = 12155;
  cmGotoLineNumber     = 12156;
  cmSwitchWrap         = 12157;
  cmSwitchHiLine       = 12158;
  cmSwitchHiColumn     = 12159;
  cmUpString           = 12160;
  cmLowString          = 12161;
  cmCapString          = 12162;
  cmInsertTime         = 12166;
  cmInsertDate         = 12167;
  cmMoveBlockStart     = 12168;
  cmMoveBlockEnd       = 12169;
  cmInsertOn           = 12170;
  cmInsertOff          = 12171;
  cmIndentOn           = 12172;
  cmIndentOff          = 12173;
  cmSelectMacro        = 12174;
  cmPlayMacro          = 12175;
  cmSwitchBrackets     = 12176;
  cmBracketPair        = 12177;
  cmUpWord             = 12178;
  cmLowWord            = 12179;
  cmCapWord            = 12180;
  cmSwitchDrawMode     = 12181;
  cmLoadXlatTable      = 12182;
  cmAdvancePortSetup   = 12183;
  cmNavyLinkSetup      = 12184;
  cmSwitchKeyMapping   = 12185;{-$VIV}
  cmOpenFileAtCursor   = 12186;{-$VIV}
  cmEditSPF            = 12187;{-$VIV}
  cmEditINI            = 12188;{-$VIV}
  cmChangeCharCase     = 12189;
  cmSwitchTabReplace   = 12190;{-$VOL}
  cmEditCrLfMode       = 12191;
  cmEditLfMode         = 12192;
  cmEditCrMode         = 12193;
  cmNext2              = 12194;
  cmPrev2              = 12195;
  cmNext3              = 12196;
  cmPrev3              = 12197;
  cmGotoLineNumber2    = 12198;
  cmSwitchSmartTab     = 12199;
  cmPlaceMarker1       = 14001;
  cmPlaceMarker2       = 14002;
  cmPlaceMarker3       = 14003;
  cmPlaceMarker4       = 14004;
  cmPlaceMarker5       = 14005;
  cmPlaceMarker6       = 14006;
  cmPlaceMarker7       = 14007;
  cmPlaceMarker8       = 14008;
  cmPlaceMarker9       = 14009;
  cmGoToMarker1        = 14011;
  cmGoToMarker2        = 14012;
  cmGoToMarker3        = 14013;
  cmGoToMarker4        = 14014;
  cmGoToMarker5        = 14015;
  cmGoToMarker6        = 14016;
  cmGoToMarker7        = 14017;
  cmGoToMarker8        = 14018;
  cmGoToMarker9        = 14019;
  cmBackSearch         = 14020;

  cmCopyUnselect       = 10001;
  cmRereadDir          = 10004;
  cmRereadInfo         = 10005;
  cmPushName           = 10006;
  cmRereadTree         = 10007;
  cmTree               = 10009;
  cmFileView           = 10011;
  cmFileEdit           = 10013;
  cmInsertBlock        = 10014;
  cmEnableView         = 10016;
  cmDisableView        = 10017;
  cmDisableSky         = 10018;
  cmChangeDrv          = 10019;
  cmInsertDrive        = 10021;
  cmMenuOn             = 10022;
  cmMenuOff            = 10023;
  cmExecGrabber        = 10024;
  cmCloseDialer        = 10025;
  cmFileTextView       = 10026; {AK155}

  cmFindEdit           = 10101;
  cmFindView           = 10102;
  cmFindPrint          = 10103;
  cmFindSortBy         = 10104;
  cmFindMakeList       = 10105;
  cmFindGotoFile       = 10106;

  cmPanelXSelect       = 10203;
  cmPanelXUnselect     = 10204;
  cmPanelCompare       = 10211;
  cmPushFirstName      = 10214;
  cmPushFullName       = 10215;

  cmInsertName         = 10301;
  cmInfoPresent        = 10302;
  cmGetDirName         = 10303;
  cmLocalReread        = 10304;
  cmDlgNotFound        = 10305;

  cmPrepareToDelete    = 10401;
  cmChangeDirectory    = 10402;
  cmChangeTree         = 10403;
  cmLoadViewFile       = 10404;
  cmGLoadViewFile      = 10405;
  cmLViewFile          = 10406;
  cmIntFileView        = 10410;
  cmIntFileEdit        = 10411;

  cmExecCommandLine    = 10500;
  cmExecFile           = 10501;
  cmDropped            = 10502;
  cmFindCalculator     = 10503;
  cmFindTree           = 10504;
  cmFindTempDrive      = 10505;
  cmTempDrive          = 10506;
  cmCopyToTemp         = 10508;
  cmClearCommandLine   = 10509;

  cmSystemSetup        = 10600;
  cmChangeColors       = 10601;
  cmExtFileEdit        = 10602;
  cmMenuFileEdit       = 10603;
  cmExternalViewers    = 10604;
  cmExternalEditors    = 10605;
  cmScreenRest         = 10606;
  cmHexMode            = 10607;
  cmSearchFor          = 10608;
  cmContinueSearch     = 10609;
  cmStartup            = 10610;
  cmInterfaceSetup     = 10611;
  cmSaversSetup        = 10612;
  cmChScreenMode       = 10613;

  cmSSortName          = 10801;
  cmSSortExt           = 10802;
  cmSSortSize          = 10803;
  cmSSortTime          = 10804;
  cmSSortUnSort        = 10800;
  cmShowFields         = 10805;
  cmShowMemo           = 10806;
  cmEditDBField        = 10807;

  cmNewTable           = 10901;
  cmChangeWidth        = 10904;
  cmSheetLoad          = 10905;
  cmChangeFormat       = 10906;
  cmInsertLine         = 10907;
  cmDeleteLine         = 10908;
  cmInsertColumn       = 10909;
  cmDeleteColumn       = 10910;
  cmRecalc             = 10911;
  cmFindCell           = 10912;
  cmReplaceCell        = 10913;
  cmSearchAgain        = 10914;
  cmGotoError          = 10916;
  cmImportToFile       = 10917;
  cmImportToClip       = 10918;
  cmSaveSheetAs        = 10919;
  cmToggleDescriptions = 10920;
  cmToggleShowMode     = 10921;
  cmToggleLongNames    = 10922;
  cmOpenWPSWindow      = 10923;
  cmCreateWPSObject    = 10924;

  cmExtract            = 11000;
  cmSetPassword        = 11001;
  cmArcDelete          = 11002;
  cmArcView            = 11003;
  cmArcTest            = 11004;
  cmTermWrite          = 11005;
  cmTermLog            = 11006;
  cmTermVisible        = 11007;
  cmExtractTo          = 11008;
  cmFullView           = 11009;
  cmArchiveFiles       = 11010;
  cmExecString         = 11011;
  cmReceiveFile        = 11012;
  cmSendFile           = 11013;
  cmGetFileName        = 11014;
  cmShowOutput         = 11015;
  cmUnArchive          = 11016;
  cmCheckArchive       = 11017;
  cmInsertFile         = 11018;
  cmMarkFiles          = 11019;
  cmUnMarkFiles        = 11020;
  cmMarkFile           = 11021;
  cmRChDrive           = 11022;
  cmRViewFile          = 11023;
  cmReanimateFile      = 11024;
  cmNewGame            = 11025;
  cmEraseGroup         = 11026;
  cmPutInClipboard     = 11027;
  cmGetFromClipboard   = 11028;
  cmReboundPanel       = 11029;
  cmCopyCollection     = 11030;
  cmSendInitModem      = 11031;
  cmSendModemBreak     = 11032;
  cmReadArchive        = 11033;
  cmRereadForced       = 11034;
  cmMakeForced         = 11035;
  cmFindForced         = 11036;
  cmTouchFile          = 11037;
  cmDoSendLocated      = 11038;
  cmGetUserParams      = 11039;
  cmTreeChanged        = 11040;
  cmCloseFormat        = 11041;
  cmCloseLinked        = 11042;
  cmGetUserParamsWL    = 11043;

  cmQuickChange1       = 11100;
  cmQuickChange2       = 11101;
  cmQuickChange3       = 11102;
  cmQuickChange4       = 11103;
  cmQuickChange5       = 11104;
  cmQuickChange6       = 11105;
  cmQuickChange7       = 11106;
  cmQuickChange8       = 11107;
  cmQuickChange9       = 11108;

  cmUpdateHexViews     = 11111;

  {  Additional Commands   }
  cmAddVariable        = 11205;
  cmRenVariable        = 11206;
  cmDelVariable        = 11207;

  cmSetup              = 19001;
  cmStop               = 19002;
  cmShowHi             = 19004;
  cmSendTerminalString = 19005;
  cmPostHideLeft       = 19006;
  cmPostHideRight      = 19007;
  cmDuplicateLine      = 19008;
  { Special Commands }
  cmChangeInactive     = 19010;
  cmMainMenu           = 19011;
  cmAddFilter          = 19012;
  cmGetDirInfo         = 19014;
  cmSinglePrint        = 19015;
{ cmSingleAttr         = 19016;} {JO}
  cmSingleTag          = 19017;
  cmSingleUntag        = 19018;
  cmGetCurrentPosFiles = 19019;
  cmSyncClipOut        = 19020;
  cmSyncClipIn         = 19021;
  cmWindowsPaste       = 19022;
  cmWindowsCopy        = 19023;
  cmReleaseFile        = 19024;
  cmFastRename         = 19025;
  cmGetNetInfo         = 19026; {-$VIV}
  cmViewFilter         = 19027;

  cmMemInfo            =   100;



  { Terminal/ScrollBack }
  cmScrollBack         = 19114;
  cmSBBKill            = 19115;
  cmSBBStore           = 19116;
  cmSBBGrab            = 19117;

  cdPlay              = 19900 ;
  cdStop              = 19901 ;
  cdNext              = 19902 ;
  cdPrev              = 19903 ;
  cdEject             = 19904 ;
  cdPause             = 19905 ;
  cdFFwd              = 19906 ;
  cdFRew              = 19907 ;
  cdSetup             = 19908 ;
  cdDisplay           = 19909 ;
  cdZoom              = 19910 ;
  cdMode              = 19911 ;
  cdOrder             = 19912 ;
  cdTitle             = 19913 ;
  cdMix               = 19914 ;
  cdSong              = 19915 ;

  cmTextMode          = 1002;
  cmDirMode           = 1003;
  cmFATMode           = 1004;
  cmFixFile           = 1005;
  cmFixDir            = 1006;
  cmFixDrive          = 1007;
  cmFixCluster        = 1008;
  cmFixSector         = 1009;
  cmFixBlockWrite     = 240;

  cmOpenFile          = 3001;
  cmOpenWindow        = 3002;
  cmChangeMode        = 3003;
  cmFindFile          = 3004;
  cmCreatePanel       = 3006;
  cmCreateTree        = 3007;
  cmAbout             = 3008;
  cmRefresh           = 3009;
  cmXViewFile         = 3011;
  cmShowUserScreen    = 3012;
  cmFormatDisk        = 3013;
  cmXEditFile         = 3015;
  cmMemoryInfo        = 3016;
  cmLoadDesk          = 5434;
  cmSaveDesk          = 5435;
  cmRetrieveSwp       = 5436; {JO}
 {cmDiskCopy          = 3017;}
  cmSystemDisk        = 3018;
  cmReanimator        = 3019;
  cmDiskEdit          = 3020;
  cmSetVolumeLabel    = 3021;
  cmCalculator        = 3022;
  cmHideShowTools     = 3023;
  cmChangeUserMode1   = 3024;
  cmChangeUserMode2   = 3025;
  cmUserMenu          = 3026;
  cmLocalMenuFileEdit = 3027;
  cmGetName           = 3028;
  cmPhoneBook         = 3030;
  cmStoreColors       = 3031;
  cmLoadColors        = 3032;
  cmHangUp            = 3033;
  cmSetupModem        = 3034;
  cmTerminal          = 3035;
  cmUndial            = 3036;
  cmTotalReread       = 3038;
  cmHideWindows       = 3040;
  cmHistoryList       = 3041;
  cmGame              = 3043;
  cmKillUsed          = 3044;
  cmFMDefaults        = 3045;
  cmEditorDefaults    = 3046;
  cmSetupArchive      = 3047;
  cmUpdateConfig      = 3048;
  cmPrintFile         = 3049;
  cmPrintFileEd       = 30491; {JO} {temporarily}
  cmHighlightGroups   = 3050;
  cmSetupMouse        = 3054;
  cmSetupPrinter      = 3055;
  cmFilePrint         = 3056;
  cmNavyLink          = 3057;
  cmColumnDefaultsDisk= 3058; {DataCompBoy}
  cmEditQuickRun      = 3060;
  cmRestart           = 3061; {DataCompBoy}
  cmColumnDefaultsFind= 3062; {DataCompBoy}
  cmColumnDefaultsTemp= 3063; {DataCompBoy}
  cmColumnDefaultsArch= 3064; {DataCompBoy}
  cmColumnDefaultsArvd= 3065; {DataCompBoy}
  cmASCIITable        = 3066;
  cmSystemInfo        = 3067;

  cmLoConfigArchiver  = 3101;

  cmConfigACE         = 3101;
  cmConfigARJ         = 3104;
  cmConfigHA          = 3109;
  cmConfigLHA         = 3114;
  cmConfigRAR         = 3117;
  cmConfigZIP         = 3123;
  cmConfigAIN         = 3102;
  cmConfigARC         = 3103;
  cmConfigBS2         = 3105;
  cmConfigBSA         = 3106;
  cmConfigCAB         = 3107;
  cmConfigCHZ         = 3108;
  cmConfigHAP         = 3110;
  cmConfigHPK         = 3111;
  cmConfigHYP         = 3112;
  cmConfigIS3         = 3113;
  cmConfigLIM         = 3115;
  cmConfigQUARK       = 3116;
  cmConfigSQZ         = 3118;
  cmConfigTAR         = 3119;
  cmConfigTGZ         = 3120;
  cmConfigUC2         = 3121;
  cmConfigUFA         = 3122;
  cmConfigZOO         = 3124;
  cmConfigZXZ         = 3125;
  cmHiConfigArchiver  = 3125;
  cmUpdateArcFile     = 3200;

  cmForceRescan       = 3217;
  cmGlobalUserMenu    = 3220;
  cmEditMenu          = 3221;
  cmClearData         = 3222;
  cmSearchAdvance     = 3223;
  cmCountrySetup      = 3224;
  cmSetupConfirmation = 3225;
  cmTerminalDefaults  = 3226;
  cmGetTeam           = 3227;
  cmOpenSmartpad      = 3236;
  cmTetrisPreview     = 3237;
  cmTetrisIncLevel    = 3238;
  cmPlayCD            = 3239;
  cmEnvEdit           = 3240;
  cmEditHGL           = 3241;
{ cmSingleCopy        = 3242;} {JO}
{ cmSingleRename      = 3243;} {JO}
  cmExecuteDOScmd     = 3244;
  cmFMSetup           = 3246;
  cmDriveInfoSetup    = 3247;
  cmHideCmdLine       = 3248;
  cmRecountDirs       = 3249;
  cmChLngId           = 3250;
  cmIsRightPanel      = 3251;

  cmLookForPanels     = 3550; { Are there any panels? }
  cmFirstTimePanel    = 3551; { Open a panel if none exist }
  cmReverseSearch     = 3552;
  cmEditHistory       = 3553;
  cmViewHistory       = 3554;

  cmOpenClipBoard     = 3555; {-$VOL}

  cmShowTimeInfo      = 3556; {DataCompBoy}

  cmTextView          = 22189;
  cmHexView           = 22190;
  cmDBFView           = 22191;
  cmWKZView           = 22192;

  cmSetValue          = 10000;
  cmSetError          = 10001;
  cmCalcValue         = 10002;
  cmCopyClip          = 10003;

  cmGetPtr1           = 23000;
  cmGetPtr2           = 23001;

  { R - Commands      }

  cmRFormat           = 24000;
  cmNewStrColl        = 24001;
  cmGetCmpNfo         = 24418;
  cmRSearchAdvance    = 24123;

  cmCalendar          = 27000;

{ Open/Save commands }

  cmOpen              = 27;
  cmSave              = 28;
  cmSaveAll           = 29;


{ Editor Commands }

  cmBlockRead         = 120;
  cmBlockWrite        = 121;
  cmBlockPrint        = 122;

  cmFJustify          = 123;
  cmFCenter           = 124;
  cmFLeft             = 125;
  cmFRight            = 126;

  cmSortBlock         = 127;
  cmCalcBlock         = 128;
  cmCopyBlock         = 129;
  cmMoveBlock         = 130;
  cmHideBlock         = 131;
  cmUpcaseBlock       = 132;
  cmLowcaseBlock      = 133;
  cmCapitalizeBlock   = 134;
  cmIndentBlock       = 135;
  cmUnIndentBlock     = 136;
  cmRusEngConv        = 137; {-$VIV 20.05.99--}
  cmChangeCaseBlock   = 138;
  cmRevSortBlock      = 139;

{ Panel Commands }

  PanelCommands = [150..199];

  cmViewFile          = 150;
  cmEditFile          = 151;
  cmIntViewFile       = 153;
  cmIntEditFile       = 154;
  cmCopyFiles         = 155;
  cmTempCopyFiles     = 156;
  cmPanelLongCopy     = 157;
  cmMoveFiles         = 158;
  cmPanelMkDir        = 159;
  cmPanelErase        = 160;
  cmMakeList          = 161;
  cmCompareDir        = 162;
  cmSetFAttr          = 163;
  cmPanelSetup        = 164;
  cmSortBy            = 165;
  cmPanelSelect       = 166;
  cmPanelUnselect     = 167;
  cmPanelInvertSel    = 168;
  cmPanelReread       = 169;
  cmChangeDrive       = 170;
  cmChangeDir         = 171;
  cmSetupColumns      = 172;
  cmPanelPrint        = 173;
  cmPanelArcFiles     = 174;
  cmExtractArchive    = 175;
  cmPanelMakeList     = 176;
  cmListOfDirs        = 177;
  cmCountLen          = 178;
  cmUUEncodeFile      = 179;
  cmUUDecodeFile      = 180;
  cmDirHistory        = 181;
  cmUnpDiskImg        = 182;
  cmSingleDel         = 183;
  cmAdvFilter         = 184;
  cmSingleAttr        = 185; {JO}
  cmDirBranch         = 186;
  cmChangeNameCase    = 187;
  cmSetEALongname     = 188;

{ ??? Commands }

  cmViewText          = 189;
  cmViewHex           = 190;
  cmViewDBF           = 191;
  cmViewWKZ           = 192;
  cmPrintBlock        = 193;
  cmDialPhone         = 194;
  cmWindowManager     = 195;
  cmClearDeskTop      = 196;
  cmUnWrap            = 197;
  cmSingleCopy        = 198; {JO}
  cmSingleRename      = 199; {JO}
  cmGotoCell          = 508;


{$IFDEF OS_DOS}
 {$I SORT_DOS.INC}
{$ENDIF}
{$IFDEF Win32}
 {$I SORT_W32.INC}
{$ENDIF}
{$IFDEF OS2}
 {$I SORT_OS2.INC}
{$ENDIF}

{ DblWnd Commands }

  DblWndCommands = [cmMaxi,200..219];

  cmDirTree           = 200;
  cmQuickView         = 201;
  cmSwapPanels        = 202;
  cmHideLeft          = 203;
  cmHideRight         = 204;
  cmChangeLeft        = 205;
  cmChangeRight       = 206;
  cmDiskInfo          = 207;
  cmSwitchOther       = 208;
  cmNetInfo           = 209; {-$VIV}


{ Phonebook Commands }

  PhonesCommands      = [221..228];
  cmPhoneBookMode     = 221;
  cmImportPhones      = 222;
  cmSearchPhone       = 223;
  cmHideInactive      = 224;
  cmCopyPhone         = 225;
  cmInsertPhone       = 226;
  cmDeletePhone       = 227;
  cmEditPhone         = 228;

  cmPalVGA            = 229;


  cmSelectMenuCommands= 30000;
  MoreCommand         = 30998;
  NoneCommand         = 30999;
  cmSwitch            = 31000;

{ Help Contexts }

  hcNew          = $FF01;
  hcOpen         = $FF02;
  hcSave         = $FF03;
  hcSaveAs       = $FF04;
  hcSaveAll      = $FF05;
  hcChangeDir    = $FF06;
  hcDosShell     = $FF07;
  hcExit         = $FF08;

  hcUndo         = $FF10;
  hcCut          = $FF11;
  hcCopy         = $FF12;
  hcPaste        = $FF13;
  hcClear        = $FF14;

  hcTile         = $FF20;
  hcCascade      = $FF21;
  hcCloseAll     = $FF22;
  hcResize       = $FF23;
  hcZoom         = $FF24;
  hcNext         = $FF25;
  hcPrev         = $FF26;
  hcClose        = $FF27;

  ofSelectable  = $0001;
  ofTopSelect   = $0002;
  ofFirstClick  = $0004;
  ofFramed      = $0008;
  ofPreProcess  = $0010;
  ofPostProcess = $0020;
  ofBuffered    = $0040;
  ofTileable    = $0080;
  ofCenterX     = $0100;
  ofCenterY     = $0200;
  ofCentered    = $0300;
  ofValidate    = $0400;
  ofSecurity    = $0800;
  ofVersion     = $3000;
  ofVersion10   = $0000;
  ofVersion20   = $1000;

  {Wrap constants} {DataCompBoy}
  wmNone        = 0;
  wmWidth       = 1;
  wmWords       = 2;

Type
  TCRLF = (cfNone, cfCRLF, cfCR, cfLF);


TYPE
  TStrIdx = (
    dlAltTable,
    erInvalidFormula,
    erInvalidCell,
    erInvalidValue,
    erInvalidFunction,
    erRecurseTooDeep,
    erDeepDependence,
    erInvalidFileFormat,
    erInvalidIF,
    erGotoInvalidNumber,
    erCantReadDesktop,
    erDesktopError,
    erInvalidDesktop,
    erCantCreateFile,
    erNotEnoughMemory,
    erCantOpenConfig,
    erCantOpenHelp,
    erInvalidDrive,
    erInvalidFileName,
    erNoDiskSpace,
    erNoDiskSpacePre,
    erTextNotFound,
    erNoQuickDirs,
    erInvalidOvrSize,

    eruuNoStuff,
    eruuUnknownMode,
    eruuBlankLine,
    eruuNonUULine,
    eruuLineNo,
    eruuIncorrectTerm,
    dlUUDecoding,

    dlPromptForQDir,
    dlOpenFile,
    dlOpenFileName,
    dlSaveFileAs,
    dlSaveFileAsName,
    dlFileExist,
    dlCanNotWrite,
    dlValuteDialog,
    dlValuteDlgName,
    dlValuteDlgSymbol,
    dlValuteDlgSForward,
    dlValuteDlgSComma,
    dlOptions,
    dlCellDialog,
    dlCellAsIs,
    dlCellDec,
    dlCellBool,
    dlCellCurrency,
    dlCellDisplay,
    dlCellJLeft,
    dlCellJRight,
    dlCellJCenter,
    dlCellJustify,
    dlCellCurrencyName,
    dlCellDecimals,
    dlCellWidth,
    dlCellExponent,
    dlCellNothing,
    dlCellProtect,
    dlCellComma,
    dlCellPercent,
    dlGotoCellNum,
    dlGotoCellName,
    dlFindCellTitle,
    dlFindCellPattern,
    dlFindCellSensitive,
    dlFindCellWhole,
    dlFindCellSearchAs,
    dlFindCellString,
    dlFindCellValue,
    dlReplaceTitle,
    dlReplacePattern,
    dlReplacePrompt,
    dlYesButton,
    dlOKButton,
    dlNoButton,
    dlNextButton,
    dlAppendButton,
    dl2YesButton,
    dlAllButton,
    dlCancelButton,
    dlHelpButton,
    dlTreeButton,
    dlReanimateButton,
    dlDriveButton,
    dlViewButton,
    dlHexASCIIButton,
    dlMkDirButton,
    dlRereadButton,
    dlOverwriteButton,
    dlSkipButton,
    dlYesForAll,
    dlRead,
    dlWrite,
    dlChooseDir,
    dlChooseFile,
    dlColors,
    dlColorsGroup,
    dlColorsItem,
    dlColorsText,
    dlColorsBack,
    dlColorsColor,
    dlColorsNormal,
    dlColorsHighlight,
    dlColorsUnderline,
    dlColorsInverse,

    dlDBViewNumeric,
    dlDBViewCharacter,
    dlDBViewMemo,
    dlDBViewLogical,
    dlDBViewDate,
    dlDBViewFloat,
    dlDBViewPicture,
    dlDBViewEmpty,
    dlDBViewNoMemo,
    dlDBViewViewMemo,
    dlDBViewInfo,
    dlDBViewInfoString,
    dlDBViewSearchNot,
    dlDBViewSearch,
    dlDBViewSearchingIn,
    dlDBViewTextSearch,
    dlDBEditField,
    dlDBValue,
    dlDBCantEdit,
    dlDBEmptyStruc,

    dlDICurDir,
    dlDINoFiles,
    dlDIFiles,
    dlDIBytes,
    dlDIFile,
    dlDIByte,
    dlDIWith,
    dlDITotalDisk,
    dlDIFreeDisk,
    dlDIVolumeID,
    dlDISerialNo, { Rainbow }
    dlDIFileSys, { Rainbow }
    dlDIMemoryTotal,
    dlDIMemoryForUser,
    dlDIMemoryForDN,
    dlDIEMS1,
    dlDIEMS2,
    dlDIXMSFree,
    dlDINone,

    dlStdDlgManyFiles,
    dlDirectory,
    dlFile,
    dlDrive,
    dlFiles,
    dlDirectoriesLabel,
    dlClear,
    dlOpen,
    dlStop,
    dlReplace,
    dlWarning,
    dlCloseButton,
    dlOCloseButton,
    dlCopyButton,
    dlRenameButton,
    dlDTDiskFull1,
    dlDTDiskFull2,
    dlEraseRO,
    dlEraseCantDelDir,
    dlEraseDirNotEmpty,
    dlEraseConfirm1,
    dlEraseConfirms1,
    dlEraseConfirm2,
    dlErase,
    dlErasingFile,
    dlErasingDir,
    dlErasingNoFile,
    dlEraseConfirmDir,
    dlFBBNotReadSource,
    dlFBBOver1,
    dlFBBOver2,
    dlFBBInsertDisk,
    dlFBBDiskFull1,
    dlFBBNoCreate,
    dlFBBNoWrite,
    dlFBBDiskFull2,
    dlFBBFragment,
    dlFBBDEFragment,
    dlFBBFragmentTo,
    dlFBBNoOpen,
    dlSplit_to,
    dlFlushingBuffers,
    dlComparing,
    dlFCOver,
    dlFCItself,
    dlFCNotDiskSpace,
    dlFCCopy1,
    dlFCMove1,
    dlFCCopy2,
    dlFCMove2,
    dlFCNoRename1,
    dlFCNoRename2,
    dlDIDir,
    dlFCNoCreateDir,
    dlFCCopy,
    dlFCMove,
    dlFCVerifyFailed,
    dlNoFilesSelected,
    dlBytesIn,
    dlSelectedFiles,
    dlTotal,
    dlFileFind,
    dlTempDrive,
    dlNoFilesFound,
    dlFilesFound,
    dlChangeDir,
    dlSetAttr,
    dlSortName,
    dlSortLongName,
    dlSortExt,
    dlSortLongExt,
    dlSortType,
    dlSortDate,
    dlSortCrDate,
    dlSortLADate,
    dlSortDIZ,
    dlSortSize,
    dlSortUnsorted,
    dlViewQuery,
    dlViewSearchFor,
    dlViewCase,

    dlArcMsg1,
    dlArcMsg4,
    dlArcMsg5,
    dlArcMsg6,
    dlArcMsg7,
    dlArcMsg8,
    dlArcReadArc,
    dlArcExtract,
    dlArcDest,
    dlArcSetPsw,
    dlArcMove,
    dlArcAdd,
    dlArcInto,
    dlArcOpt,
    dlArcDelete,
    dlArcName,
    dlArcUSize,
    dlArcPSize,
    dlArcRatio,
    dlArcDate,

    dlQueryExit,
    dlStoreColorPal,
    dlLoadColorPal,
    dlFileName,
    dlDisconnect,
    dlPercentComplete,
    dlTopLFN,
    dlTopSplit,
    dlTopName,
    dlTopSize,
    dlTopTime,
    dlTopDate,
    dlTopCrTime,
    dlTopCrDate,
    dlTopLATime,
    dlTopLADate,
    dlTopPath,
    dlTopOriginal,
    dlTopPacked,
    dlTopRatio,
    dlASCIIChart,
    dlWorksheet,
    dlNotSaved,
    dlExportTitle,
    dlExportLabel,
    dlRunButton,
    dlDropButton,
    dlKillButton,
    dlDeleteButton,
    dlWindowsLabel,
    dlWindowManager,
    dlPhoneBook,
    dlPhonesLabel,
    dlPhonesLabelGroup,
    dlPhonesLabelPhones,
    dlPhonesUpDir,
    dlDialButton,
    dlEditButton,
    dlPhoneAppend,
    dlPhoneDelete,
    dlPhoneDeleteQuery,
    dlFileManager,
    dlDBNumeric,
    dlDBCharacter,
    dlDBMemo,
    dlDBLogical,
    dlDBDate,
    dlDBFloat,
    dlDBPicture,
    dlDBViewName,
    dlJanuary,
    dlFebruary,
    dlMarch,
    dlApril,
    dlMay,
    dlJune,
    dlJuly,
    dlAugust,
    dlSeptember,
    dlOctober,
    dlNovember,
    dlDecember,
    dlCluster,
    dlSector,
    dl1st,
    dl2nd,
    dl3rd,
    dlSometh,
    dlCopyOfFAT,
    dlReservedSector,
    dlDiskFixer,
    dlFixerFill,
    dlCopyTo,
    dlNumberClusters,
    dlStartingCluster,
    dlValidClusters,
    dlClusterRange,
    dlNumberSectors,
    dlStartingSector,
    dlValidSectors,
    dlSectorRange,
    dlSBootSector,
    dlBootSector,
    dlS1stFAT,
    dlS2ndFAT,
    dlRootDirectory,
    dlUnusedDirEntry,
    dlSearching,
    dlPasteFromTitle,
    dlPasteFromLabel,
    dlQueryModified,
    dlReadingFile,
    dlWritingFile,
    dlQueryReplace,
    dlEditTitle,
    dlReanimateTitle,
    dlReanimateView,
    dlReanimateHead,
    dlReanimateScan,
    dlReanimateDirectory,
    dlReanimateRecovered,
    dlReanimateGood,
    dlReanimatePoor,
    dlReanimateAverage,
    dlReanimateWrite,
    dlCantReanimate,
    dlGameTitle,
    dlNewButton,
    dlSetupButton,
    dlTop10Button,
    dlPauseButton,
    dlTetName,
    dlTetrisBest,
    dlTetrisNext,
    dlGameScore,
    dlGameLevel,
    dlGameLines,
    dlGameScore2,
    dlGameInfo,
    dlTetris,
    dlPentix,
    dlTop10,
    dlGameOver,
    dlTotalWrite,
    dlTotalWrited,
    dlBytes,
    dlViewFile,
    dlExportingDIZ,
    dlDeletingDIZ,
    dlDeletingSource,
    erIntoItself,
    dlCreatingDirectory,
    dlPhoneDirDelete,
    dlPleaseStandBy,
    dlArcPacking,
    dlArcUnpacking,
    dlLoadingViewer,
    dlLoadingEditor,
    dlErrorsOccurred,
    dlSelectDirectory,
    dlScreenSaverSetup,
    dlSS_S_electedSavers,
    dlSS_A_dd,
    dlSS_R_emove,
    dlSSA_v_ailableSavers,
    dlSS_N_ever,
    dlSS_1_minute,
    dlSS_2_minutes,
    dlSS_5_minutes,
    dlSS1_0_minutes,
    dlSS_T_ime,
    dlSSUse_M_ouse,
    dlCalculator,
    dlCalcEx_p_ression,
    dlCalcCopy_a_s,
    dlUnknownWindowType,
    dlColorsTitle,
    dl_C_olor,
    dl_B_ackground,
    dl_F_oreground,
    dl_I_tem,
    dl_G_roup,
    dlFmtOs2Warning,
    dlFmtAdvWarning,
    dlFmtMsgStr,
    dlFmtTimeStr,
    dlFmtNewDisk,
    dlFmtContQuery,
    dlFmtErrorSysAreas,
    dlFmtNoDisk,
    dlFmtDiskProtected,
    dlFmtDiskFormatted,
    dlFmtNonDos,
    dlFmtQuickFormat,
    dlFmtTrack0Bad,
    dlFmtNoSysFiles,
    dlFmtBadInterleave,
    dlFmtNoFloppy,
    dlFmt_F_astFormat,
    dlFmt_D_OSFormat,
    dlFmt_S_afeFormat,
    dlFmt_Q_uickFormat,
    dlFmt_M_akeDiskBootable,
    dlFmtFo_r_cedFormatting,
    dlFmtMarkFullTrack,
    dlFmtOptimi_z_eSectors,
    dlFmtAdvButton,
    dlFmtStartButton,
    dlFmtAreYou,
    dlFmtDriveHas1,
    dlFmtDriveHas2,
    dlFBBBinary,
    dlFBBVerify,
    dlQueryCreateDir,
    dlQueryAbort,
    dlFCNotOverDir,
    dlNotRenameDir,
    dlFCRename,
    dlFCRenameNew,
    dlNoPossibleName,
    dlEditDesc,
    dl_D_escription,
    dlFC_Writing,
    dlFC_Written,
    dlFC_Total,
    dlFC_To,
    dlFC_Reading,
    dlFC_WasRead,
    dlFCCheckingDirs,
    dlFC_Older,
    dlFC_Exists,
    dlQueryCancelSearch,
    dlReadingList,
    {dlListDriveTop,}
    dlFillQuery,
    dlSystemInfo,
    dlSI_Ports,
    dlSI_OSVer,
    dlSI_Lpt,
    dlSI_COM,
    dlSI_Memory,
    dlSI_Main,
    dlSI_MainBoard,
    dlSI_TotalMem,
    dlSI_Expanded,
    dlSI_Extended,
    dlSI_MemAvail,
    dlSI_MaxAvail,
    dlSI_DiskDrivers,
    dlSI_1stHard,
    dlSI_2ndHard,
    dlSI_FloppyDrives,
    dlSI_Heads,
    dlSI_Tracks,
    dlSI_SectTrack,
    dlSI_NotPresent,
    dlDetailButton,
    dlTSRTitle,
    dlMemoryInfo,
    dlHookVectors,
    dlUnknown,
    dlMI_CmdLine,
    dlMI_PgmPath,
    dlMI_DeviceHandle1,
    dlMI_DeviceHandle2,
    dlMI_DeviceName,
    dlMI_DeviceFlags,
    dlMI_ResArea,
    dlMI_Files,
    dlMI_Buffers,
    dlMI_FCBs,
    dlMI_Drives,
    dlMI_Stacks,
    dlMI_IFS,
    dlMI_XBuffers,
    dlMsgWarning,
    dlMsgError,
    dlMsgInformation,
    dlMsgConfirm,
    dlMsgQuery,
    dlMsgAbout,

    dlED_VertNeed,
    dlED_Printed,
    dlED_Print,
    dlED_PrintQuery,
    dlED_PrintLine,
    dlED_OpenFile,
    dlED_OverQuery,
    dlED_ModifyRO,

    dlModemNoSetup,
    dlNoModemPort,
    dlManualDial,
    dl_P_honeNumber,
    dlMD_Manual,
    dlPB_SearchPhone,
    dlPB_S_earchString,
    dlPB_NoFind,
    dlPB_ImportPhones,
    dlPB_Working,
    dlPB_CnvReport,
    dlAutoDialer,
    dlAD_Name,
    dlAD_Queue,
    dlAD_Status,
    dlAD_Redial,
    dlAD_ForceDial,
    dlAD_Delete,
    dlAD_HoldUnhold,
    dlAD_Unhold,
    dlAD_Hold,
    dlAD_Dialing,
    dlAD_Holded1,
    dlAD_Holded2,
    dlAD_NoDial,
    dlAD_ModemSays,
    dlPrintingPaused,
    dlPrinting,
    dlPrintOut,
    dlPrintNoInit,
    dlPrintCancelQuery,
    dlPManagerTitle,
    dlPDeleteQeury1,
    dlPDeleteQeury2,
    dlCantPrint,
    dlCantPrintFile,
    dlTermProtocol,
    dlTermProtocolFile,
    dlTermProtocolRece,
    dlTermProtocolSent,
    dlTermProtocolErrors,
    dlTermProtocolEstimate,
    dlTermReceXmodem,
    dlTermReceiving,
    dlTermWaitAnswer,
    dlTermSendFile,
    dlTermSending,
    dlTermWriteFile,
    dlTermSetLog,
    dlTermCommandsHistory,
    dlTermCopy_H_istory,
    dlTermCopy_C_lipboard,
    dlTerminalTitle,
    dlScanningDirs,
    dlTreeFilesWith,
    dlTree1FileWith,
    dlMNUNotFound,
    dlMenuParams,
    dlMenuParamLabel,
    dlGrabWelcome,
    dlValueNotInRange,
    dlEnvVarLabel,
    dlEnvDelConfirm,
    dlEnvAddTitle,
    dlEnvVariable,
    dlEnvRenVar,
    dlFmtDriveLabel,
    dlFmtCapacityLabel,
    dlFmtVolumeL,
    dlFmtFTypeLabel,
    dlFmtTitle,

    dlTeamHint,

    dlTeamAll,
    dlTeam2_000,
    dlTeam2_001,
    dlTeam2_002,
    dlTeam2_003,
    dlTeam2_004,
    dlTeam000,
    dlTeam001,
    dlTeam002,
    dlTeam003,
    dlTeam004,
    dlTeam005,
    dlTeam006,
    dlTeam007,
    dlTeam008,
    dlTeam009,
    dlTeam010,
    dlTeam011,
    dlTeam012,
    dlTeam013,
    dlTeam014,
    dlTeam015,
    dlTeam016,
    dlTeam017,
    dlTeam018,
    dlTeam019,
    dlTeam020,
    dlTeam021,
    dlTeam022,
    dlTeam023,
    dlTeam024,
    dlTeam025,
    dlTeam026,
    dlTeam027,
    dlTeam028,
    dlTeam029,
    dlTeam030,
    dlTeam031,
    dlTeam032,
    dlTeam033,
    dlTeam034,
    dlTeam035,
    dlTeam036,
    dlTeam037,
    dlTeam038,
    dlTeam039,
    dlTeam040,
    dlTeam041,
    dlTeam042,
    dlTeam043,
    dlTeam044,
    dlTeam045,
    dlTeam046,
    dlTeam047,
    dlTeam048,
    dlTeam049,
    dlTeam050,
    dlTeam051,
    dlTeam052,
    dlTeam053,
    dlTeam054,
    dlTeam055,
    dlTeam056,
    dlTeam057,
    dlTeam058,
    dlTeam059,
    dlTeam060,
    dlTeam061,
    dlTeam062,
    dlTeam063,
    dlTeam064,
    dlTeam065,
    dlTeam066,
    dlTeam067,
    dlTeam068,
    dlTeam069,
    dlTeam070,
    dlTeam071,
    dlTeam072,
    dlTeam073,
    dlTeam074,
    dlTeam075,
    dlTeam076,
    dlTeam077,
    dlTeam078,
    dlTeam079,
    dlTeam080,
    dlTeam081,
    dlTeam082,
    dlTeam083,
    dlTeam084,
    dlTeam085,
    dlTeam086,
    dlTeam087,
    dlTeam088,
    dlTeam089,
    dlTeam090,
    dlTeam091,
    dlTeam092,
    dlTeam093,
    dlTeam094,
    dlTeam095,
    dlTeam096,
    dlTeam097,
    dlTeam098,
    dlTeam099,
    dlTeam100,
    dlTeam101,
    dlTeam102,
    dlTeam103,
    dlTeam104,
    dlTeam105,
    dlTeam106,
    dlTeam107,
    dlTeam108,
    dlTeam109,
    dlTeam110,
    dlTeam111,
    dlTeam112,
    dlTeam113,
    dlTeam114,
    dlTeam115,
    dlTeam116,
    dlTeam117,
    dlTeam118,
    dlTeam119,
    dlTeam120,
    dlTeam121,
    dlTeam122,
    dlTeam123,
    dlTeam124,
    dlTeam125,
    dlTeam126,
    dlTeam127,
    dlTeam128,
    dlTeam129,
    dlTeam130,
    dlTeam131,
    dlTeam132,
    dlTeam133,
    dlTeam134,
    dlTeam135,
    dlTeam136,
    dlTeam137,
    dlTeam138,
    dlTeam139,
    dlTeam140,
    dlTeam141,
    dlTeam142,
    dlTeam143,
    dlTeam144,
    dlTeam145,
    dlTeam146,
    dlTeam147,
    dlTeam148,
    dlTeam149,
    dlTeam150,
    dlTeam151,

    dlHelp,
    dlOutputTitle,

    dlCDPlayer,
    dlCDPlayerNoCD,
    dlCDTrack,
    dlCDPlayTrack,
    dlCDSelectTrack,

    dlCDTrackButton,
    dlCDAddButton,
    dlCDDeleteButton,
    dlCDSaveButton,
    dlCD_R_Button,
    dlCD_C_Button,

    dlCDTrackTime,
    dlCDTrackLeft,
    dlCDDiskTime,
    dlCDDiskLeft,

    dlCDButtonRew,
    dlCDButtonPrev,
    dlCDButtonPlay,
    dlCDButtonNext,
    dlCDButtonFFwd,
    dlCDButtonPause,
    dlCDButtonStop,
    dlCDButtonEject,

    dlCDButtonSet,
    dlCDButtonTime,
    dlCDButtonMode,
    dlCDButtonMix,
    dlCDButtonOrder,
    dlCDButtonTitle,

    dlNoCD,
    dlCloseCD,

    dlDITemporary,
    dlDIFileFind,
    dlDIFFMask,
    dlBranch,
    dlDICurArchive,
    dlDIArcTotalFiles,
    dlDIPackedSize,
    dlDIUnpackedSize,
    dlUpDir,
    dlSubDir,
    dlWKZ_Empty,
    dlPM_Print,
    dlArvid_Title,
    dlArvid_Type,
    dlArvid_TimeUsed,
    dlArvid_TimeLeft,
    dlArvid_TapeDir,
    dlUUDecode,             {+UUDecode}
    dlUUDecodingto,
    dlUUDecodeFiles,
    dlUUDecodeErrors,       {-UUDecode}
    dlUUEncode,             {+UUEncode}
    dlUUEncoding,
    dlUUEncodeSFN,
    dlUUEncodeOS,
    dlUUEncodeCreated,
    dlUUEncodeTime,
    dlUUEncodeSize,
    dlUUEncodeSections,
    dlUUEncodeLines,        {-UUEncode}
    dlFileIsSmall,
    dlMaxFiles,

    dlleSection,
    dlleSections,
    dlleListed,
    dlleCalculated,
    dlleFileSizeMismatch,
    dlleFileCRCMismatch,
    dlleErrorOpenTMP,
    dlleCantCreate,
    dlleFailedToDecode,
    dlleFileNameExp,
    dlleFileNamesMismatch,
    dlleFileTimeExp,
    dlleInvFileTimeNum,
    dlleCRC_Err,
    dlleSizeMism,
    dlleChkSumFmt,
    dlleSectionHdr,
    dlleMaxSectNumMism,
    dlleDuplicateSection,
    dlleOfFile,
    dlleIsAbsent,
    dlleAreAbsent,
    dlleUnexpEND,
    dlleNoTerm,
    dlleFailedFinal,

    dlOverflow,
    dlElapsedTime,
    dlEditHistory,
    dlReplacesMade,
    dlScrollBack,

    dlColorsBlack,
    dlColorsDark,
    dlColorsGray,
    dlColorsWhite,
    dlColorsColors,
    dlColorsDefault,
    dlColorsBlinking,
    dlColorsB_W_,
    dlColorsMono,
    dlColorsVGA,
    dlColors_VGA,
    dlColors_P_alette,
    dlColorsColor_,
    dlColors_R_ed,
    dlColors_G_reen,
    dlColors_B_lue,
    dlColors_G_ray,
    dlColors_D_efault,

    dlColors_C00,
    dlColors_C01,
    dlColors_C02,
    dlColors_C03,
    dlColors_C04,
    dlColors_C05,
    dlColors_C06,
    dlColors_C07,
    dlColors_C08,
    dlColors_C09,
    dlColors_C10,
    dlColors_C11,
    dlColors_C12,
    dlColors_C13,
    dlColors_C14,
    dlColors_C15,
    dlReanimateNone,
    dlCantReanimateDirectory,
    dlAutoexecWarning,
    dlSelectXLT,
    dlSetDirHistory,
    dlSetEditHistory,
    dlSetViewHistory,

    dlLinkTitle,          {+LINK}
    dlLinkStarted,
    dlLinkClose,
    dlLinkErrors,
    dlLinkWaitingCmd,
    dlLinkSendingFile,
    dlLinkReceivingFile,
    dlLinkWaitFile,
    dlLinkQueryExit,
    dlLinkNoRemote,
    dlLinkTransmitFile,
    dlLinkNoRemoteLink,
    dlLinkRemoteError,
    dlLinkRemoteWait,
    dlLinkEraseDirCfm,
    dlLinkErasingFile,
    dlLinkErase,
    dlLinkRemoteDirInfo,
    dlLinkSend,
    dlLinkRcv,            {-LINK}

    dlBytesOf,
    dlExecDOScmdHdr,
    dlExecDOScmdLine,
    dlPnlDescription,

    dlCE_CriticalError,
    dlCE_WriteProtected,
    dlCE_DiskNotReady,
    dlCE_DataIntegrity,
    dlCE_SeekError,
    dlCE_UnknownMedia,
    dlCE_SectorNotFound,
    dlCE_OutOfPaper,
    dlCE_WriteFault,
    dlCE_ReadFault,
    dlCE_GeneralFailure,
    dlCE_BadImageOfFAT,
    dlCE_DeviceError,
    dlCE_InsertDisk,
    dlCE_SharingViolation,
    dlCE_WrongDisk,        {JO}
    dlCE_DiskFull,
    dlCE_DeviceInUse,      {/JO}
    dlCE_Filename_Exced_Range, {AK155}
    dlCE_Dirty_Flag,           {/AK155}
    dlCE_Filename_Illegal,     {JO}
    dlCE_ErrWndTitle,

    dlCE_ButAbort,
    dlCE_ButRetry,
    dlCE_ButIgnore,

    dlViewSaveXlat,

    dlFileMask,  {-$VIV}
    dlNetInfo,
    dlServer,
    dlConnections,
    dlLookedFiles,
    dlVeryDeepSearch,
    dlLookedDirs,

    dlSE_Dir2Long, {-VOL}

    stDaysWeek,  {-SSK}

    dlOldFiles,
    dlOldFilesNoWarn,

    dlPresent,
    dlAbsent,

    dlImage,
    dlImageError,
    dlUnpackImages,
    dlTargetDir,

        {-DataCompBoy-}
    dlSPF01,
    dlSPF02,
    dlSPF03,
    dlSPF04,
    dlSPF05,
    dlSPF06,
    dlSPF07,
    dlSPF08,
    dlSPF09,
    dlSPF10,
    dlSPF11,
    dlSPF12,
    dlSPF13,
    dlSPF14,
    dlSPF15,
    dlSPF16,
    dlSPF17,
    dlSPF18,
    dlSPF19,
    dlSPF20,
    dlSPF21,
    dlSPF22,
    dlSPF23,
    dlSPF24,
    dlSPF25,
    dlSPFht,
    dlCopied,
    dlTestButton,
        {-DataCompBoy-}
    dlArvidNeedDisk,
    dlArvidCmdFileCreated,
    dlArvidCmdFileAppended,
    dlArvidCanChangeOnlyAVT,
    dlArvidVolumeIsNotTape,
    dlArvidNoReal,

    dlAnonymous,  (*X-Man*)
    dlMenuItemOn, (*X-Man*)
    dlMenuItemOff,(*X-Man*)
    {-DataCompBoy-}
    dlMachineTypeFF,      {+SYSINFO}
    dlMachineTypeFE,
    dlMachineTypeFD,
    dlMachineTypeFC_1,
    dlMachineTypeFC_2,
    dlMachineTypeFC_4,
    dlMachineTypeFC_5,
    dlMachineTypeFC_6,
    dlMachineTypeFC_8,
    dlMachineTypeFC_0b,
    dlMachineTypeFC_20,
    dlMachineTypeFC_42,
    dlMachineTypeFC_45,
    dlMachineTypeFC_48,
    dlMachineTypeFC_4F,
    dlMachineTypeFC_50,
    dlMachineTypeFC_51,
    dlMachineTypeFC_52,
    dlMachineTypeFC_94,
    dlMachineTypeFC_else,
    dlMachineTypeFB86,
    dlMachineType80,
    dlMachineTypeFA_1,
    dlMachineTypeFA_else,
    dlMachineTypeF9,
    dlMachineTypeF8_4_9_B,
    dlMachineTypeF8_else,
    dlMachineTypeB6,
    dlMachineType9A,
    dlMachineType2D,
    dlMachineTypeE1,
    dlMachineType30,
    dlMachineTypeElse,
    dlMHz,
    dlUnknownDiskDriverType,
    dlEmulated,
    dlInternal,
    dlNone,
    dlUnknownCPUFPU,         {-SYSINFO}
    {-DataCompBoy-}
    dlcTitle,    {JO}{+CALENDAR}
    dlcLine,
    dlcJanuary,
    dlcFebruary,
    dlcMarch,
    dlcApril,
    dlcMay,
    dlcJune,
    dlcJuly,
    dlcAugust,
    dlcSeptember,
    dlcOctober,
    dlcNovember,
    dlcDecember, {JO}{-CALENDAR}

    dlOpenNewWindow,
    dlMore, {JO}
    dlFindText,
    dlViewFilter,

    dlTrashCaption, {X-Man}
    dlFilDir,
    dlDirectories,
    dlFindPanel,
    dlListPanel, {-DataCompBoy-}
    dlEmpty,
    dlFileSearch,
    dlKillBuffer,
    dlSaveScrollback,
    dlScrollbackSaved,
    dlWriteASCII,
    dlWriteAvatar,
    dlWriteAnsi,
    dlCopyToTerminal,
    dlCopyToHistory,
    dlCopyToClipboard,
    dlSaveAsASCII,
    dlSaveAsANSI,
    dlSaveAsAvatar,
    dlPktView,
    dlPktHeader,
    dlPktFile,
    dlPktMsg,
    dlViewMsg,
    dlSaveMsg,
    dlMsgFile,
    dlErrorWriting,
    dlNetMailView,
    dlLastMsg,
    dlFirstMsg,
    dlDelMsg,
    dlNotPkt,
    dlReadingPkt,
    dlLine,
    dlNameLong,
    dlNameShort,
    dlFullInfo,
    dlDescr,
    dlPath,
    dlAdvancedFormat,   {JO}
    dlBootSector1,
    dlEntries,
    dlDirEntries,
    dlSize,
    dlClusterSize,
    dlTracks,
    dlHeads,
    dlSectors1,
    dlSectors2,
    dlStartTr1,
    dlStartTr2,
    dlStartTr3,
    dlInterleave1,
    dlInterleave2,
    dlSectorSh1,
    dlSectorSh2,
    dlSectorSh3,
    dlHeadSectSh1,
    dlHeadSectSh2,
    dlHeadSectSh3,
    dlTrackSectSh1,
    dlTrackSectSh2,
    dlTrackSectSh3,
    dlTrack,
    dlSectorSize1,
    dlSectorSize2,
    dlDrivesType,
    dlDisk,
    dlDOS,
    dlAutodetect,       {/JO}

    dl_Failed_to_enumerate_EA, {JO - Extended Attributes}
    dl_Failed_to_retrieve_EA,
    dl_Failed_to_store_EA,
    dlEditEALongname,
    dl_EALongname, {/JO}
    dlChangeLngWarning, {JO}
    dl_Failed_to_set_volume_label, {JO}

    sdtError,         {X-Man >>>}
    sdtFixed,
    sdtRemovable,
    sdtRemote,
    sdtCDROM,
    sdtDblSpace,
    sdtSUBST,
    sdtStacker,
    sdtRAMDrive,
    sdtDublDisk,
    sdtBernoully,
    sdtDiskreet,
    sdtSuperStor,     {X-Man <<<}
    sdtFixed1,        {JO}

    dlDoes_Not_Exist, {JO}
    dlCmdLineTooLong, {JO}
    dlCantLoad,       {JO}
    dlIniDataReadError, {PZ}
    dlIniDataTooLong,   {PZ}
    dlNotValidForFullscreen, {JO}
    dlCreateObject,   {JO}
    dlObjectTitle,
    dl_CodePage_FS_Error, {JO}
    dlCre,            {JO}
    dlLac             {JO}
      );

      TDlgIdx = (
        dlgSelect,
        dlgUnselect,
    dlgSetupArc,
    dlgCommandsHistory,
    dlgExtract,
    dlgSetPassword,
    dlgCompareDirs,
    dlgFilesAttr,
    dlgFileAttr,
    dlgFindParms,
    dlgArcParms,
    dlgDiskParms,
    dlgFMSetup,
    dlgFMDefaults,
    dlgPanelSetup,
    dlgDriveInfoSetup,
    dlgDiskError,
    dlgMakeList,
    dlgCopyDialog,
    dlgRenFl,
    dlgRenameDialog,
    dlgMkDir,
    dlgFileFind,
    dlgEditorReplace,
    dlgEditorFind,
    dlgEditorFormat,
    dlgGotoLine,
    dlgHighlightGroups,
    dlgColors,
    dlgWkzMenuBar,
    dlgSetCellFormat,
    dlgGotoCellNumber,
    dlgFindCell,
    dlgReplaceCell,
    dlgDbFind,
    dlgFixerMenu,
    dlgGameSetup,
    dlgPrinterSetup,
    dlgEditDirectory,
    dlgAppendDirectory,
    dlgEditNumber,
    dlgAppendNumber,
    dlgMainMenu,
    dlgStatusLine,
    dlgEditorDefaults,
    dlgSystemSetup,
    dlgSetupModem,
    dlgSetupTerminal,
    dlgSaversSetup,
    dlgInterfaceSetup,
    dlgMouseSetup,
    dlgStartupSetup,
    dlgArchiveFiles,
    dlgReenterPassword,
    dlgAdvanceSearch,
    dlgCountrySetup,
    dlgConfirmations,
    dlgEditorCommands,
    dlgEditorMenu,
    dlgGotoAddress,
    dlgCDOptions,
    dlgPhoneBook,
    dlgDenie,
    dlgManualDial,
    dlgCalculator,
    dlgWindowManager,
    dlgVolumeLabel,
    dlgEditEnvironment,
    dlgTetrisWinner,
    dlgSplitFile,
    dlgCombineFile,
    dlgTetrisTop10,
    dlgPentixTop10,
    dlgAdvancedFilter,
    dlgUUDecode, {UUDECODE}
    dlgUUEncode, {UUECNODE}
    dlgOverwriteQuery,
    dlgAppendQuery, {AK155}
    dlgViewerFind,
    dlgArvidFileFind,
    dlgArvidFindResults,
    dlgDirectoryHistory,
    dlgEditHistory,
    dlgViewHistory,
    dlgAdvancedCOMSetup,
    dlgCpuFlagInfo,
    dlgFTNInfo,
    dlgSrchFailed,
    {DataCompBoy}
    dlgSetupColumnsDisk,
    dlgSetupColumnsFind,
    dlgSetupColumnsTemp,
    dlgSetupColumnsArch,
    dlgSetupColumnsArvd,
    dlgNextSection,
    dlgNameCase,
    dlgChScreenMode {JO}
  );

IMPLEMENTATION END.
