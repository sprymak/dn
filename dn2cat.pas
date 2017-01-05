library Dn2Cat;
(******

Dynamic Link Library with ALL DN/2 procedures and objects
Written by Cat 2:5030/1326.13
(for use in DN/2 Plugin Version)

******)


{$IFNDEF VIRTUALPASCAL}
{$ERROR Compile this library with Virtual Pascal!}
{$ENDIF}

{$B-,D-,H-,I-,J+,P+,Q-,R-,T-,V-,W-,X+,Z-}
{&AlignCode+,AlignData+,AlignRec-,Asm-,Cdecl-,Comments-,Delphi+,Frame-,G3+}
{&LocInfo-,Open32-,Optimise+,OrgName-,SmartLink+,Speed+,Use32-,ZD-}
{$M 80000}

{$I STDEFINE.INC}
{$IFNDEF PLUGIN}
{$ERROR 'PLUGIN' define required in STDEFINE.INC for compile this library!}
{$ENDIF}

uses
  {$IFDEF OS2} OS2Base,OS2Def,EAOper,FlOs2Tl,Dn2PmApi, {$ENDIF}
  {$IFDEF WIN32} Windows,ExeHdr,Strings,FlW32Tl, {$ENDIF}
  {$IFDEF REGEXP} RegExp, {$ENDIF}
  {$IFDEF MIRRORVARS} Vars, {$ENDIF}
  {Os2MM,PMObj,}Dos,{CRT,}
  VPSysLow,VPUtils,{Consts}
  Objects,Drivers,Memory,Views,Menus,Dialogs,DNApp,
  Advance,Advance1,Advance2,Advance3,Advance4,Advance6,Advance7,
  ArchDet,Archiver,{ArchRead,}ArchSet,ArcView,
  Arc_zip,Arc_lha,Arc_rar,Arc_ace,Arc_ha,Arc_cab,
  {$IFNDEF MINARCH}
  Arc_ain,Arc_bs2,Arc_bsa,Arc_chz,Arc_hap,
  Arc_hpk,Arc_hyp,Arc_is3,Arc_lim,Arc_qrk,Arc_sqz,
  Arc_tar,{Arc_tgz,}Arc_uc2,Arc_ufa,Arc_zoo,Arc_zxz,
  {$ENDIF}
  {Arvid,AridAvt,ArvidTdr,}
  AsciiTab,{BackTrc,BigArr,BigArray,BStrings,}Calc,Calculat,Calendar,
  {CallSpcb,}CCalc,{CDPlayer,CDUtil,}CellsCol,CmdLine,Collect,{Colls,}
  Colors,ColorSel,ColorVGA,Commands,{CPUType,}DblWnd,DBView,DBWatch,
  {DefColl,}DiskImg,DiskInfo,{DiskTool,}DNExec,{DNFormat,}DNHelp,DNIni,
  DNStdDlg,DNUtil,{DosMem,DPMI,}Drivers2,Drives,{DrvTypes,}Ed2,
  Editor,EdWin,Eraser,ErrMess,{ExtKbd,ExtraMem,}FBB,FileCopy,FileDiz,
  FileFind,{FileList,}FileLst,FilesCol,FindObj,{Flage,}FlPanel,
  FlPanelX,FlTools,{FmtUnit,Format,}FStorage,FViewer,Gauge,Gauges,
  {GetConst,Hash,}HelpFile,HelpKern,HideView,HighLite,HistList,Histries,
  Idlers,{IniFiles,Keyboard,LFN,LFNCol,}LFNVP,{ListMakr,}Macro,{MapEngn,}
  {MemInfo,}Messages,MicroEd,MicroEd2,ObjType,{Overlay1,Overlays,}
  PackF_RS,{Pack_LZW,}Phones,{PktView,PrintMan,}Profile,RegAll,{RspFile,}
  RStrings,{SBlocks,ScrollBk,}Scroller,{Search,}Setups,{StakDump,StarSky,}
  Startup,StrView,Swe,{SysInfo,Tetris,}TitleSet,{TRect_,}Tree,UFnMatch,
  UniWin,UserMenu,{UserSavr,}UUCode,U_KeyMap,U_MyApp,U_SrchF,Validate,
  VideoMan,{WinClp,}WinClpVP,{Wizard,}XDblWnd,XTime,
  Plugin, PlugRez, {Cat}
  DN1;

{$EXPORT:BYNAME}

{$IFDEF OS2}
 {$EXPORT  System}
 {$EXPORT  OS2Base,OS2Def,EAOper,FlOs2Tl,Dn2PmApi}
{$ENDIF}
{$IFDEF WIN32}
 {$EXPORT  System}
 {$EXPORT  Windows,ExeHdr,Strings,FlW32Tl}
{$ENDIF}
{$IFDEF MIRRORVARS}
 {$EXPORT  Vars}
{$ENDIF}
{$IFDEF REGEXP}
 {$EXPORT  RegExp}
{$ENDIF}
{$EXPORT  Dos}
{$EXPORT  VPSysLow,VPUtils}
{$EXPORT  Objects,Drivers,Memory,Views,Menus,Dialogs,DNApp}
{$EXPORT  Advance,Advance1,Advance2,Advance3,Advance4,Advance6,Advance7}
{$EXPORT  ArchDet,Archiver,ArchSet,ArcView}
{$EXPORT  Arc_zip,Arc_lha,Arc_rar,Arc_ace,Arc_ha,Arc_cab}
{$IFNDEF MINARCH}
 {$EXPORT  Arc_ain,Arc_bs2,Arc_bsa,Arc_chz,Arc_hap}
 {$EXPORT  Arc_hpk,Arc_hyp,Arc_is3,Arc_lim,Arc_qrk,Arc_sqz}
 {$EXPORT  Arc_tar,Arc_uc2,Arc_ufa,Arc_zoo,Arc_zxz}
{$ENDIF}
{$EXPORT  AsciiTab,Calc,Calculat,Calendar}
{$EXPORT  CCalc,CellsCol,CmdLine,Collect}
{$EXPORT  Colors,ColorSel,ColorVGA,Commands,DblWnd,DBView,DBWatch}
{$EXPORT  DiskImg,DiskInfo,DNExec,DNHelp,DNIni}
{$EXPORT  DNStdDlg,DNUtil,Drivers2,Drives,Ed2}
{$EXPORT  Editor,EdWin,Eraser,ErrMess,FBB,FileCopy,FileDiz}
{$EXPORT  FileFind,FileLst,FilesCol,FindObj,FlPanel}
{$EXPORT  FlPanelX,FlTools,FStorage,FViewer,Gauge,Gauges}
{$EXPORT  HelpFile,HelpKern,HideView,HighLite,HistList,Histries}
{$EXPORT  Idlers,LFNVP,Macro}
{$EXPORT  Messages,MicroEd,MicroEd2,ObjType}
{$EXPORT  PackF_RS,Pack_LZW,Phones,Profile,RegAll}
{$EXPORT  RStrings,Scroller,Setups}
{$EXPORT  Startup,StrView,Swe,TitleSet,Tree,UFnMatch}
{$EXPORT  UniWin,UserMenu,UUCode,U_KeyMap,U_MyApp,U_SrchF,Validate}
{$EXPORT  VideoMan,WinClpVP,XDblWnd,XTime}
{$EXPORT  Plugin,PlugRez}
{$EXPORT  DN1}

initialization
end.
