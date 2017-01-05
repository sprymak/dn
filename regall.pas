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
UNIT RegAll;

INTERFACE

procedure RegisterAll;

IMPLEMENTATION

Uses
{$IFNDEF RCP}
     Arc_ZIP,  Arc_LHA,  Arc_RAR,  Arc_ACE,  Arc_HA,   Arc_CAB,
{$IFNDEF MINARCH}
     Arc_ARC,  Arc_BSA,  Arc_BS2,  Arc_HYP,  Arc_LIM,  Arc_HPK,  Arc_TAR,
     {$IFDEF TGZ} Arc_TGZ, {$ENDIF} Arc_ZXZ,  Arc_QRK,  Arc_AIN,  Arc_CHZ,  Arc_HAP,  Arc_IS3,
     Arc_SQZ,  Arc_UC2,  Arc_UFA,  Arc_ZOO,
{$ENDIF}
     Archiver, Arcview,  Arvid,    Asciitab, Ccalc,
     Collect,  Diskinfo, Dnapp,    Dnstddlg, Dnutil,   Drives,   Ed2,
     Editor,   Filefind, Filescol, Flpanel,  Fstorage, Fviewer,  Gauges,
     Histries, Microed,  Startup,  Tree,     Uniwin,   Usermenu, Xdblwnd,
     Helpkern,
{$IFDEF SpreadSheet}     Calc,     Cellscol,           {$ENDIF}
{$IFDEF Calendar}        Calendar,                     {$ENDIF}
{$IFDEF CDPlayer}        Cdplayer,                     {$ENDIF}
{$IFDEF DBView}          Dbview,                       {$ENDIF}
{$IFDEF MODEM}           Scrollbk, Terminal, uDialer,
           {$IFDEF LINK} Navylink, {$ENDIF}            {$ENDIF}
{$IFDEF PrintManager}    Printman,                 {$ENDIF}
{$IFDEF Game}            Tetris,                       {$ENDIF}
{$IFDEF NetInfo}         Netinfo,                      {$ENDIF}
{$IFDEF PHONES}          Phones,                       {$ENDIF}
{$ENDIF !RCP}
{$IFDEF CHCOL}           Colorsel,                     {$ENDIF}
     Dialogs,  Menus,    Objects,  ObjType,  Scroller, Setups,
     Validate, Views,    SWE {$IFDEF UserSaver}, UserSavr{$ENDIF};

const
  NumRElms =
{$IFDEF RCP}31{$ELSE} 120
  {$IFDEF MODEM}       +7 {$IFDEF LINK} +2 {$ENDIF} {$ENDIF}
  {$IFDEF SpreadSheet} +5 {$ENDIF}
  {$IFDEF Game}        +3 {$ENDIF}
  {$IFDEF PrintManager}+4 {$ENDIF}
  {$IFDEF CDPlayer}    +2 {$ENDIF}
  {$IFDEF DBView}      +4 {$ENDIF}
  {$IFDEF Calendar}    +2 {$ENDIF}   {JO}
  {$IFDEF TrashCan}    +1 {$ENDIF}
  {$IFDEF NETINFO}     +1 {$ENDIF}
  {$IFDEF TGZ}         +1 {$ENDIF}
  {$IFDEF MINARCH}    -18 {$ENDIF}
  {$IFDEF PHONES}      +4 {$ENDIF}
  {$IFDEF UserSaver}   +1 {$ENDIF}
{$ENDIF}
  {$IFDEF SS}          +2 {$ENDIF}
  {$IFDEF CHCOL}       +7 {$ENDIF}
  ;
  RegArray : array[1..NumRElms] of TStreamRec = (
{$IFNDEF RCP}
      { Arc_ZIP }
      (ObjType: otZIPArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_ZIP.TZIPArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_ZIP.TZIPArchive.Load;
       Store  : @Arc_ZIP.TZIPArchive.Store)
      { Arc_LHA }
     ,(ObjType: otLHAArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_LHA.TLHAArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_LHA.TLHAArchive.Load;
       Store  : @Arc_LHA.TLHAArchive.Store)
      { Arc_RAR }
     ,(ObjType: otRARArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_RAR.TRARArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_RAR.TRARArchive.Load;
       Store  : @Arc_RAR.TRARArchive.Store)
      { Arc_CAB }
     ,(ObjType: otCABArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_CAB.TCABArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_CAB.TCABArchive.Load;
       Store  : @Arc_CAB.TCABArchive.Store)
      { Arc_ACE }
     ,(ObjType: otACEArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_ACE.TACEArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_ACE.TACEArchive.Load;
       Store  : @Arc_ACE.TACEArchive.Store)
      { Arc_HA }
     ,(ObjType: otHAArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_HA.THAArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_HA.THAArchive.Load;
       Store  : @Arc_HA.THAArchive.Store)
{$IFNDEF MINARCH}
      { Arc_arc }
     ,(ObjType: otARCArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_arc.TARCArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_arc.TARCArchive.Load;
       Store  : @Arc_arc.TARCArchive.Store)
      { Arc_bsa }
     ,(ObjType: otBSAArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_bsa.TBSAArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_bsa.TBSAArchive.Load;
       Store  : @Arc_bsa.TBSAArchive.Store)
      { Arc_bs2 }
     ,(ObjType: otBS2Archiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_bs2.TBS2Archive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_bs2.TBS2Archive.Load;
       Store  : @Arc_bs2.TBS2Archive.Store)
      { Arc_hyp }
     ,(ObjType: otHYPArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_hyp.THYPArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_hyp.THYPArchive.Load;
       Store  : @Arc_hyp.THYPArchive.Store)
      { Arc_lim }
     ,(ObjType: otLIMArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_lim.TLIMArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_lim.TLIMArchive.Load;
       Store  : @Arc_lim.TLIMArchive.Store)
      { Arc_hpk }
     ,(ObjType: otHPKArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_hpk.THPKArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_Hpk.THPKArchive.Load;
       Store  : @Arc_hpk.THPKArchive.Store)
      { Arc_TAR }
     ,(ObjType: otTARArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_TAR.TTARArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_TAR.TTARArchive.Load;
       Store  : @Arc_TAR.TTARArchive.Store)
{$IFDEF TGZ}
      { Arc_TGZ }
     ,(ObjType: otTGZArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_TGZ.TTGZArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_TGZ.TTGZArchive.Load;
       Store  : @Arc_TGZ.TTGZArchive.Store)
{$ENDIF}
      { Arc_ZXZ }
     ,(ObjType: otZXZArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_ZXZ.TZXZArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_ZXZ.TZXZArchive.Load;
       Store  : @Arc_ZXZ.TZXZArchive.Store)
      { Arc_QRK }
     ,(ObjType: otQuArkArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_QRK.TQuArkArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_QRK.TQuArkArchive.Load;
       Store  : @Arc_QRK.TQuArkArchive.Store)
      { Arc_UFA }
     ,(ObjType: otUFAArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_UFA.TUFAArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_UFA.TUFAArchive.Load;
       Store  : @Arc_UFA.TUFAArchive.Store)
      { Arc_IS3 }
     ,(ObjType: otIS3Archiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_IS3.TIS3Archive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_IS3.TIS3Archive.Load;
       Store  : @Arc_IS3.TIS3Archive.Store)
      { Arc_SQZ }
     ,(ObjType: otSQZArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_SQZ.TSQZArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_SQZ.TSQZArchive.Load;
       Store  : @Arc_SQZ.TSQZArchive.Store)
      { Arc_HAP }
     ,(ObjType: otHAPArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_HAP.THAPArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_HAP.THAPArchive.Load;
       Store  : @Arc_HAP.THAPArchive.Store)
      { Arc_ZOO }
     ,(ObjType: otZOOArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_ZOO.TZOOArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_ZOO.TZOOArchive.Load;
       Store  : @Arc_ZOO.TZOOArchive.Store)
      { Arc_CHZ }
     ,(ObjType: otCHZArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_CHZ.TCHZArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_CHZ.TCHZArchive.Load;
       Store  : @Arc_CHZ.TCHZArchive.Store)
      { Arc_UC2 }
     ,(ObjType: otUC2Archiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_UC2.TUC2Archive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_UC2.TUC2Archive.Load;
       Store  : @Arc_UC2.TUC2Archive.Store)
      { Arc_AIN }
     ,(ObjType: otAINArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arc_AIN.TAINArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arc_AIN.TAINArchive.Load;
       Store  : @Arc_AIN.TAINArchive.Store)
{$ENDIF MINARCH}
      { Archiver }
     ,(ObjType: otARJArchiver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Archiver.TARJArchive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Archiver.TARJArchive.Load;
       Store  : @Archiver.TARJArchive.Store)
     ,(ObjType: otFileInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Archiver.TFileInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @Archiver.TFileInfo.Load;
       Store  : @Archiver.TFileInfo.Store)
{$IFDEF UserSaver}
     ,(ObjType: otUserSaver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(UserSavr.TUserSaver){$IFDEF OFFS}^{$ENDIF});
       Load   : @UserSavr.TUserSaver.Load;
       Store  : @UserSavr.TUserSaver.Store)
{$ENDIF UserSaver}
      { ArcView }
     ,(ObjType: otArcDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ArcView.TArcDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @ArcView.TArcDrive.Load;
       Store  : @ArcView.TArcDrive.Store)
      { Arvid }
     ,(ObjType: otArvidDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Arvid.TArvidDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Arvid.TArvidDrive.Load;
       Store  : @Arvid.TArvidDrive.Store)
      { AsciiTab }
     ,(ObjType: otTable;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(AsciiTab.TTable){$IFDEF OFFS}^{$ENDIF});
       Load   : @AsciiTab.TTable.Load;
       Store  : @AsciiTab.TTable.Store)
     ,(ObjType: otReport;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(AsciiTab.TReport){$IFDEF OFFS}^{$ENDIF});
       Load   : @AsciiTab.TReport.Load;
       Store  : @AsciiTab.TReport.Store)
     ,(ObjType: otASCIIChart;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(AsciiTab.TASCIIChart){$IFDEF OFFS}^{$ENDIF});
       Load   : @AsciiTab.TASCIIChart.Load;
       Store  : @AsciiTab.TASCIIChart.Store)
      { Calc }
{$IFDEF SpreadSheet}
     ,(ObjType: otCalcWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calc.TCalcWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calc.TCalcWindow.Load;
       Store  : @Calc.TCalcWindow.Store)
     ,(ObjType: otCalcView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calc.TCalcView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calc.TCalcView.Load;
       Store  : @Calc.TCalcView.Store)
     ,(ObjType: otCalcInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calc.TCalcInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calc.TCalcInfo.Load;
       Store  : @Calc.TCalcInfo.Store)
     ,(ObjType: otInfoView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calc.TInfoView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calc.TInfoView.Load;
       Store  : @Calc.TInfoView.Store)
      { CellsCol }
     ,(ObjType: otCellCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(CellsCol.TCellCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @CellsCol.TCellCollection.Load;
       Store  : @CellsCol.TCellCollection.Store)
{$ENDIF SpreadSheet}
{$IFDEF Calendar}
      { Calendar }
     ,(ObjType: otCalendarView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calendar.TCalendarView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calendar.TCalendarView.Load;
       Store  : @Calendar.TCalendarView.Store)
     ,(ObjType: otCalendarWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Calendar.TCalendarWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Calendar.TCalendarWindow.Load;
       Store  : @Calendar.TCalendarWindow.Store)
{$ENDIF Calendar}
      { CCalc }
     ,(ObjType: otCalcLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(CCalc.TCalcLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @CCalc.TCalcLine.Load;
       Store  : @CCalc.TCalcLine.Store)
     ,(ObjType: otIndicator;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(CCalc.TIndicator){$IFDEF OFFS}^{$ENDIF});
       Load   : @CCalc.TIndicator.Load;
       Store  : @CCalc.TIndicator.Store)
{$IFDEF CDPlayer}
      { CDPlayer }
     ,(ObjType: otCdplayer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(CDPlayer.TCdPlayer){$IFDEF OFFS}^{$ENDIF});
       Load   : @CDPlayer.TCDplayer.Load;
       Store  : @CDPlayer.TCDplayer.Store)
     ,(ObjType: otCDCounter;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(CDPlayer.TCounter){$IFDEF OFFS}^{$ENDIF});
       Load   : @CDPlayer.TCounter.Load;
       Store  : @CDPlayer.TCounter.Store )
{$ENDIF CDPlayer}
      { Collect }
     ,(ObjType: otCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Collect.TCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Collect.TCollection.Load;
       Store  : @Collect.TCollection.Store)
     ,(ObjType: otLineCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Collect.TLineCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Collect.TLineCollection.Load;
       Store  : @Collect.TLineCollection.Store)
     ,(ObjType: otStringCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Collect.TStringCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Collect.TStringCollection.Load;
       Store  : @Collect.TStringCollection.Store)
     ,(ObjType: otStrCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Collect.TStrCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Collect.TStrCollection.Load;
       Store  : @Collect.TStrCollection.Store)
     ,(ObjType: otStringList;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Collect.TStringList){$IFDEF OFFS}^{$ENDIF});
       Load   : @Collect.TStringList.Load;
       Store  : nil),
{$ENDIF !RCP}
{$IFDEF CHCOL}
      { ColorSel }
      (ObjType: otColorSelector;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TColorSelector){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TColorSelector.Load;
       Store  : @ColorSel.TColorSelector.Store)
     ,(ObjType: otMonoSelector;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TMonoSelector){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TMonoSelector.Load;
       Store  : @ColorSel.TMonoSelector.Store)
     ,(ObjType: otColorDisplay;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TColorDisplay){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TColorDisplay.Load;
       Store  : @ColorSel.TColorDisplay.Store)
     ,(ObjType: otColorGroupList;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TColorGroupList){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TColorGroupList.Load;
       Store  : @ColorSel.TColorGroupList.Store)
     ,(ObjType: otColorItemList;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TColorItemList){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TColorItemList.Load;
       Store  : @ColorSel.TColorItemList.Store)
     ,(ObjType: otColorDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.TColorDialog){$IFDEF OFFS}^{$ENDIF});
       Load   : @ColorSel.TColorDialog.Load;
       Store  : @ColorSel.TColorDialog.Store)
     ,(ObjType: otR_BWSelector;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ColorSel.T_BWSelector){$IFDEF OFFS}^{$ENDIF});
       Load:    @ColorSel.T_BWSelector.Load;
       Store:   @ColorSel.T_BWSelector.Store),
{$ENDIF CHCOL}
{$IFNDEF RCP}
{$IFDEF DBView}
      { DBView }
      (ObjType: otDBWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DBView.TDBWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @DBView.TDBWindow.Load;
       Store  : @DBView.TDBWindow.Store)
     ,(ObjType: otDBViewer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DBView.TDBViewer){$IFDEF OFFS}^{$ENDIF});
       Load   : @DBView.TDBViewer.Load;
       Store  : @DBView.TDBViewer.Store)
     ,(ObjType: otDBIndicator;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DBView.TDBIndicator){$IFDEF OFFS}^{$ENDIF});
       Load   : @DBView.TDBIndicator.Load;
       Store  : @DBView.TDBIndicator.Store)
     ,(ObjType: otFieldListBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DBView.TFieldListBox){$IFDEF OFFS}^{$ENDIF});
       Load   : @DBView.TFieldListBox.Load;
       Store  : @DBView.TFieldListBox.Store),
{$ENDIF DBView}
{$ENDIF !RCP}
      { Dialogs }
      (ObjType: otDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TDialog){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TDialog.Load;
       Store  : @Dialogs.TDialog.Store)
     ,(ObjType: otInputLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TInputLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TInputLine.Load;
       Store  : @Dialogs.TInputLine.Store)
     ,(ObjType: otHexLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.THexLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.THexLine.Load;
       Store  : @Dialogs.THexLine.Store)
     ,(ObjType: otButton;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TButton){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TButton.Load;
       Store  : @Dialogs.TButton.Store)
     ,(ObjType: otCluster;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TCluster){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TCluster.Load;
       Store  : @Dialogs.TCluster.Store)
     ,(ObjType: otRadioButtons;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TRadioButtons){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TRadioButtons.Load;
       Store  : @Dialogs.TRadioButtons.Store)
     ,(ObjType: otCheckBoxes;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TCheckBoxes){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TCheckBoxes.Load;
       Store  : @Dialogs.TCheckBoxes.Store)
     ,(ObjType: otMultiCheckBoxes;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TMultiCheckBoxes){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TMultiCheckBoxes.Load;
       Store  : @Dialogs.TMultiCheckBoxes.Store)
     ,(ObjType: otListBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TListBox){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TListBox.Load;
       Store  : @Dialogs.TListBox.Store)
     ,(ObjType: otStaticText;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TStaticText){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TStaticText.Load;
       Store  : @Dialogs.TStaticText.Store)
     ,(ObjType: otLabel;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TLabel){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TLabel.Load;
       Store  : @Dialogs.TLabel.Store)
     ,(ObjType: otHistory;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.THistory){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.THistory.Load;
       Store  : @Dialogs.THistory.Store)
     ,(ObjType: otParamText;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Dialogs.TParamText){$IFDEF OFFS}^{$ENDIF});
       Load   : @Dialogs.TParamText.Load;
       Store  : @Dialogs.TParamText.Store)
{$IFNDEF RCP}
      { DiskInfo }
     ,(ObjType: otDiskInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DiskInfo.TDiskInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @DiskInfo.TDiskInfo.Load;
       Store  : @DiskInfo.TDiskInfo.Store)
      { DnApp }
     ,(ObjType: otBackground;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnApp.TBackground){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnApp.TBackground.Load;
       Store  : @DnApp.TBackground.Store)
     ,(ObjType: otDesktop;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnApp.TDesktop){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnApp.TDesktop.Load;
       Store  : @DnApp.TDesktop.Store)
     ,(ObjType: otStringCache;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnApp.TCacheCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnApp.TCacheCollection.Load;
       Store  : @DnApp.TCacheCollection.Store)
      { DnStdDlg }
     ,(ObjType: otFileInputLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileInputLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnStdDlg.TFileInputLine.Load;
       Store  : @DnStdDlg.TFileInputLine.Store)
     ,(ObjType: otFileCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnStdDlg.TFileCollection.Load;
       Store  : @DnStdDlg.TFileCollection.Store)
     ,(ObjType: otFileList;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileList){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnStdDlg.TFileList.Load;
       Store  : @DnStdDlg.TFileList.Store)
     ,(ObjType: otFileInfoPane;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileInfoPane){$IFDEF OFFS}^{$ENDIF});
       Load   : @DnStdDlg.TFileInfoPane.Load;
       Store  : @DnStdDlg.TFileInfoPane.Store)
     ,(ObjType: otFileDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileDialog){$IFDEF OFFS}^{$ENDIF});
       Load:    @DnStdDlg.TFileDialog.Load;
       Store:   @DnStdDlg.TFileDialog.Store)
     ,(ObjType: otSortedListBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DnStdDlg.TSortedListBox){$IFDEF OFFS}^{$ENDIF});
       Load:    @DnStdDlg.TSortedListBox.Load;
       Store:   @DnStdDlg.TSortedListBox.Store)
      { DNUtil }
     ,(ObjType: otDataSaver;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(DNUtil.TDataSaver){$IFDEF OFFS}^{$ENDIF});
       Load   : @DNUtil.TDataSaver.Load;
       Store  : @DNUtil.TDataSaver.Store)
      { Drives }
     ,(ObjType: otDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Drives.TDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @Drives.TDrive.Load;
       Store  : @Drives.TDrive.Store)
      { Ed2 }
     ,(ObjType: otInfoLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Ed2.TInfoLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @Ed2.TInfoLine.Load;
       Store  : @Ed2.TInfoLine.Store)
     ,(ObjType: otBookLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Ed2.TBookmarkLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @Ed2.TBookmarkLine.Load;
       Store  : @Ed2.TBookmarkLine.Store)
      { Editor }
     ,(ObjType: otXFileEditor;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Editor.TXFileEditor){$IFDEF OFFS}^{$ENDIF});
       Load   : @Editor.TXFileEditor.Load;
       Store  : @Editor.TXFileEditor.Store)
      { FileFind }
     ,(ObjType: otFindDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FileFind.TFindDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @FileFind.TFindDrive.Load;
       Store  : @FileFind.TFindDrive.Store)
     ,(ObjType: otTempDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FileFind.TTempDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @FileFind.TTempDrive.Load;
       Store  : @FileFind.TTempDrive.Store)
      { FilesCol }
     ,(ObjType: otFilesCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FilesCol.TFilesCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @FilesCol.TFilesCollection.Load;
       Store  : @FilesCol.TFilesCollection.Store)
      { FlPanel }
     ,(ObjType: otFilePanel;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TFilePanel){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TFilePanel.Load;
       Store  : @FlPanel.TFilePanel.Store)
     ,(ObjType: otFlPInfoView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TInfoView){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TInfoView.Load;
       Store  : @FlPanel.TInfoView.Store)
     ,(ObjType: otTopView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TTopView){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TTopView.Load;
       Store  : @FlPanel.TTopView.Store)
     ,(ObjType: otSeparator;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TSeparator){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TSeparator.Load;
       Store  : @FlPanel.TSeparator.Store)
     ,(ObjType: otSpecScroll;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TSpecScroll){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TSpecScroll.Load;
       Store  : @FlPanel.TSpecScroll.Store)
     ,(ObjType: otDriveLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FlPanel.TDriveLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @FlPanel.TDriveLine.Load;
       Store  : @FlPanel.TDriveLine.Store)
      { FStorage }
     ,(ObjType: otDirStorage;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FStorage.TDirStorage){$IFDEF OFFS}^{$ENDIF});
       Load   : @FStorage.TDirStorage.Load;
       Store  : @FStorage.TDirStorage.Store)
      { FViewer }
     ,(ObjType: otFileViewer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FViewer.TFileViewer){$IFDEF OFFS}^{$ENDIF});
       Load   : @FViewer.TFileViewer.Load;
       Store  : @FViewer.TFileViewer.Store)
     ,(ObjType: otFileWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FViewer.TFileWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @FViewer.TFileWindow.Load;
       Store  : @FViewer.TFileWindow.Store)
     ,(ObjType: otViewScroll;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FViewer.TViewScroll){$IFDEF OFFS}^{$ENDIF});
       Load   : @FViewer.TViewScroll.Load;
       Store  : @FViewer.TViewScroll.Store)
     ,(ObjType: otHFileViewer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FViewer.THFileViewer){$IFDEF OFFS}^{$ENDIF});
       Load   : @FViewer.THFileViewer.Load;
       Store  : @FViewer.THFileViewer.Store)
     ,(ObjType: otViewInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(FViewer.TViewInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @FViewer.TViewInfo.Load;
       Store  : @FViewer.TViewInfo.Store)
      { Gauges }
{$IFDEF TrashCan}
     ,(ObjType: otTrashCan;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Gauges.TTrashCan){$IFDEF OFFS}^{$ENDIF});
       Load   : @Gauges.TTrashCan.Load;
       Store  : @Gauges.TTrashCan.Store)
{$ENDIF TrashCan}
     ,(ObjType: otKeyMacros;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Gauges.TKeyMacros){$IFDEF OFFS}^{$ENDIF});
       Load   : @Gauges.TKeyMacros.Load;
       Store  : @Gauges.TKeyMacros.Store)
      { HelpKern }
      ,(ObjType: otHelpTopic;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(HelpKern.THelpTopic){$IFDEF OFFS}^{$ENDIF});
       Load:    @HelpKern.THelpTopic.Load;
       Store:   @HelpKern.THelpTopic.Store),
      (ObjType: otHelpIndex;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(HelpKern.THelpIndex){$IFDEF OFFS}^{$ENDIF});
       Load:    @HelpKern.THelpIndex.Load;
       Store:   @HelpKern.THelpIndex.Store)
      { Histries }
     ,(ObjType: otEditHistoryCol;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Histries.TEditHistoryCol){$IFDEF OFFS}^{$ENDIF});
       Load   : @Histries.TEditHistoryCol.Load;
       Store  : @Histries.TEditHistoryCol.Store)
     ,(ObjType: otViewHistoryCol;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Histries.TViewHistoryCol){$IFDEF OFFS}^{$ENDIF});
       Load   : @Histries.TViewHistoryCol.Load;
       Store  : @Histries.TViewHistoryCol.Store)
      { Menus }
{$ENDIF !RCP}
     ,(ObjType: otMenuBar;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Menus.TMenuBar){$IFDEF OFFS}^{$ENDIF});
       Load   : @Menus.TMenuBar.Load;
       Store  : @Menus.TMenuBar.Store)
     ,(ObjType: otMenuBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Menus.TMenuBox){$IFDEF OFFS}^{$ENDIF});
       Load   : @Menus.TMenuBox.Load;
       Store  : @Menus.TMenuBox.Store)
     ,(ObjType: otStatusLine;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Menus.TStatusLine){$IFDEF OFFS}^{$ENDIF});
       Load   : @Menus.TStatusLine.Load;
       Store  : @Menus.TStatusLine.Store)
     ,(ObjType: otMenuPopup;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Menus.TMenuPopup){$IFDEF OFFS}^{$ENDIF});
       Load   : @Menus.TMenuPopup.Load;
       Store  : @Menus.TMenuPopup.Store)
{$IFNDEF RCP}
      { Microed }
     ,(ObjType: otFileEditor;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Microed.TFileEditor){$IFDEF OFFS}^{$ENDIF});
       Load   : @Microed.TFileEditor.Load;
       Store  : @Microed.TFileEditor.Store)
     ,(ObjType: otEditWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(EdWin.TEditWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @EdWin.TEditWindow.Load;
       Store  : @EdWin.TEditWindow.Store)
{$IFDEF MODEM}
{$IFDEF LINK}
      { NavyLink }
     ,(ObjType: otLinker;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(NavyLink.TLinker){$IFDEF OFFS}^{$ENDIF});
       Load   : @NavyLink.TLinker.Load;
       Store  : @NavyLink.TLinker.Store)
     ,(ObjType: otLinkDrive;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(NavyLink.TLinkDrive){$IFDEF OFFS}^{$ENDIF});
       Load   : @NavyLink.TLinkDrive.Load;
       Store  : @NavyLink.TLinkDrive.Store)
{$ENDIF LINK IN MODEM}
      { Dialer }
     ,(ObjType: otAutoDialer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(uDialer.TAutoDialer){$IFDEF OFFS}^{$ENDIF});
       Load   : @uDialer.TAutoDialer.Load;
       Store  : @uDialer.TAutoDialer.Store)
     ,(ObjType: otDialBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(uDialer.TDialBox){$IFDEF OFFS}^{$ENDIF});
       Load   : @uDialer.TDialBox.Load;
       Store  : @uDialer.TDialBox.Store)
      { ScrollBk }
     ,(ObjType: otScrollBackWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ScrollBk.TScrollBackWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @ScrollBk.TScrollBackWindow.Load;
       Store  : @ScrollBk.TScrollBackWindow.Store)
     ,(ObjType: otScrollBack;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(ScrollBk.TScrollBack){$IFDEF OFFS}^{$ENDIF});
       Load   : @ScrollBk.TScrollBack.Load;
       Store  : @ScrollBk.TScrollBack.Store)
{$ENDIF MODEM}
{$IFDEF NETINFO}
      { NetInfo }
     ,(ObjType: otNetInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(NetInfo.TNetInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @NetInfo.TNetInfo.Load;
       Store  : @NetInfo.TNetInfo.Store)
{$ENDIF NETINFO}
{$IFDEF PHONES}
     ,(ObjType: otDStringView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(StrView.TDStringView){$IFDEF OFFS}^{$ENDIF});
       Load   : @StrView.TDStringView.Load;
       Store  : @StrView.TDStringView.Store)
     ,(ObjType: otPhone;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Phones.TPhone){$IFDEF OFFS}^{$ENDIF});
       Load   : @Phones.TPhone.Load;
       Store  : @Phones.TPhone.Store)
     ,(ObjType: otPhoneDir;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Phones.TPhoneDir){$IFDEF OFFS}^{$ENDIF});
       Load   : @Phones.TPhoneDir.Load;
       Store  : @Phones.TPhoneDir.Store)
     ,(ObjType: otPhoneCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Phones.TPhoneCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Phones.TPhoneCollection.Load;
       Store  : @Phones.TPhoneCollection.Store)
{$ENDIF PHONES}
{$IFDEF PrintManager}
      { PrintManager }
     ,(ObjType: otStringCol;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(PrintMan.TStringCol){$IFDEF OFFS}^{$ENDIF});
       Load   : @PrintMan.TStringCol.Load;
       Store  : @PrintMan.TStringCol.Store)
     ,(ObjType: otPrintManager;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(PrintMan.TPrintManager){$IFDEF OFFS}^{$ENDIF});
       Load   : @PrintMan.TPrintManager.Load;
       Store  : @PrintMan.TPrintManager.Store)
     ,(ObjType: otPrintStatus;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(PrintMan.TPrintStatus){$IFDEF OFFS}^{$ENDIF});
       Load   : @PrintMan.TPrintStatus.Load;
       Store  : @PrintMan.TPrintStatus.Store)
     ,(ObjType: otPMWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(PrintMan.TPMWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @PrintMan.TPMWindow.Load;
       Store  : @PrintMan.TPMWindow.Store)
{$ENDIF PrintManager}
{$ENDIF !RCP}
      { Scroller }
     ,(ObjType: otScroller;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Scroller.TScroller){$IFDEF OFFS}^{$ENDIF});
       Load   : @Scroller.TScroller.Load;
       Store  : @Scroller.TScroller.Store)
     ,(ObjType: otListViewer;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Scroller.TListViewer){$IFDEF OFFS}^{$ENDIF});
       Load   : @Scroller.TListViewer.Load;
       Store  : @Scroller.TLIstViewer.Store)
      { Setups }
     ,(ObjType: otSysDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TSysDialog){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TSysDialog.Load;
       Store  : @Setups.TSysDialog.Store)
     ,(ObjType: otCurrDriveInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TCurrDriveInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TCurrDriveInfo.Load;
       Store  : @Setups.TCurrDriveInfo.Store)
     ,(ObjType: otMouseBar;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TMouseBar){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TMouseBar.Load;
       Store  : @Setups.TMouseBar.Store)
{$IFDEF SS}
     ,(ObjType: otSaversDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TSaversDialog){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TSaversDialog.Load;
       Store  : @Setups.TSaversDialog.Store)
     ,(ObjType: otSaversListBox;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TSaversListBox){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TSaversListBox.Load;
       Store  : @Setups.TSaversListBox.Store)
{$ENDIF SS}
     ,(ObjType: otUpperTable;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Setups.TUpperTable){$IFDEF OFFS}^{$ENDIF});
       Load   : @Setups.TUpperTable.Load;
       Store  : @Setups.TUpperTable.Store)
{$IFNDEF RCP}
      { Startup }
     ,(ObjType: otTextCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Startup.TTextCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Startup.TTextCollection.Load;
       Store  : @Startup.TTextCollection.Store)
      { Terminal }
{$IFDEF Modem}
     ,(ObjType: otTerminalWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Terminal.TTerminalWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Terminal.TTerminalWindow.Load;
       Store  : @Terminal.TTerminalWindow.Store)
     ,(ObjType: otTerminal;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Terminal.TTerminal){$IFDEF OFFS}^{$ENDIF});
       Load   : @Terminal.TTerminal.Load;
       Store  : @Terminal.TTerminal.Store)
     ,(ObjType: otPortInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Terminal.TPortInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @Terminal.TPortInfo.Load;
       Store  : @Terminal.TPortInfo.Store)
{$ENDIF Modem}
{$IFDEF Game}
      { Tetris }
     ,(ObjType: otGameWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tetris.TGameWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tetris.TGameWindow.Load;
       Store  : @Tetris.TGameWindow.Store)
     ,(ObjType: otGameView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tetris.TGameView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tetris.TGameView.Load;
       Store  : @Tetris.TGameView.Store)
     ,(ObjType: otGameInfo;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tetris.TGameInfo){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tetris.TGameInfo.Load;
       Store  : @Tetris.TGameInfo.Store)
{$ENDIF Game}
      { Tree }
     ,(ObjType: otTreeView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreeView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreeView.Load;
       Store  : @Tree.TTreeView.Store)
     ,(ObjType: otTreeReader;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreeReader){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreeReader.Load;
       Store  : @Tree.TTreeReader.Store)
     ,(ObjType: otTreeWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreeWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreeWindow.Load;
       Store  : @Tree.TTreeWindow.Store)
     ,(ObjType: otTreePanel;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreePanel){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreePanel.Load;
       Store  : @Tree.TTreePanel.Store)
     ,(ObjType: otTreeDialog;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreeDialog){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreeDialog.Load;
       Store  : @Tree.TTreeDialog.Store)
     ,(ObjType: otTreeInfoView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TTreeInfoView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TTreeInfoView.Load;
       Store  : @Tree.TTreeInfoView.Store)
     ,(ObjType: otHTreeView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.THTreeView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.THTreeView.Load;
       Store  : @Tree.THTreeView.Store)
     ,(ObjType: otDirCollection;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Tree.TDirCollection){$IFDEF OFFS}^{$ENDIF});
       Load   : @Tree.TDirCollection.Load;
       Store  : @Tree.TDirCollection.Store)
      { UniWin }
     ,(ObjType: otEditScrollBar;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(UniWin.TEditScrollBar){$IFDEF OFFS}^{$ENDIF});
       Load   : @UniWin.TEditScrollBar.Load;
       Store  : @UniWin.TEditScrollBar.Store)
     ,(ObjType: otEditFrame;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(UniWin.TEditFrame){$IFDEF OFFS}^{$ENDIF});
       Load   : @UniWin.TEditFrame.Load;
       Store  : @UniWin.TEditFrame.Store)
      { UserMenu }
     ,(ObjType: otUserWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(UserMenu.TUserWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @UserMenu.TUserWindow.Load;
       Store  : @UserMenu.TUserWindow.Store)
     ,(ObjType: otUserView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(UserMenu.TUserView){$IFDEF OFFS}^{$ENDIF});
       Load   : @UserMenu.TUserView.Load;
       Store  : @UserMenu.TUserView.Store)
      { Validate }
{$ENDIF !RCP}
     ,(ObjType: otFilterValidator;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Validate.TFilterValidator){$IFDEF OFFS}^{$ENDIF});
       Load   : @Validate.TFilterValidator.Load;
       Store  : @Validate.TFilterValidator.Store)
     ,(ObjType: otRangeValidator;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Validate.TRangeValidator){$IFDEF OFFS}^{$ENDIF});
       Load   : @Validate.TRangeValidator.Load;
       Store  : @Validate.TRangeValidator.Store)
      { Views }
     ,(ObjType: otView;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TView){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TView.Load;
       Store  : @Views.TView.Store)
     ,(ObjType: otFrame;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TFrame){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TFrame.Load;
       Store  : @Views.TFrame.Store)
     ,(ObjType: otScrollBar;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TScrollBar){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TScrollBar.Load;
       Store  : @Views.TScrollBar.Store)
     ,(ObjType: otGroup;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TGroup){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TGroup.Load;
       Store  : @Views.TGroup.Store)
     ,(ObjType: otWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TWindow.Load;
       Store  : @Views.TWindow.Store)
{$IFNDEF RCP}
     ,(ObjType: otMyScrollBar;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(Views.TMyScrollBar){$IFDEF OFFS}^{$ENDIF});
       Load   : @Views.TMyScrollBar.Load;
       Store  : @Views.TMyScrollBar.Store)
      { XDblWnd }
     ,(ObjType: otDoubleWindow;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(XDblWnd.TXDoubleWindow){$IFDEF OFFS}^{$ENDIF});
       Load   : @XDblWnd.TXDoubleWindow.Load;
       Store  : @XDblWnd.TXDoubleWindow.Store)
{$ENDIF !RCP}
     ,(ObjType: otColorPoint;
       VmtLink: {$IFDEF OFFS}Ofs{$ENDIF}(TypeOf(SWE.TColorPoint){$IFDEF OFFS}^{$ENDIF});
       Load   : @SWE.TColorPoint.Load;
       Store  : @SWE.TColorPoint.Store)
  );

procedure RegisterAll;
var
  I: Integer;
begin
  for I := 1 to NumRElms do RegisterType(RegArray[I]);
end;

END.
