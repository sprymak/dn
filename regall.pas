{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.07/DOS
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

UNIT RegAll;

INTERFACE
{$i stdefine.inc}

procedure RegisterAll;

IMPLEMENTATION

Uses
{$IFNDEF RCP}
     Arc_ZIP,  Arc_LHA,  Arc_RAR,  Arc_ACE,  Arc_HA,   Arc_CAB,
{$IFNDEF MINARCH}
     Arc_ARC,  Arc_BSA,  Arc_BS2,  Arc_HYP,  Arc_LIM,  Arc_HPK,  Arc_TAR,
     Arc_TGZ,  Arc_ZXZ,  Arc_QRK,  Arc_AIN,  Arc_CHZ,  Arc_HAP,  Arc_IS3,
     Arc_SQZ,  Arc_UC2,  Arc_UFA,  Arc_ZOO,
{$ENDIF}
     Archiver, Arcview,  Arvid,    Asciitab, Ccalc,
     Collect,  Diskinfo, Dnapp,    Dnstddlg, Dnutil,   Drives,   Ed2,
     Editor,   Filefind, Filescol, Flpanel,  Fstorage, Fviewer,  Gauges,
     Histries, Microed,  Startup,  Tree,     Uniwin,   Usermenu, Xdblwnd,
     Helpkernel,
{$IFDEF SpreadSheet}     Calc,     Cellscol,           {$ENDIF}
{$IFDEF Calendar}        Calendar,                     {$ENDIF}
{$IFDEF CDPlayer}        Cdplayer,                     {$ENDIF}
{$IFDEF DBView}          Dbview,                       {$ENDIF}
{$IFDEF MODEM}           Scrollbk, Terminal, uDialer,
           {$IFDEF LINK} Navylink, {$ENDIF}            {$ENDIF}
{$IFDEF PrintManager}    Printmanager,                 {$ENDIF}
{$IFDEF Game}            Tetris,                       {$ENDIF}
{$IFDEF NetInfo}         Netinfo,                      {$ENDIF}
{$IFDEF PHONES}          Phones,                       {$ENDIF}
{$ENDIF !RCP}
{$IFDEF CHCOL}           Colorsel,                     {$ENDIF}
     Dialogs,  Menus,    Objects,  ObjType,  Scroller, Setups,
     Validate, Views;

const
  NumRElms =
{$IFDEF RCP}31{$ELSE} 122
  {$IFDEF MODEM}       +7 {$IFDEF LINK} +2 {$ENDIF} {$ENDIF}
  {$IFDEF SpreadSheet} +5 {$ENDIF}
  {$IFDEF Game}        +3 {$ENDIF}
  {$IFDEF PrintManager}+4 {$ENDIF}
  {$IFDEF CDPlayer}    +2 {$ENDIF}
  {$IFDEF DBView}      +4 {$ENDIF}
  {$IFDEF Calendar}    +2 {$ENDIF}   {JO}
  {$IFDEF TrashCan}    +1 {$ENDIF}
  {$IFDEF NETINFO}     +1 {$ENDIF}
  {$IFDEF MINARCH}    -18 {$ENDIF}
  {$IFDEF PHONES}      +4 {$ENDIF}
{$ENDIF}
  {$IFDEF SS}          +2 {$ENDIF}
  {$IFDEF CHCOL}       +7 {$ENDIF}
  ;
  RegArray : array[1..NumRElms] of TStreamRec = (
{$IFNDEF RCP}
      { Arc_ZIP }
      (ObjType: otZIPArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_ZIP.TZIPArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_ZIP.TZIPArchive.Load;
       Store  : @Arc_ZIP.TZIPArchive.Store)
      { Arc_LHA }
     ,(ObjType: otLHAArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_LHA.TLHAArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_LHA.TLHAArchive.Load;
       Store  : @Arc_LHA.TLHAArchive.Store)
      { Arc_RAR }
     ,(ObjType: otRARArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_RAR.TRARArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_RAR.TRARArchive.Load;
       Store  : @Arc_RAR.TRARArchive.Store)
      { Arc_CAB }
     ,(ObjType: otCABArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_CAB.TCABArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_CAB.TCABArchive.Load;
       Store  : @Arc_CAB.TCABArchive.Store)
      { Arc_ACE }
     ,(ObjType: otACEArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_ACE.TACEArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_ACE.TACEArchive.Load;
       Store  : @Arc_ACE.TACEArchive.Store)
      { Arc_HA }
     ,(ObjType: otHAArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_HA.THAArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_HA.THAArchive.Load;
       Store  : @Arc_HA.THAArchive.Store)
{$IFNDEF MINARCH}
      { Arc_arc }
     ,(ObjType: otARCArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_arc.TARCArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_arc.TARCArchive.Load;
       Store  : @Arc_arc.TARCArchive.Store)
      { Arc_bsa }
     ,(ObjType: otBSAArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_bsa.TBSAArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_bsa.TBSAArchive.Load;
       Store  : @Arc_bsa.TBSAArchive.Store)
      { Arc_bs2 }
     ,(ObjType: otBS2Archiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_bs2.TBS2Archive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_bs2.TBS2Archive.Load;
       Store  : @Arc_bs2.TBS2Archive.Store)
      { Arc_hyp }
     ,(ObjType: otHYPArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_hyp.THYPArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_hyp.THYPArchive.Load;
       Store  : @Arc_hyp.THYPArchive.Store)
      { Arc_lim }
     ,(ObjType: otLIMArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_lim.TLIMArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_lim.TLIMArchive.Load;
       Store  : @Arc_lim.TLIMArchive.Store)
      { Arc_hpk }
     ,(ObjType: otHPKArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_hpk.THPKArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_Hpk.THPKArchive.Load;
       Store  : @Arc_hpk.THPKArchive.Store)
      { Arc_TAR }
     ,(ObjType: otTARArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_TAR.TTARArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_TAR.TTARArchive.Load;
       Store  : @Arc_TAR.TTARArchive.Store)
      { Arc_TGZ }
     ,(ObjType: otTGZArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_TGZ.TTGZArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_TGZ.TTGZArchive.Load;
       Store  : @Arc_TGZ.TTGZArchive.Store)
      { Arc_ZXZ }
     ,(ObjType: otZXZArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_ZXZ.TZXZArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_ZXZ.TZXZArchive.Load;
       Store  : @Arc_ZXZ.TZXZArchive.Store)
      { Arc_QRK }
     ,(ObjType: otQuArkArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_QRK.TQuArkArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_QRK.TQuArkArchive.Load;
       Store  : @Arc_QRK.TQuArkArchive.Store)
      { Arc_UFA }
     ,(ObjType: otUFAArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_UFA.TUFAArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_UFA.TUFAArchive.Load;
       Store  : @Arc_UFA.TUFAArchive.Store)
      { Arc_IS3 }
     ,(ObjType: otIS3Archiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_IS3.TIS3Archive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_IS3.TIS3Archive.Load;
       Store  : @Arc_IS3.TIS3Archive.Store)
      { Arc_SQZ }
     ,(ObjType: otSQZArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_SQZ.TSQZArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_SQZ.TSQZArchive.Load;
       Store  : @Arc_SQZ.TSQZArchive.Store)
      { Arc_HAP }
     ,(ObjType: otHAPArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_HAP.THAPArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_HAP.THAPArchive.Load;
       Store  : @Arc_HAP.THAPArchive.Store)
      { Arc_ZOO }
     ,(ObjType: otZOOArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_ZOO.TZOOArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_ZOO.TZOOArchive.Load;
       Store  : @Arc_ZOO.TZOOArchive.Store)
      { Arc_CHZ }
     ,(ObjType: otCHZArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_CHZ.TCHZArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_CHZ.TCHZArchive.Load;
       Store  : @Arc_CHZ.TCHZArchive.Store)
      { Arc_UC2 }
     ,(ObjType: otUC2Archiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_UC2.TUC2Archive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_UC2.TUC2Archive.Load;
       Store  : @Arc_UC2.TUC2Archive.Store)
      { Arc_AIN }
     ,(ObjType: otAINArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arc_AIN.TAINArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arc_AIN.TAINArchive.Load;
       Store  : @Arc_AIN.TAINArchive.Store)
{$ENDIF MINARCH}
      { Archiver }
     ,(ObjType: otARJArchiver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Archiver.TARJArchive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Archiver.TARJArchive.Load;
       Store  : @Archiver.TARJArchive.Store)
     ,(ObjType: otFileInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Archiver.TFileInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Archiver.TFileInfo.Load;
       Store  : @Archiver.TFileInfo.Store)
     ,(ObjType: otUserSaver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Archiver.TUserSaver){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Archiver.TUserSaver.Load;
       Store  : @Archiver.TUserSaver.Store)
      { ArcView }
     ,(ObjType: otArcDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ArcView.TArcDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ArcView.TArcDrive.Load;
       Store  : @ArcView.TArcDrive.Store)
      { Arvid }
     ,(ObjType: otArvidDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Arvid.TArvidDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Arvid.TArvidDrive.Load;
       Store  : @Arvid.TArvidDrive.Store)
      { AsciiTab }
     ,(ObjType: otTable;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(AsciiTab.TTable){$IFNDEF DPMI}^{$ENDIF});
       Load   : @AsciiTab.TTable.Load;
       Store  : @AsciiTab.TTable.Store)
     ,(ObjType: otReport;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(AsciiTab.TReport){$IFNDEF DPMI}^{$ENDIF});
       Load   : @AsciiTab.TReport.Load;
       Store  : @AsciiTab.TReport.Store)
     ,(ObjType: otASCIIChart;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(AsciiTab.TASCIIChart){$IFNDEF DPMI}^{$ENDIF});
       Load   : @AsciiTab.TASCIIChart.Load;
       Store  : @AsciiTab.TASCIIChart.Store)
      { Calc }
{$IFDEF SpreadSheet}
     ,(ObjType: otCalcWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calc.TCalcWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calc.TCalcWindow.Load;
       Store  : @Calc.TCalcWindow.Store)
     ,(ObjType: otCalcView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calc.TCalcView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calc.TCalcView.Load;
       Store  : @Calc.TCalcView.Store)
     ,(ObjType: otCalcInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calc.TCalcInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calc.TCalcInfo.Load;
       Store  : @Calc.TCalcInfo.Store)
     ,(ObjType: otInfoView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calc.TInfoView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calc.TInfoView.Load;
       Store  : @Calc.TInfoView.Store)
      { CellsCol }
     ,(ObjType: otCellCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(CellsCol.TCellCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @CellsCol.TCellCollection.Load;
       Store  : @CellsCol.TCellCollection.Store)
{$ENDIF SpreadSheet}
{$IFDEF Calendar}
      { Calendar }
     ,(ObjType: otCalendarView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calendar.TCalendarView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calendar.TCalendarView.Load;
       Store  : @Calendar.TCalendarView.Store)
     ,(ObjType: otCalendarWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Calendar.TCalendarWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Calendar.TCalendarWindow.Load;
       Store  : @Calendar.TCalendarWindow.Store)
{$ENDIF Calendar}
      { CCalc }
     ,(ObjType: otCalcLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(CCalc.TCalcLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @CCalc.TCalcLine.Load;
       Store  : @CCalc.TCalcLine.Store)
     ,(ObjType: otIndicator;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(CCalc.TIndicator){$IFNDEF DPMI}^{$ENDIF});
       Load   : @CCalc.TIndicator.Load;
       Store  : @CCalc.TIndicator.Store)
{$IFDEF CDPlayer}
      { CDPlayer }
     ,(ObjType: otCdplayer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(CDPlayer.TCdPlayer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @CDPlayer.TCDplayer.Load;
       Store  : @CDPlayer.TCDplayer.Store)
     ,(ObjType: otCDCounter;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(CDPlayer.TCounter){$IFNDEF DPMI}^{$ENDIF});
       Load   : @CDPlayer.TCounter.Load;
       Store  : @CDPlayer.TCounter.Store )
{$ENDIF CDPlayer}
      { Collect }
     ,(ObjType: otCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Collect.TCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Collect.TCollection.Load;
       Store  : @Collect.TCollection.Store)
     ,(ObjType: otLineCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Collect.TLineCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Collect.TLineCollection.Load;
       Store  : @Collect.TLineCollection.Store)
     ,(ObjType: otStringCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Collect.TStringCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Collect.TStringCollection.Load;
       Store  : @Collect.TStringCollection.Store)
     ,(ObjType: otStrCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Collect.TStrCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Collect.TStrCollection.Load;
       Store  : @Collect.TStrCollection.Store)
     ,(ObjType: otStringList;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Collect.TStringList){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Collect.TStringList.Load;
       Store  : nil),
{$ENDIF !RCP}
{$IFDEF CHCOL}
      { ColorSel }
      (ObjType: otColorSelector;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TColorSelector){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TColorSelector.Load;
       Store  : @ColorSel.TColorSelector.Store)
     ,(ObjType: otMonoSelector;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TMonoSelector){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TMonoSelector.Load;
       Store  : @ColorSel.TMonoSelector.Store)
     ,(ObjType: otColorDisplay;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TColorDisplay){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TColorDisplay.Load;
       Store  : @ColorSel.TColorDisplay.Store)
     ,(ObjType: otColorGroupList;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TColorGroupList){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TColorGroupList.Load;
       Store  : @ColorSel.TColorGroupList.Store)
     ,(ObjType: otColorItemList;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TColorItemList){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TColorItemList.Load;
       Store  : @ColorSel.TColorItemList.Store)
     ,(ObjType: otColorDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.TColorDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ColorSel.TColorDialog.Load;
       Store  : @ColorSel.TColorDialog.Store)
     ,(ObjType: otR_BWSelector;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ColorSel.T_BWSelector){$IFNDEF DPMI}^{$ENDIF});
       Load:    @ColorSel.T_BWSelector.Load;
       Store:   @ColorSel.T_BWSelector.Store),
{$ENDIF CHCOL}
{$IFNDEF RCP}
{$IFDEF DBView}
      { DBView }
      (ObjType: otDBWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DBView.TDBWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DBView.TDBWindow.Load;
       Store  : @DBView.TDBWindow.Store)
     ,(ObjType: otDBViewer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DBView.TDBViewer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DBView.TDBViewer.Load;
       Store  : @DBView.TDBViewer.Store)
     ,(ObjType: otDBIndicator;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DBView.TDBIndicator){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DBView.TDBIndicator.Load;
       Store  : @DBView.TDBIndicator.Store)
     ,(ObjType: otFieldListBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DBView.TFieldListBox){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DBView.TFieldListBox.Load;
       Store  : @DBView.TFieldListBox.Store),
{$ENDIF DBView}
{$ENDIF !RCP}
      { Dialogs }
      (ObjType: otDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TDialog.Load;
       Store  : @Dialogs.TDialog.Store)
     ,(ObjType: otInputLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TInputLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TInputLine.Load;
       Store  : @Dialogs.TInputLine.Store)
     ,(ObjType: otHexLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.THexLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.THexLine.Load;
       Store  : @Dialogs.THexLine.Store)
     ,(ObjType: otButton;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TButton){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TButton.Load;
       Store  : @Dialogs.TButton.Store)
     ,(ObjType: otCluster;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TCluster){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TCluster.Load;
       Store  : @Dialogs.TCluster.Store)
     ,(ObjType: otRadioButtons;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TRadioButtons){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TRadioButtons.Load;
       Store  : @Dialogs.TRadioButtons.Store)
     ,(ObjType: otCheckBoxes;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TCheckBoxes){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TCheckBoxes.Load;
       Store  : @Dialogs.TCheckBoxes.Store)
     ,(ObjType: otMultiCheckBoxes;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TMultiCheckBoxes){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TMultiCheckBoxes.Load;
       Store  : @Dialogs.TMultiCheckBoxes.Store)
     ,(ObjType: otListBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TListBox){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TListBox.Load;
       Store  : @Dialogs.TListBox.Store)
     ,(ObjType: otStaticText;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TStaticText){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TStaticText.Load;
       Store  : @Dialogs.TStaticText.Store)
     ,(ObjType: otLabel;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TLabel){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TLabel.Load;
       Store  : @Dialogs.TLabel.Store)
     ,(ObjType: otHistory;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.THistory){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.THistory.Load;
       Store  : @Dialogs.THistory.Store)
     ,(ObjType: otParamText;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Dialogs.TParamText){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Dialogs.TParamText.Load;
       Store  : @Dialogs.TParamText.Store)
{$IFNDEF RCP}
      { DiskInfo }
     ,(ObjType: otDiskInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DiskInfo.TDiskInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DiskInfo.TDiskInfo.Load;
       Store  : @DiskInfo.TDiskInfo.Store)
      { DnApp }
     ,(ObjType: otBackground;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnApp.TBackground){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnApp.TBackground.Load;
       Store  : @DnApp.TBackground.Store)
     ,(ObjType: otDesktop;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnApp.TDesktop){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnApp.TDesktop.Load;
       Store  : @DnApp.TDesktop.Store)
     ,(ObjType: otStringCache;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnApp.TCacheCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnApp.TCacheCollection.Load;
       Store  : @DnApp.TCacheCollection.Store)
      { DnStdDlg }
     ,(ObjType: otFileInputLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileInputLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnStdDlg.TFileInputLine.Load;
       Store  : @DnStdDlg.TFileInputLine.Store)
     ,(ObjType: otFileCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnStdDlg.TFileCollection.Load;
       Store  : @DnStdDlg.TFileCollection.Store)
     ,(ObjType: otFileList;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileList){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnStdDlg.TFileList.Load;
       Store  : @DnStdDlg.TFileList.Store)
     ,(ObjType: otFileInfoPane;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileInfoPane){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DnStdDlg.TFileInfoPane.Load;
       Store  : @DnStdDlg.TFileInfoPane.Store)
     ,(ObjType: otFileDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TFileDialog){$IFNDEF DPMI}^{$ENDIF});
       Load:    @DnStdDlg.TFileDialog.Load;
       Store:   @DnStdDlg.TFileDialog.Store)
     ,(ObjType: otSortedListBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DnStdDlg.TSortedListBox){$IFNDEF DPMI}^{$ENDIF});
       Load:    @DnStdDlg.TSortedListBox.Load;
       Store:   @DnStdDlg.TSortedListBox.Store)
      { DNUtil }
     ,(ObjType: otDataSaver;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(DNUtil.TDataSaver){$IFNDEF DPMI}^{$ENDIF});
       Load   : @DNUtil.TDataSaver.Load;
       Store  : @DNUtil.TDataSaver.Store)
      { Drives }
     ,(ObjType: otDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Drives.TDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Drives.TDrive.Load;
       Store  : @Drives.TDrive.Store)
      { Ed2 }
     ,(ObjType: otInfoLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Ed2.TInfoLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Ed2.TInfoLine.Load;
       Store  : @Ed2.TInfoLine.Store)
     ,(ObjType: otBookLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Ed2.TBookmarkLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Ed2.TBookmarkLine.Load;
       Store  : @Ed2.TBookmarkLine.Store)
      { Editor }
     ,(ObjType: otXFileEditor;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Editor.TXFileEditor){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Editor.TXFileEditor.Load;
       Store  : @Editor.TXFileEditor.Store)
      { FileFind }
     ,(ObjType: otFindDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FileFind.TFindDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FileFind.TFindDrive.Load;
       Store  : @FileFind.TFindDrive.Store)
     ,(ObjType: otTempDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FileFind.TTempDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FileFind.TTempDrive.Load;
       Store  : @FileFind.TTempDrive.Store)
      { FilesCol }
     ,(ObjType: otFilesCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FilesCol.TFilesCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FilesCol.TFilesCollection.Load;
       Store  : @FilesCol.TFilesCollection.Store)
      { FlPanel }
     ,(ObjType: otFilePanel;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TFilePanel){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TFilePanel.Load;
       Store  : @FlPanel.TFilePanel.Store)
     ,(ObjType: otFlPInfoView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TInfoView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TInfoView.Load;
       Store  : @FlPanel.TInfoView.Store)
     ,(ObjType: otTopView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TTopView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TTopView.Load;
       Store  : @FlPanel.TTopView.Store)
     ,(ObjType: otSeparator;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TSeparator){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TSeparator.Load;
       Store  : @FlPanel.TSeparator.Store)
     ,(ObjType: otSpecScroll;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TSpecScroll){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TSpecScroll.Load;
       Store  : @FlPanel.TSpecScroll.Store)
     ,(ObjType: otDriveLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FlPanel.TDriveLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FlPanel.TDriveLine.Load;
       Store  : @FlPanel.TDriveLine.Store)
      { FStorage }
     ,(ObjType: otDirStorage;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FStorage.TDirStorage){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FStorage.TDirStorage.Load;
       Store  : @FStorage.TDirStorage.Store)
      { FViewer }
     ,(ObjType: otFileViewer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FViewer.TFileViewer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FViewer.TFileViewer.Load;
       Store  : @FViewer.TFileViewer.Store)
     ,(ObjType: otFileWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FViewer.TFileWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FViewer.TFileWindow.Load;
       Store  : @FViewer.TFileWindow.Store)
     ,(ObjType: otViewScroll;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FViewer.TViewScroll){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FViewer.TViewScroll.Load;
       Store  : @FViewer.TViewScroll.Store)
     ,(ObjType: otHFileViewer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FViewer.THFileViewer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FViewer.THFileViewer.Load;
       Store  : @FViewer.THFileViewer.Store)
     ,(ObjType: otViewInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(FViewer.TViewInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @FViewer.TViewInfo.Load;
       Store  : @FViewer.TViewInfo.Store)
      { Gauges }
{$IFDEF TrashCan}
     ,(ObjType: otTrashCan;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Gauges.TTrashCan){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Gauges.TTrashCan.Load;
       Store  : @Gauges.TTrashCan.Store)
{$ENDIF TrashCan}
     ,(ObjType: otKeyMacros;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Gauges.TKeyMacros){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Gauges.TKeyMacros.Load;
       Store  : @Gauges.TKeyMacros.Store)
      { HelpKernel }
      ,(ObjType: otHelpTopic;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(HelpKernel.THelpTopic){$IFNDEF DPMI}^{$ENDIF});
       Load:    @HelpKernel.THelpTopic.Load;
       Store:   @HelpKernel.THelpTopic.Store),
      (ObjType: otHelpIndex;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(HelpKernel.THelpIndex){$IFNDEF DPMI}^{$ENDIF});
       Load:    @HelpKernel.THelpIndex.Load;
       Store:   @HelpKernel.THelpIndex.Store)
      { Histries }
     ,(ObjType: otEditHistoryCol;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Histries.TEditHistoryCol){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Histries.TEditHistoryCol.Load;
       Store  : @Histries.TEditHistoryCol.Store)
     ,(ObjType: otViewHistoryCol;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Histries.TViewHistoryCol){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Histries.TViewHistoryCol.Load;
       Store  : @Histries.TViewHistoryCol.Store)
      { Menus }
{$ENDIF !RCP}
     ,(ObjType: otMenuBar;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Menus.TMenuBar){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Menus.TMenuBar.Load;
       Store  : @Menus.TMenuBar.Store)
     ,(ObjType: otMenuBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Menus.TMenuBox){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Menus.TMenuBox.Load;
       Store  : @Menus.TMenuBox.Store)
     ,(ObjType: otStatusLine;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Menus.TStatusLine){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Menus.TStatusLine.Load;
       Store  : @Menus.TStatusLine.Store)
     ,(ObjType: otMenuPopup;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Menus.TMenuPopup){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Menus.TMenuPopup.Load;
       Store  : @Menus.TMenuPopup.Store)
{$IFNDEF RCP}
      { Microed }
     ,(ObjType: otFileEditor;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Microed.TFileEditor){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Microed.TFileEditor.Load;
       Store  : @Microed.TFileEditor.Store)
     ,(ObjType: otEditWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(EdWin.TEditWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @EdWin.TEditWindow.Load;
       Store  : @EdWin.TEditWindow.Store)
{$IFDEF MODEM}
{$IFDEF LINK}
      { NavyLink }
     ,(ObjType: otLinker;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(NavyLink.TLinker){$IFNDEF DPMI}^{$ENDIF});
       Load   : @NavyLink.TLinker.Load;
       Store  : @NavyLink.TLinker.Store)
     ,(ObjType: otLinkDrive;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(NavyLink.TLinkDrive){$IFNDEF DPMI}^{$ENDIF});
       Load   : @NavyLink.TLinkDrive.Load;
       Store  : @NavyLink.TLinkDrive.Store)
{$ENDIF LINK IN MODEM}
      { Dialer }
     ,(ObjType: otAutoDialer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(uDialer.TAutoDialer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @uDialer.TAutoDialer.Load;
       Store  : @uDialer.TAutoDialer.Store)
     ,(ObjType: otDialBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(uDialer.TDialBox){$IFNDEF DPMI}^{$ENDIF});
       Load   : @uDialer.TDialBox.Load;
       Store  : @uDialer.TDialBox.Store)
      { ScrollBk }
     ,(ObjType: otScrollBackWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ScrollBk.TScrollBackWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ScrollBk.TScrollBackWindow.Load;
       Store  : @ScrollBk.TScrollBackWindow.Store)
     ,(ObjType: otScrollBack;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(ScrollBk.TScrollBack){$IFNDEF DPMI}^{$ENDIF});
       Load   : @ScrollBk.TScrollBack.Load;
       Store  : @ScrollBk.TScrollBack.Store)
{$ENDIF MODEM}
{$IFDEF NETINFO}
      { NetInfo }
     ,(ObjType: otNetInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(NetInfo.TNetInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @NetInfo.TNetInfo.Load;
       Store  : @NetInfo.TNetInfo.Store)
{$ENDIF NETINFO}
{$IFDEF PHONES}
     ,(ObjType: otDStringView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(StrView.TDStringView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @StrView.TDStringView.Load;
       Store  : @StrView.TDStringView.Store)
     ,(ObjType: otPhone;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Phones.TPhone){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Phones.TPhone.Load;
       Store  : @Phones.TPhone.Store)
     ,(ObjType: otPhoneDir;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Phones.TPhoneDir){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Phones.TPhoneDir.Load;
       Store  : @Phones.TPhoneDir.Store)
     ,(ObjType: otPhoneCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Phones.TPhoneCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Phones.TPhoneCollection.Load;
       Store  : @Phones.TPhoneCollection.Store)
{$ENDIF PHONES}
{$IFDEF PrintManager}
      { PrintManager }
     ,(ObjType: otStringCol;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(PrintManager.TStringCol){$IFNDEF DPMI}^{$ENDIF});
       Load   : @PrintManager.TStringCol.Load;
       Store  : @PrintManager.TStringCol.Store)
     ,(ObjType: otPrintManager;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(PrintManager.TPrintManager){$IFNDEF DPMI}^{$ENDIF});
       Load   : @PrintManager.TPrintManager.Load;
       Store  : @PrintManager.TPrintManager.Store)
     ,(ObjType: otPrintStatus;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(PrintManager.TPrintStatus){$IFNDEF DPMI}^{$ENDIF});
       Load   : @PrintManager.TPrintStatus.Load;
       Store  : @PrintManager.TPrintStatus.Store)
     ,(ObjType: otPMWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(PrintManager.TPMWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @PrintManager.TPMWindow.Load;
       Store  : @PrintManager.TPMWindow.Store)
{$ENDIF PrintManager}
{$ENDIF !RCP}
      { Scroller }
     ,(ObjType: otScroller;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Scroller.TScroller){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Scroller.TScroller.Load;
       Store  : @Scroller.TScroller.Store)
     ,(ObjType: otListViewer;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Scroller.TListViewer){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Scroller.TListViewer.Load;
       Store  : @Scroller.TLIstViewer.Store)
      { Setups }
     ,(ObjType: otSysDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TSysDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TSysDialog.Load;
       Store  : @Setups.TSysDialog.Store)
     ,(ObjType: otCurrDriveInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TCurrDriveInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TCurrDriveInfo.Load;
       Store  : @Setups.TCurrDriveInfo.Store)
     ,(ObjType: otMouseDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TMouseDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TMouseDialog.Load;
       Store  : @Setups.TMouseDialog.Store)
     ,(ObjType: otMouseBar;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TMouseBar){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TMouseBar.Load;
       Store  : @Setups.TMouseBar.Store)
{$IFDEF SS}
     ,(ObjType: otSaversDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TSaversDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TSaversDialog.Load;
       Store  : @Setups.TSaversDialog.Store)
     ,(ObjType: otSaversListBox;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TSaversListBox){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TSaversListBox.Load;
       Store  : @Setups.TSaversListBox.Store)
{$ENDIF SS}
     ,(ObjType: otUpperTable;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Setups.TUpperTable){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Setups.TUpperTable.Load;
       Store  : @Setups.TUpperTable.Store)
{$IFNDEF RCP}
      { Startup }
     ,(ObjType: otTextCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Startup.TTextCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Startup.TTextCollection.Load;
       Store  : @Startup.TTextCollection.Store)
      { Terminal }
{$IFDEF Modem}
     ,(ObjType: otTerminalWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Terminal.TTerminalWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Terminal.TTerminalWindow.Load;
       Store  : @Terminal.TTerminalWindow.Store)
     ,(ObjType: otTerminal;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Terminal.TTerminal){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Terminal.TTerminal.Load;
       Store  : @Terminal.TTerminal.Store)
     ,(ObjType: otPortInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Terminal.TPortInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Terminal.TPortInfo.Load;
       Store  : @Terminal.TPortInfo.Store)
{$ENDIF Modem}
{$IFDEF Game}
      { Tetris }
     ,(ObjType: otGameWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tetris.TGameWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tetris.TGameWindow.Load;
       Store  : @Tetris.TGameWindow.Store)
     ,(ObjType: otGameView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tetris.TGameView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tetris.TGameView.Load;
       Store  : @Tetris.TGameView.Store)
     ,(ObjType: otGameInfo;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tetris.TGameInfo){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tetris.TGameInfo.Load;
       Store  : @Tetris.TGameInfo.Store)
{$ENDIF Game}
      { Tree }
     ,(ObjType: otTreeView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreeView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreeView.Load;
       Store  : @Tree.TTreeView.Store)
     ,(ObjType: otTreeReader;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreeReader){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreeReader.Load;
       Store  : @Tree.TTreeReader.Store)
     ,(ObjType: otTreeWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreeWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreeWindow.Load;
       Store  : @Tree.TTreeWindow.Store)
     ,(ObjType: otTreePanel;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreePanel){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreePanel.Load;
       Store  : @Tree.TTreePanel.Store)
     ,(ObjType: otTreeDialog;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreeDialog){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreeDialog.Load;
       Store  : @Tree.TTreeDialog.Store)
     ,(ObjType: otTreeInfoView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TTreeInfoView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TTreeInfoView.Load;
       Store  : @Tree.TTreeInfoView.Store)
     ,(ObjType: otHTreeView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.THTreeView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.THTreeView.Load;
       Store  : @Tree.THTreeView.Store)
     ,(ObjType: otDirCollection;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Tree.TDirCollection){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Tree.TDirCollection.Load;
       Store  : @Tree.TDirCollection.Store)
      { UniWin }
     ,(ObjType: otEditScrollBar;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(UniWin.TEditScrollBar){$IFNDEF DPMI}^{$ENDIF});
       Load   : @UniWin.TEditScrollBar.Load;
       Store  : @UniWin.TEditScrollBar.Store)
     ,(ObjType: otEditFrame;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(UniWin.TEditFrame){$IFNDEF DPMI}^{$ENDIF});
       Load   : @UniWin.TEditFrame.Load;
       Store  : @UniWin.TEditFrame.Store)
      { UserMenu }
     ,(ObjType: otUserWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(UserMenu.TUserWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @UserMenu.TUserWindow.Load;
       Store  : @UserMenu.TUserWindow.Store)
     ,(ObjType: otUserView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(UserMenu.TUserView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @UserMenu.TUserView.Load;
       Store  : @UserMenu.TUserView.Store)
      { Validate }
{$ENDIF !RCP}
     ,(ObjType: otFilterValidator;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Validate.TFilterValidator){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Validate.TFilterValidator.Load;
       Store  : @Validate.TFilterValidator.Store)
     ,(ObjType: otRangeValidator;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Validate.TRangeValidator){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Validate.TRangeValidator.Load;
       Store  : @Validate.TRangeValidator.Store)
      { Views }
     ,(ObjType: otView;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TView){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TView.Load;
       Store  : @Views.TView.Store)
     ,(ObjType: otFrame;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TFrame){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TFrame.Load;
       Store  : @Views.TFrame.Store)
     ,(ObjType: otScrollBar;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TScrollBar){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TScrollBar.Load;
       Store  : @Views.TScrollBar.Store)
     ,(ObjType: otGroup;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TGroup){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TGroup.Load;
       Store  : @Views.TGroup.Store)
     ,(ObjType: otWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TWindow.Load;
       Store  : @Views.TWindow.Store)
{$IFNDEF RCP}
     ,(ObjType: otMyScrollBar;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(Views.TMyScrollBar){$IFNDEF DPMI}^{$ENDIF});
       Load   : @Views.TMyScrollBar.Load;
       Store  : @Views.TMyScrollBar.Store)
      { XDblWnd }
     ,(ObjType: otDoubleWindow;
       VmtLink: {$IFNDEF DPMI}Ofs{$ENDIF}(TypeOf(XDblWnd.TXDoubleWindow){$IFNDEF DPMI}^{$ENDIF});
       Load   : @XDblWnd.TXDoubleWindow.Load;
       Store  : @XDblWnd.TXDoubleWindow.Store)
{$ENDIF !RCP}
  );

procedure RegisterAll;
var
  I: Integer;
begin
  for I := 1 to NumRElms do RegisterType(RegArray[I]);
end;

END.
