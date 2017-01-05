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
unit RegAll;

interface

procedure RegisterAll;

implementation

uses
  {$IFNDEF RCP}
  Arc_Zip, Arc_LHA, arc_RAR, arc_ACE, arc_HA, arc_CAB,
  {$IFNDEF MINARCH}
  arc_arc, arc_bsa, arc_bs2, arc_hyp, arc_lim, arc_hpk, arc_TAR,
  arc_ZXZ, arc_QRK, arc_AIN, arc_CHZ, arc_HAP, arc_IS3, arc_SQZ,
  arc_UC2, arc_UFA, arc_ZOO, arc_TGZ,
  {$ENDIF}
  {$IFDEF ARVID}
  Arvid,
  {$ENDIF}
  Archiver, ArcView, ASCIITab, CCalc, Collect, DiskInfo, DNApp,
  DNStdDlg, DNUtil, Drives, ed2, Editor, FileFind, FilesCol,
  FlPanel, FStorage, FViewer, Gauges, Histries, Microed, Startup,
  Tree, UniWin, UserMenu, XDblWnd, HelpKern,
  {$IFDEF SpreadSheet}Calc, CellsCol, {$ENDIF}
  {$IFDEF Calendar}Calendar, {$ENDIF}
  {$IFDEF DBView}DBView, {$ENDIF}
  {$IFDEF MODEM}ScrollBk, Terminal, uDialer,
  {$IFDEF LINK}NavyLink, {$ENDIF} {$ENDIF}
  {$IFDEF PrintManager}PrintMan, {$ENDIF}
  {$IFDEF Game}Tetris, {$ENDIF}
  {$IFDEF NetInfo}NetInfo, {$ENDIF}
  {$IFDEF PHONES}Phones, {$ENDIF}
  {$ENDIF !RCP}
  ColorSel,
  Dialogs, Menus, Objects, ObjType, Scroller, Setups,
  Validate, Views, SWE {$IFDEF UserSaver}, UserSavr {$ENDIF};

const
  NumRElms =
  {$IFDEF RCP}31 {$ELSE}120
  {$IFDEF MODEM}+7 {$IFDEF LINK}+2 {$ENDIF} {$ENDIF}
  {$IFDEF SpreadSheet}+5 {$ENDIF}
  {$IFDEF Game}+3 {$ENDIF}
  {$IFDEF PrintManager}+4 {$ENDIF}
  {$IFDEF DBView}+4 {$ENDIF}
  {$IFDEF Calendar}+2 {$ENDIF} {JO}
  {$IFDEF TrashCan}+1 {$ENDIF}
  {$IFDEF NETINFO}+1 {$ENDIF}
  {$IFDEF MINARCH}-18 {$ENDIF}
  {$IFDEF ARVID}+1 {$ENDIF}
  {$IFDEF PHONES}+4 {$ENDIF}
  {$IFDEF UserSaver}+1 {$ENDIF}
  {$ENDIF}
  {$IFDEF SS}+2 {$ENDIF}
  +7
  ;
  RegArray: array[1..NumRElms] of TStreamRec = (
  {$IFNDEF RCP}
  { Arc_ZIP }
  (ObjType: otZIPArchiver;
  VmtLink: (TypeOf(Arc_Zip.TZIPArchive));
  Load: @Arc_ZIP.TZIPArchive.Load;
  Store: @Arc_ZIP.TZIPArchive.Store)
  { Arc_LHA }
  , (ObjType: otLHAArchiver;
  VmtLink: (TypeOf(Arc_LHA.TLHAArchive));
  Load: @Arc_LHA.TLHAArchive.Load;
  Store: @Arc_LHA.TLHAArchive.Store)
  { Arc_RAR }
  , (ObjType: otRARArchiver;
  VmtLink: (TypeOf(arc_RAR.TRARArchive));
  Load: @Arc_RAR.TRARArchive.Load;
  Store: @Arc_RAR.TRARArchive.Store)
  { Arc_CAB }
  , (ObjType: otCABArchiver;
  VmtLink: (TypeOf(arc_CAB.TCABArchive));
  Load: @Arc_CAB.TCABArchive.Load;
  Store: @Arc_CAB.TCABArchive.Store)
  { Arc_ACE }
  , (ObjType: otACEArchiver;
  VmtLink: (TypeOf(arc_ACE.TACEArchive));
  Load: @Arc_ACE.TACEArchive.Load;
  Store: @Arc_ACE.TACEArchive.Store)
  { Arc_HA }
  , (ObjType: otHAArchiver;
  VmtLink: (TypeOf(arc_HA.THAArchive));
  Load: @Arc_HA.THAArchive.Load;
  Store: @Arc_HA.THAArchive.Store)
  {$IFNDEF MINARCH}
  { Arc_arc }
  , (ObjType: otARCArchiver;
  VmtLink: (TypeOf(arc_arc.TARCArchive));
  Load: @Arc_arc.TARCArchive.Load;
  Store: @Arc_arc.TARCArchive.Store)
  { Arc_bsa }
  , (ObjType: otBSAArchiver;
  VmtLink: (TypeOf(arc_bsa.TBSAArchive));
  Load: @Arc_bsa.TBSAArchive.Load;
  Store: @Arc_bsa.TBSAArchive.Store)
  { Arc_bs2 }
  , (ObjType: otBS2Archiver;
  VmtLink: (TypeOf(arc_bs2.TBS2Archive));
  Load: @Arc_bs2.TBS2Archive.Load;
  Store: @Arc_bs2.TBS2Archive.Store)
  { Arc_hyp }
  , (ObjType: otHYPArchiver;
  VmtLink: (TypeOf(arc_hyp.THYPArchive));
  Load: @Arc_hyp.THYPArchive.Load;
  Store: @Arc_hyp.THYPArchive.Store)
  { Arc_lim }
  , (ObjType: otLIMArchiver;
  VmtLink: (TypeOf(arc_lim.TLIMArchive));
  Load: @Arc_lim.TLIMArchive.Load;
  Store: @Arc_lim.TLIMArchive.Store)
  { Arc_hpk }
  , (ObjType: otHPKArchiver;
  VmtLink: (TypeOf(arc_hpk.THPKArchive));
  Load: @Arc_Hpk.THPKArchive.Load;
  Store: @Arc_hpk.THPKArchive.Store)
  { Arc_TAR }
  , (ObjType: otTARArchiver;
  VmtLink: (TypeOf(arc_TAR.TTARArchive));
  Load: @Arc_TAR.TTARArchive.Load;
  Store: @Arc_TAR.TTARArchive.Store)
  { Arc_TGZ }
  , (ObjType: otTGZArchiver;
  VmtLink: (TypeOf(arc_TGZ.TTGZArchive));
  Load: @Arc_TGZ.TTGZArchive.Load;
  Store: @Arc_TGZ.TTGZArchive.Store)
  { Arc_ZXZ }
  , (ObjType: otZXZArchiver;
  VmtLink: (TypeOf(arc_ZXZ.TZXZArchive));
  Load: @Arc_ZXZ.TZXZArchive.Load;
  Store: @Arc_ZXZ.TZXZArchive.Store)
  { Arc_QRK }
  , (ObjType: otQUARKArchiver;
  VmtLink: (TypeOf(arc_QRK.TQuArkArchive));
  Load: @Arc_QRK.TQuArkArchive.Load;
  Store: @Arc_QRK.TQuArkArchive.Store)
  { Arc_UFA }
  , (ObjType: otUFAArchiver;
  VmtLink: (TypeOf(arc_UFA.TUFAArchive));
  Load: @Arc_UFA.TUFAArchive.Load;
  Store: @Arc_UFA.TUFAArchive.Store)
  { Arc_IS3 }
  , (ObjType: otIS3Archiver;
  VmtLink: (TypeOf(arc_IS3.TIS3Archive));
  Load: @Arc_IS3.TIS3Archive.Load;
  Store: @Arc_IS3.TIS3Archive.Store)
  { Arc_SQZ }
  , (ObjType: otSQZArchiver;
  VmtLink: (TypeOf(arc_SQZ.TSQZArchive));
  Load: @Arc_SQZ.TSQZArchive.Load;
  Store: @Arc_SQZ.TSQZArchive.Store)
  { Arc_HAP }
  , (ObjType: otHAPArchiver;
  VmtLink: (TypeOf(arc_HAP.THAPArchive));
  Load: @Arc_HAP.THAPArchive.Load;
  Store: @Arc_HAP.THAPArchive.Store)
  { Arc_ZOO }
  , (ObjType: otZOOArchiver;
  VmtLink: (TypeOf(arc_ZOO.TZOOArchive));
  Load: @Arc_ZOO.TZOOArchive.Load;
  Store: @Arc_ZOO.TZOOArchive.Store)
  { Arc_CHZ }
  , (ObjType: otCHZArchiver;
  VmtLink: (TypeOf(arc_CHZ.TCHZArchive));
  Load: @Arc_CHZ.TCHZArchive.Load;
  Store: @Arc_CHZ.TCHZArchive.Store)
  { Arc_UC2 }
  , (ObjType: otUC2Archiver;
  VmtLink: (TypeOf(arc_UC2.TUC2Archive));
  Load: @Arc_UC2.TUC2Archive.Load;
  Store: @Arc_UC2.TUC2Archive.Store)
  { Arc_AIN }
  , (ObjType: otAINArchiver;
  VmtLink: (TypeOf(arc_AIN.TAINArchive));
  Load: @Arc_AIN.TAINArchive.Load;
  Store: @Arc_AIN.TAINArchive.Store)
  {$ENDIF MINARCH}
  { Archiver }
  , (ObjType: otARJArchiver;
  VmtLink: (TypeOf(Archiver.TARJArchive));
  Load: @Archiver.TARJArchive.Load;
  Store: @Archiver.TARJArchive.Store)
  , (ObjType: otFileInfo;
  VmtLink: (TypeOf(Archiver.TFileInfo));
  Load: @Archiver.TFileInfo.Load;
  Store: @Archiver.TFileInfo.Store)
  {$IFDEF UserSaver}
  , (ObjType: otUserSaver;
  VmtLink: (TypeOf(UserSavr.TUserSaver));
  Load: @UserSavr.TUserSaver.Load;
  Store: @UserSavr.TUserSaver.Store)
  {$ENDIF UserSaver}
  { ArcView }
  , (ObjType: otArcDrive;
  VmtLink: (TypeOf(ArcView.TArcDrive));
  Load: @ArcView.TArcDrive.Load;
  Store: @ArcView.TArcDrive.Store)
  {$IFDEF ARVID}
  { Arvid }
  , (ObjType: otArvidDrive;
  VmtLink: (TypeOf(Arvid.TArvidDrive));
  Load: @Arvid.TArvidDrive.Load;
  Store: @Arvid.TArvidDrive.Store)
  {$ENDIF}
  { AsciiTab }
  , (ObjType: otTable;
  VmtLink: (TypeOf(ASCIITab.TTable));
  Load: @AsciiTab.TTable.Load;
  Store: @AsciiTab.TTable.Store)
  , (ObjType: otReport;
  VmtLink: (TypeOf(ASCIITab.TReport));
  Load: @AsciiTab.TReport.Load;
  Store: @AsciiTab.TReport.Store)
  , (ObjType: otASCIIChart;
  VmtLink: (TypeOf(ASCIITab.TASCIIChart));
  Load: @AsciiTab.TASCIIChart.Load;
  Store: @AsciiTab.TASCIIChart.Store)
  { Calc }
  {$IFDEF SpreadSheet}
  , (ObjType: otCalcWindow;
  VmtLink: (TypeOf(Calc.TCalcWindow));
  Load: @Calc.TCalcWindow.Load;
  Store: @Calc.TCalcWindow.Store)
  , (ObjType: otCalcView;
  VmtLink: (TypeOf(Calc.TCalcView));
  Load: @Calc.TCalcView.Load;
  Store: @Calc.TCalcView.Store)
  , (ObjType: otCalcInfo;
  VmtLink: (TypeOf(Calc.TCalcInput));
  Load: @Calc.TCalcInput.Load;
  Store: @Calc.TCalcInput.Store)
  , (ObjType: otInfoView;
  VmtLink: (TypeOf(Calc.TInfoView));
  Load: @Calc.TInfoView.Load;
  Store: @Calc.TInfoView.Store)
  { CellsCol }
  , (ObjType: otCellCollection;
  VmtLink: (TypeOf(CellsCol.TCellCollection));
  Load: @CellsCol.TCellCollection.Load;
  Store: @CellsCol.TCellCollection.Store)
  {$ENDIF SpreadSheet}
  {$IFDEF Calendar}
  { Calendar }
  , (ObjType: otCalendarView;
  VmtLink: (TypeOf(Calendar.TCalendarView));
  Load: @Calendar.TCalendarView.Load;
  Store: @Calendar.TCalendarView.Store)
  , (ObjType: otCalendarWindow;
  VmtLink: (TypeOf(Calendar.TCalendarWindow));
  Load: @Calendar.TCalendarWindow.Load;
  Store: @Calendar.TCalendarWindow.Store)
  {$ENDIF Calendar}
  { CCalc }
  , (ObjType: otCalcLine;
  VmtLink: (TypeOf(CCalc.TCalcLine));
  Load: @CCalc.TCalcLine.Load;
  Store: @CCalc.TCalcLine.Store)
  , (ObjType: otIndicator;
  VmtLink: (TypeOf(CCalc.TIndicator));
  Load: @CCalc.TIndicator.Load;
  Store: @CCalc.TIndicator.Store)
  { Collect }
  , (ObjType: otCollection;
  VmtLink: (TypeOf(Collect.TCollection));
  Load: @Collect.TCollection.Load;
  Store: @Collect.TCollection.Store)
  , (ObjType: otLineCollection;
  VmtLink: (TypeOf(Collect.TLineCollection));
  Load: @Collect.TLineCollection.Load;
  Store: @Collect.TLineCollection.Store)
  , (ObjType: otStringCollection;
  VmtLink: (TypeOf(Collect.TStringCollection));
  Load: @Collect.TStringCollection.Load;
  Store: @Collect.TStringCollection.Store)
  , (ObjType: otStrCollection;
  VmtLink: (TypeOf(Collect.TStrCollection));
  Load: @Collect.TStrCollection.Load;
  Store: @Collect.TStrCollection.Store)
  , (ObjType: otStringList;
  VmtLink: (TypeOf(Collect.TStringList));
  Load: @Collect.TStringList.Load;
  Store: nil),
  {$ENDIF !RCP}
  { ColorSel }
  (ObjType: otColorSelector;
  VmtLink: (TypeOf(ColorSel.TColorSelector));
  Load: @ColorSel.TColorSelector.Load;
  Store: @ColorSel.TColorSelector.Store)
  , (ObjType: otMonoSelector;
  VmtLink: (TypeOf(ColorSel.TMonoSelector));
  Load: @ColorSel.TMonoSelector.Load;
  Store: @ColorSel.TMonoSelector.Store)
  , (ObjType: otColorDisplay;
  VmtLink: (TypeOf(ColorSel.TColorDisplay));
  Load: @ColorSel.TColorDisplay.Load;
  Store: @ColorSel.TColorDisplay.Store)
  , (ObjType: otColorGroupList;
  VmtLink: (TypeOf(ColorSel.TColorGroupList));
  Load: @ColorSel.TColorGroupList.Load;
  Store: @ColorSel.TColorGroupList.Store)
  , (ObjType: otColorItemList;
  VmtLink: (TypeOf(ColorSel.TColorItemList));
  Load: @ColorSel.TColorItemList.Load;
  Store: @ColorSel.TColorItemList.Store)
  , (ObjType: otColorDialog;
  VmtLink: (TypeOf(ColorSel.TColorDialog));
  Load: @ColorSel.TColorDialog.Load;
  Store: @ColorSel.TColorDialog.Store)
  , (ObjType: otR_BWSelector;
  VmtLink: (TypeOf(ColorSel.T_BWSelector));
  Load: @ColorSel.T_BWSelector.Load;
  Store: @ColorSel.T_BWSelector.Store),
  {$IFNDEF RCP}
  {$IFDEF DBView}
  { DBView }
  (ObjType: otDBWindow;
  VmtLink: (TypeOf(DBView.TDBWindow));
  Load: @DBView.TDBWindow.Load;
  Store: @DBView.TDBWindow.Store)
  , (ObjType: otDBViewer;
  VmtLink: (TypeOf(DBView.TDBViewer));
  Load: @DBView.TDBViewer.Load;
  Store: @DBView.TDBViewer.Store)
  , (ObjType: otDBIndicator;
  VmtLink: (TypeOf(DBView.TDBIndicator));
  Load: @DBView.TDBIndicator.Load;
  Store: @DBView.TDBIndicator.Store)
  , (ObjType: otFieldListBox;
  VmtLink: (TypeOf(DBView.TFieldListBox));
  Load: @DBView.TFieldListBox.Load;
  Store: @DBView.TFieldListBox.Store),
  {$ENDIF DBView}
  {$ENDIF !RCP}
  { Dialogs }
  (ObjType: otDialog;
  VmtLink: (TypeOf(Dialogs.TDialog));
  Load: @Dialogs.TDialog.Load;
  Store: @Dialogs.TDialog.Store)
  , (ObjType: otInputLine;
  VmtLink: (TypeOf(Dialogs.TInputLine));
  Load: @Dialogs.TInputLine.Load;
  Store: @Dialogs.TInputLine.Store)
  , (ObjType: otHexLine;
  VmtLink: (TypeOf(Dialogs.THexLine));
  Load: @Dialogs.THexLine.Load;
  Store: @Dialogs.THexLine.Store)
  , (ObjType: otButton;
  VmtLink: (TypeOf(Dialogs.TButton));
  Load: @Dialogs.TButton.Load;
  Store: @Dialogs.TButton.Store)
  , (ObjType: otCluster;
  VmtLink: (TypeOf(Dialogs.TCluster));
  Load: @Dialogs.TCluster.Load;
  Store: @Dialogs.TCluster.Store)
  , (ObjType: otRadioButtons;
  VmtLink: (TypeOf(Dialogs.TRadioButtons));
  Load: @Dialogs.TRadioButtons.Load;
  Store: @Dialogs.TRadioButtons.Store)
  , (ObjType: otCheckBoxes;
  VmtLink: (TypeOf(Dialogs.TCheckBoxes));
  Load: @Dialogs.TCheckBoxes.Load;
  Store: @Dialogs.TCheckBoxes.Store)
  , (ObjType: otMultiCheckBoxes;
  VmtLink: (TypeOf(Dialogs.TMultiCheckBoxes));
  Load: @Dialogs.TMultiCheckBoxes.Load;
  Store: @Dialogs.TMultiCheckBoxes.Store)
  , (ObjType: otListBox;
  VmtLink: (TypeOf(Dialogs.TListBox));
  Load: @Dialogs.TListBox.Load;
  Store: @Dialogs.TListBox.Store)
  , (ObjType: otStaticText;
  VmtLink: (TypeOf(Dialogs.TStaticText));
  Load: @Dialogs.TStaticText.Load;
  Store: @Dialogs.TStaticText.Store)
  , (ObjType: otLabel;
  VmtLink: (TypeOf(Dialogs.TLabel));
  Load: @Dialogs.TLabel.Load;
  Store: @Dialogs.TLabel.Store)
  , (ObjType: otHistory;
  VmtLink: (TypeOf(Dialogs.THistory));
  Load: @Dialogs.THistory.Load;
  Store: @Dialogs.THistory.Store)
  , (ObjType: otParamText;
  VmtLink: (TypeOf(Dialogs.TParamText));
  Load: @Dialogs.TParamText.Load;
  Store: @Dialogs.TParamText.Store)
  {$IFNDEF RCP}
  { DiskInfo }
  , (ObjType: otDiskInfo;
  VmtLink: (TypeOf(DiskInfo.TDiskInfo));
  Load: @DiskInfo.TDiskInfo.Load;
  Store: @DiskInfo.TDiskInfo.Store)
  { DnApp }
  , (ObjType: otBackground;
  VmtLink: (TypeOf(DNApp.TBackground));
  Load: @DnApp.TBackground.Load;
  Store: @DnApp.TBackground.Store)
  , (ObjType: otDesktop;
  VmtLink: (TypeOf(DNApp.TDesktop));
  Load: @DnApp.TDesktop.Load;
  Store: @DnApp.TDesktop.Store)
  , (ObjType: otStringCache;
  VmtLink: (TypeOf(DNApp.TCacheCollection));
  Load: @DnApp.TCacheCollection.Load;
  Store: @DnApp.TCacheCollection.Store)
  { DnStdDlg }
  , (ObjType: otFileInputLine;
  VmtLink: (TypeOf(DNStdDlg.TFileInputLine));
  Load: @DnStdDlg.TFileInputLine.Load;
  Store: @DnStdDlg.TFileInputLine.Store)
  , (ObjType: otFileCollection;
  VmtLink: (TypeOf(DNStdDlg.TFileCollection));
  Load: @DnStdDlg.TFileCollection.Load;
  Store: @DnStdDlg.TFileCollection.Store)
  , (ObjType: otFileList;
  VmtLink: (TypeOf(DNStdDlg.TFileList));
  Load: @DnStdDlg.TFileList.Load;
  Store: @DnStdDlg.TFileList.Store)
  , (ObjType: otFileInfoPane;
  VmtLink: (TypeOf(DNStdDlg.TFileInfoPane));
  Load: @DnStdDlg.TFileInfoPane.Load;
  Store: @DnStdDlg.TFileInfoPane.Store)
  , (ObjType: otFileDialog;
  VmtLink: (TypeOf(DNStdDlg.TFileDialog));
  Load: @DnStdDlg.TFileDialog.Load;
  Store: @DnStdDlg.TFileDialog.Store)
  , (ObjType: otSortedListBox;
  VmtLink: (TypeOf(DNStdDlg.TSortedListBox));
  Load: @DnStdDlg.TSortedListBox.Load;
  Store: @DnStdDlg.TSortedListBox.Store)
  { DNUtil }
  , (ObjType: otDataSaver;
  VmtLink: (TypeOf(DNUtil.TDataSaver));
  Load: @DNUtil.TDataSaver.Load;
  Store: @DNUtil.TDataSaver.Store)
  { Drives }
  , (ObjType: otDrive;
  VmtLink: (TypeOf(Drives.TDrive));
  Load: @Drives.TDrive.Load;
  Store: @Drives.TDrive.Store)
  { Ed2 }
  , (ObjType: otInfoLine;
  VmtLink: (TypeOf(ed2.TInfoLine));
  Load: @Ed2.TInfoLine.Load;
  Store: @Ed2.TInfoLine.Store)
  , (ObjType: otBookLine;
  VmtLink: (TypeOf(ed2.TBookmarkLine));
  Load: @Ed2.TBookmarkLine.Load;
  Store: @Ed2.TBookmarkLine.Store)
  { Editor }
  , (ObjType: otXFileEditor;
  VmtLink: (TypeOf(Editor.TXFileEditor));
  Load: @Editor.TXFileEditor.Load;
  Store: @Editor.TXFileEditor.Store)
  { FileFind }
  , (ObjType: otFindDrive;
  VmtLink: (TypeOf(FileFind.TFindDrive));
  Load: @FileFind.TFindDrive.Load;
  Store: @FileFind.TFindDrive.Store)
  , (ObjType: otTempDrive;
  VmtLink: (TypeOf(FileFind.TTempDrive));
  Load: @FileFind.TTempDrive.Load;
  Store: @FileFind.TTempDrive.Store)
  { FilesCol }
  , (ObjType: otFilesCollection;
  VmtLink: (TypeOf(FilesCol.TFilesCollection));
  Load: @FilesCol.TFilesCollection.Load;
  Store: @FilesCol.TFilesCollection.Store)
  { FlPanel }
  , (ObjType: otFilePanel;
  VmtLink: (TypeOf(FlPanel.TFilePanel));
  Load: @FlPanel.TFilePanel.Load;
  Store: @FlPanel.TFilePanel.Store)
  , (ObjType: otFlPInfoView;
  VmtLink: (TypeOf(FlPanel.TInfoView));
  Load: @FlPanel.TInfoView.Load;
  Store: @FlPanel.TInfoView.Store)
  , (ObjType: otTopView;
  VmtLink: (TypeOf(FlPanel.TTopView));
  Load: @FlPanel.TTopView.Load;
  Store: @FlPanel.TTopView.Store)
  , (ObjType: otSeparator;
  VmtLink: (TypeOf(FlPanel.TSeparator));
  Load: @FlPanel.TSeparator.Load;
  Store: @FlPanel.TSeparator.Store)
  , (ObjType: otSpecScroll;
  VmtLink: (TypeOf(FlPanel.TSpecScroll));
  Load: @FlPanel.TSpecScroll.Load;
  Store: @FlPanel.TSpecScroll.Store)
  , (ObjType: otDriveLine;
  VmtLink: (TypeOf(FlPanel.TDriveLine));
  Load: @FlPanel.TDriveLine.Load;
  Store: @FlPanel.TDriveLine.Store)
  { FStorage }
  , (ObjType: otDirStorage;
  VmtLink: (TypeOf(FStorage.TDirStorage));
  Load: @FStorage.TDirStorage.Load;
  Store: @FStorage.TDirStorage.Store)
  { FViewer }
  , (ObjType: otFileViewer;
  VmtLink: (TypeOf(FViewer.TFileViewer));
  Load: @FViewer.TFileViewer.Load;
  Store: @FViewer.TFileViewer.Store)
  , (ObjType: otFileWindow;
  VmtLink: (TypeOf(FViewer.TFileWindow));
  Load: @FViewer.TFileWindow.Load;
  Store: @FViewer.TFileWindow.Store)
  , (ObjType: otViewScroll;
  VmtLink: (TypeOf(FViewer.TViewScroll));
  Load: @FViewer.TViewScroll.Load;
  Store: @FViewer.TViewScroll.Store)
  , (ObjType: otHFileViewer;
  VmtLink: (TypeOf(FViewer.THFileViewer));
  Load: @FViewer.THFileViewer.Load;
  Store: @FViewer.THFileViewer.Store)
  , (ObjType: otViewInfo;
  VmtLink: (TypeOf(FViewer.TViewInfo));
  Load: @FViewer.TViewInfo.Load;
  Store: @FViewer.TViewInfo.Store)
  { Gauges }
  {$IFDEF TrashCan}
  , (ObjType: otTrashCan;
  VmtLink: (TypeOf(Gauges.TTrashCan));
  Load: @Gauges.TTrashCan.Load;
  Store: @Gauges.TTrashCan.Store)
  {$ENDIF TrashCan}
  , (ObjType: otKeyMacros;
  VmtLink: (TypeOf(Gauges.TKeyMacros));
  Load: @Gauges.TKeyMacros.Load;
  Store: @Gauges.TKeyMacros.Store)
  { HelpKern }
  , (ObjType: otHelpTopic;
  VmtLink: (TypeOf(HelpKern.THelpTopic));
  Load: @HelpKern.THelpTopic.Load;
  Store: @HelpKern.THelpTopic.Store),
  (ObjType: otHelpIndex;
  VmtLink: (TypeOf(HelpKern.THelpIndex));
  Load: @HelpKern.THelpIndex.Load;
  Store: @HelpKern.THelpIndex.Store)
  { Histries }
  , (ObjType: otEditHistoryCol;
  VmtLink: (TypeOf(Histries.TEditHistoryCol));
  Load: @Histries.TEditHistoryCol.Load;
  Store: @Histries.TEditHistoryCol.Store)
  , (ObjType: otViewHistoryCol;
  VmtLink: (TypeOf(Histries.TViewHistoryCol));
  Load: @Histries.TViewHistoryCol.Load;
  Store: @Histries.TViewHistoryCol.Store)
  { Menus }
  {$ENDIF !RCP}
  , (ObjType: otMenuBar;
  VmtLink: (TypeOf(Menus.TMenuBar));
  Load: @Menus.TMenuBar.Load;
  Store: @Menus.TMenuBar.Store)
  , (ObjType: otMenuBox;
  VmtLink: (TypeOf(Menus.TMenuBox));
  Load: @Menus.TMenuBox.Load;
  Store: @Menus.TMenuBox.Store)
  , (ObjType: otStatusLine;
  VmtLink: (TypeOf(Menus.TStatusLine));
  Load: @Menus.TStatusLine.Load;
  Store: @Menus.TStatusLine.Store)
  , (ObjType: otMenuPopup;
  VmtLink: (TypeOf(Menus.TMenuPopup));
  Load: @Menus.TMenuPopup.Load;
  Store: @Menus.TMenuPopup.Store)
  {$IFNDEF RCP}
  { Microed }
  , (ObjType: otFileEditor;
  VmtLink: (TypeOf(Microed.TFileEditor));
  Load: @Microed.TFileEditor.Load;
  Store: @Microed.TFileEditor.Store)
  , (ObjType: otEditWindow;
  VmtLink: (TypeOf(EdWin.TEditWindow));
  Load: @EdWin.TEditWindow.Load;
  Store: @EdWin.TEditWindow.Store)
  {$IFDEF MODEM}
  {$IFDEF LINK}
  { NavyLink }
  , (ObjType: otLinker;
  VmtLink: (TypeOf(NavyLink.TLinker));
  Load: @NavyLink.TLinker.Load;
  Store: @NavyLink.TLinker.Store)
  , (ObjType: otLinkDrive;
  VmtLink: (TypeOf(NavyLink.TLinkDrive));
  Load: @NavyLink.TLinkDrive.Load;
  Store: @NavyLink.TLinkDrive.Store)
  {$ENDIF LINK IN MODEM}
  { Dialer }
  , (ObjType: otAutoDialer;
  VmtLink: (TypeOf(uDialer.TAutoDialer));
  Load: @uDialer.TAutoDialer.Load;
  Store: @uDialer.TAutoDialer.Store)
  , (ObjType: otDialBox;
  VmtLink: (TypeOf(uDialer.TDialBox));
  Load: @uDialer.TDialBox.Load;
  Store: @uDialer.TDialBox.Store)
  { ScrollBk }
  , (ObjType: otScrollBackWindow;
  VmtLink: (TypeOf(ScrollBk.TScrollBackWindow));
  Load: @ScrollBk.TScrollBackWindow.Load;
  Store: @ScrollBk.TScrollBackWindow.Store)
  , (ObjType: otScrollBack;
  VmtLink: (TypeOf(ScrollBk.TScrollBack));
  Load: @ScrollBk.TScrollBack.Load;
  Store: @ScrollBk.TScrollBack.Store)
  {$ENDIF MODEM}
  {$IFDEF NETINFO}
  { NetInfo }
  , (ObjType: otNetInfo;
  VmtLink: (TypeOf(NetInfo.TNetInfo));
  Load: @NetInfo.TNetInfo.Load;
  Store: @NetInfo.TNetInfo.Store)
  {$ENDIF NETINFO}
  {$IFDEF PHONES}
  , (ObjType: otDStringView;
  VmtLink: (TypeOf(StrView.TDStringView));
  Load: @StrView.TDStringView.Load;
  Store: @StrView.TDStringView.Store)
  , (ObjType: otPhone;
  VmtLink: (TypeOf(Phones.TPhone));
  Load: @Phones.TPhone.Load;
  Store: @Phones.TPhone.Store)
  , (ObjType: otPhoneDir;
  VmtLink: (TypeOf(Phones.TPhoneDir));
  Load: @Phones.TPhoneDir.Load;
  Store: @Phones.TPhoneDir.Store)
  , (ObjType: otPhoneCollection;
  VmtLink: (TypeOf(Phones.TPhoneCollection));
  Load: @Phones.TPhoneCollection.Load;
  Store: @Phones.TPhoneCollection.Store)
  {$ENDIF PHONES}
  {$IFDEF PrintManager}
  { PrintManager }
  , (ObjType: otStringCol;
  VmtLink: (TypeOf(PrintMan.TStringCol));
  Load: @PrintMan.TStringCol.Load;
  Store: @PrintMan.TStringCol.Store)
  , (ObjType: otPrintManager;
  VmtLink: (TypeOf(PrintMan.TPrintManager));
  Load: @PrintMan.TPrintManager.Load;
  Store: @PrintMan.TPrintManager.Store)
  , (ObjType: otPrintStatus;
  VmtLink: (TypeOf(PrintMan.TPrintStatus));
  Load: @PrintMan.TPrintStatus.Load;
  Store: @PrintMan.TPrintStatus.Store)
  , (ObjType: otPMWindow;
  VmtLink: (TypeOf(PrintMan.TPMWindow));
  Load: @PrintMan.TPMWindow.Load;
  Store: @PrintMan.TPMWindow.Store)
  {$ENDIF PrintManager}
  {$ENDIF !RCP}
  { Scroller }
  , (ObjType: otScroller;
  VmtLink: (TypeOf(Scroller.TScroller));
  Load: @Scroller.TScroller.Load;
  Store: @Scroller.TScroller.Store)
  , (ObjType: otListViewer;
  VmtLink: (TypeOf(Scroller.TListViewer));
  Load: @Scroller.TListViewer.Load;
  Store: @Scroller.TListViewer.Store)
  { Setups }
  , (ObjType: otSysDialog;
  VmtLink: (TypeOf(Setups.TSysDialog));
  Load: @Setups.TSysDialog.Load;
  Store: @Setups.TSysDialog.Store)
  , (ObjType: otCurrDriveInfo;
  VmtLink: (TypeOf(Setups.TCurrDriveInfo));
  Load: @Setups.TCurrDriveInfo.Load;
  Store: @Setups.TCurrDriveInfo.Store)
  , (ObjType: otMouseBar;
  VmtLink: (TypeOf(Setups.TMouseBar));
  Load: @Setups.TMouseBar.Load;
  Store: @Setups.TMouseBar.Store)
  {$IFDEF SS}
  , (ObjType: otSaversDialog;
  VmtLink: (TypeOf(Setups.TSaversDialog));
  Load: @Setups.TSaversDialog.Load;
  Store: @Setups.TSaversDialog.Store)
  , (ObjType: otSaversListBox;
  VmtLink: (TypeOf(Setups.TSaversListBox));
  Load: @Setups.TSaversListBox.Load;
  Store: @Setups.TSaversListBox.Store)
  {$ENDIF SS}
  , (ObjType: otUpperTable;
  VmtLink: (TypeOf(Setups.TUpperTable));
  Load: @Setups.TUpperTable.Load;
  Store: @Setups.TUpperTable.Store)
  {$IFNDEF RCP}
  { Startup }
  , (ObjType: otTextCollection;
  VmtLink: (TypeOf(Startup.TTextCollection));
  Load: @Startup.TTextCollection.Load;
  Store: @Startup.TTextCollection.Store)
  { Terminal }
  {$IFDEF Modem}
  , (ObjType: otTerminalWindow;
  VmtLink: (TypeOf(Terminal.TTerminalWindow));
  Load: @Terminal.TTerminalWindow.Load;
  Store: @Terminal.TTerminalWindow.Store)
  , (ObjType: otTerminal;
  VmtLink: (TypeOf(Terminal.TTerminal));
  Load: @Terminal.TTerminal.Load;
  Store: @Terminal.TTerminal.Store)
  , (ObjType: otPortInfo;
  VmtLink: (TypeOf(Terminal.TPortInfo));
  Load: @Terminal.TPortInfo.Load;
  Store: @Terminal.TPortInfo.Store)
  {$ENDIF Modem}
  {$IFDEF Game}
  { Tetris }
  , (ObjType: otGameWindow;
  VmtLink: (TypeOf(Tetris.TGameWindow));
  Load: @Tetris.TGameWindow.Load;
  Store: @Tetris.TGameWindow.Store)
  , (ObjType: otGameView;
  VmtLink: (TypeOf(Tetris.TGameView));
  Load: @Tetris.TGameView.Load;
  Store: @Tetris.TGameView.Store)
  , (ObjType: otGameInfo;
  VmtLink: (TypeOf(Tetris.TGameInfo));
  Load: @Tetris.TGameInfo.Load;
  Store: @Tetris.TGameInfo.Store)
  {$ENDIF Game}
  { Tree }
  , (ObjType: otTreeView;
  VmtLink: (TypeOf(Tree.TTreeView));
  Load: @Tree.TTreeView.Load;
  Store: @Tree.TTreeView.Store)
  , (ObjType: otTreeReader;
  VmtLink: (TypeOf(Tree.TTreeReader));
  Load: @Tree.TTreeReader.Load;
  Store: @Tree.TTreeReader.Store)
  , (ObjType: otTreeWindow;
  VmtLink: (TypeOf(Tree.TTreeWindow));
  Load: @Tree.TTreeWindow.Load;
  Store: @Tree.TTreeWindow.Store)
  , (ObjType: otTreePanel;
  VmtLink: (TypeOf(Tree.TTreePanel));
  Load: @Tree.TTreePanel.Load;
  Store: @Tree.TTreePanel.Store)
  , (ObjType: otTreeDialog;
  VmtLink: (TypeOf(Tree.TTreeDialog));
  Load: @Tree.TTreeDialog.Load;
  Store: @Tree.TTreeDialog.Store)
  , (ObjType: otTreeInfoView;
  VmtLink: (TypeOf(Tree.TTreeInfoView));
  Load: @Tree.TTreeInfoView.Load;
  Store: @Tree.TTreeInfoView.Store)
  , (ObjType: otHTreeView;
  VmtLink: (TypeOf(Tree.THTreeView));
  Load: @Tree.THTreeView.Load;
  Store: @Tree.THTreeView.Store)
  , (ObjType: otDirCollection;
  VmtLink: (TypeOf(Tree.TDirCollection));
  Load: @Tree.TDirCollection.Load;
  Store: @Tree.TDirCollection.Store)
  { UniWin }
  , (ObjType: otEditScrollBar;
  VmtLink: (TypeOf(UniWin.TEditScrollBar));
  Load: @UniWin.TEditScrollBar.Load;
  Store: @UniWin.TEditScrollBar.Store)
  , (ObjType: otEditFrame;
  VmtLink: (TypeOf(UniWin.TEditFrame));
  Load: @UniWin.TEditFrame.Load;
  Store: @UniWin.TEditFrame.Store)
  { UserMenu }
  , (ObjType: otUserWindow;
  VmtLink: (TypeOf(UserMenu.TUserWindow));
  Load: @UserMenu.TUserWindow.Load;
  Store: @UserMenu.TUserWindow.Store)
  , (ObjType: otUserView;
  VmtLink: (TypeOf(UserMenu.TUserView));
  Load: @UserMenu.TUserView.Load;
  Store: @UserMenu.TUserView.Store)
  { Validate }
  {$ENDIF !RCP}
  , (ObjType: otFilterValidator;
  VmtLink: (TypeOf(Validate.TFilterValidator));
  Load: @Validate.TFilterValidator.Load;
  Store: @Validate.TFilterValidator.Store)
  , (ObjType: otRangeValidator;
  VmtLink: (TypeOf(Validate.TRangeValidator));
  Load: @Validate.TRangeValidator.Load;
  Store: @Validate.TRangeValidator.Store)
  { Views }
  , (ObjType: otView;
  VmtLink: (TypeOf(Views.TView));
  Load: @Views.TView.Load;
  Store: @Views.TView.Store)
  , (ObjType: otFrame;
  VmtLink: (TypeOf(Views.TFrame));
  Load: @Views.TFrame.Load;
  Store: @Views.TFrame.Store)
  , (ObjType: otScrollBar;
  VmtLink: (TypeOf(Views.TScrollBar));
  Load: @Views.TScrollBar.Load;
  Store: @Views.TScrollBar.Store)
  , (ObjType: otGroup;
  VmtLink: (TypeOf(Views.TGroup));
  Load: @Views.TGroup.Load;
  Store: @Views.TGroup.Store)
  , (ObjType: otWindow;
  VmtLink: (TypeOf(Views.TWindow));
  Load: @Views.TWindow.Load;
  Store: @Views.TWindow.Store)
  {$IFNDEF RCP}
  , (ObjType: otMyScrollBar;
  VmtLink: (TypeOf(Views.TMyScrollBar));
  Load: @Views.TMyScrollBar.Load;
  Store: @Views.TMyScrollBar.Store)
  { XDblWnd }
  , (ObjType: otDoubleWindow;
  VmtLink: (TypeOf(XDblWnd.TXDoubleWindow));
  Load: @XDblWnd.TXDoubleWindow.Load;
  Store: @XDblWnd.TXDoubleWindow.Store)
  {$ENDIF !RCP}
  , (ObjType: otColorPoint;
  VmtLink: (TypeOf(SWE.TColorPoint));
  Load: @SWE.TColorPoint.Load;
  Store: @SWE.TColorPoint.Store)
  );

procedure RegisterAll;
  var
    i: integer;
  begin
    for i := 1 to NumRElms do
      RegisterType(RegArray[i]);
  end;

end.
