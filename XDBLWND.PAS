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

unit XDblWnd;

interface

uses
  DblWnd, Drivers, Views, Objects;

type

      PXDoubleWindow = ^TXDoubleWindow;
      TXDoubleWindow = object(TDoubleWindow)
         procedure SetState(AState: Word; Enable: Boolean); virtual;
         function  GetPalette: PPalette; virtual;
         procedure HandleEvent(var Event: TEvent); virtual;
      end;

implementation
uses Commands, advance, advance1, advance2, advance3, DNApp, DNUtil;

function TXDoubleWindow.GetPalette;
 const S: String[Length(CDoubleWindow)] = CDoubleWindow;
begin
 GetPalette := @S;
end;


procedure TXDoubleWindow.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState, Enable);
  if (AState and sfDragging <> 0) or
    (AState and (sfSelected+sfActive) <> 0) then Separator^.Draw;
  if AState = sfSelected then
  begin
    SetState(sfActive, Enable);
{$IFDEF TrashCan}
    TrashCan^.Hide;
{$ENDIF}
    if Enable then EnableCommands(DblWndCommands)
              else DisableCommands(DblWndCommands);
  end
{$IFDEF TrashCan}
   else
   if TrashCan^.ImVisible then begin
    TrashCan^.Show;
    TrashCan^.MakeFirst;
   end;
{$ENDIF}
end;


procedure TXDoubleWindow.HandleEvent;
  var CE: Boolean;
      LVisible, RVisible, LSelected: Boolean;
      EV: TEvent;
begin
  if RightView<>nil
   then RVisible := RightView^.GetState(sfVisible)
   else RVisible := false;
  if LeftView<>nil then begin
   LVisible := LeftView^.GetState(sfVisible);
   LSelected := LeftView^.GetState(sfSelected);
  end else begin
   LVisible := false;
   LSelected := false;
  end;
  CE := On;
  case Event.What of
   evKeyDown: case Event.KeyCode of
               {kbCtrlP,}
               kbCtrlLeft,
               kbCtrlRight,
               kbAlt1, kbAlt2, kbAlt3, kbAlt4, kbAlt5,
               kbAlt6, kbAlt7, kbAlt8, kbAlt9, kbAlt0,
               kbCtrl1, kbCtrl2, kbCtrl3, kbCtrl4, kbCtrl5, {DataCompBoy}
               kbCtrl6, kbCtrl7, kbCtrl8, kbCtrl9, kbCtrl0, {DataCompBoy}
               kbAltLeft,kbAltRight,
               kbCtrlAltZ,{Knave 11.09.99} {DataCompBoy}
               $1A1B,$1B1D,$1A1A,$1B1B:
                begin HandleCommand(Event); CE := Off; end;
              end;
   evBroadcast: case Event.Command of
                  cmLookForPanels, cmGetUserParams, cmGetUserParamsWL,
                cmChangeDirectory, cmChangeDrv,
                   cmIsRightPanel: begin HandleCommand(Event); CE := Off end;
                end;
   evCommand: case Event.Command of
                cmChangeDirectory:
                  begin
                    if LTree = nil then
                    begin
                    if LSelected then
                      LeftView^.HandleEvent(Event) else
                      RightView^.HandleEvent(Event);
                    end else
                    begin
                    if not LSelected then
                      LeftView^.HandleEvent(Event) else
                      RightView^.HandleEvent(Event);
                    End;
                    Exit;
                  end;
                cmChangeTree: if LTree <> nil then LTree^.HandleEvent(Event);
                cmRereadInfo: begin
                               if LeftView <> nil then LeftView^.HandleEvent(Event);
                               if RightView <> nil then RightView^.HandleEvent(Event);
                               Exit;
                              end;
                cmCloseLinked,
                cmMakeForced,
                cmRereadForced,
                cmTotalReread,
                cmReboundPanel,
                cmRereadDir: begin
                              EV := Event;
                              LPanel^.HandleEvent(Event);
                              Event := EV;
                              RPanel^.HandleEvent(Event);
                              ClearEvent(Event);
                             end;
              cmPushName,
              cmGetDirName: begin
                             if LType = dtPanel then LeftView^.HandleEvent(Event);
                             if RType = dtPanel then RightView^.HandleEvent(Event);
                             Exit
                            end;
               cmGetDirInfo,
               cmZoom,
               cmMaxi,
               cmGetName,
               cmPostHideRight,
               cmPostHideLeft,
               cmChangeInactive,
               cmPanelCompare,
               cmDiskInfo,
{$IFDEF NETINFO} cmNetInfo, {-$VIV} {$ENDIF}
               cmLoadViewFile,
               cmGLoadViewFile,
               cmEnableView,
               cmDisableView,
               cmPushFullName,
               cmPushFirstName,
               cmPushInternalName,
               cmFindTree,
               cmRereadTree,
               cmHideLeft,
               cmHideRight,
               cmChangeLeft,
               cmChangeRight,
               cmDirTree,
               cmQuickView,
               cmSwapPanels,
               cmSwitchOther: begin HandleCommand(Event); CE := Off end;
              end;
  end;

   if (Event.What <> evNothing) and CE then
      inherited HandleEvent(Event);
end;

end.
