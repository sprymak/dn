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
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}

unit NetInfo;
interface
uses Objects, Drivers, Views, Dialogs, Dos, HideView, RStrings, advance,
     advance1, advance2, advance3, Novell;

type
     PNetInfoRec = ^TNetInfoRec;
     TNetInfoRec = record
       Title: PString;
     end;

     PNetInfo = ^TNetInfo;
     TNetInfo = object(THideView)
      Info: TNetInfoRec;
      constructor Init(R: TRect);
      constructor Load(var S: TStream);
      procedure ReadData(Loc: Boolean);
      procedure ShowView; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      function GetPalette: PPalette; virtual;
      procedure Draw; virtual;
      destructor Done; virtual;
     end;

const
    CNetInfo     = #23#24;

implementation
uses Startup, ExtraMem, DNApp, Commands, DNHelp, Tree, xTime, DnIni;

constructor TNetInfo.Load;
begin
  TObject.Init;
  inherited Load(S);
end;

constructor TNetInfo.Init;
begin
 TView.Init(R);
 HelpCtx := hcNetInfo;
 EventMask := evCommand;
 Options := Options or ofSelectable or ofTopSelect;
end;

destructor TNetInfo.Done;
begin
 DisposeStr(Info.Title);
 inherited Done;
end;

procedure TNetInfo.ShowView;
begin
 ReadData(Off);
 inherited ShowView;
end;

function TNetInfo.GetPalette;
 const S: String[Length(CNetInfo)] = CNetInfo;
begin
 GetPalette := @S;
end;

procedure TNetInfo.ReadData;
 var
     MemBlocks, I: Word;
begin
 Abort:=Off;
 DisposeStr(Info.Title);
 FillChar(Info, SizeOf(Info), 0);
 Message(Owner, evCommand, cmGetNetInfo, @Info);
end;

procedure TNetInfo.HandleEvent;
begin
 inherited HandleEvent(Event);
 if (Event.What = evCommand) then
  case Event.Command of
   cmInfoPresent: begin ClearEvent(Event); Event.InfoPtr := Owner end;
   cmRereadInfo: begin
                   ReadData(Off); DrawView; ClearEvent(Event);
                 end;
   cmLocalReread: begin
                   ReadData(On); DrawView; ClearEvent(Event);
                  end;
  end;
end;

procedure TNetInfo.Draw;
 var B: TDrawBuffer;
     C,Y: Word;
     CC: Boolean;
     count, station, retcode: integer;
     server, username: string;
     hex_id, datetime: string;
     conntype: integer;

procedure Wrt(S: String);
 var I: Integer;
begin
 MoveChar(B, ' ', C, Size.X);
 I := (Size.X-CStrLen(S)) div 2;
 if (I < 0) or CC then I := 0;
 MoveCStr(B[I], S, C);
 WriteLine(0,Y,Size.X,1,B);
 Inc(Y);
end;

begin
 C := (GetColor(1) shl 8) or (GetColor(2) and 255); Y:=0; CC:=False;
 Wrt(GetString(dlNetInfo));
 Wrt(#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196);
 cc := true;
 for count := 1 to 5 do
 begin
   get_file_server_name(count,server);
   if server <> '' then
   begin
    set_preferred_connection_id(count);
    GetServerInfo;
    GetStation(Station, retcode);
    GetConnectionInfo(Station, UserName, hex_id, conntype, datetime, retcode);
    Wrt(#218 + GetString(dlServer) + ' ~' + server + '~ ' +
              ItoS(AServerInfo^.NetwareVers)+'.'+ItoS(AServerInfo^.NetwareSubV));
    if (not NoLevelsInfo) then
      Wrt(#195'SFT level ' + ItoS(AServerInfo^.SFT_Level) + ' TTS level ' + ItoS(AServerInfo^.TTS_Level));
    Wrt(#195 + GetString(dlConnections) + ': Max ' + SStr(AServerInfo^.Connections_max, 3,' ')
                   + ' Use ' + SStr(AServerInfo^.Connections_in_use, 3,' ')
                   + ' Peak ' + SStr(AServerInfo^.peak_Connections_used, 3,' '));
    Wrt(#192'User ' + UserName + ' Connect '+SStr(Station, 3,' ')+ ' Logged '+datetime);
   end;
 end;
 cc := false;
 Wrt(#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196#196);

 MoveChar(B, ' ', C, Size.X);
 if Y <= Size.Y - 1 then WriteLine(0, Y, Size.X, Size.Y - Y + 1, B);
end;

end.
