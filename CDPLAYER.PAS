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
//  dn269-cdplayer_options_help_fix.patch
//
//  2.7.0
//  dn270-cdplayer_help_windows_fix.patch
//  dn270-add_cdplayer_help_topics.patch
//  dn328-CDPlayer(i)-Tab_fix.patch
//  dn368-cdplayer_crash_fix_for_WinXP.patch
//
//  3.7.0
//  dn370-cdplayer(if)-fix_and_improve.patch
//  dn31005-bp_to_vp_on_off_true_false.patch
//
//  4.9.0
//
//////////////////////////////////////////////////////////////////////////}
{$I STDEFINE.INC}
{Improved by Sergey Biryukov aka Flash, 2003}
UNIT CDPlayer;

{.$DEFINE UPD}

INTERFACE

USES xTime, Dialogs, Drivers, Objects, Views, Collect;

CONST
  CDplace: TPoint = ( X:0; Y:0 );
  CDsize: TPoint = ( X:0; Y:0 );
  CDparams: array[ 1..3 ] of AWord = ( 3, 0, 0 );
  CDmixer : String= '';

  CD_Player: PView = nil;

PROCEDURE RunPlayer;

{$IFDEF UPD}
PROCEDURE UpdateCDplayer;
{$ENDIF}

CONST
  cpNormal = 0;
  cpIntro  = 1;
  cpShuffle= 2;
  cpOne    = 3;
  cpOrder  = 4;

  cpAutoplay  = 1;
  cpAutoEject = 2;
  cpLockDoor  = 4;
  cpRepeat    = 8;

  cpTrack     = 0;
  cpLeftTrack = 1;
  cpDisk      = 2;
  cpLeftDisk  = 3;


TYPE
  _Status = ( noDisk, Play, Stopped, Paused, newDisk );

  _Err = record
     Stat: byte;
     Code: byte;
  end;

  _Diskinfo = record
    FirstTrack, LastTrack: byte;
    LeadStart: Longint;
  end;

  _TrackInfo = record
     case Boolean of
       True: ( CFrame: byte;
               CSec: byte;
               CMin: byte;
               CC: byte;
               CStat: byte );
       False: ( Addr: longint;
                Stat: byte )
  end;

  _TLen = record LSec, Lmin: byte end;

  _Tracks = array[ 1..100 ] of _TrackInfo;
  _Tlens  = array[ 1..100 ] of _Tlen;
  _Order  = array[ 1..100 ] of byte;

  _QChannel = record
     ADR,
     TNO,
     IDX,
     Rmin,
     RSec,
     Rfrm,
     Rzero,
     AMin,
     Asec,
     Afrm: byte;
     Stat: aword;
   end;

  _UpcCode = record
    ADR: byte;
    UPC: array[ 1..7 ] of byte;
    ZERO,
    AFrame: byte;
  end;

  PCounter = ^TCounter;
  TCounter = object( TView )
    Line: array[ 1..3 ] of string[ 30 ];
    Constructor Init( R: Trect );
    Procedure SetCounter( T, A, B: byte );
    Destructor Done; virtual;
    Procedure Draw; virtual;
    procedure Awaken; virtual;
  end;

  TReqHdr = Array [0..49] of byte;

  PCdPlayer = ^TCdPlayer;
  TCdPlayer = Object( TDialog )
    DiskOk: boolean;
    CdDisk: AInt;
    LowTrack,
    HighTrack: byte;
    DiskMin,
    DiskSec: byte;
    Tracks: _tracks;

    { new }
    DiskId    : Longint;
    DiskTitle : PString;
    Descr : PLineCollection ;
    TrkDescr : String;
    TrkOrder : String;
    TrkPlay  : String;
    { new }

    TLen: _tlens;
    Order: _Order;
    Counter: PCounter;
    Info0: PParamText;
    Info1: PParamText;
    Info2: PParamText;
    Qchannel: _Qchannel;
    UpdCnt: TEventTimer;
    NewPlay: TEventTimer;
    reqHdr: TReqHdr;
    Status: _Status;
    Err: _Err;
    LastShuffle: byte;
    Parm: array[ 1..3 ] of longint;
    Parmt: longint;
    ParmD: longint;
   { Parm0: string[ 10 ];
     Parm1: string[ 10 ]; }
    Parm2: string[ 20 ];
    CD_Loaded : boolean ;
    Constructor Init( R: Trect; ACdDisk: integer );
    Procedure   LoadDiskInfo;
    Procedure   Display( T, M, S: byte );
    Destructor  Done; virtual;
    Procedure   Update; virtual;
    Procedure   HandleEvent( var Event: Tevent );virtual;
    Procedure   Zoom; Virtual;
    Function    Valid( Command: Word ): Boolean; virtual;
    procedure   Store( var S: TStream ); virtual;
    constructor Load( var S: TStream );
    procedure   Awaken; virtual;
    procedure   SizeLimits(var Min, Max: TPoint); virtual;

  private
    ButtonRew,
    ButtonPrev,
    ButtonPlay,
    ButtonNext,
    ButtonFFwd,
    ButtonPause,
    ButtonStop,
    ButtonEject,
    ButtonSet,
    ButtonTime,
    ButtonMode,
    ButtonMix,
    ButtonOrder,
    ButtonTitle: PButton;
    CDTrack: PStaticText;
    View: PView;

    procedure  _SetOrder;

    procedure  _Play;
    procedure  _Stop;
    procedure  _Next;
    procedure  _Prev;
    procedure  _Pause;
    procedure  _Eject;
    procedure  _Load;
    procedure  _FForward;
    procedure  _FRewind;
    procedure  _Setup;
    procedure  _Mix;
    procedure  _Door( DLock: Boolean );
    procedure  _Song( ASong : byte )  ;

    function   _DiskStatus: boolean;
    function   _DiskIsChanged: boolean;

    Function MakeTrackList : PLineCollection ;
    Procedure SelectTrack ;
    Procedure SelectOrder ;
    Procedure GetDescription ;
    Function  GetOrder : boolean ;

    procedure InsertControls;
    procedure DeleteControls;
    end;

const Playing: Boolean = False;
      CurTrack: Byte = 0;

IMPLEMENTATION

USES advance, advance1, advance2, advance3, Messages, Commands, RStrings,
     DNHelp, DOS, LFN, DnApp, Startup, CDUtil, Menus
     {$IFDEF DPMI}, Dpmi, DosMem{$ENDIF}
     ;

FUNCTION CD_GetFirstDrive( var firstDrive: Integer ): integer;
{$IFNDEF DPMI}
assembler;
asm
  mov ax, $1500
  mov bx, 0
  int $2f
  mov ax, bx {;# of drives }
  les bx, firstDrive
  mov es:[ bx ].word, cx
{$ELSE}
var R: DpmiRegisters;
begin
  fillword(R, SizeOf(R) shr 1, 0);
  r.ax := $1500;
  r.bx := 0;
  SimulateRealModeInt($2F, R);
  firstDrive := R.CX;
  CD_GetFirstDrive := R.BX;
{$ENDIF}
end;

FUNCTION CD_Req( drv: integer;
                  cmd: byte;
                  size: integer;
                  Var reqHdr: TReqHdr ):boolean;
var Buf: Pointer;
{$IFDEF DPMI}
    R: DpmiRegisters;
    B: Pointer;
{$ENDIF}
begin
  reqHdr[ 1 ] := 0;
  reqHdr[ 2 ] := cmd;
  reqHdr[ 0 ] := 20;
  reqHdr[ 18 ]:= lo( size );
  reqHdr[ 19 ]:= hi( size );
{$IFNDEF DPMI}
  Buf := System.Addr( reqHdr[ 20 ] );
  move( Buf, reqHdr[ 14 ], sizeOf( Buf ));

  asm
    les bx, reqHdr
    mov cx, drv
    mov ax, $1510
    int $2f
  end;
{$ELSE}
  FillWord(R, SizeOf(R) shr 1, 0);
  B := GetDosMem(SizeOf(reqHdr), R.ES);
  Buf := Ptr(R.ES, 20);
  Move( Buf, reqHdr[ 14 ], sizeOf( Buf ));
  Move(reqHdr, B^, SizeOf(reqHdr));
  R.BX := 0;
  R.CX := Drv;
  R.AX := $1510;
  SimulateRealModeInt($2F, R);
  Move(B^, reqHdr, SizeOf(reqHdr));
  FreeDosMem(B);
{$ENDIF}

  CD_req := reqHdr[ 4 ] and $80 = $80;
end;

FUNCTION CD_IsChanged( drive: integer ): boolean;
var
   reqHdr: TReqHdr;
   Err: byte;
 begin
  Fillchar( reqHdr, SizeOf( reqHdr ), 0 );
  reqHdr[ 20 ] := 9; { Changed? cmd }
  CD_Req( drive, 3, 1, reqHdr );
  CD_IsChanged := ( reqHdr[ 21 ] = $FF );
 end;

FUNCTION CD_IsPaused( drive: integer ): boolean;
var
   reqHdr: TReqHdr;
   Err: byte;
 begin
  fillchar( reqHdr, SizeOf( reqHdr ), 0 );
  reqHdr[ 20 ] := 15; { AudioStat cmd }
  CD_Req( drive, 3, 3, reqHdr );
  CD_IsPaused := ( reqHdr[ 21 ] and 1 = 1 );
 end;


FUNCTION CD_IsDiskPresent( drive: integer ): boolean;
var
   reqHdr: TReqHdr;
   Err: byte;
   devpar: WORD;
 begin
  fillchar( reqHdr, SizeOf( reqHdr ), 0 );
  reqHdr[ 20 ] := 6; { DiskStat cmd }
  Err := 0;
  if CD_Req( drive, 3, 5, reqHdr ) then Err := reqHdr[ 3 ];
  move( reqHdr[ 21 ], devpar, sizeOf( devPar ));
{  CD_IsDiskPresent := (( reqHdr[ 22 ] and $1 ) = 1 ) and}
   CD_IsDiskPresent := ( DevPar and { $801} 1 = 0 ) and
   (( Err <> 2 ) and ( Err <> 12 ))
 end;

PROCEDURE CD_DiskInfo( drive: integer; var buf );
var
   reqHdr: TReqHdr;
   Err: byte;
 begin
  fillchar( reqHdr, SizeOf( reqHdr ), 0 );
  reqHdr[ 20 ] := 10; { Disk Info cmd }
  CD_Req( drive, 3, 7, reqHdr );
  move( reqHdr[ 21 ], buf, 6 );
end;

PROCEDURE CD_TrackInfo( drive, track: integer; var buf );
var
   reqHdr: TReqHdr;
   Err: byte;
 begin
  fillchar( reqHdr, SizeOf( reqHdr ), 0 );
  reqHdr[ 20 ]:= 11; { Track Info cmd }
  reqHdr[ 21 ]:= track;
  CD_Req( drive, 3, 7, reqHdr );
  move( reqHdr[ 22 ], buf, 5 );
end;

FUNCTION CD_Stop( drv: integer; var reqHdr: TReqHdr ): word;
var
   statBuf: array[ 0..19 ] of byte;
   C: word;
   CC: array[ 1..2 ] of byte absolute C;
{$IFDEF DPMI}
    R: DpmiRegisters;
    B: Pointer;
{$ENDIF}
begin
  fillchar( reqHdr, SizeOf( reqHdr ), 10 );

  reqHdr[ 1 ]:=0;
  reqHdr[ 2 ]:=133;
  reqHdr[ 0 ]:=$5;

{$IFNDEF DPMI}
  asm
    les bx, reqHdr
    mov cx, drv
    mov ax, $1510
    int $2f
  end;
{$ELSE}
  FillWord(R, SizeOf(R) shr 1, 0);
  B := GetDosMem(SizeOf(reqHdr), R.ES);
  Move(reqHdr, B^, SizeOf(reqHdr));
  R.BX := 0;
  R.CX := Drv;
  R.AX := $1510;
  SimulateRealModeInt($2F, R);
  Move(B^, reqHdr, SizeOf(reqHdr));
  FreeDosMem(B);
{$ENDIF}

 Move( reqHdr[ 3 ], CC, 2 );

 CD_Stop := C;
 Playing := False;
end;

FUNCTION CD_Play( drv: integer; FromTrack, toTrack: _TrackInfo;
                  var reqHdr: TReqHdr ): boolean;

var
   DM, DS, DF: integer;
   TL: longint;
{$IFDEF DPMI}
    R: DpmiRegisters;
    B: Pointer;
{$ENDIF}

 begin
  fillchar( reqHdr, SizeOf( reqHdr ), $12 );
  CD_Play := false;
 if ( fromTrack.stat and $40 > 0 ) then Exit;
 if not ( CD_IsPaused( drv )) then CD_Stop( drv, reqHdr ); { Always stop before playing! }
 reqHdr[ 1 ]  := 0;
 reqHdr[ 2 ]  := 132;
 reqHdr[ 0 ]  :=$16;
 reqHdr[ $d ] := 1; { Red book address mode }
 move( FromTrack, reqHdr[ $E ], 4 );
 DM :=  toTrack.CMin - FromTrack.Cmin;
 DS :=  toTrack.CSec - FromTrack.CSec;
 DF :=  toTrack.CFrame - FromTrack.CFrame;
 if DF < 0 then begin  Dec( DS ); inc( DF, 75 ) end;
 if DS < 0 then begin  Dec( DM ); inc( DS, 60 ) end;
 TL := Longint( Longint( DM ) * 4500 + Longint( DS ) * 75 + DF - 150 );
 move( TL, reqHdr[ $12 ] , 4 );

{$IFNDEF DPMI}
  asm
    les bx, reqHdr
    mov cx, drv
    mov ax, $1510
    int $2f
  end;
{$ELSE}
  FillWord(R, SizeOf(R) shr 1, 0);
  B := GetDosMem(SizeOf(reqHdr), R.ES);
  Move(reqHdr, B^, SizeOf(reqHdr));
  R.BX := 0;
  R.CX := Drv;
  R.AX := $1510;
  SimulateRealModeInt($2F, R);
  Move(B^, reqHdr, SizeOf(reqHdr));
  FreeDosMem(B);
{$ENDIF}
  Playing := True;
 end;




FUNCTION CD_GetDriverVersion:integer;
{$IFNDEF DPMI}
 assembler;
 asm
  mov ax, $150C
  int $2f
  mov ax, bx
{$ELSE}
 var R: DpmiRegisters;
 begin
  FillWord(R, SizeOf(R) shr 1, 0);
  R.AX:=$150C;
  SimulateRealModeInt($2F, R);
  CD_GetDriverVersion:=R.BX;
{$ENDIF}
 end;


PROCEDURE  TimeSub( var M1, S1: integer; M2, S2: byte );
    begin
      Inc( S1, 60 );
      Dec( S1, S2 );
      Dec( M1, M2 );
      if S1>59 then DEC( S1, 60 )
               else DEC( M1 );
     end;

PROCEDURE  TimeAdd( var M1, S1: integer; M2, S2: byte );
    begin
      Inc( S1, S2 );
      Inc( M1, M2 );
      if S1>59 then begin
                     DEC( S1, 60 );
                     Inc( M1 );
                    end;
     end;


PROCEDURE GetDigit( Dig: byte; var S: string );
begin
  case Dig of
       0: S := '‹‹‹€ €€‹€';
       1: S := '  ‹  €  €';
       2: S := '‹‹‹‹‹€€‹‹';
       3: S := '‹‹‹‹‹€‹‹€';
       4: S := '‹ ‹€‹€  €';
       5: S := '‹‹‹€‹‹‹‹€';
       6: S := '‹‹‹€‹‹€‹€';
       7: S := '‹‹‹  €  €';
       8: S := '‹‹‹€‹€€‹€';
       9: S := '‹‹‹€‹€‹‹€';
{n}  $6E: S := '   ‹‹‹€ €';
{o}  $6F: S := '   ‹‹‹€‹€';
{C}  $43: S := '   ‹‹‹€‹‹';
{D}  $44: S := '   ‹‹ €‹€';

     else S := '         ';
  end;
end;

  Constructor TCounter.Init;
  var I: byte;
        begin
            R.B.Y := R.A.Y + 3;
            R.B.X := R.A.X + 18 + 9;
            inherited Init( R );
            for I := 1 to 3 do Line[ i ] := Strg(' ', Pred( SizeOf( Line[ i ] )));

         end;

PROCEDURE TCounter.Awaken;
    var I: byte;
  begin
    inherited Awaken;
    for I := 1 to 3 do Line[ i ] := Strg(' ', Pred( SizeOf( Line[ i ] )));
  end;

PROCEDURE TCounter.SetCounter( T, A, B: byte );
  var S: String[ 9 ];
      I: byte;
PROCEDURE InsertText( From: byte );
   var I: byte;
    begin
      for I := 1 to 3 do Move( S[ pred( I )*3+1 ], Line[ I, From ], 3 );
    end;
      begin
      for I:=1 to 3 do fillChar( Line[ I, 1 ], Length( Line[ I ] ), 32 );
      if T < $FF then
        begin
           GetDigit( T div 10, S ); InsertText( 1 );
           GetDigit( T mod 10, S ); InsertText( 5 );
           if T > 0 then
           begin
            S := Strg(' ', 9 ); if A > 9 then GetDigit( A div 10, S );
            InsertText( 10 );
            GetDigit( A mod 10, S ); InsertText( 14 );
            GetDigit( B div 10, S ); InsertText( 20 );
            GetDigit( B mod 10, S ); InsertText( 24 );
            Line[ 2, 9+9 ] := #223;
            Line[ 3, 9+9 ] := #223;
           end;
         end
        else
         begin
           GetDigit( $6E, S ); InsertText( 1 );
           GetDigit( $6F, S ); InsertText( 5 );

           GetDigit( $43, S ); InsertText( 10 );
           GetDigit( $44, S ); InsertText( 14 );
         end;
      end;
PROCEDURE TCounter.Draw;
  var C, I: byte;
      T: TDrawBuffer;
      begin
      C  := GetColor( 1 );
      for I := 0 to Pred( Size.Y ) do
       begin
        MoveStr( T, Line[ I+1 ], C );
        WriteLine( 0, I, Size.X, Succ( I ), T );
       end;
      end;
  Destructor TCounter.Done;
      begin
        Inherited Done;
      end;

CONSTRUCTOR TCdPlayer.Init;
var
  R1: Trect;
  TR : word ;
begin
 {if CD_GetFirstDrive( cdDrive ) = 0 then Fail;}
  R1 := R;
  R1.B.X := R1.A.X + 36;
  R1.B.Y := R1.A.Y + 9;

  if R.B.Y - R.A.Y > 9 then begin
    R.B.X := R.A.X + 36;
    R.B.Y := R.A.Y + {10}15;
  end else
    R := R1;
    {R.Assign( 1, 4, 35, 9 );  }   { 34 7 59 12 }
    {R.Assign( 1, 4, 60, 14 ); }
  inherited Init( R, GetString( dlCDPlayer ));
  Number := GetNum;
  {Options := Options or ofCentered;}
  Flags := Flags or wfZoom;

  ZoomRect := R1;
  CdDisk := ACDdisk;

  {$ifdef upd}
  NewTimer( UpdCnt, 18 );
  {$else}
  UpdTicks := 18;
  RegisterToBackGround( @Self );
  {$endif}

  HelpCTX := hcCDplayer;

    DiskId    := 0 ;
    DiskTitle := Nil;
    New(Descr,Init(100,10));
    for TR :=1 to 100 do Descr^.Insert(Nil);

  InsertControls;
  LoadDiskInfo;
  if (LowTrack = 0) and (HighTrack = 0) then
   begin MessageBox(GetString(dlCDNoDiskInfo),nil,mfOKButton);; Exit; end;
  if CurTrack = 0 then CurTrack := 1; { Flash 07-07-2003 }
end;

PROCEDURE TCdPlayer.Awaken;
begin
  inherited Awaken;
end;

PROCEDURE TCdPlayer.Store;
begin
  DeleteControls;
  inherited Store( S );
  PutSubViewPtr( S, Counter );
  PutSubViewPtr( S, Info0 );
  PutSubViewPtr( S, Info1 );
  PutSubViewPtr( S, Info2 );
  S.Write( CurTrack, SizeOf( CurTrack ));
  S.Write( Order, SizeOf( Order ));
  S.Write( LastShuffle, SizeOf( LastShuffle ));
  S.Write( CDParams, SizeOf( CDParams));
  S.Write( CdDisk, SizeOf(CdDisk) );

  {!!SF}
  S.Write( DiskId , SizeOF(DiskId) ) ;
  S.WriteStr( DiskTitle );
  S.Put( Descr );
  S.WriteStr( @TrkOrder );
  S.WriteStr( @TrkPlay );

 end;

constructor TCdPlayer.Load;
var P : PString;
begin
  inherited Load( S );
  GetSubViewPtr( S, Counter );
  GetSubViewPtr( S, Info0 );
  GetSubViewPtr( S, Info1 );
  GetSubViewPtr( S, Info2 );
  S.Read( CurTrack, SizeOf( CurTrack ));
  S.Read( Order, SizeOf( Order ));
  S.Read( LastShuffle, SizeOf( LastShuffle ));
  S.Read( CDParams, SizeOf( CDParams ));
  S.Read( CdDisk, SizeOf(CdDisk) );

  {!!SF}
  S.Read( DiskId , SizeOF(DiskId) ) ;
  DiskTitle := S.ReadStr;
  Descr := PLineCollection(S.Get);
  P := S.ReadStr; TrkOrder := CnvString(P); DisposeStr(P);
  P := S.ReadStr; TrkPlay := CnvString(P); DisposeStr(P);

  UpdTicks := 18;
  RegisterToBackGround( @Self );
  CD_Player := @Self;
  {Coder := GlobalMessage( evCommand, cmXdata+1, nil );}
  LoadDiskInfo;
  { Flash 07-07-2003 >>> }
  InsertControls;
  if not Playing then Display( Succ( HighTrack-LowTrack ), DiskMin, DiskSec );
  if (opSys and opWNT) = opWNT then CurTrack := 1;
  { Flash 07-07-2003 <<< }
  Update;
  Redraw;
end;

{ Flash 07-07-2003 >>> }
procedure TCDPlayer.DeleteControls;
begin
Delete(ButtonRew);
Delete(ButtonPrev);
Delete(ButtonPlay);
Delete(ButtonNext);
Delete(ButtonFFwd);
Delete(ButtonPause);
Delete(ButtonStop);
Delete(ButtonEject);
Delete(ButtonSet);
Delete(ButtonTime);
Delete(ButtonMode);
Delete(ButtonMix);
Delete(ButtonOrder);
Delete(ButtonTitle);
Delete(Counter);
Delete(CDTrack);
Delete(Info0);
Delete(Info1);
Delete(Info2);
end;

procedure TCDPlayer.InsertControls;
var R: TRect;
begin
  R.Assign( 1, 6, 7, 8 );
  ButtonRew := New( PButton, Init( R, GetString(dlCDButtonRew), cdFRew, bfBroadcast ));
  Insert( ButtonRew );

  R.Assign( 7, 6, 12, 8 );
  ButtonPrev := New( PButton, Init( R, GetString(dlCDButtonPrev), cdPrev, bfBroadcast ));
  Insert( ButtonPrev );

  R.Assign( 12, 6, 23, 8 );
  ButtonPlay := New( PButton, Init( R, GetString(dlCDButtonPlay), cdPlay, bfBroadcast ));
  Insert( ButtonPlay );

  R.Assign( 23, 6, 28, 8 );
  ButtonNext := New( PButton, Init( R, GetString(dlCDButtonNext), cdNext, bfBroadcast ));
  Insert( ButtonNext );

  R.Assign( 28, 6, 34, 8 );
  ButtonFFwd := New( PButton, Init( R, GetString(dlCDButtonFFwd), cdFFwd, bfBroadcast ));
  Insert( ButtonFFwd );

  R.Assign( 1, 8, 10, 10 );
  ButtonPause := New( PButton, Init( R, GetString(dlCDButtonPause), cdPause, bfBroadCast ));
  Insert( ButtonPause );

  R.Move(9,0); Inc(R.B.X,6);
  ButtonStop := New( PButton, Init( R, GetString(dlCDButtonStop), cdStop, bfBroadcast ));
  Insert( ButtonStop );

  R.Move(15,0); Dec(R.B.X,6);
  ButtonEject := New( PButton, Init( R, GetString(dlCDButtonEject), cdEject, bfBroadCast ));
  Insert( ButtonEject );

  if Size.Y < 10 then
   begin ButtonPause^.Hide; ButtonStop^.Hide; ButtonEject^.Hide; end;

  R.Assign( 1,  10, 12,  12 );
  ButtonSet := New( PButton, Init( R, GetString(dlCDButtonSet), cdSetup, bfBroadCast ));
  Insert( ButtonSet );

  R.Move( 11, 0 );
  ButtonTime := New( PButton, Init( R, GetString(dlCDButtonTime), cdDisplay, bfBroadCast ));
  Insert( ButtonTime );

  R.Move( 11, 0 );
  ButtonMode := New( PButton, Init( R, GetString(dlCDButtonMode), cdMode, bfBroadCast ));
  Insert( ButtonMode );

  R.Assign( 1, 12, 12,  14 );
  ButtonMix := New( PButton, Init( R, GetString(dlCDButtonMix), cdMix, bfBroadCast ));
  Insert( ButtonMix );

  R.Move( 11, 0 );
  ButtonOrder := New( PButton, Init( R, GetString(dlCDButtonOrder), cdOrder, bfBroadCast ));
  Insert( ButtonOrder );

  R.Move( 11, 0 );
  ButtonTitle := New( PButton, Init( R, GetString(dlCDButtonTitle), cdTitle, bfBroadCast ));
  Insert( ButtonTitle );

  R.Assign( 2, 2, 34, 5 );
  View := New( PView, Init( R ));
  View^.Options := View^.Options or ofFramed;
  Insert( View );

  R.Assign( 4, 2, 32, 3 );
  Counter := New( PCounter, Init( R ));
  {Counter^.Options := Counter^.Options or ofFramed;}
  Insert( Counter );

  R.Assign( 2,5,2,6 );
  New( Info2, Init( R, '%s', 1 ));
  Insert( Info2 );

  R.Assign( 33, 2, 34, 5 );
  New( Info0, Init( R,'%c'+^M +'%c'+^M+'%c',3));
  {Info0^.Options := Info0^.Options or ofFramed;}
  Insert( Info0 );

  FreeStr := GetString(dlCDTrack);
  R.Assign( 11-Length(FreeStr), 1, 11, 2 );
  CDTrack := New( PStaticText, Init( R, FreeStr ));
  Insert( CDTrack );

  R.Move( 13, 0 ); R.B.X := R.A.X + 4;
  New( Info1, Init( R, '%s', 1 ));
  Insert( Info1 );

  SelectNext( False );
  SelectNext( False );
  SelectNext( False );
end;
{ Flash 07-07-2003 <<< }

DESTRUCTOR TcdPlayer.Done;
begin
  DeleteControls;
  ConfigModified := not (Origin.Equals(CDSize)) or not (CDSize.Equals(Size));
  CDplace := Origin;
  CDsize  := Size;
  CD_Player := Nil;
  DisposeStr( DiskTitle );
  Dispose( Descr,Done );

  Inherited Done;
end;


   Function TCdPlayer.MakeTrackList : PLineCollection ;
    var C : PLineCollection;
        I : word ;
     begin
       New(C,Init(10,10));
        for I := LowTrack to HighTrack do
         C^.Insert(NewStr(
                 SStr( I- LowTrack +1 ,2,' ' ) + ' '#179' ' +
               + SStr( Tlen[I].Lmin,2,' ' ) + ':' +
               + SStr( Tlen[I].Lsec,2,'0' ) + ' '#179' ' +
               CnvString(Descr^.At(I))
         ));
      MakeTrackList := C ;
     end;

    Procedure TCdPlayer.SelectTrack ;
     var T : _OrderRec ;
         s : string ;
         I : byte ;
         P : PDialog ;
      begin
       if status=noDisk then Exit ;
       S := '';
       for  I:= LowTrack to HighTrack do AddStr(S, char(I));

       with T do begin
                  pCD := @Self ;
                  Selection := QChannel.TNO - LowTrack;
                  pOrder := @S ;
                 end;

        { Flash >>> }
        P := PDialog(New(POrder,Init(False, Succ(HighTrack-LowTrack) ,'')));
        P^.HelpCtx:=hcCDPlayerAddSong;
        P^.SetData(T);
        if Desktop^.ExecView(P)=cmOk
        { Flash <<< }
          then begin P^.GetData(T); _Song( T.Selection+1 ); end;
      end;

    Procedure TCdPlayer.SelectOrder ;
     var T : _OrderRec ;
         s : string ;
         I : byte ;
         P : PDialog ;
      begin
       S := trkOrder ;
       {for  I:= LowTrack to HighTrack do AddStr(S, char(I));}

       T.Selection := 0;

       if (S<>'') and (CDparams[ 2 ] = cpOrder) then
       T.Selection := Length(S) - Length(TrkPlay);

       with T do begin
                  pCD := @Self ;
                  pOrder := @S ;
                 end;

        { Flash >>> }
        P := PDialog(New(POrder,Init(True,Length(S),'')));
        P^.HelpCtx:=hcCDPlayerOrder;
        P^.SetData(T);
        if Desktop^.ExecView(P)=cmOk
        { Flash <<< }
          then begin
                 P^.GetData(T);
                 trkOrder := S;
                 trkPlay  := S;
                 {_Song( T.Selection+1 );}
                 CDparams[ 2 ] := cpOrder ;
                 _Stop;
                 _Play;
               end;
      end;


PROCEDURE TcdPlayer.Update;
  FUNCTION CheckEject: boolean;
  begin
    CheckEject := False;
    if CDparams[ 1 ] and cpRepeat > 0 then Exit;
    if CDparams[ 1 ] and cpAutoEject > 0 then
        begin
           _Eject;
           CheckEject := True;
           Exit;
        end;
    _Stop;
    CheckEject := True;
  end;

  FUNCTION Reached( M, S: byte ):boolean;
  var DT, DS: integer;
  begin
    with Qchannel do begin
       DT := M*60 + S;
       DS := AMin*60 + Asec;
       Reached := ( DS+2>=DT );
    end;
  end;

  PROCEDURE NormalPlay;
  begin
    with Qchannel do begin
      TNO := CurTrack; { Flash 07-07-2003 }
      if TNO < HighTrack then Exit;
      with Tracks[ HighTrack+1 ] do if not Reached( CMin, CSec ) then Exit;
      if checkEject then Exit;
      CD_Play( CDdisk, tracks[ LowTrack ], tracks[ HighTrack+1 ], reqHdr );
    end;
  end;

  PROCEDURE IntroPlay;
  var TTo: byte;
  begin
    if not TimerExpired( NewPlay ) then Exit;
    with Qchannel do begin
      TNO := CurTrack; Inc(CurTrack); { Flash 07-07-2003 }
      TTO := TNO + 1;
      if TTO > ( HighTrack ) then begin
        if CheckEject then Exit;
        TTO := HighTrack;
        CD_Play( CDdisk, tracks[ LowTrack ], tracks[ LowTrack+1 ], reqHDr );
      end else
        CD_Play( CDdisk, tracks[ TNO + 1 ], tracks[ TTO + 1 ], reqHdr );
      NewTimerSecs( NewPlay, 17 );
    end;
  end;

  PROCEDURE ShufflePlay;
  var I: byte;
  begin
    with Qchannel do begin
      with Tracks[ LastShuffle+1 ] do if not Reached( CMin, CSec ) then Exit;
      TNO := CurTrack; { Flash 07-07-2003 }
      for I:= LowTrack to HighTrack do
        if TNO = Order[ I ] then Break;
      if not TimerExpired( NewPlay ) Then Exit;
      if I = HighTrack then begin
        if CheckEject then Exit;
        I := LowTrack;
      end else
        Inc( I );
      I := Order[ I ];
      CD_Play( CDdisk, tracks[ I ], tracks[ I+1 ], reqHdr );
      NewTimerSecs( NewPlay, 3 );
      LastShuffle := I; CurTrack := I; { Flash 07-07-2003 }
    end;
  end;

  PROCEDURE OnePlay;
  var I: byte;
  begin
    with Qchannel do begin
      TNO := CurTrack; { Flash 07-07-2003 }
      with Tracks[ LastShuffle+1 ] do if not Reached( CMin, CSec ) then Exit;
      if not TimerExpired( NewPlay ) Then Exit;
      {if TNO = LastShuffle+1 then } begin
        NewTimerSecs( NewPlay, 3 );
        if CheckEject then Exit;
        CD_Play( CDdisk, tracks[ TNO ], tracks[ TNO+1 ], reqHdr );
        LastShuffle := TNO;
      end
    end;
  end;

  PROCEDURE OrderPlay;
  var I: byte;
  begin
    with Qchannel do begin
      with Tracks[ LastShuffle+1 ] do if not Reached( CMin, CSec ) then Exit;
      if not TimerExpired( NewPlay ) Then Exit;
      DelFC(TrkPlay);
      if TrkPlay='' then begin
        if CheckEject then Exit;
        TrkPlay := TrkOrder ;
      end;
       I := byte(TrkPlay[ 1 ]);
      CD_Play( CDdisk, tracks[ I ], tracks[ I+1 ], reqHdr );
      NewTimerSecs( NewPlay, 3 );
      LastShuffle := I;
    end;
  end;

begin
  {$ifdef upd}
  if not TimerExpired( UpdCnt ) then Exit;
  NewTimerSecs( UpdCnt, 1 );
  {$endif}

  DiskOk := not _DiskStatus;
  {if not DiskOk then Exit;}

  if ( Status = noDisk ) and DiskOk then
    {if  _DiskIsChanged then} begin
      LoadDiskInfo;
      if _DiskStatus then begin
                           Status := NewDisk;
                           NewTimerSecs( NewPlay, 5 );
                          end;
    end;
    {
    else begin
      Status := Stopped;
      Display( Succ( HighTrack-LowTrack ), DiskMin, DiskSec );
    end;
    }

  if Status = Play then case CDparams[ 2 ] of
    cpNormal: NormalPlay;
    cpIntro: IntroPlay;
    cpShuffle: ShufflePlay;
    cpOne: OnePlay;
    cpOrder: if TrkOrder<>'' then OrderPlay
                             else NormalPlay;
   end;

  with Qchannel do case Status of
    Play, Paused: Display( TNO, RMin, RSec );
    noDisk: Display( $FF, 0, 0 );
    NewDisk: if TimerExpired( NewPlay ) then
               if ( CDparams[ 1 ] and cpAutoPlay > 0 ) then _Play;
  end;

end;

procedure TcdPlayer.SizeLimits;
begin
  Min.X := 36;
  Min.Y := 7;
  Max.X := 36;{59}
  Max.Y := 15;{10}
end;

PROCEDURE TcdPlayer.Display;
var DS, DM: integer;
    I : word ;
    PP: Pstring ;
    Tdscr : string;

  PROCEDURE GetShAcc;
  var I: byte;
  begin
    with  Qchannel do
    begin
      DS := 0;
      DM := 0;
      for I := LowTrack to HighTrack do
      with Tlen[ Order[ I ] ] do begin
        if TNO = Order[ I ] then Break;
        TimeAdd( DM, DS, LMin, LSec );
      end;
      TimeAdd( DM, DS, M, S );
    end;
  end;

  PROCEDURE GetShDcc;
  var I: byte;
  begin
    with  Qchannel do begin
      DS := 0;
      DM := 0;
      for I := HighTrack downto LowTrack do with Tlen[ Order[ I ] ] do begin
        TimeAdd( DM, DS, LMin, LSec );
        if TNO = Order[ I ] then Break;
      end;
      TimeSub( DM, DS, M, S );
      end;
    end;

  PROCEDURE GetOrAcc;
  var I: byte;
      C : Integer ;
  begin
    with  Qchannel do
    begin
      DS := 0;
      DM := 0;
    C := Length(TrkOrder)-Length(TrkPlay);
      if C>1 then
      for I := 1 to C do
      with Tlen[ byte(TrkOrder[I]) ] do
        TimeAdd( DM, DS, LMin, LSec );
      TimeAdd( DM, DS, M, S );
    end;
  end;

 PROCEDURE GetOrDcc;
  var I: byte;
      C : Integer ;
  begin
    with  Qchannel do
    begin
      DS := 0;
      DM := 0;
    C := Length(TrkPlay);
      if C>0 then
      for I := C downto 1 do
      with Tlen[ byte(TrkPlay[I]) ] do
        TimeAdd( DM, DS, LMin, LSec );
      TimeSub( DM, DS, M, S );
    end;
  end;

  var C1,C2,C3 : char ;

begin
  if Counter = nil then exit;

  if SkyVisible then Exit;

  if Info0 <> nil then begin
    C2 :=' ';
    Case CDparams[ 2 ] of
      cpNormal: C2 := 'N';
      cpIntro:  C2 := 'I';
      cpShuffle: C2 := 'S';
      cpOne:    C2 := '1';
      cpOrder: if (TrkOrder<>'') then C2 := 'O'
                                 else C2 := 'N';
    end;
    C1 := ' ';
    if CDparams[ 1 ] and cpAutoPlay > 0 then C1 :='A'; { 'Auto' }
    if CDparams[ 1 ] and cpRepeat > 0 then C1 :='R';  {'Repeat'}
    C3 := ' ';
    if CDparams[ 1 ] and cpAutoEject > 0 then C3 :='J';
    if CDparams[ 1 ] and cpLockDoor  > 0 then C3 :='L';
    Parm[ 1 ] := Longint( c1 );
    Parm[ 2 ] := Longint( c2 );
    Parm[ 3 ] := Longint( c3 );
    Info0^.SetData( Parm );
  end;


  if Info1 <> nil then begin
    Case CdParams[ 3 ] of
      0: Parm2 := GetString(dlCDTrackTime);
      1: Parm2 := GetString(dlCDTrackLeft);
      2: Parm2 := GetString(dlCDDiskTime);
      3: Parm2 := GetString(dlCDDiskLeft);
    end;
    if (opSys and opWNT) = opWNT then Parm2 := GetString(dlCDTrackLength);
    Parmt := Longint( @Parm2 );
    with Info1^ do begin
      GrowTo( Length( Parm2 ), 1 );
      SetData( ParmT );
    end;

   if Info2<>nil then
    begin
     if (Status in [play,paused]) and (Descr <> nil)
        and (Descr^.Count <> 0) and (Qchannel.TNO-LowTrack >= 0)
     then Tdscr := CnvString(Descr^.At( Qchannel.TNO-LowTrack ))
     else Tdscr := '';
     if Pos(';',Tdscr)>0 then system.Delete(Tdscr,Pos(';',Tdscr),255);
     ParmD := Longint( @TrkDescr );
     if TrkDescr<>Tdscr then
      begin
        TrkDescr := Tdscr ;
        Info2^.GrowTo( Length( TrkDescr ),1);
        Info2^.SetData( ParmD );
      end;

    end;

  end;
  if not (Status in [Play,Paused]) then begin
    Counter^.SetCounter( T, M, S );
    Counter^.DrawView;
    Exit;
  end;
  with QChannel do begin
    if (opSys and opWNT) = opWNT then
     begin DS := Tlen[TNO+1].Lsec; DM := Tlen[TNO+1].Lmin; end else
    case CDparams[ 3 ] of
      cpTrack: begin
          DS := S;
          DM := M;
        end;
      cpLeftTrack: begin
          DS := tracks[ TNO+1 ].CSec - ASec;
          DM := tracks[ TNO+1 ].CMin - AMin;
        end;
      cpDisk: begin
          DS := ASec;
          DM := AMin;
          if CDparams[ 2 ] = cpShuffle then GetShAcc
           else if CDparams[ 2 ] = cpOrder then GetOrAcc;
        end;
      cpLeftDisk: begin
          DS := DiskSec - Asec;
          DM := DiskMin - Amin;
          if CDparams[ 2 ] = cpShuffle then GetShDcc
          else if CDparams[ 2 ] = cpOrder then GetOrDcc;
        end;
    end{ case };
  end;
  if DS < 0 then begin
    Dec( DM );
    Inc( DS, 60 );
  end;
   if DM > 99
     then Counter^.SetCounter( T, DM div 60, DM mod 60 )
     else Counter^.SetCounter( T, DM, DS );
  Counter^.DrawView;
end;

 Procedure TcdPlayer.GetDescription ;
 var F : PTextReader ;
     T,S : String;
     T1 : string[2];
     TR : Word ;
  Begin

    DisposeStr(title);
    Title := NewStr( GetString( dlCDPlayer ) );
    Frame^.DrawView;

    DisposeStr(DiskTitle);
    DiskTitle := Nil ;
    Descr^.FreeAll ;

    for TR :=1 to 100 do Descr^.Insert(Nil);

    T := SourceDir + 'CD\' + UpStrg(Hex8(DiskID)) + '.CD'; {DataCompBoy}
    F := New(PTextReader, Init(T));
    if F = nil then Exit;

     while Not F^.EOF do
      begin
          S := F^.GetStr;;
          if S<>'' then
           begin
            if UpStrg(Copy(S,1,6))='TITLE:' then DiskTitle := NewStr(Copy(S,7,255))
            else
              if Pos(':',S)=3 then
               begin
                  T1 := Copy(S,1,2);
                  System.Delete(S,1,3); DelLeft(S); DelRight(S);
                  if T1[1] = '0' then DelFC(T);
                  TR := StoI(T1);
                  if (TR>0) and (TR<100) then
                   begin
                    if (Descr^.At(TR-1)<>Nil) then Descr^.AtFree(TR-1);
                    Descr^.AtInsert(TR-1,NewStr(S))
                   end;
               end;
           end;
      end;
     Dispose(F,Done);

    if CnvString(DiskTitle)<>'' then
     begin
        DisposeStr(Title);
        Title := NewStr(CnvString(DiskTitle));
        DelLeft(Title^); DelRight(Title^);
        Frame^.DrawView;
     end;

  End;

Function TcdPlayer.GetOrder ;
 var F : PTextReader ;
     T,S : String;
     T1 : string[2];
     TR : Byte ;
     NewOrder : String ;
  Begin

    GetOrder := False ;
    NewOrder := '';

    T := SourceDir + 'CD\' + Hex8(DiskID) + '.TRK'; {DataCompBoy}

    F := New(PTextReader, Init(T));
    if F = nil then Exit;


     while Not F^.Eof do
      begin
          S := F^.GetStr;
          if S<>'' then
               begin
                  TR := StoI(S);
                  if (TR>0) and (TR<100) then
                   AddStr(NewOrder,char((TR-1)+LowTrack));
               end;
      end;
     Dispose(F,Done);

  if NewOrder<>'' then
     begin
         GetOrder := True ;
         trkOrder := NewOrder ;
         trkPlay  := NewOrder ;
     end;
  End;



PROCEDURE TcdPlayer.LoadDiskInfo;
var DD: _DiskInfo;
  I: byte;
  DS, DM: integer;
begin
  if CD_ISdiskPresent( CDdisk ) then begin
    CD_DiskInfo( CDdisk, DD );
    with DD do begin
      LowTrack  := FirstTrack;
      HighTrack := LastTrack;
      { Flash 07-07-2003 >>> }
      if LowTrack > 1 then
       begin
       LowTrack := 0; FirstTrack := 0;
       HighTrack := 0; LastTrack := 0;
       end;
      { Flash 07-07-2003 <<< }

      for I := FirstTrack to LastTrack do
        CD_TrackInfo( CDdisk, i, Tracks[ i ] ); { read tracks info }
      Tracks[ LastTrack+1 ].addr := LeadStart;
      DiskMin := Tracks[ LastTrack+1 ].CMin;
      DiskSec := Tracks[ LastTrack+1 ].CSec;

      for I := FirstTrack to LastTrack+1 do with TLen[ i ] do begin
        DM := Tracks[ I+1 ].CMin;
        DS := Tracks[ I+1 ].CSec;
        with Tracks[ I ] do
          TimeSub( DM, DS, CMin, CSec );
          LMin := DM;
          LSec := DS;
        end;
      end;

      Display( Succ( HighTrack-LowTrack ), DiskMin, DiskSec );
      Status := Stopped;

      DiskOk := not _DiskStatus;

      if DiskOk then with Qchannel do begin
        if ( TNO > 0 ) and (( RSec <> 0 ) or ( RMin<>0 )) or Playing
          then begin Status := Play; Playing := True; end
          else begin
            Status := NewDisk;
            NewTimerSecs( NewPlay, 5 );
          end;
      if CDparams[ 1 ] and cpLockDoor > 0 then _Door( True );
    end; { with }

    { calcDiskID }
     DiskID := GetCrc( 0,
                       Tracks[ LowTrack ] ,
                       SizeOF(_TrackInfo ) * Succ(HighTrack-LowTrack)) ;
     GetDescription;
     _SetOrder;

  end else begin
    Display( $FF, 0, 0 );
    Status := NoDisk;
  end;
end;

PROCEDURE TcdPlayer._Play;
begin
  if Status = noDisk then Exit;
  with QChannel do begin
    { Flash 07-07-2003 >>> }
    if CDParams[2] = cpShuffle then
     begin TNO := order[ lowTrack ]; CurTrack := TNO; end;
    { Flash 07-07-2003 <<< }
    if Status = Play then begin
         RMin := 0;
         RSec := 0;
         Display( TNO, Rmin, Rsec );
         CD_Play( CDdisk, tracks[ TNO ], tracks[ highTrack+1 ] ,
                  reqHDr );
    end else case CDparams[ 2 ] of
      cpOne,
      cpNormal,
      cpShuffle: begin
          CD_Play( CDdisk, tracks[ order[ lowTrack ] ], tracks[ highTrack+1 ], reqHdr );
          LastShuffle := order[ lowTrack ];
        end;
      cpIntro: begin
          CD_Play( CDdisk, tracks[ order[ lowTrack ] ], tracks[ highTrack+1 ], reqHdr );
          LastShuffle := order[ lowTrack ];
          NewTimerSecs( NewPlay, 17 );
        end;
      cpOrder:
       if trkOrder<>'' then
        begin
          TrkPlay := TrkOrder ;
          CD_Play( CDdisk, tracks[ byte(Trkorder[1]) ],
                           tracks[ byte(Trkorder[1])+1 ], reqHdr );
          LastShuffle := byte(Trkorder[1]);
        end
         else

        begin
          CD_Play( CDdisk, tracks[ order[ lowTrack ] ], tracks[ highTrack+1 ], reqHdr );
          LastShuffle := order[ lowTrack ];
        end;

    end;
  end;
  Status := Play;
end;

PROCEDURE TcdPlayer._Stop;
begin
  Display( 0, $CC, $CC );
  CD_Stop( CDDISK, reqHdr );
  Status := Stopped;
end;

PROCEDURE TcdPlayer._Next;
begin
  if not DiskOk then Exit;
  with QChannel do begin
    if TNO >= HighTrack then begin _Stop; Exit end;
    TNO := CurTrack; Inc(CurTrack); { Flash 07-07-2003 }
    Rmin := 0;
    Rsec := 0;
    Display( TNO+1, 0, 0 );
    CD_Play( CDdisk, tracks[ TNO + 1 ], tracks[ highTrack+1 ] ,
             reqHdr );
    Status := Play;
    {fillChar( UpdCnt, SizeOf( UpdCnt ), 0 );}
    LastShuffle := order[ TNO + 1 ];
    Update;
  end;
end;

PROCEDURE TcdPlayer._Prev;
begin
  if not DiskOk then Exit;
  with QChannel do begin
    if TNO <= LowTrack then begin _Stop; Exit end;
    TNO := CurTrack; Dec(CurTrack); { Flash 07-07-2003 }
    RMin := 0;
    RSec := 0;
    Display( TNO-1, 0, 0 );
    {
    if Status = Stopped then
      begin
        Dec( TNO );
        AMin := tracks[ tno ].CMin;
        ASec := tracks[ tno ].CSec;
        AFrm := 0;
        Exit;
      end;
    }
    CD_Play( CDdisk, tracks[ TNO - 1 ], tracks[ highTrack+1 ], reqHdr );
    Status := Play;
    {fillChar( UpdCnt, SizeOf( UpdCnt ), 0 );}
    LastShuffle := order[ TNO - 1 ];
    Update;
  end;
end;

PROCEDURE TcdPlayer._Pause;
var Res: _trackInfo;
begin
  { Flash 07-07-2003 >>> }
  if (opSys and opWNT) = opWNT then
    begin MessageBox(GetString(dlCDNTWarning),nil,mfOKButton); Exit; end;
  { Flash 07-07-2003 <<< }
  if Status = Play then begin
    if _DiskStatus
      then Exit; _Stop; Status := Paused; Exit end
      else if Status = Paused then begin
        with QChannel do begin
          FillChar( Res, SizeOf( Res ), 0 );
          Res.CMin := AMin;
          Res.CSec := ASec;
          Res.CFrame := AFrm;
          Display( TNO, Rmin, Rsec );
          CD_Play( CDdisk, Res, tracks[ highTrack+1 ], reqHdr );
          Status := Play;
        end;
      end;
end;

FUNCTION TcdPlayer._DiskStatus;
     begin
      _DiskStatus := True;
      fillchar( reqHdr, SizeOf( reqHdr ), 0 );
      reqHdr[ 20 ]:=12; { Get Audio Q-Channel }
      reqHdr[ 21 ]:=1;
      if CD_Req( cdDisk, 3, 11, reqHdr ) then
       begin
        if ( reqHDr[ 3 ] = 2 ) or ( reqHDr[ 3 ] = 12 ) then
           Status := NoDisk
          else _DiskStatus := false;
        Exit;
       end;
      move( reqHdr[ 21 ], Qchannel, 10 );
      { Flash 07-07-2003 >>> }
      if (opSys and opWNT) = opWNT then QChannel.TNO := CurTrack else
      with Qchannel do
       begin
       TNO :=(( TNO and $f )+(( TNO and $f0 )) * 10 div 16 );
       CurTrack := TNO;
       end;
      { Flash 07-07-2003 <<< }
      _DiskStatus := false;
  end;

FUNCTION TcdPlayer._diskIsChanged;
     begin
      fillchar( reqHdr, SizeOf( reqHdr ), 0 );
      reqHdr[ 20 ] := 9; { Changed? cmd }
      CD_Req( CdDisk, 3, 1, reqHdr );
      _DiskIsChanged := ( reqHdr[ 21 ] = $FF );
     end;

PROCEDURE TcdPlayer._Door;
     begin
      if Status = play then _Stop;
      fillchar( reqHdr, SizeOf( reqHdr ), 0 );
      reqHdr[ 20 ] := 1; { Lock cmd }
      reqHdr[ 21 ] := Ord( DLock );
      CD_Req( CdDisk , 12, 2, reqHdr );
      {Status := noDisk;}
     end;

PROCEDURE TcdPlayer._Eject;
  var E: Tevent;
     begin
      E.What := evCommand;
      E.Command := cmAbout;
      if Status = play then _Stop;
      fillchar( reqHdr, SizeOf( reqHdr ), 0 );
      reqHdr[ 20 ] := 0; { Eject cmd }
      CD_Req( CdDisk , 12, 1, reqHdr );
      Status := noDisk;
      _Door( False );    { Unlock Door }
      Cd_loaded := False ;
     end;

PROCEDURE TcdPlayer._Load;
  var E: Tevent;
     begin
      if Status <> noDisk then Exit;
       if Cd_Loaded then begin _Eject; Exit; end;
       {Cd_Loaded := not Cd_Loaded;}
      fillchar( reqHdr, SizeOf( reqHdr ), 0 );
      reqHdr[ 20 ] := 5; { Load cmd }
      CD_Req( CdDisk , 12, 1, reqHdr );
      Cd_Loaded := True;
     end;

PROCEDURE TcdPlayer._FForward;
   var DM, DS: integer;
       TR: _TrackInfo;
     begin
      if Status <> play then Exit;
      if _DiskStatus then Exit;
      { Flash 07-07-2003 >>> }
      if (opSys and opWNT) = opWNT then
       begin MessageBox(GetString(dlCDNTWarning),nil,mfOKButton); Exit; end;
      { Flash 07-07-2003 <<< }
      with QChannel do
       begin
         fillchar( TR, SizeOf( Tr ), 0 );
         DM := AMin*60 + Asec;
         DS := tracks[ TNO+1 ].CMin*60 + tracks[ TNO+1 ].CSec;

         TR.CMin := AMin;
         TR.CSec := ASec;
         TR.Cframe := 0;

         DS := DS-DM;
         if DS < 2 then
          if TNO < HighTrack then
              DS := 15;
         if DS > 15 then DS:=15;

         with TR do
          begin
           inc( CSec, DS );
           Inc( CMin, CSec div 60 );
           CSec := CSec mod 60;
          end;
        {Display( TNO, Rmin, Rsec );}
        CD_Play( CDdisk, Tr, tracks[ highTrack+1 ] ,
                 reqHdr );
        Update;
       end;
     end;

PROCEDURE TcdPlayer._FRewind;
var
  DM, DS: integer;
  TR: _TrackInfo;
begin
  if Status <> play then Exit;
  if _DiskStatus then Exit;
  { Flash 07-07-2003 >>> }
  if (opSys and opWNT) = opWNT then
   begin MessageBox(GetString(dlCDNTWarning),nil,mfOKButton); Exit; end;
  { Flash 07-07-2003 <<< }
  with QChannel do begin
    fillchar( TR, SizeOf( Tr ), 0 );
    DM := AMin*60 + Asec;
    DS := tracks[ TNO ].CMin*60 + tracks[ TNO ].CSec;

    TR.CMin := AMin;
    TR.CSec := ASec;
    TR.Cframe := 0;

    DS := DM-DS;
    if DS < 2 then
      if TNO > LowTrack
        then DS := 15;
    if DS > 15 then DS:=15;
    with TR do begin
      dec( CSec, DS );
      dec( CMin, byte( CSec > 59 ));
      if CSec > 59 then
      Inc( CSec, 60 );
    end;
    {Display( TNO, Rmin, Rsec );}
    CD_Play( CDdisk, Tr, tracks[ highTrack+1 ], reqHdr );
    {fillChar( UpdCnt, SizeOf( UpdCnt ), 0 );}
    Update;
  end;
end;

PROCEDURE TcdPlayer._Setup;
var
  Prm : record
   P1,P2 : word;
   P3    : string;
   end;
begin

 with PRM do begin
  P1 := CDparams[ 1 ];
  P2 := CDparams[ 2 ];
  P3 := CDmixer ;

  if ExecResource( dlgCdOptions, PRM ) = cmOk then begin { Flash }
   CDmixer := P3;
   CDparams[ 1 ] := P1;
    if P2 <> CDparams[ 2 ]  then begin
      CDparams[ 2 ] := P2;
      _Stop;
      _SetOrder;
      if CDparams[ 2 ] <> cpShuffle then CurTrack := 1; { Flash 07-07-2003 }
    end;
    CDparams[ 2 ] := P2;
    _Door( CDparams[ 1 ] and cpLockDoor > 0 );
    ConfigModified := True;
  end;
 end;
end;

  Procedure TcdPlayer._Song ;
   var trk,trks : word ;
       P : PDialog;
      begin

       if status=noDisk then  Exit ;

       trks := Succ(HighTrack - LowTrack) ;
        trk := trks ;
       {trk := 42 ;}
       if ASong=0 then
         begin
           { Flash >>> }
           P := PDialog(New( PSPad , Init( trk ) ));
           P^.SetData(trk);
           if Desktop^.ExecView(P) <> cmOk then Exit;
           P^.GetData(trk);
           { Flash <<< }
         End
         Else trk := ASong ;
       if trk > trks then Exit ;



        if not DiskOk then Exit ;
        with QChannel do
        begin

         TNO := LowTrack - 1 + trk ;
         CurTrack := TNO; { Flash 07-07-2003 }
         if TNO < LowTrack then
          begin
           _Stop;
           Exit;
          end;
          RMin := 0;
          RSec := 0;
         Display(TNO ,0,0);
         cd_Play( CDdisk , tracks[ TNO ] , tracks[highTrack+1] ,
                  reqHdr ) ;
         status := Play ;
         {fillChar(UpdCnt,SizeOf(UpdCnt),0);}
         LastShuffle := order[TNO] ;
         Update ;
        end ;
      end;

PROCEDURE TcdPlayer._Mix;
 begin
 end;

PROCEDURE TcdPlayer._SetOrder;
var
  I: byte;
  S, D: byte;
begin
  if status=noDisk then Exit ;
  LastShuffle := LowTrack;
  for I := LowTrack to HighTrack do order[ i ] := I;

  GetOrder ;

  if CDparams[ 2 ] <> cpShuffle  then Exit;
  Randomize;
  For I:=LowTrack to HighTrack do begin
    S := LowTrack + Random( Succ( HighTrack-LowTrack ));
    D := order[ i ];
    order[ i ] := order[ S ];
    order[ s ] := D;
  end;
end;

{ Flash 07-07-2003 >>> }
PROCEDURE TcdPlayer.Zoom;
var CurButton: PButton;
begin
  if Size.Y > 9 then begin
    GrowTo( 36, 9 );
    CurButton:=PButton(Current);
    ButtonPause^.Hide;
    ButtonStop^.Hide;
    ButtonEject^.Hide;
    CurButton^.Select;
    while Current^.Origin.Y > 7 do SelectNext(False);
  end else begin
    CurButton:=PButton(Current);
    ButtonPause^.Show;
    ButtonStop^.Show;
    ButtonEject^.Show;
    CurButton^.Select;
    GrowTo( 36, 15 );
  end;
end;
{ Flash 07-07-2003 <<< }

FUNCTION TcdPlayer.Valid( Command: Word ): Boolean;
begin
  Valid := inherited Valid( Command ) ;
  if ((Command = cmClose) and (( Confirms and cfExitCDconfirm ) > 0 ))
   then
    Valid := MessageBox( GetString( dlCloseCD ), nil, mfConfirmation+ mfYesButton or mfNoButton ) = cmYes ;
end;

PROCEDURE TcdPlayer.HandleEvent;
  var MS : Tpoint ;
begin
  if ( Event.What = evBroadCast ) or
     ( Event.What = evCommand ) then begin
    case Event.Command of
      cdPlay: _Play;
      cdStop: _Stop;
      cdPrev: _Prev;
      cdNext: _Next;
      cdPause: _Pause;
      cdEject: if Status=noDisk then _Load else _Eject;
      cdZoom: Zoom;
      cdFFwd: _FForward;
      cdFREW: _FRewind;
      cdSetup: _Setup;
      cdDisplay: begin
          { Flash 07-07-2003 >>> }
          if (opSys and opWNT) = opWNT then
           begin MessageBox(GetString(dlCDNTWarning),nil,mfOKButton); Exit; end;
          { Flash 07-07-2003 <<< }
          Inc( CDparams[ 3 ] );
          if CDparams[ 3 ] > 3 then CDparams[ 3 ] := 0;
          {fillChar( UpdCnt, SizeOf( UpdCnt ), 0 );}
          Update;
        end;
      cdZoom: Zoom;
      cdTitle: begin
                      GetDescription;
                      EditDescr(@Self);
               end;
      cdOrder: SelectOrder;
      cdMix: if CDmixer <>'' then
               Message(Application, evCommand, cmExecString, @CDmixer) else
               MessageBox(GetString(dlCDNoMixerSet),nil,mfOKButton);
      cdMode: begin
          Inc( CDparams[ 2 ] );
          if CDparams[ 2 ] > 3 then CDparams[ 2 ] := 0;
          _Stop;
          _SetOrder;
          {fillChar( UpdCnt, SizeOf( UpdCnt ), 0 );}
          Update;
        end;
      cmGetName: begin
          if Status = NoDisk then FreeStr := GetString(dlCDPlayerNoCD)
                             else FreeStr := CnvString(DiskTitle);
          if FreeStr<>'' then FreeStr:= ' - '+FreeStr;
          PString(Event.InfoPtr)^ := GetString( dlCDPlayer ) + FreeStr;
          ClearEvent(Event);
          Exit;
        end;
      else{ case } begin
          Inherited HandleEvent( Event );
          Exit;
        end;
    end{ case };
    ClearEvent( Event );
    Exit;
  end;


 if Event.What = evMouseDown then
            begin
             MakeLocal(Event.Where,MS);
             if (MS.X<33) and (MS.Y < 5) and (MS.Y > 1) then _Song(0)
              else if (MS.Y=5) and (MS.X<30) then SelectTrack
               else if (MS.X>32) and (MS.X<36) and (MS.Y>0) then _Setup
                else if (MS.Y=1) and (MS.X>18) then  begin
                                                      Inc( CDparams[ 3 ] );
                                                      if CDparams[ 3 ] > 3 then CDparams[ 3 ] := 0;
                                                      Update;
                                                     end;
            end;

  if Event.What = evKeyDown then begin
    case Upcase( Event.CharCode ) of
      #27: begin
          Event.What := evCommand; Event.Command := cmClose; Event.InfoPtr := nil;
          inherited HandleEvent( Event );
          Exit;
        end;
      'P': _Play;
      'S': _Stop;
      'U': _Pause;
      'V', '<', ',': _Prev;
      'N', '>', '.': _Next;
      'F', '}', ']': _FForward;
                      { begin
                        Event.Command := cdFFwd;
                        ProcessCmd;
                       end; }
      'B', '{', '[': _FRewind;
                    { begin
                      Event.Command := cdFRew;
                      ProcessCmd;
                     end; }
      'J': if Status=noDisk then _Load else _Eject;
      'Z': Zoom;
      '0'..'9'  :  _Song( byte(Event.CharCode) - byte ('0')) ;
      'O': SelectOrder ;
      {'T': begin  GetDescription;
                  EditDescr(@Self);
           end;}                         {disabled by JITRsoftware}
      'R': SelectTrack; {added by JITRsoftware}
      else
       Case Event.KeyCode of
        KbUp:    _Next;
        KbDown:  _Prev;
        KBPgUp:  _Next;
        KbPgDn:  _Prev;
        KbLeft:  _FRewind;
        KbRight: _FForward;
               { Flash 11-02-2003 >>> }
        KbTab: begin
                if (Size.Y = 9) and (PButton(Current)^.Command = cdFFwd) then
                 repeat SelectNext(False) until Current^.Origin.Y < 7
                else SelectNext(False);
               end;
        KbShiftTab: begin
                     if (Size.Y = 9) and (PButton(Current)^.Command = cdFRew) then
                      repeat SelectNext(True) until Current^.Origin.Y < 7
                     else SelectNext(True);
                    end;
               { Flash 11-02-2003 <<< }
       else
      begin
        inherited HandleEvent( Event );
        Exit;
      end;
     end; { case }
    end{ case };
    ClearEvent( Event );
    Exit;
  end;
  inherited HandleEvent( Event );
end;

PROCEDURE RunPlayer;
var
  R, R1: TRect;
  CD: integer;
  DRV: String[ 20 ];
  PP: Pointer;
  C: Char;
  Items: PMenuItem;
  Menu: PMenu;
  P: PMenuBox;
  N: Word;
begin
  if CD_Player <> nil then begin
    CD_Player^.Select;
    Exit;
  end;

  CD := -1;
  case CD_GetFirstDrive( CD ) of
   0: begin
       DRV := 'MSCDEX.EXE';
       if ( Lo( DosVersion )>=20 ) and ( Hi( DosVersion )>=10 )
        then DRV := 'VCDROM.SYS';
       PP := @DRV;
       Msg(dlNoCD, @PP, mfError or mfOkButton );
       Exit;
      end;
   1:
   else begin
         Items:=nil;
         cd:=0;
         For C:='Z' downto 'A' do
          if IsDriveCDROM(C) then begin
           FreeStr:='~'+C+'~:';
           Items := NewItem(FreeStr, '', kbNoKey, cmSelectMenuCommands + Ord(C) - Ord('A'),
            hcCDPlayerSelectCDROM, Items); { Flash }
           inc(cd);
          end;
         { Flash >>> }
         R.Assign(Desktop^.Size.X div 2 - 1,
                  Desktop^.Size.Y div 2 - CD div 2,
                  Desktop^.Size.X div 2 + 1,
                  Desktop^.Size.Y div 2 + CD div 2 + CD mod 2);
         { Flash <<< }
         Menu := NewMenu(Items);
         P := New(PMenuBox, Init(R, Menu, nil));
         P^.Options := P^.Options or ofCentered;
         P^.HelpCtx := hcCDPlayerSelectCDROM; { Flash }

         N := Desktop^.ExecView(P); { Flash }
         Dispose(P,Done);
         DisposeMenu(Menu);
         if N >= cmSelectMenuCommands
          then CD:=N - cmSelectMenuCommands
          else exit;
        end;
  end;

  Desktop^.GetExtent( R1 );
  if ( CDplace.X > R1.B.X ) or
     ( CDplace.Y > R1.B.Y ) then
       CDplace.X := -100;

  if CDsize.X < 0 then CdSize.X := 60;
  if CDsize.Y < 0 then CdSize.Y := 11;

  R.Assign( 0, 0, 10, 11 );

  if CdPlace.X > -100 then R.A := CDplace;

  R.B.X := R.A.X + CDsize.X;
  R.B.Y := R.A.Y + CDsize.Y;

  CD_Player := New( PCdPlayer, Init( R, CD ));
  if CDplace.X = -100 then with CD_Player^ do Options := Options or ofCentered;

  Desktop^.Insert( CD_Player );
end;

{$IFDEF UPD}
PROCEDURE UpdateCdPlayer;
begin
  if CD_Player = nil then Exit;
  PCdPlayer( CD_Player )^.Update;
end;
{$ENDIF}

END.
