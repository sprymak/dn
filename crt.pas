//ÛßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßÛ
//Û                                                       Û
//Û      Virtual Pascal Runtime Library.  Version 2.1.    Û
//Û      CRT Interface unit for OS/2 & Win32              Û
//Û      ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÛ
//Û      Copyright (C) 1995-2000 vpascal.com              Û
//Û                                                       Û
//ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
{AK155 Žâáî¤  ¢ë¡à®è¥­® ¢á¥ ­¥¨á¯®«ì§ã¥¬®¥. € ¨á¯®«ì§ã¥âáï
¢ë¢®¤ (Writeln) ¨ § ¯®¬¨­ ­¨¥ à §¬¥à®¢ ®ª­  ¯à¨
¨­¨æ¨ «¨§ æ¨¨ ¨ ¯à¨ á¬¥­¥ ¢¨¤¥®à¥¦¨¬ .
   â® ¢à¥¬¥­­®¥ á®áâ®ï­¨¥, ­ ¤® ¡ã¤¥â ¤®à §®¡à âìáï,  ªªãà â­®
á¤¥« âì à ¡®âã á íªà ­®¬ ã á¥¡ï,   CRT ã¡à âì ¯®«­®áâìî.
   20-10-2001 (beta3)
}
{$S-,R-,Q-,I-,X+,T-,Cdecl-,OrgName-,AlignRec-,Use32+}

unit Crt;

interface

procedure GetLastMode;
procedure SetWindowPos;

implementation
uses VpSysLow, Dos;
const
  LightGray     = 7;
  TextAttr: Byte = LightGray;   { Current text attribute }

{ Interface procedures }

{procedure AssignCrt(var F: Text);
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure ClrEol;
}

{ Private variables }

var
  WindMin: Word;                { Window upper left coordinates }
  WindMax: Word;                { Window lower right coordinates }
  NormAttr: Byte;
  ScreenSize: tSysPoint;

{ Selects low intensity characters.                                     }

{ Setups window coordinates }

procedure SetWindowPos;
begin
  WindMin := 0;
  WindMax := ScreenSize.x - 1 + (ScreenSize.y - 1) shl 8;
end;

{ Stores current video mode in LastMode }

procedure GetLastMode;
begin
  SysTvGetScrMode( @ScreenSize );
end;

procedure LowVideo;
begin
  TextAttr := TextAttr and $F7;
end;

{ Selects normal intensity characters.                                  }

procedure NormVideo;
begin
  TextAttr := NormAttr;
end;

{ Selects high-intensity characters.                                    }

procedure HighVideo;
begin
  TextAttr := TextAttr or $08;
end;

{ Delays a specified number of milliseconds. }

procedure Delay(MS: Longint);
begin
  SysCtrlSleep( MS );
end;

{ Plays sound of a specified frequency and duration.                    }

procedure PlaySound(Freq,Duration: Longint);
begin
  SysBeepEx(Freq, Duration);
end;

{$IFDEF DPMI32}
procedure Sound(Hz: Word);
begin
  SysSound(Hz);
end;

procedure NoSound;
begin
  SysNoSound;
end;
{$ENDIF}

{ Do line feed operation }

procedure LineFeed;
var
  Cell: SmallWord;
begin
  Cell := Ord(' ') + TextAttr shl 8;
  SysScrollUp(Lo(WindMin),Hi(WindMin),Lo(WindMax),Hi(WindMax),1,Cell);
end;

{ Outputs packed string to the CRT device }

type
  PWin32Cell = ^TWin32Cell;
  TWin32Cell = record
    Ch:     SmallWord;
    Attr:   SmallWord;
  end;

procedure WritePackedString(S: PChar; Len: Longint);
var
  Buf: array[1..256] of Char;
  I,BufChars: Integer;
  X,Y:        SmallWord;
  X1,Y1:      Longint;
  C:          Char;

  procedure FlushBuffered;
  begin
    If BufChars > 0 then
    begin
      SysWrtCharStrAtt(@Buf, BufChars, X1, Y1, TextAttr);
      BufChars := 0;
      X1 := X;
      Y1 := Y;
    end;
  end;

begin
  SysGetCurPos(X, Y);
  BufChars := 0;
  X1 := X;
  Y1 := Y;
  for I := 0 to Len - 1 do
  begin
    C := S[I];
    case C of
      ^J:               { Line Feed }
        begin
          FlushBuffered;
          {$IFDEF LINUX}
          X := Lo(WindMin);
          x1 := x;
          {$ENDIF}
          if Y >= Hi(WindMax) then LineFeed else Inc(Y);
          y1 := y;
        end;
      {$IFNDEF LINUX}
      ^M:               { Carriage return }
        begin
          FlushBuffered;
          X := Lo(WindMin);
          x1 := x;
        end;
      {$ENDIF}
      ^H:               { Backspace }
        begin
          FlushBuffered;
          if X > Lo(WindMin) then Dec(X);
          if X1 > Lo(WindMin) then Dec(X1);
        end;
(*
      ^G:               { Bell }
        SysBeep;
*)
      else
        Inc(BufChars);
        Buf[BufChars] := C;
        Inc(X);
        if X > Lo(WindMax) then
        begin
          FlushBuffered;
          X := Lo(WindMin);
          X1 := X;
          Inc(Y);
          if Y > Hi(WindMax) then
          begin
            FlushBuffered;
            LineFeed;
            Y := Hi(WindMax);
          end;
          Y1 := Y;
        end;
    end;
  end;
  FlushBuffered;
  SysTVSetCurPos(X, Y);
end;

{ CRT text file I/O functions }

function CrtWrite(var F: Text): Longint;
begin
  with TextRec(F) do
  begin
    WritePackedString(PChar(BufPtr),BufPos);
    BufPos := 0;
  end;
  CrtWrite := 0;                { I/O result = 0: success }
end;

function CrtReturn(var F: Text): Longint;
begin
  CrtReturn := 0;               { I/O result = 0: success }
end;

function CrtOpen(var F: Text): Longint;
begin
  with TextRec(F) do
  begin
    CloseFunc := @CrtReturn;
      Mode := fmOutput;
      InOutFunc := @CrtWrite;
      FlushFunc := @CrtWrite;
  end;
  CrtOpen := 0;                 { I/O result = 0: success }
end;

{ Associates a text file with CRT device.                               }

procedure AssignCrt(var F: Text);
begin
  with TextRec(F) do
  begin
    Handle := $FFFFFFFF;
    Mode := fmClosed;
    BufSize := SizeOf(Buffer);
    BufPtr := @Buffer;
    OpenFunc := @CrtOpen;
    Name[0] := #0;
  end;
end;

begin
  GetLastMode;
  SetWindowPos;
  AssignCrt(Output); ReWrite(Output);
end.

