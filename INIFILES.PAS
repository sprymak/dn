{/////////////////////////////////////////////////////////////////////////
//
//  Dos Navigator Open Source 1.51.10
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

UNIT
  IniFiles;

INTERFACE

USES
  Objects, Collect;

TYPE
  { Collection of strings like "video=vesa.bgi"  }
  { Note, "=" must be directly between key and   }
  {                   value, no blanks allowed   }
  { Empty key means insertion of ordinary string }
  { It may be used to implement indexed logfiles }
  { Of course, these lines must not contain
  { strings with "=" or starting with "[".       }
  PIniSection =^TIniSection;
  TIniSection = Object( TCollection )
    TheName: PString;
    constructor Init( const AName: String );
    destructor Done; virtual;
    function GetIndexOf( Key: String ): Integer;
    function Get( const Key: String ): String;
    procedure Put( const Key, Value: String; var Modified: Boolean );
    function GetKeyAt( const Index: Integer ): String;
    function GetValueAt( const Index: Integer ): String;
    function Name: String;
    procedure FreeItem( Item: Pointer ); virtual;
    procedure Insert( Item: Pointer ); virtual;
    constructor Load( var S: TStream );
    procedure Store( var S: TStream ); virtual;
  end;

  { Collection of TIniSections }
  PIniFile =^TIniFile;
  TIniFile = Object( TCollection )
    Modified: Boolean;
    Name: PString;
    constructor Init( FileName: String; var AStatus: Integer );
    destructor Done; virtual;
    function Get( const Section, Key: String ): String;
    procedure Put( const Section, Key, Value: String );
    function GetSection( Section: String ): PIniSection;
  end;

IMPLEMENTATION

USES
  advance, advance1, advance2, advance3;

CONST
  CrLf: Array [ 0..1 ] of Char = ( #13, #10 );

FUNCTION PStr2Str( P: PString ): String;
begin
  If P <> NIL
    then PStr2Str := P^
    else PStr2Str := ''
end;

FUNCTION ReadString( var Stream: TStream ): String;
var
  S: String;
  C: Char;
label
  Loop, Failure;
begin
  S := '';
Loop:
  Stream.Read( C, 1 );
  If ( C <> #13 ) and ( Stream.Status = stOK ) then begin
    S := S + C;
    goto Loop;
  end;
  ReadString := S;
  If Stream.Status <> stOK then Failure: begin
    Stream.Reset;
    Exit;
  end;
  Stream.Read( C, 1 );
  If ( C <> #10 ) or ( Stream.Status <> stOK ) then begin
    Stream.Reset;
    Stream.Seek( Stream.GetPos - 1 );
  end;
end;

{                                INI Section
{----------------------------------------------------------------------------}
CONSTRUCTOR TIniSection.Init;
begin
  inherited Init( 5, 5 );
  TheName := NewStr( AName );
end;

FUNCTION RemoveLeadSpaces( const S: String ): String;
var I: Integer;
begin
  I := 1;
  While ( I <= Length( S )) and ( S[ I ] = ' ' ) do Inc( I );
  RemoveLeadSpaces := Copy( S, I, 255 );
end;

FUNCTION TruncSpace(S:String): String;
var I: Byte;
begin
  I:=Length(S); While S[I]=' ' do DEC(I);
  S[0]:=Char(I);
  TruncSpace:=S
end;


CONSTRUCTOR TIniSection.Load;
var
  Base, L: LongInt;
  Str: String;
  function Simplify( S: String ): String;
  var
    I: Integer;
  begin
    I := Pos( '=', S );
    If I <> 0 then begin
      Simplify := RemoveLeadSpaces( TruncSpace( Copy( S, 1, I - 1 )))
        + '=' + RemoveLeadSpaces( TruncSpace( Copy( S, I + 1, 255 )));
    end else
      Simplify := RemoveLeadSpaces( TruncSpace( S ));
  end;
label
  Loop, Skip;
begin
  TCollection.Init(5, 5);
  Base := S.GetPos;
  Str := ReadString( S );
  If ( Str[ 1 ] = '[' ) and ( Str[ Length( Str ) ] = ']' ) then begin
    TheName := NewStr( Copy( Str, 2, Length( Str ) - 2 ));
  end else begin
    TheName := NIL;
    S.Seek( Base );
  end;

Loop:
  Str := Simplify( ReadString( S ));
  If Str <> '' then begin
    Insert( NewStr( Str ));
    goto Loop;
  end;

Skip:
  L := S.GetPos;
  Str := ReadString( S );
  If Str <> ''
    then S.Seek( L )
    else
  If S.GetPos <> L then goto Skip;

  If S.GetPos = Base then begin
    inherited Done;
    Fail
  end;
end;

PROCEDURE TIniSection.FreeItem;
begin
  DisposeStr( PString(Item) );
end;

PROCEDURE TIniSection.Insert;
begin
  If Item <> NIL then inherited Insert( Item );
end;

PROCEDURE TIniSection.Store;
var
  Str: String;
  procedure DoPutItem( P: PString ); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    Str := PStr2Str( P ) + #13#10;
    S.Write( Str[ 1 ], Length( Str ));
  end;
begin
  Str := PStr2Str( TheName );
  If Str <> '' then begin
    Str := '[' + Str + ']' + #13#10;
    S.Write( Str[ 1 ], Length( Str ));
  end;
  ForEach( @DoPutItem );
  S.Write( CrLf, SizeOf( CrLf ));
end;

DESTRUCTOR TIniSection.Done;
begin
  inherited Done;
  DisposeStr( TheName );
end;

FUNCTION TIniSection.Name;
begin
  Name := PStr2Str( TheName )
end;

FUNCTION TIniSection.GetIndexOf;
var
  P: PString;
  S: String;
  I: Integer;
  function Search( P: PString ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
  var
    S: String;
    I: Integer;
  begin
    S := UpStrg( PStr2Str( P ));
    I := Pos( Key, S );
    Search := ( I = 1 ) and ( S[ Length( Key ) + 1 ] = '=' );
  end;
begin
  UpStr( Key );
  GetIndexOf := IndexOf( FirstThat( @Search ));
end;

FUNCTION TIniSection.Get;
var
  I: Integer;
  S: String;
begin
  I := GetIndexOf( Key );
  If I >= 0 then begin
    S := PStr2Str( At( I ));
    I := Pos( '=', S );
    If I > 0 then begin
      Get := Copy( S, I + 1, 255 );
      Exit
    end;
  end;
  Get := ''
end;

PROCEDURE TIniSection.Put;
var
  I: Integer;
  S: String;
  NewValue: String;
begin
  If Key = '' then begin
    Insert( NewStr( Value ));
    Exit;
  end;
  I := GetIndexOf( Key );
  NewValue := Key + '=' + Value;
  If I >= 0 then begin
    S := PStr2Str( At( I ));
    If S <> NewValue then begin
      AtReplace( I, NewStr( NewValue ));
      Modified := True;
    end;
  end else begin
    Insert( NewStr( NewValue ));
    Modified := True;
  end;
end;

FUNCTION TIniSection.GetKeyAt;
var
  S: String;
  I: Integer;
begin
  S := PStr2Str( At( Index ));
  I := PosChar( '=', S );
  If I = 0
    then GetKeyAt := ''
    else GetKeyAt := TruncSpace( Copy( S, 1, I - 1 ));
end;

FUNCTION TIniSection.GetValueAt;
var
  S: String;
  I: Integer;
begin
  S := PStr2Str( At( Index ));
  I := PosChar( '=', S );
  If I = 0
    then GetValueAt := ''
    else GetValueAt := RemoveLeadSpaces( Copy( S, I + 1, 255 ));
end;

{                                  INI File
{----------------------------------------------------------------------------}
CONSTRUCTOR TIniFile.Init;
var
  T: TBufStream;
  P: PIniSection;
begin
  inherited Init( 5, 5 );
  Name := NewStr( FileName );
  T.Init( FileName, stOpenRead, 512 );
  If T.Status = stOK then
    repeat
      P := New( PIniSection, Load( T ));
      If P <> NIL
        then Insert( P );
    until P = NIL;
  AStatus := T.Status;
  T.Done;
end;

DESTRUCTOR TIniFile.Done;
var
  T: TBufStream;
  procedure DoPutItem( P: PIniSection ); {$IFDEF BIT_16}far;{$ENDIF}
  begin
    If P <> NIL then P^.Store( T );
  end;
begin
  If Modified then begin
    T.Init( PStr2Str( Name ), stCreate, 512 );
    ForEach( @DoPutItem );
    T.Done;
  end;
  DisposeStr( Name );
  inherited Done;
end;

FUNCTION TIniFile.GetSection;
  function Search( P: PIniSection ): boolean; {$IFDEF BIT_16}far;{$ENDIF}
  begin
    Search := UpStrg( P^.Name ) = Section
  end;
begin
  UpStr( Section );
  GetSection := FirstThat( @Search )
end;

FUNCTION TIniFile.Get;
var
  P: PIniSection;
begin
  P := GetSection( Section );
  If P <> NIL
    then Get := P^.Get( Key )
    else Get := ''
end;

PROCEDURE TIniFile.Put;
var
  P: PIniSection;
begin
  P := GetSection( Section );
  If P = NIL then begin
    New( P, Init( Key ));
    Insert( P );
    Modified := True;
  end;
  P^.Put( Key, Value, Modified );
end;

END.
