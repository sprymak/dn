(*****************************************************************
 * Copyright (C), 2001 Pawel Ziemian, All rights reserved        *
 *****************************************************************)

(*****************************************************************
 *
 * SOURCE FILE: REGEXP1.PAS
 *
 * MODULE NAME: Regular Expression for Dos Navigator
 *
 * PURPOSE:
 *
 * AUTHOR:      Pawel Ziemian (PZ)
 *
 * REVIEWED BY:
 *
 * HISTORY:     Ver   Date       Sign   Description
 *
 *              001   2001.09.16 PZ     Created
 *
 *****************************************************************)

{$I STDEFINE.INC}

unit RegExp1;

interface

uses
  RegExp, xTime;

type

  PRegExpEx = ^TRegExpEx;
  TRegExpEx = object (TRegExp)
    Timer : TEventTimer;
    constructor Init;
    procedure   Error ( AStatus : TRegExpStatus ); virtual;
    function    CheckBreak : Boolean; virtual;
    procedure   Escape ( AChar : Char; var ASubExp : PChar; var ALen : Integer ); virtual;
  end;

implementation

uses
  Objects, DnApp, Drivers, Commands, Messages, DnIni;

constructor TRegExpEx.Init;
begin
  inherited Init;
  NewTimer ( Timer, 1 );
end;

procedure   TRegExpEx.Error ( AStatus : TRegExpStatus );
var
  s : string;
  t : string;
  i : TStrIdx;
  p : record
        s : PString;
        d : Longint;
      end;
begin
  case AStatus of
    resCanceled:                    i := dlResCanceled;
    resRegExpTooBig,
    resStackOverflow:               i := dlResTooComplex;
    resOutOfSpace:                  i := dlResOutOfSpace;
    resUnmatchedParenthesis:        i := dlResExpectedParenthesis;
    resStarPlusOperandCouldBeEmpty: i := dlResOperandCouldBeEmpty;
    resNestedStarQuotePlus:         i := dlResNested;
    resInvalidEscape,
    resInvalidPredefinedExp,
    resUndefinedPredefinedExp:      i := dlResInvalidEscape;
    resInvalidSetRange:             i := dlResInvalidSetRange;
    resUnmatchedSquareBracket:      i := dlResExpectedSquareBracket;
    resOperatorFollowsNothing:      i := dlResFollowsNothing;
    resTrailingBackSlash:           i := dlResTrailingBackSlash;
  else                              i := dlResUnknown;
  end;
  s   := '%s'^M+GetString ( i );
  t   := GetString ( dlResErrorTitle );
  p.s := @t;
  p.d := Ord(AStatus);
  MessageBox ( s, @p, mfOkButton+mfError );
end;

function    TRegExpEx.CheckBreak : Boolean;
var
  E : TEvent;
begin
  CheckBreak := False;
  if TimerExpired ( Timer ) then begin
    NewTimer ( Timer, 3 );
    Application^.Idle;
    GetKeyEvent ( E );
    CheckBreak := (E.What = evKeyDown) and (E.KeyCode = kbEsc);
  end;
end;

procedure   TRegExpEx.Escape ( AChar : Char; var ASubExp : PChar; var ALen : Integer );
var
  p : PString;
begin
  { At first try to get already defined subexpression }
  inherited Escape ( AChar, ASubExp, ALen );
  { If not defined use own subexpressions }
  if (ASubExp = nil) or (ALen <= 0) then begin
    p := nil;
    case AChar of
      '0': p := @RegExpStr0;
      '1': p := @RegExpStr1;
      '2': p := @RegExpStr2;
      '3': p := @RegExpStr3;
      '4': p := @RegExpStr4;
      '5': p := @RegExpStr5;
      '6': p := @RegExpStr6;
      '7': p := @RegExpStr7;
      '8': p := @RegExpStr8;
      '9': p := @RegExpStr9;
    end;
    if p <> nil then begin
      ASubExp := @((p^)[1]);
      ALen    := Length(p^);
    end;
  end;
end;

end.
