unit _RegExp;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines, _Objects;

type
  PRegExp = ^TRegExp;
  TRegExp = object(TObject)
    FStatus: TRegExpStatus;
    FStart: integer;
    FLength: integer;
    Constructor Init;
    {destructor Done; virtual;}
    procedure Reset;
    function CompileString(const AExpression: String): boolean;
    function CompileStr(AExpression: PChar): boolean;
    function Compile(AExpression: PChar; ALength: integer): boolean;
    function Execute(AString: PChar; ALength: integer): boolean;
    function SubstituteString(ASrc: PChar; const AReplace: String;
      var ADest: String): boolean;
    function SubstituteStr(ASrc, AReplace: PChar; ADest: PChar; var
      ALength: integer): boolean;
    function Substitute(ASrc, AReplace: PChar; ARLen: integer; ADest:
      PChar; var ADLen: integer): boolean;
    procedure Error(AStatus: TRegExpStatus); virtual;
    function CheckBreak: boolean; virtual;
    procedure Escape(AChar: Char; var ASubExp: PChar; var ALen:
      integer); virtual;
    private
    FFlags: Set of
    (
    ffCompiled,
    ffAnchored,
    ffStart,
    ffParsing,
    ffMatchNext,
    ffBreak,
    ffAutoTag
    );
    FCodeSize: word;
    FCodeData: PChar;
    FStartP: array[1..9] of integer;
    FEndP: array[1..9] of integer;
    FStartCh: Char;
    FMust: PString;
    FInput: PChar;
    FInputBol: PChar;
    FInputEol: PChar;
    FLStack: array[1..10] of integer;
    FLStackId: integer;
    end;

implementation

uses
  _DNFuncs;

Constructor TRegExp.Init;
  begin
    _TRegExp^.Init(nil, @Self);
  end;

procedure TRegExp.Reset;
  begin
    _TRegExp^.Reset(@Self);
  end;

function TRegExp.CompileString(const AExpression: String): boolean;
  begin
    Result := _TRegExp^.CompileString(AExpression, @Self);
  end;

function TRegExp.CompileStr(AExpression: PChar): boolean;
  begin
    Result := _TRegExp^.CompileStr(AExpression, @Self);
  end;

function TRegExp.Compile(AExpression: PChar; ALength: integer):
    boolean;
  begin
    Result := _TRegExp^.Compile(AExpression, ALength, @Self);
  end;

function TRegExp.Execute(AString: PChar; ALength: integer): boolean;
  begin
    Result := _TRegExp^.Execute(AString, ALength, @Self);
  end;

function TRegExp.SubstituteString(ASrc: PChar; const AReplace:
    String; var ADest: String): boolean;
  begin
    Result := _TRegExp^.SubstituteString(ASrc, AReplace, ADest,
      @Self);
  end;

function TRegExp.SubstituteStr(ASrc, AReplace: PChar; ADest: PChar;
    var ALength: integer): boolean;
  begin
    Result := _TRegExp^.SubstituteStr(ASrc, AReplace, ADest, ALength,
      @Self);
  end;

function TRegExp.Substitute(ASrc, AReplace: PChar; ARLen: integer;
    ADest: PChar; var ADLen: integer): boolean;
  begin
    Result := _TRegExp^.Substitute(ASrc, AReplace, ARLen, ADest,
      ADLen, @Self);
  end;

procedure TRegExp.Error(AStatus: TRegExpStatus);
  assembler; {&Frame-}
asm
end;

function TRegExp.CheckBreak: boolean;
  assembler; {&Frame-}
asm
end;

procedure TRegExp.Escape(AChar: Char; var ASubExp: PChar; var ALen:
    integer);
  assembler; {&Frame-}
asm
end;

end.
