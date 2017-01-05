unit Excepts;
(******

Exception Handling unit
(for use exceptions without SysUtils unit)
Written by Cat 2:5030/1326.13

******)


{$H+,J+,P+,S-,T-,W-,R-,Z-,X+}
{&Delphi+,AlignRec-,AlignData+,CDecl-,Use32-,Open32-}

interface

type
  Exception = class
  end;

  EOutOfMemory = class(Exception);
  EAnyException = class(Exception);

  ExceptClass = class of Exception;

implementation

uses
  VPSysLow;

var
  OutOfMemory: EOutOfMemory;
  AnyException: EAnyException;

procedure ErrorHandler(ErrorCode: Integer; ErrorAddr: Pointer);
var
  E: Exception;
begin
  if ErrorCode = 1 then
    E := OutOfMemory
  else
    E := AnyException;
  raise E at ErrorAddr;
end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer);
begin
  Halt($EE);
end;

function GetExceptionClass(P: POSExceptionRecord): ExceptClass;
begin
  Result := EAnyException;
end;

function GetExceptionObject(P: POSExceptionRecord): Exception;
begin
  Result := nil;
end;

procedure InitExceptions;
begin
  EOutOfMemory.Create;
  EAnyException.Create;
  ErrorProc := @ErrorHandler;
  ExceptProc := @ExceptHandler;
  ExceptionClass := Exception;
  ExceptClsProc := @GetExceptionClass;
  ExceptObjProc := @GetExceptionObject;
end;

begin
  InitExceptions;
end.
