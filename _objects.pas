unit _Objects;
(******

DN/2 Plugin Interface - object model
Copyright (C) 2002 Aleksej Kozlov (Cat)
2:5030/1326.13

******)

{&Delphi+}
{&Use32+}

interface

uses
  _Defines;

type
  PEmptyObject = ^TEmptyObject;
  TEmptyObject = object
    Constructor Init;
    procedure Free;
    destructor Done; virtual;
    end;

  PObject = ^TObject;
  TObject = object(TEmptyObject)
    ObjectIsInited: boolean;
    Constructor Init;
    {destructor Done; virtual;}
    end;

implementation

uses
  _DNFuncs;

Constructor TEmptyObject.Init;
  begin
    _TEmptyObject^.Init(nil, @Self);
  end;

procedure TEmptyObject.Free;
  begin
    _TEmptyObject^.Free(@Self);
  end;

destructor TEmptyObject.Done;
  assembler; {&Frame-}
asm
end;

Constructor TObject.Init;
  begin
    _TObject^.Init(nil, @Self);
  end;

end.
