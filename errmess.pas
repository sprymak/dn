{$I STDEFINE.INC} {Cat}

unit ErrMess;

interface
{AK155}
procedure MessFileNotOpen(const Path: string; RC: longint);
procedure MessFileNotRename(const s1, s2: string; ErrCode: longint); {AK155}

implementation

uses
  DnApp, Messages, Commands, advance;

procedure MessFileNotOpen(const Path: string; RC: longint);
begin
  MessageBox(GetString(dlFBBNoOpen) + Path {$IFDEF SHOWRC} + ^M^C'(RC=%d)' {$ENDIF},
      @RC, mfError + mfOKButton);
end;

procedure MessFileNotRename(const s1, s2: string; ErrCode: longint);
begin
  MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)
      {$IFDEF SHOWRC} + ^M^C'(RC=%d)' {$ENDIF} + ^M^C  + Cut(S1,20)
      +GetString(dlFCNoRename2)+Cut(S2,20),
      @ErrCode, mfError+mfOKButton)
end;

end.

