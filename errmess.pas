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
  MessageBox(GetString(dlFBBNoOpen) + Path + ^M^C'(RC = %d)',
      @RC, mfError + mfOKButton);
  end;

procedure MessFileNotRename(const s1, s2: string; ErrCode: longint);
  begin
  MessageBox(GetString(dlFCNoRename1)+GetString(dlDIFile)
      + ' (RC = %d)'^M^C  + Cut(S1,20)
      +GetString(dlFCNoRename2)+Cut(S2,20),
      @ErrCode, mfError+mfOKButton)
  end;

end.

