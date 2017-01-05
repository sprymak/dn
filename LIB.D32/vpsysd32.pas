//€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€
//€  Reminder                                             €
//€ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ€
//€  Call _make.cmd  in the source\d32 directory          €
//€  after any change                                     €
//€                                                       €
//ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ

(*$DEFINE THREAD_SUPPORT*)      // SysCtrlCreateThread
(*$DEFINE CLIP_SUPPORT*)        // SysClipCopy
(*$DEFINE PE2LE_P2*)            // relocation depacker code

//€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€
//€                                                       €
//€      Virtual Pascal Runtime Library.  Version 2.0.    €
//€      System interface layer DOS/DPMI32                €
//€      ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ€
//€      Copyright (C) 1995-98 fPrint UK Ltd              €
//€                                                       €
//ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ

const
  mausmaske=$ffff;

Function Max( a,b : Longint ) : Longint; inline;
  begin
    if a > b then
      Max := a
    else
      Max := b;
  end;

Function Min( a,b : Longint ) : Longint; inline;
  begin
    if a < b then
      Min := a
    else
      Min := b;
  end;

// not implemented:
// do not know
//  * realmode or protected mode selector ?
//  * flat to what selector (probably DSeg)
// use dpmi32.dosseg_linear,protsel_linear
procedure SysSysSelToFlat(var P: Pointer);
  begin
    //NOT_IMPLEMENTED
    asm int 3 end;
  end;

procedure SysSysFlatToSel(var P: Pointer);
  begin
    //NOT_IMPLEMENTED
    asm int 3 end;
  end;



// return linear address of video mem
function SysGetTextVideoMemBase:longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,SegB000
    cmp byte [Seg0040+$49],$07          // 7=mono
    je @ret

    mov eax,SegB800
  @ret:
  end;

function SysGetTextVideoModus:longint;(*$FRAME-*)(*$USES NONE*)
  asm
    movzx eax,byte [Seg0040+$49]        // Bildschirmmodus 0..255
  end;

function SysGetTextVideoColumns:longint;(*$FRAME-*)(*$USES NONE*)
  asm
    movzx eax,smallword [Seg0040+$4a]   // Anzahl Bildschirmspalten
  end;

function SysGetTextVideoRows:longint;(*$FRAME-*)(*$USES NONE*)
  asm
    movzx eax,byte [Seg0040+$84]        // Anzahl Zeilen -1 (EGA+)
    inc eax
  end;

function SysGetTextFontHeight:longint;(*$FRAME-*)(*$USES NONE*)
  asm
    movzx eax,smallword [Seg0040+$85]
  end;

function berechnebildschirmspeicherposition(x,y:smallword):longint;(*$FRAME-*)(*$USES EBX,EDX*)
  asm
    call SysGetTextVideoColumns         // eax:=Anzahl Bildschirmspalten
    movzx ebx,y
    mul ebx                             // Zeilen*Spalten(80) -> eax
    movzx ebx,x
    add ebx,eax                         // -> ebx
    shl ebx,1                           // *2
    call SysGetTextVideoMemBase         // -> eax
    add eax,ebx                         // Basis+Offset
  end;

function SysFileStdIn: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    sub eax,eax                         // 0
  end;

function SysFileStdOut: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,1                           // 1
  end;

function SysFileStdErr: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,2                           // 2
  end;

function SysFileOpen(FileName: PChar; Mode: Longint; var Handle: Longint): Longint;(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3d                          // OPEN FILE
    mov al,byte [mode]
    mov edx,filename
    int $21
    jc @ret

    mov edx,handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileCreate(FileName: PChar; Mode,Attr: Longint; var Handle: Longint): Longint;(*$FRAME-*)(*$USES ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3c                          // CREATE OR TRUNCATE
    mov ecx,attr
    mov edx,filename
    int $21
    jc @ret

    mov edx,handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileCreateNEW(FileName: PChar; Attr: Longint; var Handle: Longint): Longint;(*$FRAME-*)(*$USES ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$5b                          // CREATE NEW FILE
    mov ecx,attr
    mov edx,filename
    int $21
    jc @ret

    mov edx,handle
    mov [edx],eax
    sub eax,eax

  @ret:
  end;

function SysFileSeek(Handle,Distance,Method: Longint; var Actual: Longint): Longint;(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$42                          // SET CURRENT FILE POSITION
    mov al,byte [Method]
    mov ebx,Handle
    mov dx,smallword [Distance+0]       // CX:DX
    mov cx,smallword [Distance+2]
    int $21
    jc @ret

    // DX:AX
    mov ecx,Actual
    mov [ecx  ],ax
    mov [ecx+2],dx
    sub eax,eax

  @ret:
  end;

function SysFileRead(Handle: Longint; var Buffer; Count: Longint; var Actual: Longint): Longint;(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$3f
    mov ebx,handle
    mov ecx,count
    mov edx,buffer
    int $21
    jc @fehler

    // eax=gelesen
    mov edx,actual // Actual:=eax
    mov [edx],eax
    sub eax,eax
    jmp @ret

  @fehler:
    // eax=Fehler
    sub ecx,ecx    // Actual:=0
    mov edx,actual
    mov [edx],ecx

  @ret:
    call teste_strg_c
  end;

function SysFileWrite(Handle: Longint; const Buffer; Count: Longint; var Actual: Longint): Longint;(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub eax,eax                         // Bit 31..16=0
    mov ah,$40
    mov ebx,handle
    mov ecx,count
    mov edx,buffer
    int $21
    jc @fehler

    mov edx,actual
    mov [edx],eax
    sub eax,eax
    jmp @ret

  @fehler:
    sub ecx,ecx
    mov edx,actual
    mov [edx],ecx

  @ret:
    call teste_strg_c
  end;

function SysFileSetSize(Handle,NewSize: Longint): Longint;assembler;
  (*$FRAME+*)(*$USES NONE*)
  var
    posi:longint;
  asm
    // seek to
    push Handle
    push NewSize                // offset
    push 0                      // from begin of file
    lea eax,posi
    push eax
    call SysFileSeek
    or eax,eax
    jnz @fehler

    // write 0 Byte to truncate:
    push Handle                 // Handle
    push 0                      // Buffer (@nil^)
    push 0                      // Count
    lea eax,posi                // @Actual
    push eax
    call SysFileWrite
    or eax,eax
    jnz @fehler

    // null byte written can be an error...
    // if filesize<>NewSize then error occured:

    push Handle
    push 0                      // no offset
    push 2                      // to end of file
    lea eax,posi
    push eax
    call SysFileSeek
    or eax,eax
    jnz @fehler

    mov eax,[posi]              // final SysFileSetSize check
    cmp eax,[NewSize]
    jne @fehler

    sub eax,eax
    jmp @ret

  @fehler:
    or eax,-1

  @ret:
  end;

function SysFileClose(Handle: Longint): Longint;(*$FRAME-*)(*$USES EBX*)
  asm
    sub eax,eax
    mov ebx,Handle

    // if (Handle > 2) or (Handle < 0) then
    cmp ebx,0
    jl @1
    cmp ebx,2
    jle @ret

  @1:
    mov ah,$3e                          // close file using handle
    int $21
    jc @ret

    sub eax,eax                         // SysFileClose:=0

  @ret:
  end;

function SysFileFlushBuffers(Handle: Longint): Longint;(*$FRAME-*)(*$USES EBX*)
  asm
    sub eax,eax
    mov ah,$68
    mov ebx,handle
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileDelete(FileName: PChar): Longint;(*$FRAME-*)(*$USES EDX*)
  asm
    sub eax,eax
    mov ah,$41
    mov edx,FileName
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileMove(OldName,NewName: PChar): Longint;(*$FRAME-*)(*$USES EDX,EDI*)
  asm
    sub eax,eax
    mov ah,$56
    mov edx,OldName // DS:EDX
    mov edi,NewName // ES:EDI
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysFileIsDevice(Handle: Longint): longint;(*$FRAME-*)(*$USES EBX,EDX*)
  asm
    mov eax,$4400
    mov ebx,Handle
    int $21

    jc @fehler

    sub eax,eax                         // 0 .. file
    test dl,$80                         // Bit 7 = 1 -> Char/Block DEV
    setnz al                            // 1 .. device
    jmp @ret

   @fehler:
    sub eax,eax

  @ret:
  end;

function SysDirGetCurrent(Drive: Longint; Path: PChar): Longint;(*$FRAME-*)(*$USES EDX,ESI,EDI*)
  asm
    mov eax,Drive
    mov edi,Path

    or eax,eax
    jnz @laufwerk_bestimmt

    mov ah,$19
    int $21       // -> AL: 0=A 1=B ..
    inc al

  @laufwerk_bestimmt:
    mov dl,al
    add al,'A'-1
    stosb
    mov ax,'\:'   // 'X:\'
    stosw

    mov esi,edi
    sub eax,eax
    stosd
    mov ah,$47    // get current directory -> ESI (without 'X:\')
    int $21       // DL 1=A 2=B ..
    jc @ret

    sub eax,eax   // SysDirGetCurrent:=0

  @ret:
  end;

function SysDirSetCurrent(Path: PChar): Longint;(*$FRAME-*)(*$USES ECX,EDX,ESI*)
  asm
    sub ecx,ecx                         // SysDirSetCurrent:=0

    mov esi,path
    mov eax,[esi]
    cmp al,0                            // SysDirSetCurrent('')
    je @ret

    cmp ah,':'
    jne @ohne_laufwerk

    // drive change
    movzx eax,al                        // Drive:=UpCase(Path[1])
    push eax
    call _upcase
    lea edx,[eax-'A']                   // dl:=Ord(Drive)-Ord('A')

    mov ah,$0e                          // set drive dl (0..31)
    int $21

    mov ah,$19                          // get drive (0..31)
    int $21

    cmp al,dl                           // success ?
    je @1                               // yes -> set path

    mov ecx,15                          // invalid drive
    jmp @ret

  @1:                                   // Length('A:')
    inc esi
    inc esi

  @ohne_laufwerk:

    // path change
    cmp byte [esi],0                    // SysDirSetCurrent('A:')
    je @ret

    sub eax,eax
    mov ah,$3b
    mov edx,esi // DS:DX
    int $21
    jnc @ret

    mov ecx,eax

  @ret:
    mov eax,ecx
  end;


function SysDirCreate(Path: PChar): Longint;(*$FRAME-*)(*$USES EDX*)
  asm
    sub eax,eax
    mov ah,$39
    mov edx,path
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysDirDelete(Path: PChar): Longint;(*$FRAME-*)(*$USES EDX*)
  asm
    sub eax,eax
    mov ah,$3a
    mov edx,path
    int $21
    jc @ret

    sub eax,eax

  @ret:
  end;

function SysMemAvail: Longint;
  var
    dpmimeminfo:dpmimeminfo09_typ;
  begin
    dpmi_getmeminfo09(dpmimeminfo);

    with dpmimeminfo do
      begin
        if (free_pages<>$ffffffff) then
          begin
            if free_pages>(500*1024) then
              SysMemAvail:=500*1024*4096 // ~~ max(longint)
            else
              if free_pages>1 then
                SysMemAvail:=(free_pages-1)*4096
              else
                SysMemAvail:=0;
          end
        else
          begin
            if largest_available_block_in_bytes=$ffffffff then
              SysMemAvail:=512*1024 // wird hoffentlich erstmal reichen
            else
              if largest_available_block_in_bytes>2000 then
                SysMemAvail:=largest_available_block_in_bytes-2000
              else
                SysMemAvail:=0;
          end;
      end;

  end;


function SysMemAlloc(Size,Flags: Longint; var MemPtr: Pointer): Longint;
(*$FRAME-*)(*$USES EBX,ECX,ESI,EDI*)
  asm
    // Flags wird ignoriert

    mov eax,$0501       // DPMI 0.9+ ALLOCATE MEMORY BLOCK
    mov ecx,[size]      // BX:CX
    add ecx,(15+4)      // + auf 16 Byte ausgereichtet
                        // + 4 zum Speichern der Handhabe

    mov ebx,ecx
    shr ebx,16
    int $31
    jc @fehler

    // BX:CX = Adresse
    // SI:DI = Handhabe

    shl ebx,16
    mov bx,cx

    // auf 16 Byte ausrichten, mit Platz fÅr Handhabe
    lea eax,[ebx+(15+4)]
    and eax,$fffffff0

    // SI:DI Handhabe abspeichern
    mov [eax-4+2],si
    mov [eax-4+0],di

    // MemPtr:=eax
    mov edi,MemPtr
    mov [edi],eax
    // Result:=0
    sub eax,eax
    jmp @ret

  @fehler:
    // MemPtr :=nil
    mov edi,MemPtr
    and longint [edi],0
    // Result:=8
    mov eax,8

  @ret:
  end;

function SysMemFree(MemPtr: Pointer): Longint;(*$FRAME-*)(*$USES ESI,EDI*)
  asm
    mov eax,[memptr]

    // get Handle (SI:DI)
    mov esi,[eax-4+2]
    mov edi,[eax-4+0]

    mov eax,$00000502    // DPMI 0.9+ FREE MEMORY BLOCK
    int $31
    jc @ret

    sub eax,eax

  @ret:
  end;


function SysSysMsCount: Longint;(*$FRAME-*)(*$USES EDX*)
  asm
    // ungenau (nur 1/18 Sekunde)
    mov eax,[Seg0040+$6c]               // SysSysMsCount:=meml[seg0040+$6c]*55;
    mov edx,55                          // 1024/18.2
    mul edx
  end;

function SysFileOpen_Create(Open: Boolean;FileName: PChar; Mode,Attr,Action: Longint; var Handle: Longint): Longint;(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub eax,eax
    mov edx,filename

    cmp open,true
    jne @open_false

    cmp action,0
    jne @1

    // open=true, action=0
    // SysFileOpen(FileName,Mode,Handle);
    push edx
    push Mode
    push handle
    call SysFileOpen
    // SysFileOpen_Create:=eax
    jmp @ret

  @1:
    cmp action,open_CreateIfNew
    jne @2
    // open=true, action=open_CreateIfNew
    // SysFileOpen(FileName,Mode,Handle);
    push edx
    push Mode
    push handle
    call SysFileOpen
    or eax,eax
    // SysFileOpen_Create:=eax (0)
    jz @ret

    // SysFileCreate
    push edx
    push Mode
    push Attr
    push handle
    call SysFileCreate
    jmp @ret

  @2:
    // open=true, action=TruncateIfExist
    push edx
    push Mode
    push handle
    call SysFileOpen
    or eax,eax
    // SysFileOpen_Create:=error
    jnz @ret
    // SysFileSetSize(Handle,0)
    mov eax,Handle
    push longint [eax] // Handle
    push 0
    call SysFileSetSize
    jmp @ret

    //*** open = false ***

  @open_false:
    cmp action,0
    jne @3
    // open=false, action=0
    // SysFileCreateNEW(FileName,Attr,Handle)
    push edx
    push Attr
    push Handle
    call SysFileCreateNEW
    jmp @ret

  @3:
    // open=false, action=TruncateIfExist
    // SysFileCreate(FileName,Mode,Attr,Handle)
    push edx
    push Mode
    push Attr
    push Handle
    call SysFileCreate
    // jmp @ret
  @ret:
  end;

function SysFileCopy(_Old, _New: PChar; _Overwrite: Boolean): Boolean;
  var
    d1,d2       :file;
    p           :array[0..4*1024-1] of byte;
    org_FileMode:longint;
    groesse,
    pos,
    block,
    wirklich    :longint;
    fehler      :longint;
  begin
    SysFileCopy:=false;

    Assign(d1,_old);
    Assign(d2,_new);

    org_FileMode:=FileMode;
    FileMode:=open_access_WriteOnly;

    if _overwrite then
      begin
        (*I-*)
        Rewrite(d2,1);
        (*$I+*)
        FileMode:=org_FileMode;
        if IOResult<>0 then
          Exit;
      end
    else
      begin
        (*$I-*)
        Reset(d2,1);
        (*$I+*)
        if IOResult=0 then
          begin
            FileMode:=org_FileMode;
            Exit;
          end;

        (*$I-*)
        Rewrite(d2,1);
        FileMode:=org_FileMode;
        (*$I+*)
        if IOResult<>0 then
          Exit;
      end;


    (*$I-*)
    FileMode:=open_access_ReadOnly+open_share_DenyNone;
    Reset(d1,1);
    FileMode:=org_FileMode;
    (*$I+*)

    if InOutRes<>0 then
      begin
        Close(d2);
        Exit;
      end;

    groesse:=FileSize(d1);
    pos:=0;
    repeat
      if pos=groesse then
        begin
          SysFileCopy:=true;
          Break;
        end;

      block:=groesse-pos;
      if block>SizeOf(p) then
        block:=SizeOf(p);

      BlockRead (d1,p,block,wirklich);
      if block<>wirklich then Break;
      BlockWrite(d2,p,block,wirklich);
      if block<>wirklich then Break;

      Inc(pos,block);
    until false;

    Close(d1);
    Close(d2);
  end;

function SysCtrlSelfAppType: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,1 // Vollbild ... should be 0 (Online Help is wrong)
  end;

function SysCtrlCreateThread(Attrs: Pointer; StackSize: Longint; Func,Param: Pointer; Flags: Longint; var Tid: Longint): Longint;
  begin
    (*$IFDEF THREAD_SUPPORT*)
    dpmi32_install_thread_code;
    SysCtrlCreateThread:=
      Dpmi32CreateThread(tid,Func,Ofs(Param),Flags,StackSize);
    (*$ELSE*)
    SysCtrlCreateThread:=-1;
    (*$ENDIF*)
  end;

function SysCtrlKillThread(Handle: Longint): Longint;
  begin
    (*$IFDEF THREAD_SUPPORT*)
    SysCtrlKillThread:=
      Dpmi32KillThread(Handle,0);
    (*$ELSE*)
    SysCtrlKillThread:=-1;
    (*$ENDIF*)
  end;

function SysCtrlSuspendThread(Handle: Longint): Longint;
  begin
    (*$IFDEF THREAD_SUPPORT*)
    SysCtrlSuspendThread:=
      Dpmi32SuspendThread(Handle);
    (*$ELSE*)
    SysCtrlSuspendThread:=-1;
    (*$ENDIF*)
  end;

function SysCtrlResumeThread(Handle: Longint): Longint;
  begin
    (*$IFDEF THREAD_SUPPORT*)
    SysCtrlResumeThread:=
      Dpmi32ResumeThread(Handle);
    (*$ELSE*)
    SysCtrlResumeThread:=-1;
    (*$ENDIF*)
  end;

function SysGetThreadId: Longint;
  begin
    (*$IFDEF THREAD_SUPPORT*)
    SysGetThreadId:=
      Dpmi32GetThreadId;
    (*$ELSE*)
    // -> GetThreadId
    SysGetThreadId:=1;
    (*$ENDIF*)
  end;

procedure SysCtrlExitThread(ExitCode: Longint);
  begin
    (*$IFDEF THREAD_SUPPORT*)
    Dpmi32KillThread(Dpmi32GetThreadId,ExitCode);
    (*$ELSE*)
    //NOT_IMPLEMENTED
    (*$ENDIF*)
  end;

procedure SysCtrlExitProcess(ExitCode: Longint);(*$FRAME-*)(*$USES NONE*)
  asm
    // debug support:  See  deb_link.pas
    nop
    mov ah,$4c
    mov al,byte [exitcode] // lo(ExitCode)
    int $21
  end;

function SysCtrlGetModuleName(Handle: Longint; Buffer: PChar): Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    // not used ?
    //NOT_IMPLEMENTED
    @1:
    mov eax,[Buffer]
    push offset @1
    push eax
    push 260
    call SysGetModuleName
    sub eax,eax
  end;

procedure SysCtrlEnterCritSec;(*$FRAME-*)(*$USES NONE*)
  asm
    (*$IFDEF THREAD_SUPPORT*)
    jmp Dpmi32EnterCritSec
    (*$ELSE*)
    //cli
    (*$ENDIF*)
  end;

procedure SysCtrlLeaveCritSec;(*$FRAME-*)(*$USES NONE*)
  asm
    (*$IFDEF THREAD_SUPPORT*)
    jmp Dpmi32LeaveCritSec
    (*$ELSE*)
    //sti
    (*$ENDIF*)
  end;


function SysGetProcessId: Longint;
  begin
    Result := sel_psp;
  end;

var
  dpmi32_tlsmapmem      :array[0..SharedMemSize-1] of byte;

const
  tls1:integer=0;

function SysCtrlGetTlsMapMem: Pointer;

begin
  Result:=@dpmi32_tlsmapmem;
  if tls1=0 then
  begin
    tls1:=1;
    FillChar(Result^, SharedMemSize, $FF);
    FillChar(Result^, SizeOf(TSharedMem), 0);
    System.GetMemoryManager( PSharedMem(Result)^.TlsMemMgr );
  end;
end;


var
  syscmdln_var          :array[0..512] of char;
  parax_length_array    :array[0..100] of longint;
  SysCmdlnCount_var     :longint;

function SysCmdln: PChar;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,offset syscmdln_var
  end;

function SysCmdlnCount: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,SysCmdlnCount_var
  end;

procedure SysCmdlnParam(Index: Longint; var Param: ShortString);(*$FRAME-*)(*$USES EAX,ECX,EDX,ESI,EDI*)
  asm
    // Param:=''
    mov edi,param
    mov byte [edi],0

    mov edx,index
    mov esi,offset parax_length_array
    mov edi,offset syscmdln_var
    sub ecx,ecx
    cld

    @schleife:
    lodsd

    // out of Paramstr ?
    or eax,eax // 0 ?
    jz @ret

    // counter=index ?
    cmp edx,ecx
    je @gefunden

    // skip eax chars, increment counter
    add edi,eax
    inc edi     // length(' ')
    inc ecx     // inc(counter)
    jmp @schleife

    @gefunden:

    // Parameter like "Demo"
    cmp byte [edi],'"'
    jne @nicht_anf

    // Length('"DEMO"')-2*Length('"')
    dec eax
    dec eax
    // skip leading '"'
    inc edi

    @nicht_anf:

    mov esi,edi
    mov ecx,eax
    mov edi,Param
    mov al,cl
    stosb
    jecxz @ret
    rep movsb

    @ret:
  end;

function SysGetEnvironment: PChar;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,Environment
  end;

procedure berechne_parameter;
  var
    quelle,
    ziel                :PChar;
    l1                  :longint;
  begin
    FillChar(syscmdln_var      ,SizeOf(syscmdln_var      ),0);
    FillChar(parax_length_array,SizeOf(parax_length_array),0);
    SysCmdlnCount_var:=0;

    // Paramstr(0)
    quelle:=Environment;
    while MemW[Ofs(quelle^)]<>0 do
      Inc(quelle);

    Inc(quelle,4); // skip byte $00 $00 $01 $00

    l1:=0;
    while quelle[l1]<>#0 do
      Inc(l1);

    Move(quelle^,syscmdln_var,l1);
    ziel:=@syscmdln_var[l1+1];  // Cmdline=Paramstr(0)+#0+Paramstr(1)+' '...
                                //                       ^
    parax_length_array[SysCmdlnCount_var]:=l1;

    // ParamStr(1+)
    quelle:=Ptr(seg_psp+$81);
    while quelle[0] in [#9,' '] do
      Inc(quelle);

    repeat
      // End of Parameters ?
      if quelle[0] in [#0,#10,#13] then
        Break;

      // skip spaces ?
      if quelle[0] in [#9,' '] then
        begin
          Inc(quelle);
          Continue;
        end;

      // found ..
      Inc(SysCmdlnCount_var);

      if SysCmdlnCount_var>1 then
        begin
          // separate : ' '
          ziel[0]:=' ';
          Inc(ziel);
        end;

      if quelle[0]='"' then
        begin
          // search terminating '"'
          l1:=1;
          while not (quelle[l1] in [#0,#10,#13,'"']) do
            Inc(l1);
          // not including '"'

          // copy calculated para ('"DEMO')
          Move(quelle^,ziel^,l1);
          Inc(ziel,l1);
          // move start
          Inc(quelle,l1);

          // trailing '"'
          ziel[0]:='"';
          Inc(ziel);

          // store length('"DEMO"')
          parax_length_array[SysCmdlnCount_var]:=l1+1;

          // skip trailing '"'
          if quelle[0]='"' then
            Inc(quelle);
        end
      else
        begin
          // search terminating ' '
          l1:=0;
          while not (quelle[l1] in [#0,#9,#10,#13,' ']) do
            Inc(l1);

          // copy calculated para
          Move(quelle^,ziel^,l1);
          Inc(ziel,l1);
          // move start
          Inc(quelle,l1);

          parax_length_array[SysCmdlnCount_var]:=l1;
        end;

    until false;

  end;


function SysOsVersion: Longint;(*$FRAME-*)(*$USES EBX,ECX*)
  asm
    sub eax,eax
    mov ah,$30
    int $21
  end;

procedure schreibe_edi;
(*$FRAME-*)(*$USES NONE*)
  asm
    or edi,edi // if Assigned(edi)
    jz @ret

    mov [edi],eax

  @ret:
  end;

procedure SysGetDateTime(Year,Month,Day,DayOfWeek,Hour,Minute,Second,MSec: PLongint);assembler;
(*$FRAME-*)(*$USES ALL*)
  asm
    mov ebp,offset schreibe_edi

    mov ah,$2a // get system date
    int $21

    mov edi,dayofweek
    movzx eax,al
    call ebp

    mov edi,year
    movzx eax,cx
    call ebp

    mov edi,month
    movzx eax,dh
    call ebp

    mov edi,day
    movzx eax,dl
    call ebp

    mov ah,$2c // get system time
    int $21

    mov edi,hour
    movzx eax,ch
    call ebp

    mov edi,minute
    movzx eax,cl
    call ebp

    mov edi,second
    movzx eax,dh
    call ebp

    mov edi,msec
    movzx eax,dl        // eax=h
    shl eax,1           // eax=2*h
    lea eax,[eax+eax*4] // eax=10*h
    movzx eax,dl
    call ebp
  end;

procedure SysSetDateTime(Year,Month,Day,Hour,Minute,Second,MSec: PLongint);
(*$FRAME-*)(*$USES ALL*)
  asm
    mov ah,$2a // get system date
    int $21

    mov esi,year
    or esi,esi
    jz @1
    mov cx,smallword [esi]
  @1:

    mov esi,month
    or esi,esi
    jz @2
    mov dh,byte [esi]
  @2:

    mov esi,day
    or esi,esi
    jz @3
    mov dl,byte [esi]
  @3:

    mov ah,$2b // set system date
    int $21


    mov ah,$2c // get system time
    int $21

    mov esi,hour
    or esi,esi
    jz @4
    mov ch,byte [esi]
  @4:

    mov esi,minute
    or esi,esi
    jz @5
    mov cl,byte [esi]
  @5:

    mov esi,second
    or esi,esi
    jz @6
    mov dh,byte [esi]
  @6:

    mov esi,msec
    or esi,esi
    jz @7

    push ecx
      push edx
        mov eax,[esi]
        cdq
        mov ecx,10
        div ecx
      pop edx
    pop ecx
    mov al,al
  @7:

    mov ah,$2d // set system time
    int $21

  end;

function SysVerify(SetValue: Boolean; Value: Boolean): Boolean;(*$FRAME-*)(*$USES EDX*)
  asm
    cmp setvalue,true
    jne @verify_lesen

    sub eax,eax // off
    cmp value,true
    sete al    // on

    mov ah,$2e // al=verify
    mov dl,0
    int $21

  @verify_lesen:

    mov ah,$54
    int $21

    // al Verify  boolean  ord
    //  0  off    false    0
    //  1  on     true     1
  end;

function SysDiskFreeLong(Drive: Byte): TQuad;assembler;
  (*$FRAME-*)(*$USES ALL*)
  var
    q4:comp;
  asm
    mov ah,$36
    mov dl,drive
    int $21

    cmp ax,$ffff
    je @fehler

    movzx eax,ax
    movzx ebx,bx
    movzx ecx,cx
    mul ebx
    mul ecx
    // -> EDX:EAX

    jmp @ret

  @fehler:
    // return -1
    or eax,-1
    mov edx,eax

  @ret:
    // return edx:eax
    mov longint [q4+0],eax
    mov longint [q4+4],edx

    // SysDiskFreeLong:=q4;
    fild qword ptr [q4]
    fwait
  end;

function SysDiskSizeLong(Drive: Byte): TQuad;assembler;
  (*$FRAME-*)(*$USES ALL*)
  var
    q4:comp;
  asm
    mov ah,$36
    mov dl,drive
    int $21

    cmp ax,$ffff
    je @fehler

    movzx eax,ax
    movzx edx,dx
    movzx ecx,cx
    mul edx
    mul ecx
    // -> EDX:EAX

    jmp @ret

  @fehler:
    // return -1
    or eax,-1
    mov edx,eax

  @ret:
    // return edx:eax
    mov longint [q4+0],eax
    mov longint [q4+4],edx

    // SysDiskSizeLong:=q4;
    fild qword ptr [q4]
    fwait
  end;

function SysGetFileAttr(FileName: PChar; var Attr: Longint): Longint;
(*$FRAME-*)(*$USES ECX,EDX*)
  asm
    mov eax,$00004300
    mov edx,filename
    sub ecx,ecx
    int $21
    jc @ret

    mov edx,attr
    mov [edx],ecx
    sub eax,eax

  @ret:
  end;

function SysSetFileAttr(FileName: PChar; Attr: Longint): Longint;
(*$FRAME-*)(*$USES ECX,EDX*)
  asm
    mov eax,$00004301
    mov ecx,attr
    mov edx,filename
    int $21
    jc @ret

    sub eax,eax

    @ret:
  end;

function SysGetFileTime(Handle: Longint; var Time: Longint): Longint;
(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    sub ecx,ecx
    sub edx,edx

    mov eax,$00005700
    mov ebx,handle
    int $21
    jc @ret

    shl edx,16 // CX=Zeit DX=Datum
    or ecx,edx

    mov edx,time
    mov [edx],ecx
    sub eax,eax

    @ret:
  end;

function SysSetFileTime(Handle: Longint; Time: Longint): Longint;
(*$FRAME-*)(*$USES EBX,ECX,EDX*)
  asm
    mov eax,$00005701
    mov ebx,handle

    mov cx,smallword [time+0]
    mov dx,smallword [time+2]
    int $21
    jc @ret

    sub eax,eax

    @ret:
  end;

procedure sichere_DTA(var org_dta:real_mode_ptr_typ);assembler;
(*$FRAME-*)(*$USES EAX,ESI,EDI*)
  var
    regs:real_mode_call_structure_typ;
  asm
    mov [regs.ah_],$2f  // GET DTA
    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,org_dta
    mov ax,[regs.ds_]   // -> ES:BX
    mov [edi+real_mode_ptr_typ.seg_],ax
    mov eax,[regs.ebx_]
    mov [edi+real_mode_ptr_typ.ofs_],ax
  end;

procedure setze_DTA(var dta:real_mode_ptr_typ);assembler;
(*$FRAME-*)(*$USES EAX,ESI,EDI*)
  var
    regs:real_mode_call_structure_typ;
  asm
    mov edi,dta
    mov [regs.ah_],$1a  // SET DTA DS:DX
    mov ax,[edi+real_mode_ptr_typ.seg_]
    mov [regs.ds_],ax
    mov ax,[edi+real_mode_ptr_typ.ofs_]
    mov [regs.edx_],eax
    lea eax,regs
    push eax
    push $21
    call intr_realmode
  end;


procedure bearbeite_sr(var F: TOSSearchRec; IsPChar: Boolean);
  begin
    with F do
      begin
        attr:=dos_dta.attr;
        time:=dos_dta.time;
        size:=dos_dta.size;

        if ispchar then
          StrCopy(PChar(@name),dos_dta.name)
        else
          name:=StrPas(dos_dta.name);
      end;
  end;

const
  ofs_suchmaske=$200; // genug Platz fÅr TOSSearchRec

function SysFindFirst(Path: PChar; Attr: Longint; var F: TOSSearchRec; IsPChar: Boolean): Longint;
  var
    reg                 :real_mode_call_structure_typ;
    org_dta             :real_mode_ptr_typ;
  begin
    init_register(reg);
    f.attr_must:=attr shr 8;

    // copy '*.*'#0
    StrCopy(PChar(@Mem[segdossyslow32+ofs_suchmaske]),Path);

    sichere_DTA(org_dta);
    setze_DTA(arbeits_dta);
    FillChar(Mem[segdossyslow32],SizeOf(f.dos_dta),0);

    with reg do
      begin
        ah_:=$4e;            // FINDFIRST
        ds_:=segdossyslow16; // DS:DX
        dx_:=ofs_suchmaske;
        cx_:=attr;
        intr_realmode(reg,$21);
      end;

    setze_DTA(org_dta);

    // copy arbeits_dta -> F

    Move(Mem[segdossyslow32],f.dos_dta,SizeOf(f.dos_dta));
    bearbeite_sr(f,IsPChar);

    // Error on FindFirst ?
    if (reg.flags_ and flags_carry)<>0 then
      begin
        SysFindFirst:=reg.ax_;
        Exit;
      end;

    // success ... but matches attr ?
    if (f.attr and f.attr_must)=f.attr_must then
      begin
        SysFindFirst:=0; // success !
        Exit;
      end;

    // let findnext do the work ...
    SysFindFirst:=SysFindNext(f,ispchar);
  end;

function SysFindNext(var F: TOSSearchRec; IsPChar: Boolean): Longint;
  var
    reg                 :real_mode_call_structure_typ;
    org_dta             :real_mode_ptr_typ;
  begin
    Move(f.dos_dta,Mem[segdossyslow32],SizeOf(f.dos_dta));
    init_register(reg);

    repeat

      sichere_DTA(org_dta);
      setze_DTA(arbeits_dta);

      reg.ah_:=$4f;            // FINDNEXT
      intr_realmode(reg,$21);

      setze_DTA(org_dta);

      // copy arbeits_dta -> F
      Move(Mem[segdossyslow32],f.dos_dta,SizeOf(f.dos_dta));
      bearbeite_sr(f,IsPChar);

      // found ?
      if (reg.flags_ and flags_carry)<>0 then
        begin
          SysFindNext:=reg.ax_;
          Exit;
        end;

      // success ... but matches attr ?
      if (f.attr and f.attr_must)=f.attr_must then
        begin
          SysFindNext:=0; // success !
          Exit;
        end;

    until false;
  end;

function SysFindClose(var F: TOSSearchRec): Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    sub eax,eax
  end;

// Check if file exists; if it does, update FileName parameter
// to include correct case of existing file
// VpSysOs2.PAS
function SysFileAsOS(FileName: PChar): Boolean;
  var
    SRec: TOSSearchRec;
    P: PChar;
  begin
    Result := False;
    if SysFindFirst(FileName, $37, SRec, False) = 0 then
      begin
        if SRec.Name[1] <> #0 then
          begin
            // Replace filename part with data returned by OS
            P := StrRScan(FileName, '\');
            if P = nil then
              P := FileName
            else
              inc(P); // Point to first character of file name
            strPcopy(P, SRec.Name);
          end;
        SysFindClose(SRec );
        Result := True;
      end;
  end;

function SysFileSearch(Dest,Name,List: PChar): PChar;
  var
    sr                  :TOSSearchRec;
    posi,
    ende,
    arbeit              :PChar;
    tmp                 :array[0..260-1] of char;
    pfad                :array[0..260-1] of char;
    verzeichnis,
    gefunden            :boolean;

  begin
    FillChar(Dest^,{260}255,#0);
    SysFileSearch:=Dest;

    if Name[0]=#0 then // Name='' ?
      Exit;

    SysFileExpand(tmp,Name);
    arbeit:=StrEnd(tmp)-1;
    if not (arbeit[0] in ['\','/']) then
      StrCat(tmp,'\');
    StrCat(tmp,'*.*');

    // Verzeichnis oder Datei ?
    // $10** weil OS/2 sonst Åberall den DatentrÑgername findet
    verzeichnis:=(SysFindFirst(tmp,$10ff,sr,true)=0); // true=(sr.name=pchar)
    SysFindClose(sr);

    if verzeichnis then
      SysFileExpand(Dest,Name)
    else
      begin

        StrCopy(tmp,'.\;');
        StrCat(tmp,List);

        arbeit:=StrEnd(tmp)-1;

        // make sure there is trailing ';'
        if arbeit[0]<>';' then
          StrCat(tmp,';');

        posi:=@tmp;
        repeat
          ende:=StrPos(posi,';');

          ende[0]:=#0;
          Inc(ende);

          StrCopy(pfad,posi);
          arbeit:=StrEnd(pfad)-1;
          if not (arbeit[0] in ['\','/']) then
            StrCat(pfad,'\');
          StrCat(pfad,Name);

          gefunden:=(SysFindFirst(pfad,$ff,sr,true)=0);
          SysFindClose(sr);

          if gefunden then
            begin
              SysFileExpand(Dest,Pfad);
              Exit;
            end;

          posi:=ende;

        until posi[0]=#0;
      end;
  end;

// aus VpSysOs2.PAS
function SysFileExpand(Dest,Name: PChar): PChar;
  var
    I,J,L               : Integer;
    C                   : Char;
    CurDir              : array[0..259] of Char;

  procedure AdjustPath;
    begin
      if (Dest[J-2] = '\') and (Dest[J-1] = '.') then
        Dec(J,2)
      else
        if (j>3) and (Dest[J-3] = '\') and (Dest[J-2] = '.') and (Dest[J-1] = '.') then
          begin
            Dec(J, 3);
            if Dest[J-1] <> ':' then
              repeat
                Dec(J);
              until Dest[J] = '\';
          end;
    end;

  begin
    L := StrLen(Name);

    if (L >= 2) and ((Name[1] = ':') or (Name[0] = '\') and (Name[1] = '\')) then
      begin                         // Path is already in form 'X:\Path'
        if (L >= 3) and (Name[2] in ['\','/']) then
          StrCopy(Dest, Name)
        else
          begin                     // Path is in form 'X:Path'
            SysDirGetCurrent(Ord(UpCase(Name[0])) - (Ord('A') - 1), CurDir);
            if StrLen(CurDir) > 3 then
              StrCat(CurDir, '\');
            StrLCat(StrCopy(Dest, CurDir), PChar(@Name[2]), 259);
          end;
      end
    else
      begin                         // Path is without drive letter
        SysDirGetCurrent(0, CurDir);// Get default drive & directory
        if StrLen(CurDir) > 3 then
          StrCat(CurDir, '\');
        if Name[0] in ['\','/'] then
          StrLCopy(Dest, PChar(@CurDir[0]), 2) // 'X:' only
        else
          StrCopy(Dest, CurDir);
        StrLCat(Dest, Name, 259);
      end;

    I := 0; J := 0;
    for I := 0 to StrLen(Dest)-1 do
      begin
        C := Dest[I];
        if C='/' then
          begin
            C:='\';
            Dest[I]:=C;
          end;
        if C = '\' then AdjustPath;
        Dest[J] := C;
        Inc(J);
      end;

    AdjustPath;
    if Dest[J-1] = ':' then
      begin
        Dest[J] := '\';
        Inc(J);
      end;

    Dest[J] := #0;
    Result := Dest;
  end;

// wird ignoriert:
// * Env (Async=true)
// * PID (Async=true or Async=false)
// * StdIn,StdOut,StdErr (Async=true)

threadvar
  LastAsync             :Boolean;
  last_exitcode         :longint;

function SysExecute(Path,CmdLine,Env: PChar; Async: Boolean; PID: PLongint; StdIn,StdOut,StdErr: Longint): Longint;
  type
    dos_exe_parablock_typ=
      packed record
        env_seg         :smallword;
        cmdline_ptr     :real_mode_ptr_typ;
        fcb5c           :real_mode_ptr_typ;
        fcb6c           :real_mode_ptr_typ;
        res_sssp        :real_mode_ptr_typ;
        res_csip        :real_mode_ptr_typ;
      end;

    os2_startdata_typ=
      packed record
        cb              :smallword;             // length of structure (must be 0018h,001Eh,0020h,0032h,or 003Ch)
        relation        :smallword;             // relation of new process to caller (00h independent, 01h child)
        foreback        :smallword;             // fore/background (00h foreground, 01h background)
        trace           :smallword;             // trace options (00h-02h, 00h = no trace)
        title           :real_mode_ptr_typ;     // pointer to ASCIZ program title (max 62 chars) or 0000h:0000h
        name            :real_mode_ptr_typ;     // pointer to ASCIZ program name (max 128 chars) or 0000h:0000h
        args            :real_mode_ptr_typ;     // pointer to ASCIZ program args (max 144 chars) or 0000h:0000h
        termq           :longint;               // "TermQ" (currently reserved, must be 00000000h)
        // $18
        env             :real_mode_ptr_typ;     // pointer to environment (max 486 bytes) or 0000h:0000h
        inheritance     :smallword;             // inheritance (00h or 01h)
        // $1e
        session_type    :smallword;             // session type 0,1,2,3,4,7
        // $20
        icon            :real_mode_ptr_typ;     // pointer to ASCIZ icon filename (max 128 chars) or 0000h:0000h
        pgmhandle       :longint;
        pgmcontrol      :smallword;
        initial_column  :smallword;
        initial_row     :smallword;
        initial_with    :smallword;
        initial_height  :smallword;
        // $32
        reserved1       :smallword;
        objectbuffer    :longint;
        objectbufferlen :longint;
      end;

  var
    l1                  :longint;
    regs                :real_mode_call_structure_typ;
    env_laenge          :longint;
    env_low16           :smallword;
    env_low32           :longint;
    StdHandles          :array[0..2] of Longint;
    NewHandles          :array[0..2] of Longint;
    OldHandles          :array[0..2] of Longint;
    I                   :Longint;
  begin
    LastAsync := Async;
    last_exitcode:=-1;

    init_register(regs);

    StrCopy(PChar(@Mem[segdossyslow32]),Path);
    l1:=StrLen(CmdLine);
    Mem[segdossyslow32+$100]:=l1+1;
    Mem[segdossyslow32+$101]:=Ord(' ');
    Move(CmdLine^,Mem[segdossyslow32+$102],l1);
    Mem[segdossyslow32+$102+l1  ]:=$0d;
    Mem[segdossyslow32+$102+l1+1]:=$00;

    FillChar(dos_exe_parablock_typ(Mem[segdossyslow32+$200]),SizeOf(dos_exe_parablock_typ),0);
    with     dos_exe_parablock_typ(Mem[segdossyslow32+$200]) do
      begin
        cmdline_ptr.seg_:=segdossyslow16;
        cmdline_ptr.ofs_:=$100;
      end;


    if async then
      begin
        if taskmgr then (*** DR-DOS TASKMGR ***)
          begin
            with regs do
              begin
                ax_:=$2707; // create new task
                bx_:=$0200; // ES:BX = PARABLOCK
                dx_:=$0000; // DS:DX = Programm
                cx_:=0;     // switch
                ds_:=segdossyslow16;
                es_:=segdossyslow16;
              end;
            intr_realmode(regs,$2f);
            SysExecute:=0;
            last_exitcode:=0; // unbekannt
          end
        else if os2 then (*** OS/2 VDM/VMB ***)
          begin
            FillChar(os2_startdata_typ(Mem[segdossyslow32+$200]),SizeOf(os2_startdata_typ),0);
            with     os2_startdata_typ(Mem[segdossyslow32+$200]) do
              begin
                cb:=$18;
                name.seg_:=segdossyslow16;
                name.ofs_:=0;
                args.seg_:=segdossyslow16;
                args.ofs_:=$102;
              end;

            with regs do
              begin
                ah_:=$64;
                bx_:=$0025;
                cx_:=$636c;
                si_:=$200; // DS:SI
                ds_:=segdossyslow16;
              end;
            intr_realmode(regs,$21);

            SysExecute:=regs.ax_;
            last_exitcode:=0; // unbekannt
          end
        else (*** no support for async ***)
          begin
            SysExecute:=-1;
            last_exitcode:=0;
          end
      end
    else (*** async =false -> DOS 2.x+ ***)
      begin
        // Umgebung > 1MB kopieren
        // 1. LÑnge berechnen
        if Env=nil then env:=Environment;
        env_laenge:=0;
        while MemW[ofs(Env^)+env_laenge]<>0 do
          Inc(env_laenge);
        Inc(env_laenge,2);
        // 2. Speicher anfordern
        if getdosmem(env_low16,env_laenge)<>0 then
          begin
            SysExecute:=8; // Mem ...
            last_exitcode:=0;
          end
        else
          begin
            // 3. Umgebung kopieren
            env_low32:=dosseg_linear(env_low16);
            Move(Env^,Mem[env_low32],env_laenge);
            dos_exe_parablock_typ(Mem[segdossyslow32+$200]).env_seg:=env_low16;


            with regs do
              begin
                ax_:=$4b00; // EXEC
                bx_:=$0200; // ES:BX = PARABLOCK
                dx_:=$0000; // DS:DX = Programm
                ds_:=segdossyslow16;
                es_:=segdossyslow16;
              end;


            StdHandles[0] := StdIn;
            StdHandles[1] := StdOut;
            StdHandles[2] := StdErr;

            for I := 0 to 2 do
              if StdHandles[I] <> -1 then
                begin
                  OldHandles[I] := $FFFFFFFF;       // Save original StdIn to OldIn
                  NewHandles[I] := I;
                  DPMI32DupHandle(NewHandles[I], OldHandles[I]);
                  DPMI32DupHandle(StdHandles[I], NewHandles[I]);
                end;

            // swapvectors
            entferne_exception_behandlungen;
            // exec
            intr_realmode(regs,$21);
            // swapvectors
            installiere_exception_behandlungen;

            for I := 0 to 2 do
              if StdHandles[I] <> -1 then
                begin
                  DPMI32DupHandle(OldHandles[I], NewHandles[I]);
                  SysFileClose(OldHandles[I]);
                end;


            freedosmem(env_low16);

            if (regs.flags_ and flags_carry)<>0 then // ERROR
              begin
                SysExecute:=regs.ax_;
                last_exitcode:=-1;
              end
            else
              begin
                SysExecute:=0;
                // Exitcode ermittlen
                (*$Alters EAX,ECX*)
                asm
                  mov ah,$4d
                  int $21
                  movzx ecx,ax // hi(ax)=0/1/2/3 lo(ax)=exit
                  push offset last_exitcode
                  call _GetTlsVar
                  mov [eax],ecx
                end;
              end
          end; (* else getdosmem(env_low16) *)
      end; (* else async *)
  end;

function SysExitCode: Longint;
  begin
    SysExitCode:=last_exitcode;
  end;

const
  max_sem               =100;
  max_sem_namelen       =80;
  mutexsem_sig          =Ord('M')
                        +Ord('U') shl 8
                        +Ord('T') shl 16
                        +Ord('X') shl 24;
  eventsem_sig          =Ord('E')
                        +Ord('V') shl 8
                        +Ord('S') shl 16
                        +Ord('E') shl 24;

type
  sem_types             =(sem_type_unused,sem_type_mutex,sem_type_event);

  mutexsem_typ=
    packed record
      sig               :longint;
      status            :longint;
      eigentuemer       :longint;
    end;

  eventsem_typ=
    packed record
      sig               :longint;
      status            :longint;
    end;


  mutexsem_z_typ        =^mutexsem_typ;
  eventsem_z_typ        =^eventsem_typ;


var
  sem_array                     :
    array[1..max_sem] of
      record
        sem_name                :array[0..max_sem_namelen] of Char;
        opencount               :longint;
        case used:sem_types of
          sem_type_unused       :(private_data:byte );
          sem_type_mutex        :(mutex:mutexsem_typ);
          sem_type_event        :(event:eventsem_typ);
        end;

const
  sem_init_complete             :boolean=false;

procedure init_semaphore_table;
  begin
    if sem_init_complete then Exit;

    FillChar(sem_array,SizeOf(sem_array),0);
    sem_init_complete:=true;
  end;



function Search_MutexSem(_Name:pChar):longint;
  var
    i:longint;
  begin
    Result:=0;
    if StrLen(_Name)=0 then Exit;

    for i:=Low(sem_array) to High(sem_array) do
      with sem_array[i] do
        if used=sem_type_mutex then
          if StrComp(sem_name,_Name)=0 then
            begin
              Result:=i;
              Exit;
            end;
  end;

function Search_EventSem(_Name:pChar):longint;
  var
    i:longint;
  begin
    Result:=0;

    i:=StrLen(_Name);
    if (i=0) or (i>max_sem_namelen) then Exit;

    for i:=Low(sem_array) to High(sem_array) do
      with sem_array[i] do
        if used=sem_type_event then
          if StrComp(sem_name,_Name)=0 then
            begin
              Result:=i;
              Exit;
            end;
  end;

function Search_Unused_Sem:longint;
  var
    i:longint;
  begin
    Result:=0;
    for i:=Low(sem_array) to High(sem_array) do
      if sem_array[i].used=sem_type_unused then
        begin
          Result:=i;
          Exit;
        end;
  end;


function Search_Used_Sem(const _Handle: TSemhandle):longint;
  var
    i:longint;
  begin
    Result:=0;
    for i:=Low(sem_array) to High(sem_array) do
      if Ofs(sem_array[i].private_data)=_Handle then
        begin
          Result:=i;
          Exit;
        end;
  end;

procedure Free_Sem(const i:longint);
  begin
    FillChar(sem_array[i],SizeOf(sem_array[i]),0);
  end;


(*** Sem*Event *****************************************************)

function SemCreateEvent(_Name: pChar; _Shared, _State: Boolean): TSemHandle;
  var
    i:longint;
  begin
    Result:=-1;

    // allready exist ?
    if Search_EventSem(_Name)<>0 then Exit;

    // get free entry
    i:=Search_Unused_Sem;
    if i=0 then Exit;

    with sem_array[i] do
      begin
        if _Name<>nil then
          StrCopy(sem_name,_Name);
        opencount:=1;
        used:=sem_type_event;
        event.sig:=eventsem_sig;
        event.status:=Ord(_State);
        Result:=Ofs(event);
      end;

  end;

function SemAccessEvent(_Name: pChar): TSemHandle;
  var
    i:longint;
  begin
    Result:=-1;

    // allready exist ?
    i:=Search_EventSem(_Name);
    if i=0 then Exit;


    with sem_array[i] do
      begin
        SysCtrlEnterCritSec;
        Inc(opencount);
        SysCtrlLeaveCritSec;
        Result:=Ofs(event);
      end;
  end;

function SemPostEvent(_Handle: TSemhandle): Boolean;
  begin
    with eventsem_z_typ(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        Inc(status);

        SysCtrlLeaveCritSec;

        Result:=true;
      end;
  end;

function SemWaitEvent(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
  var
    t0,t1:longint;
  begin
    with eventsem_z_typ(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        t0:=SysSysMsCount;


        repeat
          SysCtrlEnterCritSec;

          if status>0 then
            begin
              Dec(status);
              SysCtrlLeaveCritSec;
              Result:=true;
              Exit;
            end;

          SysCtrlLeaveCritSec;

          Dpmi32MultiThread;

          t1:=SysSysMsCount;

          if t1<t0 then Inc(t1,$1800b0);
        until (_TimeOut<>-1) and (t0+_TimeOut>=t1);
      end;

    Result:=false;
  end;

procedure SemCloseEvent(_Handle: TSemHandle);
  var
    i:longint;
  begin
    i:=Search_Used_Sem(_Handle);
    if i=0 then
      RunError(204);

    with eventsem_z_typ(_Handle)^ do
      begin
        if sig<>eventsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        with sem_array[i] do
          begin
            Dec(opencount);
            if opencount=0 then
              Free_Sem(i);
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

(*** Sem*Mutex *****************************************************)

function SemCreateMutex(_Name: PChar; _Shared, _State: Boolean): TSemHandle;
  var
    i:longint;
  begin
    Result:=-1;

    // allready exist ?
    if Search_MutexSem(_Name)<>0 then Exit;

    // get free entry
    i:=Search_Unused_Sem;
    if i=0 then Exit;

    with sem_array[i] do
      begin
        if _Name<>nil then
          StrCopy(sem_name,_Name);
        opencount:=1;
        used:=sem_type_mutex;
        mutex.sig:=mutexsem_sig;
        mutex.status:=Ord(_State);
        mutex.eigentuemer:=SysGetThreadId;
        Result:=Ofs(mutex);
      end;

  end;

function SemAccessMutex(_Name: PChar): TSemHandle;
  var
    i:longint;
  begin
    init_semaphore_table; // called by init_exe...

    Result:=-1;

    // allready exist ?
    i:=Search_MutexSem(_Name);
    if i=0 then Exit;


    with sem_array[i] do
      begin
        SysCtrlEnterCritSec;
        Inc(opencount);
        SysCtrlLeaveCritSec;
        Result:=Ofs(mutex);
      end;
  end;

function SemRequestMutex(_Handle: TSemHandle; _TimeOut: Longint): Boolean;
  var
    t0,t1:longint;
  begin
    with mutexsem_z_typ(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        t0:=SysSysMsCount;


        repeat
          SysCtrlEnterCritSec;

          if (status=0) or (eigentuemer=SysGetThreadId) then
            begin
              Inc(status);
              eigentuemer:=SysGetThreadId;
              SysCtrlLeaveCritSec;
              Result:=true;
              Exit;
            end;

          SysCtrlLeaveCritSec;

          Dpmi32MultiThread;

          t1:=SysSysMsCount;

          if t1<t0 then Inc(t1,$1800b0);
        until (_TimeOut<>-1) and (t0+_TimeOut>=t1);
      end;

    Result:=false;
  end;

function SemReleaseMutex(_Handle: TSemHandle): Boolean;
  begin
    with mutexsem_z_typ(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        if status=0 then
          Result:=false
        else
          begin
            Dec(status);        // release ..
            Result:=true;
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

procedure SemCloseMutex(_Handle: TSemHandle);
  var
    i:integer;
  begin
    i:=Search_Used_Sem(_Handle);
    if i=0 then
      RunError(204);

    with mutexsem_z_typ(_Handle)^ do
      begin
        if sig<>mutexsem_sig then
          RunError(204);

        SysCtrlEnterCritSec;

        with sem_array[i] do
          begin
            Dec(opencount);
            if opencount=0 then
              Free_Sem(i);
          end;

        SysCtrlLeaveCritSec;
      end;
  end;

function SysMemInfo(_Base: Pointer; _Size: Longint; var _Flags: Longint): Boolean;
  begin
    //NOT_IMPLEMENTED
    SysMemInfo:=false;
  end;

function SysSetMemProtection(_Base: Pointer; _Size: Longint; _Flags: Longint): Boolean;
  begin
    //NOT_IMPLEMENTED
    SysSetMemProtection:=false;
  end;

procedure SysMessageBox(_Msg, _Title: PChar; _Error: Boolean);
  begin
    SysDisplayConsoleError(true,_Title,_Msg);
  end;

function SysGetVolumeLabel(Drive: Char): ShortString;
  var
    sr                  :TOSSearchRec;
    root                :array[0..6] of char;
  const
    suchmaske=$0800  // must  volume
             +$0008  // allow volume
             +$0020; // allow archive
  begin
    root:='@:\*.*'#0;
    root[0]:=drive;

    if SysFindFirst(root,suchmaske,sr,false)=0 then
      begin
        SysGetVolumeLabel:=sr.name;
        if Pos('.',Result)>0 then
          Delete(Result,Pos('.',Result),1);
      end
    else
      SysGetVolumeLabel:='';
  end;

function SysSetVolumeLabel(Drive: Char; _Label: ShortString): Boolean;
  var
    OrgDir              :string;        // current directory of <Drive>
    OrgDrive            :string;        // current drive and diretory
    rc                  :longint;
    regs                :real_mode_call_structure_typ;

  type
    extended_fcb=
      packed record
        sign_ext        :byte; // $ff
        res01           :array[1..5] of byte;
        attr            :byte;
        drive_number    :byte; // 1=A
        filename_ext    :array[1..8+3] of char;
        rest            :array[1..25] of byte;
      end;


  begin
    SysSetVolumeLabel:=false;
    Drive:=UpCase(Drive);
    if Length(_Label)>8+3 then Exit;

    // 1. get old drive and directory
    GetDir(0                    ,OrgDrive);
    GetDir(Ord(Drive)-Ord('A')+1,OrgDir  );

    // 2. go to root
    ChDir(Drive+':\');

    // 3. delete old label
    FillChar(extended_fcb(Mem[segdossyslow32]),SizeOf(extended_fcb),0);
    with extended_fcb(Mem[segdossyslow32]) do
      begin
        sign_ext        :=$ff;
        attr            :=$08; // directory mask
        drive_number    :=Ord(Drive)-Ord('A')+1;
        filename_ext    :='????????'+'???';
      end;

    with regs do
      begin
        ah_:=$13;                       // DELETE FILE USING FCB
        ds_:=segdossyslow16;            // DS:DX
        dx_:=0;
      end;
    intr_realmode(regs,$21);


    // 4. create new label
    FillChar(extended_fcb(Mem[segdossyslow32]),SizeOf(extended_fcb),0);
    with extended_fcb(Mem[segdossyslow32]) do
      begin
        sign_ext        :=$ff;
        attr            :=$08; // directory bit
        drive_number    :=Ord(Drive)-Ord('A')+1;
        filename_ext    :='        '+'   ';
        Move(_Label[1],filename_ext,Length(_Label));
      end;

    with regs do
      begin
        ah_:=$16;                       // CREATE OR TRUNCATE FILE USING FCB
        ds_:=segdossyslow16;            // DS:DX
        dx_:=0;
        intr_realmode(regs,$21);
        SysSetVolumeLabel:=(al_=0) or (_Label='')
      end;


    // 5. restore directory and drive
    ChDir(OrgDir  );
    ChDir(OrgDrive);

  end;

function SysGetForegroundProcessId: Longint;
  begin
    //NOT_IMPLEMENTED
    SysGetForegroundProcessId:=-1;
  end;

function SysGetBootDrive: Char;(*$FRAME-*)(*$USES EDX*)
  asm
    mov eax,$3305
    mov dl,1
    int $21
    add dl,'A'-1
    movzx eax,dl
  end;


// TDriveType = ( dtFloppy, dtHDFAT, dtHDHPFS, dtInvalid,
//               dtNovellNet, dtCDRom, dtLAN, dtHDNTFS, dtUnknown,
//               dtTVFS, dtHDExt2 );

function SysGetDriveType(Drive: Char): TDriveType;assembler;
(*$FRAME-*)(*$USES ECX,ESI*)
  var
    regs                :real_mode_call_structure_typ;
  asm
    push longint [Drive]
    call _upcase
    sub al,'A'
    movzx esi,al        // esi=drive0 ( // 0=A: ... )

    // ******************************* Valid Drive ?
    call SysGetValidDrives
    mov ecx,esi         // drive0
    shr eax,cl
    and al,1            // Bit 0
    cmp al,1
    je @gueltig

    mov eax,dtInvalid
    jmp @ret

  @gueltig:
    // ******************************* CD-ROM ?
    // INT $2F,AX=$150b
    mov [regs.ax_],$150b
    mov [regs.bx_],$eeee
    mov [regs.ecx_],esi // drive0

    lea eax,regs
    push eax
    push $2f
    call intr_realmode

    cmp [regs.bx_],$eeee
    je @kein_cdrom

    cmp [regs.ax_],0    // 0=unsupported
    je @kein_cdrom

    mov eax,dtCDRom
    jmp @ret

  @kein_cdrom:
    // ******************************* NOVELL NETWARE
    // INT $21,AX=$EF01 .. Novell NetWare - WORKSTATION - GET DRIVE FLAG TABLE
    mov [regs.ax_],$ef01

    lea eax,regs
    push eax
    push $21
    call intr_realmode

    cmp [regs.ax_],0
    jne @kein_novellnet

    // ES:SI -> network shell's 32-byte drive flag table
    movzx eax,[regs.ds_]
    shl eax,4           // dosseg_linear
    movzx ecx,[regs.si_]
    add ecx,eax
    add ecx,esi         // drive0
    cmp byte [ecx],$00  // drive is not mapped
    je @kein_novellnet
    cmp byte [ecx],$80  // mapped to local drive
    je @kein_novellnet

    mov eax,dtNovellNet
    jmp @ret

  @kein_novellnet:
    // ******************************* GENERIC NETWORK
    // INT $21,AX=$4409
    mov [regs.ax_],$4409 // CHECK IF BLOCK DEVICE REMOTE
    mov eax,esi         // drive0
    inc al
    mov [regs.bl_],al   // drive: A:=1 ..
    and [regs.edx_],0

    lea eax,regs
    push eax
    push $21
    call intr_realmode
    mov eax,[regs.edx_]
    test eax,(1 shl 12) // Bit 12 =1  -> removable
    jz @kein_netzwerk

    mov eax,dtLAN
    jmp @ret

  @kein_netzwerk:
    cmp esi,1           // drive0  A:(0) / B:(1)
    ja @kein_floppy

    mov eax,dtFloppy
    jmp @ret

  @kein_floppy:
    // FAT ? HPFS ? ..
    // mov eax,dtUnknown
    mov eax,dtHDFAT

  @ret:
  end;

function SysGetSystemSettings: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov ah,$09 // GET KEYBOARD FUNCTIONALITY
    int $16    // al Bit 5
    shr al,5
    and eax,1
  end;

const
  CrtScanCode: Byte = 0;

function SysKeyPressed: Boolean;(*$FRAME-*)(*$USES EDX*)
  asm
    call teste_strg_c
    (*$IFDEF THREAD_SUPPORT*)
    call Dpmi32MultiThread
    (*$ENDIF*)

    mov dl,true
    cmp CrtScanCode,0
    jne @ret

    mov ah,$11
    int $16
    jnz @ret

    mov dl,false

  @ret:
    mov al,dl
  end;

procedure SysFlushKeyBuf;(*$FRAME-*)(*$USES EAX*)
  asm
  @schleife:

    call teste_strg_c
    call SysKeyPressed
    cmp al,false
    je @ret

    call SysReadKey
    jmp @schleife

  @ret:
    (*$IFDEF THREAD_SUPPORT*)
    call Dpmi32MultiThread
    (*$ENDIF*)
  end;

function SysReadKey: Char;
  var
    scan                :byte;
    ascii               :char;
  begin
    teste_strg_c;

    If CrtScanCode <> 0 then
      begin
        SysReadKey:=Chr(CrtScanCode);
        CrtScanCode:=0;
      end
    else
      begin
        (*$IFDEF THREAD_SUPPORT*)
        if IsMultiThread then
          while not SysKeyPressed do
            Dpmi32MultiThread;
        (*$ENDIF*)

        asm (*$Alters EAX*)
        @nochmal:
          mov ah,$10
          int $16
          mov scan,ah
          mov ascii,al
          cmp ax,$2e03 // ^C
          jne @kein_strg_c

          mov strg_c_gefunden,true
          call teste_strg_c
          jmp @nochmal

        @kein_strg_c:
        end;

        case ascii of
          #$00:CrtScanCode:=scan;
          #$E0:           {   Up, Dn, Left Rt Ins Del Home End PgUp PgDn C-Home C-End C-PgUp C-PgDn C-Left C-Right C-Up C-Dn }
            if scan       in [$48,$50,$4B,$4D,$52,$53,$47, $4F,$49, $51, $77,   $75,  $84,   $76,   $73,   $74,    $8D, $91] then
            begin
              CrtScanCode:=scan;
              ascii:=#0;
            end;
        end;
        SysReadKey:=ascii;
      end;
    end;

// VPUTILS.PeekKey
function SysPeekKey(Var Ch:Char):boolean;
  var
    scan                :byte;
    ascii               :char;
    taste_vorhanden     :boolean;
  begin
    teste_strg_c;
    (*$IFDEF THREAD_SUPPORT*)
    Dpmi32MultiThread;
    (*$ENDIF*)

    // 2*readkey for "F1"
    if CrtScanCode<>0 then
      begin
        Ch:=chr(CrtScanCode);
        SysPeekKey:=true;
        Exit;
      end;

    asm (*$Alters EAX*)
      mov taste_vorhanden,false
      mov ah,$11
      int $16
      jz @unveraendert

      mov scan,ah
      mov ascii,al
      mov taste_vorhanden,true

    @unveraendert:
    end;

    if not taste_vorhanden then
      begin
        SysPeekKey:=false;
        Exit;
      end;

    case ascii of
      #$E0:           {   Up, Dn, Left Rt Ins Del Home End PgUp PgDn C-Home C-End C-PgUp C-PgDn C-Left C-Right C-Up C-Dn }
        if scan       in [$48,$50,$4B,$4D,$52,$53,$47, $4F,$49, $51, $77,   $75,  $84,   $76,   $73,   $74,    $8D, $91] then
          ascii:=#0;
    end;

    Ch:=ascii;
    SysPeekKey:=true;

  end;

const
  video_seite_0=0;

procedure SysGetCurPos(var X, Y: SmallWord);
  (*$FRAME-*)(*$USES EAX,ESI*)
  asm
    movzx eax,byte [Seg0040+$50]
    mov esi,[x]
    mov [esi],ax
    movzx eax,byte [Seg0040+$51]
    mov esi,[y]
    mov [esi],ax
  end;

procedure SysSetCurPos(X,Y: SmallWord);(*$FRAME-*)(*$USES EAX,EBX,EDX*)
  asm
    mov ah,$02
    mov bh,video_seite_0
    mov dh,byte [x]
    mov dl,byte [y]
    int $10
  end;

procedure SysWrtCharStrAtt(CharStr: Pointer; Len,X,Y: SmallWord; var Attr: Byte);(*$FRAME-*)(*$USES ALL*)
  asm
    call teste_strg_c

    movzx ecx,[len]
    jecxz @ret

    push [x+0].longint
      push [y+4].longint
        call berechnebildschirmspeicherposition
    mov edi,eax
    mov esi,[attr]
    mov ah,[esi]
    mov esi,[charstr]
    cld

    @l1:

    lodsb
    stosw
    loop @l1

    @ret:

  end;

procedure SysScrollUp(X1,Y1,X2,Y2,Lines,Cell: SmallWord);(*$FRAME-*)(*$USES ALL*)
  asm
    movzx eax,[Lines]
    // nichts zu tun ?
    or eax,eax
    jz @ret
    // 32 Bit Lines
    mov [Lines].longint,eax

    // Anzahl Byte pro Bildschimzeile im Speicher
    call SysGetTextVideoColumns
    shl eax,1 // *2
    mov ebp,eax

    // Zielzeile
    push [X1].longint
      push [Y1+4].longint
        call berechnebildschirmspeicherposition
    mov edi,eax

    // Anzahl 16 Bit Worte pro Zeile
    mov eax,[X2].longint
    sub eax,[X1].longint
    inc eax
    movzx ecx,ax

    // Anzahl Zeilen des Fensters
    mov eax,[Y2].longint
    sub eax,[Y1].longint
    inc eax
    movzx ebx,ax

    // mehr als vorhandene Zeilen rollen ?
    mov eax,[Lines].longint
    cmp ebx,eax
    jae @keine_korrektur
    mov eax,ebx
    mov [Lines].longint,eax

  @keine_korrektur:

    // Leseposition
    // eax=[Lines]
    mul ebp
    lea esi,[edi+eax]

    // Rollen notwendig ?
    mov eax,ebx
    sub eax,[Lines].longint     // eax:=Anzahl Zeilen zu verschieben ( wenn <0 dann 0)



    // eax = Anzahl zu verschiebende Zeilen
    // ecx = Anzahl zu verschiebende SmallWord pro Zeile
    // esi/edi = Quelle/Ziel der Verschiebung
    // ebx = Anzahl Zeilen des Bildschirmfensters

    // ebp = Anzahl Byte bis zum nÑchsten Anfang im Fenster
    sub ebp,ecx
    sub ebp,ecx

    cld

  @verschiebe_schleife:
    cmp eax,0           // <=0 ?
    jle @fuellen

    push ecx
      push ecx
        shr ecx,1
        rep movsd
      pop ecx
      and ecx,1
      rep movsw
    pop ecx
    add esi,ebp
    add edi,ebp
    dec eax             // Anzahl Zeilen zu verschieben
    dec ebx             // Anzahl Zeilen Åbrig zu fÅllen
    jmp @verschiebe_schleife

  @fuellen:
    mov ax,[Cell]
    shl eax,16
    mov ax,[Cell]

  @fuellen_schleife:
    cmp ebx,0
    jle @fuellen_schleife_ende

    push ecx
      push ecx
        shr ecx,1
        rep stosd
      pop ecx
      and ecx,1
      rep stosw
    pop ecx
    add edi,ebp
    dec ebx
    jmp @fuellen_schleife

  @fuellen_schleife_ende:
  @ret:
end;

procedure SysScrollDn(X1,Y1,X2,Y2,Lines,Cell: SmallWord);(*$FRAME-*)(*$USES ALL*)
  asm
    movzx eax,[Lines]
    // nichts zu tun ?
    or eax,eax
    jz @ret
    // 32 Bit Lines
    mov [Lines].longint,eax

    // Anzahl Byte pro Bildschimzeile im Speicher
    call SysGetTextVideoColumns
    shl eax,1 // *2
    mov ebp,eax

    // Zielzeile
    push [X1].longint
      push [Y2+4].longint
        call berechnebildschirmspeicherposition
    mov edi,eax

    // Anzahl 16 Bit Worte pro Zeile
    mov eax,[X2].longint
    sub eax,[X1].longint
    inc eax
    movzx ecx,ax

    // Anzahl Zeilen des Fensters
    mov eax,[Y2].longint
    sub eax,[Y1].longint
    inc eax
    movzx ebx,ax

    // mehr als vorhandene Zeilen rollen ?
    mov eax,[Lines].longint
    cmp ebx,eax
    jae @keine_korrektur
    mov eax,ebx
    mov [Lines].longint,eax

  @keine_korrektur:

    // Leseposition
    // eax=[Lines]
    mul ebp
    mov esi,edi
    sub esi,eax

    // Rollen notwendig ?
    mov eax,ebx
    sub eax,[Lines].longint     // eax:=Anzahl Zeilen zu verschieben ( wenn <0 dann 0)



    // eax = Anzahl zu verschiebende Zeilen
    // ecx = Anzahl zu verschiebende SmallWord pro Zeile
    // esi/edi = Quelle/Ziel der Verschiebung
    // ebx = Anzahl Zeilen des Bildschirmfensters

    // ebp = Anzahl Byte bis zum nÑchsten Anfang im Fenster
    add ebp,ecx
    add ebp,ecx

    cld

  @verschiebe_schleife:
    cmp eax,0           // <=0 ?
    jle @fuellen

    push ecx
      push ecx
        shr ecx,1
        rep movsd
      pop ecx
      and ecx,1
      rep movsw
    pop ecx
    sub esi,ebp
    sub edi,ebp
    dec eax             // Anzahl Zeilen zu verschieben
    dec ebx             // Anzahl Zeilen Åbrig zu fÅllen
    jmp @verschiebe_schleife

  @fuellen:
    mov ax,[Cell]
    shl eax,16
    mov ax,[Cell]

  @fuellen_schleife:
    cmp ebx,0
    jle @fuellen_schleife_ende

    push ecx
      push ecx
        shr ecx,1
        rep stosd
      pop ecx
      and ecx,1
      rep stosw
    pop ecx
    sub edi,ebp
    dec ebx
    jmp @fuellen_schleife

  @fuellen_schleife_ende:
  @ret:

  end;


type
  TCharCaseTable = array[0..255] of Char;

var
  UpperCaseTable: TCharCaseTable;
  LowerCaseTable: TCharCaseTable;
  AnsiUpperCaseTable: TCharCaseTable;
  AnsiLowerCaseTable: TCharCaseTable;
  WeightTable: TCharCaseTable;
const
  CaseTablesInitialized: Boolean = False;

procedure InitCaseTables;
var
  I,J: Integer;
begin
  for I := 0 to 255 do
  begin
    UpperCaseTable[I] := Chr(I);
    LowerCaseTable[I] := Chr(I);
    AnsiUpperCaseTable[I] := Chr(I);
    AnsiLowerCaseTable[I] := Chr(I);
    if I in [Ord('A')..Ord('Z')] then
      LowerCaseTable[I] := Chr(I + (Ord('a')-Ord('A')));
    if I in [Ord('a')..Ord('z')] then
      UpperCaseTable[I] := Chr(I - (Ord('a')-Ord('A')));
  end;
  SysGetCaseMap(SizeOf(AnsiUpperCaseTable), AnsiUpperCaseTable);
  for I := 255 downto 0 do
  begin
    J := Ord(AnsiUpperCaseTable[I]);
    if (J <> I) {and (AnsiLowerCaseTable[J] <> chr(J))} then
      AnsiLowerCaseTable[J] := Chr(I);
  end;
  SysGetWeightTable(SizeOf(WeightTable), WeightTable);
  CaseTablesInitialized := True;
end;

procedure ConvertCase(S1,S2: PChar; Count: Integer; var Table: TCharCaseTable); {&USES esi,edi} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@1
                Call    InitCaseTables
              @@1:
                xor     eax,eax
                mov     esi,S1
                mov     edi,S2
                mov     ecx,Count
                mov     edx,Table
                jecxz   @@3
              @@2:
                dec     ecx
                mov     al,[esi+ecx]
                mov     al,[edx+eax]
                mov     [edi+ecx],al
                jnz     @@2
              @@3:
end;

procedure SysChangeCase(Source, Dest: PChar; Len: Longint; NewCase: TCharCase);
begin
  case NewCase of
    ccLower:     ConvertCase(Source, Dest, Len, LowerCaseTable);
    ccUpper:     ConvertCase(Source, Dest, Len, UpperCaseTable);
    ccAnsiLower: ConvertCase(Source, Dest, Len, AnsiLowerCaseTable);
    ccAnsiUpper: ConvertCase(Source, Dest, Len, AnsiUpperCaseTable);
  end;
end;

function SysLowerCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiLowerCaseTable);
  Result := s;
end;

function SysUpperCase(s: PChar): PChar;
begin
  ConvertCase(s, s, strlen(s), AnsiUpperCaseTable);
  Result := s;
end;

function MemComp(P1,P2: Pointer; L1,L2: Integer; T1,T2: PChar): Integer; {&USES ebx,esi,edi,ebp} {&FRAME-}
asm
                cmp     CaseTablesInitialized,0
                jne     @@0
                Call    InitCaseTables
              @@0:
                mov     ecx,L1
                mov     eax,L2
                mov     esi,P1
                mov     edi,P2
                cmp     ecx,eax
                jbe     @@1
                mov     ecx,eax
              @@1:
                mov     ebx,T1
                mov     ebp,T2
                xor     eax,eax
                xor     edx,edx
                test    ecx,ecx
                jz      @@5
              @@2:
                mov     al,[esi]
                mov     dl,[edi]
                inc     esi
                inc     edi
                test    ebp,ebp
                mov     al,[ebx+eax]    // Table1
                mov     dl,[ebx+edx]
                jz      @@3
                mov     al,[ebp+eax]    // Table2
                mov     dl,[ebp+edx]
              @@3:
                cmp     al,dl
                jne     @@RET
                dec     ecx
                jnz     @@2
              @@5:
                mov     eax,L1
                mov     edx,L2
              @@RET:
                sub     eax,edx
end;

function SysCompareStrings(s1, s2: PChar; l1, l2: Longint; IgnoreCase: Boolean): Longint;
begin
  if IgnoreCase then
    Result := MemComp(s1, s2, l1, l2, PChar(@WeightTable), nil)
  else
    Result := MemComp(s1, s2, l1, l2, PChar(@AnsiUpperCaseTable), PChar(@WeightTable));
end;

procedure SysGetCaseMap(TblLen: Longint; Tbl: PChar );assembler;
(*$FRAME-*)(*$USES EAX,ECX,ESI,EDI*)
  var
    regs:real_mode_call_structure_typ;
  asm
    // DOS 3.3+
    mov [regs.ax_],$6502        // get pointer to uppercase table
    or eax,-1                   // ax:=$ffff
    mov [regs.ebx_],eax         // code page (FFFFh=global code page)
    mov [regs.edx_],eax         // country ID (FFFFh=current country)
    mov ax,segdossyslow16
    mov [regs.es_],ax
    and [regs.edi_],0           // -> country information buffer
    mov [regs.cx_],1+4          // size of buffer

    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,[segdossyslow32]

    //  00  byte        info ID
    //  01  DWORD       pointer to uppercase table

    //  00  word        table size ($80)
    //  02  128 byte    upper case equivalents

    movzx eax,smallword [edi+1+2]
    // 16:16->32 Bit
    shl eax,4
    movzx esi,smallword [edi+1+0]
    lea esi,[eax+esi+2]

    // Tbl bearbeiten ..
    mov ecx,TblLen
    mov edi,Tbl
    sub eax,eax
    sub esi,128
    jecxz @ret

    cld

  @schleife:

    mov al,[edi]
    cmp al,$80
    jb @ascii

    mov al,[esi+eax]
    mov [edi],al
    jmp @weiter

  @ascii:
    cmp al,'a'
    jb @weiter

    cmp al,'z'
    ja @weiter

    sub al,'a'-'A'

  @weiter:
    stosb
    loop @schleife

  @ret:
  end;

procedure SysGetWeightTable(TblLen: Longint; WeightTable: PChar);assembler;
(*$FRAME-*)(*$USES EAX,ECX,ESI,EDI*)
  var
    regs:real_mode_call_structure_typ;
  asm
    // DOS 3.3+
    mov [regs.ax_],$6506        // get pointer to collating sequence table
    or eax,-1                   // ax:=$ffff
    mov [regs.ebx_],eax         // code page (FFFFh=global code page)
    mov [regs.edx_],eax         // country ID (FFFFh=current country)
    mov ax,segdossyslow16
    mov [regs.es_],ax
    and [regs.edi_],0           // -> country information buffer
    mov [regs.cx_],1+4          // size of buffer

    lea eax,regs
    push eax
    push $21
    call intr_realmode

    mov edi,[segdossyslow32]
    movzx eax,smallword [edi+1+2]  //   01h    DWORD   pointer to collating table

    // 16:16->32 Bit
    shl eax,4
    // kopieren
    movzx esi,smallword [edi+1+0]
    lea esi,[eax+esi+2]
    mov edi,WeightTable
    mov ecx,TblLen // 256 !
    cld
    rep movsb
  end;

// copy from VpSysOS2.Pas
var
  PrevXcptProc: Pointer=nil;

function SignalHandler(Report:       PExceptionReportRecord;
                       Registration: PExceptionRegistrationRecord;
                       Context:      PContextRecord;
                       P:            Pointer): Longint; stdcall;
begin
  Result := xcpt_Continue_Search;
  if Report^.ExceptionNum = xcpt_Ctrl_Break then
    if Assigned(CtrlBreakHandler) and CtrlBreakHandler then
      Result := xcpt_Continue_Execution;
  XcptProc := PrevXcptProc;
end;

procedure SysCtrlSetCBreakHandler;
begin
//  if not Assigned(XcptProc) then
//    begin
//      PrevXcptProc:=XcptProc;
//      XcptProc := @SignalHandler;
//    end
//  else
//    RunError(201); // can only have one CBreakHandler
  PrevXcptProc:=nil;
  XcptProc := @SignalHandler;
end;


const
  big_jft16             :smallword=0;
  big_jft32             :longint  =0;

function SysFileIncHandleCount(Count: Longint): Longint;
  const
    count_255=255;
  begin
    Result:=0;

    //   PSP:$0018=Default JFT [20]
    //   DOS 3.X:
    //   PSP:$0032=SIZE(JFT)
    //   PSP:$0034=ADDR(JFT)

    if  (big_jft16=0)                   // not allocated
    and (MemW[seg_psp+$34]=$0018)       // not moved
     then
      begin
        if GetDosMem(big_jft16,count_255)=0 then
          begin
            big_jft32:=DosSeg_Linear(big_jft16);
            FillChar(Mem[big_jft32],count_255,$ff);

            // copy old table
            Move(Mem[DosSeg_Linear(MemW[seg_psp+$36])+MemW[seg_psp+$34]],Mem[big_jft32],20);

            // install new table
            MemW[seg_psp+$34]:=0;
            MemW[seg_psp+$36]:=big_jft16;
            MemW[seg_psp+$32]:=count_255;
          end
        else
          Result:=8;            // no memory
      end
    else
      Result:=1;                // 2. Call
  end;

function SysGetCodePage: Longint;assembler;(*$FRAME-*)(*$USES EBX,EDX*)
  asm
    sub ebx,ebx

    mov eax,$6601               // DOS 3.3+ - GET GLOBAL CODE PAGE TABLE
    int $21

    mov eax,ebx
  end;

function SysReadAttributesAt(x,y: SmallWord): Byte;(*$FRAME-*)(*$USES EBX*)
  asm
    movzx eax,x
    movzx ebx,y
    push eax
    push ebx
    call berechnebildschirmspeicherposition
    mov al,[eax+1]
  end;

function SysReadCharAt(x,y: SmallWord): Char;(*$FRAME-*)(*$USES EBX*)
  asm
    movzx eax,x
    movzx ebx,y
    push eax
    push ebx
    call berechnebildschirmspeicherposition
    mov al,[eax]
  end;


procedure SysSound(freq:longint);(*$FRAME-*)(*$USES EAX,EDX*)
  asm
    mov eax,$001234dd // $ffff * 18.2 -> 1.19318 Mhz
    sub edx,edx
    cmp [freq],edx
    jne @nicht_null

    inc [freq]

  @nicht_null:
    div [freq]

    push eax

      in al,$61
      test al,$03
      jnz @schon_an

      or al,$03
      out $61,al

  @schon_an:

      mov al,$b6
      out $43,al // Timer

    pop eax

    out $42,al
    mov al,ah
    out $42,al

  end;

procedure SysNoSound;(*$FRAME-*)(*$USES EAX*)
  asm
    in al,$61
    and al,$fc
    out $61,al
  end;

procedure delay_loop;(*$FRAME-*)(*$USES NONE*) // EAX,EDI,EBX
  asm
  @schleife:

    out $ed,al // Zeit verbrauchen
    dec eax
    jz @ende

    cmp ebx,[edi]
    je @schleife

    @ende:
  end;

const // initialised because can be called before SysLowInit
  tausendstel_durchleaufe:longint=1;

procedure SysCtrlSleep(Delay: Integer);(*$FRAME-*)(*$USES ALL*)
  asm
    (*$IFDEF THREAD_SUPPORT*)
    cmp IsMultiThread,true
    jne @singe_thread_code

    mov eax,[delay]
    cdq
    mov ebx,55                          // 1/1000 s = 55 * 1/18 s
    div ebx
    mov esi,Seg0040+$006c               // MEML[$40:$6c]= 1/18 timer tick
    add eax,[esi]

  @noch_warten:
    call Dpmi32MultiThread

    mov edx,[esi]
    sub edx,eax

    jl @noch_warten

    jmp @ret

  @singe_thread_code:
    (*$ENDIF*)
    (*
    mov ah,$86
    mov ecx,[delay]
    shl ecx,10                          //milli->mikro
    mov edx,ecx
    shr ecx,16
    stc
    int $15
    jmp @ret*)

    mov edi,Seg0040                     // [edi] Ñndert sich nicht
    mov ebx,[edi]
    mov ecx,[delay]                     // Millisekunden
    jecxz @ret

  @schleife:
    cmp strg_c_gefunden,true
    je @ret

    mov eax,tausendstel_durchleaufe
    call delay_loop
    loop @schleife

   @ret:
  end;

procedure calibrate_delayloop;(*$FRAME-*)(*$USES ALL*)
  asm
    mov eax,1
    mov edi,Seg0040+$6c
    mov edx,[edi]
    // AnwÑrmen
    call delay_loop

  @warte_auf_wechsel:
    mov ebx,[edi]
    cmp ebx,edx
    je @warte_auf_wechsel

    sub eax,eax
    call delay_loop
    neg eax
    sub edx,edx
    mov ecx,55 // 1000 / 18.2
  //        56 // 1024 / 18.2
    div ecx
    or eax,eax
    jnz @g0
    inc eax
  @g0:
    mov tausendstel_durchleaufe,eax
  end;


procedure SysBeepEx(Freq,Dur: LongInt);(*$FRAME-*)(*$USES NONE*)
  asm
    push Freq
    call SysSound
    push Dur
    call SysCtrlSleep
    call SysNoSound
    call teste_strg_c
  end;

function SysGetVideoModeInfo( Var Cols, Rows, Colours : Word ): Boolean;(*$FRAME-*)(*$USES ESI,EDI*)
  asm
    call SysGetTextVideoColumns // Anzahl Bildschirmspalten
    mov edi,cols
    mov [edi],eax

    call SysGetTextVideoRows    // Anzahl Zeilen
    mov edi,rows
    mov [edi],eax

    mov edi,colours
    mov [edi].longint,16        // Co80 -> 16 colours
                                // Bw80 -> 2 colours but blink/underline/inverse
    mov al,true
  end;


function SysGetVisibleLines( var Top, Bottom: Longint ): Boolean;(*$FRAME-*)(*$USES EDI*)
  asm
    mov edi,Top
    sub eax,eax;inc eax         // 1
    mov [edi],eax
    mov edi,Bottom
    call SysGetTextVideoRows    // Anzahl Zeilen
    mov [edi],eax
    mov al,true
  end;


function SysSetVideoMode(Cols, Rows: Word): Boolean;

 procedure setvesa(nummer:smallword);
   var
     erfolg:boolean;
   begin
     asm (*&Alters EAX,EBX*)
       mov erfolg,false
       mov ax,$4f02
       mov bx,nummer
       int $10
       cmp ah,$00 // erfolgreich
       jne @1
       mov erfolg,true
       @1:
     end;
     SysSetVideoMode:=erfolg;
   end;

 procedure setze_modus(nummer,font:byte);
   begin
     SysSetVideoMode:=true;
     asm (*$Alters EAX*)
       mov ah,$00
       mov al,nummer
       int $10
     end;

     case font of
       0:;
       8:
         asm (*$Alters EAX,EBX*)
           mov ax,$1112
           mov bl,0
           int $10
         end;
      14:
         asm (*$Alters EAX,EBX*)
           mov ax,$1111
           mov bl,0
           int $10
         end;
(*      16:
         asm ( *$Alters EAX,EBX * )
           mov ax,$1114
           mov bl,0
           int $10
         end;     *)
     else
       SysSetVideoMode:=false;
     end;
   end;
 begin
   SysSetVideoMode:=false;

   if cols=40 then
     case rows of
       25:
         case video_adapter_found of
           cga_found,
           ega_found,
           vga_found:setze_modus($01,0);
         end;
       28:
         if video_adapter_found=vga_found then
           setze_modus($01,14); // 400 div 28
       43:
         if video_adapter_found=ega_found then
           setze_modus($01, 8); // 350 div 43
       50:
         if video_adapter_found=vga_found then
           setze_modus($01, 8); // 400 div 50
     end;

   if cols=80 then
     case rows of
       25:
         case video_adapter_found of
           mda_found:setze_modus($07,0);
           cga_found,
           ega_found,
           vga_found:setze_modus($03,0);
         end;
       28:
         if video_adapter_found=vga_found then
           setze_modus($03,14); // 400 div 28
       43:
         if video_adapter_found=ega_found then
           setze_modus($03, 8); // 350 div 43
       50:
         if video_adapter_found=vga_found then
           setze_modus($03, 8); // 400 div 50
       60:
         setvesa($108);
     end;

   if cols=132 then
     case rows of
       25:setvesa($109);
       43:setvesa($10a);
       50:setvesa($10b);
       60:setvesa($10c);
     end;

 end;

procedure detect_mdacgaegavga;(*$FRAME-*)(*$USES ALL*)
  asm
    sub edx,edx                         // mda_found
    cmp byte [Seg0040+$49],7            // 7=MDA/Hercules
    je @ret

    inc edx                             // cga_found
    mov ah,$12
    mov bl,$10
    int $10
    cmp bl,$10
    je @ret

    inc edx                             // ega_found
    mov ax,$1a00
    int $10
    cmp al,$1a
    jne @ret

    cmp bl,7 // VGA mono
    jl @ret

    cmp bl,8 // VGA colour
    ja @ret

    inc edx                             // vga_found

    @ret:
    (*$IFOPT Z+*)
    mov video_adapter_found,edx
    (*$ELSE*)
    mov video_adapter_found, dl
    (*$ENDIF*)
  end;

function SysTVDetectMouse: Longint;(*$FRAME-*)(*$USES EBX*)
  asm
    mov ax,$0021 // Software Reset
    sub ebx,ebx
    int $33
    cmp bx,$ffff // $ffff -> 2
    jne @1

    mov ebx,2

    @1:
    mov eax,ebx // SysTVDetectMouse:=eax
  end;


var
  maus_rmcbs            :real_mode_call_structure_typ;
  maus_rm_ptr           :real_mode_ptr_typ;

const
  // not implemented as an qeue: only one element
  maus_event_vorhanden  :boolean=false;

var
  maus_event            :TSysMouseEvent;

procedure maus_eventhandler;(*$FRAME-*)(*$USES NONE*)
  asm
    // DS:ESI = DOS SS:SP
    // ES:EDI = maus_rmcbs

    push eax
      push ds

        // WDOSX 2000.07.13
        mov eax,ds
        lsl eax,eax
        cmp eax,esi
        ja @ntBugDone

        movzx esi,si
        movzx edi,di

      @ntBugDone:

        mov ds,cs:[seldata]

        // Bearbeiten
        cmp maus_event_vorhanden,false
        jne @1

        mov maus_event_vorhanden,true

        call SysSysMsCount
        mov [maus_event.smeTime   ],eax

        // Y
        mov eax,es:[edi+real_mode_call_structure_typ.edx_]
        shl eax,16
        // X
        mov  ax,es:[edi+real_mode_call_structure_typ.cx_]
        // y/8, x/8
        shr eax,3
        mov [maus_event.smePos],eax

        // BUTTONS
        mov al,es:[edi+real_mode_call_structure_typ.bl_]
        mov [maus_event.smeButtons],al

      @1:

      pop ds
    pop eax

    // RETF simulieren
    cld
    lodsw  // RETF:IP
    mov es:[edi+real_mode_call_structure_typ.ip_],ax
    lodsw  // RETF:CS
    mov es:[edi+real_mode_call_structure_typ.cs_],ax

    add es:[edi+real_mode_call_structure_typ.sp_],2*2

    // REAL MODE
    iretd
  end;

procedure SysTVInitMouse(var X,Y: Integer);assembler;
(*$FRAME-*)(*$USES ALL*)
  var
    regs                :real_mode_call_structure_typ;
  asm
    mov maus_event_vorhanden,false

    // mouse_goto(0,0)
    mov ax,$0004
    sub ecx,ecx
    sub edx,edx
    int $33

    // maus_rm_ptr initialisieren
    and maus_rm_ptr,0

    mov eax,$0303 // ALLOCATE REAL MODE CALLBACK ADDRESS
    mov esi,offset maus_eventhandler // DS:ESI
    mov edi,offset maus_rmcbs        // ES:EDI
    push ds
      push cs
      pop ds
      int $31
    pop ds
    jc @dpmi_err

    // -> CX:DX
    mov [maus_rm_ptr.ofs_],dx
    mov [maus_rm_ptr.seg_],cx

    mov [regs.ax_],$000c        // SET ALTERNATE MOUSE USER HANDLER
    mov [regs.es_],cx
    mov [regs.edx_],edx
    mov [regs.cx_],mausmaske    // call mask .. Bit 0..5

    lea eax,regs
    push eax
    push $33
    call intr_realmode

    @dpmi_err:

    // SysTVUpdateMouseWhere
    sub ecx,ecx // x
    sub edx,edx // y
    mov ax,$0003
    int $33
    mov edi,x
    shr ecx,3
    mov [edi],ecx
    mov edi,y
    shr edx,3
    mov [edi],edx
  end;

procedure SysTVDoneMouse(Close: Boolean);assembler;
(*$FRAME-*)(*$USES ALL*)
  var
    regs                :real_mode_call_structure_typ;
  asm
    cmp maus_rm_ptr,0
    je @ohne

    mov [regs.ax_],$0021        // Software Reset

    lea eax,regs
    push eax
    push $33                    // Maus
    call intr_realmode

    mov dx,[maus_rm_ptr.ofs_]
    mov cx,[maus_rm_ptr.seg_]
    mov eax,$0304 // FREE REAL MODE CALLBACK ADDRESS CX:DX
    int $31

    and maus_rm_ptr,0

    @ohne:
  end;

procedure SysTVShowMouse;(*$FRAME-*)(*$USES EAX*)
  asm
    sub eax,eax    // mov ax,$0001
    inc eax
    int $33
  end;

procedure SysTVHideMouse;(*$FRAME-*)(*$USES EAX*)
  asm
    mov ax,$0002
    int $33
  end;

procedure SysTVUpdateMouseWhere(var X,Y: Integer);(*$FRAME-*)(*$USES ALL*)
  asm
    sub ecx,ecx // x
    sub edx,edx // y
    mov ax,$0003
    int $33
    mov edi,x
    shr ecx,3
    mov [edi],ecx
    mov edi,y
    shr edx,3
    mov [edi],edx
  end;

function SysTVGetMouseEvent(var Event: TSysMouseEvent): Boolean;(*$FRAME-*)(*$USES ESI,EDI*)
  asm
    mov al,false
    cmp maus_event_vorhanden,false
    je @ret

    mov maus_event_vorhanden,false

    mov esi,offset maus_event
    mov edi,Event
    cld
    movsd // TSysMouseEvent.smeTime
    movsd // TSysMouseEvent.smePos
    movsb // TSysMouseEvent.smeButtons
    mov al,true

    @ret:
  end;

procedure SysTVKbdInit;(*$FRAME-*)(*$USES NONE*)
  asm
  end;

function SysTVGetKeyEvent(var Event: TSysKeyEvent): Boolean;(*$FRAME-*)(*$USES EDI*)
  asm
    mov dl,false

    mov edi,Event
    sub eax,eax
    mov [edi+TSysKeyEvent.skeKeyCode],ax

    call SysTVGetShiftState
    mov [edi+TSysKeyEvent.skeShiftState],al

    mov ah,$11
    int $16
    jz @unveraendert

    mov ah,$10
    int $16
    mov [edi+TSysKeyEvent.skeKeyCode],ax
    mov dl,true

    @unveraendert:
    mov al,dl
  end;

function SysTVPeekKeyEvent(var Event: TSysKeyEvent): Boolean;(*$FRAME-*)(*$USES EDX,EDI*)
  asm
    mov dl,false
    mov edi,Event
    sub eax,eax
    mov [edi+TSysKeyEvent.skeKeyCode],ax

    call SysTVGetShiftState
    mov [edi+TSysKeyEvent.skeShiftState],al

    mov ah,$11
    int $16
    jz @unveraendert

    mov [edi+TSysKeyEvent.skeKeyCode],ax
    mov dl,true

    @unveraendert:
    mov al,dl
  end;

function SysTVGetShiftState: Byte;(*$FRAME-*)(*$USES NONE*)
  asm
    mov ah,$02
    int $16
  end;

procedure SysTVSetCurPos(X,Y: Integer);(*$FRAME-*)(*$USES EAX,EBX,EDX*)
  asm
    mov ah,$02
    mov bh,video_seite_0
    mov dh,byte [y]
    mov dl,byte [x]
    int $10
  end;

procedure SysTVSetCurType(Y1,Y2: Integer; Show: Boolean);
  begin
    if (y1<0) or (y2<0) then
      begin
        y1:=(-y1*(SysGetTextFontHeight-1)) div 100; // 0040:0085=Font Hîhe
        y2:=(-y2*(SysGetTextFontHeight-1)) div 100;
      end;

    asm (*$Alters EAX,ECX*)
      mov ah,$01
      mov ch,byte [y1]
      mov cl,byte [y2]
      cmp show,true
      je @1
      or ch,$20 // or cx,$2000
      @1:
      int $10
    end;
  end;

procedure SysTVGetCurType(var Y1,Y2: Integer; var Visible: Boolean);(*$FRAME-*)(*$USES EAX,EBX,ECX,EDX,EDI*)
  asm
    mov ah,$03
    mov bh,video_seite_0
    int $10

    mov edi,y1
    movzx eax,ch
    and eax,$1f  // Bit 4..0
    mov [edi],eax

    mov edi,y2
    movzx eax,cl
    and eax,$1f  // Bit 4..0
    mov [edi],eax

    mov al,true
    and ch,$20   // Bit 5(13) invisible
    cmp ch,0
    je @sichtbar

    mov al,false

    @sichtbar:

    mov edi,Visible
    mov [edi],al
  end;

procedure SysTVShowBuf(Pos,Size: Integer);(*$FRAME-*)(*$USES NONE*)
  asm
  end;

procedure SysTVClrScr;(*$FRAME-*)(*$USES EAX,ECX,EDI*)
  asm
    call SysGetTextVideoMemBase
    mov edi,eax

    movzx ecx,smallword [Seg0040+$4c]   // Size Video MEM
    shr ecx,1                           // div 2
    cld
    mov ax,$0720                        // Space character, white on black
    rep stosw
  end;

function SysTVGetScrMode(Size: PSysPoint): Integer;
  begin
    if size<>nil then
      with size do
        begin
          x:=SysGetTextVideoColumns;
          y:=SysGetTextVideoRows;
        end;

    if (SysGetTextVideoModus in [$00,$01,$02,$03,$07]) then
      begin
       if SysGetTextVideoRows=25 then          // Zeilen
         SysTVGetScrMode:=SysGetTextVideoModus
       else
         SysTVGetScrMode:=SysGetTextVideoModus+$100;
      end
    else
      SysTVGetScrMode:=$00ff; // Non-standard

  end;

procedure SysTVSetScrMode(Mode: Integer);(*$FRAME-*)(*$USES EAX,EBX*)
  asm
    mov eax,mode
    push eax
      mov ah,$00
      int $10
    pop eax
    cmp ah,0
    je @ret

    mov ax,$1112 // Font 8x8
    mov bl,0
    int $10

    @ret:
  end;

function SysTVGetSrcBuf: Pointer;(*$FRAME-*)(*$USES NONE*)
  asm
    call SysGetTextVideoMemBase
  end;

procedure SysTVInitCursor;(*$FRAME-*)(*$USES NONE*)
  asm
  end;

function SysPlatformID: Longint;(*$FRAME-*)(*$USES NONE*)
  asm
    mov eax,-2
  end;

procedure SysBeep;(*$FRAME-*)(*$USES EAX*)
  asm
    mov al,$07
    int $29
  end;

function ValidDrive(d: byte): boolean; (*$USES EBX,ECX,EDX*)
  asm
    mov ah,$19   // Get Current Default Drive -> al
    int $21
    mov cl,al    // -> cl

    xor ebx,ebx
    mov ah,$0e   // Select Disk
    mov dl,d
    int $21

    mov ah,$19   // Get Current Default Drive
    int $21
    cmp al,dl
    jne @1
    inc ebx
@1:
    mov ah,$0e   // Select Disk
    mov dl,cl
    int $21
    mov eax,ebx
  end;

const
  ValidDriveMap: longint = 0;
  FirstCall: boolean = true;

{AK155 è‡•¶≠®© ¢†‡®†≠‚ Ø‡®≠„§®‚•´Ï≠Æ ÆØ‡†Ë®¢†´ ¢·• °„™¢Î, ¢™´ÓÁ†Ô
‰´ÆØØ®™®, Á‚Æ Ø‡®¢Æ§®´Æ ™ ¶„‚™®¨ ‚Æ‡¨Æß†¨. èÆ·™Æ´Ï™„ ‰´ÆØØ®™®
Æ°ÎÁ≠Æ ≠• ¨Æ£„‚ ¨Æ≠‚®‡Æ¢†‚Ï·Ô-‡†ß¨Æ≠‚®‡Æ¢†‚Ï·Ô, Ô ·§•´†´
®Â ÆØ‡Æ· Æ§≠Æ™‡†‚≠Î¨. ù‚Æ ¢‡•¨•≠≠Æ• ® ≠• ÆÁ•≠Ï ÂÆ‡ÆË•• ‡•Ë•≠®•,
‚†™ ™†™ ¢·•-‚†™® °Î¢†•‚ subst, † ‰®ß®Á•·™®Â ‰´ÆØØ®™Æ¢ ¨Æ¶•‚ ≠• °Î‚Ï,
Æ·Æ°•≠≠Æ B:}
function SysGetValidDrives: Longint;
  var
    c: byte;
    m: longint;
  begin
  if FirstCall then
    begin
    c := 0; m := 1; ValidDriveMap := 0; FirstCall := false;
    end
  else
    begin
    c := 2; m := 4; ValidDriveMap := ValidDriveMap and 3;
    end;
  repeat
    if ValidDrive(c) then
      ValidDriveMap := ValidDriveMap + m;
    m := m shl 1; inc(c);
  until c = 32;
  SysGetValidDrives := ValidDriveMap;
  end;

(*$IFDEF CLIP_SUPPORT*)
const
  clip_puffer_groesse   =64*1024-1;             // max transfer size
  clip_puffer_16        :smallword=0;           // realmode segment
  clip_puffer_32        :longint=0;             // protmode offset
  clip_typ              =7;                     // OEM text
(*$ENDIF*)


function SysClipCanPaste: Boolean;
  (*$IFDEF CLIP_SUPPORT*)
  (*$FRAME-*)(*$USES ESI,EDI*)
  asm
    sub edi,edi                         // false
    mov esi,$00001700                   // MS Windows "WINOLDAP"
    mov eax,esi
    int $2f
    cmp eax,esi
    je @ret                             // nicht installiert

    cmp [clip_puffer_32],edi            // already Assigned ?
    jne @erfolg

    lea eax,clip_puffer_16
    push eax
    push clip_puffer_groesse
    call GetDosMem
    or eax,eax
    jnz @ret

    push [clip_puffer_16].longint
    call dosseg_linear
    mov [clip_puffer_32],eax

  @erfolg:
    inc edi                             // true
  @ret:
    mov eax,edi
  end;
  (*$ELSE*)
  begin
    SysClipCanPaste:=false;
  end;
  (*$ENDIF*)

function SysClipCopy(P: PChar; Size: Longint): Boolean;
  (*$IFDEF CLIP_SUPPORT*)
  var
    r:real_mode_call_structure_typ;
  begin
    SysClipCopy:=false;
    if SysClipCanPaste                  // installiert ?
    and (Size<clip_puffer_groesse)      // Size zu gro· ?
     then
      with r do
        begin
          Move(P^,Mem[clip_puffer_32],Size); // -> 1. MB

          init_register(r);

          eax_:=$1701;                  // OPEN CLIPBOARD
          intr_realmode(r,$2f);

          eax_:=$1703;                  // SET CLIPBOARD DATA
          edx_:=clip_typ;
          es_:=clip_puffer_16;          // ES:BX
          ebx_:=0;
          esi_:=0;                      // SI shl 16+CX
          ecx_:=Size;
          intr_realmode(r,$2f);

          if r.ax_<>0 then
            SysClipCopy:=true;

          eax_:=$1708;                  // Close Clipboard
          intr_realmode(r,$2f);

        end;
  end;
  (*$ELSE*)
  begin
    SysClipCopy:=false;
  end;
  (*$ENDIF*)

function SysClipPaste(var Size: Integer): Pointer;
  (*$IFDEF CLIP_SUPPORT*)
  var
    r:real_mode_call_structure_typ;
    s:longint;
    p:PChar;
  begin
    SysClipPaste:=nil;
    Size:=0;

    if SysClipCanPaste then             // installiert ?
      with r do
        begin
          init_register(r);

          eax_:=$1701;                  // OPEN CLIPBOARD
          intr_realmode(r,$2f);

          eax_:=$1704;
          edx_:=clip_typ;
          intr_realmode(r,$2f);         // GET CLIPBOARD DATA SIZE
          s:=r.ax_+r.dx_ shl 16;

          if (s>0) and (s<=clip_puffer_groesse) then
            begin

              eax_:=$1705;                  // GET CLIPBOARD DATA
              edx_:=clip_typ;
              es_:=clip_puffer_16;          // ES:BX
              ebx_:=0;
              intr_realmode(r,$2f);

              if r.ax_=0 then               // error, or no data in this format in Clipboard
                Exit;

              GetMem(p,s+1);
              Move(Mem[clip_puffer_32],p^,s);
              p[s]:=#0;
              Size:=StrLen(p)+1;
              SysClipPaste:=p;
            end;

          eax_:=$1708;                  // Close Clipboard
          intr_realmode(r,$2f);
        end;
  end;
  (*$ELSE*)
  begin
    Size:=0;
    SysClipPaste:=nil;
  end;
  (*$ENDIF*)

function SysLoadResourceString(ID: Longint; Buffer: PChar; BufSize: Longint): PChar;(*$FRAME+*)(*$USES ECX*)
  asm
    // Buffer^:=#0
    mov eax,[Buffer]
    mov byte [eax],0

    push [ID]
    call resource_ermittle_adresse_resourcestring
    test eax,eax
    je @ret


    movzx ecx,smallword [eax]
    inc eax
    inc eax

    // min(ecx,eax)
    cmp ecx,[BufSize]
    jle @weniger
    mov ecx,[BufSize]
  @weniger:
    pushad // $Frame+
      push [Buffer]             // Dest
      push eax                  // Source
      push ecx                  // MaxLen
      call StrLCopy
    popad

  @ret:
    // SysLoadResourceString:=Buffer;
    mov eax,[Buffer]
  end;


// this functionality is not available in DOS
// but it can be emulated by including resources from
// unit esyserr into executable

function SysGetSystemError(Code: Longint; Buffer: PChar; BufSize: Longint; var MsgLen: Longint): PChar;
  const
    System_Error        :array[0..13] of char='System Error #';
  var
    num                 :string[10];
    res                 :resource_string_z;
  begin
    res:=resource_ermittle_adresse_resourcestring(Code);
    if Assigned(res) then
      begin
        MsgLen:=Min(res^.len,BufSize);
        StrLCopy(Buffer,res^.str,MsgLen);
      end
    else
      begin
        Str(Code,num);
        if BufSize>Length(System_Error)+Length(num) then
          begin
            Move(System_Error,Buffer^,Length(System_Error));
            Move(num[1],Mem[Ofs(Buffer^)+Length(System_Error)],Length(num));
            MsgLen:=Length(System_Error)+Length(num);
          end
        else
          begin
            Buffer^:='?';
            MsgLen:=1;
          end;
      end;
    SysGetSystemError:=Buffer;
  end;


function SysGetModuleName(var Address: Pointer; Buffer: PChar; BufSize: Longint): PChar;
  begin
    (* ParamStr(0) *)
    SysGetModuleName:=StrLCopy(Buffer,CmdLine,BufSize);
  end;

function SysFileUNCExpand(Dest,Name: PChar): PChar;
  var
    regs                :real_mode_call_structure_typ;
    tmp                 :array[0..260] of char;
  begin
    SysFileExpand(tmp,Name);
    StrCopy(PChar(@Mem[segdossyslow32]),tmp);
    with regs do
      begin
        ah_:=$60;             // "TRUENAME" - CANONICALIZE FILENAME OR PATH
        ds_:=segdossyslow16;  // DS:SI -> ES:DI
        si_:=$0000;
        es_:=segdossyslow16;
        di_:=$0200;
        intr_realmode(regs,$21);
        if (flags_ and flags_carry)=0 then
          StrCopy(Dest,PChar(@Mem[segdossyslow32+$0200]))
        else
          StrCopy(Dest,tmp);
      end;
    SysFileUNCExpand:=Dest;
  end;

type
  country_info_typ=
    packed record
      date_format               :smallword;
        //        0 = USA    mm dd yy
        //        1 = Europe dd mm yy
        //        2 = Japan  yy mm dd
      currency_symbol           :array[0..5-1] of char;
      thousands_separator       :array[0..2-1] of char;
      decimal_separator         :array[0..2-1] of char;
      date_separator            :array[0..2-1] of char;
      time_separator            :array[0..2-1] of char;
      currency_format           :byte;
        //        bit 2 = set if currency symbol replaces decimal point
        //        bit 1 = number of spaces between value and currency symbol
        //        bit 0 = 0 if currency symbol precedes value
        //                1 if currency symbol follows value
      number_of_digits_after_decimal_in_currency:byte;
      time_format               :byte;
        //        bit 0 = 0 if 12-hour clock
        //                1 if 24-hour clock
      address_of_case_map_routine:real_mode_ptr_typ;
      data_list_separator       :array[0..1] of char;
      reserved                  :array[1..10] of byte;
    end;

var
  country_info          :country_info_typ;

procedure hole_country_info;
  var
    regs                :real_mode_call_structure_typ;
  begin
    FillChar(Mem[segdossyslow32],SizeOf(country_info_typ),0);
    with regs do
      begin
        ax_:=$3800;
        ds_:=segdossyslow16; // -> DS:DX
        dx_:=0;
        intr_realmode(regs,$21);
      end;
    Move(Mem[segdossyslow32],country_info,SizeOf(country_info_typ));
  end;

procedure SysGetCurrencyFormat(CString: PChar; var CFormat, CNegFormat, CDecimals: Byte; var CThousandSep, CDecimalSep: Char);
  begin
    hole_country_info;
    StrCopy(CString,country_info.currency_symbol);
    CFormat             :=country_info.currency_format and $3;
                              // '$1'
                              // '1$'
                              // '$ 1'
                              // '1 $'
    case country_info.currency_format and $3 of // nicht vorhanden
      0:CNegFormat      := 3; //  '$1-'
      1:CNegFormat      := 5; //  '-1$'
      2:CNegFormat      :=11; //  '$ 1-'
      3:CNegFormat      := 8; //  '-1 $'
    end;
    CDecimals           :=country_info.number_of_digits_after_decimal_in_currency;
    CThousandSep        :=country_info.thousands_separator[0];
    CDecimalSep         :=country_info.decimal_separator[0];
  end;

const
  ShortDateFormat_tab:array[0..2] of pchar=
    ('mm/dd/yy',
     'dd/mm/yy',
     'yy/mm/dd');
  LongDateFormat_tab:array[0..2] of pchar=
    ('mmmm d, yyyy',
     'dd mmmm yyyy',
     'yyyy mmmm d');

procedure SysGetDateFormat(var DateSeparator: Char; ShortDateFormat,LongDateFormat: PChar);
  begin
    hole_country_info;
    DateSeparator:=country_info.date_separator[0];
    if country_info.date_format>2 then
      country_info.date_format:=0;
    StrCopy(ShortDateFormat,ShortDateFormat_tab[country_info.date_format]);
    StrCopy(LongDateFormat ,LongDateFormat_tab [country_info.date_format]);
  end;

procedure SysGetTimeFormat(var TimeSeparator: Char; TimeAMString,TimePMString,ShortTimeFormat,LongTimeFormat: PChar);
  begin
    hole_country_info;
    TimeSeparator:=country_info.time_separator[0];
    StrCopy(TimeAMString    ,'am');
    StrCopy(TimePMString    ,'pm');
    StrCopy(ShortTimeFormat ,'hh:mm');
    StrCopy(LongTimeFormat  ,'hh:mm:ss');
  end;

procedure SysDisplayConsoleError(PopupErrors: Boolean; Title, Msg: PChar);assembler;
(*$FRAME-*)(*$USES ALL*)

  procedure newline;(*$FRAME-*)(*$USES NONE*)
    asm
      mov al,13
      int $29
      mov al,10
      int $29
    end;

  procedure display_pchar;(*$FRAME-*)(*$USES NONE*)
    asm
    @1:
      lodsb
      cmp al,0
      je @2
      int $29
      jmp @1
    @2:
    end;

  asm
    // could be impoved : PopupErrors is ignored

    call newline
    mov esi,title
    cld

    call display_pchar
    call newline
    // length of title+1
    mov ecx,esi
    sub ecx,title
    mov al,'='
  @3:
    int $29
    loop @3

    call newline
    mov esi,msg
    call display_pchar
    call newline
  end;

procedure SysDisableHardErrors;
  begin
    DPMI32.install_i24;
  end;

function SysKillProcess(Process: Longint): Longint;
  begin
    //NOT_IMPLEMENTED
    SysKillProcess:=-1;
  end;

function SysAllocSharedMem(Size: Longint; var MemPtr: Pointer): Longint;
  begin
    //NOT_IMPLEMENTED       DPMI 1.0 has shared memory...
    SysAllocSharedMem:=-1;
  end;

function SysGiveSharedMem(MemPtr: Pointer): Longint;
  begin
    //NOT_IMPLEMENTED
    SysGiveSharedMem:=-1;
  end;

function SysPipeCreate(var ReadHandle,WriteHandle: Longint; Size: Longint): Longint;
  (*$IFDEF PIPE_SUPPORT*)
  !
  (*$ELSE*)
  begin
    //NOT_IMPLEMENTED
    SysPipeCreate:=-1;
  end;
  (*$ENDIF*)

function SysPipePeek(Pipe: Longint; Buffer: Pointer; BufSize: Longint; var BytesRead: Longint; var IsClosing: Boolean): Longint;
  (*$IFDEF PIPE_SUPPORT*)
  !
  (*$ELSE*)
  begin
    //NOT_IMPLEMENTED
    SysPipePeek:=-1;
  end;
  (*$ENDIF*)

function SysPipeClose(Pipe: Longint): Longint;
  (*$IFDEF PIPE_SUPPORT*)
  begin
    SysPipeClose:=SysFileClose(Pipe);
  end;
  (*$ELSE*)
  begin
    //NOT_IMPLEMENTED
    SysPipeClose:=-1;
  end;
  (*$ENDIF*)

procedure SysDisplayGUIError(Title, Msg: PChar);
  begin
    SysDisplayConsoleError(true,Title,Msg);
  end;


procedure getdosstack_und_fs;(*$FRAME-*)(*$USES EAX,EBX,EDX*)
  asm
    mov eax,$0100               // DPMI 0.9+ ALLOCATE DOS MEMORY BLOCK
    mov bx,(4096+100)/16        // in 16 Byte Blocks
    int $31
    jnc @1

  @fehler:
    push 8
    call _RunError
    //************

  @1:
    mov segdossyslow16,ax
    mov arbeits_dta.seg_,ax
    mov arbeits_dta.ofs_,0

    movzx eax,ax
    shl eax,4                   // dosseg_linear
    mov segdossyslow32,eax


    // Allocate real mode stack
    mov eax,$0100               // DPMI 0.9+ ALLOCATE DOS MEMORY BLOCK
    mov bx,2048/16
    int $31
    jc @fehler

    mov real_mode_stack.seg_,ax
    mov real_mode_stack.ofs_,2048-4


    // Allocate Memory for TIB-Emulation (FS:[0]..)
    mov eax,$0100
    mov bx,4                    // 4*16 Byte sind mehr als genug
    int $31
    jc @fehler

    // -> Selector DX
    mov sel_fs,dx
    mov fs,dx
  end;

procedure untersuche_dos;(*$FRAME-*)(*$USES ALL*)
  asm
    mov ax,$2700 // NW/DR-DOS Taskmgr
    int $2f      // 00->false
    cmp al,$ff   // ff->true
    sete taskmgr

    mov ax,$4010 // OS/2 2.0+
    int $2f
    cmp ax,$4010
    setne os2

  end;

procedure berechne_segmente;assembler;
  (*$FRAME-*)(*$USES ALL*)
  var
    regs:real_mode_call_structure_typ;
  asm

//--    movzx eax,sel_psp   // PSP: SEL->OFS
//--    push eax
//--    call protsel_linear
//--    mov seg_psp,eax
//--
//--    movzx eax,smallword [eax+$2c] // ENV= PSP:002c
//--    push eax
//--    call protsel_linear
//--    cmp eax,-1
//--    jne @1
//--
//--    // eclipse os has sel 0 for env ...
//--    push 216
//--    call _runerror
//--
//--  @1:
//--    mov Environment,eax

    lea esi,[regs]
    push esi
    call init_register

    mov [regs.ah_],$62             // get PSP seg
    and [regs.ebx_],0

    push esi
    push $21
    call intr_realmode

    mov eax,[regs.ebx_]
    shl eax,4
    cmp [eax].smallword,$20cd
    je @valid_psp

    push 216                            // RTE_Access_Violation
    call _RunError

  @valid_psp:
    mov [seg_psp],eax

    movzx eax,smallword [eax+$2c]       // environment selector= PSP:$002c
    push eax
    call protsel_linear

    cmp eax,-1
    je @invalid_env_sel

    cmp [eax-$10].byte,'M'              // valid memory control block before env ?
    je @valid_env
    cmp [eax-$10].byte,'Z'
    je @valid_env

  @invalid_env_sel:                     // causeway+dos+himem.sys ...

    mov eax,[seg_psp]
    movzx eax,smallword [eax+$2c]       // environment segement = PSP:$002c
    shl eax,4

  @valid_env:
    mov [Environment],eax

  end;

procedure bestimme_dpmi_version;(*$FRAME-*)(*$USES ALL*)
  asm
    mov eax,$0400
    int $31
    cmp ax,$0090
    jne @1
    mov al,   90
    @1:
    mov dpmi_version,eax
    dec cl          // 3=386 4=486 ..
    mov Test8086,cl

    // switch CauseWay into DOS4G mode
    mov eax,$ff00
    mov edx,$0078
    int $21
  end;

procedure SysLowInit;assembler;(*$FRAME-*)(*$USES NONE*)
  const
    stack_push= 4 // push esi
              + 4 // push eax
              + 4 // call VpSysLow.SysLowInit
              +16 // SizeOf(TExcFrame)
              + 4;// call System.Initexe
  asm
    // query DPMI version and 386/486.. processor
    call bestimme_dpmi_version

    // get memory in first megabyte for FS:[0] Tib emulation
    // and for real mode DOS API calls
    call getdosstack_und_fs

    // initialize TIB-Emulation
    push eax
      push esi

        sub esi,esi
        // Head of exception handler chain
        or fs:[esi+Tib.Tib_PExchain],-1

        // bigger end of Stack
        lea eax,[esp+stack_push]
        mov fs:[esi+Tib.Tib_PStackLimit],eax
        // lower end of Stack
        sub eax,stacksize
        mov fs:[esi+Tib.Tib_PStack],eax

        // unused
        sub eax,eax
        mov fs:[esi+Tib.Tib_PTib2],eax
        mov fs:[esi+Tib.Tib_Version],eax

        // thread #1
        inc eax
        mov fs:[esi+Tib.Tib_Ordinal],eax

      pop esi
    pop eax

    // calculate some real mode memory locations
    call berechne_segmente
    // prepare SysCmdln,SysCmdlnCount
    call berechne_parameter
    // detect video adapter type
    call detect_mdacgaegavga
    // for SysCtrlSleep
    call calibrate_delayloop
    // check for Multitaskers
    call untersuche_dos
    // install handlers for exceptions and ^C
    call installiere_exception_behandlungen

    (*$IFDEF PE2LE_P2*)
    jmp @ret

    //*****************************************************************
    // copy from pe2le: start.pas (procedure start2;)
    // if included here pe2le will find it
    // and save nearly 4096 bytes (1 page alignment)

    call @pop_edx
  @call_pop_edx:
    db 'WATCOM',0
  @pop_edx:
    pop edx
    sub edx,'5555'      // @call_pop_edx-@start2_virtuapl_pascal_code...

    mov eax,'EIP '
    add eax,edx
    push eax

      mov esi,'RELO'
      add esi,edx
      push esi          // Ab hier wird spÑter der Speicher gelîscht

        cld
        lodsd
        lea edi,[eax+edx]
  @schleife:
        add [edi],edx
        add edi,4

        sub eax,eax
        lodsb

        // mehr als 253 Byte Unterschied ?
        cmp al,254
        jb @klein
        // Ende der RELO-Daten ?
        // (al=255)
        ja  @ende

        // >253
        lodsd
  @klein:
        add edi,eax
        jmp @schleife

  @ende:

      pop edi           // FillChar(Mem[edi],ecx,0)
      push es           // ES (PSP) sichern
        push ds
        pop es          // ES:=Datenbereich

        sub eax,eax
        mov ecx,'VAR0'
        push ecx
          shr ecx,2     // div 4
          rep stosd
        pop ecx
        and ecx,3       // mod 4
        rep stosb
      pop es

      ret               // zum Programmstart

    db '****'           // hier wird eine Fixup auf [1:0] verweisen
    db '****'           // mehr Platz damit niemals Åber Seitengrenzen

    //
    //*****************************************************************

    @ret:
    (*$ENDIF*)
  end;

