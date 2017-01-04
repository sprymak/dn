{ÚÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂÂ¿}
{ÃÅÅ ”y­ªæ¨ï ¯®áâp®¥­¨ï åíè  áâp®ª¨. ‘¯®pâ¨p®¢ ­  ¨§ Modula 2 (c)JPI ÅÅ´}
{ÃÅÅ  ®pâ¨«: €­â®­ ”¥¤®p®¢ aka DataCompBoy                          ÅÅ´}
{ÃÅÅ          ¢ ç¥áâì Œ â pëª¨­®© “«ìï­ë...                          ÅÅ´}
{ÀÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÙ}
unit hash;
interface

Function HashString(S: PChar; Len: Word; Range: Word) : Word; far;

implementation

Function HashString(S: PChar; Len: Word; Range: Word) : Word; assembler;
const segm= 12; offs=10; ssize=8; modulus=6;
 asm
  push ds; push es; push si; push di;
  sub dx,dx
  sub ax,ax
  mov cx,[bp][ssize]
  jcxz @Exit
  mov ds,[bp][segm]
  mov si,[bp][offs]
  sub bx,bx
  mov di,[bp][ssize]
@loop:
  lodsb
  or al,al
  jz @EndLoop
  xor dl,al
  rol dx,1
  rol dx,1
  rol dx,1
  add dx,di
  sub dx,cx
  loop @loop
@EndLoop:
  mov ax,dx
  sub dx,dx
  div word [bp][modulus]
@Exit:
  mov ax,dx
  pop di; pop si; pop es; pop ds
 end;
end.