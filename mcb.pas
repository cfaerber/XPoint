{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 2000 by Robert B”ck                                         }
{ (c) 2000 XP2 Team, weitere Informationen unter:                 }
{                   http://www.xp2.de/                            }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ --------------------------------------------------------------- }
{ $Id$ }

unit mcb;

{$I XPDEFINE.INC }

interface

uses xpglobal;

type mcbp=^mcbt;
     mcbt=record
            id:char;
            psp_seg:word;
            size:word;
            fill:array[1..3] of byte;
            pname:array[1..8] of char;
          end;

function firstmcb:mcbp;
function nextmcb(mcb:mcbp):mcbp;
function ispsp(mcb:mcbp):boolean;
function getmcbprog(mcb:mcbp):string;
function getmcbenvseg(mcb:mcbp):word;
function getmcbenvprog(envseg:word):string;
function shortp(s:string):string;

implementation

uses
  Typeform;

var dosmaj:byte;

function firstmcb:mcbp; assembler;
  asm
    mov ax,5200h
    int 21h
    mov dx,es:[bx-2]
    xor ax,ax
  end;

function nextmcb(mcb:mcbp):mcbp; assembler;
  asm
    les bx,mcb
    mov dx,es
    add dx,es:[bx+3]
    inc dx
    xor ax,ax
  end;

function ispsp(mcb:mcbp):boolean; assembler;
  asm
    les bx,mcb
    mov es,es:[bx+1]
    cmp word ptr es:[0],20CDh
    mov al,TRUE
    je @e
    mov al,FALSE
  @e:
  end;

function getmcbprog(mcb:mcbp):string;
  var i:integer;
      s:string;
  begin
    s:='';
    i:=1;
    if (dosmaj>=4) and ispsp(mcb) then
    while (i<=8) and (mcb^.pname[i] in ['A'..'Z']) do begin
      s:=s+mcb^.pname[i];
      inc(i);
    end;
    getmcbprog:=s;
  end;

function getmcbenvseg(mcb:mcbp):word; assembler;
  asm
    les bx,mcb
    mov es,es:[bx+1]
    mov ax,es:[2Ch]
  end;

function getmcbenvprog(envseg:word):string;
  var s:string;
      envofs:word;
  begin
    s:='';
    if envseg<>0 then begin
      envofs:=0;
      repeat
        inc(envofs);
      until memw[envseg:envofs]=0;
      inc(envofs,2);
      if memw[envseg:envofs]=1 then begin
        inc(envofs,2);
        repeat
          s:=s+char(mem[envseg:envofs]);
          inc(envofs)
        until mem[envseg:envofs]=0;
      end;
    end;
    getmcbenvprog:=s;
  end;

function shortp(s:string):string;
  var p:integer;
  begin
    p:=cpos('\',s);
    while p>0 do begin
      delete(s,1,p);
      p:=cpos('\',s);
    end;
    p:=cpos('.',s);
    if p>0 then s:=copy(s,1,p-1);
    shortp:=s;
  end;

begin
  asm
    mov ax,3000h
    int 21h
    mov dosmaj,al
  end;
end.

{
  $Log$
  Revision 1.1.2.2  2001/08/11 22:17:52  mk
  - changed Pos() to cPos() when possible, saves 1814 Bytes ;)

  Revision 1.1.2.1  2000/11/21 22:40:37  mk
  - MCB-Code von XP2 (Robert Boeck) hinzugefuegt um auf schon geladenes XP zu testen

  Revision 1.1  2000/11/17 02:22:36  rb
  Mehrfachstart in einer Shell verhindern, auch wenn der Prompt in der Shell
  ge„ndert wurde. Funktioniert ab DOS 4.0.


}
