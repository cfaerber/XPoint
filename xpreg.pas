{ $Id$

   OpenXP main source file
   Copyright (C) 2000-2002 OpenXP team (www.openxp.de)
   Copyright (C) 1991-1999 Peter Mandrella (www.mandrella.de)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{ Registrierung }

{$I xpdefine.inc }

unit xpreg;

interface

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,inout,keys,maus2,maske,resource,xp0,xp1,xp1input,xpnt,xpglobal;

procedure OpenXPInfo;
procedure BetaMessage;
procedure ShowAboutDialog;

implementation

uses
  Winxp;

procedure OpenXPInfo;
var x,y,i : Integer;
    s     : string;
    p     : byte;

    msglines    : byte;

    sely  : byte;
    z     : taste;
begin
  msglines:=ival(getres2(520,0));
  msgbox(70,msglines+8,'',x,y);
  moff;
  wrt(x+3,y+1,'Open \\//     '+
              RightStr('           ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 50));
  wrt(x+3,y+2,'     //\\ XP');
  s:=x_copyright + ' ' + author_name;
  wrt(x+67-length(s),y+2,s);

  for i:=1 to msglines do begin
    s:=getres2(520,i);
    gotoxy(x+3,y+4+i);
    repeat
      p:=cposx('*',s);
      Wrt2(LeftStr(s,p-1));
      delete(s,1,p);
      p:=cposx('*',s);
      attrtxt(col.colmboxhigh);
      Wrt2(LeftStr(s,p-1));
      attrtxt(col.colmbox);
      delete(s,1,p);
    until s='';
    end;
  mon;
  sely:=y+msglines+6;
  attrtxt(col.colmbox);
  mwrt(x+3,sely,sp(65));
  z:='';
  pushhp(1550);
  ReadButton(x+3,sely,2,'*'+getres2(520,30),1, true,z);
  pophp;
  closebox;
  freeres;
end;

procedure BetaMessage;
var x,y,i : Integer;
    msglines    : byte;
    z     : taste;
    s:String;
begin
  msglines:=ival(getres2(530,0));
  msgbox(73,msglines+7,'',x,y);
  moff;
  wrt(x+3,y+1,'Open \\//    '+
              RightStr('              ' + verstr+pformstr+betastr+' (c) 1992-99 '+pm, 53));
  wrt(x+3,y+2,'     //\\ XP');
  s:=x_copyright + ' ' + author_name;
  wrt(x+69-length(s),y+2,s);

  for i:=1 to msglines do
  begin
    s:=getres2(530,i);
    wrt(x+3,y+3+i, s);
  end;
  mon;
  pushhp(1550);
  quit := (ReadButton(x+45,y+msglines+5,2,'*'+getres2(530,30),1,true,z) <> 1);
  pophp;
  closebox;
  freeres;
end;

procedure ShowAboutDialog;
var x,y : integer;
    z   : taste;
    ver : string;
    addxVer,addxInf,addxDia,addy : shortint;
begin
  addy := 0;
  addxVer := 0;
  addxInf := 0;
  addxDia := 0;
  ver := xp_xp+' '+verstr+betastr;
  {$IFDEF Snapshot}
    addy := addy+1;
  {$ENDIF}
//  if registriert.r2 then addy := addy+1;
  if length(ver) > 28 then  { Versionsstring lÑnger als PM-Copyright }
  begin
    if odd(length(ver)) then
     addxDia := length(ver)-27
    else
      addxDia := length(ver)-28;
    addxInf := addxDia div 2;
  end else                  { Versionsstring gleich lang oder kÅrzer als PM-Copyright }
  begin
    if odd(length(ver)) then
      addxVer := (27-length(ver)) div 2
    else
      addxVer := (28-length(ver)) div 2;
  end;
  diabox(34+addxDia,17+addy,'',x,y);
  moff;
  attrtxt(col.colmboxhigh);
  wrt(x+15+addxInf,y+2,'\\//');
  wrt(x+15+addxInf,y+3,'//\\' );
  wrt(x+3+addxVer,y+5,ver);
  {$IFDEF Snapshot}
    wrt(x+3+addxVer,y+6,'Snapshot');
  {$ENDIF}
  attrtxt(col.colmbox);
  wrt(x+9+addxInf,y+2,'Open');
  wrt(x+21+addxInf,y+3,'XP');
  wrt(x+3+addxInf,y+7+addy,'(c) 1992-99   '+pm);
  wrt(x+3+addxInf,y+8+addy,x_copyright+' '+author_name);
  wrt(x+3+addxInf,y+11+addy,'eMail: '+author_mail);
  mon;
  ReadButton(x+12+addxInf,y+14+addy,1,'*   ^OK   ',1,true,z);
  closebox;
  freeres;
end;

end.
