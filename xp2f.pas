{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

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

{ CrossPoint Config - Farben, F-Tasten, Feiertage }

{$I xpdefine.inc }

unit xp2f;

interface


procedure EditFkeys(typ:byte);    { 0=Zusatz, 1=Alt, 2=Ctrl, 3=Shift todo: enum}
procedure CfgColors;


implementation  { -------------------------------------------------- }

uses
  sysutils,
{$IFDEF NCRT }
  xpcurses,
{$ENDIF }
  typeform,inout,keys,winxp,maske,maus2,resource,
  xp0,xp1,xp1help,xp1input,xp2,
  xpglobal;

procedure EditFkeys(typ:byte);
{ const n_typ : array[0..3] of string[9] =
              ('ohne Kopf','mit Kopf','Puffer','Quote'); }
var anzahl  : Integer;
    x,y,p,i : Integer;
    txt     : string;
    t       : taste;
    modi    : boolean;

  procedure edit(p:byte);
  var x,y,i : Integer;
      s     : string;
      brk   : boolean;
  begin
    with fkeys[iif(p>10,4,typ)][iif(p>10,p-10,p)] do begin
      dialog(55,12,iifs(txt='',getres2(240,1)+' ',txt)+strs(p),x,y);  { 'Zusatz-Menue' }
      maddstring(3,2,getres2(240,4),menue,20,20,''); mhnr(440);   { 'Menueanzeige  ' }
      maddstring(3,4,getres2(240,5),prog,35,248,'');  { 'Programmname ' }
      s:=getres2(240,ntyp+7);
      maddstring(3,6,getres2(240,6),s,10,10,'');      { '$FILE-Nachr. ' }
      for i:=7 to 10 do
        mappsel(true,getres2(240,i));
      maddbool(36,6,getres2(240,11),bname);           { 'aus Betreff' }
      maddbool(3,8,getres2(240,12),warten);           { 'Warten'      }
      maddbool(3,9,getres2(240,13),vollbild);         { 'Vollbild'    }
      maddbool(3,10,getres2(240,14),listout);         { 'Ausgabe an Lister' }
      maddbool(3,11,getres2(240,17),autoexec);        { 'AUTOEXEC-Verzeichnis bearbeiten' }
      if speicher=0 then speicher:=600;
      maddint(32,9,getres2(240,15),speicher,5,3,50,600);  { 'Speicher: ' }
      maddtext(39+length(getres2(240,15)),9,getres2(240,16),col.coldialog);   { 'KByte' }
      readmask(brk);
      modi:=modi or mmodified;
      if not brk then
        for i:=0 to 3 do
          if UpperCase(s)=UpperCase(getres2(240,i+7)) then ntyp:=i;
      freeres;
      enddialog;
      end;
  end;

  procedure maus_bearbeiten;
  var xx,yy  : integer;
      inside : boolean;
      outside: boolean;
  begin
    maus_gettext(xx,yy);
    inside:=(xx>x) and (xx<x+72+length(txt)) and (yy>y+1) and (yy<=y+anzahl+1);
    outside:=(xx<x) or (xx>x+72+length(txt)) or (yy<y) or (yy>y+anzahl+2);
    if inside then
      if (t=mausleft) or (t=mauslmoved) then
        p:=yy-y-1
      else if (t=mausunleft) or (t=mausldouble) then
        t:=keycr
      else
        t:=#0
    else if outside then
      if (t=mausunleft) or (t=mausunright) then
        t:=keyesc
      else
        t:=#0
    else
      t:=#0;
  end;

  function chk(b:boolean):char;
  begin
    chk:=iifc(b,'�',' ');
  end;

begin
  if typ=0 then anzahl:=iif(screenlines=25,19,20)
  else anzahl:=10;
  case typ of
    0 : txt:='';
    1 : txt:='Shift-F';
    2 : txt:='Ctrl-F';
    3 : txt:='Alt-F';
  end;
  selbox(73+length(txt),anzahl+3,getres2(240,iif(typ=0,1,2)),x,y,false);
  attrtxt(col.colsel2high);            { 'Zusatz-Menue' / 'Funktionstasten' }
  mwrt(x+1,y+1,forms(sp(5+length(txt))+getres2(240,3),73+length(txt)-2));
               { 'Menue           Programm                   $FILE      B W L A  Mem' }
  p:=1;
  modi:=false;
  repeat
    moff;
    for i:=1 to anzahl do
    begin
      if i=p then
	attrtxt(col.colsel2bar)
      else
	attrtxt(col.colsel2box);
      with fkeys[iif(i>10,4,typ)][iif(i>10,i-10,i)] do
      begin
        wrt(x+1,y+1+i,' '+forms(txt+strs(i),length(txt)+3));
        if menue+prog='' then
	  Wrt2(sp(67))
	else
	begin
          Wrt2(' ' + forms(menue,14) + ' ' + forms(prog,26));
          if copy(prog,1,1)='*' then
            Wrt2(sp(18) + chk(autoexec) + '      ')
          else
	     Wrt2(' ' + forms(getres2(240,ntyp+7),10)
		  + ' ' + chk(bname)
		  + ' ' + chk(warten)
		  + ' ' + chk(listout)
		  + ' ' + chk(autoexec) +  '      '{
		  + ' ' + IntToStr(speicher)});
	end;
      end;
    end;
    mon;
    repeat
      if auswahlcursor then begin
        gotoxy(x+1,y+1+p);
        get(t,curon);
        end
      else
        get(t,curoff);
      if (t>=mausfirstkey) and (t<=mauslastkey) then
        maus_bearbeiten;
    until t<>#0;
    if (t=keyup) and (p>1) then dec(p);
    if (t=keydown) and (p<anzahl) then inc(p);
    if (t=keyhome) or (t=keypgup) then p:=1;
    if (t=keyend) or (t=keypgdn) then p:=anzahl;
    if (t=keycr) or (UpperCase(t)='E') then edit(p);
  until t=keyesc;
  closebox;
  freeres;
  if modi then begin
    case typ of
      0 : zusatz_menue;
      3 : setaltfkeys;
    end;
    dispfunctionkeys(false);
    GlobalModified;
    end;
end;


{ --- Farbsetup ---------------------------------------------------- }


procedure savecolors;
var t : text;
    i : integer;

  procedure wrl(const s:string);
  begin
    write(t,s,'=');
  end;

  procedure wrh(b:byte);
  begin
    write(t,'$',LowerCase(hex(b,2)),' ');
  end;

  procedure wrhl(b:byte);
  begin
    wrh(b);
    writeln(t);
  end;

begin
  assign(t,colcfgfile);
  rewrite(t);
  if ioresult<>0 then begin
    rfehler1(107,UpperCase(colcfgfile));  { 'Fehler beim Schreiben von %s' }
    exit;
    end;
  writeln(t,'## ',getres(241));   { 'CrossPoint - Farbkonfiguration' }
  writeln(t);
  with col do begin
    for i:=0 to 3 do begin
      wrl('Menue'+strs(i));
      wrh(colmenu[i]); wrh(colmenuhigh[i]); wrh(colmenuinv[i]);
      wrh(colmenuinvhi[i]); wrh(colmenudis[i]); wrhl(colmenuseldis[i]);
      end;
    wrl('Hotkeys'); wrh(colkeys); wrh(colkeyshigh); wrh(colkeysact);
                    wrhl(colkeysacthi);
    wrl('Trennlinien'); wrhl(coltline);
    wrl('Bretter'); wrh(colbretter); wrh(colbretterinv); wrh(colbretterhi);
                    wrhl(colbrettertr);
    wrl('Msgs'); wrh(colmsgs); wrh(colmsgshigh); wrh(colmsgsinv);
                 wrh(colmsgsinfo); wrh(colmsgsuser); wrhl(colmsgsinvuser);
    wrl('MBox'); wrh(colmbox); wrh(colmboxrahmen); wrhl(colmboxhigh);
    wrl('Dialog'); wrh(coldialog); wrh(coldiarahmen); wrh(coldiahigh);
                   wrh(coldiainp); wrh(coldiamarked); wrh(coldiaarrows);
                   wrh(coldiasel); wrh(coldiaselbar); wrhl(coldiabuttons);
    wrl('Sel1'); wrh(colselbox); wrh(colselrahmen); wrh(colselhigh); wrhl(colselbar);
    wrl('Sel2'); wrh(colsel2box); wrh(colsel2rahmen); wrh(colsel2high);
                 wrhl(colsel2bar);
    wrl('Buttons'); wrh(colbutton); wrh(colbuttonhigh); wrhl(colbuttonarr);
    wrl('Utility'); wrh(colutility); wrh(colutihigh); wrhl(colutiinv);
    wrl('Hilfe'); wrh(colhelp); wrh(colhelphigh); wrh(colhelpqvw); wrhl(colhelpslqvw);
    wrl('Lister'); wrh(collisttext); wrh(collistmarked); wrh(collistselbar);
                   wrh(collistfound); wrh(colliststatus); wrh(collistquote[1]);
                   wrh(collistscroll); wrh(collistheader); wrh(collisthigh);
                   wrh(collistqhigh[1]); wrh(collistheaderhigh);
                   wrhl(collistsignatur);
    wrl('Editor'); wrh(coledittext); wrh(coleditmarked); wrh(coleditstatus);
                   wrh(coleditmessage); wrh(coledithead); wrh(coleditquote[1]);
                   wrh(coleditendmark); wrh(coleditmenu); wrh(coleditmenuhi);
                   wrh(coleditmenuinv); wrhl(coledithiinv);
    wrl('Quotes'); for i:=2 to 9 do wrh(collistquote[i]);
                   for i:=2 to 9 do wrh(collistqhigh[i]);
                   for i:=2 to 8 do wrh(coleditquote[i]);
                   wrhl(coleditquote[9]);
    wrl('ArcViewer'); wrhl(colarcstat);
    wrl('Maps'); wrhl(colmapsbest);
    wrl('Mailer'); wrh(colmailer); wrh(colmailerhigh); wrhl(colmailerhi2);
    wrl('Border'); wrhl(colborder);
    wrl('Priority'); wrh(colmsgsprio1); wrh(colmsgsprio2);
                     wrh(colmsgsprio4) ;wrhl(colmsgsprio5);
    end;
  close(t);
end;


procedure at(c:byte);
begin
  attrtxt(c);
end;

procedure wrkeys(high:boolean);
var len :integer;
begin
  len:=Screenwidth-80;
  with col do begin
    at(colkeys);
    wrt(31,2,forms('  Alle  Brief  Textfile  Binaer  Spezial  Lesen    ',len+50));
    wrt(31,screenlines,forms('F1-Hilfe  F6-Makros  F9-DOS'
                              +dup(13+len,' ')
                              +RightStr(sp(10)+xp_xp,10),len+50));
    at(colkeyshigh);
    wrt(33,2,'A'); wrt(39,2,'B'); wrt(47,2,'e'); wrt(57,2,'i');
    wrt(63,2,'S'); wrt(72,2,'L');
    wrt(31,screenlines,'F1'); wrt(41,screenlines,'F6');
    wrt(52,screenlines,'F9');
    if high then begin
      at(colkeysact);
      wrt(62,2,' Spezial ');
      at(colkeysacthi);
      wrt(63,2,'S');
      end;
    end;
end;

procedure showcol;
var
  Len: Integer;
begin
  len := ScreenWidth-80+50;
  with col do begin
    at(colmenu[0]);
    moff;
    wrt(31,1,forms(
    '  '+iifs(xp_xp='CrossPoint','X','?')+'Point  Wartung  Nachricht  NeTcall  Fido  Edit ',len));
    at(colmenuhigh[0]);
    if xp_xp='CrossPoint' then wrt(33,1,'X')
    else wrt(33,1,'?');
    wrt(41,1,'W'); wrt(50,1,'N'); wrt(63,1,'T');
    wrt(70,1,'F'); wrt(76,1,'E');
    wrkeys(false);
    at(colTline);
    wrt(31,3,dup(len,'�'));
    wrt(31,screenlines-1,dup(len,'�'));
    at(colbretter);
    wrt(31,4,forms('  /Z-NETZ/RECHNER/IBM/ALLGEMEINES',len));
    wrt(31,5,forms('  /Z-NETZ/RECHNER/IBM/BINAER',len));
    wrt(31,7,forms('  /Z-NETZ/RECHNER/IBM/PROGRAMMIEREN',len));
    wrt(31,9,forms('  /Z-NETZ/RECHNER/IBM/VIREN',len));
    wrt(31,11,forms('  /Z-NETZ/TELECOM/0130',len));
    wrt(31,12,forms('  /Z-NETZ/TELECOM/MODEM',len));
    at(colbretterinv);
    wrt(31,6,forms('  /Z-NETZ/RECHNER/IBM/HARDWARE       (gewaehlt)',len));
    at(colbretterhi);
    wrt(31,8,forms('  /Z-NETZ/RECHNER/IBM/SPIELE         (markiert)',len));
    at(colbrettertr);
    wrt(31,10,'  '+dup(ScreenWidth-80+37,'-')+' Trennzeile');
    at(colbretter);
    mon;
    clwin(31,ScreenWidth,13,screenlines-2);
    end;
end;

procedure showmsgs;
  var len :integer;
begin
  len:=Screenwidth-80+50;
  with col do begin
    at(colmsgsinfo);
    moff;
    wrt(31,4,forms(' /Z-NETZ/DATENSCHUTZ/ALLGEMEIN',len));
    at(colmsgs);
    wrt(31, 5,forms('      391 01.11.91 GUENNI@GCS.ZER          Datenschutz',len));
    wrt(31, 6,forms(' >    592 03.11.91 H_TIETZ@TELEMAIL.ZER    Re: Personunion',len));
    wrt(31, 8,forms(' >    140 04.11.91 GUIDO@FISHTOWN.ZER      Re: Datenschutz',len));
    wrt(31,11,forms('      619 16.11.91 BYTEBOOSTERS@AME.ZER    infos zu fido :-)',len));
    wrt(31,12,forms(' >     2k 16.11.91 P.SCHAAR@LINK-HH.ZER    Re: Datenklau',len));
    at(colmsgshigh);
    wrt(31, 7,forms('�     262 04.11.91 R.SAUER@BIONIC.ZER      Re: Personunion',len));
    wrt(31, 9,forms('�     532 05.11.91 U.FLECKENSTEIN@MAX-002. Personunion',len));
    at(colmsgsinv);
    wrt(31,10,forms('      532 10.11.91 E.PETERSEN@TRILOS.ZER   Re: U.F',len));
    at(colmsgs);
    mon;
    clwin(31,ScreenWidth,13,screenlines-2);
    end;
end;

procedure showmenus0;
begin
  with col do begin
    at(colmenu[0]);
    moff;
    wrt(31,1,forms('   Point  Wartung  Nachricht  NeTcall  Fido  Edit ', Screenwidth-80+50));
    at(colmenuhigh[0]);
    wrt(33,1,'X'); wrt(41,1,'W'); wrt(50,1,'N'); wrt(63,1,'T');
    wrt(70,1,'F'); wrt(76,1,'E');
    at(colmenuinv[0]);
    if xp_xp='CrossPoint' then wrt(32,1,' XPoint ')
    else wrt(33,1,' Point ');
    at(colmenuinvhi[0]);
    if xp_xp='CrossPoint' then wrt(33,1,'X')
    else wrt(34,1,'P');
    mon;
    end;
end;

procedure wwin(l,r,o,u:byte; txt:string; rahmen,text:byte);
begin
  normattr:=rahmen; normtxt; forcecolor:=true;
  rahmen1(l,r,o,u,'');
  wshadow(l+1,r+1,o+1,u+1);
  if txt<>'' then mwrt(l+2,o,' '+txt+' ');
  forcecolor:=false;
  at(text);
  clwin(l+1,r-1,o+1,u-1);
end;

procedure showmenus1;
begin
  with col do begin
    wwin(32,48,2,11,'',colmenu[1],colmenu[1]);
    at(colmenudis[1]);
    moff;
    wrt(33,3,' Registrierung ');
    at(colmenuinv[1]);
    wrt(33,4,' Import..      ');
    at(colmenu[1]);
    wrt(33,5,' Export..     ');
    wrt(33,6,' Statistik..');
    wrt(33,7,' Terminal');
    wrt(33,8,' DOS        F9');
    wrt(32,9,'���������������Ĵ');
    wrt(33,10,' Beenden');
    at(colmenuhigh[1]);
    wrt(34,5,'E'); wrt(34,6,'S'); wrt(34,7,'T');
    wrt(34,8,'D'); wrt(34,10,'B');
    at(colmenuinvhi[1]);
    wrt(34,4,'I');
    mon;
    end;
end;

procedure showmenus2;
begin
  with col do begin
    wwin(34,55,5,13,'',colmenu[2],colmenu[2]);
    moff;
    wrt(35,6,' Puffer einlesen ');
    wrt(35,7,' Fremdformat     ');
    wrt(35,8,' Userliste       ');
    wrt(34,10,'��������������������Ĵ');
    wrt(35,12,' Yuppie-Nachrichten');
    at(colmenudis[2]);
    wrt(35,9,' Brettliste      ');
    at(colmenuinv[2]);
    wrt(35,11,' Spezial            ');
    at(colmenuhigh[2]);
    wrt(36,6,'P'); wrt(36,7,'F'); wrt(36,8,'U'); wrt(36,12,'Y');
    at(colmenuinvhi[2]);
    wrt(36,11,'S');
    mon;
    end;
end;

procedure showmenus3;
begin
  with col do begin
    wwin(36,57,12,17,'',colmenu[3],colmenu[3]);
    moff;
    wrt(37,13,' Kaffee kochen      ');
    wrt(37,14,' Pizza holen        ');
    at(colmenudis[3]);
    wrt(37,15,' Diskette auswerfen ');
    at(colmenuinv[3]);
    wrt(37,16,' Drucker sprengen   ');
    at(colmenuhigh[3]);
    wrt(38,13,'K'); wrt(38,14,'P');
    at(colmenuinvhi[3]);
    wrt(38,16,'D');
    mon;
    end;
end;

procedure smsgbox;
begin
  with col do begin
    wwin(40,70,10,15,'Meldung',colmboxrahmen,colmbox);
    mwrt(43,12,'normaler Text');
    at(colmboxhigh);
    mwrt(43,13,'hervorgehobener Text');
    end;
end;

procedure sdialog;
var i : byte;
begin
  with col do begin
    wwin(32,78,10,20,'Eingabefenster',coldiarahmen,coldialog);
    moff;
    wrt(35,14,'Eingabefeld:');
    wrt(35,16,'..mit Auswahlliste:');
    wrt(72,16,#25);
    wrt(42,18,'Schalter');
    at(coldiahigh);
    wrt(35,12,'hervorgehobener Text');
    at(coldiainp); wrt(56,14,' ');
    at(coldiamarked); write('markierte Eingabe');
    at(coldiainp); write('  ');
    wrt(56,16,' Auswahltext Nr');
    at(coldiaarrows); write(#16);
    wwin(56,76,17,21,'',coldiasel,coldiasel);
    at(coldiaselbar);
    for i:=1 to 3 do begin
      wrt(57,17+i,' Auswahltext Nr. ');
      write(i,' ');
      at(coldiasel);
      end;
    at(coldiabuttons);
    wrt(35,18,' [x] ');
    mon;
    end;
end;

procedure sel1;
begin
  with col do begin
    wwin(49,61,10,18,'',colselrahmen,colselbox);
    moff;
    wrt(50,11,' Alles');
    wrt(50,13,' Neues');
    wrt(50,14,' Heute');
    wrt(50,15,' Datum');
    wrt(50,16,' Zeit');
    wrt(50,17,' Sichern');
    at(colselbar);
    wrt(50,12,'�Ungelesen ');
    at(colselhigh);
    wrt(51,11,'A'); wrt(51,13,'N'); wrt(51,14,'H'); wrt(51,15,'D');
    wrt(51,16,'Z'); wrt(51,17,'S');
    mon;
    end;
end;

procedure sel2;
begin
  with col do begin
    wwin(33,78,7,19,'',colsel2rahmen,colsel2box);
    moff;
    wrt(34,8,forms(' AB      Add Backwards',44));
    wrt(34,9,forms(' ACDC    Allow Controller to die peacefully',44));
    wrt(34,10,forms(' AIB     Attack Innocent Bystander',44));
    at(colsel2bar);
    wrt(34,11,forms(' BMP     Branch and Make Popcorn',44));
    at(colsel2box);
    wrt(34,12,forms(' CFP     Change and Forget Password',44));
    wrt(34,13,forms(' CRN     Convert to Roman Numerals',44));
    wrt(34,14,forms(' DCGC    Dump Confusing Garbage to Console',44));
    wrt(34,15,forms(' DCR     Double precision CRash',44));
    wrt(34,16,forms(' DK      Destroy Klingons',44));
    at(colsel2rahmen);
    wrt(33,17,hbar(46));
    at(colbutton);
    wrt(36,18,' Ja ');
    wrt(43,18,'  Nein  ');
    wrt(53,18,'  Vielleicht  ');
    at(colbuttonarr);
    wrt(35,18,#16); wrt(40,18,#17);
    at(colbuttonhigh);
    wrt(37,18,'J'); wrt(45,18,'N'); wrt(55,18,'V');
    mon;
    end;
end;

procedure subox;
begin
  with col do begin
    wwin(35,75,10,18,'Kalender u.ae.',colutility,colutility);
    moff;
    wrt(40,12,'normaler Text');
    at(colutihigh);
    wrt(40,14,'hervorgehobener Text');
    at(colutiinv);
    wrt(40,16,'inverser Text');
    mon;
    end;
end;

procedure shelp;
begin
  with col do begin
    wwin(35,76,12,18,'',colhelp,colhelp);
    moff;
    wrt(37,14,'Dies ist ein ');
    at(colhelphigh); write('hervorgehobener');
    at(colhelp); write(' Hilfstext');
    wrt(37,15,'mit ');
    at(colhelpQVW); write('Querverweis');
    at(colhelp); write(' und ');
    at(colhelpslQVW); write('gewaehltem');
    at(colhelp); write(' Querver-');
    wrt(37,16,'weis.');
    mon;
    end;
end;

procedure slister;
var i, len :integer;
begin
  len:=Screenwidth-80+50;
  with col do begin
    at(colliststatus);
    moff;
    wrt(31,1,forms('    1    20       '#31'    F1-Hilfe',len));
    at(collistheader);
    wrt(31,2,forms('Empfaenger : /Z-NETZ/TELECOM/POINTS ',len));
    wrt(31,3,forms('Betreff    : NEU: CrossPoint v2.0   ',len));
    wrt(31,4,forms('Absender   : P.MANDRELLA@SPS-HH.ZER ',len));
    wrt(31,5,forms('Datum      : Fr 06.11.92, 00:11     ',len));
    wrt(31,6,forms(dup(len,'-'),len));
    at(collisttext);
    clwin(31,ScreenWidth,7,screenlines);
    wrt(31,18,'- normaler Text -');
    wrt(31,24,'markiertes');
    wrt(47,25,'Wort');
    for i:=9 downto 1 do begin
      at(collistquote[i]);
      wrt(31,17-i,forms(dup(i,'>')+' Quote Ebene '+strs(i)+'  (hervorgehoben)',len-1));
      at(col.collistqhigh[i]);
      wrt(48+i,17-i,'hervorgehoben');
      end;
    at(collistselbar);
    wrt(31,20,forms('- Auswahlbalken -',len-1));
    at(collistmarked);
    wrt(31,22,forms('- markierte Zeile -',len));
    at(collistfound);
    wrt(42,24,'Wort');
    at(collisthigh);
    wrt(31,25,'hervorgehobenes');
    at(collistsignatur);
    wrt(31,27,'--');
    wrt(31,28,'Signatur');
    at(collistscroll);
    for i:=2 to 5 do wrt(ScreenWidth,i,'�');
    for i:=6 to 10 do wrt(ScreenWidth,i,'�');
    for i:=11 to screenlines do wrt(ScreenWidth,i,'�');
    mon;
    end;
end;

procedure seditor;
  var len :integer;
begin
  len:=Screenwidth-80+50;
  with col do begin
    at(coledithead);
    moff;
    wrt(31,1,forms(' Nachricht an  SYSOP@MEINEBOX.ZER           21:13 ',len));
    wrt(31,2,forms(' Betreff:      Testnachricht                      ',len));
    at(coleditstatus);
    wrt(31,3,forms(' Z 1       S 1           155k         G:\1932.TMP ',len));
    at(coledittext);
    clwin(31,ScreenWidth,4,screenlines);
    wrt(31,7,'- normaler Text -');
    at(coleditquote[3]);
    wrt(31,9,'>>> Quote Ebene 3');
    at(coleditquote[2]);
    wrt(31,10,'>> Quote Ebene 2');
    at(coleditquote[1]);
    wrt(31,11,'> zitierter Text (Quote)');
    at(coleditmarked);
    wrt(31,13,'- markierter Text -');
    at(coleditendmark);
    wrt(31,14,#4);
    at(coleditmenu);
    wrt(57,7, '� Menue ����������Ŀ');
    wrt(57,8, '� Kopieren       * �');
    wrt(57,9, '� Ausschneiden   - �');
    wrt(57,10,'� Einfuegen      + �');
    wrt(57,11,'� Laden        ^KR �');
    wrt(57,12,'� Speichern    ^KW �');
    wrt(57,13,'������������������Ĵ');
    wrt(57,14,'� Umbruch aus   F3 �');
    wrt(57,15,'� Umbruch ein   F4 �');
    wrt(57,16,'������������������Ĵ');
    wrt(57,17,'� Optionen         �');
    wrt(57,18,'� .. sichern       �');
    wrt(57,19,'��������������������');
    wshadow(58,77,8,20);
    at(coleditmenuhi);
    wrt(59,9,'A'); wrt(59,10,'E'); wrt(59,11,'L'); wrt(59,12,'S');
    wrt(59,14,'U'); wrt(60,15,'m'); wrt(59,17,'O'); wrt(62,18,'s');
    at(coleditmenuinv);
    wrt(58,8,' Kopieren       * ');
    at(coledithiinv);
    wrt(59,8,'K');
    mon;
    end;
end;

procedure smailer;
begin
  with col do begin
    wwin(31,78,6,19,'',colmailer,colmailer);
    moff;
    wrt(31,8,hbar(48));
    wrt(31,17,hbar(48));
    wrt(69,7,'00:00:12');
    wrt(33,9,'Senden'); wrt(33,11,'Dateigroesse');
    wrt(33,12,'uebertragen'); wrt(33,14,'Restzeit');
    wrt(57,11,'Blockgroesse'); wrt(57,12,'Durchsatz        cps');
    wrt(57,14,'Fehler');
    at(colmailerhi2);
    wrt(33,7,^P' Klystron BBS, Koblenz (2:243/97)');
    at(colmailerhigh);
    wrt(43,9,'06DDFFEF.SU2'); wrt(44,11,'     9273');
    wrt(44,12,'     4096'); wrt(45,14,'00:00:03');
    wrt(67,9,'CRC-32'); wrt(68,11,' 1024');
    wrt(68,12,' 1588'); wrt(68,14,'    0');
    wrt(33,16,dup(20,'�'));
    mon;
    end;
end;

procedure skeys;
begin
  showcol;
  wrkeys(true);
end;


var
  menup : array[0..20] of shortint = (1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);

function readmenu(nr:byte):shortint;
const x     = 1;
      y     = 3;
var ma     : map;
    i,n,ml : integer;
    t      : taste;
    p      : shortint;
    ms     : string;

  procedure display;
  var i : integer;
  begin
    moff;
    for i:=1 to n do
      with ma^[i] do
      begin
        if i=p then
          attrtxt($70)
        else
          attrtxt(7);

        Wrt(x, y+i-1, '  ' + LeftStr(mstr,max(0,hpos-1)));
        if hpos>0 then
        begin
          if i<>p then attrtxt(15);
          Wrt2(mstr[hpos]);
          if i=p then attrtxt($70) else attrtxt(7);
        end;
        Wrt2(forms(Mid(mstr,hpos+1),ml+2-hpos));
      end;
    mon;
  end;

  procedure maus_bearbeiten;
  var mx,my : integer;
      inside: boolean;
  begin
    maus_gettext(mx,my);
    inside:=(mx>=x) and (mx<=x+ml+3) and (my>=y) and (my<=y+n-1);
    if inside then
      if (t=mausleft) or (t=mauslmoved) then p:=my-y+1
      else if t=mausunleft then t:=keycr
      else if t=mausunright then t:=keyesc
      else
    else
      if t=mausunright then t:=keyesc;
  end;

begin
  ms:=GetRes2(201,nr);
  menu[25]:='X,'+ms;
  new(ma);
  splitmenu(25,ma,n,false);
  ml:=0;
  for i:=1 to n do
    ml:=max(ml,length(ma^[i].mstr));
  p:=menup[nr];
  attrtxt(7);
  clwin(1,28,3,screenlines);
  repeat
    display;
    get(t,curoff);
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    for i:=1 to n do
      if UpCase(t[1])=ma^[i].hkey then begin
        p:=i; t:=keycr;
        display;
        end;
    if t=keyup then
      if p=1 then p:=n
      else dec(p);
    if t=keydown then
      if p=n then p:=1
      else inc(p);
    if (t=keyhome) or (t=keypgup) then p:=1;
    if (t=keyend) or (t=keypgdn) then p:=n;
  until (t=keycr) or (t=keyesc);
  menup[nr]:=p;
  dispose(ma);
  if t=keyesc then readmenu:=0
  else readmenu:=p;
end;


var ssp:procedure;


procedure cset(var b:byte);
var y,ax,xp,yp : shortint;
    t          : taste;
    mb,nn      : byte;

  {$R-}
  procedure display;
  const s1 : string = '���Ŀ';
        s2 : string = '�';
        s3 : string = '�����';
        s0 : string = '                           ';
        sx : string = ' * ';
  var i,j,x3,xx : byte;
  begin
    attrtxt(7);
    moff;
    fwrt(2,y-1,s0);
    fwrt(2,screenlines,s0);
    for i:=0 to 15 do begin    { Zeichne Farbpalette}
      xx:=3;
      wrt(2,y+i,' ');
      for j:=0 to 7 do begin
        attrtxt(16*(j+ax)+i);
        fwrt(xx,y+i,sx);
        inc(xx,3);
        end;
      attrtxt(7);
      wrt(27,y+i,' ');
      end;
    at(15);
    x3:=3*xp+2;
{$IFDEF NCRT }
    FWrt(x3,y+yp-1,s1);
    FWrt(x3,y+yp,s2);
    FWrt(x3+4,y+yp,s2);
    FWrt(x3,y+yp+1,s3);
{$ELSE }
    sdisp(x3,y+yp-1,s1);
    sdisp(x3,y+yp,s2);
    sdisp(x3+4,y+yp,s2);
    sdisp(x3,y+yp+1,s3);
{$ENDIF }
    wrt(1,y+7,iifc(ax>0,#17,' '));
    wrt(28,y+7,iifc(ax+7<nn,#16,' '));
    mon;
  end;
{$IFDEF Debug }
  {$R+}
{$ENDIF }

  procedure maus_bearbeiten;
  var xx,yy  : integer;
      inside : boolean;
  begin
    maus_gettext(xx,yy);
    inside:=(xx>=3) and (xx<=26) and (yy>=y) and (yy<=y+15);
    if t=mausunright then
      t:=keyesc
    else if inside then begin
      if (t=mausleft) or (t=mauslmoved) then begin
        xp:=(xx-3)div 3;
        yp:=(yy-y);
        end;
      if t=mausldouble then
        t:=keycr;
      end
    else if ((t=mausleft) or (t=mausldouble)) and (yy>=y) and (yy<=y+15)
            and (xx<30) then begin
      if xx<3 then
        if ax>0 then begin
          dec(ax);
          if xp<7 then inc(xp);
          end
        else
          if xp>0 then dec(xp)
        else
      else if (xx>=27) and (xx<=28) then
        if ax<nn-7 then begin
          inc(ax);
          if xp>0 then dec(xp);
          end
        else
          if xp<7 then inc(xp);
      end
    else if t=mausleft then
      t:=keycr;
  end;

begin
  y:=screenlines-16;
  attrtxt(7);
  clwin(1,27,y-1,y+16);
  nn:=15;
  yp:=b and 15;
  xp:=b div 16;
  if xp>7 then begin
    ax:=min(8,xp-4); dec(xp,ax);
    end
  else
    ax:=0;
  mb:=b;
  pushhp(53);
  repeat
    b:=(16*(xp+ax)+yp);
    ssp;
    display;
    repeat
      get(t,curoff)
    until (t<>mausmoved) and (t<>mausrmoved);
    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;
    if t=keyup then
      if yp>0 then dec(yp) else yp:=15;
    if t=keydown then
      if yp<15 then inc(yp) else yp:=0;
    if t=keyhome then yp:=0;
    if t=keyend then yp:=15;
    if t=keyleft then
      if xp>0 then dec(xp)
      else if ax>0 then dec(ax);
    if t=keyrght then
      if xp<7 then inc(xp)
      else if ax<nn-7 then inc(ax);
    if t=keyclft then begin
      ax:=0; xp:=0;
      end;
    if t=keycrgt then begin
      ax:=nn-7; xp:=nn-ax;
      end;
    if (t=keytab) and (nn>7) then
      if ax<nn-7 then ax:=nn-7
      else ax:=0;
  until (t=keycr) or (t=keyesc);
  pophp;
  if t=keyesc then begin
    b:=mb; ssp; end;
end;

procedure CfgColors;
var i     : integer;
    n,m,l : shortint;
    p: ScrPtr;
begin
  // wpushs(1,ScreenWidth,1,ScreenLines,'-'); { Ganzen Screen sichern, ohne Rahmen }
  sichern(p);
  m2t:=false;
  attrtxt(7);
  moff;
  clrscr;
  for i:=1 to screenlines do wrt(29,i,'�');
  attrtxt($70);
  wrt(1,1,center(getres(200),27));
  mon;
  pushhp(52);
  repeat
    ssp:=showcol; ssp;
    n:=readmenu(0);
    with col do
    case n of
       1 : repeat    { Hauptmeue }
             ssp:=showmenus0; ssp;
             m:=readmenu(1);
             case m of
               1 : cset(colmenu[0]);
               2 : cset(colmenuhigh[0]);
               3 : cset(colmenuinv[0]);
               4 : cset(colmenuinvhi[0]);
               5 : cset(colmenudis[0]);
               6 : cset(colmenuseldis[0]);
             end;
           until m=0;
   2,3,4 : begin     { Untermenue 1.-3. Ebene }
             showmenus0;
             ssp:=showmenus1;
             if n>2 then begin
               ssp; ssp:=showmenus2;
               if n>3 then begin
                 ssp; ssp:=showmenus3;
                 end;
               end;
             repeat
               ssp;
               m:=readmenu(n);
               case m of
                 1 : cset(colmenu[n-1]);
                 2 : cset(colmenuhigh[n-1]);
                 3 : cset(colmenuinv[n-1]);
                 4 : cset(colmenuinvhi[n-1]);
                 5 : cset(colmenudis[n-1]);
                 6 : cset(colmenuseldis[n-1]);
               end;
             until m=0;
           end;
       5 : repeat    { F-Tasten }
             ssp:=skeys; ssp;
             m:=readmenu(5);
             case m of
               1 : cset(colkeyshigh);
               2 : cset(colkeys);
               3 : cset(colkeysacthi);
               4 : cset(colkeysact);
             end;
           until m=0;
       6 : repeat    { Verschiedenes }
             ssp;
             m:=readmenu(6);
             case m of
               1 : cset(coltline);
               2 : cset(colarcstat);
               3 : cset(colmapsbest);
             end;
           until m=0;
       7 : repeat    { Bretter/User }
             ssp;
             m:=readmenu(7);
             case m of
               1 : cset(colbretter);
               2 : cset(colbretterinv);
               3 : cset(colbretterhi);
               4 : cset(colbrettertr);
             end;
           until m=0;
       8 : repeat    { Nachrichten }
             ssp:=showmsgs;
             ssp;
             m:=readmenu(8);
             case m of
               1 : cset(colmsgs);
               2 : cset(colmsgshigh);
               3 : cset(colmsgsinv);
               4 : cset(colmsgsinfo);
               5 : cset(colmsgsuser);
               6 : cset(colmsgsinvuser);
               7 : cset(colmsgsprio1);
               8 : cset(colmsgsprio2);
               9 : cset(colmsgsprio4);
              10 : cset(colmsgsprio5);
             end;
           until m=0;
       9 : repeat    { Message-Box }
             ssp:=smsgbox; ssp;
             m:=readmenu(9);
             case m of
               1 : cset(colmbox);
               2 : cset(colmboxrahmen);
               3 : cset(colmboxhigh);
             end;
           until m=0;
      10 : repeat    { Dialog-Box }
             ssp:=sdialog; ssp;
             m:=readmenu(10);
             case m of
               1 : cset(coldialog);
               2 : cset(coldiarahmen);
               3 : cset(coldiahigh);
               4 : cset(coldiainp);
               5 : cset(coldiamarked);
               6 : cset(coldiaarrows);
               7 : cset(coldiasel);
               8 : cset(coldiaselbar);
               9 : cset(coldiabuttons);
             end;
           until m=0;
      11 : repeat    { Select-Box 1 }
             ssp:=sel1; ssp;
             m:=readmenu(11);
             case m of
               1 : cset(colselbox);
               2 : cset(colselrahmen);
               3 : cset(colselhigh);  { z.Zt. nicht verwendet }
               4 : cset(colselbar);
             end;
           until m=0;
      12 : repeat    { Select-Box 2 }
             ssp:=sel2; ssp;
             m:=readmenu(12);
             case m of
               1 : cset(colsel2box);
               2 : cset(colsel2rahmen);
               3 : cset(colsel2high);
               4 : cset(colsel2bar);
             end;
           until m=0;
      13 : repeat    { Buttons }
             ssp:=sel2; ssp;
             m:=readmenu(13);
             case m of
               1 : cset(colbutton);
               2 : cset(colbuttonhigh);
               3 : cset(colbuttonarr);
             end;
           until m=0;
      14 : repeat    { Utilities }
             ssp:=subox; ssp;
             m:=readmenu(14);
             case m of
               1 : cset(colutility);
               2 : cset(colutihigh);
               3 : cset(colutiinv);
             end;
           until m=0;
      15 : repeat    { Hilfe }
             ssp:=shelp; ssp;
             m:=readmenu(15);
             case m of
               1 : cset(colhelp);
               2 : cset(colhelphigh);
               3 : cset(colhelpQVW);
               4 : cset(colhelpslQVW);
             end;
           until m=0;
      16 : repeat    { Lister }
             ssp:=slister; ssp;
             m:=readmenu(16);
             case m of
               1 : cset(collistheader);
               2 : cset(collistheaderhigh);
               3 : cset(collisttext);
               4 : cset(collistselbar);
               5 : cset(collistmarked);
               6 : cset(collistfound);
               7 : cset(colliststatus);
               8 : cset(collistscroll);
               9 : cset(collisthigh);
              10 : repeat
                     ssp;
                     l:=readmenu(19);
                     if l>0 then cset(collistquote[l]);
                   until l=0;
              11 : repeat
                     ssp;
                     l:=readmenu(19);
                     if l>0 then cset(collistqhigh[l]);
                   until l=0;
              12 : cset(collistsignatur);
             end;
           until m=0;
      17 : repeat    { Editor }
             ssp:=seditor; ssp;
             m:=readmenu(17);
             case m of
               1 : cset(coledithead);
               2 : cset(coleditstatus);
               3 : cset(coledittext);
               4 : repeat
                     ssp;
                     l:=readmenu(19);
                     if l>0 then cset(coleditquote[l]);
                   until l=0;
               5 : cset(coleditmarked);
               6 : cset(coleditendmark);
               7 : cset(coleditmenu);
               8 : cset(coleditmenuhi);
               9 : cset(coleditmenuinv);
              10 : cset(coledithiinv);
             end;
           until m=0;
      18 : repeat    { Mailer }
             ssp:=smailer; ssp;
             m:=Readmenu(18);
             case m of
               1 : cset(colmailer);
               2 : cset(colmailerhigh);
               3 : cset(colmailerhi2);
             end;
           until m=0;
      19 : if ReadJN(getres(242),true) then   { 'Standard-Farbeinstellungen wiederherstellen' }
             defaultcolors;
      20 : savecolors;
      21 : n:=-1;
    end;
  until n<=0;
  freeres;
  pophp;
  setcolors;
  m2t:=true;
  showscreen(false);
  aufbau:=true;
  menurestart:=(n=0);
  // wpop;
  holen(p);
end;

{
  $Log$
  Revision 1.37  2003/09/11 22:30:07  mk
  - added special color for signatures

  Revision 1.36  2003/08/23 20:17:10  mk
  - allow longer program name in Zusatzmen�
  - fixed little display problem in Zusatzmen�

  Revision 1.35  2002/12/12 11:58:44  dodi
  - set $WRITEABLECONT OFF

  Revision 1.34  2002/12/06 14:27:28  dodi
  - updated uses, comments and todos

  Revision 1.33  2002/07/25 20:43:54  ma
  - updated copyright notices

  Revision 1.32  2002/02/21 13:52:32  mk
  - removed 21 hints and 28 warnings

  Revision 1.31  2002/01/30 17:18:13  mk
  - do not create fkeys record dynamically, because the record containts
    ansistrings and FPC has problems with array->pointer of record with
    ansistrings

  Revision 1.30  2002/01/13 15:07:27  mk
  - Big 3.40 Update Part I

  Revision 1.29  2001/09/10 15:58:02  ml
  - Kylix-compatibility (xpdefines written small)
  - removed div. hints and warnings

  Revision 1.28  2001/08/03 21:40:43  ml
  - compilable with fpc (linux)

  Revision 1.27  2001/07/28 12:04:11  mk
  - removed crt unit as much as possible

  Revision 1.26  2001/07/23 16:05:18  mk
  - added some const parameters
  - changed most screen coordinates from byte to integer (saves some kb code)

  Revision 1.25  2001/03/13 19:24:56  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.24  2000/10/17 10:05:48  mk
  - Left->LeftStr, Right->RightStr

  Revision 1.23  2000/08/17 13:33:06  mk
  MO:- Farbsetup fuer Screen > 80 Zeichen angepasst

  Revision 1.22  2000/07/27 13:41:50  mk
  - weitere Anpassungen um Spaltenzahlen groesser 80 zu nutzen

  Revision 1.21  2000/07/27 10:13:01  mk
  - Video.pas Unit entfernt, da nicht mehr noetig
  - alle Referenzen auf redundante ScreenLines-Variablen in screenLines geaendert
  - an einigen Stellen die hart kodierte Bildschirmbreite in ScreenWidth geaendert
  - Dialog zur Auswahl der Zeilen/Spalten erstellt

  Revision 1.20  2000/07/20 16:49:57  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.19  2000/07/11 21:39:21  mk
  - 16 Bit Teile entfernt
  - AnsiStrings Updates
  - ein paar ASM-Routinen entfernt

  Revision 1.18  2000/07/07 11:00:33  hd
  - AnsiString
  - Fix: JumpSection/JumpKey in xpcfg.pas, Zugriffsverletzung

  Revision 1.17  2000/07/06 08:58:45  hd
  - AnsiString

  Revision 1.16  2000/07/04 12:04:22  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.15  2000/06/29 13:00:55  mk
  - 16 Bit Teile entfernt
  - OS/2 Version l�uft wieder
  - Jochens 'B' Fixes �bernommen
  - Umfangreiche Umbauten f�r Config/Anzeigen/Zeilen
  - Modeminitialisierung wieder an alten Platz gelegt
  - verschiedene weitere fixes

  Revision 1.14  2000/06/23 15:59:18  mk
  - 16 Bit Teile entfernt

  Revision 1.13  2000/06/22 19:53:31  mk
  - 16 Bit Teile ausgebaut

  Revision 1.12  2000/05/19 19:48:54  mk
  - C/A/F baut jetzt den Bildschirm wieder korrekt auf

  Revision 1.11  2000/05/06 17:29:21  mk
  - DOS DPMI32 Portierung

  Revision 1.10  2000/05/04 18:43:16  jg
  - Lister: eigene Headerfarbe fuer hervorgehobene Nachrichten
    entsprechender Menuepunkt unter Config/Anzeige/Farben/Lister

  Revision 1.9  2000/05/02 19:14:00  hd
  xpcurses statt crt in den Units

  Revision 1.8  2000/04/29 14:01:00  jg
  - Config/Anzeige/Farbe beachtet "Menue bei ESC verlassen" Einstellung.

  Revision 1.7  2000/04/28 14:52:52  jg
  - Einzeln konfigurierbare Farben fuer Prioritaeten 1,2,4 und 5
    Bits 3-5 im Mbase-Eintrag "Flags" werden hierfuer benutzt !

  Revision 1.6  2000/03/20 11:26:21  mk
  - SDisp-Routine teilweise nach Win32 portiert

  Revision 1.5  2000/02/15 20:43:36  mk
  MK: Aktualisierung auf Stand 15.02.2000

}
end.

