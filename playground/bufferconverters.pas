{  $Id$

   OpenXP message buffer converters
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

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

{ OpenXP message buffer converters }
unit BufferConverters;


procedure ZtoFido(source,dest:string; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
procedure ZtoMaggi(source,dest:string; pronet:boolean; screen:byte);
procedure MaggiToZ(source,dest:string; pronet:boolean; screen:byte);
procedure ZtoQuick(source,dest:string; gs:boolean; screen:byte);
procedure QuickToZ(source,dest:string; gs:boolean; screen:byte);
procedure ZtoMaus(source,dest:string; screen:byte);
procedure MausToZ(source,dest:string; screen:byte);


implementation

procedure ZtoFido(source,dest:string; ownfidoadr:string; screen:byte;
                  addpkts:addpktpnt; alias:boolean);
var d         : DB;
    akas      : string;
    BoxName       : string;
    orgdest   : string;
    bfile     : string;
    p,i       : byte;
    t         : text;
    bpsave    : BoxPtr;
    sout      : string;

  procedure Convert;
  const
    pc: array[false..true] of string = ('', '1A');
  var
    fnet: integer;
    f: boolean;
  begin
    with BoxPar^ do
    begin
      f:=OutFilter(source);
      if (f4d or alias) then
        fnet:= -1
      else
        fnet:= fPointNet;

      DoZFido(1,                           { Richtung ZC->FTS }
              MagicBrett,                       { Basisebene }
              source,                           { Quelldatei }
              sout+dest,                        { Zieldatei }
              OwnFidoAdr,                       { Absender }
              boxname,                          { Empfaenger }
              fnet,                             { FakeNet }
              passwort,                         { Paketpassword }
              pc[(f4d or alias) and fTosScan],  { PC aendern wg. TosScan? }
              LocalINTL,                        { INTL }
              false,                            { Keep VIA }
              true,                             { Requests }
              false,1,1);                           { Leere loeschen? }
    end;
      if f then _era(source);
  end;

begin { ZtoFido }
  Debug.DebugLog('xpnetcall','converting ZC to fido',DLInform);
  sout:=Boxpar^.sysopout;
  Convert;
  orgdest:=dest;
  akas:=Boxpar^.SendAKAs;
  assign(t,'ZFIDO.CFG');
  rewrite(t);
  writeln(t,'# ',getres(721));    { 'Temporäre Fido-Konfigurationsdatei' }
  writeln(t);
  writeln(t,'Bretter=',BoxPar^.boxname,' ',boxpar^.MagicBrett);
  addpkts^.anzahl:=0;
  addpkts^.akanz:=0;
  if akas<>'' then begin
    dbOpen(d,BoxenFile,1);
    bpsave:=boxpar;
    new(boxpar);
    repeat
      p:=blankpos(akas);
      if p=0 then p:=length(akas)+1;
      if p>3 then begin
        BoxName:=LeftStr(akas,p-1);
        akas:=trim(mid(akas,p));
        dbSeek(d,boiName,UpperCase(BoxName));
        if not dbfound then begin
          Debug.DebugLog('xpnetcall','box is no server BoxName: "'+BoxName+'"',DLError);
          rfehler1(733,BoxName);         { 'Ungültiger AKA-Eintrag - %s ist keine Serverbox!' }
          end
        else begin
          Debug.DebugLog('xpnetcall','reading BoxName parameters',DLInform);
          ReadBoxPar(nt_Fido,BoxName);
          writeln(t,'Bretter=',BoxName,' ',boxpar^.magicbrett);
          if addpkts^.akanz<maxaddpkts then begin   { !! }
            inc(addpkts^.akanz);
            addpkts^.akabox[addpkts^.akanz]:=BoxName;
            addpkts^.reqfile[addpkts^.akanz]:='';
            end;
          bfile:=dbReadStr(d,'dateiname');
          if FileExists(bfile+BoxFileExt) then begin
            alias:=(dbReadInt(d,'script') and 4<>0);
            with BoxPar^ do
              if alias then
                OwnFidoAdr:=LeftStr(boxname,cpos('/',boxname))+pointname
              else
                OwnFidoAdr:=boxname+'.'+pointname;
            source:=bfile+BoxFileExt;
            dest:=formi(ival(LeftStr(dest,8))+1,8)+'.PKT';
            Convert;
            if FileExists(sout+dest) then begin
              inc(addpkts^.anzahl);
              addpkts^.addpkt[addpkts^.anzahl]:=dest;
              addpkts^.abfile[addpkts^.anzahl]:=bfile;
              addpkts^.abox[addpkts^.anzahl]:=BoxName;
              end;
            end;   { exist .PP }
          end;   { BoxName found }
        end;
    until (p<=3) or (addpkts^.anzahl=maxaddpkts);
    dbClose(d);
    if bpsave^.uparcer<>'' then          { falls gepackte Mail }
      bpsave^.uparcer:=boxpar^.uparcer;
    dispose(boxpar);
    boxpar:=bpsave;
    dest:=orgdest;
    for i:=1 to addpkts^.anzahl do
      dest:=dest+' '+addpkts^.addpkt[i];
    exchange(boxpar^.uparcer,'$PUFFER',dest);
    end
  else Debug.DebugLog('xpnetcall','no akas',DLWarning);
  Debug.DebugLog('xpnetcall','converting to fido finished',DLInform);
  close(t);
end;


procedure ZtoMaggi(source,dest:string; pronet:boolean; screen:byte);
var c : string[10];
    f : boolean;
begin
  f:=OutFilter(source);
  if pronet then DelPronetfiles;
  if pronet then c:='-zp'
  else c:='-zm'+iifs(msgids,' -m','');
  with BoxPar^ do
    shell('MAGGI.EXE '+c+' -n'+MagicNET+' '+source+' '+dest+' '+box+'.BL',400,screen);
  if f then _era(source);
end;

procedure MaggiToZ(source,dest:string; pronet:boolean; screen:byte);
var c : string[10];
begin
  if pronet then c:='-pz'
  else c:='-mz';
  with BoxPar^ do
    shell('MAGGI.EXE '+c+' -n'+MagicNET+' '+source+' '+dest+' '+box+'.BL',400,
          screen);
end;

procedure ZtoQuick(source,dest:string; gs:boolean; screen:byte);
var f : boolean;
begin
  f:=OutFilter(source);
  shell('MAGGI.EXE -zq '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
  if f then _era(source);
end;

procedure QuickToZ(source,dest:string; gs:boolean; screen:byte);
begin
  shell('MAGGI.EXE -qz '+iifs(gs,'-g ','')+source+' '+dest,300,screen);
end;

procedure ZtoMaus(source,dest:string; screen:byte);
var opt : string[10];
    f   : boolean;
begin
  MakeMimetypCfg;
  f:=OutFilter(source);
  if MausPSA then opt:=''
  else opt:='-psa ';
  if not boxpar^.Brettmails then opt:=opt+'-on ';
  with BoxPar^ do
    shell('MAGGI.EXE -zs '+opt+'-b'+box+' -h'+MagicBrett+' -i -it '+
          iifs(maxmaus,'-mm ','')+source+' '+dest,300,screen);
  if f then _era(source);
end;

procedure MausToZ(source,dest:string; screen:byte);
begin
  with BoxPar^ do
    shell('MAGGI.EXE -sz -b'+box+' -h'+MagicBrett+' -it '+source+' '+dest,
          600,screen);
end;

end.

{
 $log$
}