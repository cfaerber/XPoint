{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ Boxname.ITI laden. Wird von XPMAUS, XP8 und MAGGI verwendet. }

{$I XPDEFINE.INC}
{$IFDEF BP }
  {$O+,F+}
{$ENDIF }

unit  xp_iti;

interface

uses  typeform,fileio, xpglobal;

const MaxMausInfos = 200;

type  MausInfRec = record
                     ID        : string[5];    { aus ITI }
                     text      : string[40];   { aus ITI }
                     crcflag   : boolean;      { aus ITI }
                     netflag   : char;         { aus ITI }
                     intervall : shortint;     { aus Boxname.INF }
                     crc       : longint;      { aus Boxname.INF }
                     lastdate  : datetimest;   { aus Boxname.INF }
                     edflag    : boolean;      { fÅr xpmaus.MausEditInfos }
                   end;
      MausInfArr = array[1..MaxMausInfos] of MausInfRec;
      MausInfAP  = ^MausInfArr;


procedure MausReadITI(const bfile:string; info:MausInfAP; var infos:integer);


implementation  { --------------------------------------------------- }


procedure SetDefaults(info:MausInfAP);
var i : integer;
begin
  fillchar(info^,sizeof(info^),0);
  for i:=1 to MaxMausInfos do begin
    info^[i].intervall:=0;
    info^[i].lastdate:='01.01.1980';
    info^[i].crc:=-1;
    info^[i].crcflag:=true;
    info^[i].netflag:='L';
    end;
end;


procedure MausBuildInfolist(info:MausInfAP; var infos:integer);

  procedure add(id,text:string);
  begin
    inc(infos);
    info^[infos].id:=id;
    info^[infos].text:=text;
  end;

begin
  infos:=0;
  add('IIE','EinfÅhrung in die MAUS');
  add('IIB','Bedienungsanleitung');
  add('IIA','MenÅ-Kurzanleitung');
  add('IIG','Gruppen-Anleitung');
  add('IIH','Hardware der MAUS');
  add('III','Technische Informationen');
  add('IIM','MAUS-Beitrag');
  add('IIL','Login-Zeiten');
  add('IIT','MausTausch-Anleitung');
  add('IIK','Kommerzielles');
  add('IIP','PM-Manifest');
  add('IIN','Nutzungsbedingungen');
  add('INA','MausNet-Anleitung');
  add('INK','Kurze Boxen-Liste');
  add('INL','Lange Boxen-Liste');
  add('ING','Netzgruppen-Liste');
  add('INP','Netzplan');
  add('IGT','Box-Vorspann');
  add('IGE','Box-Abspann');
  add('IGS','Spruch des Tages');
  add('IGK','Kurze Gruppenliste');
  add('IGL','Lange Gruppenliste');
  add('ITG','Maschinenlesbare Gruppenliste');
  add('ITI','Maschinenlesbare Infoliste');
end;


procedure MausReadITI(const bfile:string; info:MausInfAP; var infos:integer);
var t : text;
    s : string;
begin
  infos:=0;
  SetDefaults(info);
  assign(t,bfile+'.iti');
  if not existf(t) then
    MausBuildInfolist(info,infos)
  else begin
    reset(t);
    s:='';
    repeat
      while not eof(t) and (firstchar(s)<>'#') do
        readln(t,s);
      if not eof(t) then begin
        inc(infos);
        info^[infos].ID:=ustr(mid(s,2));
        repeat
          readln(t,s);
          case firstchar(s) of
            ':' : info^[infos].text:=mid(s,2);
            'F' : begin
                    delfirst(s);
                    while s<>'' do begin
                      case firstchar(s) of
                        'C' : info^[infos].crcflag:=(s[2]='+');
                        'I' : info^[infos].netflag:=s[2];
                      end;                  { 'N'etz, 'L'okal, 'U'ser }
                      delete(s,1,2);
                      end;
                  end;
          end;
        until (firstchar(s)='#') or eof(t);
        if info^[infos].text='' then dec(infos);
        end;
    until eof(t) or (infos=MaxMausInfos);
    close(t);
    end;
end;


end.

