{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ CrossPoint - HauptmenÅ }

{ variable MenÅpunkte setzen }
procedure enabledisable;
const Nachricht = 3;     { /Nachricht }
      Unvers    = 4;     { Nachricht / Unversandt }
      Suchen    = 5;     { Nachricht / Suchen }
      Wartung   = 7;     { /Wartung }
      Anzeige   = 9;     { Config / Anzeige }
      Export    = 36;    { XPoint / Export }
      Extrakt   = 12;    { Nachricht / Extrakt }
      Alle      = 14;    { Nachricht / Alle }
      Edit      = 16;    { /Edit }
      Maps      = 19;    { Nachricht / Brettmanager }
      Weiter    = 21;    { Nachricht / Weiterleiten }
      Fileliste = 27;    { Fido / Filelisten }
      Sonstiges = 33;    { Wartung / Sonstiges }
      PGP       = 40;    { Nachricht / PGP }
var
    unversandt : byte;
    nachrichten: boolean;
    uvse,uvsl  : boolean;
    mtyp       : char;
    _brett     : string[5];

begin
  nachrichten:= (dispmode>=10) and (dispmode<=19);
  setenable(Nachricht,1,not markaktiv or (dispmode=11));  { Suchen }
  setenable(Nachricht,2,(dispmode<>1) and (dispmode<>2) and not empty);
  setenable(Nachricht,4,not empty and nachrichten);
  setenable(Nachricht,5,not empty and nachrichten);
  setenable(Nachricht,6,not empty and nachrichten);

  uvse:=true; uvsl:=true;
  setenable(Weiter,10,true);
  if (dispmode=10) and not empty then begin
    dbReadN(mbase,mb_brett,_brett);
    if _brett[1]='U' then setenable(Weiter,10,false);    { PM-Archiv }
    dbReadN(mbase,mb_unversandt,unversandt);
    dbReadN(mbase,mb_typ,mtyp);
    if unversandt and 1=0 then begin
      uvsl:=false; uvse:=false; end
    else
      if mtyp='B' then uvse:=false;
    end;
  if empty or (dispmode<10) or (dispmode>19) then begin
    uvse:=false; uvsl:=false; end;
  setenable(Unvers,1,uvse);
  setenable(Unvers,2,uvsl);
  setenable(Unvers,3,uvsl);
  setenable(Unvers,5,dispmode<>11);
  setenable(Wartung,1,dispmode<>11);
  setenable(Wartung,9,dispmode<>12);
  setenable(Anzeige,5,not ParLCD);    { Config/Anzeige/Zeilen }

  setenable(Extrakt,1,nachrichten);
  setenable(Extrakt,2,nachrichten);
  setenable(Extrakt,3,dispmode<>11);
  setenable(Extrakt,4,dispmode=12);
  setenable(Extrakt,7,nachrichten);
  setenable(Suchen,6,markanz<>0);
  setenable(Suchen,7,markanz<>0);
  setenable(Alle,3,dispmode<>11);
  setenable(Alle,4,dispmode<>11);
{  setenable(Alle,5,(dispmode<>10) or (rdmode<>1));  /N/A/Lesen }
  setenable(Maps,6,not empty and nachrichten);    { Brettliste }
  setenable(Fileliste,2,not empty and nachrichten);
  setenable(sonstiges,3,dispmode<>11);

  setenable(Edit,6,dispmode<>20);
  setenable(Export,3,nachrichten);
  setenable(PGP,2,nachrichten and not empty);
  setenable(PGP,3,nachrichten and not empty);
  setenable(PGP,4,nachrichten and not empty);
end;


procedure Reorg_Stat(stat,repair:boolean);
var brk : boolean;
begin
  MsgReorgScan(not stat,repair,brk);
  if stat then signal;
  if not stat and not brk then begin
    MsgReorg;
    if (dispmode>10) and (dispmode<=19) then disprec[1]:=0;
    aufbau:=true;
    end;
end;

procedure Breorg(user,adrbuch:boolean);
begin
  BU_reorg(user,adrbuch);
  signal;
  disprec[1]:=0;
  aufbau:=true;
end;

(*                       { Ausgeklammert, siehe auch xp4o2.pas und xp2.pas }
procedure _SetLanguage;
begin
  SetLanguage;
  setmainkeys(dispmode);
end;
*)

procedure menuopt(t:taste);
const
  lastmenusel : integer = 0;
var m,m1     : integer;
    nix      : string[20];
    adr      : string[20];
    oldlines : byte;
begin
  menurestart:=false;
  repeat
   if (t=keyf4) and (lastmenusel>0) and       { v- MenÅpunkt nicht disabled }
      (menable[lastmenusel div 16] and (1 shl (lastmenusel and 15))=0) then
      m:=lastmenusel
    else begin
      m:=getmenu(0,t,0,0);
      if m>0 then lastmenusel:=m;
      end;
    menurestart:=false;
    mauszuo:=true; mauszuu:=true;
    nix:='';
    m1:=m and 15;
    case m div 16 of
      1 : case m1 of           { XPoint }
            1 : copyright(false);
            5 : terminal(true);
            6 : dosshell;
            8 : if AskSave then quit:=true;
          end;

      2 : case m1 of           { Zusatz }
               1 : kalender;
               2 : FileArcViewer('');
           5..14 : begin
                     getfilename:=_getfilename;
                     isempty:=empty; _p:=p;
                     prog_call(0,m1-4);
                   end;
          end;

      3 : case m1 of           { Nachricht }
            11 : msgdirect;
          end;

      4 : case m1 of           { Unversandt }
            1 : Unversandt(true,true);
            2 : Unversandt(true,false);
            3 : Unversandt(false,false);
            4 : UV_Stat;
            5 : Zeige_Unversandt;
          end;

      5 : case m1 of           { Suchen }
            1 : if Suche(getres(414),'','') then select(11);   { 'Volltext' }
            2 : if Suche(getres(415),'Betreff','') then select(11);  { 'Betreff' }
            3 : if Suche(getres(416),'Absender','') then select(11); { 'User' }
            4 : if Suche(getres(417),'*','') then select(11);  { 'Spezial' }
            5 : SucheWiedervorlage;
            6 : if markanz=0 then
                  rfehler(415)   { 'Keine Nachrichten markiert!' }
                else
                  select(11);
            7 : begin
                  markanz:=0;
                  aufbau:=true;
                end;
 	    8 : if Suche(getres(437),'MsgID','') then select(11); { 'MessageID' }
          end;

      6 : case m1 of           { Config }
            3 : path_config;
            6 : DruckConfig;
            8 : begin EditFkeys(0); menurestart:=true; end;
            9 : CfgSave;
          end;

      7 : case m1 of           { Wartung }
            1 : Reorg_Stat(false,false);
            2 : PackAll(false);
            3 : Breorg(false,false);
            4 : Breorg(true,false);
            5 : Breorg(true,true);
            6 : DupeKill (false);
            8 : ChangePollbox;
          end;

      9 : case m1 of           { Anzeige }
            1 : CfgColors;
            2 : brett_config;  { Bretter }
            3 : NachrichtenanzeigeCfg;
            4 : begin UniEdit(4); menurestart:=true; end;
            6 : MiscAnzeigeCfg;  { Screen-Saver u.a. }
            7 : begin
                  shell('zconfig.exe',200,1);
{$IFDEF Ver32 } { Eine Fehlerabfrage wird nicht gemacht, also Fehler lîschen }
                  doserror := 0;
{$ENDIF }
                  menurestart:=true;
                end;
            8 : AccessibilityOptions;
          end;

     10 : case m1 of           { Import }
            1 : readpuffer;
            2 : Readfremdpuffer;
            3 : ImportUserliste;
            4 : ImportBrettliste;
            6 : ImportYuppiebase;
          end;

     11 : begin                { Anzeige/Zeilen }
            ParZeilen:=0;
            oldlines:=screenlines;
            newscreenlines(m1);
            showscreen(false);
            XPhilite:=0;
            showlastkeys;
            aufbau:=true;
            if screenlines<>oldlines then
              GlobalModified;
          end;

     12 : case m1 of
            1 : extrakt(1,aktdispmode,rdmode);    { Nachricht/Extrakt }
            2 : uudecode;
            7 : mimedecode;
            3 : extrakt(2,aktdispmode,rdmode);
            4 : extrakt(4,aktdispmode,rdmode);
            5 : extrakt(3,aktdispmode,rdmode);
          end;

     14 : case m1 of
            7 : msgall(8,aktdispmode,rdmode);       { Nachricht/Alle }
          else  msgall(m1,aktdispmode,rdmode);
          end;

     15 : case m1 of                          { Netcall }
            1 : EinzelNetcall('');                           { Einzeln }
            2 : AutoTiming(0,true,false);                    { Alle    }
            3 : AnrufStat;                                   { Letzte Anrufe }
            4 : netcall_at('','');                           { Um..    }
            5 : netcall(true,'',false,true,false);           { Relogin }
            6 : netcall(false,'',false,false,false);         { Online  }
            8 : AutoTiming(0,false,false);
            9 : UniEdit(1);
          end;

     16 : case m1 of                                  { Edit }
            2..5 : if UniSel(m1-1,true,'')='' then;
            6    : begin
                     select(20);    { AutoVersand }
                     ShowMain(0);
                   end;
            7    : EditText;
            8    : if UniSel(5,true,'')='' then;
          end;

     17 : case m1 of           { Statistik }
            1 : MemStat;
            2 : Reorg_Stat(true,false);
            3 : DatabaseStat;
            5 : GruppenStat;
            6 : MultiStat(0);
            7 : MultiStat(1);
           10 : FragStat;
          end;

     18 : case m1 of
            1..4 : ModemConfig(m1);
{$IFDEF CAPI }
            5    : IsdnConfig;
{$ENDIF CAPI }
          end;

     19 : case m1 of           { MAPS }
            1 : MapsBrettliste(0);
            2 : if dispmode=0 then
                  if dbEOF(bbase) then  { noch keine Bretter angelegt }
                    MapsDelbrett('U')   { .. erzwingt Listenauswahl }
                  else
                    MapsDelBrett(iifs(bmarkanz=0,dbReadstr(bbase,'brettname'),''))
                else begin
                  dbSeek(bbase,biBrett,ustr(dispspec));
                  MapsDelBrett(dispspec);
                  end;
            3 : MapsCommands(1);
            4 : MapsCommands(0);
            6 : MapsReadList;
            7 : MapsReadFile;
            8 : MausEditInfos;
          end;

     20 : case m1 of           { Fileserver }
            1 : FS_command('',1);
            2 : FS_command('SENDEN',0);
            4 : FS_command('HILFE',0);
            5 : FS_command('',2);
            6 : FS_command('SERVICE',0);
          end;

     21 : case m1 of           { Weiterleiten }
            1     : Weiterleit(1,true);      { Kopie     }
            2     : Weiterleit(7,true);      { Original  }
            3     : Weiterleit(2,true);      { EditTo    }
            4     : Weiterleit(3,true);      { QuoteTo   }
            6     : empfang_bestaetigen(nix);
            7     : CancelMessage;
            8     : Weiterleit(4,true);      { Erneut    }
            9     : Weiterleit(5,true);      { Archiv    }
           10     : pm_archiv(false);
         end;

     22 : if not (dispmode in [10..19]) then
            rfehler(442)    { 'Nur in der NachrichtenÅbersicht mîglich.' }
          else
            case m1 of           { éndern }
              1 : ModiBetreff;
              2 : ModiEmpfdatum;
              3 : ModiRot13;
              4 : ModiText;
              5 : ModiTyp;
              6 : ModiGelesen;
              7 : ModiHighlite;
            end;

     23 : begin
            case m1 of
              1..3 : EditFkeys(m1);      { F-Tasten }
              4    : UniEdit(2);         { Makros }
            end;
            menurestart:=true;
          end;

     24 : case m1 of               { Extern }
            1 : xlistoptions;
            2 : editoptions;
            3 : shelloptions;
            4 : ArcOptions;
            5 : pmcOptions;
            6 : PGP_Options;
          { 7 : ViewerOptions;   - nicht mehr verwenden: kînnte noch }
          {                        in keydef.cfg stehen              }
          end;

     26 : GebuehrenZaehler(m1=1);    { Zusatz/GebÅhren }

     27 : case m1 of          { Fileserver-Liste }
            1 : FS_command('FILES',0);
            2 : FS_ReadList(true);
            3 : FS_ReadList(false);
          end;

     28 : case m1 of          { Optionen }
            1 : options;
            2 : UI_options;
            3 : ListOptions;
           13 : Editor_options;   
            4 : msgoptions;
           12 : adroptions;
            6 : netcalloptions;
            9 : EditPassword;
        (* 10 : _SetLanguage; *)
           11 : TerminalOptions;
          end;

     29 : case m1 of          { Brettindex }
            1 : ReorgBrettindex;
            2 : _AlphaBrettindex;
          end;

     30 : case m1 of          { Fido/Nodelisten }
            1 : UniEdit(5);
            2 : ManualDiff;
            3 : SetShrinkNodelist;
            4 : NodelistIndex;
            6 : NodeStatistik;
            7 : NodelistBrowser;
            8 : NodelistSeek;
          end;

     31 : case m1 of          { Fido }
            1 : AutoCrash:=FidoRequest('','');
            4 : begin
                  if netcall(true,'',false,false,true) then;   { Crash   }
                  if (autocrash=' alle ') or (autocrash=CrashTemp) then begin
                    AutoTiming(iif(autocrash=CrashTemp,1,0),false,true);
                    autocrash:='';
                    end;
                end;
            6 : FidoOptions;
          end;

     32 : case m1 of          { Fido/Fileliste }
            1 : ReadFidolist;
            2 : DelFidolist;
            3 : begin
                  adr:=FidoSeekfile;
                  if (adr<>'') and netcall(true,adr,false,false,true) then;
                end;
          end;

     33 : case m1 of          { Wartung / Sonstiges }
            1 : BezugNeuaufbau;
            2 : BezugReadmids;
            3 : Reorg_Stat(true,true);
            4 : bd_setzen(true);     { DatumsbezÅge wiederherstellen }
          end;

     36 : case m1 of           { Export }
            1 : ExportUB(true);
            2 : ExportUB(false);
            3 : MsgAll(7,aktdispmode,rdmode);
            4 : ScreenShot;
          end;

     37 : case m1 of          { Config/Optionen/Netze }
            1 : NetOptions;
            2 : SizeOptions;
            3 : NetEnable;
          end;

     38 : case m1 of          { Fido/Filescan }
            1 : AddFileechos;
            2 : RemoveFileechos;
            3 : FilescanCommands(1);
            4 : FilescanReadlist;
            5 : FilescanReadFile;
            6 : FilescanCommands(0);
          end;

     39 : begin
            case m1 of          { Config/GebÅhren }
              1 : UniEdit(3);
              2 : UniEdit(6);
              3 : begin
                    Pushhp(979);
                    EditFile(FeierDat,false,false,0,false);
                    pophp;
                  end;
              4 : GebuehrOptions;
            end;
            menurestart:=true;
          end;

     40 : case m1 of            { Nachricht/PGP }
            1 : PGP_RequestKey;
            2 : PGP_ImportKey(false);
            3 : PGP_DecodeMsg(false);
            4 : PGP_DecodeMsg(true);
            6 : PGP_EditKey;
            7 : PGP_RemoveID;
          end;

    end;
  until leaveconfig or not menurestart or (t=keyf4);
end;


procedure Maus_Auswertung(ya:shortint; var t:taste);
const lasty  : integer = 0;
      shortp : shortint = 0;
var x,y : integer;
    n   : shortint;

  procedure page;
  begin
    if t=mauslmoved then begin
      if not keypressed then
        if y>lasty then t:=keypgdn
        else if y<lasty then t:=keypgup;
      end
    else
      if y<screenlines div 2 then t:=keypgup
      else t:=keypgdn;
  end;

  function shortkeypos(x:integer):shortint;
  var i : integer;
  begin
    i:=1;
    while (i<=shortkeys) and (x>shortkey[i].keypos+shortkey[i].keylen) do
      inc(i);
    if (i<=shortkeys) and (x>=shortkey[i].keypos-1) and
       (x<=shortkey[i].keypos+shortkey[i].keylen) then
      shortkeypos:=i
    else
      shortkeypos:=0;
  end;

  procedure dispshort(high:boolean);
  var x,c1,c2 : byte;
      ofs     : word;
      ks1,ks2 : byte;
  begin
    c1:=iif(high,col.colkeysact,col.colkeys);
    c2:=iif(high,col.colkeysacthi,col.colkeyshigh);
    with shortkey[shortp] do begin
      if keyspot>0 then begin
        ks1:=keyspot; ks2:=keyspot; end
      else begin
        ks1:=1; ks2:=-keyspot;
        end;
      ofs:=155+2*keypos;
      moff;
      for x:=1 to keylen+2 do
        if (x>ks1) and (x-1<=ks2) then
{$IFDEF BP }
          mem[base:ofs+2*x]:=c2
        else
          mem[base:ofs+2*x]:=c1;
{$ELSE }
 ; { Hier wird von den MenÅpunkten das Attribut geÑndert }
{$ENDIF}
      mon;
      end;
  end;

begin
  maus_gettext(x,y);
  if y=2 then begin
    n:=ShortkeyPos(x);
    if t=mausunleft then begin
      if (n>0) and (n=shortp) then t:=shortkey[n].key;
      if shortp>0 then begin
        dispshort(false); shortp:=0;
        end;
      end else
    if (t=mausleft) or (t=mausldouble) then begin
      if n>0 then begin
        shortp:=n;
        dispshort(true);
        end;
      end else
    if (t=mauslmoved) and (shortp>0) then
      dispshort(n=shortp);
    end
  else begin
    if shortp<>0 then begin
      dispshort(false);
      if t=mausunleft then
        shortp:=0;
      end;

    if (y>=4+ya) and (y<screenlines-1) then begin
      if (t=mausright) {or (t=mausrmoved)} then t:=keyesc else
      if (t=mausleft) or (t=mauslmoved) then
        if (x>2) and (x<79) then p:=y-3-ya else
        page
      else
        if t=mausldouble then
          if (x>2) and (x<79) then
            if dispext and (dispmode in [0,1,2,20]) then
              t:='e'
            else
              t:=keycr
          else page;
      end else
    if t=mausleft then begin
      if y=1 then t:=mainkey(x);
      end;
    end;
  lasty:=y;
  mauskey:=(t<mausfirstkey) or (t>mauslastkey);
end;
{ 
  $Log$
  Revision 1.12  2000/04/14 14:55:35  jg
  - Bugfix: es gab zwei Routinen namens Editoptions...

  Revision 1.11  2000/04/11 16:38:42  jg
  - Config/Optionen/Editor
  - Hilfe der Editoroptionen jetzt kontextsensitiv

  Revision 1.10  2000/03/14 15:15:39  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.9  2000/03/09 23:39:33  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.8  2000/03/08 22:36:33  mk
  - Bugfixes f¸r die 32 Bit-Version und neue ASM-Routinen

  Revision 1.7  2000/03/03 21:12:49  jg
  - Config-Optionen-Sprache ausgeklammert
  - Sprachabfrage bei allererstem Start eingebaut

  Revision 1.6  2000/03/02 18:32:24  mk
  - Code ein wenig aufgeraeumt

  Revision 1.5  2000/02/21 22:48:01  mk
  MK: * Code weiter gesaeubert

  Revision 1.4  2000/02/18 17:28:08  mk
  AF: Kommandozeilenoption Dupekill hinzugefuegt

}
