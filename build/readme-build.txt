Autor: Christian Böttger cb@openxp.de, 2003-10-18

Hier der Ablauf für die build scripts:

(workdir ist hart kodiert auf /home/boettger/openxp/, die Versionen liegen in
/home/boettger/openxp/3.8/openxp/ bzw /home/boettger/openxp/3.9/openxp/ )

mkdailysnap
    |
    +--- snapshot (Parameter 3.8 oder 3.9)
         |
         +--- cvs up -C version.inc;
         |    ./inc_build_nr.pl;
         |    eval $(./get_build_nr.pl)
         |    
         +--- buildxp
         |    |
         |    + eval $(./get_build_nr.pl)
         |    |
         |    +--- makexp
         |         |
         |         + eval $(./get_build_nr.pl)
         |         
         +--- make-rpm.sh
              |
              + eval $(./get_build_nr.pl)

mkdailysnap:
 prüft per diff, ob sich im CVS gegenüber -1Day was geändert hat, wobei
  Änderungen an version.inc ignoriert werden
 läuft als root, wird von cron aufgerufen

snapshot:
 wird mit Parameter 3.8 oder 3.9 aufgerufen, um ins passende workdir
  gehen zu können
 holt die version.inc aus dem CVS
 erhöht version.inc durch Aufruf von inc_build_nr.pl
 schiebt die neue version.inc ins CVS
 geht ins für die Version passende workdir
 ruft buildxp auf
 prüft, ob ein openxp binary erzeugt wurde, falls nein: exit
 falls ja, ruft es make-rpm.sh auf
 packt ein source tarball als tar.bz2
 schiebt die Ergebnisse auf den ftp server
 setzt die file owner wieder von root auf den owner des workdir
 läuft als root, wegen make-rpm.sh

buildxp:
 muss im workdir der gewünschten Version aufgerufen werden
 holt sich die Parameter mit get_build_nr.pl
 löscht alle alten .o, .ppu etc
 macht ein cvs up mit zur Version (3.8 oder 3.9) passendem -r Parameter
 ruft makexp auf
 prüft, ob binaries erzeugt wurden
 falls nein: verschickt die Fehlermail an die cvs-Liste mit passend
  zusammengebautem Subject
 ruft rc und ihs auf
 falls binaries da sind, packt es die binary tarballs
 kann als normaler user laufen

makexp:
  muss im workdir der gewünschten Version aufgerufen werden
  holt sich die Parameter mit get_build_nr.pl
  setzt passend zur Version (3.8 oder 3.9) die fpc Optionen
  ruft fpc und Kylix auf für openxp
  kann das binary mit upx packen   
  stellt die Logfiles für fpc und Kylix in Archiven zusammen   
  ruft fpc auf für rc und ihs
  kann als normaler user laufen

make-rpm.sh:
  muss im workdir der gewünschten Version aufgerufen werden
  holt sich die Parameter mit get_build_nr.pl
  legt ein temp-workdir an
  checkt die Quellen der gewünschten Version (3.8 oder 3.9) aus
  packt daraus das source tarball für rpm
  und schiebt es nach /usr/src/packages/SOURCES/ mit richtigem Dateinamen
  löscht das tmp-workdir
  ruft rpm -ba auf mit zur Version passendem .spec
   (das benutzt dann den tarball aus /usr/src/packages/SOURCES/ und baut
    daraus .src.rpm und .i386.rpm)
  muss natürlich als root laufen  

get_build_nr.pl:
 holt aus version.inc und xpglobal die Versionsnummern
 setzt die ENV Variablen OPENXP_MAINVER, OPENXP_SUBVER und OPENXP_BUILD,
  die dann von allen Skripten benutzt werden, um die Dateinamen richtig 
  zusammen zu bauen, in die richtigen Verzeichnisse zu wechseln und das 
  Subject der Fehlermeldung zusammen zu bauen
 erzeugt mit richtigem Dateinamen und Inhalt ein passendes .spec
   dafür wird openxp.spec als Template genommen

inc_build_nr.pl:
 inkrementiert die Versionnummer und erzeugt ein neues version.inc

Es sind mehrere Skripte, damit man die Teile des Prozesses auch halb-
manuell unabhängig durchführen kann: makexp, um nur ein neues Binary zu
bauen; buildxp, um auch neue rc und ihs Ergebnisse zu haben und Fehler 
an die Liste zu schicken; snapshot um einen snapshot auch manuell
anstarten zu können (manchmal hängt sich der cron weg, vermutlich immer
dann, wenn T-Online dem Rechner während des Laufes den täglichen Kick  
verpasst). Da sich alle Skripte aus Konsistenzgründen die Parameter für
die Version aus get_build_nr.pl holen, darf dieses Skript die
Versionsnummer nur auslesen, nicht aber neu setzen. Dafür ist inc_
zuständig, das nur aufgerufen wird, wenn auch tatsächlich ein snapshot
gebaut wird.

