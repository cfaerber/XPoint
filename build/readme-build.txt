Autor: Christian B�ttger cb@openxp.de, 2003-10-18

Hier der Ablauf f�r die build scripts:

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
 pr�ft per diff, ob sich im CVS gegen�ber -1Day was ge�ndert hat, wobei
  �nderungen an version.inc ignoriert werden
 l�uft als root, wird von cron aufgerufen

snapshot:
 wird mit Parameter 3.8 oder 3.9 aufgerufen, um ins passende workdir
  gehen zu k�nnen
 holt die version.inc aus dem CVS
 erh�ht version.inc durch Aufruf von inc_build_nr.pl
 schiebt die neue version.inc ins CVS
 geht ins f�r die Version passende workdir
 ruft buildxp auf
 pr�ft, ob ein openxp binary erzeugt wurde, falls nein: exit
 falls ja, ruft es make-rpm.sh auf
 packt ein source tarball als tar.bz2
 schiebt die Ergebnisse auf den ftp server
 setzt die file owner wieder von root auf den owner des workdir
 l�uft als root, wegen make-rpm.sh

buildxp:
 muss im workdir der gew�nschten Version aufgerufen werden
 holt sich die Parameter mit get_build_nr.pl
 l�scht alle alten .o, .ppu etc
 macht ein cvs up mit zur Version (3.8 oder 3.9) passendem -r Parameter
 ruft makexp auf
 pr�ft, ob binaries erzeugt wurden
 falls nein: verschickt die Fehlermail an die cvs-Liste mit passend
  zusammengebautem Subject
 ruft rc und ihs auf
 falls binaries da sind, packt es die binary tarballs
 kann als normaler user laufen

makexp:
  muss im workdir der gew�nschten Version aufgerufen werden
  holt sich die Parameter mit get_build_nr.pl
  setzt passend zur Version (3.8 oder 3.9) die fpc Optionen
  ruft fpc und Kylix auf f�r openxp
  kann das binary mit upx packen   
  stellt die Logfiles f�r fpc und Kylix in Archiven zusammen   
  ruft fpc auf f�r rc und ihs
  kann als normaler user laufen

make-rpm.sh:
  muss im workdir der gew�nschten Version aufgerufen werden
  holt sich die Parameter mit get_build_nr.pl
  legt ein temp-workdir an
  checkt die Quellen der gew�nschten Version (3.8 oder 3.9) aus
  packt daraus das source tarball f�r rpm
  und schiebt es nach /usr/src/packages/SOURCES/ mit richtigem Dateinamen
  l�scht das tmp-workdir
  ruft rpm -ba auf mit zur Version passendem .spec
   (das benutzt dann den tarball aus /usr/src/packages/SOURCES/ und baut
    daraus .src.rpm und .i386.rpm)
  muss nat�rlich als root laufen  

get_build_nr.pl:
 holt aus version.inc und xpglobal die Versionsnummern
 setzt die ENV Variablen OPENXP_MAINVER, OPENXP_SUBVER und OPENXP_BUILD,
  die dann von allen Skripten benutzt werden, um die Dateinamen richtig 
  zusammen zu bauen, in die richtigen Verzeichnisse zu wechseln und das 
  Subject der Fehlermeldung zusammen zu bauen
 erzeugt mit richtigem Dateinamen und Inhalt ein passendes .spec
   daf�r wird openxp.spec als Template genommen

inc_build_nr.pl:
 inkrementiert die Versionnummer und erzeugt ein neues version.inc

Es sind mehrere Skripte, damit man die Teile des Prozesses auch halb-
manuell unabh�ngig durchf�hren kann: makexp, um nur ein neues Binary zu
bauen; buildxp, um auch neue rc und ihs Ergebnisse zu haben und Fehler 
an die Liste zu schicken; snapshot um einen snapshot auch manuell
anstarten zu k�nnen (manchmal h�ngt sich der cron weg, vermutlich immer
dann, wenn T-Online dem Rechner w�hrend des Laufes den t�glichen Kick  
verpasst). Da sich alle Skripte aus Konsistenzgr�nden die Parameter f�r
die Version aus get_build_nr.pl holen, darf dieses Skript die
Versionsnummer nur auslesen, nicht aber neu setzen. Daf�r ist inc_
zust�ndig, das nur aufgerufen wird, wenn auch tats�chlich ein snapshot
gebaut wird.

