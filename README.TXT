Project: "Free the source by giving proper names!" :-)
------------------------------------------------------
$Id$

First column: Old file name
Second column: Proposed new file name

If you decide to rename a file to the proposed new one, follow this
procedure:

- ask if someone's working on this file (use xpdev mailinglist)
- add a good description (and a license notice if there's none yet) at
  the top of the file
- think of good procedure names and rename them properly - AND do a full text
  search on *whole* XP source and rename any references accordingly
- comment everything in the interface part of units in fpDoc style (comment
  *before* declaration, for example
  { this procedure is really stupid }
  procedure UnbelieveableStupid;
  ;-)
- delete the CVS logs at the bottom (keep only DOLLAR Log DOLLAR): Logs have
  grown and are partly contraproductive if using full text search on the
  source.
- move the entry you processed in this list to the bottom of the list and
  comment it properly
- commit *everything* that has to do with these changes in one batch
  with a proper log - and don't forget this list.

[FILES TO PROCESS ----------------------------------------------------------]

archive.pas compressors.pas
clip.pas clipboard.pas
crc.pas

databas1.inc ?
databas2.inc
database.inc
database.pas
databaso.pas

datadef.pas datadefglobal.pas ?
datadef1.pas datadeflocal.pas ?
dbase.pas ?
debug.pas (wird durch log.pas ersetzt, sobald letztere fertig ist)
direct.pas (Klasse TDirectory, wuerde ich in eine Art neue fileio packen)
docform.pas (eigenst. Programm, formatiert txt in Blocksatz; benoetigt?)
eddef.pas editordef.pas
editor.inc
editor.pas
encoder.pas txtcodecs.pas (wuerde ich in eine neue Unit packen, in der auch decoder sind)
exxec.pas ...
feiertag.pas (in eine Datums- und Zeitunit integrieren?)
fileio.pas 
gpltools.pas txtcodecs.pas (ROT13-Decoder, zusammen mit encoder.pas in eine neue Unit?)

help.pas
ihs.pas

inout.pas

 --- folgendes in ein separates Netcall-Verzeichnis?
ipaddr.pas (Kurze Erklaerung im Unitkopf fehlt!)
ipaddr.rst
ipcclass.pas (Erklaerung fehlt, was ist das?)
ncnntp.pas (ueberall fehlen die Kurzbeschreibungen im Unitkopf)
ncnntp.rst
ncpop3.pas
ncsmtp.pas
ncsocket.pas
netcall.pas ncbase.pas (damit's zusammenhaengt...?)
xp-fm.pas ncfido.pas (wenn fertig integriert...)
 ---

keys.pas
lister.pas
log.pas
maggi.pas
maske.pas (so lassen, wird ja eh' irgendwann umgeschmissen)
maske.inc
maus2.pas (braucht IRGENDJEMAND eine Maus unter XP?- Ansonsten kicken...)
modem.pas
montage.pas (mit feiertag zusammenlegen)
mouse.pas (siehe maus2.pas)

ncurses.pas
ndiff.pas

osdbsd.inc  (alle loeschen, IMHO kein gutes Konzept, wird auch nicht gebraucht bisher)
osddos32.inc
osdepend.pas
osdlinux.inc
osdos2.inc
osdwin32.inc

pmconv.pas (wird' gebraucht?- Sonst loeschen)
printerx.pas
rc.pas
regexpr.pas regularexpressions.pas (wir haben's ja, ansonsten koennte es auch Register Expire oder sowas heissen...)
resource.pas
stack.pas
stringtools.pas (allgemeine Konverterunit?)
timer.pas
typeform.pas (in Stringtools und Datetools aufteilen?)
unicode.pas 
utftools.pas unicodetools.pas 
uucico.pas (was ist das ueberhaupt?)
uucp-fl1.pas (wird's gebraucht?)
uucp-g.inc uucp-fl1.inc (sonst weiss man nicht, wozu's gehoert)
uuz.pas zc2rfc.pas (oder sowas, uuz ist irgendwie nicht aussagekraeftig, oder?)
viewer.pas (was ist das?- Hinweis im Kopf fehlt. Ist's ein kompletter Viewer
 oder nur ein "Typ-Herausfinder"?)

win2.pas (entfaellt mit neuem Ausgabemodell)
winxp.pas

xp.pas openxp.pas (? :-)

XP-D.RES (in resource-Verzeichnis)
XP-E.RES

xp-fm.pas ncfido.pas, sobald in Netcall-Klassenstruktur integriert
xp-fm.inc

xp0.pas xpdeclarations.pas
xp1.pas xpgeneralroutines.pas ( :-( )
xp10.inc 
xp10.pas xpscheduler.pas (ist zwar mehr drin, aber eine Mix-Unit reicht; der Rest sollte ausgegliedert werden)
xp10p.inc
xp1help.pas xphelp.pas (und Hotkeys...)
xp1input.pas (entfaellt irgendwann...)
xp1menu.inc (ebenso)
xp1o.pas (wie xp1.pas :-( )

[hier hat's mich verlassen ;-)]
xp1o2.pas
xp1s.inc
xp2.pas
xp2c.pas
xp2cfg.inc
xp2db.pas
xp2f.pas
xp3.pas
xp3ex.pas
xp3o.inc
xp3o.pas
xp3o2.pas
xp4.inc
xp4.pas
xp4d.inc
xp4e.pas
xp4o.inc
xp4o.pas
xp4o2.pas
xp4o3.pas
xp4w.inc
xp5.pas
xp6.pas
xp6l.pas
xp6o.pas
xp6s.inc
xp7.inc
xp7.pas
xp7f.pas
xp7l.pas
xp7o.pas
xp7o.bak
xp7.pas
xp7u.inc
xp8.inc
xp8.pas
xp8fs.inc
xp9.pas
xp9.inc
xp9bp.pas
xpauto.pas
xpcc.pas
xpcfg.pas
xpcurses.pas
xpdatum.pas
xpdefine.inc
xpdiff.pas
xpdos32.pas
xpe.pas
xpeasy.pas
xpf1.inc
xpf2.pas
xpfido.pas
xpfidonl.pas
xpfiles.inc
xpfm-d.rq
xpfm-e.rq
xpftnadr.pas
xpglobal.pas
xpheader.inc
xpimpexp.pas
xpipc.pas
xpkeys.pas
xplinux.pas
xpmakehd.inc
xpmaus.pas
xpme.pas
xpmecol.inc
xpmime.pas
xpnntp.pas
xpnodes.pas
xpnt.pas
xpos2.pas
xpreg.pas
xpstat.pas
xpterm.pas
xpuu.pas
xpuu-d.rq
xpuu-e.rq
xpview.pas
xpwin32.pas
xpx.pas
xp_des.pas
xp_iti.pas
xp_pgp.pas
xp_uue.pas
yup2pkt.pas
zcrfc.pas
zfido.pas
zftools.pas
zmodem.pas
zpr.pas

[FILES PROCESSED -----------------------------------------------------------]

(none yet)

[CVS logs ------------------------------------------------------------------]
Please delete logs in a way no more than a few logs are visible here.

$Log$
Revision 1.1  2000/12/10 18:35:28  ma
- suggesting a way to slowly get properly names sources
