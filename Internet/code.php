<?
/* $Id$ */

// set it to
// - de for German
// - en for English
//
$language = 'de';

// Libs
require("menu.php");
require("webtools.php");
// Start everything
// after this function you cannot use header anymore!
ShowHeader("OpenXP Online");

/* Below this point you can insert html-code as you like, but remind the ShowFooter! */
?>

<h2>Developer info</h2>

<h3>General</h3>

OpenXP is developed by many people. Therefore every developer has to follow
some rules.

<p>For coordination of the OpenXP project there is a
<a href="http://mail1.sourceforge.net/mailman/listinfo/openxp-dev">maillinglist</a>.
English postings will be answered in english - and are very welcome :-).

<p>You can use <a href="http://fries7-73.stw.uni-jena.de/cvs/cvsweb.cgi">cvsWeb</a>
to browse the CVS repository and see file logs (both login and password "cvs").

<p>OpenXP compiles with <a href="http://www.freepascal.org">FreePascal</a>
and <a href="http://www.vpascal.com">Virtual Pascal</a>.

<h3>Legal</h3>

<b>Be aware OpenXP is an open source project. That means any changes you make
to it are free and not someone's property and can be used in any other
open source project as well. This is especially true for the XP2 project
with which fixes and changes are shared.</b>
<br>Any changes you make to the OpenXP 3.70 sources are subject to the file's
license. Licenses possible are the <i>BSD license</i> (basically a Public
Domain license with protected author credits), the <i>LGPL</i> (Lesser GNU
Public License; stronger copyleft than BSD as any derivative of the code has
to be at least LGPL again) and <i>GPL</i> (strong copyleft).
<br>If you create or check in any new units, please choose one of these licenses
and add a proper not in the unit's header.
<p>For information on Open Source licenses visit <a href="http://www.opensource.org">
OpenSource.org</a> or the <a href="http://www.fsf.org/copyleft/licenses.html">
FSF license page</a>. Some licenses are also available on the
<a href="ftp://ftp.openxp.de/openxp/devdoc/license">OpenXP ftp server</a>.

<h3>Getting the source code</h3>
<a href="http://www.cvshome.com">CVS</a> is used for managing
the sources. A Windows and OS/2 version can be found on the
<a href="ftp://ftp.openxp.de/openxp/tools">OpenXP ftp server</a>.

<p>CVS uses a "main source archive", called repository. Every developer uses
a copy of this repository called "working directory" on his machine. CVS
cares for keeping both archives in sync.
<br>You create your working directory by installing CVS and entering
<br><font face="Courier New, Courier, mono">cvs -d :pserver:<i>username</i>@fries7-73.stw.uni-jena.de:/usr/local/cvs login</font>
<br>You are prompted for a password (guest read-only login username "cvs",
password "cvs"). The password is stored in ~/.cvspass (when not using Unix you
have to specify a home directory by "set home=<i>directory</i>" before).
<br>Then, you can build your working directory by entering
<br><font face="Courier New, Courier, mono">cvs -d :pserver:<i>username</i>@fries7-73.stw.uni-jena.de:/usr/local/cvs checkout openxp</font>
<br>Now, you have a working directory in ./openxp. Feel free to explore ;-).
<br>BTW, you don't have to use the -d parameter of CVS anymore since the
repository position is saved in the CVS directories in your working
directory.

<p>To update the working directory (get the repository changes since
the last working directory update), enter
<br><font face="Courier New, Courier, mono">cvs update</font>
<br>being the OpenXP directory the current directory. If you made
changes to your working directory, they will be merged with the
changes that are obtained from the repository.

<p>To update the repository with the changes you made to your working
directory you need a full CVS account. Then, you can enter
<br><font face="Courier New, Courier, mono">cvs commit <i>file</i></font>.
<br>You will be prompted for a log message.

<hr>

Manchmal ist es n&ouml;tig, eine &auml;ltere Version vom CVS-Server zu ziehen. 
Durch die Option -r wird ein Release Tag angegeben.
<br><FONT FACE="Courier New, Courier, mono">cvs up -r Beta_3_20_21</FONT>
<br>Mit diesen Befehlen wird entweder Beta 3.20.21 ausgecheckt.
<p>OpenXP hat mittlerweile drei Entwicklerb&auml;ume, den 3.20 (16 Bit DOS), den 
3.30 (16 Bit DOS) und 3.70 (32 Bit). Diese Entwicklerb&auml;ume sind unabh&auml;ngig 
voneinander und k&ouml;nnen folgenderma&szlig;en ausgecheckt werden: 
<br><font face="Courier New, Courier, mono">cvs up -r Branch_3_20_Release <br>
  cvs up -r Branch_3_30_Release</font>
<br>In der Version 3.20 sind im Prinzip keine &Auml;nderungen mehr erlaubt, au&szlig;er 
notwendige Bugfixes. In den 3.30 und 3.70er B&auml;umen werden jeweils die 16 
und 32 Bit Versionen weiterentwickelt. Ein Commit ohne weitere Angaben bezieht sich
auf die Version 3.70.

<p>Alle weiteren CVS-Befehle sind in der Dokumentation des Programmes bzw. in 
dem untenstehenden Buch erkl&auml;rt. Trotzdem ein paar kurze Hinweise (mit
&quot;rw&quot; gekennzeichnete Hinweise gelten nur f&uuml;r Entwickler mit
vollem CVS-Zugriff):
<ul>
<li>cvs update erzeugt keine Verzeichnisse, die inzwischen im Repository angelegt
wurden; das macht erst cvs checkout openxp (aus dem h&ouml;herliegenden Verzeichnis).
<li>cvs diff (datei) gibt die &Auml;nderungen an, die an der jeweiligen Datei seit
dem letzten Update lokal gemacht wurden - sollte also die Basis f&uuml;r sinnvolle
Logeintr&auml;ge sein bzw. bem&uuml;ht werden, falls man vergessen hat, was man
alles ver&auml;ndert hat.
<li>rw: Will man &Auml;nderungen an einer Datei machen, die sich aber hinziehen
werden, und dabei vermeiden, da&szlig einem ein anderer Entwickler in die Quere
kommt, die jeweilige Datei mit cvs edit (datei) als "wird gerade bearbeitet"
kennzeichnen. Dieses Flag wird gel&ouml;scht, sobald ein unedit oder commit
gemacht wird. Bis dahin ist die Datei f&uuml;r alle Leute, die nicht auch ein
cvs edit gemacht haben read-only. <i>ACHTUNG:</i> Auf keinen Fall die Angabe
des Dateinamens vergessen!
<li>rw: Nach cvs watch add (datei) bekommt man bei jeder &Auml;nderung der
jeweiligen Datei eine Mail.
<li>Mit cvs watchers kann man sich eine Liste der "zusehenden" Entwickler
anzeigen lassen. Sinnvoll f&uuml;r Entwickler, die einen speziellen
Teil des Projekts maintainen und &uuml;ber jede &Auml;nderung, die daran nicht
von ihnen gemacht wird, explizit informiert werden wollen - und nat&uuml;rlich
praktisch, falls man wissen will, welcher Entwickler f&uuml;r die Datei
haupts&auml;chlich zust&auml;ndig ist. Das Format der Anzeige ist (Datei)
(Entwickerk&uuml;rzel) (Benachrichtigung bei welchen Ereignissen?).
</ul>

<h3>Coding Standard</h3>
Dinge, die man als Entwickler wissen und beachten sollte:
<ul>
<li>Dateien, die man sich kurz ansehen sollte: XPDefine.inc (f&uuml;r $IFDEFs),
Typeform (die unvermeidlichen Typumformungen, Integer to String etc.), XPGlobal
(u.a. plattformunabh&auml;ngige Typdefinitionen), Debug.
<li>Es werden Ansistrings benutzt (wird per $H+ in XPDefine.inc eingeschaltet).
Kurzabri&szlig; dazu: Statt eines Array of Char mit der L&auml;nge des Strings
in Arr[0] wird ein Pointer gespeichert, der auf den (nullterminierten) String
zeigt. Speicherallokation und -freigabe wird vom Compiler zur Laufzeit erledigt.
Das hei&szlig;t: Keine Zugriffe auf String[0], stattdessen SetLength(String,
Len) oder a:=Length(String). Keine Moves auf Strings. Keine Fillchars auf Strings.
Keine Pointer auf Strings (unn&ouml;tig, da eh' schon verpointert). Keine
L&auml;ngenbeschr&auml;nkung von Strings der Art var s: String[3] (unn&ouml;tig,
da eh' dynamisch). Falls man mittels String[Zeichen]:=Ch einen String ver&auml;ndern
will, darauf achten, <i>vorher</i> mittels SetLength eine ausreichende L&auml;nge
angegeben zu haben.
<li>Standardtyp f&uuml;r ganze Zahlen ist Integer. Byte, Word und DWord werden
nur verwendet, wenn es das Programmumfeld erfordert (z.b. Records). Integer ist
auf einer 16 Bit Platform 2 Byte, auf einer 32 Bit Platform 4 Byte groß und
immer das <i>schnellste</i> CPU Register.
<li>Bei dem Inline-Assembler muss darauf geachtet werden, das die Variablen SI,
DI, ES ohne Probleme verwendet werden dürfen. Die Variable DS muss jedoch vorher
gesichert werden.
<li>Bei Units darauf achten, da&szlig; im Interface-Teil jede Einzelheit genau
dokumentiert ist (siehe z.B. Modem.pas).
<li>Da die Bezeichner teilweise sehr ung&uuml;nstig gew&auml;hlt sind, sind
(sinnvolle) Umbenennungen erlaubt und erw&uuml;nscht. Sollte allerdings eine
&Auml;nderung hierbei &Auml;nderungen in mehreren Dateien nachsichziehen,
ist das - nach einer Ank&uuml;ndigung auf der Developer-Liste - entweder komplett
in einem Rutsch beim Commit zu machen oder per {* xxx}-Kommentar erstmal nur
vorzumerken.
<li>Hinweise im Source: Entweder per {* xxx}-Kommentar (f&uuml;r Kleinigkeiten,
die nicht dringend sind, aber irgendwann mal ge&auml;ndert werden sollten) oder
per {$hint xxx} - das nervt dann aber bei jedem Kompilieren, also sparsam dosieren.
Kommentare im Code der Art {(K&uuml;rzel) Habe das-und-das ge&auml;ndert} sind in
jedem Fall unn&ouml;tig: Sowas bekommt jeder per cvs diff heraus, wenn es
interessiert; bei gr&ouml;&szlig;eren &Auml;nderungen sollte man das eh' in die
Mailinglist posten.
<li>CVS-Logeintr&auml;ge: Bitte darauf achten, da&szlig; die Logs auch im Kontext
Sinn ergeben. Also lieber mehrere einzelne Commits als ein gro&szlig;es, bei dem
man dann aber f&uuml;r die einzelnen Dateien die Logeintr&auml;ge vergessen kann.
<li>Variablen und Prozedurbezeichner: Buchstaben kosten kein Geld. Also Bezeichner
lieber zu lang als zu kurz w&auml;hlen. Globale Bezeichner, die k&uuml;rzer als
f&uuml;nf Buchstaben sind (und dabei m&ouml;glichst noch mit einem Underscore
anfangen oder Zahlen beinhalten ;-), <i>k&ouml;nnen</i> gar nicht gut gew&auml;hlt sein.
<li>XP sollte nach den eigenen &Auml;nderungen noch vollst&auml;ndig kompilierbar sein.
Das l&auml;&szlig;t sich einfach mittels der make.bat &uuml;berpr&uuml;fen.
<li>Bei jeder Funktion m&ouml;glichst den Zugriff auf globale Variablen vermeiden -
wenn das doch gemacht wird bzw. unvermeidlich ist, sollte diese Tatsache sehr deutlich
in einem Kommentar beim Prozedurkopf hervorgehoben werden.
</ul>

<h3>&Auml;nderungen einbringen</h3>
Da an diesem Projekt viele Leute  arbeiten, ist es wichtig, einen einheitlichen
Standard zu haben, wie neuer Code in die offizielle Distribution gebracht wird.
Dazu wird das GNU Tool DIFF genutzt, welches auch auf unserem
<a href="ftp://ftp.openxp.de/openxp" target="_self">ftp-server</a> liegt. 
Um potentiellen Problemen vorzubeugen, mu&szlig; DIFF mit der Option -u verwendet 
werden (Dank an Robo f&uuml;r diesen Hinweis).

<h3>Buch-Empfehlungen</h3>
<TABLE WIDTH="75%" BORDER="1">
<TR><TD><A HREF="http://www.amazon.de/exec/obidos/ASIN/1556154844/openxpprojekt"><IMG SRC="1556154844_m.gif" BORDER="0"><BR> 
</A><A HREF="http://www.amazon.de/exec/obidos/ASIN/1556154844/openxpprojekt"> 
Jetzt bestellen!</A></TD><TD>Ein absoulter Klassiker ist Code Complete von Steve 
McConnell. Ich empfehle dieses Buch jedem, der Software entwickelt.</TD></TR>
<TR><TD><A HREF="http://www.amazon.de/exec/obidos/ASIN/1576104907/openxpprojekt"><IMG SRC="1576104907_l.gif" BORDER="0"><BR> 
</A><A HREF="http://www.amazon.de/exec/obidos/ASIN/1576104907/openxpprojekt"> 
Jetzt bestellen!</A> </TD><TD><P>Dieses Buch behandelt zwei gro&szlig;e Themen: 
</P><P>1. Open Source Software: Es werden Hinweise f&uuml;r das durchf&uuml;hren 
von Open Source Projekten gegeben<BR>2. CVS: Das Versionsmanagement-System wird 
komplett bis hin zur Administration erl&auml;utert</P></TD></TR></TABLE><p><hr> 
<p><img src= 
"http://cgicounter.kundenserver.de/cgi-bin/cnt?clsid=fbd3392f1f13ed524c740ca04dec22161"> 
<a href="http://www.libranet.com/petition.html"><img src="linux.gif" width="90" 
height="30" border="0" alt="Sign The Linux Driver Petition"></a><br> 

<? 
// this is the last command, it closes the document
ShowFooter(); 
?> 
