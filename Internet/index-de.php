<?
/* $id$ */

require("webtools.php");

// Define the path-level of the beta
$v33 = '6&szlig;';
$v37 = '2&szlig;';

?>
<!doctype html public "-//W3C//DTD HTML 3.2 Final//EN">
<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"> 
	<title>OpenXP Online</title> 
</head>
<body bgcolor="White" text="Black" leftmargin=10 topmargin=10>
<basefont face="Arial,Helvetica,sans-serif">
<table width='100%'>
	<tr><td align='center' valign='middle'><h1>OpenXP Crosspoint Projekt Homepage</h1></td></tr>
	<tr><td align='right' valign='bottom'><small><a href='http://www.openxp.com/'>English</a></small></td></tr>
</table>
<hr color="Blue" noshade size=1>
<!-- Main table -->
<table width='100%' border='0' cellspacing='0' cellpadding='4'><tr>
<!-- Download Row -->
<td width=170 align='left' valign='top'>
	<table width="100%" border="1" bordercolor="Blue" cellpadding="4" cellspacing="0">
	<tr bgcolor="Yellow">
		<th align="center" nowrap>16 Bit Release 3.20</th>
	</tr>
	<tr>
		<td align="left">
		DOS 16 Bit Komplett<br>
		<a href="ftp://ftp.openxp.de/openxp/oxp320d.zip">OXP320D.ZIP</a>
		<small><? echo(GetFileSize("openxp/oxp320d.zip")); ?></small><br>
                DOS 16 Bit Update<br>
                <a href="ftp://ftp.openxp.de/openxp/oxp320du.rar"> OXP320DU.RAR</a>
		<small><? echo(GetFileSize("openxp/oxp320du.rar")); ?></small><br>
                DOS 16 Bit Englisch<br>
                <a href="ftp://ftp.openxp.de/openxp/oxp320de.zip"> OXP320DE.ZIP</a>
		<small><? echo(GetFileSize("openxp/oxp320de.zip")); ?></small><br>
                Sourcecode<br>
                <a href="ftp://ftp.openxp.de/openxp/oxp320_s.rar"> OXP320_S.RAR</a>
		<small><? echo(GetFileSize("openxp/oxp320_s.rar")); ?></small>
		</td>
	</tr>
	</table>
	<table width="100%" border="1" bordercolor="Blue" cellpadding="4" cellspacing="0">
	<tr bgcolor="Yellow">
		<th align="center" nowrap>16 Bit Beta 3.30.<? echo($v33); ?></th>
	</tr>
	<tr>
		<td align="left">
		DOS 16 Bit Komplett<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp330db.zip">OXP330DB.ZIP</a>
		<small><? echo(GetFileSize("openxp/beta/oxp330db.zip")); ?></small><br>
		DOS 16 Bit Update<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp330du.rar">OXP330DU.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp330du.rar")); ?></small><br>
		Sourcecode<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp330_s.rar">OXP330_S.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp330_s.rar")); ?></small>
		</td>
	</tr>
	</table>
	<table width="100%" border="1" bordercolor="Blue" cellpadding="4" cellspacing="0">
	<tr bgcolor="Yellow">
		<th align="center" nowrap>32 Bit Beta 3.70.<? echo($v37); ?></th>
	</tr>
	<tr>
		<td align="left">
		Grundpaket<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp370_g.rar">OXP370_G.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp370_g.rar")); ?></small><br>
		DOS 32 Bit EXE<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp370pe.rar">OXP370PE.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp370pe.rar")); ?></small><br>
		Win 32 Bit EXE<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp370we.rar">OXP370WE.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp370we.rar")); ?></small><br>
		Linux 32 Bit Komplett<br>
		NOT AVAILABLE<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp370le.rpm">OXP370LE.RPM</a>
		<small><? echo(GetFileSize("openxp/beta/oxp370le.rpm")); ?></small><br>
		Sourcecode<br>
		<a href="ftp://ftp.openxp.de/openxp/beta/oxp370_s.rar">OXP370_S.RAR</a>
		<small><? echo(GetFileSize("openxp/beta/oxp370_s.rar")); ?></small>
		</td>
	</tr>
	</table>
	<table width="100%" border="1" bordercolor="Blue" cellpadding="4" cellspacing="0">
	<tr bgcolor="Yellow">
		<th align="center" nowrap>Links</th>
	</tr>
	<tr>
		<td align="left">
		<a href="ftp://ftp.openxp.de">OpenXP FTP Server</a><br>
		<a href="http://www.crosspoint.de/">CrossPoint</a><br>
		<a href="http://www.freepascal.org/">FreePascal</a>
		</td>
	</tr>
	</table>
</td>

<!-- Info Row -->
<td align='left' valign='top'>
	<p>Crosspoint (XP) ist ein Text-basierendes Mail-Programm f&uuml;r viele 
        verschiedene Mailboxennetze wie FIDO, ZConnect und nat&uuml;rlich auch 
        Internet Mail und News. Im Dezember '99 hat der Autor 
	<a href="http://www.crosspoint.de">Peter Mandrella</a> den Quellcode von 
	CrossPoint.3.20&szlig; freigegeben. Diese Version wurde in den letzten 3 Jahren 
	von ihm entwickelt, aber nie ver&ouml;ffentlicht.</p>
	
	<p>Das war f&uuml;r mich der Anla&szlig;, den vorliegenden Source als Ausgangspunkt 
        f&uuml;r eine weitere Entwicklung zu nutzen. Im Internet haben sich dann 
        viele Entwickler und Interessierte zusammengefunden und an diesem Projekt 
        mitgeholfen. Das entstandene Projekt hei&szlig;t OpenXP. Mittlerweile 
        wurden die Sourcen von mir und anderen Autoren des OpenXP Teams deutlich 
        erweitert und fehlerbereinigt. Die zahlreichen &Auml;nderungen sind in 
        den Logfiles f&uuml;r die Version <a href="log320.html">3.20</a>, <a href="log330.html">3.30 
        (DOS 16 Bit)</a> und <a href="log370.html">3.70 (32 Bit)</a> nachzulesen. 
        Die genauen &Auml;nderungen im Sourcecode zeigt das 
	<a href="http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt">CVS 
        Log</a>. Die <a href="handbuch">Dokumentation</a> im HTML-Format ist ebenfalls 
        online. Bugs und Features sind auf den Seiten von 
	<a href="https://sourceforge.net/projects/openxp/"><b>SourceForge</b></a> 
        zu finden.</p>
	<p>Ziel ist die Weiterentwicklung von OpenXP sowohl unter DOS als auch die 
        Portierung auf die g&auml;ngigen 32 Bit Betriebssysteme wie Windows, OS/2 
        und Linux. &Uuml;ber den aktuellen Stand der Linux-Portierung informiert 
        die <a href="http://www.uni-jena.de/%7Ei7lema/xplinux.htm">Linux-Seite</a>.</p>
      	<p><b>English speaking</b> users developers and contributors: please visit 
        <a href="http://www.btinternet.com/%7Eoutpost.one">Martin Fosters Homepage</a> 
        with the english translated version of OpenXP and sign on to our 
	<a href="http://mail1.sourceforge.net/mailman/listinfo/openxp-engl">english 
        mailing list</a>. </p>
	<p><b>An alle Programmierer und Interessierte</b> 
	<p>Es werden weiterhin engagierte Mitarbeiter gesucht. Dabei werden nicht 
        nur Programmierer, sondern auch Betatester gebraucht. Die Dokumenation 
        mu&szlig; auch weiter &uuml;berarbeitet werden. Es gibt allerdings au&szlig;er 
        Ruhm nichts daran zu verdienen. Die Weiterentwicklung wird von mir in 
        einer <a href="http://mail1.sourceforge.net/mailman/listinfo/openxp-dev">Mailingliste</a> 
        koordiniert. Interessenten wenden sich bitte direkt an die Liste. Die 
        <a href="code.html">Hinweise f&uuml;r Entwickler</a> sind f&uuml;r die 
        Arbeit am Sourcecode unerl&auml;sslich. Auf <a href="https://sourceforge.net/project/?group_id=3766">SourceForge</a> 
        befindet sich eine OpenXP-Seite, bei der Bugs gemeldet werden k&ouml;nnen. 
        Die &Auml;nderungen der Dateien k&ouml;nnen &uuml;ber <a href="http://fries7-73.stw.uni-jena.de/cvs/cvsweb.cgi">cvsWeb</a> 
        komfortabel betrachtet werden. Einloggen mit Username &quot;cvs&quot;, 
        Passwort &quot;cvs&quot;. 
	<p><b>Links f&uuml;r weitergehende Informationen zu Crosspoint</b> 
	<ul>
        <li>Eine Seite f&uuml;r Einsteiger befindet sich unter <a href="http://www.stad.de">http://www.stad.de</a>. 
        </li>
        <li> Frank Mollenhauer hat unter <a href="http://www.kuddelsoft.de">http://www.kuddelsoft.de</a> 
          viele Tips f&uuml;r XP User zusammengefasst</li>
        <li>Helmut Hullen pflegt unter <a href="http://www.openxp.de/todo">http://www.openxp.de/todo</a> 
          eine ToDo Liste</li>
        <li>Die <a href="wl"> Wunschliste</a> wird von Sebastian Mannke 
	<a href="mailto:sm@openxp.de">sm@openxp.de</a> herausgegeben.</li>
	</ul>
	<p>Markus K&auml;mmerer, <a href="http://www.happyarts.de"> http://www.happyarts.de</a><br>
        eMail: <a href="mailto:mk@openxp.de">mk@openxp.de</a>, FIDO 2:248/2004 
</td>
</tr></table>

<hr size="1"><b>An 
dem Projekt beteiligte Personen &amp; Spenden</b><br><p>Alle Entwickler arbeiten 
an diesem Projekt v&ouml;llig uneigenn&uuml;tzig und erhalten keinerlei Geld f&uuml;r 
ihre Arbeit. Unabh&auml;ngig davon mu&szlig; Crosspoint weiterhin an Autor von 
XP Peter Mandrella (<a href=
"http://www.crosspoint.de">http://www.crosspoint.de</a>) registriert werden! Wenn 
Sie uns jedoch auch finanziell bei der Arbeit unterst&uuml;tzen m&ouml;chten, 
ist nichts dagegen einzuwenden, wenn Sie den Entwicklern eine <i>freiwillige</i> 
Spende &uuml;berweisen. Da wir im OpenXP Team viele Programmierer sind und unterschiedlich 
viel Zeit aufwenden, entscheiden Sie bitte selbst, welche(r) Programmierer von 
Ihnen unterst&uuml;tzt wird, in dem Sie ihm direkt die Spende &uuml;berweisen. 
Im <a href=
"log320.html">Logfile</a> sind die Personen, welche die &Auml;nderungen gemacht 
haben vermerkt. Wir danken schon jetzt f&uuml;r die Unterst&uuml;tzung, die uns 
erm&ouml;glichen wird, weiter viel Zeit in dieses Projekt zu investieren. <br><br>
<table width="100%" border="0" cellspacing="1" cellpadding="1">
  <tr> 
    <th nowrap width="14%" bgcolor="#EEEE00">Name 
    <th width="5%" bgcolor="#EEEE00"> 
      <div align="center">K&uuml;rzel</div>
    <th nowrap width="28%" bgcolor="#EEEE00">Homepage 
    <th width="20%" bgcolor="#EEEE00">Job 
    <th width="33%" bgcolor="#EEEE00">Konto f&uuml;r Spenden 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Markus K&auml;mmerer </b> 
    <td width="5%"> 
      <div align="center"><a href="mailto:mk@openxp.de">MK</a></div>
    <td nowrap width="28%"> 
      <p> <a href="http://www.happyarts.de"> http://www.happyarts.de</a> 
    <td width="20%">Projektkoordinierung 
    <td width="33%">Konto 268863800, BLZ 38070724, Bank 24 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Flups Baumann 
    <td width="5%"> 
      <div align="center"><a href="mailto:fb@openxp.de">FB</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Maggi 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Hinrich Donner</b> 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:hd@openxp.de">HD</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Linux-Portierung 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Christian Faulhammer 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:cf@openxp.de">CF</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Dokumentation 
    <td width="33%">Keine Spenden erw&uuml;nscht 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Claus F&auml;rber 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:cl@openxp.de">CL</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">UUZ 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Martin Foster </b> 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:mf@openxp.de">MF</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Englische &Uuml;bersetzung 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Frank Ellert 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:fe@openxp.de">FE</a></div>
    <td nowrap width="28%"><a href="http://www.free.de/%7Efe">http://www.free.de/~fe</a> 
    <td width="20%">&nbsp; 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Mathias Helm 
    <td width="5%"> 
      <div align="center"><a href="mailto:ke@openxp.de">KE</a></div>
    <td nowrap width="28%"><a href="http://basedrum.de">http://basedrum.de</a> 
    <td width="20%">ZConnect 
    <td width="33%" bgcolor="#e0e0e0">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Kai Henningsen 
    <td width="5%"> 
      <div align="center"><a href="mailto:kh@openxp.de">KH</a></div>
    <td nowrap width="28%"><a href="http://www.westfalen.de/private/khms/"> http://www.westfalen.de/private/khms/</a> 
    <td width="20%">&nbsp; 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%" bgcolor="#e0e0e0"><b>Michael Heydekamp</b> 
    <td width="5%"> 
      <div align="center"><a href="mailto:my@openxp.de">MY</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Misc, Design, Hilfe, Doku 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Helmut Hullen 
    <td width="5%"> 
      <div align="center"><a href="mailto:hh@openxp.de">HH</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">ToDo-Liste 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Jochen Gehring </b> 
    <td width="5%"> 
      <div align="center"><a href="mailto:jg@openxp.de">JG</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Misc, Assembler 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Malte Kiesel</b> 
    <td width="5%"> 
      <div align="center"><a href="mailto:ma@openxp.de">MA</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">32 Bit Mailer 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Klaus P. Kleinsimon 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:kpk@gmx.net">KP</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Kontakt zu P. Mandrella 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Matthias Leonhardt </b> 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:ml@openxp.de">ML</a></div>
    <td nowrap width="28%"><a href="http://www.uni-jena.de/%7Ei7lema"> http://www.uni-jena.de/~i7lema</a> 
    <td width="20%">Linux-Portierung 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Michael Koppel 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:mo@openxp.de">MO</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Programmierung 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Sebastian Mannke 
    <td width="5%"> 
      <div align="center"><a href="mailto:sm@openxp.de">SM</a></div>
    <td nowrap width="28%"><a href="http://www.bbsliste.de">http://www.bbsliste.de</a> 
    <td width="20%">Wunschliste 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%">Heiko Schoenfeld 
    <td width="5%"> 
      <div align="center"><a href="mailto:hs@openxp.de">HS</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Misc 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Stefan Vinke</b> 
    <td width="5%"> 
      <div align="CENTER"><a href="mailto:sv@openxp.de">SV</a></div>
    <td nowrap width="28%">&nbsp; 
    <td width="20%">Misc, Hilfe 
    <td width="33%">&nbsp; 
  <tr bgcolor="#e0e0e0"> 
    <td nowrap width="14%"><b>Martin Wodrich </b> 
    <td width="5%"> 
      <div align="center"><a href="mailto:mw@openxp.de">MW</a></div>
    <td nowrap width="28%"> 
      <p> <a href="http://wodrich.webprovider.com"> http://wodrich.webprovider.com</a> 
    <td width="20%">&nbsp; 
    <td width="33%">&nbsp; 
</table>
<br> Alle Personen sind unter der Adresse 
xx@openxp.de per eMail erreichbar, wobei xx f&uuml;r das K&uuml;rzel aus dieser 
Liste steht. Personen mit fett gedruckten Namen haben direkten Zugriff auf den 
CVS-Server und k&ouml;nnen &Auml;nderungen einspielen. <br><hr size="1"> <p><img src= 
"http://cgicounter.kundenserver.de/cgi-bin/cnt?clsid=fbd3392f1f13ed524c740ca04dec22161"> 
<a href="http://www.libranet.com/petition.html"><img src=
"linux.gif" width="90" height="30" border="0" alt=
"Sign The Linux Driver Petition"></a><br>
</body>
</html>
 