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
<h2>&Uuml;ber OpenXP</h2>

Crosspoint (XP) ist ein textbasierendes Mailprogramm f&uuml;r viele 
verschiedene Mailboxennetze wie das Fidonet, ZConnect und nat&uuml;rlich
auch Internet Mail und News. Im Dezember '99 hat der Autor Peter Mandrella
den Quellcode von CrossPoint 3.20&szlig; freigegeben. Eine ver&ouml;ffentlichte
Version von CrossPoint 3.20 hat es jedoch von ihm nicht gegeben.

<br>Mittlerweile ist daraus durch die Arbeit vieler Freiwilliger OpenXP
entstanden.

<p>Es existieren drei Versionen von OpenXP:

<p>OpenXP 3.20, eine fehlerbereinigte Version
von CrossPoint 3.20&szlig;, in die nur noch Bugfixes einflie&szlig;en.

<p>OpenXP 3.30, 16 Bit, in das etliche Erweiterungen eingeflossen sind.

<p>OpenXP 3.70, eine reine 32-Bit-Version, die
relativ stark ver&auml;ndert wurde und von der es neben der Dos-Version auch
eine Native-Version f&uuml;r Windows, OS/2 und Linux/FreeBSD gibt. Wegen
der gro&szlig;en Umstellungen sind viele Funktionen dieser Version allerdings
entweder noch nicht wieder freigeschaltet oder funktionieren noch nicht richtig.

<h3>Lizenz</h3>

Alle Versionen von OpenXP au&szlig;er der Version 3.70 unterliegen der
SLizenz von Peter Mandrella, m&uuml;ssen also registriert werden. OpenXP 3.70
unterliegt der GPL, ist also komplett kostenlos.

<? 
// this is the last command, it closes the document
ShowFooter(); 
?> 
