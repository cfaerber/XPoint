<?
/* $Id$ */

/* file contains menu and links definitions */

$ext	= '.php3';			// When ever update to php4, change this to '.php'
if (eregi("^gut", $SERVER_NAME)) {	// for my own purpose
	$ext = '.php';
};

$Menu = array(
	0 => array("de"	=> "Hauptseite",	// German Title
		"en" 	=> "Home",	// English
		"url"   => "index" . $ext . "?file=main-" . $language . ".html",
		"sub"	=> false),		// sub menu?
	1 => array("de"	=> "News",
		"en" 	=> "News",
		"url"   => "index" . $ext . "?news=news-" . $language . ".txt?genindex=0",
		"sub"	=> true),
	2 => array("de"	=> "FAQ",
		"en" 	=> "FAQ",
		"sub"	=> false),
	3 => array("de"	=> "Allgemein",
		"en" 	=> "general",
		"url"	=> "index" . $ext . "?news=faq-de.txt?genindex=1",
		"sub"	=> true),
	4 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> "index" . $ext . "?news=faq340-de.txt?genindex=1",
		"sub"	=> true),
	5 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "index" . $ext . "?news=faq370-de.txt?genindex=1",
		"sub"	=> true),
	6 => array("de"	=> "Downloads",
		"en" 	=> "Downloads",
		"url"	=> "index" . $ext . "?file=download-de.html",
		"sub"	=> false),
	7 => array("de"	=> "Handbuch",
		"en" 	=> "Manual",
		"url"	=> "index" . $ext . "?file=handbuch/index.html",
		"sub"	=> false),
	8 => array("de"	=> "Kontakt",
		"en" 	=> "Contact",
		"url"	=> "index" . $ext . "?file=contact-" . $language . ".html",
		"sub"	=> false),
	9 => array("de"	=> "Logs",
		"en" 	=> "Logs",
		"sub"	=> false),
	10 => array("de"	=> "3.20",
		"en" 	=> "3.20",
		"first"	=> true,		// a little workaround
		"url"	=> "index" . $ext . "?file=log320.html",
		"sub"	=> true),
	11 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> "index" . $ext . "?file=log330.html",
		"sub"	=> true),
	12 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "index" . $ext . "?file=log370.html",
		"sub"	=> true),
	13 => array("de"	=> "Entwicklerinfos",
		"en" 	=> "Developer info",
		"url"	=> "index" . $ext . "?file=code-en.html",
		"sub"	=> false),
	14 => array("de"	=> "Hinweise",
		"en" 	=> "Hints",
		"url"	=> "index" . $ext . "?news=hints-de.txt?genindex=1",
		"sub"	=> true),
	15 => array("de"	=> "CVS Log",
		"en" 	=> "CVS log",
		"url"	=> "index" . $ext . "?bare=http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt",
		"sub"	=> true),
	16 => array("de"	=> "Links",
		"en" 	=> "Links",
		"url"	=> "links-" .$language . $ext . "?file=links-" . $language . ".html",
		"sub"	=> false));


?>
