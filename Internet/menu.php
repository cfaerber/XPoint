<?
/* $Id$ */

/* file contains menu definitions */

$indexname = "index.php4";

$Menu = array(
	0 => array("de"	=> "Hauptseite",	// German Title
		"en" 	=> "Main",	// English
		"url"   => $indexname . "?file=main-" . $language . ".html",
		"sub"	=> false),		// sub menu?
	1 => array("de"	=> "Newsarchiv",
		"en" 	=> "News archive",
		"url"   => $indexname . "?news=news-" . $language . ".txt&genindex=0",
		"sub"	=> true),
	2 => array("de"	=> "FAQ",
		"en" 	=> "FAQ",
		"sub"	=> false),
	3 => array("de"	=> "Allgemein",
		"en" 	=> "General",
		"url"	=> $indexname . "?news=faq-de.txt&genindex=1",
		"sub"	=> true),
	4 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> $indexname . "?news=faq340-de.txt&genindex=1",
		"sub"	=> true),
	5 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> $indexname . "?news=faq370-de.txt&genindex=1",
		"sub"	=> true),
	6 => array("de"	=> "Downloads",
		"en" 	=> "Downloads",
		"url"	=> $indexname . "?file=download-" . $language . ".html",
		"sub"	=> false),
	7 => array("de"	=> "Handbuch",
		"en" 	=> "Manual",
//		"url"	=> $indexname . "?file=de/handbuch/index.html",
		"sub"	=> false),
	8 => array("de"	=> "Kontakt",
		"en" 	=> "Contact",
		"url"	=> $indexname . "?file=contact-" . $language . ".html",
		"sub"	=> false),
	9 => array("de"	=> "Logs",
		"en" 	=> "Logs",
		"sub"	=> false),
	10 => array("de"	=> "3.20",
		"en" 	=> "3.20",
		"first"	=> true,		// a little workaround
		"url"	=> $indexname . "?file=log320.html",
		"sub"	=> true),
	11 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> $indexname . "?file=log340.html",
		"sub"	=> true),
	12 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> $indexname . "?file=log370.html",
		"sub"	=> true),
	13 => array("de"	=> "Entwicklerinfos",
		"en" 	=> "Developer info",
		"url"	=> $indexname . "?file=code-" . $language . ".html",
		"sub"	=> false),
	14 => array("de"	=> "Hinweise",
		"en" 	=> "Hints",
		"url"	=> $indexname . "?news=hints-de.txt&genindex=1",
		"sub"	=> true),
	15 => array("de"	=> "CVS Log",
		"en" 	=> "CVS log",
		"url"	=> $indexname . "?bare=http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt",
		"sub"	=> true),
	16 => array("de"	=> "Links",
		"en" 	=> "Links",
		"url"	=> $indexname . "?file=links-" . $language . ".html",
		"sub"	=> false));


?>
