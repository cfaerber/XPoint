<?
/* $Id$ */

/* file contains menu and links definitions */

$ext	= '.php3';			// When ever update to php4, change this to '.php'
if (eregi("^gut", $SERVER_NAME)) {	// for my own purpose
	$ext = '.php';
};

$Links = array(
	0 => array("name" => "CrossPoint", "url" => "http://www.crosspoint.de/"),
	1 => array("name" => "FreePascal", "url" => "http://www.freepascal.org/"));

$Menu = array(
	0 => array("de"	=> "Hauptseite",	// German Title
		"en" 	=> "Home",	// English
                "url"   => "index" . $ext . "?file=main-" . $language . ".html",
		"sub"	=> false),		// sub menu?
	1 => array("de"	=> "Info",
		"en" 	=> "Info",
		"sub"	=> false),
	2 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> "index" . $ext . "?news=info340-" . $language . ".html?genindex=0",
		"sub"	=> true),
	3 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "index" . $ext . "?news=info370-" . $language . ".html?genindex=0",
		"sub"	=> true),
	4 => array("de"	=> "Downloads",
		"en" 	=> "Downloads",
		"url"	=> "index" . $ext . "?file=download-de.html",
		"sub"	=> false),
	5 => array("de"	=> "Handbuch",
		"en" 	=> "Manual",
		"url"	=> "index" . $ext . "?file=handbuch/index.html",
		"sub"	=> false),
	6 => array("de"	=> "Kontakt",
		"en" 	=> "Contact",
		"url"	=> "index" . $ext . "?file=contact-" . $language . ".html",
		"sub"	=> false),
	7 => array("de"	=> "Logs",
		"en" 	=> "Logs",
		"sub"	=> false),
	8 => array("de"	=> "3.20",
		"en" 	=> "3.20",
		"first"	=> true,		// a little workaround
		"url"	=> "index" . $ext . "?file=log320.html",
		"sub"	=> true),
	9 => array("de"	=> "3.40",
		"en" 	=> "3.40",
		"url"	=> "index" . $ext . "?file=log330.html",
		"sub"	=> true),
	10 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "index" . $ext . "?file=log370.html",
		"sub"	=> true),
	11 => array("de"	=> "Infos f&uuml;r Programmierer",
		"en" 	=> "Programmer info",
		"url"	=> "index" . $ext . "?file=code-en.html",
		"sub"	=> false),
	12 => array("de"	=> "Coding style",
		"en" 	=> "Coding style",
		"url"	=> "index" . $ext . "?news=hints-de.html?genindex=1",
		"sub"	=> true),
	13 => array("de"	=> "CVS Log",
		"en" 	=> "CVS log",
		"url"	=> "index" . $ext . "?bare=http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt",
		"sub"	=> true));


?>
