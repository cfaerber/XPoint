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
	0 => array("de"	=> "Informationen",	// German Title
		"en" 	=> "Information",	// English
                "url"   => "index-".$language.$ext,
		"sub"	=> false),		// sub menu?
	1 => array("de"	=> "3.30",
		"en"	=> "3.30",
		"url"	=> "info330-" . $language . $ext,
		"sub" => true),
	2 => array("de"	=> "3.70",
		"en"	=> "3.70",
		"url"	=> "info370-" . $language . $ext,
		"sub" => true),
	3 => array("de"	=> "Logs",
		"en" 	=> "Logs",
		"sub"	=> false),
	4 => array("de"	=> "3.20",
		"en" 	=> "3.20",
		"first"	=> true,		// a little workaround
		"url"	=> "show" . $ext . "?file=log320.html",
		"sub"	=> true),
	5 => array("de"	=> "3.30",
		"en" 	=> "3.30",
		"url"	=> "show" . $ext . "?file=log330.html",
		"sub"	=> true),
	6 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "show" . $ext . "?file=log370.html",
		"sub"	=> true),
	7 => array("de"	=> "Handbuch",
		"en" 	=> "Manual",
		"url"	=> "show" . $ext . "?file=handbuch/index.html",
		"sub"	=> false),
	8 => array("de"	=> "Downloads",
		"en" 	=> "Downloads",
		"url"	=> "download" . $ext,
		"sub"	=> false),
	9 => array("de"	=> "Kontakt",
		"en" 	=> "Contact",
		"url"	=> "contact" . $ext,
		"sub"	=> false),
	10 => array("de"	=> "Infos f&uuml;r Programmierer",
		"en" 	=> "Programmer info",
		"url"	=> "code" . $ext,
		"sub"	=> false),
  	11 => array("de"	=> "CVS Log",
		"en" 	=> "CVS log",
		"url"	=> "show" . $ext . "?file=http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt",
		"sub"	=> true));


?>
