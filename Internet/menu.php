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
		"en" 	=> "Informations",	// English
		"url"	=> "info" . $ext,	// url
		"sub"	=> false),		// sub menu?
	1 => array("de"	=> "Logs:",
		"en" 	=> "Logs:",
		"sub"	=> true),
	2 => array("de"	=> "3.20",
		"en" 	=> "3.20",
		"first"	=> true,		// a little workaround
		"url"	=> "show" . $ext . "?file=log320.html",
		"sub"	=> true),
	3 => array("de"	=> "3.30",
		"en" 	=> "3.30",
		"url"	=> "show" . $ext . "?file=log330.html",
		"sub"	=> true),
	4 => array("de"	=> "3.70",
		"en" 	=> "3.70",
		"url"	=> "show" . $ext . "?file=log370.html",
		"sub"	=> true),
	5 => array("de"	=> "CVS-Log",
		"en" 	=> "CVS-Log",
		"url"	=> "show" . $ext . "?file=http://fries7-73.stw.uni-jena.de/openxp/ChangeLog.txt",
		"sub"	=> true),
	6 => array("de"	=> "OpenXP Handbuch",
		"en" 	=> "OpenXP Manual",
		"url"	=> "show" . $ext . "?file=handbuch/index.html",
		"sub"	=> true),
	7 => array("de"	=> "Downloads",
		"en" 	=> "Downloads",
		"url"	=> "download" . $ext,
		"sub"	=> false),
	8 => array("de"	=> "Programmierer",
		"en" 	=> "Programmers",
		"url"	=> "programmer-" . $language . $ext,
		"sub"	=> false),
	9 => array("de"	=> "Hinweise",
		"en" 	=> "Hints",
		"url"	=> "code-" . $language . $ext,
		"sub"	=> true),
       10 => array("de"	=> "E-Mail",
		"en" 	=> "Mail",
		"url"	=> "mailto:info@openxp.de",
		"sub"	=> false));


?>