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

	2 => array("de"	=> "FAQs",
		"en" 	=> "FAQs",
		"sub"	=> false),
	3 => array("de"	=> "Allgemein",
		"en" 	=> "General",
		"url"	=> $indexname . "?news=faq-de.txt&genindex=1",
		"sub"	=> true),
	4 => array("de"	=> "Lizenzen",
		"en" 	=> "Licenses",
		"url"	=> $indexname . "?news=faqgpl-de.txt&genindex=1",
		"sub"	=> true),

	5 => array("de"	=> "OpenXP/32",
		"en" 	=> "OpenXP/32",
		"sub"	=> false),
	6 => array("de"	=> "FAQ",
		"en" 	=> "FAQ",
		"url"	=> $indexname . "?news=faq32-de.txt&genindex=1",
		"sub"	=> true),
	7 => array("de"	=> "Setup-Infos",
		"en" 	=> "Infos for setup",
		"url"	=> $indexname . "?file=setup32-de.html",
		"sub"	=> true),
	8 => array("de"	=> "Download",
		"en" 	=> "Download",
		"url"	=> $indexname . "?file=download32-" . $language . ".html",
		"sub"	=> true),
	9 => array("de"	=> "Bugs",
		"en" 	=> "Bugs",
		"url"	=> "https://sourceforge.net/bugs/?group_id=3766",
		"sub"	=> true),
	10 => array("de"	=> "Logs",
		"en" 	=> "Logs",
		"url"	=> $indexname . "?file=log32.html",
		"sub"	=> true),

	11 => array("de"	=> "Kontakt",
		"en" 	=> "Contact",
		"url"	=> $indexname . "?file=contact-" . $language . ".html",
		"sub"	=> false),

	12 => array("de"	=> "Entwicklerinfos",
		"en" 	=> "Developer info",
		"url"	=> $indexname . "?file=code-" . $language . ".html",
		"sub"	=> false),
	13 => array("de"	=> "Hinweise",
		"en" 	=> "Hints",
		"url"	=> $indexname . "?news=hints-de.txt&genindex=1",
		"sub"	=> true),
	14 => array("de"	=> "CVSWeb",
		"en" 	=> "CVSWeb",
		"url"	=> "http://openxpcvs.dyndns.org/cvs/cvsweb.cgi/openxp/",
		"sub"	=> true),
	15 => array("de"	=> "CVS Log",
		"en" 	=> "CVS log",
		"url"	=> $indexname . "?bare=http://openxpcvs.dyndns.org/openxp/ChangeLog.txt",
		"sub"	=> true),

	16 => array("de"	=> "Links",
		"en" 	=> "Links",
		"url"	=> $indexname . "?file=links-" . $language . ".html",
		"sub"	=> false));
?>
