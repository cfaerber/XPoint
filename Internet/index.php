<?
/* $Id$ */

// Calling this file without parameters results in displaying home-$lang.html
// Calling with "?file=filename.html" results in displaying filename.html
// Calling with "?bare=filename.txt" results in displaying filename.txt (text mode)
// Calling with "?news=filename.txt?genindex=1" results in displaying interpreted news from filename.txt

$ext	= '.php3';			// When ever update to php4, change this to '.php'
if (eregi("^gut", $SERVER_NAME)) {	// for my own purpose
	$ext = '.php';
};

if (eregi('.de$', $SERVER_NAME)) {
	$language="de";
} else {
	$language="en";
};

require("menu.php");
require("webtools.php");
ShowHeader("OpenXP Online");

if ($file!="")
  include($file);
else
  if ($bare!="")
    { echo("<plaintext>"); include($bare); echo("</plaintext>"); }
  else
    if ($news!="")
      ShowNews($news,$genindex);
    else
      include("main-".$language.".html");

ShowFooter();
?>
