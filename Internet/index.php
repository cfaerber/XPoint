<?
/* $Id$ */

// Calling this file without parameters results in displaying home-$lang.html
// Calling this file with "?file=filename.html" results in displaying filename.html
// Calling this file with "?news=filename.txt?genindex=1" results in displaying interpreted news from filename.txt

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

if ($file=="") {
  if ($news=="")
    include("main-".$language.".html");
  else {
    ShowNews($news,$genindex);
  }
} else {
  include($file);
}

ShowFooter();
?>
