<?
/* $Id$ */

// Calling this file without parameters results in displaying home-$lang.html
// Calling with "?file=filename.html" results in displaying filename.html
// Calling with "?bare=filename.txt" results in displaying filename.txt (text mode)
// Calling with "?news=filename.txt&genindex=1" results in displaying interpreted news from filename.txt

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
else if ($bare!="") { 
  set_time_limit(240);
  if($bare_fp = fopen($bare,"r")) {
    echo("<pre>"); 
    while($bare_d = fread($bare_fp,8192)) {
      set_time_limit(120);
      echo htmlspecialchars(ereg_replace("[^\x09\x0D\x0A\x20-\x7F\xA0-\xFF]","?",$bare_d)); }
    echo("</pre>");
  }
}
else if ($news!="") {
  ShowNews($news,$genindex);
}
else {
  include("main-".$language.".html");
}

ShowFooter();

?>
