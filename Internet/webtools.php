<?
/* $Id$ */

// inserts ftp file size
function GetFileSize($fn) {
	$fh = ftp_connect('ftp.openxp.de');
	if ($fh) {
		if (ftp_login($fh,"anonymous","webmaster@openxp.de")) {
			$result = sprintf ("(%01.2f MB)", (ftp_size($fh, $fn)/1024/1024));

		};
		ftp_quit($fh);
	};
	return $result;
};

// inserts header (including navigation menu)
function ShowHeader($title) {
	global $language, $Menu;

	header ("Content-Type: text/html; charset=iso-8859-1");
	header ("Content-Language: ".$language);
	
	// now we use XHTML 1.0
	echo("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\">\n");
	echo("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"".$language."\" lang=\"".$language."\">\n");
	echo("<head>\n");
	// depends on the language
	if ($language == "de") {
		echo("<meta name=\"copyright\" content=\"Copyright by OpenXP-Team, &copy; 1999-" . htmlspecialchars(date("Y")) . ", all rights reserved\" />\n");
    		echo("<meta name=\"description\" content=\"Information zu einem Point-Programm\" />\n");
	} else {
		echo("<meta name=\"copyright\" content=\"Copyright by OpenXP Team, &copy; 1999-" . htmlspecialchars(date("Y")) . ", all rights reserved\" />\n");
		echo("<meta name=\"description\" content=\"Information on a point program\" />\n");
	};
	echo("<meta name=\"keywords\" content=\"crosspoint,xp,mail,news,email,point,fido,zconnect,maus,rfc,mua,newsreader\" />\n");
	// we like robots because of the web-directories
    	echo("<meta name=\"robots\" content=\"index\" />\n");
	echo("<title>".htmlspecialchars($title)."</title>\n</head>\n\n");

	// now the body follows
	echo("<body bgcolor=\"white\" text=\"black\">\n");
	echo("\n<table width=\"100%\"><tr>\n<td width=\"75\">&nbsp;</td>");
	// link to the other language
	if ($language == "de") {
	  echo("\n<td align=\"center\"><h1><a name=\"top\">OpenXP-Projekt</a></h1></td>");
	  echo("\n<td width=\"75\" align=\"right\">");
	  echo("<small><a href=\"http://www.openxp.com/\">English</a></small>");
	} else {
	  echo("\n<td align=\"center\"><h1><a name=\"top\">OpenXP Project</a></h1></td>");
	  echo("\n<td width=\"75\" align=\"right\">");
	  echo("<small><a href=\"http://www.openxp.de/\">Deutsch</a></small>");
	};
	echo("</td></tr></table>");
	echo("<hr noshade=\"noshade\" size=\"1\" />");

	// open main table
	echo("\n<table border=\"0\" cellspacing=\"0\" cellpadding=\"10\"><tr>");
	// build site map
	echo("\n<td align=\"left\" valign=\"top\" width=\"140\">");
	// now underlaying a blue table and then set the header
	echo("\n<table width=\"100%\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" bgcolor=\"blue\"><tr>");
	echo("\n<td>\n<table width=\"100%\" border=\"0\" cellpadding=\"4\"><tr bgcolor=\"yellow\">");
	echo("\n<th align=\"center\">Site Map</th>\n</tr>\n<tr bgcolor=\"white\">\n<td align=\"left\">");
	echo("\n<dl>"); // start definition list
	$InSub = false;
	reset($Menu);
	do {
		$Item = current($Menu);
		if ($Item["sub"]) {
			echo("\n<dd><small>"); $closeitem = "</small></dd>";
		} else {
			echo("\n<dt>"); $closeitem = "</dt>";
		};
		// show the item
		if (isset($Item["url"])) {
			echo("<a href=\"" . htmlspecialchars($Item["url"]) . "\">" . htmlspecialchars($Item[$language]) . "</a>");
		} else {
			echo(htmlspecialchars($Item[$language]));
		};
		echo($closeitem);
		// fetch next element
	} while (next($Menu) != false);
	// that's it. now close left cell and go to main part
	echo("</dl>\n</td></tr>\n</table>");
	echo("</td></tr></table>\n</td>\n<td align=\"left\" valign=\"top\">\n");
	// Document is prepared now
}; // ShowHeader


// concludes the HTML page
function ShowFooter() {
	global $language;
        echo("\n</td></tr><tr>");
	echo("\n</td></tr></table>\n<hr noshade=\"noshade\" size=\"1\" />");
	echo("\n<table width=\"100%\"><tr>\n<td align=\"left\" width=\"25%\">");
	if ($language == "de") {
		echo("<a href=\"#top\">Seitenanfang</a>\n");
	} else {
		echo("<a href=\"#top\">Top</a>\n");
	};
        echo("\n</td><td align=\"center\" width=\"50%\"><a href=\"http://sourceforge.net\"><img src=\"http://sourceforge.net/sflogo.php?group_id=3766&amp;type=1\" width=\"88\" height=\"31\" border=\"0\" alt=\"SourceForge\" /></a>"); 
	echo("</td><td align=\"right\" width=\"25%\">");
	echo("<small>Published by <a href=\"mailto:dev@openxp.de\">OpenXP Team</a></small>\n");
	// close the doc
	echo("</td></tr></table></body>\n</html>\n");
};


// generates a contact table from a file
function ShowContactTable($tablefile) {
	global $language;
	// generate table header
	echo("<table width=\"100%\" border=\"0\" cellspacing=\"1\" cellpadding=\"2\"><tr>");
	echo("\n<th nowrap=\"nowrap\" width=\"20%\" bgcolor=\"#EEEE00\">Name/Homepage</th>");
	echo("\n<th width=\"5%\" bgcolor=\"#EEEE00\" align=\"center\">K&uuml;rzel</th>");
	echo("\n<th nowrap=\"nowrap\" width=\"75%\" bgcolor=\"#EEEE00\">Aufgabenfeld</th></tr>"); 

	$ptfile = fopen($tablefile,"r");
	if ($ptfile==false) return 0;
	// interpret input file entries
	while (!feof($ptfile)) {
	  echo("\n<tr bgcolor=\"#e0e0e0\">");
	  // Name/homepage
	  echo("\n<td nowrap=\"nowrap\">".fgets($ptfile,120)."</td>");
	  // Short/mail
	  echo("\n<td nowrap=\"nowrap\" align=\"center\">".fgets($ptfile,150)."</td>");
	  // Job
	  $job=fgets($ptfile,150);
	  if ($language=="en") { $job=fgets($ptfile,150); } else { fgets($ptfile,150); }
	  echo("\n<td nowrap=\"nowrap\">".$job."</td></tr>");
	  // skip empty line
	  fgets($ptfile,10);
	}
	fclose($ptfile);

	// generate table end
	echo("\n</table>\n");
}

// generates feature list from file
function ShowFeatureList($tablefile) {
	global $language;

	// generate table header
	echo("<table border=\"0\" cellspacing=\"1\" cellpadding=\"2\"><tr>\n");
	echo("<th nowrap=\"nowrap\" bgcolor=\"#EEEE00\">Feature</th>");
	echo("\n<th nowrap=\"nowrap\" bgcolor=\"#EEEE00\">3.20</th>");
	echo("\n<th nowrap=\"nowrap\" bgcolor=\"#EEEE00\">3.40</th>");
	echo("\n<th nowrap=\"nowrap\" bgcolor=\"#EEEE00\">3.70</th></tr>");

	$ptfile = fopen($tablefile,"r");
	if ($ptfile==false) return 0;
	// interpret input file entries
	while (!feof($ptfile)) {
	  echo("\n<tr bgcolor=\"#e0e0e0\">");
	  // Feature
	  $feature=fgets($ptfile,200);
	  if ($language=="en") { $feature=fgets($ptfile,200); } else { fgets($ptfile,200); }
	  echo("\n<td nowrap=\"nowrap\">".$feature."</td>");
	  // Supported in versions...
	  echo("\n<td nowrap=\"nowrap\" align=\"center\">".fgets($ptfile,20)."</td>");
	  echo("\n<td nowrap=\"nowrap\" align=\"center\">".fgets($ptfile,20)."</td>");
	  echo("\n<td nowrap=\"nowrap\" align=\"center\">".fgets($ptfile,20)."</td></tr>");
	  fgets($ptfile,10);
	}
	fclose($ptfile);

	// generate table end
	echo("</table>");
}

// generates a news page from a file
function ShowNews($newsfile,$genindex) {
	// show overall headline
	$pnfile = fopen($newsfile,"r");
	if ($pnfile==false) return 0;
	echo("\n<h2>".fgets($pnfile,200)."</h2>");
	fgets($pnfile,10);

	// generate index if necessary
	if ($genindex==1) {
	  $iarticle = 0;
	  echo("\n<ul>");
	  while (!feof($pnfile)) {
	    $iarticle++;
	    $headline=fgets($pnfile,200);
	    echo("\n<li><a href=\"#art".$iarticle."\">".$headline."</a></li>");
	    do {
	      $headline=trim(fgets($pnfile,1000));
	    } while(($headline!="")and(!feof($pnfile))and($headline!="+"));
	  }
	  echo("\n</ul>\n<hr noshade=\"noshade\" size=\"1\" />");
	  rewind($pnfile);
	  fgets($pnfile,200); fgets($pnfile,10);
	}
	
	// show articles
	$iarticle = 0;
	while (!feof($pnfile)) {
	  $iarticle++;
	  $headline=fgets($pnfile,200);
	  echo("\n<h3><a name=\"art".$iarticle."\">".$headline."</a></h3>\n");
	  do {
	    $headline=fgets($pnfile,1000);
	    $headline=trim($headline);
	    if(($headline!="")and($headline!="+")) echo($headline."\n");
	  } while(($headline!="")and(!feof($pnfile))and($headline!="+"));
	}
	fclose($pnfile);
}

// inserts only first entry from newsfile
function InsertLatestNews($newsfile) {
	$pnfile = fopen($newsfile,"r");
	if ($pnfile==false) return 0;
	fgets($pnfile,200); fgets($pnfile,10); // skip headline + space
	echo("\n<p><big>".trim(fgets($pnfile,200))."</big>\n<br /><small>");
	do {
	  $news=trim(fgets($pnfile,1000));
	  if ($news=="+") {
	    echo("</small></p>\n<p><big>".trim(fgets($pnfile,200))."</big>\n<br /><small>");
	  } else { 
  	    echo($news)."\n";
	  }
	} while(($news!="")and(!feof($pnfile)));
      echo("</small></p>\n");
	echo("<hr noshade=\"noshade\" size=\"1\" />");
	fclose($pnfile);
}

// show download table
function ShowDownloadTable($downfile) {
	global $language;

	$pdfile = fopen($downfile,"r");
	if ($pdfile==false) return 0;

	// open ftp connection for getting file sizes
	$fhandle = ftp_connect('ftp.openxp.de');
	if ($fhandle) {
	  if (!ftp_login($fhandle,"anonymous","webmaster@openxp.de"))
	    ftp_quit($fhandle);
	}

        $popen=false;
	echo("\n");
	while(!feof($pdfile)) {
	  $line=fgets($pdfile,300);
	  if(strpos(" ".$line,"*")==1) { // headline
	    if($popen) { echo("</p>\n"); $popen=false; };
	    if($language=="de") {
	      echo("<h3>".substr($line,1)."</h3>");
	      fgets($pdfile,300);
	    } else {
	      $line=fgets($pdfile,300);
	      echo("<h3>".$line."</h3>");
	    }
	  } else {
	    if ($fhandle)
	      $fsize = sprintf("%01.2f MB", (ftp_size($fhandle, $line)/1024/1024));
	    else
	      $fsize = ""; // no ftp connection made

            if($popen) {
	      echo("<br />\n");
	    } else {
              echo("\n<p>"); $popen=true;
	    }
	      
	    if($language=="de") {
	      $fdesc=fgets($pdfile,200);
	      fgets($pdfile,200);
	    } else {
	      fgets($pdfile,200);
	      $fdesc=fgets($pdfile,200);
	    }
	    echo("\n".htmlspecialchars($fdesc));
	    echo(" (<a href=\"".htmlspecialchars("ftp://ftp.openxp.de".$line)."\">FTP</a>/");
	    echo("<a href=\"".htmlspecialchars("http://www.happyarts.de/ftp".$line)."\">HTTP</a>, ".$fsize.")");
	    fgets($pdfile,20); // skip empty line
	  }
	}

	if($popen) { echo("</p>\n"); $popen=false; };

	if ($fhandle) ftp_quit($fhandle);
	fclose($pdfile);
}
?>
