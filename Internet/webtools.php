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
	global $language, $Menu, $Links;
	
	// it's enough HTML 3.2
	echo("<!doctype html public \"-//W3C//DTD HTML 3.2 Final//EN\">\n");
	echo("<html>\n<head>\n");
	echo("<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>\n");
	// set the right lanuage
	echo("<meta http-equiv='Content-Language' content='" . $language . "'>\n");
    	echo("<meta http-equiv='expires' content='0'>\n");
    	echo("<meta name='copyright' content='Copyright by OpenXP-Team, &copy; 1999-" . date("Y") . ", all rights reserved!'>\n");
	// depends on the language
	if ($language == "de") {
    		echo("<meta name='description' content='Information zu einem Point-Programm'>\n");
		echo("<meta name='Keywords' content='mail,news,email,point,fido,zconnect,maus,rfc,mua,newsreader'>\n");
	} else {
    		echo("<meta name='description' content='Informations for a Point-Program'>\n");
		echo("<meta name='Keywords' content='mail,news,email,point,fido,zconnect,maus,rfc,mua,newsreader'>\n");
	};
	// we like robots because of the web-directories
    	echo("<meta name='robots' content='index'>\n");
	echo("<title>$title</title>\n</head>\n");
	// now the body follows
	echo("<body bgcolor='White' text='Black' leftmargin=10 topmargin=10>\n");
	echo("<a name='top'>");
	echo("<table width='100%'><td width=75 align='right' valign='bottom'>&nbsp;</td>\n");
	if ($language == "de")
	  echo("<tr><td align='center' valign='middle'><h1>OpenXP Projekt</h1></td>\n");
	else
	  echo("<tr><td align='center' valign='middle'><h1>OpenXP project</h1></td>\n");
	echo("<td width=75 align='right' valign='bottom'><small>");
	// link to the other language
	if ($language == "de") {
		echo("<a href='http://www.openxp.com/'>English</a>");
	} else {
		echo("<a href='http://www.openxp.de/'>Deutsch</a>");
	};
	echo("</small></td></tr></table></a><hr color='Blue' noshade size=1>\n");
	// now opening the main table
	echo("<table width='100%' border=0 cellspacing=0 cellpadding=5>\n");
	// <!-- Selection Part -->
	echo("<tr><td align='left' valign='top' width=200>\n");
	// now underlaying a blue table and then set the header
	echo("<table width='100%' border=0 cellpadding=0 bgcolor='Blue'><tr><td><table width='100%' border=0 cellpadding=4>\n");
	echo("<tr bgcolor='Yellow'><th align='center'>Site Map</th></tr><tr bgcolor='White'><td align='left'>\n");
	// it's open, so we have to set up the menu
	echo("<dl>"); // first element should not be a sub-element!
	$InSub = false;
	reset($Menu);
	do {
		$Item = current($Menu);
		// trigger dd or dt
		if ($Item["sub"]) {
			if (!$InSub) {
				$InSub = true;
				echo("<dd><small>");
			} else {
				if (!isset($Item["first"])) echo(",");
			};
		} else {
			if ($InSub) {
				$InSub = false;
				echo("</small>");
			};
			echo("<dt>");
		};
		// show the item
		if (isset($Item["url"])) {
			echo(" <a href='" . $Item["url"] . "'>" . $Item[$language] . "</a>");
		} else {
			echo(" " . $Item[$language]);
		};
		echo("\n");
		// fetch next element
	} while (next($Menu) != false);
	// that was the menu
	echo("</dl></td></tr></table></td></tr></table>&nbsp;<br>\n");
	// Now very simular for the links
	echo("<table width='100%' border=0 cellpadding=0 bgcolor='Blue'><tr><td><table width='100%' border=0 cellpadding=4>\n");
	echo("<tr bgcolor='Yellow'><th align='center'>Links</th></tr><tr bgcolor='White'><td align='left'>\n");
	// it's open, so we have to set up the menu
	$Item = reset($Links);
	while ($Item != false) {
		if (isset($Item["url"])) {
			echo("<a href='" . $Item["url"] . "'>" . $Item["name"] . "</a><br>\n");
		} else {
			echo("<i>" . $Item["name"] . "</i><br>\n");
		};
		$Item = next($Links);
	};
	// that's it. now closing that left cell and go to main part
	echo("</td></tr></table></td></tr></table></td><td align='left' valign='top'>\n");
	// Document ist prepared now
}; // ShowHeader


// concludes the HTML page
function ShowFooter() {
	global $language;
	echo("</td></tr></table><hr color='Blue' noshade size=1>");
	echo("<table width='100%'><tr><td align='left'>");
	if ($language == 'de') {
		echo("<a href='#top'>Seitenanfang</a>\n");
	} else {
		echo("<a href='#top'>Top</a>\n");
	};
	echo("</td><td align='right'>");
	echo("<small>Published by the <a href='mailto:info@openxp.de'>OpenXP-Team</a></small>\n");
	// close the doc
	echo("</td></tr></table></body>\n</html>\n");
};


// generates a contact table from a file
function ShowContactTable($tablefile) {
	global $language;
	// generate table header
	echo("<table width=\"100%\" border=\"0\" cellspacing=\"1\" cellpadding=\"2\">");
	echo("<tr><th nowrap width=\"20%\" bgcolor=\"#EEEE00\">Name/Homepage");
	echo("<th width=\"5%\" bgcolor=\"#EEEE00\"><div align=\"center\">K&uuml;rzel</div>");
	echo("<th nowrap width=\"75%\" bgcolor=\"#EEEE00\">Aufgabenfeld"); 

	$ptfile = fopen($tablefile,"r");
	if ($ptfile==false) return 0;
	// interpret input file entries
	while (!feof($ptfile)) {
	  echo("<tr bgcolor=\"#e0e0e0\">");
	  // Name/homepage
	  echo("<td nowrap>".fgets($ptfile,120));
	  // Short/mail
	  echo("<td nowrap><div align=\"center\">".fgets($ptfile,150));
	  // Job
	  $job=fgets($ptfile,150);
	  if ($language=="en") { $job=fgets($ptfile,150); } else { fgets($ptfile,150); }
	  echo("<td nowrap>".$job);
	  // skip empty line
	  fgets($ptfile,10);
	}
	fclose($ptfile);

	// generate table end
	echo("</table>");
}

// generates a contact table from a file
function ShowFeatureList($tablefile) {
	global $language;

	// generate table header
	echo("<table border=\"0\" cellspacing=\"1\" cellpadding=\"2\">");
	echo("<tr><th align=\"left\" nowrap bgcolor=\"#EEEE00\">Feature");
	echo("<th nowrap bgcolor=\"#EEEE00\">3.20");
	echo("<th nowrap bgcolor=\"#EEEE00\">3.40");
	echo("<th nowrap bgcolor=\"#EEEE00\">3.70");

	$ptfile = fopen($tablefile,"r");
	if ($ptfile==false) return 0;
	// interpret input file entries
	while (!feof($ptfile)) {
	  echo("<tr bgcolor=\"#e0e0e0\">");
	  // Feature
	  $feature=fgets($ptfile,200);
	  if ($language=="en") { $feature=fgets($ptfile,200); } else { fgets($ptfile,200); }
	  echo("<td nowrap>".$feature);
	  // Supported in versions...
	  echo("<td nowrap><div align=\"center\">".fgets($ptfile,20));
	  echo("<td nowrap><div align=\"center\">".fgets($ptfile,20));
	  echo("<td nowrap><div align=\"center\">".fgets($ptfile,20));
	  fgets($ptfile,10);
	}
	fclose($ptfile);

	// generate table end
	echo("</table>");
}

// generates a news page from a file
function ShowNews($newsfile,$genindex) {
	// generate index if necessary
	if ($genindex==1) {
	  $iarticle = 0;
	  $pnfile = fopen($newsfile,"r");
	  if ($pnfile==false) return 0;
	  echo("<ul>");
	  while (!feof($pnfile)) {
	    $iarticle++;
	    $headline=fgets($pnfile,200);
	    echo("<li><a href=\"#art".$iarticle."\">".$headline."</a>");
	    do {
	      $headline=fgets($pnfile,1000);
	    } while((trim($headline)!="")and(!feof($pnfile)));
	  }
	  fclose($pnfile);
	  echo("</ul>");
	}
	
	// show articles
	$iarticle = 0;
	$pnfile = fopen($newsfile,"r");
	if ($pnfile==false) return 0;
	while (!feof($pnfile)) {
	  $iarticle++;
	  $headline=fgets($pnfile,200);
	  echo("<a name=\"art".$iarticle."\"><h3>".$headline."</h3></a>");
	  do {
	    $headline=fgets($pnfile,1000);
	    if($headline!="") echo($headline);
	  } while((trim($headline)!="")and(!feof($pnfile)));
	}
	fclose($pnfile);
}

// inserts only first entry from newsfile
function InsertLatestNews($newsfile) {
	$pnfile = fopen($newsfile,"r");
	if ($pnfile==false) return 0;
	echo("<b>".fgets($pnfile,200)."</b>&nbsp;");
	do {
	  $news=fgets($pnfile,1000);
	  echo($news);
	} while((trim($news)!="")and(!feof($pnfile)));
	fclose($pnfile);
}

// show download table
function ShowDownloadTable($downfile) {
	global $language;
	$pdfile = fopen($downfile,"r");
	if ($pdfile==false) return 0;
	while(!feof($pdfile)) {
	  $line=fgets($pdfile,300);
	  if(strpos(" ".$line,"*")==1) { // headline
	    if($language=="de") {
	      echo(substr($line,1)."<p>");
	      fgets($pdfile,300);
	    } else {
	      $line=fgets($pdfile,300);
	      echo($line."<p>");
	    }
	  } else {
	    $fs=GetFileSize($line);
	    if($language=="de") {
	      echo("<a href=\"ftp://ftp.openxp.de".$line."\">".fgets($pdfile,200)."</a> ".$fs);
	      fgets($pdfile,200);
	    } else {
	      fgets($pdfile,200);
	      echo("<a href=\"ftp://ftp.openxp.de".$line."\">".fgets($pdfile,200)."</a> ".$fs);
	    }
	    fgets($pdfile,20); // skip empty line
	    echo("<br>");
	  }
	}
	fclose($pdfile);
}


?>





