<?
/* $Id$ */

$ext	= '.php3';			// When ever update to php4, change this to '.php'
if (eregi("^gut", $SERVER_NAME)) {	// for my own purpose
	$ext = '.php';
};


if (eregi('.de$', $SERVER_NAME)) {
	// a 'de' a the end is a German tld
	header("Location: index-de" . $ext);
} else {
	// all other map to english
	header("Location: index-en" . $ext);
};
exit;
?>