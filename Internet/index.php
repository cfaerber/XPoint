<?
/* $Id */
if (eregi('.de$', $SERVER_NAME)) {
	// a 'de' a the end is a German tld
	header("Location: index-de.php");
} else {
	// all other map to english
	header("Location: index-en.php");
};
exit;
?>