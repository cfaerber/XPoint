<?
/* $Id */

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

?>