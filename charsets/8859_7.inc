{ $Id$

  Unicode translation table, Free Pascal conversion automatically created
  using unicode.tablecnv, (c) 2000 Sebastian Guenther (sg@freepascal.org)

  Comments of the original Unicode source file:
  
  	Name:             ISO 8859-7:1987 to Unicode
  	Unicode version:  3.0
  	Table version:    1.0
  	Table format:     Format A
  	Date:             1999 July 27
  	Authors:          Ken Whistler <kenw@sybase.com>
  
  	Copyright (c) 1991-1999 Unicode, Inc.  All Rights reserved.
  
  	This file is provided as-is by Unicode, Inc. (The Unicode Consortium).
  	No claims are made as to fitness for any particular purpose.  No
  	warranties of any kind are expressed or implied.  The recipient
  	agrees to determine applicability of information provided.  If this
  	file has been provided on optical media by Unicode, Inc., the sole
  	remedy for any claim will be exchange of defective media within 90
  	days of receipt.
  
  	Unicode, Inc. hereby grants the right to freely use the information
  	supplied in this file in the creation of products supporting the
  	Unicode Standard, and to make copies of this file in any form for
  	internal or external distribution as long as this notice remains
  	attached.
  
  	General notes:
  
  	This table contains the data the Unicode Consortium has on how
         ISO 8859-7:1987 characters map into Unicode.
  
  	ISO 8859-7:1987 is equivalent to ISO-IR-126, ELOT 928,
  	and ECMA 118.
  
  	Format:  Three tab-separated columns
  		 Column #1 is the ISO 8859-7 code (in hex as 0xXX)
  		 Column #2 is the Unicode (in hex as 0xXXXX)
  		 Column #3 the Unicode name (follows a comment sign, '#')
  
  	The entries are in ISO 8859-7 order.
  
  	Version history
  	1.0 version updates 0.1 version by adding mappings for all
  	control characters.
  	Remap 0xA1 to U+2018 (instead of 0x02BD) to match text of 8859-7
  	Remap 0xA2 to U+2019 (instead of 0x02BC) to match text of 8859-7
  
  	Updated versions of this file may be found in:
  		<ftp://ftp.unicode.org/Public/MAPPINGS/>
  
  	Any comments or problems, contact <errata@unicode.org>
  	Please note that <errata@unicode.org> is an archival address;
  	notices will be checked, but do not expect an immediate response.
  
}

const
  ISO8859_7TransTable: T8BitTable = (
    $0000, $0001, $0002, $0003, $0004, $0005, $0006, $0007,   // $00 - $07
    $0008, $0009, $000A, $000B, $000C, $000D, $000E, $000F,   // $08 - $0F
    $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017,   // $10 - $17
    $0018, $0019, $001A, $001B, $001C, $001D, $001E, $001F,   // $18 - $1F
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,   // $20 - $27
    $0028, $0029, $002A, $002B, $002C, $002D, $002E, $002F,   // $28 - $2F
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,   // $30 - $37
    $0038, $0039, $003A, $003B, $003C, $003D, $003E, $003F,   // $38 - $3F
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,   // $40 - $47
    $0048, $0049, $004A, $004B, $004C, $004D, $004E, $004F,   // $48 - $4F
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,   // $50 - $57
    $0058, $0059, $005A, $005B, $005C, $005D, $005E, $005F,   // $58 - $5F
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,   // $60 - $67
    $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F,   // $68 - $6F
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,   // $70 - $77
    $0078, $0079, $007A, $007B, $007C, $007D, $007E, $007F,   // $78 - $7F
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,   // $80 - $87
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,   // $88 - $8F
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,   // $90 - $97
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,   // $98 - $9F
    $00A0, $2018, $2019, $00A3, $0000, $0000, $00A6, $00A7,   // $A0 - $A7
    $00A8, $00A9, $0000, $00AB, $00AC, $00AD, $0000, $2015,   // $A8 - $AF
    $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7,   // $B0 - $B7
    $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,   // $B8 - $BF
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,   // $C0 - $C7
    $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,   // $C8 - $CF
    $03A0, $03A1, $0000, $03A3, $03A4, $03A5, $03A6, $03A7,   // $D0 - $D7
    $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,   // $D8 - $DF
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,   // $E0 - $E7
    $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,   // $E8 - $EF
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,   // $F0 - $F7
    $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $0000);  // $F8 - $FF
