{ $Id$

  Unicode translation table, Free Pascal conversion automatically created
  using unicode.tablecnv, (c) 2000 Sebastian Guenther (sg@freepascal.org)

  Comments of the original Unicode source file:
  
  	Name:             ISO/IEC 8859-10:1998 to Unicode
  	Unicode version:  3.0
  	Table version:    1.1
  	Table format:     Format A
  	Date:             1999 October 11
  	Authors:          Ken Whistler <kenw@sybase.com>
  
  	Copyright (c) 1999 Unicode, Inc.  All Rights reserved.
  
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
         ISO/IEC 8859-10:1998 characters map into Unicode.
  
  	Format:  Three tab-separated columns
  		 Column #1 is the ISO/IEC 8859-10 code (in hex as 0xXX)
  		 Column #2 is the Unicode (in hex as 0xXXXX)
  		 Column #3 the Unicode name (follows a comment sign, '#')
  
  	The entries are in ISO/IEC 8859-10 order.
  
  	Version history
  	1.0 version new.
         1.1 corrected mistake in mapping of 0xA4
  
  	Updated versions of this file may be found in:
  		<ftp://ftp.unicode.org/Public/MAPPINGS/>
  
  	Any comments or problems, contact <errata@unicode.org>
  	Please note that <errata@unicode.org> is an archival address;
  	notices will be checked, but do not expect an immediate response.
  
}

const
  ISO8859_10TransTable: T8BitTable = (
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
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7,   // $A0 - $A7
    $013B, $0110, $0160, $0166, $017D, $00AD, $016A, $014A,   // $A8 - $AF
    $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7,   // $B0 - $B7
    $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,   // $B8 - $BF
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,   // $C0 - $C7
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,   // $C8 - $CF
    $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168,   // $D0 - $D7
    $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,   // $D8 - $DF
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,   // $E0 - $E7
    $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,   // $E8 - $EF
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169,   // $F0 - $F7
    $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138);  // $F8 - $FF
