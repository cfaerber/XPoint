{ $Id: cp1251.inc,v 1.1 2003/09/29 20:47:20 cl Exp $

  Unicode translation table, Free Pascal conversion automatically created
  using unicode.tablecnv, (c) 2000 Sebastian Guenther (sg@freepascal.org)

  Comments of the original Unicode source file:
  
      Name:     cp1251 to Unicode table
      Unicode version: 2.0
      Table version: 2.01
      Table format:  Format A
      Date:          04/15/98
  
      Contact:       cpxlate@microsoft.com
  
      General notes: none
  
      Format: Three tab-separated columns
          Column #1 is the cp1251 code (in hex)
          Column #2 is the Unicode (in hex as 0xXXXX)
          Column #3 is the Unicode name (follows a comment sign, '#')
  
      The entries are in cp1251 order
  
}

const
  CP1251TransTable: T8BitTable = (
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
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021,   // $80 - $87
    $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,   // $88 - $8F
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014,   // $90 - $97
    $0000, $2122, $0459, $203A, $045A, $045C, $045B, $045F,   // $98 - $9F
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7,   // $A0 - $A7
    $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,   // $A8 - $AF
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7,   // $B0 - $B7
    $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,   // $B8 - $BF
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,   // $C0 - $C7
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,   // $C8 - $CF
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,   // $D0 - $D7
    $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,   // $D8 - $DF
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,   // $E0 - $E7
    $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,   // $E8 - $EF
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,   // $F0 - $F7
    $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F);  // $F8 - $FF
