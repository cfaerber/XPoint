//   $Id$
//
//   Name:     cp857_DOSTurkish to Unicode table
//   Unicode version: 2.0
//   Table version: 2.00
//   Table format:  Format A
//   Date:          04/24/96
//   Authors:       Lori Brownell <loribr@microsoft.com>
//                  K.D. Chang    <a-kchang@microsoft.com>
//   General notes: none
//
//   Format: Three tab-separated columns
//       Column #1 is the cp857_DOSTurkish code (in hex)
//       Column #2 is the Unicode (in hex as 0xXXXX)
//       Column #3 is the Unicode name (follows a comment sign, '#')
//
//   The entries are in cp857_DOSTurkish order
//

const
  CP857TransTable: T8BitTable = ( {
$00 } $0000, { NULL
$01 } $0001, { START OF HEADING
$02 } $0002, { START OF TEXT
$03 } $0003, { END OF TEXT
$04 } $0004, { END OF TRANSMISSION
$05 } $0005, { ENQUIRY
$06 } $0006, { ACKNOWLEDGE
$07 } $0007, { BELL
$08 } $0008, { BACKSPACE
$09 } $0009, { HORIZONTAL TABULATION
$0a } $000a, { LINE FEED
$0b } $000b, { VERTICAL TABULATION
$0c } $000c, { FORM FEED
$0d } $000d, { CARRIAGE RETURN
$0e } $000e, { SHIFT OUT
$0f } $263C, { 
$10 } $25BA, { 
$11 } $25C4, { 
$12 } $0012, { DEVICE CONTROL TWO
$13 } $203C, { 
$14 } $0014, { DEVICE CONTROL FOUR
$15 } $0015, { NEGATIVE ACKNOWLEDGE
$16 } $25AC, {
$17 } $0017, { END OF TRANSMISSION BLOCK
$18 } $0018, { CANCEL
$19 } $0019, { END OF MEDIUM
$1a } $001a, { SUBSTITUTE
$1b } $001b, { ESCAPE
$1c } $001c, { FILE SEPARATOR
$1d } $001d, { GROUP SEPARATOR
$1e } $001e, { RECORD SEPARATOR
$1f } $001f, { UNIT SEPARATOR
$20 } $0020, { SPACE
$21 } $0021, { EXCLAMATION MARK
$22 } $0022, { QUOTATION MARK
$23 } $0023, { NUMBER SIGN
$24 } $0024, { DOLLAR SIGN
$25 } $0025, { PERCENT SIGN
$26 } $0026, { AMPERSAND
$27 } $0027, { APOSTROPHE
$28 } $0028, { LEFT PARENTHESIS
$29 } $0029, { RIGHT PARENTHESIS
$2a } $002a, { ASTERISK
$2b } $002b, { PLUS SIGN
$2c } $002c, { COMMA
$2d } $002d, { HYPHEN-MINUS
$2e } $002e, { FULL STOP
$2f } $002f, { SOLIDUS
$30 } $0030, { DIGIT ZERO
$31 } $0031, { DIGIT ONE
$32 } $0032, { DIGIT TWO
$33 } $0033, { DIGIT THREE
$34 } $0034, { DIGIT FOUR
$35 } $0035, { DIGIT FIVE
$36 } $0036, { DIGIT SIX
$37 } $0037, { DIGIT SEVEN
$38 } $0038, { DIGIT EIGHT
$39 } $0039, { DIGIT NINE
$3a } $003a, { COLON
$3b } $003b, { SEMICOLON
$3c } $003c, { LESS-THAN SIGN
$3d } $003d, { EQUALS SIGN
$3e } $003e, { GREATER-THAN SIGN
$3f } $003f, { QUESTION MARK
$40 } $0040, { COMMERCIAL AT
$41 } $0041, { LATIN CAPITAL LETTER A
$42 } $0042, { LATIN CAPITAL LETTER B
$43 } $0043, { LATIN CAPITAL LETTER C
$44 } $0044, { LATIN CAPITAL LETTER D
$45 } $0045, { LATIN CAPITAL LETTER E
$46 } $0046, { LATIN CAPITAL LETTER F
$47 } $0047, { LATIN CAPITAL LETTER G
$48 } $0048, { LATIN CAPITAL LETTER H
$49 } $0049, { LATIN CAPITAL LETTER I
$4a } $004a, { LATIN CAPITAL LETTER J
$4b } $004b, { LATIN CAPITAL LETTER K
$4c } $004c, { LATIN CAPITAL LETTER L
$4d } $004d, { LATIN CAPITAL LETTER M
$4e } $004e, { LATIN CAPITAL LETTER N
$4f } $004f, { LATIN CAPITAL LETTER O
$50 } $0050, { LATIN CAPITAL LETTER P
$51 } $0051, { LATIN CAPITAL LETTER Q
$52 } $0052, { LATIN CAPITAL LETTER R
$53 } $0053, { LATIN CAPITAL LETTER S
$54 } $0054, { LATIN CAPITAL LETTER T
$55 } $0055, { LATIN CAPITAL LETTER U
$56 } $0056, { LATIN CAPITAL LETTER V
$57 } $0057, { LATIN CAPITAL LETTER W
$58 } $0058, { LATIN CAPITAL LETTER X
$59 } $0059, { LATIN CAPITAL LETTER Y
$5a } $005a, { LATIN CAPITAL LETTER Z
$5b } $005b, { LEFT SQUARE BRACKET
$5c } $005c, { REVERSE SOLIDUS
$5d } $005d, { RIGHT SQUARE BRACKET
$5e } $005e, { CIRCUMFLEX ACCENT
$5f } $005f, { LOW LINE
$60 } $0060, { GRAVE ACCENT
$61 } $0061, { LATIN SMALL LETTER A
$62 } $0062, { LATIN SMALL LETTER B
$63 } $0063, { LATIN SMALL LETTER C
$64 } $0064, { LATIN SMALL LETTER D
$65 } $0065, { LATIN SMALL LETTER E
$66 } $0066, { LATIN SMALL LETTER F
$67 } $0067, { LATIN SMALL LETTER G
$68 } $0068, { LATIN SMALL LETTER H
$69 } $0069, { LATIN SMALL LETTER I
$6a } $006a, { LATIN SMALL LETTER J
$6b } $006b, { LATIN SMALL LETTER K
$6c } $006c, { LATIN SMALL LETTER L
$6d } $006d, { LATIN SMALL LETTER M
$6e } $006e, { LATIN SMALL LETTER N
$6f } $006f, { LATIN SMALL LETTER O
$70 } $0070, { LATIN SMALL LETTER P
$71 } $0071, { LATIN SMALL LETTER Q
$72 } $0072, { LATIN SMALL LETTER R
$73 } $0073, { LATIN SMALL LETTER S
$74 } $0074, { LATIN SMALL LETTER T
$75 } $0075, { LATIN SMALL LETTER U
$76 } $0076, { LATIN SMALL LETTER V
$77 } $0077, { LATIN SMALL LETTER W
$78 } $0078, { LATIN SMALL LETTER X
$79 } $0079, { LATIN SMALL LETTER Y
$7a } $007a, { LATIN SMALL LETTER Z
$7b } $007b, { LEFT CURLY BRACKET
$7c } $007c, { VERTICAL LINE
$7d } $007d, { RIGHT CURLY BRACKET
$7e } $007e, { TILDE
$7f } $007f, { DELETE
$80 } $00c7, { LATIN CAPITAL LETTER C WITH CEDILLA
$81 } $00fc, { LATIN SMALL LETTER U WITH DIAERESIS
$82 } $00e9, { LATIN SMALL LETTER E WITH ACUTE
$83 } $00e2, { LATIN SMALL LETTER A WITH CIRCUMFLEX
$84 } $00e4, { LATIN SMALL LETTER A WITH DIAERESIS
$85 } $00e0, { LATIN SMALL LETTER A WITH GRAVE
$86 } $00e5, { LATIN SMALL LETTER A WITH RING ABOVE
$87 } $00e7, { LATIN SMALL LETTER C WITH CEDILLA
$88 } $00ea, { LATIN SMALL LETTER E WITH CIRCUMFLEX
$89 } $00eb, { LATIN SMALL LETTER E WITH DIAERESIS
$8a } $00e8, { LATIN SMALL LETTER E WITH GRAVE
$8b } $00ef, { LATIN SMALL LETTER I WITH DIAERESIS
$8c } $00ee, { LATIN SMALL LETTER I WITH CIRCUMFLEX
$8d } $0131, { LATIN SMALL LETTER DOTLESS I
$8e } $00c4, { LATIN CAPITAL LETTER A WITH DIAERESIS
$8f } $00c5, { LATIN CAPITAL LETTER A WITH RING ABOVE
$90 } $00c9, { LATIN CAPITAL LETTER E WITH ACUTE
$91 } $00e6, { LATIN SMALL LIGATURE AE
$92 } $00c6, { LATIN CAPITAL LIGATURE AE
$93 } $00f4, { LATIN SMALL LETTER O WITH CIRCUMFLEX
$94 } $00f6, { LATIN SMALL LETTER O WITH DIAERESIS
$95 } $00f2, { LATIN SMALL LETTER O WITH GRAVE
$96 } $00fb, { LATIN SMALL LETTER U WITH CIRCUMFLEX
$97 } $00f9, { LATIN SMALL LETTER U WITH GRAVE
$98 } $0130, { LATIN CAPITAL LETTER I WITH DOT ABOVE
$99 } $00d6, { LATIN CAPITAL LETTER O WITH DIAERESIS
$9a } $00dc, { LATIN CAPITAL LETTER U WITH DIAERESIS
$9b } $00f8, { LATIN SMALL LETTER O WITH STROKE
$9c } $00a3, { POUND SIGN
$9d } $00d8, { LATIN CAPITAL LETTER O WITH STROKE
$9e } $015e, { LATIN CAPITAL LETTER S WITH CEDILLA
$9f } $015f, { LATIN SMALL LETTER S WITH CEDILLA
$a0 } $00e1, { LATIN SMALL LETTER A WITH ACUTE
$a1 } $00ed, { LATIN SMALL LETTER I WITH ACUTE
$a2 } $00f3, { LATIN SMALL LETTER O WITH ACUTE
$a3 } $00fa, { LATIN SMALL LETTER U WITH ACUTE
$a4 } $00f1, { LATIN SMALL LETTER N WITH TILDE
$a5 } $00d1, { LATIN CAPITAL LETTER N WITH TILDE
$a6 } $011e, { LATIN CAPITAL LETTER G WITH BREVE
$a7 } $011f, { LATIN SMALL LETTER G WITH BREVE
$a8 } $00bf, { INVERTED QUESTION MARK
$a9 } $00ae, { REGISTERED SIGN
$aa } $00ac, { NOT SIGN
$ab } $00bd, { VULGAR FRACTION ONE HALF
$ac } $00bc, { VULGAR FRACTION ONE QUARTER
$ad } $00a1, { INVERTED EXCLAMATION MARK
$ae } $00ab, { LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
$af } $00bb, { RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
$b0 } $2591, { LIGHT SHADE
$b1 } $2592, { MEDIUM SHADE
$b2 } $2593, { DARK SHADE
$b3 } $2502, { BOX DRAWINGS LIGHT VERTICAL
$b4 } $2524, { BOX DRAWINGS LIGHT VERTICAL AND LEFT
$b5 } $00c1, { LATIN CAPITAL LETTER A WITH ACUTE
$b6 } $00c2, { LATIN CAPITAL LETTER A WITH CIRCUMFLEX
$b7 } $00c0, { LATIN CAPITAL LETTER A WITH GRAVE
$b8 } $00a9, { COPYRIGHT SIGN
$b9 } $2563, { BOX DRAWINGS DOUBLE VERTICAL AND LEFT
$ba } $2551, { BOX DRAWINGS DOUBLE VERTICAL
$bb } $2557, { BOX DRAWINGS DOUBLE DOWN AND LEFT
$bc } $255d, { BOX DRAWINGS DOUBLE UP AND LEFT
$bd } $00a2, { CENT SIGN
$be } $00a5, { YEN SIGN
$bf } $2510, { BOX DRAWINGS LIGHT DOWN AND LEFT
$c0 } $2514, { BOX DRAWINGS LIGHT UP AND RIGHT
$c1 } $2534, { BOX DRAWINGS LIGHT UP AND HORIZONTAL
$c2 } $252c, { BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
$c3 } $251c, { BOX DRAWINGS LIGHT VERTICAL AND RIGHT
$c4 } $2500, { BOX DRAWINGS LIGHT HORIZONTAL
$c5 } $253c, { BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
$c6 } $00e3, { LATIN SMALL LETTER A WITH TILDE
$c7 } $00c3, { LATIN CAPITAL LETTER A WITH TILDE
$c8 } $255a, { BOX DRAWINGS DOUBLE UP AND RIGHT
$c9 } $2554, { BOX DRAWINGS DOUBLE DOWN AND RIGHT
$ca } $2569, { BOX DRAWINGS DOUBLE UP AND HORIZONTAL
$cb } $2566, { BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
$cc } $2560, { BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
$cd } $2550, { BOX DRAWINGS DOUBLE HORIZONTAL
$ce } $256c, { BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
$cf } $00a4, { CURRENCY SIGN
$d0 } $00ba, { MASCULINE ORDINAL INDICATOR
$d1 } $00aa, { FEMININE ORDINAL INDICATOR
$d2 } $00ca, { LATIN CAPITAL LETTER E WITH CIRCUMFLEX
$d3 } $00cb, { LATIN CAPITAL LETTER E WITH DIAERESIS
$d4 } $00c8, { LATIN CAPITAL LETTER E WITH GRAVE
$d5   $0000,   UNDEFINED
$d5 } $20AC, { EURO CURRENCY SIGN
$d6 } $00cd, { LATIN CAPITAL LETTER I WITH ACUTE
$d7 } $00ce, { LATIN CAPITAL LETTER I WITH CIRCUMFLEX
$d8 } $00cf, { LATIN CAPITAL LETTER I WITH DIAERESIS
$d9 } $2518, { BOX DRAWINGS LIGHT UP AND LEFT
$da } $250c, { BOX DRAWINGS LIGHT DOWN AND RIGHT
$db } $2588, { FULL BLOCK
$dc } $2584, { LOWER HALF BLOCK
$dd } $00a6, { BROKEN BAR
$de } $00cc, { LATIN CAPITAL LETTER I WITH GRAVE
$df } $2580, { UPPER HALF BLOCK
$e0 } $00d3, { LATIN CAPITAL LETTER O WITH ACUTE
$e1 } $00df, { LATIN SMALL LETTER SHARP S
$e2 } $00d4, { LATIN CAPITAL LETTER O WITH CIRCUMFLEX
$e3 } $00d2, { LATIN CAPITAL LETTER O WITH GRAVE
$e4 } $00f5, { LATIN SMALL LETTER O WITH TILDE
$e5 } $00d5, { LATIN CAPITAL LETTER O WITH TILDE
$e6 } $00b5, { MICRO SIGN
$e7 } $0000, { UNDEFINED
$e8 } $00d7, { MULTIPLICATION SIGN
$e9 } $00da, { LATIN CAPITAL LETTER U WITH ACUTE
$ea } $00db, { LATIN CAPITAL LETTER U WITH CIRCUMFLEX
$eb } $00d9, { LATIN CAPITAL LETTER U WITH GRAVE
$ec } $00ec, { LATIN SMALL LETTER I WITH GRAVE
$ed } $00ff, { LATIN SMALL LETTER Y WITH DIAERESIS
$ee } $00af, { MACRON
$ef } $00b4, { ACUTE ACCENT
$f0 } $00ad, { SOFT HYPHEN
$f1 } $00b1, { PLUS-MINUS SIGN
$f2 } $0000, { UNDEFINED
$f3 } $00be, { VULGAR FRACTION THREE QUARTERS
$f4 } $00b6, { PILCROW SIGN
$f5 } $00a7, { SECTION SIGN
$f6 } $00f7, { DIVISION SIGN
$f7 } $00b8, { CEDILLA
$f8 } $00b0, { DEGREE SIGN
$f9 } $00a8, { DIAERESIS
$fa } $00b7, { MIDDLE DOT
$fb } $00b9, { SUPERSCRIPT ONE
$fc } $00b3, { SUPERSCRIPT THREE
$fd } $00b2, { SUPERSCRIPT TWO
$fe } $25a0, { BLACK SQUARE
$ff } $00a0  { NO-BREAK SPACE
} );

//
// $Log$
// Revision 1.1  2003/09/29 20:47:20  cl
// - moved charset handling/conversion code to xplib
//
// Revision 1.1  2002/01/04 23:24:24  cl
// - added MS-DOS Codepage 857 (Multilingual Latin 5)
//