(* $Id: aliases.pl,v 1.4 2002/03/25 18:42:24 cl Exp )
(* generated from IANA charset list -- do not edit *)

function MimeCharsetCanonicalName(Const Name: String): String;
var UName: String;
begin
  UName:=UpperCase(Name);

(* ===================================================================        *)
(* CHARACTER SETS                                                             *)

(* (last updated 2002-09-30)                                                  *)

(* These are the official names for character sets that may be used in        *)
(* the Internet and may be referred to in Internet documentation.  These      *)
(* names are expressed in ANSI_X3.4-1968 which is commonly called             *)
(* US-ASCII or simply ASCII.  The character set most commonly use in the      *)
(* Internet and used especially in protocol standards is US-ASCII, this       *)
(* is strongly encouraged.  The use of the name US-ASCII is also              *)
(* encouraged.                                                                *)

(* The character set names may be up to 40 characters taken from the          *)
(* printable characters of US-ASCII.  However, no distinction is made         *)
(* between use of upper and lower case letters.                               *)

(* The MIBenum value is a unique value for use in MIBs to identify coded      *)
(* character sets.                                                            *)

(* The value space for MIBenum values has been divided into three             *)
(* regions. The first region (3-999) consists of coded character sets         *)
(* that have been standardized by some standard setting organization.         *)
(* This region is intended for standards that do not have subset              *)
(* implementations. The second region (1000-1999) is for the Unicode and      *)
(* ISO/IEC 10646 coded character sets together with a specification of a      *)
(* (set of) sub-repertoires that may occur.  The third region (>1999) is      *)
(* intended for vendor specific coded character sets.                         *)

(*         Assigned MIB enum Numbers                                          *)
(*         -------------------------                                          *)
(*         0-2             Reserved                                           *)
(*         3-999           Set By Standards Organizations                     *)
(*         1000-1999       Unicode / 10646                                    *)
(*         2000-2999       Vendor                                             *)

(* The aliases that start with "cs" have been added for use with the          *)
(* Printer MIB (see RFC 1759) and contain the standard numbers along with     *)
(* suggestive names in order to facilitate applications that want to          *)
(* display the names in user interfaces.  The "cs" stands for character       *)
(* set and is provided for applications that need a lower case first          *)
(* letter but want to use mixed case thereafter that cannot contain any       *)
(* special characters, such as underbar ("_") and dash ("-").  If the         *)
(* character set is from an ISO standard, its cs alias is the ISO             *)
(* standard number or name.  If the character set is not from an ISO          *)
(* standard, but is registered with ISO (IPSJ/ITSCJ is the current ISO        *)
(* Registration Authority), the ISO Registry number is specified as           *)
(* ISOnnn followed by letters suggestive of the name or standards number      *)
(* of the code set.  When a national or international standard is             *)
(* revised, the year of revision is added to the cs alias of the new          *)
(* character set entry in the IANA Registry in order to distinguish the       *)
(* revised character set from the original character set.                     *)


(* Character Set                                               Reference      *)
(* -------------                                               ---------      *)


(* Name: ANSI_X3.4-1968                                   [RFC1345,KXS2]      *)
(* MIBenum: 3                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-6                                                            *)
(* Alias: ANSI_X3.4-1986                                                      *)
(* Alias: ISO_646.irv:1991                                                    *)
(* Alias: ASCII                                                               *)
(* Alias: ISO646-US                                                           *)
(* Alias: US-ASCII (preferred MIME name)                                      *)
(* Alias: us                                                                  *)
(* Alias: IBM367                                                              *)
(* Alias: cp367                                                               *)
(* Alias: csASCII                                                             *)

  if UName='ANSI_X3.4-1968'          then result :='US-ASCII'              else
  if UName='ISO-IR-6'                then result :='US-ASCII'              else
  if UName='ANSI_X3.4-1986'          then result :='US-ASCII'              else
  if UName='ISO_646.IRV:1991'        then result :='US-ASCII'              else
  if UName='ASCII'                   then result :='US-ASCII'              else
  if UName='ISO646-US'               then result :='US-ASCII'              else
  if UName='US-ASCII'                then result :='US-ASCII'              else
  if UName='US'                      then result :='US-ASCII'              else
  if UName='IBM367'                  then result :='US-ASCII'              else
  if UName='CP367'                   then result :='US-ASCII'              else
  if UName='CSASCII'                 then result :='US-ASCII'              else

(* Name: ISO-10646-UTF-1                                                      *)
(* MIBenum: 27                                                                *)
(* Source: Universal Transfer Format (1), this is the multibyte               *)
(*         encoding, that subsets ASCII-7. It does not have byte              *)
(*         ordering issues.                                                   *)
(* Alias: csISO10646UTF1                                                      *)

  if UName='ISO-10646-UTF-1'         then result :='ISO-10646-UTF-1'       else
  if UName='CSISO10646UTF1'          then result :='ISO-10646-UTF-1'       else

(* Name: ISO_646.basic:1983                                [RFC1345,KXS2]     *)
(* MIBenum: 28                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ref                                                                 *)
(* Alias: csISO646basic1983                                                   *)

  if UName='ISO_646.BASIC:1983'      then result :='ISO_646.basic:1983'    else
  if UName='REF'                     then result :='ISO_646.basic:1983'    else
  if UName='CSISO646BASIC1983'       then result :='ISO_646.basic:1983'    else

(* Name: INVARIANT                                         [RFC1345,KXS2]     *)
(* MIBenum: 29                                                                *)
(* Alias: csINVARIANT                                                         *)

  if UName='INVARIANT'               then result :='INVARIANT'             else
  if UName='CSINVARIANT'             then result :='INVARIANT'             else

(* Name: ISO_646.irv:1983                                  [RFC1345,KXS2]     *)
(* MIBenum: 30                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-2                                                            *)
(* Alias: irv                                                                 *)
(* Alias: csISO2IntlRefVersion                                                *)

  if UName='ISO_646.IRV:1983'        then result :='ISO_646.irv:1983'      else
  if UName='ISO-IR-2'                then result :='ISO_646.irv:1983'      else
  if UName='IRV'                     then result :='ISO_646.irv:1983'      else
  if UName='CSISO2INTLREFVERSION'    then result :='ISO_646.irv:1983'      else

(* Name: BS_4730                                           [RFC1345,KXS2]     *)
(* MIBenum: 20                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-4                                                            *)
(* Alias: ISO646-GB                                                           *)
(* Alias: gb                                                                  *)
(* Alias: uk                                                                  *)
(* Alias: csISO4UnitedKingdom                                                 *)

  if UName='BS_4730'                 then result :='BS_4730'               else
  if UName='ISO-IR-4'                then result :='BS_4730'               else
  if UName='ISO646-GB'               then result :='BS_4730'               else
  if UName='GB'                      then result :='BS_4730'               else
  if UName='UK'                      then result :='BS_4730'               else
  if UName='CSISO4UNITEDKINGDOM'     then result :='BS_4730'               else

(* Name: NATS-SEFI                                         [RFC1345,KXS2]     *)
(* MIBenum: 31                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-1                                                          *)
(* Alias: csNATSSEFI                                                          *)

  if UName='NATS-SEFI'               then result :='NATS-SEFI'             else
  if UName='ISO-IR-8-1'              then result :='NATS-SEFI'             else
  if UName='CSNATSSEFI'              then result :='NATS-SEFI'             else

(* Name: NATS-SEFI-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 32                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-2                                                          *)
(* Alias: csNATSSEFIADD                                                       *)

  if UName='NATS-SEFI-ADD'           then result :='NATS-SEFI-ADD'         else
  if UName='ISO-IR-8-2'              then result :='NATS-SEFI-ADD'         else
  if UName='CSNATSSEFIADD'           then result :='NATS-SEFI-ADD'         else

(* Name: NATS-DANO                                         [RFC1345,KXS2]     *)
(* MIBenum: 33                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-1                                                          *)
(* Alias: csNATSDANO                                                          *)

  if UName='NATS-DANO'               then result :='NATS-DANO'             else
  if UName='ISO-IR-9-1'              then result :='NATS-DANO'             else
  if UName='CSNATSDANO'              then result :='NATS-DANO'             else

(* Name: NATS-DANO-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 34                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-2                                                          *)
(* Alias: csNATSDANOADD                                                       *)

  if UName='NATS-DANO-ADD'           then result :='NATS-DANO-ADD'         else
  if UName='ISO-IR-9-2'              then result :='NATS-DANO-ADD'         else
  if UName='CSNATSDANOADD'           then result :='NATS-DANO-ADD'         else

(* Name: SEN_850200_B                                      [RFC1345,KXS2]     *)
(* MIBenum: 35                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-10                                                           *)
(* Alias: FI                                                                  *)
(* Alias: ISO646-FI                                                           *)
(* Alias: ISO646-SE                                                           *)
(* Alias: se                                                                  *)
(* Alias: csISO10Swedish                                                      *)

  if UName='SEN_850200_B'            then result :='SEN_850200_B'          else
  if UName='ISO-IR-10'               then result :='SEN_850200_B'          else
  if UName='FI'                      then result :='SEN_850200_B'          else
  if UName='ISO646-FI'               then result :='SEN_850200_B'          else
  if UName='ISO646-SE'               then result :='SEN_850200_B'          else
  if UName='SE'                      then result :='SEN_850200_B'          else
  if UName='CSISO10SWEDISH'          then result :='SEN_850200_B'          else

(* Name: SEN_850200_C                                      [RFC1345,KXS2]     *)
(* MIBenum: 21                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-11                                                           *)
(* Alias: ISO646-SE2                                                          *)
(* Alias: se2                                                                 *)
(* Alias: csISO11SwedishForNames                                              *)

  if UName='SEN_850200_C'            then result :='SEN_850200_C'          else
  if UName='ISO-IR-11'               then result :='SEN_850200_C'          else
  if UName='ISO646-SE2'              then result :='SEN_850200_C'          else
  if UName='SE2'                     then result :='SEN_850200_C'          else
  if UName='CSISO11SWEDISHFORNAMES'  then result :='SEN_850200_C'          else

(* Name: KS_C_5601-1987                                    [RFC1345,KXS2]     *)
(* MIBenum: 36                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-149                                                          *)
(* Alias: KS_C_5601-1989                                                      *)
(* Alias: KSC_5601                                                            *)
(* Alias: korean                                                              *)
(* Alias: csKSC56011987                                                       *)

  if UName='KS_C_5601-1987'          then result :='KS_C_5601-1987'        else
  if UName='ISO-IR-149'              then result :='KS_C_5601-1987'        else
  if UName='KS_C_5601-1989'          then result :='KS_C_5601-1987'        else
  if UName='KSC_5601'                then result :='KS_C_5601-1987'        else
  if UName='KOREAN'                  then result :='KS_C_5601-1987'        else
  if UName='CSKSC56011987'           then result :='KS_C_5601-1987'        else

(* Name: ISO-2022-KR  (preferred MIME name)                [RFC1557,Choi]     *)
(* MIBenum: 37                                                                *)
(* Source: RFC-1557 (see also KS_C_5601-1987)                                 *)
(* Alias: csISO2022KR                                                         *)

  if UName='ISO-2022-KR'             then result :='ISO-2022-KR'           else
  if UName='CSISO2022KR'             then result :='ISO-2022-KR'           else

(* Name: EUC-KR  (preferred MIME name)                     [RFC1557,Choi]     *)
(* MIBenum: 38                                                                *)
(* Source: RFC-1557 (see also KS_C_5861-1992)                                 *)
(* Alias: csEUCKR                                                             *)

  if UName='EUC-KR'                  then result :='EUC-KR'                else
  if UName='CSEUCKR'                 then result :='EUC-KR'                else

(* Name: ISO-2022-JP  (preferred MIME name)               [RFC1468,Murai]     *)
(* MIBenum: 39                                                                *)
(* Source: RFC-1468 (see also RFC-2237)                                       *)
(* Alias: csISO2022JP                                                         *)

  if UName='ISO-2022-JP'             then result :='ISO-2022-JP'           else
  if UName='CSISO2022JP'             then result :='ISO-2022-JP'           else

(* Name: ISO-2022-JP-2  (preferred MIME name)              [RFC1554,Ohta]     *)
(* MIBenum: 40                                                                *)
(* Source: RFC-1554                                                           *)
(* Alias: csISO2022JP2                                                        *)

  if UName='ISO-2022-JP-2'           then result :='ISO-2022-JP-2'         else
  if UName='CSISO2022JP2'            then result :='ISO-2022-JP-2'         else

(* Name: ISO-2022-CN                                            [RFC1922]     *)
(* MIBenum: 104                                                               *)
(* Source: RFC-1922                                                           *)

  if UName='ISO-2022-CN'             then result :='ISO-2022-CN'           else

(* Name: ISO-2022-CN-EXT                                        [RFC1922]     *)
(* MIBenum: 105                                                               *)
(* Source: RFC-1922                                                           *)

  if UName='ISO-2022-CN-EXT'         then result :='ISO-2022-CN-EXT'       else

(* Name: JIS_C6220-1969-jp                                 [RFC1345,KXS2]     *)
(* MIBenum: 41                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: JIS_C6220-1969                                                      *)
(* Alias: iso-ir-13                                                           *)
(* Alias: katakana                                                            *)
(* Alias: x0201-7                                                             *)
(* Alias: csISO13JISC6220jp                                                   *)

  if UName='JIS_C6220-1969-JP'       then result :='JIS_C6220-1969-jp'     else
  if UName='JIS_C6220-1969'          then result :='JIS_C6220-1969-jp'     else
  if UName='ISO-IR-13'               then result :='JIS_C6220-1969-jp'     else
  if UName='KATAKANA'                then result :='JIS_C6220-1969-jp'     else
  if UName='X0201-7'                 then result :='JIS_C6220-1969-jp'     else
  if UName='CSISO13JISC6220JP'       then result :='JIS_C6220-1969-jp'     else

(* Name: JIS_C6220-1969-ro                                 [RFC1345,KXS2]     *)
(* MIBenum: 42                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-14                                                           *)
(* Alias: jp                                                                  *)
(* Alias: ISO646-JP                                                           *)
(* Alias: csISO14JISC6220ro                                                   *)

  if UName='JIS_C6220-1969-RO'       then result :='JIS_C6220-1969-ro'     else
  if UName='ISO-IR-14'               then result :='JIS_C6220-1969-ro'     else
  if UName='JP'                      then result :='JIS_C6220-1969-ro'     else
  if UName='ISO646-JP'               then result :='JIS_C6220-1969-ro'     else
  if UName='CSISO14JISC6220RO'       then result :='JIS_C6220-1969-ro'     else

(* Name: IT                                                [RFC1345,KXS2]     *)
(* MIBenum: 22                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-15                                                           *)
(* Alias: ISO646-IT                                                           *)
(* Alias: csISO15Italian                                                      *)

  if UName='IT'                      then result :='IT'                    else
  if UName='ISO-IR-15'               then result :='IT'                    else
  if UName='ISO646-IT'               then result :='IT'                    else
  if UName='CSISO15ITALIAN'          then result :='IT'                    else

(* Name: PT                                                [RFC1345,KXS2]     *)
(* MIBenum: 43                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-16                                                           *)
(* Alias: ISO646-PT                                                           *)
(* Alias: csISO16Portuguese                                                   *)

  if UName='PT'                      then result :='PT'                    else
  if UName='ISO-IR-16'               then result :='PT'                    else
  if UName='ISO646-PT'               then result :='PT'                    else
  if UName='CSISO16PORTUGUESE'       then result :='PT'                    else

(* Name: ES                                                [RFC1345,KXS2]     *)
(* MIBenum: 23                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-17                                                           *)
(* Alias: ISO646-ES                                                           *)
(* Alias: csISO17Spanish                                                      *)

  if UName='ES'                      then result :='ES'                    else
  if UName='ISO-IR-17'               then result :='ES'                    else
  if UName='ISO646-ES'               then result :='ES'                    else
  if UName='CSISO17SPANISH'          then result :='ES'                    else

(* Name: greek7-old                                        [RFC1345,KXS2]     *)
(* MIBenum: 44                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-18                                                           *)
(* Alias: csISO18Greek7Old                                                    *)

  if UName='GREEK7-OLD'              then result :='greek7-old'            else
  if UName='ISO-IR-18'               then result :='greek7-old'            else
  if UName='CSISO18GREEK7OLD'        then result :='greek7-old'            else

(* Name: latin-greek                                       [RFC1345,KXS2]     *)
(* MIBenum: 45                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-19                                                           *)
(* Alias: csISO19LatinGreek                                                   *)

  if UName='LATIN-GREEK'             then result :='latin-greek'           else
  if UName='ISO-IR-19'               then result :='latin-greek'           else
  if UName='CSISO19LATINGREEK'       then result :='latin-greek'           else

(* Name: DIN_66003                                         [RFC1345,KXS2]     *)
(* MIBenum: 24                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-21                                                           *)
(* Alias: de                                                                  *)
(* Alias: ISO646-DE                                                           *)
(* Alias: csISO21German                                                       *)

  if UName='DIN_66003'               then result :='DIN_66003'             else
  if UName='ISO-IR-21'               then result :='DIN_66003'             else
  if UName='DE'                      then result :='DIN_66003'             else
  if UName='ISO646-DE'               then result :='DIN_66003'             else
  if UName='CSISO21GERMAN'           then result :='DIN_66003'             else

(* Name: NF_Z_62-010_(1973)                                [RFC1345,KXS2]     *)
(* MIBenum: 46                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-25                                                           *)
(* Alias: ISO646-FR1                                                          *)
(* Alias: csISO25French                                                       *)

  if UName='NF_Z_62-010_(1973)'      then result :='NF_Z_62-010_(1973)'    else
  if UName='ISO-IR-25'               then result :='NF_Z_62-010_(1973)'    else
  if UName='ISO646-FR1'              then result :='NF_Z_62-010_(1973)'    else
  if UName='CSISO25FRENCH'           then result :='NF_Z_62-010_(1973)'    else

(* Name: Latin-greek-1                                     [RFC1345,KXS2]     *)
(* MIBenum: 47                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-27                                                           *)
(* Alias: csISO27LatinGreek1                                                  *)

  if UName='LATIN-GREEK-1'           then result :='Latin-greek-1'         else
  if UName='ISO-IR-27'               then result :='Latin-greek-1'         else
  if UName='CSISO27LATINGREEK1'      then result :='Latin-greek-1'         else

(* Name: ISO_5427                                          [RFC1345,KXS2]     *)
(* MIBenum: 48                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-37                                                           *)
(* Alias: csISO5427Cyrillic                                                   *)

  if UName='ISO_5427'                then result :='ISO_5427'              else
  if UName='ISO-IR-37'               then result :='ISO_5427'              else
  if UName='CSISO5427CYRILLIC'       then result :='ISO_5427'              else

(* Name: JIS_C6226-1978                                    [RFC1345,KXS2]     *)
(* MIBenum: 49                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-42                                                           *)
(* Alias: csISO42JISC62261978                                                 *)

  if UName='JIS_C6226-1978'          then result :='JIS_C6226-1978'        else
  if UName='ISO-IR-42'               then result :='JIS_C6226-1978'        else
  if UName='CSISO42JISC62261978'     then result :='JIS_C6226-1978'        else

(* Name: BS_viewdata                                       [RFC1345,KXS2]     *)
(* MIBenum: 50                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-47                                                           *)
(* Alias: csISO47BSViewdata                                                   *)

  if UName='BS_VIEWDATA'             then result :='BS_viewdata'           else
  if UName='ISO-IR-47'               then result :='BS_viewdata'           else
  if UName='CSISO47BSVIEWDATA'       then result :='BS_viewdata'           else

(* Name: INIS                                              [RFC1345,KXS2]     *)
(* MIBenum: 51                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-49                                                           *)
(* Alias: csISO49INIS                                                         *)

  if UName='INIS'                    then result :='INIS'                  else
  if UName='ISO-IR-49'               then result :='INIS'                  else
  if UName='CSISO49INIS'             then result :='INIS'                  else

(* Name: INIS-8                                            [RFC1345,KXS2]     *)
(* MIBenum: 52                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-50                                                           *)
(* Alias: csISO50INIS8                                                        *)

  if UName='INIS-8'                  then result :='INIS-8'                else
  if UName='ISO-IR-50'               then result :='INIS-8'                else
  if UName='CSISO50INIS8'            then result :='INIS-8'                else

(* Name: INIS-cyrillic                                     [RFC1345,KXS2]     *)
(* MIBenum: 53                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-51                                                           *)
(* Alias: csISO51INISCyrillic                                                 *)

  if UName='INIS-CYRILLIC'           then result :='INIS-cyrillic'         else
  if UName='ISO-IR-51'               then result :='INIS-cyrillic'         else
  if UName='CSISO51INISCYRILLIC'     then result :='INIS-cyrillic'         else

(* Name: ISO_5427:1981                                     [RFC1345,KXS2]     *)
(* MIBenum: 54                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-54                                                           *)
(* Alias: ISO5427Cyrillic1981                                                 *)

  if UName='ISO_5427:1981'           then result :='ISO_5427:1981'         else
  if UName='ISO-IR-54'               then result :='ISO_5427:1981'         else
  if UName='ISO5427CYRILLIC1981'     then result :='ISO_5427:1981'         else

(* Name: ISO_5428:1980                                     [RFC1345,KXS2]     *)
(* MIBenum: 55                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-55                                                           *)
(* Alias: csISO5428Greek                                                      *)

  if UName='ISO_5428:1980'           then result :='ISO_5428:1980'         else
  if UName='ISO-IR-55'               then result :='ISO_5428:1980'         else
  if UName='CSISO5428GREEK'          then result :='ISO_5428:1980'         else

(* Name: GB_1988-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 56                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-57                                                           *)
(* Alias: cn                                                                  *)
(* Alias: ISO646-CN                                                           *)
(* Alias: csISO57GB1988                                                       *)

  if UName='GB_1988-80'              then result :='GB_1988-80'            else
  if UName='ISO-IR-57'               then result :='GB_1988-80'            else
  if UName='CN'                      then result :='GB_1988-80'            else
  if UName='ISO646-CN'               then result :='GB_1988-80'            else
  if UName='CSISO57GB1988'           then result :='GB_1988-80'            else

(* Name: GB_2312-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 57                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-58                                                           *)
(* Alias: chinese                                                             *)
(* Alias: csISO58GB231280                                                     *)

  if UName='GB_2312-80'              then result :='GB_2312-80'            else
  if UName='ISO-IR-58'               then result :='GB_2312-80'            else
  if UName='CHINESE'                 then result :='GB_2312-80'            else
  if UName='CSISO58GB231280'         then result :='GB_2312-80'            else

(* Name: NS_4551-1                                         [RFC1345,KXS2]     *)
(* MIBenum: 25                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-60                                                           *)
(* Alias: ISO646-NO                                                           *)
(* Alias: no                                                                  *)
(* Alias: csISO60DanishNorwegian                                              *)
(* Alias: csISO60Norwegian1                                                   *)

  if UName='NS_4551-1'               then result :='NS_4551-1'             else
  if UName='ISO-IR-60'               then result :='NS_4551-1'             else
  if UName='ISO646-NO'               then result :='NS_4551-1'             else
  if UName='NO'                      then result :='NS_4551-1'             else
  if UName='CSISO60DANISHNORWEGIAN'  then result :='NS_4551-1'             else
  if UName='CSISO60NORWEGIAN1'       then result :='NS_4551-1'             else

(* Name: NS_4551-2                                          [RFC1345,KXS2]    *)
(* MIBenum: 58                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO646-NO2                                                          *)
(* Alias: iso-ir-61                                                           *)
(* Alias: no2                                                                 *)
(* Alias: csISO61Norwegian2                                                   *)

  if UName='NS_4551-2'               then result :='NS_4551-2'             else
  if UName='ISO646-NO2'              then result :='NS_4551-2'             else
  if UName='ISO-IR-61'               then result :='NS_4551-2'             else
  if UName='NO2'                     then result :='NS_4551-2'             else
  if UName='CSISO61NORWEGIAN2'       then result :='NS_4551-2'             else

(* Name: NF_Z_62-010                                        [RFC1345,KXS2]    *)
(* MIBenum: 26                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-69                                                           *)
(* Alias: ISO646-FR                                                           *)
(* Alias: fr                                                                  *)
(* Alias: csISO69French                                                       *)

  if UName='NF_Z_62-010'             then result :='NF_Z_62-010'           else
  if UName='ISO-IR-69'               then result :='NF_Z_62-010'           else
  if UName='ISO646-FR'               then result :='NF_Z_62-010'           else
  if UName='FR'                      then result :='NF_Z_62-010'           else
  if UName='CSISO69FRENCH'           then result :='NF_Z_62-010'           else

(* Name: videotex-suppl                                     [RFC1345,KXS2]    *)
(* MIBenum: 59                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-70                                                           *)
(* Alias: csISO70VideotexSupp1                                                *)

  if UName='VIDEOTEX-SUPPL'          then result :='videotex-suppl'        else
  if UName='ISO-IR-70'               then result :='videotex-suppl'        else
  if UName='CSISO70VIDEOTEXSUPP1'    then result :='videotex-suppl'        else

(* Name: PT2                                                [RFC1345,KXS2]    *)
(* MIBenum: 60                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-84                                                           *)
(* Alias: ISO646-PT2                                                          *)
(* Alias: csISO84Portuguese2                                                  *)

  if UName='PT2'                     then result :='PT2'                   else
  if UName='ISO-IR-84'               then result :='PT2'                   else
  if UName='ISO646-PT2'              then result :='PT2'                   else
  if UName='CSISO84PORTUGUESE2'      then result :='PT2'                   else

(* Name: ES2                                                [RFC1345,KXS2]    *)
(* MIBenum: 61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-85                                                           *)
(* Alias: ISO646-ES2                                                          *)
(* Alias: csISO85Spanish2                                                     *)

  if UName='ES2'                     then result :='ES2'                   else
  if UName='ISO-IR-85'               then result :='ES2'                   else
  if UName='ISO646-ES2'              then result :='ES2'                   else
  if UName='CSISO85SPANISH2'         then result :='ES2'                   else

(* Name: MSZ_7795.3                                         [RFC1345,KXS2]    *)
(* MIBenum: 62                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-86                                                           *)
(* Alias: ISO646-HU                                                           *)
(* Alias: hu                                                                  *)
(* Alias: csISO86Hungarian                                                    *)

  if UName='MSZ_7795.3'              then result :='MSZ_7795.3'            else
  if UName='ISO-IR-86'               then result :='MSZ_7795.3'            else
  if UName='ISO646-HU'               then result :='MSZ_7795.3'            else
  if UName='HU'                      then result :='MSZ_7795.3'            else
  if UName='CSISO86HUNGARIAN'        then result :='MSZ_7795.3'            else

(* Name: JIS_C6226-1983                                     [RFC1345,KXS2]    *)
(* MIBenum: 63                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-87                                                           *)
(* Alias: x0208                                                               *)
(* Alias: JIS_X0208-1983                                                      *)
(* Alias: csISO87JISX0208                                                     *)

  if UName='JIS_C6226-1983'          then result :='JIS_C6226-1983'        else
  if UName='ISO-IR-87'               then result :='JIS_C6226-1983'        else
  if UName='X0208'                   then result :='JIS_C6226-1983'        else
  if UName='JIS_X0208-1983'          then result :='JIS_C6226-1983'        else
  if UName='CSISO87JISX0208'         then result :='JIS_C6226-1983'        else

(* Name: greek7                                             [RFC1345,KXS2]    *)
(* MIBenum: 64                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-88                                                           *)
(* Alias: csISO88Greek7                                                       *)

  if UName='GREEK7'                  then result :='greek7'                else
  if UName='ISO-IR-88'               then result :='greek7'                else
  if UName='CSISO88GREEK7'           then result :='greek7'                else

(* Name: ASMO_449                                           [RFC1345,KXS2]    *)
(* MIBenum: 65                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO_9036                                                            *)
(* Alias: arabic7                                                             *)
(* Alias: iso-ir-89                                                           *)
(* Alias: csISO89ASMO449                                                      *)

  if UName='ASMO_449'                then result :='ASMO_449'              else
  if UName='ISO_9036'                then result :='ASMO_449'              else
  if UName='ARABIC7'                 then result :='ASMO_449'              else
  if UName='ISO-IR-89'               then result :='ASMO_449'              else
  if UName='CSISO89ASMO449'          then result :='ASMO_449'              else

(* Name: iso-ir-90                                          [RFC1345,KXS2]    *)
(* MIBenum: 66                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: csISO90                                                             *)

  if UName='ISO-IR-90'               then result :='iso-ir-90'             else
  if UName='CSISO90'                 then result :='iso-ir-90'             else

(* Name: JIS_C6229-1984-a                                   [RFC1345,KXS2]    *)
(* MIBenum: 67                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-91                                                           *)
(* Alias: jp-ocr-a                                                            *)
(* Alias: csISO91JISC62291984a                                                *)

  if UName='JIS_C6229-1984-A'        then result :='JIS_C6229-1984-a'      else
  if UName='ISO-IR-91'               then result :='JIS_C6229-1984-a'      else
  if UName='JP-OCR-A'                then result :='JIS_C6229-1984-a'      else
  if UName='CSISO91JISC62291984A'    then result :='JIS_C6229-1984-a'      else

(* Name: JIS_C6229-1984-b                                   [RFC1345,KXS2]    *)
(* MIBenum: 68                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-92                                                           *)
(* Alias: ISO646-JP-OCR-B                                                     *)
(* Alias: jp-ocr-b                                                            *)
(* Alias: csISO92JISC62991984b                                                *)

  if UName='JIS_C6229-1984-B'        then result :='JIS_C6229-1984-b'      else
  if UName='ISO-IR-92'               then result :='JIS_C6229-1984-b'      else
  if UName='ISO646-JP-OCR-B'         then result :='JIS_C6229-1984-b'      else
  if UName='JP-OCR-B'                then result :='JIS_C6229-1984-b'      else
  if UName='CSISO92JISC62991984B'    then result :='JIS_C6229-1984-b'      else

(* Name: JIS_C6229-1984-b-add                               [RFC1345,KXS2]    *)
(* MIBenum: 69                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-93                                                           *)
(* Alias: jp-ocr-b-add                                                        *)
(* Alias: csISO93JIS62291984badd                                              *)

  if UName='JIS_C6229-1984-B-ADD'    then result :='JIS_C6229-1984-b-add'  else
  if UName='ISO-IR-93'               then result :='JIS_C6229-1984-b-add'  else
  if UName='JP-OCR-B-ADD'            then result :='JIS_C6229-1984-b-add'  else
  if UName='CSISO93JIS62291984BADD'  then result :='JIS_C6229-1984-b-add'  else

(* Name: JIS_C6229-1984-hand                                [RFC1345,KXS2]    *)
(* MIBenum: 70                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-94                                                           *)
(* Alias: jp-ocr-hand                                                         *)
(* Alias: csISO94JIS62291984hand                                              *)

  if UName='JIS_C6229-1984-HAND'     then result :='JIS_C6229-1984-hand'   else
  if UName='ISO-IR-94'               then result :='JIS_C6229-1984-hand'   else
  if UName='JP-OCR-HAND'             then result :='JIS_C6229-1984-hand'   else
  if UName='CSISO94JIS62291984HAND'  then result :='JIS_C6229-1984-hand'   else

(* Name: JIS_C6229-1984-hand-add                            [RFC1345,KXS2]    *)
(* MIBenum: 71                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-95                                                           *)
(* Alias: jp-ocr-hand-add                                                     *)
(* Alias: csISO95JIS62291984handadd                                           *)

  if UName='JIS_C6229-1984-HAND-ADD' then result :='JIS_C6229-1984-hand-add' else
  if UName='ISO-IR-95'               then result :='JIS_C6229-1984-hand-add' else
  if UName='JP-OCR-HAND-ADD'         then result :='JIS_C6229-1984-hand-add' else
  if UName='CSISO95JIS62291984HANDADD' then result :='JIS_C6229-1984-hand-add' else

(* Name: JIS_C6229-1984-kana                                [RFC1345,KXS2]    *)
(* MIBenum: 72                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-96                                                           *)
(* Alias: csISO96JISC62291984kana                                             *)

  if UName='JIS_C6229-1984-KANA'     then result :='JIS_C6229-1984-kana'   else
  if UName='ISO-IR-96'               then result :='JIS_C6229-1984-kana'   else
  if UName='CSISO96JISC62291984KANA' then result :='JIS_C6229-1984-kana'   else

(* Name: ISO_2033-1983                                      [RFC1345,KXS2]    *)
(* MIBenum: 73                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-98                                                           *)
(* Alias: e13b                                                                *)
(* Alias: csISO2033                                                           *)

  if UName='ISO_2033-1983'           then result :='ISO_2033-1983'         else
  if UName='ISO-IR-98'               then result :='ISO_2033-1983'         else
  if UName='E13B'                    then result :='ISO_2033-1983'         else
  if UName='CSISO2033'               then result :='ISO_2033-1983'         else

(* Name: ANSI_X3.110-1983                                   [RFC1345,KXS2]    *)
(* MIBenum: 74                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-99                                                           *)
(* Alias: CSA_T500-1983                                                       *)
(* Alias: NAPLPS                                                              *)
(* Alias: csISO99NAPLPS                                                       *)

  if UName='ANSI_X3.110-1983'        then result :='ANSI_X3.110-1983'      else
  if UName='ISO-IR-99'               then result :='ANSI_X3.110-1983'      else
  if UName='CSA_T500-1983'           then result :='ANSI_X3.110-1983'      else
  if UName='NAPLPS'                  then result :='ANSI_X3.110-1983'      else
  if UName='CSISO99NAPLPS'           then result :='ANSI_X3.110-1983'      else

(* Name: ISO_8859-1:1987                                    [RFC1345,KXS2]    *)
(* MIBenum: 4                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-100                                                          *)
(* Alias: ISO_8859-1                                                          *)
(* Alias: ISO-8859-1 (preferred MIME name)                                    *)
(* Alias: latin1                                                              *)
(* Alias: l1                                                                  *)
(* Alias: IBM819                                                              *)
(* Alias: CP819                                                               *)
(* Alias: csISOLatin1                                                         *)

  if UName='ISO_8859-1:1987'         then result :='ISO-8859-1'            else
  if UName='ISO-IR-100'              then result :='ISO-8859-1'            else
  if UName='ISO_8859-1'              then result :='ISO-8859-1'            else
  if UName='ISO-8859-1'              then result :='ISO-8859-1'            else
  if UName='LATIN1'                  then result :='ISO-8859-1'            else
  if UName='L1'                      then result :='ISO-8859-1'            else
  if UName='IBM819'                  then result :='ISO-8859-1'            else
  if UName='CP819'                   then result :='ISO-8859-1'            else
  if UName='CSISOLATIN1'             then result :='ISO-8859-1'            else

(* Name: ISO_8859-2:1987                                    [RFC1345,KXS2]    *)
(* MIBenum: 5                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-101                                                          *)
(* Alias: ISO_8859-2                                                          *)
(* Alias: ISO-8859-2 (preferred MIME name)                                    *)
(* Alias: latin2                                                              *)
(* Alias: l2                                                                  *)
(* Alias: csISOLatin2                                                         *)

  if UName='ISO_8859-2:1987'         then result :='ISO-8859-2'            else
  if UName='ISO-IR-101'              then result :='ISO-8859-2'            else
  if UName='ISO_8859-2'              then result :='ISO-8859-2'            else
  if UName='ISO-8859-2'              then result :='ISO-8859-2'            else
  if UName='LATIN2'                  then result :='ISO-8859-2'            else
  if UName='L2'                      then result :='ISO-8859-2'            else
  if UName='CSISOLATIN2'             then result :='ISO-8859-2'            else

(* Name: T.61-7bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 75                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-102                                                          *)
(* Alias: csISO102T617bit                                                     *)

  if UName='T.61-7BIT'               then result :='T.61-7bit'             else
  if UName='ISO-IR-102'              then result :='T.61-7bit'             else
  if UName='CSISO102T617BIT'         then result :='T.61-7bit'             else

(* Name: T.61-8bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 76                                                                *)
(* Alias: T.61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-103                                                          *)
(* Alias: csISO103T618bit                                                     *)

  if UName='T.61-8BIT'               then result :='T.61-8bit'             else
  if UName='T.61'                    then result :='T.61-8bit'             else
  if UName='ISO-IR-103'              then result :='T.61-8bit'             else
  if UName='CSISO103T618BIT'         then result :='T.61-8bit'             else

(* Name: ISO_8859-3:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 6                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-109                                                          *)
(* Alias: ISO_8859-3                                                          *)
(* Alias: ISO-8859-3 (preferred MIME name)                                    *)
(* Alias: latin3                                                              *)
(* Alias: l3                                                                  *)
(* Alias: csISOLatin3                                                         *)

  if UName='ISO_8859-3:1988'         then result :='ISO-8859-3'            else
  if UName='ISO-IR-109'              then result :='ISO-8859-3'            else
  if UName='ISO_8859-3'              then result :='ISO-8859-3'            else
  if UName='ISO-8859-3'              then result :='ISO-8859-3'            else
  if UName='LATIN3'                  then result :='ISO-8859-3'            else
  if UName='L3'                      then result :='ISO-8859-3'            else
  if UName='CSISOLATIN3'             then result :='ISO-8859-3'            else

(* Name: ISO_8859-4:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 7                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-110                                                          *)
(* Alias: ISO_8859-4                                                          *)
(* Alias: ISO-8859-4 (preferred MIME name)                                    *)
(* Alias: latin4                                                              *)
(* Alias: l4                                                                  *)
(* Alias: csISOLatin4                                                         *)

  if UName='ISO_8859-4:1988'         then result :='ISO-8859-4'            else
  if UName='ISO-IR-110'              then result :='ISO-8859-4'            else
  if UName='ISO_8859-4'              then result :='ISO-8859-4'            else
  if UName='ISO-8859-4'              then result :='ISO-8859-4'            else
  if UName='LATIN4'                  then result :='ISO-8859-4'            else
  if UName='L4'                      then result :='ISO-8859-4'            else
  if UName='CSISOLATIN4'             then result :='ISO-8859-4'            else

(* Name: ECMA-cyrillic                                      [RFC1345,KXS2]    *)
(* MIBenum: 77                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-111                                                          *)
(* Alias: csISO111ECMACyrillic                                                *)

  if UName='ECMA-CYRILLIC'           then result :='ECMA-cyrillic'         else
  if UName='ISO-IR-111'              then result :='ECMA-cyrillic'         else
  if UName='CSISO111ECMACYRILLIC'    then result :='ECMA-cyrillic'         else

(* Name: CSA_Z243.4-1985-1                                  [RFC1345,KXS2]    *)
(* MIBenum: 78                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-121                                                          *)
(* Alias: ISO646-CA                                                           *)
(* Alias: csa7-1                                                              *)
(* Alias: ca                                                                  *)
(* Alias: csISO121Canadian1                                                   *)

  if UName='CSA_Z243.4-1985-1'       then result :='CSA_Z243.4-1985-1'     else
  if UName='ISO-IR-121'              then result :='CSA_Z243.4-1985-1'     else
  if UName='ISO646-CA'               then result :='CSA_Z243.4-1985-1'     else
  if UName='CSA7-1'                  then result :='CSA_Z243.4-1985-1'     else
  if UName='CA'                      then result :='CSA_Z243.4-1985-1'     else
  if UName='CSISO121CANADIAN1'       then result :='CSA_Z243.4-1985-1'     else

(* Name: CSA_Z243.4-1985-2                                  [RFC1345,KXS2]    *)
(* MIBenum: 79                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-122                                                          *)
(* Alias: ISO646-CA2                                                          *)
(* Alias: csa7-2                                                              *)
(* Alias: csISO122Canadian2                                                   *)

  if UName='CSA_Z243.4-1985-2'       then result :='CSA_Z243.4-1985-2'     else
  if UName='ISO-IR-122'              then result :='CSA_Z243.4-1985-2'     else
  if UName='ISO646-CA2'              then result :='CSA_Z243.4-1985-2'     else
  if UName='CSA7-2'                  then result :='CSA_Z243.4-1985-2'     else
  if UName='CSISO122CANADIAN2'       then result :='CSA_Z243.4-1985-2'     else

(* Name: CSA_Z243.4-1985-gr                                 [RFC1345,KXS2]    *)
(* MIBenum: 80                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-123                                                          *)
(* Alias: csISO123CSAZ24341985gr                                              *)

  if UName='CSA_Z243.4-1985-GR'      then result :='CSA_Z243.4-1985-gr'    else
  if UName='ISO-IR-123'              then result :='CSA_Z243.4-1985-gr'    else
  if UName='CSISO123CSAZ24341985GR'  then result :='CSA_Z243.4-1985-gr'    else

(* Name: ISO_8859-6:1987                                    [RFC1345,KXS2]    *)
(* MIBenum: 9                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-127                                                          *)
(* Alias: ISO_8859-6                                                          *)
(* Alias: ISO-8859-6 (preferred MIME name)                                    *)
(* Alias: ECMA-114                                                            *)
(* Alias: ASMO-708                                                            *)
(* Alias: arabic                                                              *)
(* Alias: csISOLatinArabic                                                    *)

  if UName='ISO_8859-6:1987'         then result :='ISO-8859-6'            else
  if UName='ISO-IR-127'              then result :='ISO-8859-6'            else
  if UName='ISO_8859-6'              then result :='ISO-8859-6'            else
  if UName='ISO-8859-6'              then result :='ISO-8859-6'            else
  if UName='ECMA-114'                then result :='ISO-8859-6'            else
  if UName='ASMO-708'                then result :='ISO-8859-6'            else
  if UName='ARABIC'                  then result :='ISO-8859-6'            else
  if UName='CSISOLATINARABIC'        then result :='ISO-8859-6'            else

(* Name: ISO_8859-6-E                                       [RFC1556,IANA]    *)
(* MIBenum: 81                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596E                                                         *)
(* Alias: ISO-8859-6-E (preferred MIME name)                                  *)

  if UName='ISO_8859-6-E'            then result :='ISO-8859-6-E'          else
  if UName='CSISO88596E'             then result :='ISO-8859-6-E'          else
  if UName='ISO-8859-6-E'            then result :='ISO-8859-6-E'          else

(* Name: ISO_8859-6-I                                       [RFC1556,IANA]    *)
(* MIBenum: 82                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596I                                                         *)
(* Alias: ISO-8859-6-I (preferred MIME name)                                  *)

  if UName='ISO_8859-6-I'            then result :='ISO-8859-6-I'          else
  if UName='CSISO88596I'             then result :='ISO-8859-6-I'          else
  if UName='ISO-8859-6-I'            then result :='ISO-8859-6-I'          else

(* Name: ISO_8859-7:1987                            [RFC1947,RFC1345,KXS2]    *)
(* MIBenum: 10                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-126                                                          *)
(* Alias: ISO_8859-7                                                          *)
(* Alias: ISO-8859-7 (preferred MIME name)                                    *)
(* Alias: ELOT_928                                                            *)
(* Alias: ECMA-118                                                            *)
(* Alias: greek                                                               *)
(* Alias: greek8                                                              *)
(* Alias: csISOLatinGreek                                                     *)

  if UName='ISO_8859-7:1987'         then result :='ISO-8859-7'            else
  if UName='ISO-IR-126'              then result :='ISO-8859-7'            else
  if UName='ISO_8859-7'              then result :='ISO-8859-7'            else
  if UName='ISO-8859-7'              then result :='ISO-8859-7'            else
  if UName='ELOT_928'                then result :='ISO-8859-7'            else
  if UName='ECMA-118'                then result :='ISO-8859-7'            else
  if UName='GREEK'                   then result :='ISO-8859-7'            else
  if UName='GREEK8'                  then result :='ISO-8859-7'            else
  if UName='CSISOLATINGREEK'         then result :='ISO-8859-7'            else

(* Name: T.101-G2                                            [RFC1345,KXS2]   *)
(* MIBenum: 83                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-128                                                          *)
(* Alias: csISO128T101G2                                                      *)

  if UName='T.101-G2'                then result :='T.101-G2'              else
  if UName='ISO-IR-128'              then result :='T.101-G2'              else
  if UName='CSISO128T101G2'          then result :='T.101-G2'              else

(* Name: ISO_8859-8:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 11                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-138                                                          *)
(* Alias: ISO_8859-8                                                          *)
(* Alias: ISO-8859-8 (preferred MIME name)                                    *)
(* Alias: hebrew                                                              *)
(* Alias: csISOLatinHebrew                                                    *)

  if UName='ISO_8859-8:1988'         then result :='ISO-8859-8'            else
  if UName='ISO-IR-138'              then result :='ISO-8859-8'            else
  if UName='ISO_8859-8'              then result :='ISO-8859-8'            else
  if UName='ISO-8859-8'              then result :='ISO-8859-8'            else
  if UName='HEBREW'                  then result :='ISO-8859-8'            else
  if UName='CSISOLATINHEBREW'        then result :='ISO-8859-8'            else

(* Name: ISO_8859-8-E                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 84                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598E                                                         *)
(* Alias: ISO-8859-8-E (preferred MIME name)                                  *)

  if UName='ISO_8859-8-E'            then result :='ISO-8859-8-E'          else
  if UName='CSISO88598E'             then result :='ISO-8859-8-E'          else
  if UName='ISO-8859-8-E'            then result :='ISO-8859-8-E'          else

(* Name: ISO_8859-8-I                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 85                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598I                                                         *)
(* Alias: ISO-8859-8-I (preferred MIME name)                                  *)

  if UName='ISO_8859-8-I'            then result :='ISO-8859-8-I'          else
  if UName='CSISO88598I'             then result :='ISO-8859-8-I'          else
  if UName='ISO-8859-8-I'            then result :='ISO-8859-8-I'          else

(* Name: CSN_369103                                          [RFC1345,KXS2]   *)
(* MIBenum: 86                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-139                                                          *)
(* Alias: csISO139CSN369103                                                   *)

  if UName='CSN_369103'              then result :='CSN_369103'            else
  if UName='ISO-IR-139'              then result :='CSN_369103'            else
  if UName='CSISO139CSN369103'       then result :='CSN_369103'            else

(* Name: JUS_I.B1.002                                        [RFC1345,KXS2]   *)
(* MIBenum: 87                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-141                                                          *)
(* Alias: ISO646-YU                                                           *)
(* Alias: js                                                                  *)
(* Alias: yu                                                                  *)
(* Alias: csISO141JUSIB1002                                                   *)

  if UName='JUS_I.B1.002'            then result :='JUS_I.B1.002'          else
  if UName='ISO-IR-141'              then result :='JUS_I.B1.002'          else
  if UName='ISO646-YU'               then result :='JUS_I.B1.002'          else
  if UName='JS'                      then result :='JUS_I.B1.002'          else
  if UName='YU'                      then result :='JUS_I.B1.002'          else
  if UName='CSISO141JUSIB1002'       then result :='JUS_I.B1.002'          else

(* Name: ISO_6937-2-add                                      [RFC1345,KXS2]   *)
(* MIBenum: 14                                                                *)
(* Source: ECMA registry and ISO 6937-2:1983                                  *)
(* Alias: iso-ir-142                                                          *)
(* Alias: csISOTextComm                                                       *)

  if UName='ISO_6937-2-ADD'          then result :='ISO_6937-2-add'        else
  if UName='ISO-IR-142'              then result :='ISO_6937-2-add'        else
  if UName='CSISOTEXTCOMM'           then result :='ISO_6937-2-add'        else

(* Name: IEC_P27-1                                           [RFC1345,KXS2]   *)
(* MIBenum: 88                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-143                                                          *)
(* Alias: csISO143IECP271                                                     *)

  if UName='IEC_P27-1'               then result :='IEC_P27-1'             else
  if UName='ISO-IR-143'              then result :='IEC_P27-1'             else
  if UName='CSISO143IECP271'         then result :='IEC_P27-1'             else

(* Name: ISO_8859-5:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 8                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-144                                                          *)
(* Alias: ISO_8859-5                                                          *)
(* Alias: ISO-8859-5 (preferred MIME name)                                    *)
(* Alias: cyrillic                                                            *)
(* Alias: csISOLatinCyrillic                                                  *)

  if UName='ISO_8859-5:1988'         then result :='ISO-8859-5'            else
  if UName='ISO-IR-144'              then result :='ISO-8859-5'            else
  if UName='ISO_8859-5'              then result :='ISO-8859-5'            else
  if UName='ISO-8859-5'              then result :='ISO-8859-5'            else
  if UName='CYRILLIC'                then result :='ISO-8859-5'            else
  if UName='CSISOLATINCYRILLIC'      then result :='ISO-8859-5'            else

(* Name: JUS_I.B1.003-serb                                   [RFC1345,KXS2]   *)
(* MIBenum: 89                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-146                                                          *)
(* Alias: serbian                                                             *)
(* Alias: csISO146Serbian                                                     *)

  if UName='JUS_I.B1.003-SERB'       then result :='JUS_I.B1.003-serb'     else
  if UName='ISO-IR-146'              then result :='JUS_I.B1.003-serb'     else
  if UName='SERBIAN'                 then result :='JUS_I.B1.003-serb'     else
  if UName='CSISO146SERBIAN'         then result :='JUS_I.B1.003-serb'     else

(* Name: JUS_I.B1.003-mac                                    [RFC1345,KXS2]   *)
(* MIBenum: 90                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: macedonian                                                          *)
(* Alias: iso-ir-147                                                          *)
(* Alias: csISO147Macedonian                                                  *)

  if UName='JUS_I.B1.003-MAC'        then result :='JUS_I.B1.003-mac'      else
  if UName='MACEDONIAN'              then result :='JUS_I.B1.003-mac'      else
  if UName='ISO-IR-147'              then result :='JUS_I.B1.003-mac'      else
  if UName='CSISO147MACEDONIAN'      then result :='JUS_I.B1.003-mac'      else

(* Name: ISO_8859-9:1989                                     [RFC1345,KXS2]   *)
(* MIBenum: 12                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-148                                                          *)
(* Alias: ISO_8859-9                                                          *)
(* Alias: ISO-8859-9 (preferred MIME name)                                    *)
(* Alias: latin5                                                              *)
(* Alias: l5                                                                  *)
(* Alias: csISOLatin5                                                         *)

  if UName='ISO_8859-9:1989'         then result :='ISO-8859-9'            else
  if UName='ISO-IR-148'              then result :='ISO-8859-9'            else
  if UName='ISO_8859-9'              then result :='ISO-8859-9'            else
  if UName='ISO-8859-9'              then result :='ISO-8859-9'            else
  if UName='LATIN5'                  then result :='ISO-8859-9'            else
  if UName='L5'                      then result :='ISO-8859-9'            else
  if UName='CSISOLATIN5'             then result :='ISO-8859-9'            else

(* Name: greek-ccitt                                         [RFC1345,KXS2]   *)
(* MIBenum: 91                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-150                                                          *)
(* Alias: csISO150                                                            *)
(* Alias: csISO150GreekCCITT                                                  *)

  if UName='GREEK-CCITT'             then result :='greek-ccitt'           else
  if UName='ISO-IR-150'              then result :='greek-ccitt'           else
  if UName='CSISO150'                then result :='greek-ccitt'           else
  if UName='CSISO150GREEKCCITT'      then result :='greek-ccitt'           else

(* Name: NC_NC00-10:81                                       [RFC1345,KXS2]   *)
(* MIBenum: 92                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: cuba                                                                *)
(* Alias: iso-ir-151                                                          *)
(* Alias: ISO646-CU                                                           *)
(* Alias: csISO151Cuba                                                        *)

  if UName='NC_NC00-10:81'           then result :='NC_NC00-10:81'         else
  if UName='CUBA'                    then result :='NC_NC00-10:81'         else
  if UName='ISO-IR-151'              then result :='NC_NC00-10:81'         else
  if UName='ISO646-CU'               then result :='NC_NC00-10:81'         else
  if UName='CSISO151CUBA'            then result :='NC_NC00-10:81'         else

(* Name: ISO_6937-2-25                                       [RFC1345,KXS2]   *)
(* MIBenum: 93                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-152                                                          *)
(* Alias: csISO6937Add                                                        *)

  if UName='ISO_6937-2-25'           then result :='ISO_6937-2-25'         else
  if UName='ISO-IR-152'              then result :='ISO_6937-2-25'         else
  if UName='CSISO6937ADD'            then result :='ISO_6937-2-25'         else

(* Name: GOST_19768-74                                       [RFC1345,KXS2]   *)
(* MIBenum: 94                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ST_SEV_358-88                                                       *)
(* Alias: iso-ir-153                                                          *)
(* Alias: csISO153GOST1976874                                                 *)

  if UName='GOST_19768-74'           then result :='GOST_19768-74'         else
  if UName='ST_SEV_358-88'           then result :='GOST_19768-74'         else
  if UName='ISO-IR-153'              then result :='GOST_19768-74'         else
  if UName='CSISO153GOST1976874'     then result :='GOST_19768-74'         else

(* Name: ISO_8859-supp                                       [RFC1345,KXS2]   *)
(* MIBenum: 95                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-154                                                          *)
(* Alias: latin1-2-5                                                          *)
(* Alias: csISO8859Supp                                                       *)

  if UName='ISO_8859-SUPP'           then result :='ISO_8859-supp'         else
  if UName='ISO-IR-154'              then result :='ISO_8859-supp'         else
  if UName='LATIN1-2-5'              then result :='ISO_8859-supp'         else
  if UName='CSISO8859SUPP'           then result :='ISO_8859-supp'         else

(* Name: ISO_10367-box                                       [RFC1345,KXS2]   *)
(* MIBenum: 96                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-155                                                          *)
(* Alias: csISO10367Box                                                       *)

  if UName='ISO_10367-BOX'           then result :='ISO_10367-box'         else
  if UName='ISO-IR-155'              then result :='ISO_10367-box'         else
  if UName='CSISO10367BOX'           then result :='ISO_10367-box'         else

(* Name: ISO-8859-10 (preferred MIME name)                   [RFC1345,KXS2]   *)
(* MIBenum: 13                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-157                                                          *)
(* Alias: l6                                                                  *)
(* Alias: ISO_8859-10:1992                                                    *)
(* Alias: csISOLatin6                                                         *)
(* Alias: latin6                                                              *)

  if UName='ISO-8859-10'             then result :='ISO-8859-10'           else
  if UName='ISO-IR-157'              then result :='ISO-8859-10'           else
  if UName='L6'                      then result :='ISO-8859-10'           else
  if UName='ISO_8859-10:1992'        then result :='ISO-8859-10'           else
  if UName='CSISOLATIN6'             then result :='ISO-8859-10'           else
  if UName='LATIN6'                  then result :='ISO-8859-10'           else

(* Name: latin-lap                                           [RFC1345,KXS2]   *)
(* MIBenum: 97                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: lap                                                                 *)
(* Alias: iso-ir-158                                                          *)
(* Alias: csISO158Lap                                                         *)

  if UName='LATIN-LAP'               then result :='latin-lap'             else
  if UName='LAP'                     then result :='latin-lap'             else
  if UName='ISO-IR-158'              then result :='latin-lap'             else
  if UName='CSISO158LAP'             then result :='latin-lap'             else

(* Name: JIS_X0212-1990                                      [RFC1345,KXS2]   *)
(* MIBenum: 98                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: x0212                                                               *)
(* Alias: iso-ir-159                                                          *)
(* Alias: csISO159JISX02121990                                                *)

  if UName='JIS_X0212-1990'          then result :='JIS_X0212-1990'        else
  if UName='X0212'                   then result :='JIS_X0212-1990'        else
  if UName='ISO-IR-159'              then result :='JIS_X0212-1990'        else
  if UName='CSISO159JISX02121990'    then result :='JIS_X0212-1990'        else

(* Name: DS_2089                                             [RFC1345,KXS2]   *)
(* MIBenum: 99                                                                *)
(* Source: Danish Standard, DS 2089, February 1974                            *)
(* Alias: DS2089                                                              *)
(* Alias: ISO646-DK                                                           *)
(* Alias: dk                                                                  *)
(* Alias: csISO646Danish                                                      *)

  if UName='DS_2089'                 then result :='DS_2089'               else
  if UName='DS2089'                  then result :='DS_2089'               else
  if UName='ISO646-DK'               then result :='DS_2089'               else
  if UName='DK'                      then result :='DS_2089'               else
  if UName='CSISO646DANISH'          then result :='DS_2089'               else

(* Name: us-dk                                               [RFC1345,KXS2]   *)
(* MIBenum: 100                                                               *)
(* Alias: csUSDK                                                              *)

  if UName='US-DK'                   then result :='us-dk'                 else
  if UName='CSUSDK'                  then result :='us-dk'                 else

(* Name: dk-us                                               [RFC1345,KXS2]   *)
(* MIBenum: 101                                                               *)
(* Alias: csDKUS                                                              *)

  if UName='DK-US'                   then result :='dk-us'                 else
  if UName='CSDKUS'                  then result :='dk-us'                 else

(* Name: JIS_X0201                                           [RFC1345,KXS2]   *)
(* MIBenum: 15                                                                *)
(* Source: JIS X 0201-1976.   One byte only, this is equivalent to            *)
(*         JIS/Roman (similar to ASCII) plus eight-bit half-width             *)
(*         Katakana                                                           *)
(* Alias: X0201                                                               *)
(* Alias: csHalfWidthKatakana                                                 *)

  if UName='JIS_X0201'               then result :='JIS_X0201'             else
  if UName='X0201'                   then result :='JIS_X0201'             else
  if UName='CSHALFWIDTHKATAKANA'     then result :='JIS_X0201'             else

(* Name: KSC5636                                             [RFC1345,KXS2]   *)
(* MIBenum: 102                                                               *)
(* Alias: ISO646-KR                                                           *)
(* Alias: csKSC5636                                                           *)

  if UName='KSC5636'                 then result :='KSC5636'               else
  if UName='ISO646-KR'               then result :='KSC5636'               else
  if UName='CSKSC5636'               then result :='KSC5636'               else

(* Name: ISO-10646-UCS-2                                                      *)
(* MIBenum: 1000                                                              *)
(* Source: the 2-octet Basic Multilingual Plane, aka Unicode                  *)
(*         this needs to specify network byte order: the standard             *)
(*         does not specify (it is a 16-bit integer space)                    *)
(* Alias: csUnicode                                                           *)

  if UName='ISO-10646-UCS-2'         then result :='ISO-10646-UCS-2'       else
  if UName='CSUNICODE'               then result :='ISO-10646-UCS-2'       else

(* Name: ISO-10646-UCS-4                                                      *)
(* MIBenum: 1001                                                              *)
(* Source: the full code space. (same comment about byte order,               *)
(*         these are 31-bit numbers.                                          *)
(* Alias: csUCS4                                                              *)

  if UName='ISO-10646-UCS-4'         then result :='ISO-10646-UCS-4'       else
  if UName='CSUCS4'                  then result :='ISO-10646-UCS-4'       else

(* Name: DEC-MCS                                             [RFC1345,KXS2]   *)
(* MIBenum: 2008                                                              *)
(* Source: VAX/VMS User's Manual,                                             *)
(*         Order Number: AI-Y517A-TE, April 1986.                             *)
(* Alias: dec                                                                 *)
(* Alias: csDECMCS                                                            *)

  if UName='DEC-MCS'                 then result :='DEC-MCS'               else
  if UName='DEC'                     then result :='DEC-MCS'               else
  if UName='CSDECMCS'                then result :='DEC-MCS'               else

(* Name: hp-roman8                                  [HP-PCL5,RFC1345,KXS2]    *)
(* MIBenum: 2004                                                              *)
(* Source: LaserJet IIP Printer User's Manual,                                *)
(*         HP part no 33471-90901, Hewlet-Packard, June 1989.                 *)
(* Alias: roman8                                                              *)
(* Alias: r8                                                                  *)
(* Alias: csHPRoman8                                                          *)

  if UName='HP-ROMAN8'               then result :='hp-roman8'             else
  if UName='ROMAN8'                  then result :='hp-roman8'             else
  if UName='R8'                      then result :='hp-roman8'             else
  if UName='CSHPROMAN8'              then result :='hp-roman8'             else

(* Name: macintosh                                           [RFC1345,KXS2]   *)
(* MIBenum: 2027                                                              *)
(* Source: The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991          *)
(* Alias: mac                                                                 *)
(* Alias: csMacintosh                                                         *)

  if UName='MACINTOSH'               then result :='macintosh'             else
  if UName='MAC'                     then result :='macintosh'             else
  if UName='CSMACINTOSH'             then result :='macintosh'             else

(* Name: IBM037                                              [RFC1345,KXS2]   *)
(* MIBenum: 2028                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp037                                                               *)
(* Alias: ebcdic-cp-us                                                        *)
(* Alias: ebcdic-cp-ca                                                        *)
(* Alias: ebcdic-cp-wt                                                        *)
(* Alias: ebcdic-cp-nl                                                        *)
(* Alias: csIBM037                                                            *)

  if UName='IBM037'                  then result :='IBM037'                else
  if UName='CP037'                   then result :='IBM037'                else
  if UName='EBCDIC-CP-US'            then result :='IBM037'                else
  if UName='EBCDIC-CP-CA'            then result :='IBM037'                else
  if UName='EBCDIC-CP-WT'            then result :='IBM037'                else
  if UName='EBCDIC-CP-NL'            then result :='IBM037'                else
  if UName='CSIBM037'                then result :='IBM037'                else

(* Name: IBM038                                              [RFC1345,KXS2]   *)
(* MIBenum: 2029                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-INT                                                          *)
(* Alias: cp038                                                               *)
(* Alias: csIBM038                                                            *)

  if UName='IBM038'                  then result :='IBM038'                else
  if UName='EBCDIC-INT'              then result :='IBM038'                else
  if UName='CP038'                   then result :='IBM038'                else
  if UName='CSIBM038'                then result :='IBM038'                else

(* Name: IBM273                                              [RFC1345,KXS2]   *)
(* MIBenum: 2030                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP273                                                               *)
(* Alias: csIBM273                                                            *)

  if UName='IBM273'                  then result :='IBM273'                else
  if UName='CP273'                   then result :='IBM273'                else
  if UName='CSIBM273'                then result :='IBM273'                else

(* Name: IBM274                                              [RFC1345,KXS2]   *)
(* MIBenum: 2031                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-BE                                                           *)
(* Alias: CP274                                                               *)
(* Alias: csIBM274                                                            *)

  if UName='IBM274'                  then result :='IBM274'                else
  if UName='EBCDIC-BE'               then result :='IBM274'                else
  if UName='CP274'                   then result :='IBM274'                else
  if UName='CSIBM274'                then result :='IBM274'                else

(* Name: IBM275                                              [RFC1345,KXS2]   *)
(* MIBenum: 2032                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-BR                                                           *)
(* Alias: cp275                                                               *)
(* Alias: csIBM275                                                            *)

  if UName='IBM275'                  then result :='IBM275'                else
  if UName='EBCDIC-BR'               then result :='IBM275'                else
  if UName='CP275'                   then result :='IBM275'                else
  if UName='CSIBM275'                then result :='IBM275'                else

(* Name: IBM277                                              [RFC1345,KXS2]   *)
(* MIBenum: 2033                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-CP-DK                                                        *)
(* Alias: EBCDIC-CP-NO                                                        *)
(* Alias: csIBM277                                                            *)

  if UName='IBM277'                  then result :='IBM277'                else
  if UName='EBCDIC-CP-DK'            then result :='IBM277'                else
  if UName='EBCDIC-CP-NO'            then result :='IBM277'                else
  if UName='CSIBM277'                then result :='IBM277'                else

(* Name: IBM278                                              [RFC1345,KXS2]   *)
(* MIBenum: 2034                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP278                                                               *)
(* Alias: ebcdic-cp-fi                                                        *)
(* Alias: ebcdic-cp-se                                                        *)
(* Alias: csIBM278                                                            *)

  if UName='IBM278'                  then result :='IBM278'                else
  if UName='CP278'                   then result :='IBM278'                else
  if UName='EBCDIC-CP-FI'            then result :='IBM278'                else
  if UName='EBCDIC-CP-SE'            then result :='IBM278'                else
  if UName='CSIBM278'                then result :='IBM278'                else

(* Name: IBM280                                              [RFC1345,KXS2]   *)
(* MIBenum: 2035                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP280                                                               *)
(* Alias: ebcdic-cp-it                                                        *)
(* Alias: csIBM280                                                            *)

  if UName='IBM280'                  then result :='IBM280'                else
  if UName='CP280'                   then result :='IBM280'                else
  if UName='EBCDIC-CP-IT'            then result :='IBM280'                else
  if UName='CSIBM280'                then result :='IBM280'                else

(* Name: IBM281                                              [RFC1345,KXS2]   *)
(* MIBenum: 2036                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-JP-E                                                         *)
(* Alias: cp281                                                               *)
(* Alias: csIBM281                                                            *)

  if UName='IBM281'                  then result :='IBM281'                else
  if UName='EBCDIC-JP-E'             then result :='IBM281'                else
  if UName='CP281'                   then result :='IBM281'                else
  if UName='CSIBM281'                then result :='IBM281'                else

(* Name: IBM284                                              [RFC1345,KXS2]   *)
(* MIBenum: 2037                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP284                                                               *)
(* Alias: ebcdic-cp-es                                                        *)
(* Alias: csIBM284                                                            *)

  if UName='IBM284'                  then result :='IBM284'                else
  if UName='CP284'                   then result :='IBM284'                else
  if UName='EBCDIC-CP-ES'            then result :='IBM284'                else
  if UName='CSIBM284'                then result :='IBM284'                else

(* Name: IBM285                                              [RFC1345,KXS2]   *)
(* MIBenum: 2038                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP285                                                               *)
(* Alias: ebcdic-cp-gb                                                        *)
(* Alias: csIBM285                                                            *)

  if UName='IBM285'                  then result :='IBM285'                else
  if UName='CP285'                   then result :='IBM285'                else
  if UName='EBCDIC-CP-GB'            then result :='IBM285'                else
  if UName='CSIBM285'                then result :='IBM285'                else

(* Name: IBM290                                              [RFC1345,KXS2]   *)
(* MIBenum: 2039                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: cp290                                                               *)
(* Alias: EBCDIC-JP-kana                                                      *)
(* Alias: csIBM290                                                            *)

  if UName='IBM290'                  then result :='IBM290'                else
  if UName='CP290'                   then result :='IBM290'                else
  if UName='EBCDIC-JP-KANA'          then result :='IBM290'                else
  if UName='CSIBM290'                then result :='IBM290'                else

(* Name: IBM297                                              [RFC1345,KXS2]   *)
(* MIBenum: 2040                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp297                                                               *)
(* Alias: ebcdic-cp-fr                                                        *)
(* Alias: csIBM297                                                            *)

  if UName='IBM297'                  then result :='IBM297'                else
  if UName='CP297'                   then result :='IBM297'                else
  if UName='EBCDIC-CP-FR'            then result :='IBM297'                else
  if UName='CSIBM297'                then result :='IBM297'                else

(* Name: IBM420                                              [RFC1345,KXS2]   *)
(* MIBenum: 2041                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990,                          *)
(*         IBM NLS RM p 11-11                                                 *)
(* Alias: cp420                                                               *)
(* Alias: ebcdic-cp-ar1                                                       *)
(* Alias: csIBM420                                                            *)

  if UName='IBM420'                  then result :='IBM420'                else
  if UName='CP420'                   then result :='IBM420'                else
  if UName='EBCDIC-CP-AR1'           then result :='IBM420'                else
  if UName='CSIBM420'                then result :='IBM420'                else

(* Name: IBM423                                              [RFC1345,KXS2]   *)
(* MIBenum: 2042                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp423                                                               *)
(* Alias: ebcdic-cp-gr                                                        *)
(* Alias: csIBM423                                                            *)

  if UName='IBM423'                  then result :='IBM423'                else
  if UName='CP423'                   then result :='IBM423'                else
  if UName='EBCDIC-CP-GR'            then result :='IBM423'                else
  if UName='CSIBM423'                then result :='IBM423'                else

(* Name: IBM424                                              [RFC1345,KXS2]   *)
(* MIBenum: 2043                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp424                                                               *)
(* Alias: ebcdic-cp-he                                                        *)
(* Alias: csIBM424                                                            *)

  if UName='IBM424'                  then result :='IBM424'                else
  if UName='CP424'                   then result :='IBM424'                else
  if UName='EBCDIC-CP-HE'            then result :='IBM424'                else
  if UName='CSIBM424'                then result :='IBM424'                else

(* Name: IBM437                                              [RFC1345,KXS2]   *)
(* MIBenum: 2011                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp437                                                               *)
(* Alias: 437                                                                 *)
(* Alias: csPC8CodePage437                                                    *)

  if UName='IBM437'                  then result :='IBM437'                else
  if UName='CP437'                   then result :='IBM437'                else
  if UName='437'                     then result :='IBM437'                else
  if UName='CSPC8CODEPAGE437'        then result :='IBM437'                else

(* Name: IBM500                                              [RFC1345,KXS2]   *)
(* MIBenum: 2044                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP500                                                               *)
(* Alias: ebcdic-cp-be                                                        *)
(* Alias: ebcdic-cp-ch                                                        *)
(* Alias: csIBM500                                                            *)

  if UName='IBM500'                  then result :='IBM500'                else
  if UName='CP500'                   then result :='IBM500'                else
  if UName='EBCDIC-CP-BE'            then result :='IBM500'                else
  if UName='EBCDIC-CP-CH'            then result :='IBM500'                else
  if UName='CSIBM500'                then result :='IBM500'                else

(* Name: IBM775                                                   [HP-PCL5]   *)
(* MIBenum: 2087                                                              *)
(* Source: HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996            *)
(* Alias: cp775                                                               *)
(* Alias: csPC775Baltic                                                       *)

  if UName='IBM775'                  then result :='IBM775'                else
  if UName='CP775'                   then result :='IBM775'                else
  if UName='CSPC775BALTIC'           then result :='IBM775'                else

(* Name: IBM850                                              [RFC1345,KXS2]   *)
(* MIBenum: 2009                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp850                                                               *)
(* Alias: 850                                                                 *)
(* Alias: csPC850Multilingual                                                 *)

  if UName='IBM850'                  then result :='IBM850'                else
  if UName='CP850'                   then result :='IBM850'                else
  if UName='850'                     then result :='IBM850'                else
  if UName='CSPC850MULTILINGUAL'     then result :='IBM850'                else

(* Name: IBM851                                              [RFC1345,KXS2]   *)
(* MIBenum: 2045                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp851                                                               *)
(* Alias: 851                                                                 *)
(* Alias: csIBM851                                                            *)

  if UName='IBM851'                  then result :='IBM851'                else
  if UName='CP851'                   then result :='IBM851'                else
  if UName='851'                     then result :='IBM851'                else
  if UName='CSIBM851'                then result :='IBM851'                else

(* Name: IBM852                                              [RFC1345,KXS2]   *)
(* MIBenum: 2010                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp852                                                               *)
(* Alias: 852                                                                 *)
(* Alias: csPCp852                                                            *)

  if UName='IBM852'                  then result :='IBM852'                else
  if UName='CP852'                   then result :='IBM852'                else
  if UName='852'                     then result :='IBM852'                else
  if UName='CSPCP852'                then result :='IBM852'                else

(* Name: IBM855                                              [RFC1345,KXS2]   *)
(* MIBenum: 2046                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp855                                                               *)
(* Alias: 855                                                                 *)
(* Alias: csIBM855                                                            *)

  if UName='IBM855'                  then result :='IBM855'                else
  if UName='CP855'                   then result :='IBM855'                else
  if UName='855'                     then result :='IBM855'                else
  if UName='CSIBM855'                then result :='IBM855'                else

(* Name: IBM857                                              [RFC1345,KXS2]   *)
(* MIBenum: 2047                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp857                                                               *)
(* Alias: 857                                                                 *)
(* Alias: csIBM857                                                            *)

  if UName='IBM857'                  then result :='IBM857'                else
  if UName='CP857'                   then result :='IBM857'                else
  if UName='857'                     then result :='IBM857'                else
  if UName='CSIBM857'                then result :='IBM857'                else

(* Name: IBM860                                              [RFC1345,KXS2]   *)
(* MIBenum: 2048                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp860                                                               *)
(* Alias: 860                                                                 *)
(* Alias: csIBM860                                                            *)

  if UName='IBM860'                  then result :='IBM860'                else
  if UName='CP860'                   then result :='IBM860'                else
  if UName='860'                     then result :='IBM860'                else
  if UName='CSIBM860'                then result :='IBM860'                else

(* Name: IBM861                                              [RFC1345,KXS2]   *)
(* MIBenum: 2049                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp861                                                               *)
(* Alias: 861                                                                 *)
(* Alias: cp-is                                                               *)
(* Alias: csIBM861                                                            *)

  if UName='IBM861'                  then result :='IBM861'                else
  if UName='CP861'                   then result :='IBM861'                else
  if UName='861'                     then result :='IBM861'                else
  if UName='CP-IS'                   then result :='IBM861'                else
  if UName='CSIBM861'                then result :='IBM861'                else

(* Name: IBM862                                              [RFC1345,KXS2]   *)
(* MIBenum: 2013                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp862                                                               *)
(* Alias: 862                                                                 *)
(* Alias: csPC862LatinHebrew                                                  *)

  if UName='IBM862'                  then result :='IBM862'                else
  if UName='CP862'                   then result :='IBM862'                else
  if UName='862'                     then result :='IBM862'                else
  if UName='CSPC862LATINHEBREW'      then result :='IBM862'                else

(* Name: IBM863                                              [RFC1345,KXS2]   *)
(* MIBenum: 2050                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp863                                                               *)
(* Alias: 863                                                                 *)
(* Alias: csIBM863                                                            *)

  if UName='IBM863'                  then result :='IBM863'                else
  if UName='CP863'                   then result :='IBM863'                else
  if UName='863'                     then result :='IBM863'                else
  if UName='CSIBM863'                then result :='IBM863'                else

(* Name: IBM864                                              [RFC1345,KXS2]   *)
(* MIBenum: 2051                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp864                                                               *)
(* Alias: csIBM864                                                            *)

  if UName='IBM864'                  then result :='IBM864'                else
  if UName='CP864'                   then result :='IBM864'                else
  if UName='CSIBM864'                then result :='IBM864'                else

(* Name: IBM865                                              [RFC1345,KXS2]   *)
(* MIBenum: 2052                                                              *)
(* Source: IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987)                     *)
(* Alias: cp865                                                               *)
(* Alias: 865                                                                 *)
(* Alias: csIBM865                                                            *)

  if UName='IBM865'                  then result :='IBM865'                else
  if UName='CP865'                   then result :='IBM865'                else
  if UName='865'                     then result :='IBM865'                else
  if UName='CSIBM865'                then result :='IBM865'                else

(* Name: IBM866                                                     [Pond]    *)
(* MIBenum: 2086                                                              *)
(* Source: IBM NLDG Volume 2 (SE09-8002-03) August 1994                       *)
(* Alias: cp866                                                               *)
(* Alias: 866                                                                 *)
(* Alias: csIBM866                                                            *)

  if UName='IBM866'                  then result :='IBM866'                else
  if UName='CP866'                   then result :='IBM866'                else
  if UName='866'                     then result :='IBM866'                else
  if UName='CSIBM866'                then result :='IBM866'                else

(* Name: IBM868                                              [RFC1345,KXS2]   *)
(* MIBenum: 2053                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP868                                                               *)
(* Alias: cp-ar                                                               *)
(* Alias: csIBM868                                                            *)

  if UName='IBM868'                  then result :='IBM868'                else
  if UName='CP868'                   then result :='IBM868'                else
  if UName='CP-AR'                   then result :='IBM868'                else
  if UName='CSIBM868'                then result :='IBM868'                else

(* Name: IBM869                                              [RFC1345,KXS2]   *)
(* MIBenum: 2054                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp869                                                               *)
(* Alias: 869                                                                 *)
(* Alias: cp-gr                                                               *)
(* Alias: csIBM869                                                            *)

  if UName='IBM869'                  then result :='IBM869'                else
  if UName='CP869'                   then result :='IBM869'                else
  if UName='869'                     then result :='IBM869'                else
  if UName='CP-GR'                   then result :='IBM869'                else
  if UName='CSIBM869'                then result :='IBM869'                else

(* Name: IBM870                                              [RFC1345,KXS2]   *)
(* MIBenum: 2055                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP870                                                               *)
(* Alias: ebcdic-cp-roece                                                     *)
(* Alias: ebcdic-cp-yu                                                        *)
(* Alias: csIBM870                                                            *)

  if UName='IBM870'                  then result :='IBM870'                else
  if UName='CP870'                   then result :='IBM870'                else
  if UName='EBCDIC-CP-ROECE'         then result :='IBM870'                else
  if UName='EBCDIC-CP-YU'            then result :='IBM870'                else
  if UName='CSIBM870'                then result :='IBM870'                else

(* Name: IBM871                                              [RFC1345,KXS2]   *)
(* MIBenum: 2056                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP871                                                               *)
(* Alias: ebcdic-cp-is                                                        *)
(* Alias: csIBM871                                                            *)

  if UName='IBM871'                  then result :='IBM871'                else
  if UName='CP871'                   then result :='IBM871'                else
  if UName='EBCDIC-CP-IS'            then result :='IBM871'                else
  if UName='CSIBM871'                then result :='IBM871'                else

(* Name: IBM880                                              [RFC1345,KXS2]   *)
(* MIBenum: 2057                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp880                                                               *)
(* Alias: EBCDIC-Cyrillic                                                     *)
(* Alias: csIBM880                                                            *)

  if UName='IBM880'                  then result :='IBM880'                else
  if UName='CP880'                   then result :='IBM880'                else
  if UName='EBCDIC-CYRILLIC'         then result :='IBM880'                else
  if UName='CSIBM880'                then result :='IBM880'                else

(* Name: IBM891                                              [RFC1345,KXS2]   *)
(* MIBenum: 2058                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp891                                                               *)
(* Alias: csIBM891                                                            *)

  if UName='IBM891'                  then result :='IBM891'                else
  if UName='CP891'                   then result :='IBM891'                else
  if UName='CSIBM891'                then result :='IBM891'                else

(* Name: IBM903                                              [RFC1345,KXS2]   *)
(* MIBenum: 2059                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp903                                                               *)
(* Alias: csIBM903                                                            *)

  if UName='IBM903'                  then result :='IBM903'                else
  if UName='CP903'                   then result :='IBM903'                else
  if UName='CSIBM903'                then result :='IBM903'                else

(* Name: IBM904                                              [RFC1345,KXS2]   *)
(* MIBenum: 2060                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp904                                                               *)
(* Alias: 904                                                                 *)
(* Alias: csIBBM904                                                           *)

  if UName='IBM904'                  then result :='IBM904'                else
  if UName='CP904'                   then result :='IBM904'                else
  if UName='904'                     then result :='IBM904'                else
  if UName='CSIBBM904'               then result :='IBM904'                else

(* Name: IBM905                                              [RFC1345,KXS2]   *)
(* MIBenum: 2061                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: CP905                                                               *)
(* Alias: ebcdic-cp-tr                                                        *)
(* Alias: csIBM905                                                            *)

  if UName='IBM905'                  then result :='IBM905'                else
  if UName='CP905'                   then result :='IBM905'                else
  if UName='EBCDIC-CP-TR'            then result :='IBM905'                else
  if UName='CSIBM905'                then result :='IBM905'                else

(* Name: IBM918                                              [RFC1345,KXS2]   *)
(* MIBenum: 2062                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP918                                                               *)
(* Alias: ebcdic-cp-ar2                                                       *)
(* Alias: csIBM918                                                            *)

  if UName='IBM918'                  then result :='IBM918'                else
  if UName='CP918'                   then result :='IBM918'                else
  if UName='EBCDIC-CP-AR2'           then result :='IBM918'                else
  if UName='CSIBM918'                then result :='IBM918'                else

(* Name: IBM1026                                             [RFC1345,KXS2]   *)
(* MIBenum: 2063                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP1026                                                              *)
(* Alias: csIBM1026                                                           *)

  if UName='IBM1026'                 then result :='IBM1026'               else
  if UName='CP1026'                  then result :='IBM1026'               else
  if UName='CSIBM1026'               then result :='IBM1026'               else

(* Name: EBCDIC-AT-DE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2064                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csIBMEBCDICATDE                                                     *)

  if UName='EBCDIC-AT-DE'            then result :='EBCDIC-AT-DE'          else
  if UName='CSIBMEBCDICATDE'         then result :='EBCDIC-AT-DE'          else

(* Name: EBCDIC-AT-DE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2065                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICATDEA                                                       *)

  if UName='EBCDIC-AT-DE-A'          then result :='EBCDIC-AT-DE-A'        else
  if UName='CSEBCDICATDEA'           then result :='EBCDIC-AT-DE-A'        else

(* Name: EBCDIC-CA-FR                                        [RFC1345,KXS2]   *)
(* MIBenum: 2066                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICCAFR                                                        *)

  if UName='EBCDIC-CA-FR'            then result :='EBCDIC-CA-FR'          else
  if UName='CSEBCDICCAFR'            then result :='EBCDIC-CA-FR'          else

(* Name: EBCDIC-DK-NO                                        [RFC1345,KXS2]   *)
(* MIBenum: 2067                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNO                                                        *)

  if UName='EBCDIC-DK-NO'            then result :='EBCDIC-DK-NO'          else
  if UName='CSEBCDICDKNO'            then result :='EBCDIC-DK-NO'          else

(* Name: EBCDIC-DK-NO-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2068                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNOA                                                       *)

  if UName='EBCDIC-DK-NO-A'          then result :='EBCDIC-DK-NO-A'        else
  if UName='CSEBCDICDKNOA'           then result :='EBCDIC-DK-NO-A'        else

(* Name: EBCDIC-FI-SE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2069                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISE                                                        *)

  if UName='EBCDIC-FI-SE'            then result :='EBCDIC-FI-SE'          else
  if UName='CSEBCDICFISE'            then result :='EBCDIC-FI-SE'          else

(* Name: EBCDIC-FI-SE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2070                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISEA                                                       *)

  if UName='EBCDIC-FI-SE-A'          then result :='EBCDIC-FI-SE-A'        else
  if UName='CSEBCDICFISEA'           then result :='EBCDIC-FI-SE-A'        else

(* Name: EBCDIC-FR                                           [RFC1345,KXS2]   *)
(* MIBenum: 2071                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFR                                                          *)

  if UName='EBCDIC-FR'               then result :='EBCDIC-FR'             else
  if UName='CSEBCDICFR'              then result :='EBCDIC-FR'             else

(* Name: EBCDIC-IT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2072                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICIT                                                          *)

  if UName='EBCDIC-IT'               then result :='EBCDIC-IT'             else
  if UName='CSEBCDICIT'              then result :='EBCDIC-IT'             else

(* Name: EBCDIC-PT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2073                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICPT                                                          *)

  if UName='EBCDIC-PT'               then result :='EBCDIC-PT'             else
  if UName='CSEBCDICPT'              then result :='EBCDIC-PT'             else

(* Name: EBCDIC-ES                                           [RFC1345,KXS2]   *)
(* MIBenum: 2074                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICES                                                          *)

  if UName='EBCDIC-ES'               then result :='EBCDIC-ES'             else
  if UName='CSEBCDICES'              then result :='EBCDIC-ES'             else

(* Name: EBCDIC-ES-A                                         [RFC1345,KXS2]   *)
(* MIBenum: 2075                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESA                                                         *)

  if UName='EBCDIC-ES-A'             then result :='EBCDIC-ES-A'           else
  if UName='CSEBCDICESA'             then result :='EBCDIC-ES-A'           else

(* Name: EBCDIC-ES-S                                         [RFC1345,KXS2]   *)
(* MIBenum: 2076                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESS                                                         *)

  if UName='EBCDIC-ES-S'             then result :='EBCDIC-ES-S'           else
  if UName='CSEBCDICESS'             then result :='EBCDIC-ES-S'           else

(* Name: EBCDIC-UK                                           [RFC1345,KXS2]   *)
(* MIBenum: 2077                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUK                                                          *)

  if UName='EBCDIC-UK'               then result :='EBCDIC-UK'             else
  if UName='CSEBCDICUK'              then result :='EBCDIC-UK'             else

(* Name: EBCDIC-US                                           [RFC1345,KXS2]   *)
(* MIBenum: 2078                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUS                                                          *)

  if UName='EBCDIC-US'               then result :='EBCDIC-US'             else
  if UName='CSEBCDICUS'              then result :='EBCDIC-US'             else

(* Name: UNKNOWN-8BIT                                             [RFC1428]   *)
(* MIBenum: 2079                                                              *)
(* Alias: csUnknown8BiT                                                       *)

  if UName='UNKNOWN-8BIT'            then result :='UNKNOWN-8BIT'          else
  if UName='CSUNKNOWN8BIT'           then result :='UNKNOWN-8BIT'          else

(* Name: MNEMONIC                                            [RFC1345,KXS2]   *)
(* MIBenum: 2080                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+38"                        *)
(* Alias: csMnemonic                                                          *)

  if UName='MNEMONIC'                then result :='MNEMONIC'              else
  if UName='CSMNEMONIC'              then result :='MNEMONIC'              else

(* Name: MNEM                                                [RFC1345,KXS2]   *)
(* MIBenum: 2081                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+8200"                      *)
(* Alias: csMnem                                                              *)

  if UName='MNEM'                    then result :='MNEM'                  else
  if UName='CSMNEM'                  then result :='MNEM'                  else

(* Name: VISCII                                                   [RFC1456]   *)
(* MIBenum: 2082                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVISCII                                                            *)

  if UName='VISCII'                  then result :='VISCII'                else
  if UName='CSVISCII'                then result :='VISCII'                else

(* Name: VIQR                                                     [RFC1456]   *)
(* MIBenum: 2083                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVIQR                                                              *)

  if UName='VIQR'                    then result :='VIQR'                  else
  if UName='CSVIQR'                  then result :='VIQR'                  else

(* Name: KOI8-R  (preferred MIME name)                            [RFC1489]   *)
(* MIBenum: 2084                                                              *)
(* Source: RFC 1489, based on GOST-19768-74, ISO-6937/8,                      *)
(*         INIS-Cyrillic, ISO-5427.                                           *)
(* Alias: csKOI8R                                                             *)

  if UName='KOI8-R'                  then result :='KOI8-R'                else
  if UName='CSKOI8R'                 then result :='KOI8-R'                else

(* Name: KOI8-U                                                   [RFC2319]   *)
(* MIBenum: 2088                                                              *)
(* Source: RFC 2319                                                           *)

  if UName='KOI8-U'                  then result :='KOI8-U'                else

(* Name: IBM00858                                                             *)
(* MIBenum: 2089                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM00858)    [Mahdi] *)
(* Alias: CCSID00858                                                          *)
(* Alias: CP00858                                                             *)
(* Alias: PC-Multilingual-850+euro                                            *)

  if UName='IBM00858'                then result :='IBM00858'              else
  if UName='CCSID00858'              then result :='IBM00858'              else
  if UName='CP00858'                 then result :='IBM00858'              else
  if UName='PC-MULTILINGUAL-850+EURO' then result :='IBM00858'             else

(* Name: IBM00924                                                             *)
(* MIBenum: 2090                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM00924)    [Mahdi] *)
(* Alias: CCSID00924                                                          *)
(* Alias: CP00924                                                             *)
(* Alias: ebcdic-Latin9--euro                                                 *)

  if UName='IBM00924'                then result :='IBM00924'              else
  if UName='CCSID00924'              then result :='IBM00924'              else
  if UName='CP00924'                 then result :='IBM00924'              else
  if UName='EBCDIC-LATIN9--EURO'     then result :='IBM00924'              else

(* Name: IBM01140                                                             *)
(* MIBenum: 2091                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01140)    [Mahdi] *)
(* Alias: CCSID01140                                                          *)
(* Alias: CP01140                                                             *)
(* Alias: ebcdic-us-37+euro                                                   *)

  if UName='IBM01140'                then result :='IBM01140'              else
  if UName='CCSID01140'              then result :='IBM01140'              else
  if UName='CP01140'                 then result :='IBM01140'              else
  if UName='EBCDIC-US-37+EURO'       then result :='IBM01140'              else

(* Name: IBM01141                                                             *)
(* MIBenum: 2092                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01141)    [Mahdi] *)
(* Alias: CCSID01141                                                          *)
(* Alias: CP01141                                                             *)
(* Alias: ebcdic-de-273+euro                                                  *)

  if UName='IBM01141'                then result :='IBM01141'              else
  if UName='CCSID01141'              then result :='IBM01141'              else
  if UName='CP01141'                 then result :='IBM01141'              else
  if UName='EBCDIC-DE-273+EURO'      then result :='IBM01141'              else

(* Name: IBM01142                                                             *)
(* MIBenum: 2093                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01142)    [Mahdi] *)
(* Alias: CCSID01142                                                          *)
(* Alias: CP01142                                                             *)
(* Alias: ebcdic-dk-277+euro                                                  *)
(* Alias: ebcdic-no-277+euro                                                  *)

  if UName='IBM01142'                then result :='IBM01142'              else
  if UName='CCSID01142'              then result :='IBM01142'              else
  if UName='CP01142'                 then result :='IBM01142'              else
  if UName='EBCDIC-DK-277+EURO'      then result :='IBM01142'              else
  if UName='EBCDIC-NO-277+EURO'      then result :='IBM01142'              else

(* Name: IBM01143                                                             *)
(* MIBenum: 2094                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01143)    [Mahdi] *)
(* Alias: CCSID01143                                                          *)
(* Alias: CP01143                                                             *)
(* Alias: ebcdic-fi-278+euro                                                  *)
(* Alias: ebcdic-se-278+euro                                                  *)

  if UName='IBM01143'                then result :='IBM01143'              else
  if UName='CCSID01143'              then result :='IBM01143'              else
  if UName='CP01143'                 then result :='IBM01143'              else
  if UName='EBCDIC-FI-278+EURO'      then result :='IBM01143'              else
  if UName='EBCDIC-SE-278+EURO'      then result :='IBM01143'              else

(* Name: IBM01144                                                             *)
(* MIBenum: 2095                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01144)    [Mahdi] *)
(* Alias: CCSID01144                                                          *)
(* Alias: CP01144                                                             *)
(* Alias: ebcdic-it-280+euro                                                  *)

  if UName='IBM01144'                then result :='IBM01144'              else
  if UName='CCSID01144'              then result :='IBM01144'              else
  if UName='CP01144'                 then result :='IBM01144'              else
  if UName='EBCDIC-IT-280+EURO'      then result :='IBM01144'              else

(* Name: IBM01145                                                             *)
(* MIBenum: 2096                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01145)    [Mahdi] *)
(* Alias: CCSID01145                                                          *)
(* Alias: CP01145                                                             *)
(* Alias: ebcdic-es-284+euro                                                  *)

  if UName='IBM01145'                then result :='IBM01145'              else
  if UName='CCSID01145'              then result :='IBM01145'              else
  if UName='CP01145'                 then result :='IBM01145'              else
  if UName='EBCDIC-ES-284+EURO'      then result :='IBM01145'              else

(* Name: IBM01146                                                             *)
(* MIBenum: 2097                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01146)    [Mahdi] *)
(* Alias: CCSID01146                                                          *)
(* Alias: CP01146                                                             *)
(* Alias: ebcdic-gb-285+euro                                                  *)

  if UName='IBM01146'                then result :='IBM01146'              else
  if UName='CCSID01146'              then result :='IBM01146'              else
  if UName='CP01146'                 then result :='IBM01146'              else
  if UName='EBCDIC-GB-285+EURO'      then result :='IBM01146'              else

(* Name: IBM01147                                                             *)
(* MIBenum: 2098                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01147)    [Mahdi] *)
(* Alias: CCSID01147                                                          *)
(* Alias: CP01147                                                             *)
(* Alias: ebcdic-fr-297+euro                                                  *)

  if UName='IBM01147'                then result :='IBM01147'              else
  if UName='CCSID01147'              then result :='IBM01147'              else
  if UName='CP01147'                 then result :='IBM01147'              else
  if UName='EBCDIC-FR-297+EURO'      then result :='IBM01147'              else

(* Name: IBM01148                                                             *)
(* MIBenum: 2099                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01148)    [Mahdi] *)
(* Alias: CCSID01148                                                          *)
(* Alias: CP01148                                                             *)
(* Alias: ebcdic-international-500+euro                                       *)

  if UName='IBM01148'                then result :='IBM01148'              else
  if UName='CCSID01148'              then result :='IBM01148'              else
  if UName='CP01148'                 then result :='IBM01148'              else
  if UName='EBCDIC-INTERNATIONAL-500+EURO' then result :='IBM01148'        else

(* Name: IBM01149                                                             *)
(* MIBenum: 2100                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01149)    [Mahdi] *)
(* Alias: CCSID01149                                                          *)
(* Alias: CP01149                                                             *)
(* Alias: ebcdic-is-871+euro                                                  *)

  if UName='IBM01149'                then result :='IBM01149'              else
  if UName='CCSID01149'              then result :='IBM01149'              else
  if UName='CP01149'                 then result :='IBM01149'              else
  if UName='EBCDIC-IS-871+EURO'      then result :='IBM01149'              else

(* Name: Big5-HKSCS                                                  [Yick]   *)
(* MIBenum: 2101                                                              *)
(* Source:   See (http://www.iana.org/assignments/charset-reg/Big5-HKSCS)     *)
(* Alias: None                                                                *)

  if UName='BIG5-HKSCS'              then result :='Big5-HKSCS'            else

(* Name: IBM1047                                                [Robrigado]   *)
(* MIBenum: 2102                                                              *)
(* Source: IBM1047 (EBCDIC Latin 1/Open Systems)                              *)
(* http://www-1.ibm.com/servers/eserver/iseries/software/globalization/pdf/cp01047z.pdf *)
(* Alias: IBM-1047                                                            *)

  if UName='IBM1047'                 then result :='IBM1047'               else
  if UName='IBM-1047'                then result :='IBM1047'               else

(* Name: PTCP154                                                    [Uskov]   *)
(* MIBenum: 2103                                                              *)
(* Source: See (http://www.iana.org/assignments/charset-reg/PTCP154)          *)
(* Alias: csPTCP154                                                           *)
(* Alias: PT154                                                               *)
(* Alias: CP154                                                               *)
(* Alias: Cyrillic-Asian                                                      *)

  if UName='PTCP154'                 then result :='PTCP154'               else
  if UName='CSPTCP154'               then result :='PTCP154'               else
  if UName='PT154'                   then result :='PTCP154'               else
  if UName='CP154'                   then result :='PTCP154'               else
  if UName='CYRILLIC-ASIAN'          then result :='PTCP154'               else

(* Name: UNICODE-1-1                                              [RFC1641]   *)
(* MIBenum: 1010                                                              *)
(* Source: RFC 1641                                                           *)
(* Alias: csUnicode11                                                         *)

  if UName='UNICODE-1-1'             then result :='UNICODE-1-1'           else
  if UName='CSUNICODE11'             then result :='UNICODE-1-1'           else

(* Name: SCSU                                                                 *)
(* MIBenum: 1011                                                              *)
(* Source: SCSU See (http://www.iana.org/assignments/charset-reg/SCSU)     [Scherer] *)
(* Alias: None                                                                *)

  if UName='SCSU'                    then result :='SCSU'                  else

(* Name: UTF-7                                                    [RFC2152]   *)
(* MIBenum: 1012                                                              *)
(* Source: RFC 2152                                                           *)
(* Alias: None                                                                *)

  if UName='UTF-7'                   then result :='UTF-7'                 else

(* Name: UTF-16BE                                                 [RFC2781]   *)
(* MIBenum: 1013                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if UName='UTF-16BE'                then result :='UTF-16BE'              else

(* Name: UTF-16LE                                                 [RFC2781]   *)
(* MIBenum: 1014                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if UName='UTF-16LE'                then result :='UTF-16LE'              else

(* Name: UTF-16                                                   [RFC2781]   *)
(* MIBenum: 1015                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if UName='UTF-16'                  then result :='UTF-16'                else

(* Name: CESU-8                                                    [Phipps]   *)
(* MIBenum: 1016                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr26>                      *)
(* Alias: csCESU-8                                                            *)

  if UName='CESU-8'                  then result :='CESU-8'                else
  if UName='CSCESU-8'                then result :='CESU-8'                else

(* Name: UTF-32                                                     [Davis]   *)
(* MIBenum: 1017                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)
(* Alias: None                                                                *)

  if UName='UTF-32'                  then result :='UTF-32'                else

(* Name: UTF-32BE                                                   [Davis]   *)
(* MIBenum: 1018                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)
(* Alias: None                                                                *)

  if UName='UTF-32BE'                then result :='UTF-32BE'              else

(* Name: UTF-32LE                                                   [Davis]   *)
(* MIBenum: 1019                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)
(* Alias: None                                                                *)

  if UName='UTF-32LE'                then result :='UTF-32LE'              else

(* Name: BOCU-1                                                   [Scherer]   *)
(* MIBenum: 1020                                                              *)
(* Source: http://www.unicode.org/notes/tn6/                                  *)
(* Alias: csBOCU-1                                                            *)

  if UName='BOCU-1'                  then result :='BOCU-1'                else
  if UName='CSBOCU-1'                then result :='BOCU-1'                else

(* Name: UNICODE-1-1-UTF-7                                        [RFC1642]   *)
(* MIBenum: 103                                                               *)
(* Source: RFC 1642                                                           *)
(* Alias: csUnicode11UTF7                                                     *)

  if UName='UNICODE-1-1-UTF-7'       then result :='UNICODE-1-1-UTF-7'     else
  if UName='CSUNICODE11UTF7'         then result :='UNICODE-1-1-UTF-7'     else

(* Name: UTF-8                                                    [RFC2279]   *)
(* MIBenum: 106                                                               *)
(* Source: RFC 2279                                                           *)
(* Alias: None                                                                *)

  if UName='UTF-8'                   then result :='UTF-8'                 else

(* Name: ISO-8859-13                                                          *)
(* MIBenum: 109                                                               *)
(* Source: ISO See (http://www.iana.org/assignments/charset-reg/iso-8859-13)[Tumasonis]  *)
(* Alias: None                                                                *)

  if UName='ISO-8859-13'             then result :='ISO-8859-13'           else

(* Name: ISO-8859-14                                                          *)
(* MIBenum: 110                                                               *)
(* Source: ISO See (http://www.iana.org/assignments/charset-reg/iso-8859-14) [Simonsen] *)
(* Alias: iso-ir-199                                                          *)
(* Alias: ISO_8859-14:1998                                                    *)
(* Alias: ISO_8859-14                                                         *)
(* Alias: latin8                                                              *)
(* Alias: iso-celtic                                                          *)
(* Alias: l8                                                                  *)

  if UName='ISO-8859-14'             then result :='ISO-8859-14'           else
  if UName='ISO-IR-199'              then result :='ISO-8859-14'           else
  if UName='ISO_8859-14:1998'        then result :='ISO-8859-14'           else
  if UName='ISO_8859-14'             then result :='ISO-8859-14'           else
  if UName='LATIN8'                  then result :='ISO-8859-14'           else
  if UName='ISO-CELTIC'              then result :='ISO-8859-14'           else
  if UName='L8'                      then result :='ISO-8859-14'           else

(* Name: ISO-8859-15                                                          *)
(* MIBenum: 111                                                               *)
(* Source: ISO                                                                *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/ISO-8859-15> *)
(* Alias: ISO_8859-15                                                         *)
(* Alias: Latin-9                                                             *)

  if UName='ISO-8859-15'             then result :='ISO-8859-15'           else
  if UName='ISO_8859-15'             then result :='ISO-8859-15'           else
  if UName='LATIN-9'                 then result :='ISO-8859-15'           else

(* Name: ISO-8859-16                                                          *)
(* MIBenum: 112                                                               *)
(* Source: ISO                                                                *)
(* Alias: iso-ir-226                                                          *)
(* Alias: ISO_8859-16:2001                                                    *)
(* Alias: ISO_8859-16                                                         *)
(* Alias: latin10                                                             *)
(* Alias: l10                                                                 *)

  if UName='ISO-8859-16'             then result :='ISO-8859-16'           else
  if UName='ISO-IR-226'              then result :='ISO-8859-16'           else
  if UName='ISO_8859-16:2001'        then result :='ISO-8859-16'           else
  if UName='ISO_8859-16'             then result :='ISO-8859-16'           else
  if UName='LATIN10'                 then result :='ISO-8859-16'           else
  if UName='L10'                     then result :='ISO-8859-16'           else

(* Name: GBK                                                                  *)
(* MIBenum: 113                                                               *)
(* Source: Chinese IT Standardization Technical Committee                     *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/GBK>      *)
(* Alias: CP936                                                               *)
(* Alias: MS936                                                               *)
(* Alias: windows-936                                                         *)

  if UName='GBK'                     then result :='GBK'                   else
  if UName='CP936'                   then result :='GBK'                   else
  if UName='MS936'                   then result :='GBK'                   else
  if UName='WINDOWS-936'             then result :='GBK'                   else

(* Name: GB18030                                                              *)
(* MIBenum: 114                                                               *)
(* Source: Chinese IT Standardization Technical Committee                     *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/GB18030>  *)
(* Alias: None                                                                *)

  if UName='GB18030'                 then result :='GB18030'               else

(* Name: JIS_Encoding                                                         *)
(* MIBenum: 16                                                                *)
(* Source: JIS X 0202-1991.  Uses ISO 2022 escape sequences to                *)
(*         shift code sets as documented in JIS X 0202-1991.                  *)
(* Alias: csJISEncoding                                                       *)

  if UName='JIS_ENCODING'            then result :='JIS_Encoding'          else
  if UName='CSJISENCODING'           then result :='JIS_Encoding'          else

(* Name: Shift_JIS  (preferred MIME name)                                     *)
(* MIBenum: 17                                                                *)
(* Source: This charset is an extension of csHalfWidthKatakana by             *)
(*         adding graphic characters in JIS X 0208.  The CCS's are            *)
(*         JIS X0201:1997 and JIS X0208:1997.  The                            *)
(*         complete definition is shown in Appendix 1 of JIS                  *)
(*         X0208:1997.                                                        *)
(*         This charset can be used for the top-level media type "text".      *)
(* Alias: MS_Kanji                                                            *)
(* Alias: csShiftJIS                                                          *)

  if UName='SHIFT_JIS'               then result :='Shift_JIS'             else
  if UName='MS_KANJI'                then result :='Shift_JIS'             else
  if UName='CSSHIFTJIS'              then result :='Shift_JIS'             else

(* Name: Extended_UNIX_Code_Packed_Format_for_Japanese                        *)
(* MIBenum: 18                                                                *)
(* Source: Standardized by OSF, UNIX International, and UNIX Systems          *)
(*         Laboratories Pacific.  Uses ISO 2022 rules to select               *)
(*                code set 0: US-ASCII (a single 7-bit byte set)              *)
(*                code set 1: JIS X0208-1990 (a double 8-bit byte set)        *)
(*                            restricted to A0-FF in both bytes               *)
(*                code set 2: Half Width Katakana (a single 7-bit byte set)   *)
(*                            requiring SS2 as the character prefix           *)
(*                code set 3: JIS X0212-1990 (a double 7-bit byte set)        *)
(*                            restricted to A0-FF in both bytes               *)
(*                            requiring SS3 as the character prefix           *)
(* Alias: csEUCPkdFmtJapanese                                                 *)
(* Alias: EUC-JP  (preferred MIME name)                                       *)

  if UName='EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE' then result :='EUC-JP' else
  if UName='CSEUCPKDFMTJAPANESE'     then result :='EUC-JP'                else
  if UName='EUC-JP'                  then result :='EUC-JP'                else

(* Name: Extended_UNIX_Code_Fixed_Width_for_Japanese                          *)
(* MIBenum: 19                                                                *)
(* Source: Used in Japan.  Each character is 2 octets.                        *)
(*                 code set 0: US-ASCII (a single 7-bit byte set)             *)
(*                               1st byte = 00                                *)
(*                               2nd byte = 20-7E                             *)
(*                 code set 1: JIS X0208-1990 (a double 7-bit byte set)       *)
(*                             restricted  to A0-FF in both bytes             *)
(*                 code set 2: Half Width Katakana (a single 7-bit byte set)  *)
(*                               1st byte = 00                                *)
(*                               2nd byte = A0-FF                             *)
(*                 code set 3: JIS X0212-1990 (a double 7-bit byte set)       *)
(*                             restricted to A0-FF in                         *)
(*                             the first byte                                 *)
(*                 and 21-7E in the second byte                               *)
(* Alias: csEUCFixWidJapanese                                                 *)

  if UName='EXTENDED_UNIX_CODE_FIXED_WIDTH_FOR_JAPANESE' then result :='Extended_UNIX_Code_Fixed_Width_for_Japanese' else
  if UName='CSEUCFIXWIDJAPANESE'     then result :='Extended_UNIX_Code_Fixed_Width_for_Japanese' else

(* Name: ISO-10646-UCS-Basic                                                  *)
(* MIBenum: 1002                                                              *)
(* Source: ASCII subset of Unicode.  Basic Latin = collection 1               *)
(*         See ISO 10646, Appendix A                                          *)
(* Alias: csUnicodeASCII                                                      *)

  if UName='ISO-10646-UCS-BASIC'     then result :='ISO-10646-UCS-Basic'   else
  if UName='CSUNICODEASCII'          then result :='ISO-10646-UCS-Basic'   else

(* Name: ISO-10646-Unicode-Latin1                                             *)
(* MIBenum: 1003                                                              *)
(* Source: ISO Latin-1 subset of Unicode. Basic Latin and Latin-1             *)
(*          Supplement  = collections 1 and 2.  See ISO 10646,                *)
(*          Appendix A.  See RFC 1815.                                        *)
(* Alias: csUnicodeLatin1                                                     *)
(* Alias: ISO-10646                                                           *)

  if UName='ISO-10646-UNICODE-LATIN1' then result :='ISO-10646-Unicode-Latin1' else
  if UName='CSUNICODELATIN1'         then result :='ISO-10646-Unicode-Latin1' else
  if UName='ISO-10646'               then result :='ISO-10646-Unicode-Latin1' else

(* Name: ISO-10646-J-1                                                        *)
(* Source: ISO 10646 Japanese, see RFC 1815.                                  *)

  if UName='ISO-10646-J-1'           then result :='ISO-10646-J-1'         else

(* Name: ISO-Unicode-IBM-1261                                                 *)
(* MIBenum: 1005                                                              *)
(* Source: IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261       *)
(* Alias: csUnicodeIBM1261                                                    *)

  if UName='ISO-UNICODE-IBM-1261'    then result :='ISO-Unicode-IBM-1261'  else
  if UName='CSUNICODEIBM1261'        then result :='ISO-Unicode-IBM-1261'  else

(* Name: ISO-Unicode-IBM-1268                                                 *)
(* MIBenum: 1006                                                              *)
(* Source: IBM Latin-4 Extended Presentation Set, GCSGID: 1268                *)
(* Alias: csUnicodeIBM1268                                                    *)

  if UName='ISO-UNICODE-IBM-1268'    then result :='ISO-Unicode-IBM-1268'  else
  if UName='CSUNICODEIBM1268'        then result :='ISO-Unicode-IBM-1268'  else

(* Name: ISO-Unicode-IBM-1276                                                 *)
(* MIBenum: 1007                                                              *)
(* Source: IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276         *)
(* Alias: csUnicodeIBM1276                                                    *)

  if UName='ISO-UNICODE-IBM-1276'    then result :='ISO-Unicode-IBM-1276'  else
  if UName='CSUNICODEIBM1276'        then result :='ISO-Unicode-IBM-1276'  else

(* Name: ISO-Unicode-IBM-1264                                                 *)
(* MIBenum: 1008                                                              *)
(* Source: IBM Arabic Presentation Set, GCSGID: 1264                          *)
(* Alias: csUnicodeIBM1264                                                    *)

  if UName='ISO-UNICODE-IBM-1264'    then result :='ISO-Unicode-IBM-1264'  else
  if UName='CSUNICODEIBM1264'        then result :='ISO-Unicode-IBM-1264'  else

(* Name: ISO-Unicode-IBM-1265                                                 *)
(* MIBenum: 1009                                                              *)
(* Source: IBM Hebrew Presentation Set, GCSGID: 1265                          *)
(* Alias: csUnicodeIBM1265                                                    *)

  if UName='ISO-UNICODE-IBM-1265'    then result :='ISO-Unicode-IBM-1265'  else
  if UName='CSUNICODEIBM1265'        then result :='ISO-Unicode-IBM-1265'  else

(* Name: ISO-8859-1-Windows-3.0-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2000                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.0.                       *)
(*         PCL Symbol Set id: 9U                                              *)
(* Alias: csWindows30Latin1                                                   *)

  if UName='ISO-8859-1-WINDOWS-3.0-LATIN-1' then result :='ISO-8859-1-Windows-3.0-Latin-1' else
  if UName='CSWINDOWS30LATIN1'       then result :='ISO-8859-1-Windows-3.0-Latin-1' else

(* Name: ISO-8859-1-Windows-3.1-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2001                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.1.                       *)
(*         PCL Symbol Set id: 19U                                             *)
(* Alias: csWindows31Latin1                                                   *)

  if UName='ISO-8859-1-WINDOWS-3.1-LATIN-1' then result :='ISO-8859-1-Windows-3.1-Latin-1' else
  if UName='CSWINDOWS31LATIN1'       then result :='ISO-8859-1-Windows-3.1-Latin-1' else

(* Name: ISO-8859-2-Windows-Latin-2                               [HP-PCL5]   *)
(* MIBenum: 2002                                                              *)
(* Source: Extended ISO 8859-2.  Latin-2 for Windows 3.1.                     *)
(*         PCL Symbol Set id: 9E                                              *)
(* Alias: csWindows31Latin2                                                   *)

  if UName='ISO-8859-2-WINDOWS-LATIN-2' then result :='ISO-8859-2-Windows-Latin-2' else
  if UName='CSWINDOWS31LATIN2'       then result :='ISO-8859-2-Windows-Latin-2' else

(* Name: ISO-8859-9-Windows-Latin-5                               [HP-PCL5]   *)
(* MIBenum: 2003                                                              *)
(* Source: Extended ISO 8859-9.  Latin-5 for Windows 3.1                      *)
(*         PCL Symbol Set id: 5T                                              *)
(* Alias: csWindows31Latin5                                                   *)

  if UName='ISO-8859-9-WINDOWS-LATIN-5' then result :='ISO-8859-9-Windows-Latin-5' else
  if UName='CSWINDOWS31LATIN5'       then result :='ISO-8859-9-Windows-Latin-5' else

(* Name: Adobe-Standard-Encoding                                    [Adobe]   *)
(* MIBenum: 2005                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 10J                                             *)
(* Alias: csAdobeStandardEncoding                                             *)

  if UName='ADOBE-STANDARD-ENCODING' then result :='Adobe-Standard-Encoding' else
  if UName='CSADOBESTANDARDENCODING' then result :='Adobe-Standard-Encoding' else

(* Name: Ventura-US                                               [HP-PCL5]   *)
(* MIBenum: 2006                                                              *)
(* Source: Ventura US.  ASCII plus characters typically used in               *)
(*         publishing, like pilcrow, copyright, registered, trade mark,       *)
(*         section, dagger, and double dagger in the range A0 (hex)           *)
(*         to FF (hex).                                                       *)
(*         PCL Symbol Set id: 14J                                             *)
(* Alias: csVenturaUS                                                         *)

  if UName='VENTURA-US'              then result :='Ventura-US'            else
  if UName='CSVENTURAUS'             then result :='Ventura-US'            else

(* Name: Ventura-International                                    [HP-PCL5]   *)
(* MIBenum: 2007                                                              *)
(* Source: Ventura International.  ASCII plus coded characters similar        *)
(*         to Roman8.                                                         *)
(*         PCL Symbol Set id: 13J                                             *)
(* Alias: csVenturaInternational                                              *)

  if UName='VENTURA-INTERNATIONAL'   then result :='Ventura-International' else
  if UName='CSVENTURAINTERNATIONAL'  then result :='Ventura-International' else

(* Name: PC8-Danish-Norwegian                                     [HP-PCL5]   *)
(* MIBenum: 2012                                                              *)
(* Source: PC Danish Norwegian                                                *)
(*         8-bit PC set for Danish Norwegian                                  *)
(*         PCL Symbol Set id: 11U                                             *)
(* Alias: csPC8DanishNorwegian                                                *)

  if UName='PC8-DANISH-NORWEGIAN'    then result :='PC8-Danish-Norwegian'  else
  if UName='CSPC8DANISHNORWEGIAN'    then result :='PC8-Danish-Norwegian'  else

(* Name: PC8-Turkish                                              [HP-PCL5]   *)
(* MIBenum: 2014                                                              *)
(* Source: PC Latin Turkish.  PCL Symbol Set id: 9T                           *)
(* Alias: csPC8Turkish                                                        *)

  if UName='PC8-TURKISH'             then result :='PC8-Turkish'           else
  if UName='CSPC8TURKISH'            then result :='PC8-Turkish'           else

(* Name: IBM-Symbols                                             [IBM-CIDT]   *)
(* MIBenum: 2015                                                              *)
(* Source: Presentation Set, CPGID: 259                                       *)
(* Alias: csIBMSymbols                                                        *)

  if UName='IBM-SYMBOLS'             then result :='IBM-Symbols'           else
  if UName='CSIBMSYMBOLS'            then result :='IBM-Symbols'           else

(* Name: IBM-Thai                                                [IBM-CIDT]   *)
(* MIBenum: 2016                                                              *)
(* Source: Presentation Set, CPGID: 838                                       *)
(* Alias: csIBMThai                                                           *)

  if UName='IBM-THAI'                then result :='IBM-Thai'              else
  if UName='CSIBMTHAI'               then result :='IBM-Thai'              else

(* Name: HP-Legal                                                 [HP-PCL5]   *)
(* MIBenum: 2017                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 1U                                              *)
(* Alias: csHPLegal                                                           *)

  if UName='HP-LEGAL'                then result :='HP-Legal'              else
  if UName='CSHPLEGAL'               then result :='HP-Legal'              else

(* Name: HP-Pi-font                                               [HP-PCL5]   *)
(* MIBenum: 2018                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 15U                                             *)
(* Alias: csHPPiFont                                                          *)

  if UName='HP-PI-FONT'              then result :='HP-Pi-font'            else
  if UName='CSHPPIFONT'              then result :='HP-Pi-font'            else

(* Name: HP-Math8                                                 [HP-PCL5]   *)
(* MIBenum: 2019                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 8M                                              *)
(* Alias: csHPMath8                                                           *)

  if UName='HP-MATH8'                then result :='HP-Math8'              else
  if UName='CSHPMATH8'               then result :='HP-Math8'              else

(* Name: Adobe-Symbol-Encoding                                      [Adobe]   *)
(* MIBenum: 2020                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 5M                                              *)
(* Alias: csHPPSMath                                                          *)

  if UName='ADOBE-SYMBOL-ENCODING'   then result :='Adobe-Symbol-Encoding' else
  if UName='CSHPPSMATH'              then result :='Adobe-Symbol-Encoding' else

(* Name: HP-DeskTop                                               [HP-PCL5]   *)
(* MIBenum: 2021                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 7J                                              *)
(* Alias: csHPDesktop                                                         *)

  if UName='HP-DESKTOP'              then result :='HP-DeskTop'            else
  if UName='CSHPDESKTOP'             then result :='HP-DeskTop'            else

(* Name: Ventura-Math                                             [HP-PCL5]   *)
(* MIBenum: 2022                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6M                                              *)
(* Alias: csVenturaMath                                                       *)

  if UName='VENTURA-MATH'            then result :='Ventura-Math'          else
  if UName='CSVENTURAMATH'           then result :='Ventura-Math'          else

(* Name: Microsoft-Publishing                                     [HP-PCL5]   *)
(* MIBenum: 2023                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6J                                              *)
(* Alias: csMicrosoftPublishing                                               *)

  if UName='MICROSOFT-PUBLISHING'    then result :='Microsoft-Publishing'  else
  if UName='CSMICROSOFTPUBLISHING'   then result :='Microsoft-Publishing'  else

(* Name: Windows-31J                                                          *)
(* MIBenum: 2024                                                              *)
(* Source: Windows Japanese.  A further extension of Shift_JIS                *)
(*         to include NEC special characters (Row 13), NEC                    *)
(*         selection of IBM extensions (Rows 89 to 92), and IBM               *)
(*         extensions (Rows 115 to 119).  The CCS's are                       *)
(*         JIS X0201:1997, JIS X0208:1997, and these extensions.              *)
(*         This charset can be used for the top-level media type "text",      *)
(*         but it is of limited or specialized use (see RFC2278).             *)
(*         PCL Symbol Set id: 19K                                             *)
(* Alias: csWindows31J                                                        *)

  if UName='WINDOWS-31J'             then result :='Windows-31J'           else
  if UName='CSWINDOWS31J'            then result :='Windows-31J'           else

(* Name: GB2312  (preferred MIME name)                                        *)
(* MIBenum: 2025                                                              *)
(* Source: Chinese for People's Republic of China (PRC) mixed one byte,       *)
(*         two byte set:                                                      *)
(*           20-7E = one byte ASCII                                           *)
(*           A1-FE = two byte PRC Kanji                                       *)
(*         See GB 2312-80                                                     *)
(*         PCL Symbol Set Id: 18C                                             *)
(* Alias: csGB2312                                                            *)

  if UName='GB2312'                  then result :='GB2312'                else
  if UName='CSGB2312'                then result :='GB2312'                else

(* Name: Big5  (preferred MIME name)                                          *)
(* MIBenum: 2026                                                              *)
(* Source: Chinese for Taiwan Multi-byte set.                                 *)
(*         PCL Symbol Set Id: 18T                                             *)
(* Alias: csBig5                                                              *)

  if UName='BIG5'                    then result :='Big5'                  else
  if UName='CSBIG5'                  then result :='Big5'                  else

(* Name: windows-1250                                                         *)
(* MIBenum: 2250                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1250) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1250'            then result :='windows-1250'          else

(* Name: windows-1251                                                         *)
(* MIBenum: 2251                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1251) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1251'            then result :='windows-1251'          else

(* Name: windows-1252                                                         *)
(* MIBenum: 2252                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1252)       [Wendt] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1252'            then result :='windows-1252'          else

(* Name: windows-1253                                                         *)
(* MIBenum: 2253                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1253) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1253'            then result :='windows-1253'          else

(* Name: windows-1254                                                         *)
(* MIBenum: 2254                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1254) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1254'            then result :='windows-1254'          else

(* Name: windows-1255                                                         *)
(* MIBenum: 2255                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1255) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1255'            then result :='windows-1255'          else

(* Name: windows-1256                                                         *)
(* MIBenum: 2256                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1256) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1256'            then result :='windows-1256'          else

(* Name: windows-1257                                                         *)
(* MIBenum: 2257                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1257) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1257'            then result :='windows-1257'          else

(* Name: windows-1258                                                         *)
(* MIBenum: 2258                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1258) [Lazhintseva] *)
(* Alias: None                                                                *)

  if UName='WINDOWS-1258'            then result :='windows-1258'          else

(* Name: TIS-620                                                              *)
(* MIBenum: 2259                                                              *)
(* Source: Thai Industrial Standards Institute (TISI)           [Tantsetthi]  *)

  if UName='TIS-620'                 then result :='TIS-620'               else

(* Name: HZ-GB-2312                                                           *)
(* MIBenum: 2085                                                              *)
(* Source: RFC 1842, RFC 1843                              [RFC1842, RFC1843] *)


  result:=name;
end;

(* REFERENCES                                                                 *)
(* ----------                                                                 *)

(* [RFC1345]  Simonsen, K., "Character Mnemonics & Character Sets",           *)
(*            RFC 1345, Rationel Almen Planlaegning, Rationel Almen           *)
(*            Planlaegning, June 1992.                                        *)

(* [RFC1428]  Vaudreuil, G., "Transition of Internet Mail from                *)
(*            Just-Send-8 to 8bit-SMTP/MIME", RFC1428, CNRI, February         *)
(*            1993.                                                           *)

(* [RFC1456]  Vietnamese Standardization Working Group, "Conventions for      *)
(*            Encoding the Vietnamese Language VISCII: VIetnamese             *)
(*            Standard Code for Information Interchange VIQR: VIetnamese      *)
(*            Quoted-Readable Specification Revision 1.1", RFC 1456, May      *)
(*            1993.                                                           *)

(* [RFC1468]  Murai, J., Crispin, M., and E. van der Poel, "Japanese          *)
(*            Character Encoding for Internet Messages", RFC 1468,            *)
(*            Keio University, Panda Programming, June 1993.                  *)

(* [RFC1489]  Chernov, A., "Registration of a Cyrillic Character Set",        *)
(*            RFC1489, RELCOM Development Team, July 1993.                    *)

(* [RFC1554]  Ohta, M., and K. Handa, "ISO-2022-JP-2: Multilingual            *)
(*            Extension of ISO-2022-JP", RFC1554, Tokyo Institute of          *)
(*            Technology, ETL, December 1993.                                 *)

(* [RFC1556]  Nussbacher, H., "Handling of Bi-directional Texts in MIME",     *)
(*            RFC1556, Israeli Inter-University, December 1993.               *)

(* [RFC1557]  Choi, U., Chon, K., and H. Park, "Korean Character Encoding     *)
(*            for Internet Messages", KAIST, Solvit Chosun Media,             *)
(*            December 1993.                                                  *)

(* [RFC1641]  Goldsmith, D., and M. Davis, "Using Unicode with MIME",         *)
(*            RFC1641, Taligent, Inc., July 1994.                             *)

(* [RFC1642]  Goldsmith, D., and M. Davis, "UTF-7", RFC1642, Taligent,        *)
(*            Inc., July 1994.                                                *)

(* [RFC1815]  Ohta, M., "Character Sets ISO-10646 and ISO-10646-J-1",         *)
(*            RFC 1815, Tokyo Institute of Technology, July 1995.             *)


(* [Adobe]    Adobe Systems Incorporated, PostScript Language Reference       *)
(*            Manual, second edition, Addison-Wesley Publishing Company,      *)
(*            Inc., 1990.                                                     *)

(* [ECMA Registry]  ISO-IR: International Register of Escape Sequences        *)
(*            http://www.itscj.ipsj.or.jp/ISO-IE/  Note: The current          *)
(*            registration authority is IPSJ/ITSCJ, Japan.                    *)

(* [HP-PCL5]  Hewlett-Packard Company, "HP PCL 5 Comparison Guide",           *)
(*            (P/N 5021-0329) pp B-13, 1996.                                  *)

(* [IBM-CIDT] IBM Corporation, "ABOUT TYPE: IBM's Technical Reference         *)
(*            for Core Interchange Digitized Type", Publication number        *)
(*            S544-3708-01                                                    *)

(* [RFC1842]  Wei, Y., J. Li, and Y. Jiang, "ASCII Printable                  *)
(*            Characters-Based Chinese Character Encoding for Internet        *)
(*            Messages", RFC 1842, Harvard University, Rice University,       *)
(*            University of Maryland, August 1995.                            *)

(* [RFC1843]  Lee, F., "HZ - A Data Format for Exchanging Files of            *)
(*            Arbitrarily Mixed Chinese and ASCII Characters", RFC 1843,      *)
(*            Stanford University, August 1995.                               *)

(* [RFC2152]  Goldsmith, D., M. Davis, "UTF-7: A Mail-Safe Transformation     *)
(*            Format of Unicode", RFC 2152, Apple Computer, Inc.,             *)
(*            Taligent Inc., May 1997.                                        *)

(* [RFC2279]  Yergeau, F., "UTF-8, A Transformation Format of ISO 10646",     *)
(*            RFC 2279, Alis Technologies, January, 1998.                     *)

(* [RFC2781]  Hoffman, P., Yergeau, F., "UTF-16, an encoding of ISO 10646",   *)
(*            RFC 2781, February 2000.                                        *)


(* PEOPLE                                                                     *)
(* ------                                                                     *)

(* [KXS2] Keld Simonsen <Keld.Simonsen@dkuug.dk>                              *)

(* [Choi] Woohyong Choi <whchoi@cosmos.kaist.ac.kr>                           *)

(* [Davis] Mark Davis, <mark@unicode.org>, April 2002.                        *)

(* [Lazhintseva] Katya Lazhintseva, <katyal@MICROSOFT.com>, May 1996.         *)

(* [Mahdi] Tamer Mahdi, <tamer@ca.ibm.com>, August 2000.                      *)

(* [Murai] Jun Murai <jun@wide.ad.jp>                                         *)

(* [Nussbacher] Hank Nussbacher, <hank@vm.tau.ac.il>                          *)

(* [Ohta] Masataka Ohta, <mohta@cc.titech.ac.jp>, July 1995.                  *)

(* [Phipps] Toby Phipps, <tphipps@peoplesoft.com>, March 2002.                *)

(* [Pond] Rick Pond, <rickpond@vnet.ibm.com>, March 1997.                     *)

(* [Robrigado] Reuel Robrigado, <reuelr@ca.ibm.com>, September 2002.          *)

(* [Scherer] Markus Scherer, <markus.scherer@jtcsv.com>, August 2000,         *)
(*           September 2002.                                                  *)

(* [Simonsen] Keld Simonsen, <Keld.Simonsen@rap.dk>, August 2000.             *)

(* [Tantsetthi] Trin Tantsetthi, <trin@mozart.inet.co.th>, September 1998.    *)

(* [Tumasonis] Vladas Tumasonis, <vladas.tumasonis@maf.vu.lt>, August 2000.   *)

(* [Uskov] Alexander Uskov, <auskov@idc.kz>, September 2002.                  *)

(* [Wendt] Chris Wendt, <christw@microsoft.com>, December 1999.               *)

(* [Yick] Nicky Yick, <cliac@itsd.gcn.gov.hk>, October 2000.                  *)

(* []                                                                         *)






