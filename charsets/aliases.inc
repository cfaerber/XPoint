(* $Id: aliases.pl,v 1.3 2001/09/10 18:48:18 cl Exp )
(* generated from IANA charset list -- do not edit *)

function MimeCharsetCanonicalName(Name: String): String;
begin
  Name:=UpperCase(Name);

(* ===================================================================        *)
(* CHARACTER SETS                                                             *)

(* (last updated 2001 August 23)                                              *)

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
(* (set of) sub-repetoires that may occur.  The third region (>1999) is       *)
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
(* standard, but is registered with ISO (ECMA is the current ISO              *)
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

  if name='ANSI_X3.4-1968'          then result :='US-ASCII'              else
  if name='ISO-IR-6'                then result :='US-ASCII'              else
  if name='ANSI_X3.4-1986'          then result :='US-ASCII'              else
  if name='ISO_646.IRV:1991'        then result :='US-ASCII'              else
  if name='ASCII'                   then result :='US-ASCII'              else
  if name='ISO646-US'               then result :='US-ASCII'              else
  if name='US-ASCII'                then result :='US-ASCII'              else
  if name='US'                      then result :='US-ASCII'              else
  if name='IBM367'                  then result :='US-ASCII'              else
  if name='CP367'                   then result :='US-ASCII'              else
  if name='CSASCII'                 then result :='US-ASCII'              else

(* Name: ISO-10646-UTF-1                                                      *)
(* MIBenum: 27                                                                *)
(* Source: Universal Transfer Format (1), this is the multibyte               *)
(*         encoding, that subsets ASCII-7. It does not have byte              *)
(*         ordering issues.                                                   *)
(* Alias: csISO10646UTF1                                                      *)

  if name='ISO-10646-UTF-1'         then result :='ISO-10646-UTF-1'       else
  if name='CSISO10646UTF1'          then result :='ISO-10646-UTF-1'       else

(* Name: ISO_646.basic:1983                                [RFC1345,KXS2]     *)
(* MIBenum: 28                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ref                                                                 *)
(* Alias: csISO646basic1983                                                   *)

  if name='ISO_646.BASIC:1983'      then result :='ISO_646.basic:1983'    else
  if name='REF'                     then result :='ISO_646.basic:1983'    else
  if name='CSISO646BASIC1983'       then result :='ISO_646.basic:1983'    else

(* Name: INVARIANT                                         [RFC1345,KXS2]     *)
(* MIBenum: 29                                                                *)
(* Alias: csINVARIANT                                                         *)

  if name='INVARIANT'               then result :='INVARIANT'             else
  if name='CSINVARIANT'             then result :='INVARIANT'             else

(* Name: ISO_646.irv:1983                                  [RFC1345,KXS2]     *)
(* MIBenum: 30                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-2                                                            *)
(* Alias: irv                                                                 *)
(* Alias: csISO2IntlRefVersion                                                *)

  if name='ISO_646.IRV:1983'        then result :='ISO_646.irv:1983'      else
  if name='ISO-IR-2'                then result :='ISO_646.irv:1983'      else
  if name='IRV'                     then result :='ISO_646.irv:1983'      else
  if name='CSISO2INTLREFVERSION'    then result :='ISO_646.irv:1983'      else

(* Name: BS_4730                                           [RFC1345,KXS2]     *)
(* MIBenum: 20                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-4                                                            *)
(* Alias: ISO646-GB                                                           *)
(* Alias: gb                                                                  *)
(* Alias: uk                                                                  *)
(* Alias: csISO4UnitedKingdom                                                 *)

  if name='BS_4730'                 then result :='BS_4730'               else
  if name='ISO-IR-4'                then result :='BS_4730'               else
  if name='ISO646-GB'               then result :='BS_4730'               else
  if name='GB'                      then result :='BS_4730'               else
  if name='UK'                      then result :='BS_4730'               else
  if name='CSISO4UNITEDKINGDOM'     then result :='BS_4730'               else

(* Name: NATS-SEFI                                         [RFC1345,KXS2]     *)
(* MIBenum: 31                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-1                                                          *)
(* Alias: csNATSSEFI                                                          *)

  if name='NATS-SEFI'               then result :='NATS-SEFI'             else
  if name='ISO-IR-8-1'              then result :='NATS-SEFI'             else
  if name='CSNATSSEFI'              then result :='NATS-SEFI'             else

(* Name: NATS-SEFI-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 32                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-2                                                          *)
(* Alias: csNATSSEFIADD                                                       *)

  if name='NATS-SEFI-ADD'           then result :='NATS-SEFI-ADD'         else
  if name='ISO-IR-8-2'              then result :='NATS-SEFI-ADD'         else
  if name='CSNATSSEFIADD'           then result :='NATS-SEFI-ADD'         else

(* Name: NATS-DANO                                         [RFC1345,KXS2]     *)
(* MIBenum: 33                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-1                                                          *)
(* Alias: csNATSDANO                                                          *)

  if name='NATS-DANO'               then result :='NATS-DANO'             else
  if name='ISO-IR-9-1'              then result :='NATS-DANO'             else
  if name='CSNATSDANO'              then result :='NATS-DANO'             else

(* Name: NATS-DANO-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 34                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-2                                                          *)
(* Alias: csNATSDANOADD                                                       *)

  if name='NATS-DANO-ADD'           then result :='NATS-DANO-ADD'         else
  if name='ISO-IR-9-2'              then result :='NATS-DANO-ADD'         else
  if name='CSNATSDANOADD'           then result :='NATS-DANO-ADD'         else

(* Name: SEN_850200_B                                      [RFC1345,KXS2]     *)
(* MIBenum: 35                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-10                                                           *)
(* Alias: FI                                                                  *)
(* Alias: ISO646-FI                                                           *)
(* Alias: ISO646-SE                                                           *)
(* Alias: se                                                                  *)
(* Alias: csISO10Swedish                                                      *)

  if name='SEN_850200_B'            then result :='SEN_850200_B'          else
  if name='ISO-IR-10'               then result :='SEN_850200_B'          else
  if name='FI'                      then result :='SEN_850200_B'          else
  if name='ISO646-FI'               then result :='SEN_850200_B'          else
  if name='ISO646-SE'               then result :='SEN_850200_B'          else
  if name='SE'                      then result :='SEN_850200_B'          else
  if name='CSISO10SWEDISH'          then result :='SEN_850200_B'          else

(* Name: SEN_850200_C                                      [RFC1345,KXS2]     *)
(* MIBenum: 21                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-11                                                           *)
(* Alias: ISO646-SE2                                                          *)
(* Alias: se2                                                                 *)
(* Alias: csISO11SwedishForNames                                              *)

  if name='SEN_850200_C'            then result :='SEN_850200_C'          else
  if name='ISO-IR-11'               then result :='SEN_850200_C'          else
  if name='ISO646-SE2'              then result :='SEN_850200_C'          else
  if name='SE2'                     then result :='SEN_850200_C'          else
  if name='CSISO11SWEDISHFORNAMES'  then result :='SEN_850200_C'          else

(* Name: KS_C_5601-1987                                    [RFC1345,KXS2]     *)
(* MIBenum: 36                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-149                                                          *)
(* Alias: KS_C_5601-1989                                                      *)
(* Alias: KSC_5601                                                            *)
(* Alias: korean                                                              *)
(* Alias: csKSC56011987                                                       *)

  if name='KS_C_5601-1987'          then result :='KS_C_5601-1987'        else
  if name='ISO-IR-149'              then result :='KS_C_5601-1987'        else
  if name='KS_C_5601-1989'          then result :='KS_C_5601-1987'        else
  if name='KSC_5601'                then result :='KS_C_5601-1987'        else
  if name='KOREAN'                  then result :='KS_C_5601-1987'        else
  if name='CSKSC56011987'           then result :='KS_C_5601-1987'        else

(* Name: ISO-2022-KR  (preferred MIME name)                [RFC1557,Choi]     *)
(* MIBenum: 37                                                                *)
(* Source: RFC-1557 (see also KS_C_5601-1987)                                 *)
(* Alias: csISO2022KR                                                         *)

  if name='ISO-2022-KR'             then result :='ISO-2022-KR'           else
  if name='CSISO2022KR'             then result :='ISO-2022-KR'           else

(* Name: EUC-KR  (preferred MIME name)                     [RFC1557,Choi]     *)
(* MIBenum: 38                                                                *)
(* Source: RFC-1557 (see also KS_C_5861-1992)                                 *)
(* Alias: csEUCKR                                                             *)

  if name='EUC-KR'                  then result :='EUC-KR'                else
  if name='CSEUCKR'                 then result :='EUC-KR'                else

(* Name: ISO-2022-JP  (preferred MIME name)               [RFC1468,Murai]     *)
(* MIBenum: 39                                                                *)
(* Source: RFC-1468 (see also RFC-2237)                                       *)
(* Alias: csISO2022JP                                                         *)

  if name='ISO-2022-JP'             then result :='ISO-2022-JP'           else
  if name='CSISO2022JP'             then result :='ISO-2022-JP'           else

(* Name: ISO-2022-JP-2  (preferred MIME name)              [RFC1554,Ohta]     *)
(* MIBenum: 40                                                                *)
(* Source: RFC-1554                                                           *)
(* Alias: csISO2022JP2                                                        *)

  if name='ISO-2022-JP-2'           then result :='ISO-2022-JP-2'         else
  if name='CSISO2022JP2'            then result :='ISO-2022-JP-2'         else

(* Name: ISO-2022-CN                                            [RFC1922]     *)
(* MIBenum: 104                                                               *)
(* Source: RFC-1922                                                           *)

  if name='ISO-2022-CN'             then result :='ISO-2022-CN'           else

(* Name: ISO-2022-CN-EXT                                        [RFC1922]     *)
(* MIBenum: 105                                                               *)
(* Source: RFC-1922                                                           *)

  if name='ISO-2022-CN-EXT'         then result :='ISO-2022-CN-EXT'       else

(* Name: JIS_C6220-1969-jp                                 [RFC1345,KXS2]     *)
(* MIBenum: 41                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: JIS_C6220-1969                                                      *)
(* Alias: iso-ir-13                                                           *)
(* Alias: katakana                                                            *)
(* Alias: x0201-7                                                             *)
(* Alias: csISO13JISC6220jp                                                   *)

  if name='JIS_C6220-1969-JP'       then result :='JIS_C6220-1969-jp'     else
  if name='JIS_C6220-1969'          then result :='JIS_C6220-1969-jp'     else
  if name='ISO-IR-13'               then result :='JIS_C6220-1969-jp'     else
  if name='KATAKANA'                then result :='JIS_C6220-1969-jp'     else
  if name='X0201-7'                 then result :='JIS_C6220-1969-jp'     else
  if name='CSISO13JISC6220JP'       then result :='JIS_C6220-1969-jp'     else

(* Name: JIS_C6220-1969-ro                                 [RFC1345,KXS2]     *)
(* MIBenum: 42                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-14                                                           *)
(* Alias: jp                                                                  *)
(* Alias: ISO646-JP                                                           *)
(* Alias: csISO14JISC6220ro                                                   *)

  if name='JIS_C6220-1969-RO'       then result :='JIS_C6220-1969-ro'     else
  if name='ISO-IR-14'               then result :='JIS_C6220-1969-ro'     else
  if name='JP'                      then result :='JIS_C6220-1969-ro'     else
  if name='ISO646-JP'               then result :='JIS_C6220-1969-ro'     else
  if name='CSISO14JISC6220RO'       then result :='JIS_C6220-1969-ro'     else

(* Name: IT                                                [RFC1345,KXS2]     *)
(* MIBenum: 22                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-15                                                           *)
(* Alias: ISO646-IT                                                           *)
(* Alias: csISO15Italian                                                      *)

  if name='IT'                      then result :='IT'                    else
  if name='ISO-IR-15'               then result :='IT'                    else
  if name='ISO646-IT'               then result :='IT'                    else
  if name='CSISO15ITALIAN'          then result :='IT'                    else

(* Name: PT                                                [RFC1345,KXS2]     *)
(* MIBenum: 43                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-16                                                           *)
(* Alias: ISO646-PT                                                           *)
(* Alias: csISO16Portuguese                                                   *)

  if name='PT'                      then result :='PT'                    else
  if name='ISO-IR-16'               then result :='PT'                    else
  if name='ISO646-PT'               then result :='PT'                    else
  if name='CSISO16PORTUGUESE'       then result :='PT'                    else

(* Name: ES                                                [RFC1345,KXS2]     *)
(* MIBenum: 23                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-17                                                           *)
(* Alias: ISO646-ES                                                           *)
(* Alias: csISO17Spanish                                                      *)

  if name='ES'                      then result :='ES'                    else
  if name='ISO-IR-17'               then result :='ES'                    else
  if name='ISO646-ES'               then result :='ES'                    else
  if name='CSISO17SPANISH'          then result :='ES'                    else

(* Name: greek7-old                                        [RFC1345,KXS2]     *)
(* MIBenum: 44                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-18                                                           *)
(* Alias: csISO18Greek7Old                                                    *)

  if name='GREEK7-OLD'              then result :='greek7-old'            else
  if name='ISO-IR-18'               then result :='greek7-old'            else
  if name='CSISO18GREEK7OLD'        then result :='greek7-old'            else

(* Name: latin-greek                                       [RFC1345,KXS2]     *)
(* MIBenum: 45                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-19                                                           *)
(* Alias: csISO19LatinGreek                                                   *)

  if name='LATIN-GREEK'             then result :='latin-greek'           else
  if name='ISO-IR-19'               then result :='latin-greek'           else
  if name='CSISO19LATINGREEK'       then result :='latin-greek'           else

(* Name: DIN_66003                                         [RFC1345,KXS2]     *)
(* MIBenum: 24                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-21                                                           *)
(* Alias: de                                                                  *)
(* Alias: ISO646-DE                                                           *)
(* Alias: csISO21German                                                       *)

  if name='DIN_66003'               then result :='DIN_66003'             else
  if name='ISO-IR-21'               then result :='DIN_66003'             else
  if name='DE'                      then result :='DIN_66003'             else
  if name='ISO646-DE'               then result :='DIN_66003'             else
  if name='CSISO21GERMAN'           then result :='DIN_66003'             else

(* Name: NF_Z_62-010_(1973)                                [RFC1345,KXS2]     *)
(* MIBenum: 46                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-25                                                           *)
(* Alias: ISO646-FR1                                                          *)
(* Alias: csISO25French                                                       *)

  if name='NF_Z_62-010_(1973)'      then result :='NF_Z_62-010_(1973)'    else
  if name='ISO-IR-25'               then result :='NF_Z_62-010_(1973)'    else
  if name='ISO646-FR1'              then result :='NF_Z_62-010_(1973)'    else
  if name='CSISO25FRENCH'           then result :='NF_Z_62-010_(1973)'    else

(* Name: Latin-greek-1                                     [RFC1345,KXS2]     *)
(* MIBenum: 47                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-27                                                           *)
(* Alias: csISO27LatinGreek1                                                  *)

  if name='LATIN-GREEK-1'           then result :='Latin-greek-1'         else
  if name='ISO-IR-27'               then result :='Latin-greek-1'         else
  if name='CSISO27LATINGREEK1'      then result :='Latin-greek-1'         else

(* Name: ISO_5427                                          [RFC1345,KXS2]     *)
(* MIBenum: 48                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-37                                                           *)
(* Alias: csISO5427Cyrillic                                                   *)

  if name='ISO_5427'                then result :='ISO_5427'              else
  if name='ISO-IR-37'               then result :='ISO_5427'              else
  if name='CSISO5427CYRILLIC'       then result :='ISO_5427'              else

(* Name: JIS_C6226-1978                                    [RFC1345,KXS2]     *)
(* MIBenum: 49                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-42                                                           *)
(* Alias: csISO42JISC62261978                                                 *)

  if name='JIS_C6226-1978'          then result :='JIS_C6226-1978'        else
  if name='ISO-IR-42'               then result :='JIS_C6226-1978'        else
  if name='CSISO42JISC62261978'     then result :='JIS_C6226-1978'        else

(* Name: BS_viewdata                                       [RFC1345,KXS2]     *)
(* MIBenum: 50                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-47                                                           *)
(* Alias: csISO47BSViewdata                                                   *)

  if name='BS_VIEWDATA'             then result :='BS_viewdata'           else
  if name='ISO-IR-47'               then result :='BS_viewdata'           else
  if name='CSISO47BSVIEWDATA'       then result :='BS_viewdata'           else

(* Name: INIS                                              [RFC1345,KXS2]     *)
(* MIBenum: 51                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-49                                                           *)
(* Alias: csISO49INIS                                                         *)

  if name='INIS'                    then result :='INIS'                  else
  if name='ISO-IR-49'               then result :='INIS'                  else
  if name='CSISO49INIS'             then result :='INIS'                  else

(* Name: INIS-8                                            [RFC1345,KXS2]     *)
(* MIBenum: 52                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-50                                                           *)
(* Alias: csISO50INIS8                                                        *)

  if name='INIS-8'                  then result :='INIS-8'                else
  if name='ISO-IR-50'               then result :='INIS-8'                else
  if name='CSISO50INIS8'            then result :='INIS-8'                else

(* Name: INIS-cyrillic                                     [RFC1345,KXS2]     *)
(* MIBenum: 53                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-51                                                           *)
(* Alias: csISO51INISCyrillic                                                 *)

  if name='INIS-CYRILLIC'           then result :='INIS-cyrillic'         else
  if name='ISO-IR-51'               then result :='INIS-cyrillic'         else
  if name='CSISO51INISCYRILLIC'     then result :='INIS-cyrillic'         else

(* Name: ISO_5427:1981                                     [RFC1345,KXS2]     *)
(* MIBenum: 54                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-54                                                           *)
(* Alias: ISO5427Cyrillic1981                                                 *)

  if name='ISO_5427:1981'           then result :='ISO_5427:1981'         else
  if name='ISO-IR-54'               then result :='ISO_5427:1981'         else
  if name='ISO5427CYRILLIC1981'     then result :='ISO_5427:1981'         else

(* Name: ISO_5428:1980                                     [RFC1345,KXS2]     *)
(* MIBenum: 55                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-55                                                           *)
(* Alias: csISO5428Greek                                                      *)

  if name='ISO_5428:1980'           then result :='ISO_5428:1980'         else
  if name='ISO-IR-55'               then result :='ISO_5428:1980'         else
  if name='CSISO5428GREEK'          then result :='ISO_5428:1980'         else

(* Name: GB_1988-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 56                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-57                                                           *)
(* Alias: cn                                                                  *)
(* Alias: ISO646-CN                                                           *)
(* Alias: csISO57GB1988                                                       *)

  if name='GB_1988-80'              then result :='GB_1988-80'            else
  if name='ISO-IR-57'               then result :='GB_1988-80'            else
  if name='CN'                      then result :='GB_1988-80'            else
  if name='ISO646-CN'               then result :='GB_1988-80'            else
  if name='CSISO57GB1988'           then result :='GB_1988-80'            else

(* Name: GB_2312-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 57                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-58                                                           *)
(* Alias: chinese                                                             *)
(* Alias: csISO58GB231280                                                     *)

  if name='GB_2312-80'              then result :='GB_2312-80'            else
  if name='ISO-IR-58'               then result :='GB_2312-80'            else
  if name='CHINESE'                 then result :='GB_2312-80'            else
  if name='CSISO58GB231280'         then result :='GB_2312-80'            else

(* Name: NS_4551-1                                         [RFC1345,KXS2]     *)
(* MIBenum: 25                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-60                                                           *)
(* Alias: ISO646-NO                                                           *)
(* Alias: no                                                                  *)
(* Alias: csISO60DanishNorwegian                                              *)
(* Alias: csISO60Norwegian1                                                   *)

  if name='NS_4551-1'               then result :='NS_4551-1'             else
  if name='ISO-IR-60'               then result :='NS_4551-1'             else
  if name='ISO646-NO'               then result :='NS_4551-1'             else
  if name='NO'                      then result :='NS_4551-1'             else
  if name='CSISO60DANISHNORWEGIAN'  then result :='NS_4551-1'             else
  if name='CSISO60NORWEGIAN1'       then result :='NS_4551-1'             else

(* Name: NS_4551-2                                          [RFC1345,KXS2]    *)
(* MIBenum: 58                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO646-NO2                                                          *)
(* Alias: iso-ir-61                                                           *)
(* Alias: no2                                                                 *)
(* Alias: csISO61Norwegian2                                                   *)

  if name='NS_4551-2'               then result :='NS_4551-2'             else
  if name='ISO646-NO2'              then result :='NS_4551-2'             else
  if name='ISO-IR-61'               then result :='NS_4551-2'             else
  if name='NO2'                     then result :='NS_4551-2'             else
  if name='CSISO61NORWEGIAN2'       then result :='NS_4551-2'             else

(* Name: NF_Z_62-010                                        [RFC1345,KXS2]    *)
(* MIBenum: 26                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-69                                                           *)
(* Alias: ISO646-FR                                                           *)
(* Alias: fr                                                                  *)
(* Alias: csISO69French                                                       *)

  if name='NF_Z_62-010'             then result :='NF_Z_62-010'           else
  if name='ISO-IR-69'               then result :='NF_Z_62-010'           else
  if name='ISO646-FR'               then result :='NF_Z_62-010'           else
  if name='FR'                      then result :='NF_Z_62-010'           else
  if name='CSISO69FRENCH'           then result :='NF_Z_62-010'           else

(* Name: videotex-suppl                                     [RFC1345,KXS2]    *)
(* MIBenum: 59                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-70                                                           *)
(* Alias: csISO70VideotexSupp1                                                *)

  if name='VIDEOTEX-SUPPL'          then result :='videotex-suppl'        else
  if name='ISO-IR-70'               then result :='videotex-suppl'        else
  if name='CSISO70VIDEOTEXSUPP1'    then result :='videotex-suppl'        else

(* Name: PT2                                                [RFC1345,KXS2]    *)
(* MIBenum: 60                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-84                                                           *)
(* Alias: ISO646-PT2                                                          *)
(* Alias: csISO84Portuguese2                                                  *)

  if name='PT2'                     then result :='PT2'                   else
  if name='ISO-IR-84'               then result :='PT2'                   else
  if name='ISO646-PT2'              then result :='PT2'                   else
  if name='CSISO84PORTUGUESE2'      then result :='PT2'                   else

(* Name: ES2                                                [RFC1345,KXS2]    *)
(* MIBenum: 61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-85                                                           *)
(* Alias: ISO646-ES2                                                          *)
(* Alias: csISO85Spanish2                                                     *)

  if name='ES2'                     then result :='ES2'                   else
  if name='ISO-IR-85'               then result :='ES2'                   else
  if name='ISO646-ES2'              then result :='ES2'                   else
  if name='CSISO85SPANISH2'         then result :='ES2'                   else

(* Name: MSZ_7795.3                                         [RFC1345,KXS2]    *)
(* MIBenum: 62                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-86                                                           *)
(* Alias: ISO646-HU                                                           *)
(* Alias: hu                                                                  *)
(* Alias: csISO86Hungarian                                                    *)

  if name='MSZ_7795.3'              then result :='MSZ_7795.3'            else
  if name='ISO-IR-86'               then result :='MSZ_7795.3'            else
  if name='ISO646-HU'               then result :='MSZ_7795.3'            else
  if name='HU'                      then result :='MSZ_7795.3'            else
  if name='CSISO86HUNGARIAN'        then result :='MSZ_7795.3'            else

(* Name: JIS_C6226-1983                                     [RFC1345,KXS2]    *)
(* MIBenum: 63                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-87                                                           *)
(* Alias: x0208                                                               *)
(* Alias: JIS_X0208-1983                                                      *)
(* Alias: csISO87JISX0208                                                     *)

  if name='JIS_C6226-1983'          then result :='JIS_C6226-1983'        else
  if name='ISO-IR-87'               then result :='JIS_C6226-1983'        else
  if name='X0208'                   then result :='JIS_C6226-1983'        else
  if name='JIS_X0208-1983'          then result :='JIS_C6226-1983'        else
  if name='CSISO87JISX0208'         then result :='JIS_C6226-1983'        else

(* Name: greek7                                             [RFC1345,KXS2]    *)
(* MIBenum: 64                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-88                                                           *)
(* Alias: csISO88Greek7                                                       *)

  if name='GREEK7'                  then result :='greek7'                else
  if name='ISO-IR-88'               then result :='greek7'                else
  if name='CSISO88GREEK7'           then result :='greek7'                else

(* Name: ASMO_449                                           [RFC1345,KXS2]    *)
(* MIBenum: 65                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO_9036                                                            *)
(* Alias: arabic7                                                             *)
(* Alias: iso-ir-89                                                           *)
(* Alias: csISO89ASMO449                                                      *)

  if name='ASMO_449'                then result :='ASMO_449'              else
  if name='ISO_9036'                then result :='ASMO_449'              else
  if name='ARABIC7'                 then result :='ASMO_449'              else
  if name='ISO-IR-89'               then result :='ASMO_449'              else
  if name='CSISO89ASMO449'          then result :='ASMO_449'              else

(* Name: iso-ir-90                                          [RFC1345,KXS2]    *)
(* MIBenum: 66                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: csISO90                                                             *)

  if name='ISO-IR-90'               then result :='iso-ir-90'             else
  if name='CSISO90'                 then result :='iso-ir-90'             else

(* Name: JIS_C6229-1984-a                                   [RFC1345,KXS2]    *)
(* MIBenum: 67                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-91                                                           *)
(* Alias: jp-ocr-a                                                            *)
(* Alias: csISO91JISC62291984a                                                *)

  if name='JIS_C6229-1984-A'        then result :='JIS_C6229-1984-a'      else
  if name='ISO-IR-91'               then result :='JIS_C6229-1984-a'      else
  if name='JP-OCR-A'                then result :='JIS_C6229-1984-a'      else
  if name='CSISO91JISC62291984A'    then result :='JIS_C6229-1984-a'      else

(* Name: JIS_C6229-1984-b                                   [RFC1345,KXS2]    *)
(* MIBenum: 68                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-92                                                           *)
(* Alias: ISO646-JP-OCR-B                                                     *)
(* Alias: jp-ocr-b                                                            *)
(* Alias: csISO92JISC62991984b                                                *)

  if name='JIS_C6229-1984-B'        then result :='JIS_C6229-1984-b'      else
  if name='ISO-IR-92'               then result :='JIS_C6229-1984-b'      else
  if name='ISO646-JP-OCR-B'         then result :='JIS_C6229-1984-b'      else
  if name='JP-OCR-B'                then result :='JIS_C6229-1984-b'      else
  if name='CSISO92JISC62991984B'    then result :='JIS_C6229-1984-b'      else

(* Name: JIS_C6229-1984-b-add                               [RFC1345,KXS2]    *)
(* MIBenum: 69                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-93                                                           *)
(* Alias: jp-ocr-b-add                                                        *)
(* Alias: csISO93JIS62291984badd                                              *)

  if name='JIS_C6229-1984-B-ADD'    then result :='JIS_C6229-1984-b-add'  else
  if name='ISO-IR-93'               then result :='JIS_C6229-1984-b-add'  else
  if name='JP-OCR-B-ADD'            then result :='JIS_C6229-1984-b-add'  else
  if name='CSISO93JIS62291984BADD'  then result :='JIS_C6229-1984-b-add'  else

(* Name: JIS_C6229-1984-hand                                [RFC1345,KXS2]    *)
(* MIBenum: 70                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-94                                                           *)
(* Alias: jp-ocr-hand                                                         *)
(* Alias: csISO94JIS62291984hand                                              *)

  if name='JIS_C6229-1984-HAND'     then result :='JIS_C6229-1984-hand'   else
  if name='ISO-IR-94'               then result :='JIS_C6229-1984-hand'   else
  if name='JP-OCR-HAND'             then result :='JIS_C6229-1984-hand'   else
  if name='CSISO94JIS62291984HAND'  then result :='JIS_C6229-1984-hand'   else

(* Name: JIS_C6229-1984-hand-add                            [RFC1345,KXS2]    *)
(* MIBenum: 71                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-95                                                           *)
(* Alias: jp-ocr-hand-add                                                     *)
(* Alias: csISO95JIS62291984handadd                                           *)

  if name='JIS_C6229-1984-HAND-ADD' then result :='JIS_C6229-1984-hand-add' else
  if name='ISO-IR-95'               then result :='JIS_C6229-1984-hand-add' else
  if name='JP-OCR-HAND-ADD'         then result :='JIS_C6229-1984-hand-add' else
  if name='CSISO95JIS62291984HANDADD' then result :='JIS_C6229-1984-hand-add' else

(* Name: JIS_C6229-1984-kana                                [RFC1345,KXS2]    *)
(* MIBenum: 72                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-96                                                           *)
(* Alias: csISO96JISC62291984kana                                             *)

  if name='JIS_C6229-1984-KANA'     then result :='JIS_C6229-1984-kana'   else
  if name='ISO-IR-96'               then result :='JIS_C6229-1984-kana'   else
  if name='CSISO96JISC62291984KANA' then result :='JIS_C6229-1984-kana'   else

(* Name: ISO_2033-1983                                      [RFC1345,KXS2]    *)
(* MIBenum: 73                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-98                                                           *)
(* Alias: e13b                                                                *)
(* Alias: csISO2033                                                           *)

  if name='ISO_2033-1983'           then result :='ISO_2033-1983'         else
  if name='ISO-IR-98'               then result :='ISO_2033-1983'         else
  if name='E13B'                    then result :='ISO_2033-1983'         else
  if name='CSISO2033'               then result :='ISO_2033-1983'         else

(* Name: ANSI_X3.110-1983                                   [RFC1345,KXS2]    *)
(* MIBenum: 74                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-99                                                           *)
(* Alias: CSA_T500-1983                                                       *)
(* Alias: NAPLPS                                                              *)
(* Alias: csISO99NAPLPS                                                       *)

  if name='ANSI_X3.110-1983'        then result :='ANSI_X3.110-1983'      else
  if name='ISO-IR-99'               then result :='ANSI_X3.110-1983'      else
  if name='CSA_T500-1983'           then result :='ANSI_X3.110-1983'      else
  if name='NAPLPS'                  then result :='ANSI_X3.110-1983'      else
  if name='CSISO99NAPLPS'           then result :='ANSI_X3.110-1983'      else

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

  if name='ISO_8859-1:1987'         then result :='ISO-8859-1'            else
  if name='ISO-IR-100'              then result :='ISO-8859-1'            else
  if name='ISO_8859-1'              then result :='ISO-8859-1'            else
  if name='ISO-8859-1'              then result :='ISO-8859-1'            else
  if name='LATIN1'                  then result :='ISO-8859-1'            else
  if name='L1'                      then result :='ISO-8859-1'            else
  if name='IBM819'                  then result :='ISO-8859-1'            else
  if name='CP819'                   then result :='ISO-8859-1'            else
  if name='CSISOLATIN1'             then result :='ISO-8859-1'            else

(* Name: ISO_8859-2:1987                                    [RFC1345,KXS2]    *)
(* MIBenum: 5                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-101                                                          *)
(* Alias: ISO_8859-2                                                          *)
(* Alias: ISO-8859-2 (preferred MIME name)                                    *)
(* Alias: latin2                                                              *)
(* Alias: l2                                                                  *)
(* Alias: csISOLatin2                                                         *)

  if name='ISO_8859-2:1987'         then result :='ISO-8859-2'            else
  if name='ISO-IR-101'              then result :='ISO-8859-2'            else
  if name='ISO_8859-2'              then result :='ISO-8859-2'            else
  if name='ISO-8859-2'              then result :='ISO-8859-2'            else
  if name='LATIN2'                  then result :='ISO-8859-2'            else
  if name='L2'                      then result :='ISO-8859-2'            else
  if name='CSISOLATIN2'             then result :='ISO-8859-2'            else

(* Name: T.61-7bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 75                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-102                                                          *)
(* Alias: csISO102T617bit                                                     *)

  if name='T.61-7BIT'               then result :='T.61-7bit'             else
  if name='ISO-IR-102'              then result :='T.61-7bit'             else
  if name='CSISO102T617BIT'         then result :='T.61-7bit'             else

(* Name: T.61-8bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 76                                                                *)
(* Alias: T.61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-103                                                          *)
(* Alias: csISO103T618bit                                                     *)

  if name='T.61-8BIT'               then result :='T.61-8bit'             else
  if name='T.61'                    then result :='T.61-8bit'             else
  if name='ISO-IR-103'              then result :='T.61-8bit'             else
  if name='CSISO103T618BIT'         then result :='T.61-8bit'             else

(* Name: ISO_8859-3:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 6                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-109                                                          *)
(* Alias: ISO_8859-3                                                          *)
(* Alias: ISO-8859-3 (preferred MIME name)                                    *)
(* Alias: latin3                                                              *)
(* Alias: l3                                                                  *)
(* Alias: csISOLatin3                                                         *)

  if name='ISO_8859-3:1988'         then result :='ISO-8859-3'            else
  if name='ISO-IR-109'              then result :='ISO-8859-3'            else
  if name='ISO_8859-3'              then result :='ISO-8859-3'            else
  if name='ISO-8859-3'              then result :='ISO-8859-3'            else
  if name='LATIN3'                  then result :='ISO-8859-3'            else
  if name='L3'                      then result :='ISO-8859-3'            else
  if name='CSISOLATIN3'             then result :='ISO-8859-3'            else

(* Name: ISO_8859-4:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 7                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-110                                                          *)
(* Alias: ISO_8859-4                                                          *)
(* Alias: ISO-8859-4 (preferred MIME name)                                    *)
(* Alias: latin4                                                              *)
(* Alias: l4                                                                  *)
(* Alias: csISOLatin4                                                         *)

  if name='ISO_8859-4:1988'         then result :='ISO-8859-4'            else
  if name='ISO-IR-110'              then result :='ISO-8859-4'            else
  if name='ISO_8859-4'              then result :='ISO-8859-4'            else
  if name='ISO-8859-4'              then result :='ISO-8859-4'            else
  if name='LATIN4'                  then result :='ISO-8859-4'            else
  if name='L4'                      then result :='ISO-8859-4'            else
  if name='CSISOLATIN4'             then result :='ISO-8859-4'            else

(* Name: ECMA-cyrillic                                      [RFC1345,KXS2]    *)
(* MIBenum: 77                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-111                                                          *)
(* Alias: csISO111ECMACyrillic                                                *)

  if name='ECMA-CYRILLIC'           then result :='ECMA-cyrillic'         else
  if name='ISO-IR-111'              then result :='ECMA-cyrillic'         else
  if name='CSISO111ECMACYRILLIC'    then result :='ECMA-cyrillic'         else

(* Name: CSA_Z243.4-1985-1                                  [RFC1345,KXS2]    *)
(* MIBenum: 78                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-121                                                          *)
(* Alias: ISO646-CA                                                           *)
(* Alias: csa7-1                                                              *)
(* Alias: ca                                                                  *)
(* Alias: csISO121Canadian1                                                   *)

  if name='CSA_Z243.4-1985-1'       then result :='CSA_Z243.4-1985-1'     else
  if name='ISO-IR-121'              then result :='CSA_Z243.4-1985-1'     else
  if name='ISO646-CA'               then result :='CSA_Z243.4-1985-1'     else
  if name='CSA7-1'                  then result :='CSA_Z243.4-1985-1'     else
  if name='CA'                      then result :='CSA_Z243.4-1985-1'     else
  if name='CSISO121CANADIAN1'       then result :='CSA_Z243.4-1985-1'     else

(* Name: CSA_Z243.4-1985-2                                  [RFC1345,KXS2]    *)
(* MIBenum: 79                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-122                                                          *)
(* Alias: ISO646-CA2                                                          *)
(* Alias: csa7-2                                                              *)
(* Alias: csISO122Canadian2                                                   *)

  if name='CSA_Z243.4-1985-2'       then result :='CSA_Z243.4-1985-2'     else
  if name='ISO-IR-122'              then result :='CSA_Z243.4-1985-2'     else
  if name='ISO646-CA2'              then result :='CSA_Z243.4-1985-2'     else
  if name='CSA7-2'                  then result :='CSA_Z243.4-1985-2'     else
  if name='CSISO122CANADIAN2'       then result :='CSA_Z243.4-1985-2'     else

(* Name: CSA_Z243.4-1985-gr                                 [RFC1345,KXS2]    *)
(* MIBenum: 80                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-123                                                          *)
(* Alias: csISO123CSAZ24341985gr                                              *)

  if name='CSA_Z243.4-1985-GR'      then result :='CSA_Z243.4-1985-gr'    else
  if name='ISO-IR-123'              then result :='CSA_Z243.4-1985-gr'    else
  if name='CSISO123CSAZ24341985GR'  then result :='CSA_Z243.4-1985-gr'    else

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

  if name='ISO_8859-6:1987'         then result :='ISO-8859-6'            else
  if name='ISO-IR-127'              then result :='ISO-8859-6'            else
  if name='ISO_8859-6'              then result :='ISO-8859-6'            else
  if name='ISO-8859-6'              then result :='ISO-8859-6'            else
  if name='ECMA-114'                then result :='ISO-8859-6'            else
  if name='ASMO-708'                then result :='ISO-8859-6'            else
  if name='ARABIC'                  then result :='ISO-8859-6'            else
  if name='CSISOLATINARABIC'        then result :='ISO-8859-6'            else

(* Name: ISO_8859-6-E                                       [RFC1556,IANA]    *)
(* MIBenum: 81                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596E                                                         *)
(* Alias: ISO-8859-6-E (preferred MIME name)                                  *)

  if name='ISO_8859-6-E'            then result :='ISO-8859-6-E'          else
  if name='CSISO88596E'             then result :='ISO-8859-6-E'          else
  if name='ISO-8859-6-E'            then result :='ISO-8859-6-E'          else

(* Name: ISO_8859-6-I                                       [RFC1556,IANA]    *)
(* MIBenum: 82                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596I                                                         *)
(* Alias: ISO-8859-6-I (preferred MIME name)                                  *)

  if name='ISO_8859-6-I'            then result :='ISO-8859-6-I'          else
  if name='CSISO88596I'             then result :='ISO-8859-6-I'          else
  if name='ISO-8859-6-I'            then result :='ISO-8859-6-I'          else

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

  if name='ISO_8859-7:1987'         then result :='ISO-8859-7'            else
  if name='ISO-IR-126'              then result :='ISO-8859-7'            else
  if name='ISO_8859-7'              then result :='ISO-8859-7'            else
  if name='ISO-8859-7'              then result :='ISO-8859-7'            else
  if name='ELOT_928'                then result :='ISO-8859-7'            else
  if name='ECMA-118'                then result :='ISO-8859-7'            else
  if name='GREEK'                   then result :='ISO-8859-7'            else
  if name='GREEK8'                  then result :='ISO-8859-7'            else
  if name='CSISOLATINGREEK'         then result :='ISO-8859-7'            else

(* Name: T.101-G2                                            [RFC1345,KXS2]   *)
(* MIBenum: 83                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-128                                                          *)
(* Alias: csISO128T101G2                                                      *)

  if name='T.101-G2'                then result :='T.101-G2'              else
  if name='ISO-IR-128'              then result :='T.101-G2'              else
  if name='CSISO128T101G2'          then result :='T.101-G2'              else

(* Name: ISO_8859-8:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 11                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-138                                                          *)
(* Alias: ISO_8859-8                                                          *)
(* Alias: ISO-8859-8 (preferred MIME name)                                    *)
(* Alias: hebrew                                                              *)
(* Alias: csISOLatinHebrew                                                    *)

  if name='ISO_8859-8:1988'         then result :='ISO-8859-8'            else
  if name='ISO-IR-138'              then result :='ISO-8859-8'            else
  if name='ISO_8859-8'              then result :='ISO-8859-8'            else
  if name='ISO-8859-8'              then result :='ISO-8859-8'            else
  if name='HEBREW'                  then result :='ISO-8859-8'            else
  if name='CSISOLATINHEBREW'        then result :='ISO-8859-8'            else

(* Name: ISO_8859-8-E                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 84                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598E                                                         *)
(* Alias: ISO-8859-8-E (preferred MIME name)                                  *)

  if name='ISO_8859-8-E'            then result :='ISO-8859-8-E'          else
  if name='CSISO88598E'             then result :='ISO-8859-8-E'          else
  if name='ISO-8859-8-E'            then result :='ISO-8859-8-E'          else

(* Name: ISO_8859-8-I                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 85                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598I                                                         *)
(* Alias: ISO-8859-8-I (preferred MIME name)                                  *)

  if name='ISO_8859-8-I'            then result :='ISO-8859-8-I'          else
  if name='CSISO88598I'             then result :='ISO-8859-8-I'          else
  if name='ISO-8859-8-I'            then result :='ISO-8859-8-I'          else

(* Name: CSN_369103                                          [RFC1345,KXS2]   *)
(* MIBenum: 86                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-139                                                          *)
(* Alias: csISO139CSN369103                                                   *)

  if name='CSN_369103'              then result :='CSN_369103'            else
  if name='ISO-IR-139'              then result :='CSN_369103'            else
  if name='CSISO139CSN369103'       then result :='CSN_369103'            else

(* Name: JUS_I.B1.002                                        [RFC1345,KXS2]   *)
(* MIBenum: 87                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-141                                                          *)
(* Alias: ISO646-YU                                                           *)
(* Alias: js                                                                  *)
(* Alias: yu                                                                  *)
(* Alias: csISO141JUSIB1002                                                   *)

  if name='JUS_I.B1.002'            then result :='JUS_I.B1.002'          else
  if name='ISO-IR-141'              then result :='JUS_I.B1.002'          else
  if name='ISO646-YU'               then result :='JUS_I.B1.002'          else
  if name='JS'                      then result :='JUS_I.B1.002'          else
  if name='YU'                      then result :='JUS_I.B1.002'          else
  if name='CSISO141JUSIB1002'       then result :='JUS_I.B1.002'          else

(* Name: ISO_6937-2-add                                      [RFC1345,KXS2]   *)
(* MIBenum: 14                                                                *)
(* Source: ECMA registry and ISO 6937-2:1983                                  *)
(* Alias: iso-ir-142                                                          *)
(* Alias: csISOTextComm                                                       *)

  if name='ISO_6937-2-ADD'          then result :='ISO_6937-2-add'        else
  if name='ISO-IR-142'              then result :='ISO_6937-2-add'        else
  if name='CSISOTEXTCOMM'           then result :='ISO_6937-2-add'        else

(* Name: IEC_P27-1                                           [RFC1345,KXS2]   *)
(* MIBenum: 88                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-143                                                          *)
(* Alias: csISO143IECP271                                                     *)

  if name='IEC_P27-1'               then result :='IEC_P27-1'             else
  if name='ISO-IR-143'              then result :='IEC_P27-1'             else
  if name='CSISO143IECP271'         then result :='IEC_P27-1'             else

(* Name: ISO_8859-5:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 8                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-144                                                          *)
(* Alias: ISO_8859-5                                                          *)
(* Alias: ISO-8859-5 (preferred MIME name)                                    *)
(* Alias: cyrillic                                                            *)
(* Alias: csISOLatinCyrillic                                                  *)

  if name='ISO_8859-5:1988'         then result :='ISO-8859-5'            else
  if name='ISO-IR-144'              then result :='ISO-8859-5'            else
  if name='ISO_8859-5'              then result :='ISO-8859-5'            else
  if name='ISO-8859-5'              then result :='ISO-8859-5'            else
  if name='CYRILLIC'                then result :='ISO-8859-5'            else
  if name='CSISOLATINCYRILLIC'      then result :='ISO-8859-5'            else

(* Name: JUS_I.B1.003-serb                                   [RFC1345,KXS2]   *)
(* MIBenum: 89                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-146                                                          *)
(* Alias: serbian                                                             *)
(* Alias: csISO146Serbian                                                     *)

  if name='JUS_I.B1.003-SERB'       then result :='JUS_I.B1.003-serb'     else
  if name='ISO-IR-146'              then result :='JUS_I.B1.003-serb'     else
  if name='SERBIAN'                 then result :='JUS_I.B1.003-serb'     else
  if name='CSISO146SERBIAN'         then result :='JUS_I.B1.003-serb'     else

(* Name: JUS_I.B1.003-mac                                    [RFC1345,KXS2]   *)
(* MIBenum: 90                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: macedonian                                                          *)
(* Alias: iso-ir-147                                                          *)
(* Alias: csISO147Macedonian                                                  *)

  if name='JUS_I.B1.003-MAC'        then result :='JUS_I.B1.003-mac'      else
  if name='MACEDONIAN'              then result :='JUS_I.B1.003-mac'      else
  if name='ISO-IR-147'              then result :='JUS_I.B1.003-mac'      else
  if name='CSISO147MACEDONIAN'      then result :='JUS_I.B1.003-mac'      else

(* Name: ISO_8859-9:1989                                     [RFC1345,KXS2]   *)
(* MIBenum: 12                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-148                                                          *)
(* Alias: ISO_8859-9                                                          *)
(* Alias: ISO-8859-9 (preferred MIME name)                                    *)
(* Alias: latin5                                                              *)
(* Alias: l5                                                                  *)
(* Alias: csISOLatin5                                                         *)

  if name='ISO_8859-9:1989'         then result :='ISO-8859-9'            else
  if name='ISO-IR-148'              then result :='ISO-8859-9'            else
  if name='ISO_8859-9'              then result :='ISO-8859-9'            else
  if name='ISO-8859-9'              then result :='ISO-8859-9'            else
  if name='LATIN5'                  then result :='ISO-8859-9'            else
  if name='L5'                      then result :='ISO-8859-9'            else
  if name='CSISOLATIN5'             then result :='ISO-8859-9'            else

(* Name: greek-ccitt                                         [RFC1345,KXS2]   *)
(* MIBenum: 91                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-150                                                          *)
(* Alias: csISO150                                                            *)
(* Alias: csISO150GreekCCITT                                                  *)

  if name='GREEK-CCITT'             then result :='greek-ccitt'           else
  if name='ISO-IR-150'              then result :='greek-ccitt'           else
  if name='CSISO150'                then result :='greek-ccitt'           else
  if name='CSISO150GREEKCCITT'      then result :='greek-ccitt'           else

(* Name: NC_NC00-10:81                                       [RFC1345,KXS2]   *)
(* MIBenum: 92                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: cuba                                                                *)
(* Alias: iso-ir-151                                                          *)
(* Alias: ISO646-CU                                                           *)
(* Alias: csISO151Cuba                                                        *)

  if name='NC_NC00-10:81'           then result :='NC_NC00-10:81'         else
  if name='CUBA'                    then result :='NC_NC00-10:81'         else
  if name='ISO-IR-151'              then result :='NC_NC00-10:81'         else
  if name='ISO646-CU'               then result :='NC_NC00-10:81'         else
  if name='CSISO151CUBA'            then result :='NC_NC00-10:81'         else

(* Name: ISO_6937-2-25                                       [RFC1345,KXS2]   *)
(* MIBenum: 93                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-152                                                          *)
(* Alias: csISO6937Add                                                        *)

  if name='ISO_6937-2-25'           then result :='ISO_6937-2-25'         else
  if name='ISO-IR-152'              then result :='ISO_6937-2-25'         else
  if name='CSISO6937ADD'            then result :='ISO_6937-2-25'         else

(* Name: GOST_19768-74                                       [RFC1345,KXS2]   *)
(* MIBenum: 94                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ST_SEV_358-88                                                       *)
(* Alias: iso-ir-153                                                          *)
(* Alias: csISO153GOST1976874                                                 *)

  if name='GOST_19768-74'           then result :='GOST_19768-74'         else
  if name='ST_SEV_358-88'           then result :='GOST_19768-74'         else
  if name='ISO-IR-153'              then result :='GOST_19768-74'         else
  if name='CSISO153GOST1976874'     then result :='GOST_19768-74'         else

(* Name: ISO_8859-supp                                       [RFC1345,KXS2]   *)
(* MIBenum: 95                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-154                                                          *)
(* Alias: latin1-2-5                                                          *)
(* Alias: csISO8859Supp                                                       *)

  if name='ISO_8859-SUPP'           then result :='ISO_8859-supp'         else
  if name='ISO-IR-154'              then result :='ISO_8859-supp'         else
  if name='LATIN1-2-5'              then result :='ISO_8859-supp'         else
  if name='CSISO8859SUPP'           then result :='ISO_8859-supp'         else

(* Name: ISO_10367-box                                       [RFC1345,KXS2]   *)
(* MIBenum: 96                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-155                                                          *)
(* Alias: csISO10367Box                                                       *)

  if name='ISO_10367-BOX'           then result :='ISO_10367-box'         else
  if name='ISO-IR-155'              then result :='ISO_10367-box'         else
  if name='CSISO10367BOX'           then result :='ISO_10367-box'         else

(* Name: ISO-8859-10 (preferred MIME name)                   [RFC1345,KXS2]   *)
(* MIBenum: 13                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-157                                                          *)
(* Alias: l6                                                                  *)
(* Alias: ISO_8859-10:1992                                                    *)
(* Alias: csISOLatin6                                                         *)
(* Alias: latin6                                                              *)

  if name='ISO-8859-10'             then result :='ISO-8859-10'           else
  if name='ISO-IR-157'              then result :='ISO-8859-10'           else
  if name='L6'                      then result :='ISO-8859-10'           else
  if name='ISO_8859-10:1992'        then result :='ISO-8859-10'           else
  if name='CSISOLATIN6'             then result :='ISO-8859-10'           else
  if name='LATIN6'                  then result :='ISO-8859-10'           else

(* Name: latin-lap                                           [RFC1345,KXS2]   *)
(* MIBenum: 97                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: lap                                                                 *)
(* Alias: iso-ir-158                                                          *)
(* Alias: csISO158Lap                                                         *)

  if name='LATIN-LAP'               then result :='latin-lap'             else
  if name='LAP'                     then result :='latin-lap'             else
  if name='ISO-IR-158'              then result :='latin-lap'             else
  if name='CSISO158LAP'             then result :='latin-lap'             else

(* Name: JIS_X0212-1990                                      [RFC1345,KXS2]   *)
(* MIBenum: 98                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: x0212                                                               *)
(* Alias: iso-ir-159                                                          *)
(* Alias: csISO159JISX02121990                                                *)

  if name='JIS_X0212-1990'          then result :='JIS_X0212-1990'        else
  if name='X0212'                   then result :='JIS_X0212-1990'        else
  if name='ISO-IR-159'              then result :='JIS_X0212-1990'        else
  if name='CSISO159JISX02121990'    then result :='JIS_X0212-1990'        else

(* Name: DS_2089                                             [RFC1345,KXS2]   *)
(* MIBenum: 99                                                                *)
(* Source: Danish Standard, DS 2089, February 1974                            *)
(* Alias: DS2089                                                              *)
(* Alias: ISO646-DK                                                           *)
(* Alias: dk                                                                  *)
(* Alias: csISO646Danish                                                      *)

  if name='DS_2089'                 then result :='DS_2089'               else
  if name='DS2089'                  then result :='DS_2089'               else
  if name='ISO646-DK'               then result :='DS_2089'               else
  if name='DK'                      then result :='DS_2089'               else
  if name='CSISO646DANISH'          then result :='DS_2089'               else

(* Name: us-dk                                               [RFC1345,KXS2]   *)
(* MIBenum: 100                                                               *)
(* Alias: csUSDK                                                              *)

  if name='US-DK'                   then result :='us-dk'                 else
  if name='CSUSDK'                  then result :='us-dk'                 else

(* Name: dk-us                                               [RFC1345,KXS2]   *)
(* MIBenum: 101                                                               *)
(* Alias: csDKUS                                                              *)

  if name='DK-US'                   then result :='dk-us'                 else
  if name='CSDKUS'                  then result :='dk-us'                 else

(* Name: JIS_X0201                                           [RFC1345,KXS2]   *)
(* MIBenum: 15                                                                *)
(* Source: JIS X 0201-1976.   One byte only, this is equivalent to            *)
(*         JIS/Roman (similar to ASCII) plus eight-bit half-width             *)
(*         Katakana                                                           *)
(* Alias: X0201                                                               *)
(* Alias: csHalfWidthKatakana                                                 *)

  if name='JIS_X0201'               then result :='JIS_X0201'             else
  if name='X0201'                   then result :='JIS_X0201'             else
  if name='CSHALFWIDTHKATAKANA'     then result :='JIS_X0201'             else

(* Name: KSC5636                                             [RFC1345,KXS2]   *)
(* MIBenum: 102                                                               *)
(* Alias: ISO646-KR                                                           *)
(* Alias: csKSC5636                                                           *)

  if name='KSC5636'                 then result :='KSC5636'               else
  if name='ISO646-KR'               then result :='KSC5636'               else
  if name='CSKSC5636'               then result :='KSC5636'               else

(* Name: ISO-10646-UCS-2                                                      *)
(* MIBenum: 1000                                                              *)
(* Source: the 2-octet Basic Multilingual Plane, aka Unicode                  *)
(*         this needs to specify network byte order: the standard             *)
(*         does not specify (it is a 16-bit integer space)                    *)
(* Alias: csUnicode                                                           *)

  if name='ISO-10646-UCS-2'         then result :='ISO-10646-UCS-2'       else
  if name='CSUNICODE'               then result :='ISO-10646-UCS-2'       else

(* Name: ISO-10646-UCS-4                                                      *)
(* MIBenum: 1001                                                              *)
(* Source: the full code space. (same comment about byte order,               *)
(*         these are 31-bit numbers.                                          *)
(* Alias: csUCS4                                                              *)

  if name='ISO-10646-UCS-4'         then result :='ISO-10646-UCS-4'       else
  if name='CSUCS4'                  then result :='ISO-10646-UCS-4'       else

(* Name: DEC-MCS                                             [RFC1345,KXS2]   *)
(* MIBenum: 2008                                                              *)
(* Source: VAX/VMS User's Manual,                                             *)
(*         Order Number: AI-Y517A-TE, April 1986.                             *)
(* Alias: dec                                                                 *)
(* Alias: csDECMCS                                                            *)

  if name='DEC-MCS'                 then result :='DEC-MCS'               else
  if name='DEC'                     then result :='DEC-MCS'               else
  if name='CSDECMCS'                then result :='DEC-MCS'               else

(* Name: hp-roman8                                  [HP-PCL5,RFC1345,KXS2]    *)
(* MIBenum: 2004                                                              *)
(* Source: LaserJet IIP Printer User's Manual,                                *)
(*         HP part no 33471-90901, Hewlet-Packard, June 1989.                 *)
(* Alias: roman8                                                              *)
(* Alias: r8                                                                  *)
(* Alias: csHPRoman8                                                          *)

  if name='HP-ROMAN8'               then result :='hp-roman8'             else
  if name='ROMAN8'                  then result :='hp-roman8'             else
  if name='R8'                      then result :='hp-roman8'             else
  if name='CSHPROMAN8'              then result :='hp-roman8'             else

(* Name: macintosh                                           [RFC1345,KXS2]   *)
(* MIBenum: 2027                                                              *)
(* Source: The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991          *)
(* Alias: mac                                                                 *)
(* Alias: csMacintosh                                                         *)

  if name='MACINTOSH'               then result :='macintosh'             else
  if name='MAC'                     then result :='macintosh'             else
  if name='CSMACINTOSH'             then result :='macintosh'             else

(* Name: IBM037                                              [RFC1345,KXS2]   *)
(* MIBenum: 2028                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp037                                                               *)
(* Alias: ebcdic-cp-us                                                        *)
(* Alias: ebcdic-cp-ca                                                        *)
(* Alias: ebcdic-cp-wt                                                        *)
(* Alias: ebcdic-cp-nl                                                        *)
(* Alias: csIBM037                                                            *)

  if name='IBM037'                  then result :='IBM037'                else
  if name='CP037'                   then result :='IBM037'                else
  if name='EBCDIC-CP-US'            then result :='IBM037'                else
  if name='EBCDIC-CP-CA'            then result :='IBM037'                else
  if name='EBCDIC-CP-WT'            then result :='IBM037'                else
  if name='EBCDIC-CP-NL'            then result :='IBM037'                else
  if name='CSIBM037'                then result :='IBM037'                else

(* Name: IBM038                                              [RFC1345,KXS2]   *)
(* MIBenum: 2029                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-INT                                                          *)
(* Alias: cp038                                                               *)
(* Alias: csIBM038                                                            *)

  if name='IBM038'                  then result :='IBM038'                else
  if name='EBCDIC-INT'              then result :='IBM038'                else
  if name='CP038'                   then result :='IBM038'                else
  if name='CSIBM038'                then result :='IBM038'                else

(* Name: IBM273                                              [RFC1345,KXS2]   *)
(* MIBenum: 2030                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP273                                                               *)
(* Alias: csIBM273                                                            *)

  if name='IBM273'                  then result :='IBM273'                else
  if name='CP273'                   then result :='IBM273'                else
  if name='CSIBM273'                then result :='IBM273'                else

(* Name: IBM274                                              [RFC1345,KXS2]   *)
(* MIBenum: 2031                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-BE                                                           *)
(* Alias: CP274                                                               *)
(* Alias: csIBM274                                                            *)

  if name='IBM274'                  then result :='IBM274'                else
  if name='EBCDIC-BE'               then result :='IBM274'                else
  if name='CP274'                   then result :='IBM274'                else
  if name='CSIBM274'                then result :='IBM274'                else

(* Name: IBM275                                              [RFC1345,KXS2]   *)
(* MIBenum: 2032                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-BR                                                           *)
(* Alias: cp275                                                               *)
(* Alias: csIBM275                                                            *)

  if name='IBM275'                  then result :='IBM275'                else
  if name='EBCDIC-BR'               then result :='IBM275'                else
  if name='CP275'                   then result :='IBM275'                else
  if name='CSIBM275'                then result :='IBM275'                else

(* Name: IBM277                                              [RFC1345,KXS2]   *)
(* MIBenum: 2033                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-CP-DK                                                        *)
(* Alias: EBCDIC-CP-NO                                                        *)
(* Alias: csIBM277                                                            *)

  if name='IBM277'                  then result :='IBM277'                else
  if name='EBCDIC-CP-DK'            then result :='IBM277'                else
  if name='EBCDIC-CP-NO'            then result :='IBM277'                else
  if name='CSIBM277'                then result :='IBM277'                else

(* Name: IBM278                                              [RFC1345,KXS2]   *)
(* MIBenum: 2034                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP278                                                               *)
(* Alias: ebcdic-cp-fi                                                        *)
(* Alias: ebcdic-cp-se                                                        *)
(* Alias: csIBM278                                                            *)

  if name='IBM278'                  then result :='IBM278'                else
  if name='CP278'                   then result :='IBM278'                else
  if name='EBCDIC-CP-FI'            then result :='IBM278'                else
  if name='EBCDIC-CP-SE'            then result :='IBM278'                else
  if name='CSIBM278'                then result :='IBM278'                else

(* Name: IBM280                                              [RFC1345,KXS2]   *)
(* MIBenum: 2035                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP280                                                               *)
(* Alias: ebcdic-cp-it                                                        *)
(* Alias: csIBM280                                                            *)

  if name='IBM280'                  then result :='IBM280'                else
  if name='CP280'                   then result :='IBM280'                else
  if name='EBCDIC-CP-IT'            then result :='IBM280'                else
  if name='CSIBM280'                then result :='IBM280'                else

(* Name: IBM281                                              [RFC1345,KXS2]   *)
(* MIBenum: 2036                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-JP-E                                                         *)
(* Alias: cp281                                                               *)
(* Alias: csIBM281                                                            *)

  if name='IBM281'                  then result :='IBM281'                else
  if name='EBCDIC-JP-E'             then result :='IBM281'                else
  if name='CP281'                   then result :='IBM281'                else
  if name='CSIBM281'                then result :='IBM281'                else

(* Name: IBM284                                              [RFC1345,KXS2]   *)
(* MIBenum: 2037                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP284                                                               *)
(* Alias: ebcdic-cp-es                                                        *)
(* Alias: csIBM284                                                            *)

  if name='IBM284'                  then result :='IBM284'                else
  if name='CP284'                   then result :='IBM284'                else
  if name='EBCDIC-CP-ES'            then result :='IBM284'                else
  if name='CSIBM284'                then result :='IBM284'                else

(* Name: IBM285                                              [RFC1345,KXS2]   *)
(* MIBenum: 2038                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP285                                                               *)
(* Alias: ebcdic-cp-gb                                                        *)
(* Alias: csIBM285                                                            *)

  if name='IBM285'                  then result :='IBM285'                else
  if name='CP285'                   then result :='IBM285'                else
  if name='EBCDIC-CP-GB'            then result :='IBM285'                else
  if name='CSIBM285'                then result :='IBM285'                else

(* Name: IBM290                                              [RFC1345,KXS2]   *)
(* MIBenum: 2039                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: cp290                                                               *)
(* Alias: EBCDIC-JP-kana                                                      *)
(* Alias: csIBM290                                                            *)

  if name='IBM290'                  then result :='IBM290'                else
  if name='CP290'                   then result :='IBM290'                else
  if name='EBCDIC-JP-KANA'          then result :='IBM290'                else
  if name='CSIBM290'                then result :='IBM290'                else

(* Name: IBM297                                              [RFC1345,KXS2]   *)
(* MIBenum: 2040                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp297                                                               *)
(* Alias: ebcdic-cp-fr                                                        *)
(* Alias: csIBM297                                                            *)

  if name='IBM297'                  then result :='IBM297'                else
  if name='CP297'                   then result :='IBM297'                else
  if name='EBCDIC-CP-FR'            then result :='IBM297'                else
  if name='CSIBM297'                then result :='IBM297'                else

(* Name: IBM420                                              [RFC1345,KXS2]   *)
(* MIBenum: 2041                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990,                          *)
(*         IBM NLS RM p 11-11                                                 *)
(* Alias: cp420                                                               *)
(* Alias: ebcdic-cp-ar1                                                       *)
(* Alias: csIBM420                                                            *)

  if name='IBM420'                  then result :='IBM420'                else
  if name='CP420'                   then result :='IBM420'                else
  if name='EBCDIC-CP-AR1'           then result :='IBM420'                else
  if name='CSIBM420'                then result :='IBM420'                else

(* Name: IBM423                                              [RFC1345,KXS2]   *)
(* MIBenum: 2042                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp423                                                               *)
(* Alias: ebcdic-cp-gr                                                        *)
(* Alias: csIBM423                                                            *)

  if name='IBM423'                  then result :='IBM423'                else
  if name='CP423'                   then result :='IBM423'                else
  if name='EBCDIC-CP-GR'            then result :='IBM423'                else
  if name='CSIBM423'                then result :='IBM423'                else

(* Name: IBM424                                              [RFC1345,KXS2]   *)
(* MIBenum: 2043                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp424                                                               *)
(* Alias: ebcdic-cp-he                                                        *)
(* Alias: csIBM424                                                            *)

  if name='IBM424'                  then result :='IBM424'                else
  if name='CP424'                   then result :='IBM424'                else
  if name='EBCDIC-CP-HE'            then result :='IBM424'                else
  if name='CSIBM424'                then result :='IBM424'                else

(* Name: IBM437                                              [RFC1345,KXS2]   *)
(* MIBenum: 2011                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp437                                                               *)
(* Alias: 437                                                                 *)
(* Alias: csPC8CodePage437                                                    *)

  if name='IBM437'                  then result :='IBM437'                else
  if name='CP437'                   then result :='IBM437'                else
  if name='437'                     then result :='IBM437'                else
  if name='CSPC8CODEPAGE437'        then result :='IBM437'                else

(* Name: IBM500                                              [RFC1345,KXS2]   *)
(* MIBenum: 2044                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP500                                                               *)
(* Alias: ebcdic-cp-be                                                        *)
(* Alias: ebcdic-cp-ch                                                        *)
(* Alias: csIBM500                                                            *)

  if name='IBM500'                  then result :='IBM500'                else
  if name='CP500'                   then result :='IBM500'                else
  if name='EBCDIC-CP-BE'            then result :='IBM500'                else
  if name='EBCDIC-CP-CH'            then result :='IBM500'                else
  if name='CSIBM500'                then result :='IBM500'                else

(* Name: IBM775                                                   [HP-PCL5]   *)
(* MIBenum: 2087                                                              *)
(* Source: HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996            *)
(* Alias: cp775                                                               *)
(* Alias: csPC775Baltic                                                       *)

  if name='IBM775'                  then result :='IBM775'                else
  if name='CP775'                   then result :='IBM775'                else
  if name='CSPC775BALTIC'           then result :='IBM775'                else

(* Name: IBM850                                              [RFC1345,KXS2]   *)
(* MIBenum: 2009                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp850                                                               *)
(* Alias: 850                                                                 *)
(* Alias: csPC850Multilingual                                                 *)

  if name='IBM850'                  then result :='IBM850'                else
  if name='CP850'                   then result :='IBM850'                else
  if name='850'                     then result :='IBM850'                else
  if name='CSPC850MULTILINGUAL'     then result :='IBM850'                else

(* Name: IBM851                                              [RFC1345,KXS2]   *)
(* MIBenum: 2045                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp851                                                               *)
(* Alias: 851                                                                 *)
(* Alias: csIBM851                                                            *)

  if name='IBM851'                  then result :='IBM851'                else
  if name='CP851'                   then result :='IBM851'                else
  if name='851'                     then result :='IBM851'                else
  if name='CSIBM851'                then result :='IBM851'                else

(* Name: IBM852                                              [RFC1345,KXS2]   *)
(* MIBenum: 2010                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp852                                                               *)
(* Alias: 852                                                                 *)
(* Alias: csPCp852                                                            *)

  if name='IBM852'                  then result :='IBM852'                else
  if name='CP852'                   then result :='IBM852'                else
  if name='852'                     then result :='IBM852'                else
  if name='CSPCP852'                then result :='IBM852'                else

(* Name: IBM855                                              [RFC1345,KXS2]   *)
(* MIBenum: 2046                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp855                                                               *)
(* Alias: 855                                                                 *)
(* Alias: csIBM855                                                            *)

  if name='IBM855'                  then result :='IBM855'                else
  if name='CP855'                   then result :='IBM855'                else
  if name='855'                     then result :='IBM855'                else
  if name='CSIBM855'                then result :='IBM855'                else

(* Name: IBM857                                              [RFC1345,KXS2]   *)
(* MIBenum: 2047                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp857                                                               *)
(* Alias: 857                                                                 *)
(* Alias: csIBM857                                                            *)

  if name='IBM857'                  then result :='IBM857'                else
  if name='CP857'                   then result :='IBM857'                else
  if name='857'                     then result :='IBM857'                else
  if name='CSIBM857'                then result :='IBM857'                else

(* Name: IBM860                                              [RFC1345,KXS2]   *)
(* MIBenum: 2048                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp860                                                               *)
(* Alias: 860                                                                 *)
(* Alias: csIBM860                                                            *)

  if name='IBM860'                  then result :='IBM860'                else
  if name='CP860'                   then result :='IBM860'                else
  if name='860'                     then result :='IBM860'                else
  if name='CSIBM860'                then result :='IBM860'                else

(* Name: IBM861                                              [RFC1345,KXS2]   *)
(* MIBenum: 2049                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp861                                                               *)
(* Alias: 861                                                                 *)
(* Alias: cp-is                                                               *)
(* Alias: csIBM861                                                            *)

  if name='IBM861'                  then result :='IBM861'                else
  if name='CP861'                   then result :='IBM861'                else
  if name='861'                     then result :='IBM861'                else
  if name='CP-IS'                   then result :='IBM861'                else
  if name='CSIBM861'                then result :='IBM861'                else

(* Name: IBM862                                              [RFC1345,KXS2]   *)
(* MIBenum: 2013                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp862                                                               *)
(* Alias: 862                                                                 *)
(* Alias: csPC862LatinHebrew                                                  *)

  if name='IBM862'                  then result :='IBM862'                else
  if name='CP862'                   then result :='IBM862'                else
  if name='862'                     then result :='IBM862'                else
  if name='CSPC862LATINHEBREW'      then result :='IBM862'                else

(* Name: IBM863                                              [RFC1345,KXS2]   *)
(* MIBenum: 2050                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp863                                                               *)
(* Alias: 863                                                                 *)
(* Alias: csIBM863                                                            *)

  if name='IBM863'                  then result :='IBM863'                else
  if name='CP863'                   then result :='IBM863'                else
  if name='863'                     then result :='IBM863'                else
  if name='CSIBM863'                then result :='IBM863'                else

(* Name: IBM864                                              [RFC1345,KXS2]   *)
(* MIBenum: 2051                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp864                                                               *)
(* Alias: csIBM864                                                            *)

  if name='IBM864'                  then result :='IBM864'                else
  if name='CP864'                   then result :='IBM864'                else
  if name='CSIBM864'                then result :='IBM864'                else

(* Name: IBM865                                              [RFC1345,KXS2]   *)
(* MIBenum: 2052                                                              *)
(* Source: IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987)                     *)
(* Alias: cp865                                                               *)
(* Alias: 865                                                                 *)
(* Alias: csIBM865                                                            *)

  if name='IBM865'                  then result :='IBM865'                else
  if name='CP865'                   then result :='IBM865'                else
  if name='865'                     then result :='IBM865'                else
  if name='CSIBM865'                then result :='IBM865'                else

(* Name: IBM866                                                     [Pond]    *)
(* MIBenum: 2086                                                              *)
(* Source: IBM NLDG Volume 2 (SE09-8002-03) August 1994                       *)
(* Alias: cp866                                                               *)
(* Alias: 866                                                                 *)
(* Alias: csIBM866                                                            *)

  if name='IBM866'                  then result :='IBM866'                else
  if name='CP866'                   then result :='IBM866'                else
  if name='866'                     then result :='IBM866'                else
  if name='CSIBM866'                then result :='IBM866'                else

(* Name: IBM868                                              [RFC1345,KXS2]   *)
(* MIBenum: 2053                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP868                                                               *)
(* Alias: cp-ar                                                               *)
(* Alias: csIBM868                                                            *)

  if name='IBM868'                  then result :='IBM868'                else
  if name='CP868'                   then result :='IBM868'                else
  if name='CP-AR'                   then result :='IBM868'                else
  if name='CSIBM868'                then result :='IBM868'                else

(* Name: IBM869                                              [RFC1345,KXS2]   *)
(* MIBenum: 2054                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp869                                                               *)
(* Alias: 869                                                                 *)
(* Alias: cp-gr                                                               *)
(* Alias: csIBM869                                                            *)

  if name='IBM869'                  then result :='IBM869'                else
  if name='CP869'                   then result :='IBM869'                else
  if name='869'                     then result :='IBM869'                else
  if name='CP-GR'                   then result :='IBM869'                else
  if name='CSIBM869'                then result :='IBM869'                else

(* Name: IBM870                                              [RFC1345,KXS2]   *)
(* MIBenum: 2055                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP870                                                               *)
(* Alias: ebcdic-cp-roece                                                     *)
(* Alias: ebcdic-cp-yu                                                        *)
(* Alias: csIBM870                                                            *)

  if name='IBM870'                  then result :='IBM870'                else
  if name='CP870'                   then result :='IBM870'                else
  if name='EBCDIC-CP-ROECE'         then result :='IBM870'                else
  if name='EBCDIC-CP-YU'            then result :='IBM870'                else
  if name='CSIBM870'                then result :='IBM870'                else

(* Name: IBM871                                              [RFC1345,KXS2]   *)
(* MIBenum: 2056                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP871                                                               *)
(* Alias: ebcdic-cp-is                                                        *)
(* Alias: csIBM871                                                            *)

  if name='IBM871'                  then result :='IBM871'                else
  if name='CP871'                   then result :='IBM871'                else
  if name='EBCDIC-CP-IS'            then result :='IBM871'                else
  if name='CSIBM871'                then result :='IBM871'                else

(* Name: IBM880                                              [RFC1345,KXS2]   *)
(* MIBenum: 2057                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp880                                                               *)
(* Alias: EBCDIC-Cyrillic                                                     *)
(* Alias: csIBM880                                                            *)

  if name='IBM880'                  then result :='IBM880'                else
  if name='CP880'                   then result :='IBM880'                else
  if name='EBCDIC-CYRILLIC'         then result :='IBM880'                else
  if name='CSIBM880'                then result :='IBM880'                else

(* Name: IBM891                                              [RFC1345,KXS2]   *)
(* MIBenum: 2058                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp891                                                               *)
(* Alias: csIBM891                                                            *)

  if name='IBM891'                  then result :='IBM891'                else
  if name='CP891'                   then result :='IBM891'                else
  if name='CSIBM891'                then result :='IBM891'                else

(* Name: IBM903                                              [RFC1345,KXS2]   *)
(* MIBenum: 2059                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp903                                                               *)
(* Alias: csIBM903                                                            *)

  if name='IBM903'                  then result :='IBM903'                else
  if name='CP903'                   then result :='IBM903'                else
  if name='CSIBM903'                then result :='IBM903'                else

(* Name: IBM904                                              [RFC1345,KXS2]   *)
(* MIBenum: 2060                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp904                                                               *)
(* Alias: 904                                                                 *)
(* Alias: csIBBM904                                                           *)

  if name='IBM904'                  then result :='IBM904'                else
  if name='CP904'                   then result :='IBM904'                else
  if name='904'                     then result :='IBM904'                else
  if name='CSIBBM904'               then result :='IBM904'                else

(* Name: IBM905                                              [RFC1345,KXS2]   *)
(* MIBenum: 2061                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: CP905                                                               *)
(* Alias: ebcdic-cp-tr                                                        *)
(* Alias: csIBM905                                                            *)

  if name='IBM905'                  then result :='IBM905'                else
  if name='CP905'                   then result :='IBM905'                else
  if name='EBCDIC-CP-TR'            then result :='IBM905'                else
  if name='CSIBM905'                then result :='IBM905'                else

(* Name: IBM918                                              [RFC1345,KXS2]   *)
(* MIBenum: 2062                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP918                                                               *)
(* Alias: ebcdic-cp-ar2                                                       *)
(* Alias: csIBM918                                                            *)

  if name='IBM918'                  then result :='IBM918'                else
  if name='CP918'                   then result :='IBM918'                else
  if name='EBCDIC-CP-AR2'           then result :='IBM918'                else
  if name='CSIBM918'                then result :='IBM918'                else

(* Name: IBM1026                                             [RFC1345,KXS2]   *)
(* MIBenum: 2063                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP1026                                                              *)
(* Alias: csIBM1026                                                           *)

  if name='IBM1026'                 then result :='IBM1026'               else
  if name='CP1026'                  then result :='IBM1026'               else
  if name='CSIBM1026'               then result :='IBM1026'               else

(* Name: EBCDIC-AT-DE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2064                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csIBMEBCDICATDE                                                     *)

  if name='EBCDIC-AT-DE'            then result :='EBCDIC-AT-DE'          else
  if name='CSIBMEBCDICATDE'         then result :='EBCDIC-AT-DE'          else

(* Name: EBCDIC-AT-DE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2065                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICATDEA                                                       *)

  if name='EBCDIC-AT-DE-A'          then result :='EBCDIC-AT-DE-A'        else
  if name='CSEBCDICATDEA'           then result :='EBCDIC-AT-DE-A'        else

(* Name: EBCDIC-CA-FR                                        [RFC1345,KXS2]   *)
(* MIBenum: 2066                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICCAFR                                                        *)

  if name='EBCDIC-CA-FR'            then result :='EBCDIC-CA-FR'          else
  if name='CSEBCDICCAFR'            then result :='EBCDIC-CA-FR'          else

(* Name: EBCDIC-DK-NO                                        [RFC1345,KXS2]   *)
(* MIBenum: 2067                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNO                                                        *)

  if name='EBCDIC-DK-NO'            then result :='EBCDIC-DK-NO'          else
  if name='CSEBCDICDKNO'            then result :='EBCDIC-DK-NO'          else

(* Name: EBCDIC-DK-NO-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2068                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNOA                                                       *)

  if name='EBCDIC-DK-NO-A'          then result :='EBCDIC-DK-NO-A'        else
  if name='CSEBCDICDKNOA'           then result :='EBCDIC-DK-NO-A'        else

(* Name: EBCDIC-FI-SE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2069                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISE                                                        *)

  if name='EBCDIC-FI-SE'            then result :='EBCDIC-FI-SE'          else
  if name='CSEBCDICFISE'            then result :='EBCDIC-FI-SE'          else

(* Name: EBCDIC-FI-SE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2070                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISEA                                                       *)

  if name='EBCDIC-FI-SE-A'          then result :='EBCDIC-FI-SE-A'        else
  if name='CSEBCDICFISEA'           then result :='EBCDIC-FI-SE-A'        else

(* Name: EBCDIC-FR                                           [RFC1345,KXS2]   *)
(* MIBenum: 2071                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFR                                                          *)

  if name='EBCDIC-FR'               then result :='EBCDIC-FR'             else
  if name='CSEBCDICFR'              then result :='EBCDIC-FR'             else

(* Name: EBCDIC-IT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2072                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICIT                                                          *)

  if name='EBCDIC-IT'               then result :='EBCDIC-IT'             else
  if name='CSEBCDICIT'              then result :='EBCDIC-IT'             else

(* Name: EBCDIC-PT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2073                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICPT                                                          *)

  if name='EBCDIC-PT'               then result :='EBCDIC-PT'             else
  if name='CSEBCDICPT'              then result :='EBCDIC-PT'             else

(* Name: EBCDIC-ES                                           [RFC1345,KXS2]   *)
(* MIBenum: 2074                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICES                                                          *)

  if name='EBCDIC-ES'               then result :='EBCDIC-ES'             else
  if name='CSEBCDICES'              then result :='EBCDIC-ES'             else

(* Name: EBCDIC-ES-A                                         [RFC1345,KXS2]   *)
(* MIBenum: 2075                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESA                                                         *)

  if name='EBCDIC-ES-A'             then result :='EBCDIC-ES-A'           else
  if name='CSEBCDICESA'             then result :='EBCDIC-ES-A'           else

(* Name: EBCDIC-ES-S                                         [RFC1345,KXS2]   *)
(* MIBenum: 2076                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESS                                                         *)

  if name='EBCDIC-ES-S'             then result :='EBCDIC-ES-S'           else
  if name='CSEBCDICESS'             then result :='EBCDIC-ES-S'           else

(* Name: EBCDIC-UK                                           [RFC1345,KXS2]   *)
(* MIBenum: 2077                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUK                                                          *)

  if name='EBCDIC-UK'               then result :='EBCDIC-UK'             else
  if name='CSEBCDICUK'              then result :='EBCDIC-UK'             else

(* Name: EBCDIC-US                                           [RFC1345,KXS2]   *)
(* MIBenum: 2078                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUS                                                          *)

  if name='EBCDIC-US'               then result :='EBCDIC-US'             else
  if name='CSEBCDICUS'              then result :='EBCDIC-US'             else

(* Name: UNKNOWN-8BIT                                             [RFC1428]   *)
(* MIBenum: 2079                                                              *)
(* Alias: csUnknown8BiT                                                       *)

  if name='UNKNOWN-8BIT'            then result :='UNKNOWN-8BIT'          else
  if name='CSUNKNOWN8BIT'           then result :='UNKNOWN-8BIT'          else

(* Name: MNEMONIC                                            [RFC1345,KXS2]   *)
(* MIBenum: 2080                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+38"                        *)
(* Alias: csMnemonic                                                          *)

  if name='MNEMONIC'                then result :='MNEMONIC'              else
  if name='CSMNEMONIC'              then result :='MNEMONIC'              else

(* Name: MNEM                                                [RFC1345,KXS2]   *)
(* MIBenum: 2081                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+8200"                      *)
(* Alias: csMnem                                                              *)

  if name='MNEM'                    then result :='MNEM'                  else
  if name='CSMNEM'                  then result :='MNEM'                  else

(* Name: VISCII                                                   [RFC1456]   *)
(* MIBenum: 2082                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVISCII                                                            *)

  if name='VISCII'                  then result :='VISCII'                else
  if name='CSVISCII'                then result :='VISCII'                else

(* Name: VIQR                                                     [RFC1456]   *)
(* MIBenum: 2083                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVIQR                                                              *)

  if name='VIQR'                    then result :='VIQR'                  else
  if name='CSVIQR'                  then result :='VIQR'                  else

(* Name: KOI8-R  (preferred MIME name)                            [RFC1489]   *)
(* MIBenum: 2084                                                              *)
(* Source: RFC 1489, based on GOST-19768-74, ISO-6937/8,                      *)
(*         INIS-Cyrillic, ISO-5427.                                           *)
(* Alias: csKOI8R                                                             *)

  if name='KOI8-R'                  then result :='KOI8-R'                else
  if name='CSKOI8R'                 then result :='KOI8-R'                else

(* Name: KOI8-U                                                   [RFC2319]   *)
(* MIBenum: 2088                                                              *)
(* Source: RFC 2319                                                           *)

  if name='KOI8-U'                  then result :='KOI8-U'                else

(* Name: IBM00858                                                             *)
(* MIBenum: 2089                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM00858)    [Mahdi]   *)
(* Alias: CCSID00858                                                          *)
(* Alias: CP00858                                                             *)
(* Alias: PC-Multilingual-850+euro                                            *)

  if name='IBM00858'                then result :='IBM00858'              else
  if name='CCSID00858'              then result :='IBM00858'              else
  if name='CP00858'                 then result :='IBM00858'              else
  if name='PC-MULTILINGUAL-850+EURO' then result :='IBM00858'             else

(* Name: IBM00924                                                             *)
(* MIBenum: 2090                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM00924)    [Mahdi]   *)
(* Alias: CCSID00924                                                          *)
(* Alias: CP00924                                                             *)
(* Alias: ebcdic-Latin9--euro                                                 *)

  if name='IBM00924'                then result :='IBM00924'              else
  if name='CCSID00924'              then result :='IBM00924'              else
  if name='CP00924'                 then result :='IBM00924'              else
  if name='EBCDIC-LATIN9--EURO'     then result :='IBM00924'              else

(* Name: IBM01140                                                             *)
(* MIBenum: 2091                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01140)    [Mahdi]   *)
(* Alias: CCSID01140                                                          *)
(* Alias: CP01140                                                             *)
(* Alias: ebcdic-us-37+euro                                                   *)

  if name='IBM01140'                then result :='IBM01140'              else
  if name='CCSID01140'              then result :='IBM01140'              else
  if name='CP01140'                 then result :='IBM01140'              else
  if name='EBCDIC-US-37+EURO'       then result :='IBM01140'              else

(* Name: IBM01141                                                             *)
(* MIBenum: 2092                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01141)    [Mahdi]   *)
(* Alias: CCSID01141                                                          *)
(* Alias: CP01141                                                             *)
(* Alias: ebcdic-de-273+euro                                                  *)

  if name='IBM01141'                then result :='IBM01141'              else
  if name='CCSID01141'              then result :='IBM01141'              else
  if name='CP01141'                 then result :='IBM01141'              else
  if name='EBCDIC-DE-273+EURO'      then result :='IBM01141'              else

(* Name: IBM01142                                                             *)
(* MIBenum: 2093                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01142)    [Mahdi]   *)
(* Alias: CCSID01142                                                          *)
(* Alias: CP01142                                                             *)
(* Alias: ebcdic-dk-277+euro                                                  *)
(* Alias: ebcdic-no-277+euro                                                  *)

  if name='IBM01142'                then result :='IBM01142'              else
  if name='CCSID01142'              then result :='IBM01142'              else
  if name='CP01142'                 then result :='IBM01142'              else
  if name='EBCDIC-DK-277+EURO'      then result :='IBM01142'              else
  if name='EBCDIC-NO-277+EURO'      then result :='IBM01142'              else

(* Name: IBM01143                                                             *)
(* MIBenum: 2094                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01143)    [Mahdi]   *)
(* Alias: CCSID01143                                                          *)
(* Alias: CP01143                                                             *)
(* Alias: ebcdic-fi-278+euro                                                  *)
(* Alias: ebcdic-se-278+euro                                                  *)

  if name='IBM01143'                then result :='IBM01143'              else
  if name='CCSID01143'              then result :='IBM01143'              else
  if name='CP01143'                 then result :='IBM01143'              else
  if name='EBCDIC-FI-278+EURO'      then result :='IBM01143'              else
  if name='EBCDIC-SE-278+EURO'      then result :='IBM01143'              else

(* Name: IBM01144                                                             *)
(* MIBenum: 2095                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01144)    [Mahdi]   *)
(* Alias: CCSID01144                                                          *)
(* Alias: CP01144                                                             *)
(* Alias: ebcdic-it-280+euro                                                  *)

  if name='IBM01144'                then result :='IBM01144'              else
  if name='CCSID01144'              then result :='IBM01144'              else
  if name='CP01144'                 then result :='IBM01144'              else
  if name='EBCDIC-IT-280+EURO'      then result :='IBM01144'              else

(* Name: IBM01145                                                             *)
(* MIBenum: 2096                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01145)    [Mahdi]   *)
(* Alias: CCSID01145                                                          *)
(* Alias: CP01145                                                             *)
(* Alias: ebcdic-es-284+euro                                                  *)

  if name='IBM01145'                then result :='IBM01145'              else
  if name='CCSID01145'              then result :='IBM01145'              else
  if name='CP01145'                 then result :='IBM01145'              else
  if name='EBCDIC-ES-284+EURO'      then result :='IBM01145'              else

(* Name: IBM01146                                                             *)
(* MIBenum: 2097                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01146)    [Mahdi]   *)
(* Alias: CCSID01146                                                          *)
(* Alias: CP01146                                                             *)
(* Alias: ebcdic-gb-285+euro                                                  *)

  if name='IBM01146'                then result :='IBM01146'              else
  if name='CCSID01146'              then result :='IBM01146'              else
  if name='CP01146'                 then result :='IBM01146'              else
  if name='EBCDIC-GB-285+EURO'      then result :='IBM01146'              else

(* Name: IBM01147                                                             *)
(* MIBenum: 2098                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01147)    [Mahdi]   *)
(* Alias: CCSID01147                                                          *)
(* Alias: CP01147                                                             *)
(* Alias: ebcdic-fr-297+euro                                                  *)

  if name='IBM01147'                then result :='IBM01147'              else
  if name='CCSID01147'              then result :='IBM01147'              else
  if name='CP01147'                 then result :='IBM01147'              else
  if name='EBCDIC-FR-297+EURO'      then result :='IBM01147'              else

(* Name: IBM01148                                                             *)
(* MIBenum: 2099                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01148)    [Mahdi]   *)
(* Alias: CCSID01148                                                          *)
(* Alias: CP01148                                                             *)
(* Alias: ebcdic-international-500+euro                                       *)

  if name='IBM01148'                then result :='IBM01148'              else
  if name='CCSID01148'              then result :='IBM01148'              else
  if name='CP01148'                 then result :='IBM01148'              else
  if name='EBCDIC-INTERNATIONAL-500+EURO' then result :='IBM01148'        else

(* Name: IBM01149                                                             *)
(* MIBenum: 2100                                                              *)
(* Source: IBM See (.../assignments/character-set-info/IBM01149)    [Mahdi]   *)
(* Alias: CCSID01149                                                          *)
(* Alias: CP01149                                                             *)
(* Alias: ebcdic-is-871+euro                                                  *)

  if name='IBM01149'                then result :='IBM01149'              else
  if name='CCSID01149'              then result :='IBM01149'              else
  if name='CP01149'                 then result :='IBM01149'              else
  if name='EBCDIC-IS-871+EURO'      then result :='IBM01149'              else

(* Name: Big5-HKSCS                                                           *)
(* MIBenum: 2101                                                              *)
(* Source:   See (.../assignments/character-set-info/Big5-HKSCS)     [Yick]   *)
(* Alias: None                                                                *)

  if name='BIG5-HKSCS'              then result :='Big5-HKSCS'            else

(* Name: UNICODE-1-1                                              [RFC1641]   *)
(* MIBenum: 1010                                                              *)
(* Source: RFC 1641                                                           *)
(* Alias: csUnicode11                                                         *)

  if name='UNICODE-1-1'             then result :='UNICODE-1-1'           else
  if name='CSUNICODE11'             then result :='UNICODE-1-1'           else

(* Name: SCSU                                                                 *)
(* MIBenum: 1011                                                              *)
(* Source: SCSU See (.../assignments/character-set-info/SCSU)     [Scherer]   *)
(* Alias: None                                                                *)

  if name='SCSU'                    then result :='SCSU'                  else

(* Name: UTF-7                                                    [RFC2152]   *)
(* MIBenum: 1012                                                              *)
(* Source: RFC 2152                                                           *)
(* Alias: None                                                                *)

  if name='UTF-7'                   then result :='UTF-7'                 else

(* Name: UTF-16BE                                                 [RFC2781]   *)
(* MIBenum: 1013                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if name='UTF-16BE'                then result :='UTF-16BE'              else

(* Name: UTF-16LE                                                 [RFC2781]   *)
(* MIBenum: 1014                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if name='UTF-16LE'                then result :='UTF-16LE'              else

(* Name: UTF-16                                                   [RFC2781]   *)
(* MIBenum: 1015                                                              *)
(* Source: RFC 2781                                                           *)
(* Alias: None                                                                *)

  if name='UTF-16'                  then result :='UTF-16'                else

(* Name: UNICODE-1-1-UTF-7                                        [RFC1642]   *)
(* MIBenum: 103                                                               *)
(* Source: RFC 1642                                                           *)
(* Alias: csUnicode11UTF7                                                     *)

  if name='UNICODE-1-1-UTF-7'       then result :='UNICODE-1-1-UTF-7'     else
  if name='CSUNICODE11UTF7'         then result :='UNICODE-1-1-UTF-7'     else

(* Name: UTF-8                                                    [RFC2279]   *)
(* MIBenum: 106                                                               *)
(* Source: RFC 2279                                                           *)
(* Alias: None                                                                *)

  if name='UTF-8'                   then result :='UTF-8'                 else

(* Name: ISO-8859-13                                                          *)
(* MIBenum: 109                                                               *)
(* Source: ISO See (...assignments/character-set-info/iso-8859-13)[Tumasonis]  *)
(* Alias: None                                                                *)

  if name='ISO-8859-13'             then result :='ISO-8859-13'           else

(* Name: ISO-8859-14                                                          *)
(* MIBenum: 110                                                               *)
(* Source: ISO See (...assignments/character-set-info/iso-8859-14) [Simonsen] *)
(* Alias: iso-ir-199                                                          *)
(* Alias: ISO_8859-14:1998                                                    *)
(* Alias: ISO_8859-14                                                         *)
(* Alias: latin8                                                              *)
(* Alias: iso-celtic                                                          *)
(* Alias: l8                                                                  *)

  if name='ISO-8859-14'             then result :='ISO-8859-14'           else
  if name='ISO-IR-199'              then result :='ISO-8859-14'           else
  if name='ISO_8859-14:1998'        then result :='ISO-8859-14'           else
  if name='ISO_8859-14'             then result :='ISO-8859-14'           else
  if name='LATIN8'                  then result :='ISO-8859-14'           else
  if name='ISO-CELTIC'              then result :='ISO-8859-14'           else
  if name='L8'                      then result :='ISO-8859-14'           else

(* Name: ISO-8859-15                                                          *)
(* MIBenum: 111                                                               *)
(* Source: ISO                                                                *)
(* Alias: ISO_8859-15                                                         *)

  if name='ISO-8859-15'             then result :='ISO-8859-15'           else
  if name='ISO_8859-15'             then result :='ISO-8859-15'           else

(* Name: ISO-8859-16                                                          *)
(* MIBenum: 112                                                               *)
(* Source: ISO                                                                *)
(* Alias:                                                                     *)

  if name='ISO-8859-16'             then result :='ISO-8859-16'           else

(* Name: JIS_Encoding                                                         *)
(* MIBenum: 16                                                                *)
(* Source: JIS X 0202-1991.  Uses ISO 2022 escape sequences to                *)
(*         shift code sets as documented in JIS X 0202-1991.                  *)
(* Alias: csJISEncoding                                                       *)

  if name='JIS_ENCODING'            then result :='JIS_Encoding'          else
  if name='CSJISENCODING'           then result :='JIS_Encoding'          else

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

  if name='SHIFT_JIS'               then result :='Shift_JIS'             else
  if name='MS_KANJI'                then result :='Shift_JIS'             else
  if name='CSSHIFTJIS'              then result :='Shift_JIS'             else

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

  if name='EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE' then result :='EUC-JP' else
  if name='CSEUCPKDFMTJAPANESE'     then result :='EUC-JP'                else
  if name='EUC-JP'                  then result :='EUC-JP'                else

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

  if name='EXTENDED_UNIX_CODE_FIXED_WIDTH_FOR_JAPANESE' then result :='Extended_UNIX_Code_Fixed_Width_for_Japanese' else
  if name='CSEUCFIXWIDJAPANESE'     then result :='Extended_UNIX_Code_Fixed_Width_for_Japanese' else

(* Name: ISO-10646-UCS-Basic                                                  *)
(* MIBenum: 1002                                                              *)
(* Source: ASCII subset of Unicode.  Basic Latin = collection 1               *)
(*         See ISO 10646, Appendix A                                          *)
(* Alias: csUnicodeASCII                                                      *)

  if name='ISO-10646-UCS-BASIC'     then result :='ISO-10646-UCS-Basic'   else
  if name='CSUNICODEASCII'          then result :='ISO-10646-UCS-Basic'   else

(* Name: ISO-10646-Unicode-Latin1                                             *)
(* MIBenum: 1003                                                              *)
(* Source: ISO Latin-1 subset of Unicode. Basic Latin and Latin-1             *)
(*          Supplement  = collections 1 and 2.  See ISO 10646,                *)
(*          Appendix A.  See RFC 1815.                                        *)
(* Alias: csUnicodeLatin1                                                     *)
(* Alias: ISO-10646                                                           *)

  if name='ISO-10646-UNICODE-LATIN1' then result :='ISO-10646-Unicode-Latin1' else
  if name='CSUNICODELATIN1'         then result :='ISO-10646-Unicode-Latin1' else
  if name='ISO-10646'               then result :='ISO-10646-Unicode-Latin1' else

(* Name: ISO-10646-J-1                                                        *)
(* Source: ISO 10646 Japanese, see RFC 1815.                                  *)

  if name='ISO-10646-J-1'           then result :='ISO-10646-J-1'         else

(* Name: ISO-Unicode-IBM-1261                                                 *)
(* MIBenum: 1005                                                              *)
(* Source: IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261       *)
(* Alias: csUnicodeIBM1261                                                    *)

  if name='ISO-UNICODE-IBM-1261'    then result :='ISO-Unicode-IBM-1261'  else
  if name='CSUNICODEIBM1261'        then result :='ISO-Unicode-IBM-1261'  else

(* Name: ISO-Unicode-IBM-1268                                                 *)
(* MIBenum: 1006                                                              *)
(* Source: IBM Latin-4 Extended Presentation Set, GCSGID: 1268                *)
(* Alias: csUnidoceIBM1268                                                    *)

  if name='ISO-UNICODE-IBM-1268'    then result :='ISO-Unicode-IBM-1268'  else
  if name='CSUNIDOCEIBM1268'        then result :='ISO-Unicode-IBM-1268'  else

(* Name: ISO-Unicode-IBM-1276                                                 *)
(* MIBenum: 1007                                                              *)
(* Source: IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276         *)
(* Alias: csUnicodeIBM1276                                                    *)

  if name='ISO-UNICODE-IBM-1276'    then result :='ISO-Unicode-IBM-1276'  else
  if name='CSUNICODEIBM1276'        then result :='ISO-Unicode-IBM-1276'  else

(* Name: ISO-Unicode-IBM-1264                                                 *)
(* MIBenum: 1008                                                              *)
(* Source: IBM Arabic Presentation Set, GCSGID: 1264                          *)
(* Alias: csUnicodeIBM1264                                                    *)

  if name='ISO-UNICODE-IBM-1264'    then result :='ISO-Unicode-IBM-1264'  else
  if name='CSUNICODEIBM1264'        then result :='ISO-Unicode-IBM-1264'  else

(* Name: ISO-Unicode-IBM-1265                                                 *)
(* MIBenum: 1009                                                              *)
(* Source: IBM Hebrew Presentation Set, GCSGID: 1265                          *)
(* Alias: csUnicodeIBM1265                                                    *)

  if name='ISO-UNICODE-IBM-1265'    then result :='ISO-Unicode-IBM-1265'  else
  if name='CSUNICODEIBM1265'        then result :='ISO-Unicode-IBM-1265'  else

(* Name: ISO-8859-1-Windows-3.0-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2000                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.0.                       *)
(*         PCL Symbol Set id: 9U                                              *)
(* Alias: csWindows30Latin1                                                   *)

  if name='ISO-8859-1-WINDOWS-3.0-LATIN-1' then result :='ISO-8859-1-Windows-3.0-Latin-1' else
  if name='CSWINDOWS30LATIN1'       then result :='ISO-8859-1-Windows-3.0-Latin-1' else

(* Name: ISO-8859-1-Windows-3.1-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2001                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.1.                       *)
(*         PCL Symbol Set id: 19U                                             *)
(* Alias: csWindows31Latin1                                                   *)

  if name='ISO-8859-1-WINDOWS-3.1-LATIN-1' then result :='ISO-8859-1-Windows-3.1-Latin-1' else
  if name='CSWINDOWS31LATIN1'       then result :='ISO-8859-1-Windows-3.1-Latin-1' else

(* Name: ISO-8859-2-Windows-Latin-2                               [HP-PCL5]   *)
(* MIBenum: 2002                                                              *)
(* Source: Extended ISO 8859-2.  Latin-2 for Windows 3.1.                     *)
(*         PCL Symbol Set id: 9E                                              *)
(* Alias: csWindows31Latin2                                                   *)

  if name='ISO-8859-2-WINDOWS-LATIN-2' then result :='ISO-8859-2-Windows-Latin-2' else
  if name='CSWINDOWS31LATIN2'       then result :='ISO-8859-2-Windows-Latin-2' else

(* Name: ISO-8859-9-Windows-Latin-5                               [HP-PCL5]   *)
(* MIBenum: 2003                                                              *)
(* Source: Extended ISO 8859-9.  Latin-5 for Windows 3.1                      *)
(*         PCL Symbol Set id: 5T                                              *)
(* Alias: csWindows31Latin5                                                   *)

  if name='ISO-8859-9-WINDOWS-LATIN-5' then result :='ISO-8859-9-Windows-Latin-5' else
  if name='CSWINDOWS31LATIN5'       then result :='ISO-8859-9-Windows-Latin-5' else

(* Name: Adobe-Standard-Encoding                                    [Adobe]   *)
(* MIBenum: 2005                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 10J                                             *)
(* Alias: csAdobeStandardEncoding                                             *)

  if name='ADOBE-STANDARD-ENCODING' then result :='Adobe-Standard-Encoding' else
  if name='CSADOBESTANDARDENCODING' then result :='Adobe-Standard-Encoding' else

(* Name: Ventura-US                                               [HP-PCL5]   *)
(* MIBenum: 2006                                                              *)
(* Source: Ventura US.  ASCII plus characters typically used in               *)
(*         publishing, like pilcrow, copyright, registered, trade mark,       *)
(*         section, dagger, and double dagger in the range A0 (hex)           *)
(*         to FF (hex).                                                       *)
(*         PCL Symbol Set id: 14J                                             *)
(* Alias: csVenturaUS                                                         *)

  if name='VENTURA-US'              then result :='Ventura-US'            else
  if name='CSVENTURAUS'             then result :='Ventura-US'            else

(* Name: Ventura-International                                    [HP-PCL5]   *)
(* MIBenum: 2007                                                              *)
(* Source: Ventura International.  ASCII plus coded characters similar        *)
(*         to Roman8.                                                         *)
(*         PCL Symbol Set id: 13J                                             *)
(* Alias: csVenturaInternational                                              *)

  if name='VENTURA-INTERNATIONAL'   then result :='Ventura-International' else
  if name='CSVENTURAINTERNATIONAL'  then result :='Ventura-International' else

(* Name: PC8-Danish-Norwegian                                     [HP-PCL5]   *)
(* MIBenum: 2012                                                              *)
(* Source: PC Danish Norwegian                                                *)
(*         8-bit PC set for Danish Norwegian                                  *)
(*         PCL Symbol Set id: 11U                                             *)
(* Alias: csPC8DanishNorwegian                                                *)

  if name='PC8-DANISH-NORWEGIAN'    then result :='PC8-Danish-Norwegian'  else
  if name='CSPC8DANISHNORWEGIAN'    then result :='PC8-Danish-Norwegian'  else

(* Name: PC8-Turkish                                              [HP-PCL5]   *)
(* MIBenum: 2014                                                              *)
(* Source: PC Latin Turkish.  PCL Symbol Set id: 9T                           *)
(* Alias: csPC8Turkish                                                        *)

  if name='PC8-TURKISH'             then result :='PC8-Turkish'           else
  if name='CSPC8TURKISH'            then result :='PC8-Turkish'           else

(* Name: IBM-Symbols                                             [IBM-CIDT]   *)
(* MIBenum: 2015                                                              *)
(* Source: Presentation Set, CPGID: 259                                       *)
(* Alias: csIBMSymbols                                                        *)

  if name='IBM-SYMBOLS'             then result :='IBM-Symbols'           else
  if name='CSIBMSYMBOLS'            then result :='IBM-Symbols'           else

(* Name: IBM-Thai                                                [IBM-CIDT]   *)
(* MIBenum: 2016                                                              *)
(* Source: Presentation Set, CPGID: 838                                       *)
(* Alias: csIBMThai                                                           *)

  if name='IBM-THAI'                then result :='IBM-Thai'              else
  if name='CSIBMTHAI'               then result :='IBM-Thai'              else

(* Name: HP-Legal                                                 [HP-PCL5]   *)
(* MIBenum: 2017                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 1U                                              *)
(* Alias: csHPLegal                                                           *)

  if name='HP-LEGAL'                then result :='HP-Legal'              else
  if name='CSHPLEGAL'               then result :='HP-Legal'              else

(* Name: HP-Pi-font                                               [HP-PCL5]   *)
(* MIBenum: 2018                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 15U                                             *)
(* Alias: csHPPiFont                                                          *)

  if name='HP-PI-FONT'              then result :='HP-Pi-font'            else
  if name='CSHPPIFONT'              then result :='HP-Pi-font'            else

(* Name: HP-Math8                                                 [HP-PCL5]   *)
(* MIBenum: 2019                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 8M                                              *)
(* Alias: csHPMath8                                                           *)

  if name='HP-MATH8'                then result :='HP-Math8'              else
  if name='CSHPMATH8'               then result :='HP-Math8'              else

(* Name: Adobe-Symbol-Encoding                                      [Adobe]   *)
(* MIBenum: 2020                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 5M                                              *)
(* Alias: csHPPSMath                                                          *)

  if name='ADOBE-SYMBOL-ENCODING'   then result :='Adobe-Symbol-Encoding' else
  if name='CSHPPSMATH'              then result :='Adobe-Symbol-Encoding' else

(* Name: HP-DeskTop                                               [HP-PCL5]   *)
(* MIBenum: 2021                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 7J                                              *)
(* Alias: csHPDesktop                                                         *)

  if name='HP-DESKTOP'              then result :='HP-DeskTop'            else
  if name='CSHPDESKTOP'             then result :='HP-DeskTop'            else

(* Name: Ventura-Math                                             [HP-PCL5]   *)
(* MIBenum: 2022                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6M                                              *)
(* Alias: csVenturaMath                                                       *)

  if name='VENTURA-MATH'            then result :='Ventura-Math'          else
  if name='CSVENTURAMATH'           then result :='Ventura-Math'          else

(* Name: Microsoft-Publishing                                     [HP-PCL5]   *)
(* MIBenum: 2023                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6J                                              *)
(* Alias: csMicrosoftPublishing                                               *)

  if name='MICROSOFT-PUBLISHING'    then result :='Microsoft-Publishing'  else
  if name='CSMICROSOFTPUBLISHING'   then result :='Microsoft-Publishing'  else

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

  if name='WINDOWS-31J'             then result :='Windows-31J'           else
  if name='CSWINDOWS31J'            then result :='Windows-31J'           else

(* Name: GB2312  (preferred MIME name)                                        *)
(* MIBenum: 2025                                                              *)
(* Source: Chinese for People's Republic of China (PRC) mixed one byte,       *)
(*         two byte set:                                                      *)
(*           20-7E = one byte ASCII                                           *)
(*           A1-FE = two byte PRC Kanji                                       *)
(*         See GB 2312-80                                                     *)
(*         PCL Symbol Set Id: 18C                                             *)
(* Alias: csGB2312                                                            *)

  if name='GB2312'                  then result :='GB2312'                else
  if name='CSGB2312'                then result :='GB2312'                else

(* Name: Big5  (preferred MIME name)                                          *)
(* MIBenum: 2026                                                              *)
(* Source: Chinese for Taiwan Multi-byte set.                                 *)
(*         PCL Symbol Set Id: 18T                                             *)
(* Alias: csBig5                                                              *)

  if name='BIG5'                    then result :='Big5'                  else
  if name='CSBIG5'                  then result :='Big5'                  else

(* Name: windows-1250                                                         *)
(* MIBenum: 2250                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1250) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1250'            then result :='windows-1250'          else

(* Name: windows-1251                                                         *)
(* MIBenum: 2251                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1251) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1251'            then result :='windows-1251'          else

(* Name: windows-1252                                                         *)
(* MIBenum: 2252                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1252)       [Wendt]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1252'            then result :='windows-1252'          else

(* Name: windows-1253                                                         *)
(* MIBenum: 2253                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1253) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1253'            then result :='windows-1253'          else

(* Name: windows-1254                                                         *)
(* MIBenum: 2254                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1254) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1254'            then result :='windows-1254'          else

(* Name: windows-1255                                                         *)
(* MIBenum: 2255                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1255) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1255'            then result :='windows-1255'          else

(* Name: windows-1256                                                         *)
(* MIBenum: 2256                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1256) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1256'            then result :='windows-1256'          else

(* Name: windows-1257                                                         *)
(* MIBenum: 2257                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1257) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1257'            then result :='windows-1257'          else

(* Name: windows-1258                                                         *)
(* MIBenum: 2258                                                              *)
(* Source: Microsoft  (see ../character-set-info/windows-1258) [Lazhintseva]  *)
(* Alias: None                                                                *)

  if name='WINDOWS-1258'            then result :='windows-1258'          else

(* Name: TIS-620                                                              *)
(* MIBenum: 2259                                                              *)
(* Source: Thai Industrial Standards Institute (TISI)           [Tantsetthi]  *)

  if name='TIS-620'                 then result :='TIS-620'               else

(* Name: HZ-GB-2312                                                           *)
(* MIBenum: 2085                                                              *)
(* Source: RFC 1842, RFC 1843                              [RFC1842, RFC1843] *)


  result:=name;
end;

(* REFERENCES                                                                 *)

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

(* [KXS2] Keld Simonsen <Keld.Simonsen@dkuug.dk>                              *)

(* [Choi] Woohyong Choi <whchoi@cosmos.kaist.ac.kr>                           *)

(* [Lazhintseva] Katya Lazhintseva, <katyal@MICROSOFT.com>, May 1996.         *)

(* [Mahdi] Tamer Mahdi, <tamer@ca.ibm.com>, August 2000.                      *)

(* [Murai] Jun Murai <jun@wide.ad.jp>                                         *)

(* [Ohta] Masataka Ohta, <mohta@cc.titech.ac.jp>, July 1995.                  *)

(* [Nussbacher] Hank Nussbacher, <hank@vm.tau.ac.il>                          *)

(* [Pond] Rick Pond, <rickpond@vnet.ibm.com> March 1997.                      *)

(* [Scherer] Markus Scherer, <markus.scherer@jtcsv.com>, August 2000.         *)

(* [Simonsen] Keld Simonsen, <Keld.Simonsen@rap.dk>, August 2000.             *)

(* [Tantsetthi] Trin Tantsetthi, <trin@mozart.inet.co.th>, September 1998.    *)

(* [Tumasonis] Vladas Tumasonis, <vladas.tumasonis@maf.vu.lt>, August 2000.   *)

(* [Wendt] Chris Wendt, <christw@microsoft.com>, December 1999.               *)

(* [Yick] Nicky Yick, <cliac@itsd.gcn.gov.hk>, October 2000.                  *)

(* []                                                                         *)






