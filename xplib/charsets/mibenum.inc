(* $Id$ *)
(* generated from IANA charset list -- do not edit *)

function GetCharsetFromName(Const Name: String): TMIMECharset;
var UName: String;
begin
  UName:=UpperCase(Name);

(* ===================================================================        *)
(* CHARACTER SETS                                                             *)

(* (last updated 2003-09-19)                                                  *)

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

  (* overridden preference for MIME:                                        *)


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

  if UName='ANSI_X3.4-1968'          then result := csASCII                else
  if UName='ISO-IR-6'                then result := csASCII                else
  if UName='ANSI_X3.4-1986'          then result := csASCII                else
  if UName='ISO_646.IRV:1991'        then result := csASCII                else
  if UName='ASCII'                   then result := csASCII                else
  if UName='ISO646-US'               then result := csASCII                else
  if UName='US-ASCII'                then result := csASCII                else
  if UName='US'                      then result := csASCII                else
  if UName='IBM367'                  then result := csASCII                else
  if UName='CP367'                   then result := csASCII                else
  if UName='CSASCII'                 then result := csASCII                else

(* Name: ISO-10646-UTF-1                                                      *)
(* MIBenum: 27                                                                *)
(* Source: Universal Transfer Format (1), this is the multibyte               *)
(*         encoding, that subsets ASCII-7. It does not have byte              *)
(*         ordering issues.                                                   *)
(* Alias: csISO10646UTF1                                                      *)

  if UName='ISO-10646-UTF-1'         then result := csISO10646UTF1         else
  if UName='CSISO10646UTF1'          then result := csISO10646UTF1         else

(* Name: ISO_646.basic:1983                                [RFC1345,KXS2]     *)
(* MIBenum: 28                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ref                                                                 *)
(* Alias: csISO646basic1983                                                   *)

  if UName='ISO_646.BASIC:1983'      then result := csISO646basic1983      else
  if UName='REF'                     then result := csISO646basic1983      else
  if UName='CSISO646BASIC1983'       then result := csISO646basic1983      else

(* Name: INVARIANT                                         [RFC1345,KXS2]     *)
(* MIBenum: 29                                                                *)
(* Alias: csINVARIANT                                                         *)

  if UName='INVARIANT'               then result := csINVARIANT            else
  if UName='CSINVARIANT'             then result := csINVARIANT            else

(* Name: ISO_646.irv:1983                                  [RFC1345,KXS2]     *)
(* MIBenum: 30                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-2                                                            *)
(* Alias: irv                                                                 *)
(* Alias: csISO2IntlRefVersion                                                *)

  (* overridden preference for MIME: iso-ir-2                               *)

  if UName='ISO_646.IRV:1983'        then result := csISO2IntlRefVersion   else
  if UName='ISO-IR-2'                then result := csISO2IntlRefVersion   else
  if UName='IRV'                     then result := csISO2IntlRefVersion   else
  if UName='CSISO2INTLREFVERSION'    then result := csISO2IntlRefVersion   else

(* Name: BS_4730                                           [RFC1345,KXS2]     *)
(* MIBenum: 20                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-4                                                            *)
(* Alias: ISO646-GB                                                           *)
(* Alias: gb                                                                  *)
(* Alias: uk                                                                  *)
(* Alias: csISO4UnitedKingdom                                                 *)

  (* overridden preference for MIME: ISO646-GB                              *)

  if UName='BS_4730'                 then result := csISO4UnitedKingdom    else
  if UName='ISO-IR-4'                then result := csISO4UnitedKingdom    else
  if UName='ISO646-GB'               then result := csISO4UnitedKingdom    else
  if UName='GB'                      then result := csISO4UnitedKingdom    else
  if UName='UK'                      then result := csISO4UnitedKingdom    else
  if UName='CSISO4UNITEDKINGDOM'     then result := csISO4UnitedKingdom    else

(* Name: NATS-SEFI                                         [RFC1345,KXS2]     *)
(* MIBenum: 31                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-1                                                          *)
(* Alias: csNATSSEFI                                                          *)

  (* overridden preference for MIME: iso-ir-8-1                             *)

  if UName='NATS-SEFI'               then result := csNATSSEFI             else
  if UName='ISO-IR-8-1'              then result := csNATSSEFI             else
  if UName='CSNATSSEFI'              then result := csNATSSEFI             else

(* Name: NATS-SEFI-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 32                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-8-2                                                          *)
(* Alias: csNATSSEFIADD                                                       *)

  (* overridden preference for MIME: iso-ir-8-2                             *)

  if UName='NATS-SEFI-ADD'           then result := csNATSSEFIADD          else
  if UName='ISO-IR-8-2'              then result := csNATSSEFIADD          else
  if UName='CSNATSSEFIADD'           then result := csNATSSEFIADD          else

(* Name: NATS-DANO                                         [RFC1345,KXS2]     *)
(* MIBenum: 33                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-1                                                          *)
(* Alias: csNATSDANO                                                          *)

  (* overridden preference for MIME: iso-ir-9-1                             *)

  if UName='NATS-DANO'               then result := csNATSDANO             else
  if UName='ISO-IR-9-1'              then result := csNATSDANO             else
  if UName='CSNATSDANO'              then result := csNATSDANO             else

(* Name: NATS-DANO-ADD                                     [RFC1345,KXS2]     *)
(* MIBenum: 34                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-9-2                                                          *)
(* Alias: csNATSDANOADD                                                       *)

  (* overridden preference for MIME: iso-ir-9-2                             *)

  if UName='NATS-DANO-ADD'           then result := csNATSDANOADD          else
  if UName='ISO-IR-9-2'              then result := csNATSDANOADD          else
  if UName='CSNATSDANOADD'           then result := csNATSDANOADD          else

(* Name: SEN_850200_B                                      [RFC1345,KXS2]     *)
(* MIBenum: 35                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-10                                                           *)
(* Alias: FI                                                                  *)
(* Alias: ISO646-FI                                                           *)
(* Alias: ISO646-SE                                                           *)
(* Alias: se                                                                  *)
(* Alias: csISO10Swedish                                                      *)

  (* additional alias: SWEDISH                                             *)
  (* additional alias: FINNISH                                             *)
  (* overridden preference for MIME: ISO646-SE                              *)

  if UName='SEN_850200_B'            then result := csISO10Swedish         else
  if UName='ISO-IR-10'               then result := csISO10Swedish         else
  if UName='FI'                      then result := csISO10Swedish         else
  if UName='ISO646-FI'               then result := csISO10Swedish         else
  if UName='ISO646-SE'               then result := csISO10Swedish         else
  if UName='SE'                      then result := csISO10Swedish         else
  if UName='CSISO10SWEDISH'          then result := csISO10Swedish         else
  if UName='SWEDISH'                 then result := csISO10Swedish         else
  if UName='FINNISH'                 then result := csISO10Swedish         else

(* Name: SEN_850200_C                                      [RFC1345,KXS2]     *)
(* MIBenum: 21                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-11                                                           *)
(* Alias: ISO646-SE2                                                          *)
(* Alias: se2                                                                 *)
(* Alias: csISO11SwedishForNames                                              *)

  (* overridden preference for MIME: ISO646-SE2                             *)

  if UName='SEN_850200_C'            then result := csISO11SwedishForNames else
  if UName='ISO-IR-11'               then result := csISO11SwedishForNames else
  if UName='ISO646-SE2'              then result := csISO11SwedishForNames else
  if UName='SE2'                     then result := csISO11SwedishForNames else
  if UName='CSISO11SWEDISHFORNAMES'  then result := csISO11SwedishForNames else

(* Name: KS_C_5601-1987                                    [RFC1345,KXS2]     *)
(* MIBenum: 36                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-149                                                          *)
(* Alias: KS_C_5601-1989                                                      *)
(* Alias: KSC_5601                                                            *)
(* Alias: korean                                                              *)
(* Alias: csKSC56011987                                                       *)

  (* overridden preference for MIME: iso-ir-149                             *)

  if UName='KS_C_5601-1987'          then result := csKSC56011987          else
  if UName='ISO-IR-149'              then result := csKSC56011987          else
  if UName='KS_C_5601-1989'          then result := csKSC56011987          else
  if UName='KSC_5601'                then result := csKSC56011987          else
  if UName='KOREAN'                  then result := csKSC56011987          else
  if UName='CSKSC56011987'           then result := csKSC56011987          else

(* Name: ISO-2022-KR  (preferred MIME name)                [RFC1557,Choi]     *)
(* MIBenum: 37                                                                *)
(* Source: RFC-1557 (see also KS_C_5601-1987)                                 *)
(* Alias: csISO2022KR                                                         *)

  if UName='ISO-2022-KR'             then result := csISO2022KR            else
  if UName='CSISO2022KR'             then result := csISO2022KR            else

(* Name: EUC-KR  (preferred MIME name)                     [RFC1557,Choi]     *)
(* MIBenum: 38                                                                *)
(* Source: RFC-1557 (see also KS_C_5861-1992)                                 *)
(* Alias: csEUCKR                                                             *)

  if UName='EUC-KR'                  then result := csEUCKR                else
  if UName='CSEUCKR'                 then result := csEUCKR                else

(* Name: ISO-2022-JP  (preferred MIME name)               [RFC1468,Murai]     *)
(* MIBenum: 39                                                                *)
(* Source: RFC-1468 (see also RFC-2237)                                       *)
(* Alias: csISO2022JP                                                         *)

  if UName='ISO-2022-JP'             then result := csISO2022JP            else
  if UName='CSISO2022JP'             then result := csISO2022JP            else

(* Name: ISO-2022-JP-2  (preferred MIME name)              [RFC1554,Ohta]     *)
(* MIBenum: 40                                                                *)
(* Source: RFC-1554                                                           *)
(* Alias: csISO2022JP2                                                        *)

  if UName='ISO-2022-JP-2'           then result := csISO2022JP2           else
  if UName='CSISO2022JP2'            then result := csISO2022JP2           else

(* Name: ISO-2022-CN                                            [RFC1922]     *)
(* MIBenum: 104                                                               *)
(* Source: RFC-1922                                                           *)

  if UName='ISO-2022-CN'             then result := csISO_2022_CN          else

(* Name: ISO-2022-CN-EXT                                        [RFC1922]     *)
(* MIBenum: 105                                                               *)
(* Source: RFC-1922                                                           *)

  if UName='ISO-2022-CN-EXT'         then result := csISO_2022_CN_EXT      else

(* Name: JIS_C6220-1969-jp                                 [RFC1345,KXS2]     *)
(* MIBenum: 41                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: JIS_C6220-1969                                                      *)
(* Alias: iso-ir-13                                                           *)
(* Alias: katakana                                                            *)
(* Alias: x0201-7                                                             *)
(* Alias: csISO13JISC6220jp                                                   *)

  (* overridden preference for MIME: iso-ir-13                              *)

  if UName='JIS_C6220-1969-JP'       then result := csISO13JISC6220jp      else
  if UName='JIS_C6220-1969'          then result := csISO13JISC6220jp      else
  if UName='ISO-IR-13'               then result := csISO13JISC6220jp      else
  if UName='KATAKANA'                then result := csISO13JISC6220jp      else
  if UName='X0201-7'                 then result := csISO13JISC6220jp      else
  if UName='CSISO13JISC6220JP'       then result := csISO13JISC6220jp      else

(* Name: JIS_C6220-1969-ro                                 [RFC1345,KXS2]     *)
(* MIBenum: 42                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-14                                                           *)
(* Alias: jp                                                                  *)
(* Alias: ISO646-JP                                                           *)
(* Alias: csISO14JISC6220ro                                                   *)

  (* overridden preference for MIME: ISO646-JP                              *)

  if UName='JIS_C6220-1969-RO'       then result := csISO14JISC6220ro      else
  if UName='ISO-IR-14'               then result := csISO14JISC6220ro      else
  if UName='JP'                      then result := csISO14JISC6220ro      else
  if UName='ISO646-JP'               then result := csISO14JISC6220ro      else
  if UName='CSISO14JISC6220RO'       then result := csISO14JISC6220ro      else

(* Name: IT                                                [RFC1345,KXS2]     *)
(* MIBenum: 22                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-15                                                           *)
(* Alias: ISO646-IT                                                           *)
(* Alias: csISO15Italian                                                      *)

  (* additional alias: ITALIAN                                             *)
  (* overridden preference for MIME: ISO646-IT                              *)

  if UName='IT'                      then result := csISO15Italian         else
  if UName='ISO-IR-15'               then result := csISO15Italian         else
  if UName='ISO646-IT'               then result := csISO15Italian         else
  if UName='CSISO15ITALIAN'          then result := csISO15Italian         else
  if UName='ITALIAN'                 then result := csISO15Italian         else

(* Name: PT                                                [RFC1345,KXS2]     *)
(* MIBenum: 43                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-16                                                           *)
(* Alias: ISO646-PT                                                           *)
(* Alias: csISO16Portuguese                                                   *)

  (* additional alias: PORTU                                               *)
  (* overridden preference for MIME: ISO646-PT                              *)

  if UName='PT'                      then result := csISO16Portuguese      else
  if UName='ISO-IR-16'               then result := csISO16Portuguese      else
  if UName='ISO646-PT'               then result := csISO16Portuguese      else
  if UName='CSISO16PORTUGUESE'       then result := csISO16Portuguese      else
  if UName='PORTU'                   then result := csISO16Portuguese      else

(* Name: ES                                                [RFC1345,KXS2]     *)
(* MIBenum: 23                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-17                                                           *)
(* Alias: ISO646-ES                                                           *)
(* Alias: csISO17Spanish                                                      *)

  (* additional alias: SPANISH                                             *)
  (* overridden preference for MIME: ISO646-ES                              *)

  if UName='ES'                      then result := csISO17Spanish         else
  if UName='ISO-IR-17'               then result := csISO17Spanish         else
  if UName='ISO646-ES'               then result := csISO17Spanish         else
  if UName='CSISO17SPANISH'          then result := csISO17Spanish         else
  if UName='SPANISH'                 then result := csISO17Spanish         else

(* Name: greek7-old                                        [RFC1345,KXS2]     *)
(* MIBenum: 44                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-18                                                           *)
(* Alias: csISO18Greek7Old                                                    *)

  (* overridden preference for MIME: iso-ir-18                              *)

  if UName='GREEK7-OLD'              then result := csISO18Greek7Old       else
  if UName='ISO-IR-18'               then result := csISO18Greek7Old       else
  if UName='CSISO18GREEK7OLD'        then result := csISO18Greek7Old       else

(* Name: latin-greek                                       [RFC1345,KXS2]     *)
(* MIBenum: 45                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-19                                                           *)
(* Alias: csISO19LatinGreek                                                   *)

  (* overridden preference for MIME: iso-ir-19                              *)

  if UName='LATIN-GREEK'             then result := csISO19LatinGreek      else
  if UName='ISO-IR-19'               then result := csISO19LatinGreek      else
  if UName='CSISO19LATINGREEK'       then result := csISO19LatinGreek      else

(* Name: DIN_66003                                         [RFC1345,KXS2]     *)
(* MIBenum: 24                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-21                                                           *)
(* Alias: de                                                                  *)
(* Alias: ISO646-DE                                                           *)
(* Alias: csISO21German                                                       *)

  (* additional alias: GERMAN                                              *)
  (* overridden preference for MIME: ISO646-DE                              *)

  if UName='DIN_66003'               then result := csISO21German          else
  if UName='ISO-IR-21'               then result := csISO21German          else
  if UName='DE'                      then result := csISO21German          else
  if UName='ISO646-DE'               then result := csISO21German          else
  if UName='CSISO21GERMAN'           then result := csISO21German          else
  if UName='GERMAN'                  then result := csISO21German          else

(* Name: NF_Z_62-010_(1973)                                [RFC1345,KXS2]     *)
(* MIBenum: 46                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-25                                                           *)
(* Alias: ISO646-FR1                                                          *)
(* Alias: csISO25French                                                       *)

  (* overridden preference for MIME: ISO646-FR1                             *)

  if UName='NF_Z_62-010_(1973)'      then result := csISO25French          else
  if UName='ISO-IR-25'               then result := csISO25French          else
  if UName='ISO646-FR1'              then result := csISO25French          else
  if UName='CSISO25FRENCH'           then result := csISO25French          else

(* Name: Latin-greek-1                                     [RFC1345,KXS2]     *)
(* MIBenum: 47                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-27                                                           *)
(* Alias: csISO27LatinGreek1                                                  *)

  (* overridden preference for MIME: iso-ir-27                              *)

  if UName='LATIN-GREEK-1'           then result := csISO27LatinGreek1     else
  if UName='ISO-IR-27'               then result := csISO27LatinGreek1     else
  if UName='CSISO27LATINGREEK1'      then result := csISO27LatinGreek1     else

(* Name: ISO_5427                                          [RFC1345,KXS2]     *)
(* MIBenum: 48                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-37                                                           *)
(* Alias: csISO5427Cyrillic                                                   *)

  (* overridden preference for MIME: ISO_5427                               *)

  if UName='ISO_5427'                then result := csISO5427Cyrillic      else
  if UName='ISO-IR-37'               then result := csISO5427Cyrillic      else
  if UName='CSISO5427CYRILLIC'       then result := csISO5427Cyrillic      else

(* Name: JIS_C6226-1978                                    [RFC1345,KXS2]     *)
(* MIBenum: 49                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-42                                                           *)
(* Alias: csISO42JISC62261978                                                 *)

  (* overridden preference for MIME: iso-ir-42                              *)

  if UName='JIS_C6226-1978'          then result := csISO42JISC62261978    else
  if UName='ISO-IR-42'               then result := csISO42JISC62261978    else
  if UName='CSISO42JISC62261978'     then result := csISO42JISC62261978    else

(* Name: BS_viewdata                                       [RFC1345,KXS2]     *)
(* MIBenum: 50                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-47                                                           *)
(* Alias: csISO47BSViewdata                                                   *)

  (* overridden preference for MIME: iso-ir-47                              *)

  if UName='BS_VIEWDATA'             then result := csISO47BSViewdata      else
  if UName='ISO-IR-47'               then result := csISO47BSViewdata      else
  if UName='CSISO47BSVIEWDATA'       then result := csISO47BSViewdata      else

(* Name: INIS                                              [RFC1345,KXS2]     *)
(* MIBenum: 51                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-49                                                           *)
(* Alias: csISO49INIS                                                         *)

  (* overridden preference for MIME: iso-ir-49                              *)

  if UName='INIS'                    then result := csISO49INIS            else
  if UName='ISO-IR-49'               then result := csISO49INIS            else
  if UName='CSISO49INIS'             then result := csISO49INIS            else

(* Name: INIS-8                                            [RFC1345,KXS2]     *)
(* MIBenum: 52                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-50                                                           *)
(* Alias: csISO50INIS8                                                        *)

  (* overridden preference for MIME: iso-ir-50                              *)

  if UName='INIS-8'                  then result := csISO50INIS8           else
  if UName='ISO-IR-50'               then result := csISO50INIS8           else
  if UName='CSISO50INIS8'            then result := csISO50INIS8           else

(* Name: INIS-cyrillic                                     [RFC1345,KXS2]     *)
(* MIBenum: 53                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-51                                                           *)
(* Alias: csISO51INISCyrillic                                                 *)

  (* overridden preference for MIME: iso-ir-51                              *)

  if UName='INIS-CYRILLIC'           then result := csISO51INISCyrillic    else
  if UName='ISO-IR-51'               then result := csISO51INISCyrillic    else
  if UName='CSISO51INISCYRILLIC'     then result := csISO51INISCyrillic    else

(* Name: ISO_5427:1981                                     [RFC1345,KXS2]     *)
(* MIBenum: 54                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-54                                                           *)
(* Alias: ISO5427Cyrillic1981                                                 *)

  (* overridden preference for MIME: iso-ir-54                              *)

  if UName='ISO_5427:1981'           then result := csiso_ir_54            else
  if UName='ISO-IR-54'               then result := csiso_ir_54            else
  if UName='ISO5427CYRILLIC1981'     then result := csiso_ir_54            else

(* Name: ISO_5428:1980                                     [RFC1345,KXS2]     *)
(* MIBenum: 55                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-55                                                           *)
(* Alias: csISO5428Greek                                                      *)

  (* overridden preference for MIME: iso-ir-55                              *)

  if UName='ISO_5428:1980'           then result := csISO5428Greek         else
  if UName='ISO-IR-55'               then result := csISO5428Greek         else
  if UName='CSISO5428GREEK'          then result := csISO5428Greek         else

(* Name: GB_1988-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 56                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-57                                                           *)
(* Alias: cn                                                                  *)
(* Alias: ISO646-CN                                                           *)
(* Alias: csISO57GB1988                                                       *)

  (* overridden preference for MIME: ISO646-CN                              *)

  if UName='GB_1988-80'              then result := csISO57GB1988          else
  if UName='ISO-IR-57'               then result := csISO57GB1988          else
  if UName='CN'                      then result := csISO57GB1988          else
  if UName='ISO646-CN'               then result := csISO57GB1988          else
  if UName='CSISO57GB1988'           then result := csISO57GB1988          else

(* Name: GB_2312-80                                        [RFC1345,KXS2]     *)
(* MIBenum: 57                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-58                                                           *)
(* Alias: chinese                                                             *)
(* Alias: csISO58GB231280                                                     *)

  (* overridden preference for MIME: iso-ir-58                              *)

  if UName='GB_2312-80'              then result := csISO58GB231280        else
  if UName='ISO-IR-58'               then result := csISO58GB231280        else
  if UName='CHINESE'                 then result := csISO58GB231280        else
  if UName='CSISO58GB231280'         then result := csISO58GB231280        else

(* Name: NS_4551-1                                         [RFC1345,KXS2]     *)
(* MIBenum: 25                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-60                                                           *)
(* Alias: ISO646-NO                                                           *)
(* Alias: no                                                                  *)
(* Alias: csISO60DanishNorwegian                                              *)
(* Alias: csISO60Norwegian1                                                   *)

  (* additional alias: NORWEG                                              *)
  (* overridden preference for MIME: ISO646-NO                              *)

  if UName='NS_4551-1'               then result := csISO60DanishNorwegian else
  if UName='ISO-IR-60'               then result := csISO60DanishNorwegian else
  if UName='ISO646-NO'               then result := csISO60DanishNorwegian else
  if UName='NO'                      then result := csISO60DanishNorwegian else
  if UName='CSISO60DANISHNORWEGIAN'  then result := csISO60DanishNorwegian else
  if UName='CSISO60NORWEGIAN1'       then result := csISO60DanishNorwegian else
  if UName='NORWEG'                  then result := csISO60DanishNorwegian else

(* Name: NS_4551-2                                          [RFC1345,KXS2]    *)
(* MIBenum: 58                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO646-NO2                                                          *)
(* Alias: iso-ir-61                                                           *)
(* Alias: no2                                                                 *)
(* Alias: csISO61Norwegian2                                                   *)

  (* overridden preference for MIME: ISO646-NO2                             *)

  if UName='NS_4551-2'               then result := csISO61Norwegian2      else
  if UName='ISO646-NO2'              then result := csISO61Norwegian2      else
  if UName='ISO-IR-61'               then result := csISO61Norwegian2      else
  if UName='NO2'                     then result := csISO61Norwegian2      else
  if UName='CSISO61NORWEGIAN2'       then result := csISO61Norwegian2      else

(* Name: NF_Z_62-010                                        [RFC1345,KXS2]    *)
(* MIBenum: 26                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-69                                                           *)
(* Alias: ISO646-FR                                                           *)
(* Alias: fr                                                                  *)
(* Alias: csISO69French                                                       *)

  (* additional alias: FRENCH                                              *)
  (* overridden preference for MIME: ISO646-FR                              *)

  if UName='NF_Z_62-010'             then result := csISO69French          else
  if UName='ISO-IR-69'               then result := csISO69French          else
  if UName='ISO646-FR'               then result := csISO69French          else
  if UName='FR'                      then result := csISO69French          else
  if UName='CSISO69FRENCH'           then result := csISO69French          else
  if UName='FRENCH'                  then result := csISO69French          else

(* Name: videotex-suppl                                     [RFC1345,KXS2]    *)
(* MIBenum: 59                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-70                                                           *)
(* Alias: csISO70VideotexSupp1                                                *)

  (* overridden preference for MIME: iso-ir-70                              *)

  if UName='VIDEOTEX-SUPPL'          then result := csISO70VideotexSupp1   else
  if UName='ISO-IR-70'               then result := csISO70VideotexSupp1   else
  if UName='CSISO70VIDEOTEXSUPP1'    then result := csISO70VideotexSupp1   else

(* Name: PT2                                                [RFC1345,KXS2]    *)
(* MIBenum: 60                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-84                                                           *)
(* Alias: ISO646-PT2                                                          *)
(* Alias: csISO84Portuguese2                                                  *)

  (* overridden preference for MIME: ISO646-PT2                             *)

  if UName='PT2'                     then result := csISO84Portuguese2     else
  if UName='ISO-IR-84'               then result := csISO84Portuguese2     else
  if UName='ISO646-PT2'              then result := csISO84Portuguese2     else
  if UName='CSISO84PORTUGUESE2'      then result := csISO84Portuguese2     else

(* Name: ES2                                                [RFC1345,KXS2]    *)
(* MIBenum: 61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-85                                                           *)
(* Alias: ISO646-ES2                                                          *)
(* Alias: csISO85Spanish2                                                     *)

  (* overridden preference for MIME: ISO646-ES2                             *)

  if UName='ES2'                     then result := csISO85Spanish2        else
  if UName='ISO-IR-85'               then result := csISO85Spanish2        else
  if UName='ISO646-ES2'              then result := csISO85Spanish2        else
  if UName='CSISO85SPANISH2'         then result := csISO85Spanish2        else

(* Name: MSZ_7795.3                                         [RFC1345,KXS2]    *)
(* MIBenum: 62                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-86                                                           *)
(* Alias: ISO646-HU                                                           *)
(* Alias: hu                                                                  *)
(* Alias: csISO86Hungarian                                                    *)

  (* overridden preference for MIME: ISO646-HU                              *)

  if UName='MSZ_7795.3'              then result := csISO86Hungarian       else
  if UName='ISO-IR-86'               then result := csISO86Hungarian       else
  if UName='ISO646-HU'               then result := csISO86Hungarian       else
  if UName='HU'                      then result := csISO86Hungarian       else
  if UName='CSISO86HUNGARIAN'        then result := csISO86Hungarian       else

(* Name: JIS_C6226-1983                                     [RFC1345,KXS2]    *)
(* MIBenum: 63                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-87                                                           *)
(* Alias: x0208                                                               *)
(* Alias: JIS_X0208-1983                                                      *)
(* Alias: csISO87JISX0208                                                     *)

  (* overridden preference for MIME: iso-ir-87                              *)

  if UName='JIS_C6226-1983'          then result := csISO87JISX0208        else
  if UName='ISO-IR-87'               then result := csISO87JISX0208        else
  if UName='X0208'                   then result := csISO87JISX0208        else
  if UName='JIS_X0208-1983'          then result := csISO87JISX0208        else
  if UName='CSISO87JISX0208'         then result := csISO87JISX0208        else

(* Name: greek7                                             [RFC1345,KXS2]    *)
(* MIBenum: 64                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-88                                                           *)
(* Alias: csISO88Greek7                                                       *)

  (* overridden preference for MIME: iso-ir-88                              *)

  if UName='GREEK7'                  then result := csISO88Greek7          else
  if UName='ISO-IR-88'               then result := csISO88Greek7          else
  if UName='CSISO88GREEK7'           then result := csISO88Greek7          else

(* Name: ASMO_449                                           [RFC1345,KXS2]    *)
(* MIBenum: 65                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ISO_9036                                                            *)
(* Alias: arabic7                                                             *)
(* Alias: iso-ir-89                                                           *)
(* Alias: csISO89ASMO449                                                      *)

  (* overridden preference for MIME: ISO_9036                               *)

  if UName='ASMO_449'                then result := csISO89ASMO449         else
  if UName='ISO_9036'                then result := csISO89ASMO449         else
  if UName='ARABIC7'                 then result := csISO89ASMO449         else
  if UName='ISO-IR-89'               then result := csISO89ASMO449         else
  if UName='CSISO89ASMO449'          then result := csISO89ASMO449         else

(* Name: iso-ir-90                                          [RFC1345,KXS2]    *)
(* MIBenum: 66                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: csISO90                                                             *)

  (* overridden preference for MIME: iso-ir-90                              *)

  if UName='ISO-IR-90'               then result := csISO90                else
  if UName='CSISO90'                 then result := csISO90                else

(* Name: JIS_C6229-1984-a                                   [RFC1345,KXS2]    *)
(* MIBenum: 67                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-91                                                           *)
(* Alias: jp-ocr-a                                                            *)
(* Alias: csISO91JISC62291984a                                                *)

  (* overridden preference for MIME: iso-ir-91                              *)

  if UName='JIS_C6229-1984-A'        then result := csISO91JISC62291984a   else
  if UName='ISO-IR-91'               then result := csISO91JISC62291984a   else
  if UName='JP-OCR-A'                then result := csISO91JISC62291984a   else
  if UName='CSISO91JISC62291984A'    then result := csISO91JISC62291984a   else

(* Name: JIS_C6229-1984-b                                   [RFC1345,KXS2]    *)
(* MIBenum: 68                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-92                                                           *)
(* Alias: ISO646-JP-OCR-B                                                     *)
(* Alias: jp-ocr-b                                                            *)
(* Alias: csISO92JISC62991984b                                                *)

  (* overridden preference for MIME: iso-ir-92                              *)

  if UName='JIS_C6229-1984-B'        then result := csISO92JISC62991984b   else
  if UName='ISO-IR-92'               then result := csISO92JISC62991984b   else
  if UName='ISO646-JP-OCR-B'         then result := csISO92JISC62991984b   else
  if UName='JP-OCR-B'                then result := csISO92JISC62991984b   else
  if UName='CSISO92JISC62991984B'    then result := csISO92JISC62991984b   else

(* Name: JIS_C6229-1984-b-add                               [RFC1345,KXS2]    *)
(* MIBenum: 69                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-93                                                           *)
(* Alias: jp-ocr-b-add                                                        *)
(* Alias: csISO93JIS62291984badd                                              *)

  (* overridden preference for MIME: iso-ir-93                              *)

  if UName='JIS_C6229-1984-B-ADD'    then result := csISO93JIS62291984badd else
  if UName='ISO-IR-93'               then result := csISO93JIS62291984badd else
  if UName='JP-OCR-B-ADD'            then result := csISO93JIS62291984badd else
  if UName='CSISO93JIS62291984BADD'  then result := csISO93JIS62291984badd else

(* Name: JIS_C6229-1984-hand                                [RFC1345,KXS2]    *)
(* MIBenum: 70                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-94                                                           *)
(* Alias: jp-ocr-hand                                                         *)
(* Alias: csISO94JIS62291984hand                                              *)

  (* overridden preference for MIME: iso-ir-94                              *)

  if UName='JIS_C6229-1984-HAND'     then result := csISO94JIS62291984hand else
  if UName='ISO-IR-94'               then result := csISO94JIS62291984hand else
  if UName='JP-OCR-HAND'             then result := csISO94JIS62291984hand else
  if UName='CSISO94JIS62291984HAND'  then result := csISO94JIS62291984hand else

(* Name: JIS_C6229-1984-hand-add                            [RFC1345,KXS2]    *)
(* MIBenum: 71                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-95                                                           *)
(* Alias: jp-ocr-hand-add                                                     *)
(* Alias: csISO95JIS62291984handadd                                           *)

  (* overridden preference for MIME: iso-ir-95                              *)

  if UName='JIS_C6229-1984-HAND-ADD' then result := csISO95JIS62291984handadd else
  if UName='ISO-IR-95'               then result := csISO95JIS62291984handadd else
  if UName='JP-OCR-HAND-ADD'         then result := csISO95JIS62291984handadd else
  if UName='CSISO95JIS62291984HANDADD' then result := csISO95JIS62291984handadd else

(* Name: JIS_C6229-1984-kana                                [RFC1345,KXS2]    *)
(* MIBenum: 72                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-96                                                           *)
(* Alias: csISO96JISC62291984kana                                             *)

  (* overridden preference for MIME: iso-ir-96                              *)

  if UName='JIS_C6229-1984-KANA'     then result := csISO96JISC62291984kana else
  if UName='ISO-IR-96'               then result := csISO96JISC62291984kana else
  if UName='CSISO96JISC62291984KANA' then result := csISO96JISC62291984kana else

(* Name: ISO_2033-1983                                      [RFC1345,KXS2]    *)
(* MIBenum: 73                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-98                                                           *)
(* Alias: e13b                                                                *)
(* Alias: csISO2033                                                           *)

  (* overridden preference for MIME: ISO_2033-1983                          *)

  if UName='ISO_2033-1983'           then result := csISO2033              else
  if UName='ISO-IR-98'               then result := csISO2033              else
  if UName='E13B'                    then result := csISO2033              else
  if UName='CSISO2033'               then result := csISO2033              else

(* Name: ANSI_X3.110-1983                                   [RFC1345,KXS2]    *)
(* MIBenum: 74                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-99                                                           *)
(* Alias: CSA_T500-1983                                                       *)
(* Alias: NAPLPS                                                              *)
(* Alias: csISO99NAPLPS                                                       *)

  (* overridden preference for MIME: iso-ir-99                              *)

  if UName='ANSI_X3.110-1983'        then result := csISO99NAPLPS          else
  if UName='ISO-IR-99'               then result := csISO99NAPLPS          else
  if UName='CSA_T500-1983'           then result := csISO99NAPLPS          else
  if UName='NAPLPS'                  then result := csISO99NAPLPS          else
  if UName='CSISO99NAPLPS'           then result := csISO99NAPLPS          else

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

  (* additional alias: LATIN-1                                             *)
  (* additional alias: ISO1                                                *)

  if UName='ISO_8859-1:1987'         then result := csISOLatin1            else
  if UName='ISO-IR-100'              then result := csISOLatin1            else
  if UName='ISO_8859-1'              then result := csISOLatin1            else
  if UName='ISO-8859-1'              then result := csISOLatin1            else
  if UName='LATIN1'                  then result := csISOLatin1            else
  if UName='L1'                      then result := csISOLatin1            else
  if UName='IBM819'                  then result := csISOLatin1            else
  if UName='CP819'                   then result := csISOLatin1            else
  if UName='CSISOLATIN1'             then result := csISOLatin1            else
  if UName='LATIN-1'                 then result := csISOLatin1            else
  if UName='ISO1'                    then result := csISOLatin1            else

(* Name: ISO_8859-2:1987                                    [RFC1345,KXS2]    *)
(* MIBenum: 5                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-101                                                          *)
(* Alias: ISO_8859-2                                                          *)
(* Alias: ISO-8859-2 (preferred MIME name)                                    *)
(* Alias: latin2                                                              *)
(* Alias: l2                                                                  *)
(* Alias: csISOLatin2                                                         *)

  (* additional alias: LATIN-2                                             *)
  (* additional alias: ISO2                                                *)

  if UName='ISO_8859-2:1987'         then result := csISOLatin2            else
  if UName='ISO-IR-101'              then result := csISOLatin2            else
  if UName='ISO_8859-2'              then result := csISOLatin2            else
  if UName='ISO-8859-2'              then result := csISOLatin2            else
  if UName='LATIN2'                  then result := csISOLatin2            else
  if UName='L2'                      then result := csISOLatin2            else
  if UName='CSISOLATIN2'             then result := csISOLatin2            else
  if UName='LATIN-2'                 then result := csISOLatin2            else
  if UName='ISO2'                    then result := csISOLatin2            else

(* Name: T.61-7bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 75                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-102                                                          *)
(* Alias: csISO102T617bit                                                     *)

  (* overridden preference for MIME: iso-ir-102                             *)

  if UName='T.61-7BIT'               then result := csISO102T617bit        else
  if UName='ISO-IR-102'              then result := csISO102T617bit        else
  if UName='CSISO102T617BIT'         then result := csISO102T617bit        else

(* Name: T.61-8bit                                          [RFC1345,KXS2]    *)
(* MIBenum: 76                                                                *)
(* Alias: T.61                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-103                                                          *)
(* Alias: csISO103T618bit                                                     *)

  (* overridden preference for MIME: iso-ir-103                             *)

  if UName='T.61-8BIT'               then result := csISO103T618bit        else
  if UName='T.61'                    then result := csISO103T618bit        else
  if UName='ISO-IR-103'              then result := csISO103T618bit        else
  if UName='CSISO103T618BIT'         then result := csISO103T618bit        else

(* Name: ISO_8859-3:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 6                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-109                                                          *)
(* Alias: ISO_8859-3                                                          *)
(* Alias: ISO-8859-3 (preferred MIME name)                                    *)
(* Alias: latin3                                                              *)
(* Alias: l3                                                                  *)
(* Alias: csISOLatin3                                                         *)

  (* additional alias: LATIN-3                                             *)
  (* additional alias: ISO3                                                *)

  if UName='ISO_8859-3:1988'         then result := csISOLatin3            else
  if UName='ISO-IR-109'              then result := csISOLatin3            else
  if UName='ISO_8859-3'              then result := csISOLatin3            else
  if UName='ISO-8859-3'              then result := csISOLatin3            else
  if UName='LATIN3'                  then result := csISOLatin3            else
  if UName='L3'                      then result := csISOLatin3            else
  if UName='CSISOLATIN3'             then result := csISOLatin3            else
  if UName='LATIN-3'                 then result := csISOLatin3            else
  if UName='ISO3'                    then result := csISOLatin3            else

(* Name: ISO_8859-4:1988                                    [RFC1345,KXS2]    *)
(* MIBenum: 7                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-110                                                          *)
(* Alias: ISO_8859-4                                                          *)
(* Alias: ISO-8859-4 (preferred MIME name)                                    *)
(* Alias: latin4                                                              *)
(* Alias: l4                                                                  *)
(* Alias: csISOLatin4                                                         *)

  (* additional alias: LATIN-4                                             *)
  (* additional alias: ISO4                                                *)

  if UName='ISO_8859-4:1988'         then result := csISOLatin4            else
  if UName='ISO-IR-110'              then result := csISOLatin4            else
  if UName='ISO_8859-4'              then result := csISOLatin4            else
  if UName='ISO-8859-4'              then result := csISOLatin4            else
  if UName='LATIN4'                  then result := csISOLatin4            else
  if UName='L4'                      then result := csISOLatin4            else
  if UName='CSISOLATIN4'             then result := csISOLatin4            else
  if UName='LATIN-4'                 then result := csISOLatin4            else
  if UName='ISO4'                    then result := csISOLatin4            else

(* Name: ECMA-cyrillic                                      [RFC1345,KXS2]    *)
(* MIBenum: 77                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-111                                                          *)
(* Alias: csISO111ECMACyrillic                                                *)

  (* overridden preference for MIME: iso-ir-111                             *)

  if UName='ECMA-CYRILLIC'           then result := csISO111ECMACyrillic   else
  if UName='ISO-IR-111'              then result := csISO111ECMACyrillic   else
  if UName='CSISO111ECMACYRILLIC'    then result := csISO111ECMACyrillic   else

(* Name: CSA_Z243.4-1985-1                                  [RFC1345,KXS2]    *)
(* MIBenum: 78                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-121                                                          *)
(* Alias: ISO646-CA                                                           *)
(* Alias: csa7-1                                                              *)
(* Alias: ca                                                                  *)
(* Alias: csISO121Canadian1                                                   *)

  (* additional alias: CANADIAN                                            *)
  (* overridden preference for MIME: ISO646-CA                              *)

  if UName='CSA_Z243.4-1985-1'       then result := csa7_1                 else
  if UName='ISO-IR-121'              then result := csa7_1                 else
  if UName='ISO646-CA'               then result := csa7_1                 else
  if UName='CSA7-1'                  then result := csa7_1                 else
  if UName='CA'                      then result := csa7_1                 else
  if UName='CSISO121CANADIAN1'       then result := csa7_1                 else
  if UName='CANADIAN'                then result := csa7_1                 else

(* Name: CSA_Z243.4-1985-2                                  [RFC1345,KXS2]    *)
(* MIBenum: 79                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-122                                                          *)
(* Alias: ISO646-CA2                                                          *)
(* Alias: csa7-2                                                              *)
(* Alias: csISO122Canadian2                                                   *)

  (* overridden preference for MIME: ISO646-CA2                             *)

  if UName='CSA_Z243.4-1985-2'       then result := csa7_2                 else
  if UName='ISO-IR-122'              then result := csa7_2                 else
  if UName='ISO646-CA2'              then result := csa7_2                 else
  if UName='CSA7-2'                  then result := csa7_2                 else
  if UName='CSISO122CANADIAN2'       then result := csa7_2                 else

(* Name: CSA_Z243.4-1985-gr                                 [RFC1345,KXS2]    *)
(* MIBenum: 80                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-123                                                          *)
(* Alias: csISO123CSAZ24341985gr                                              *)

  (* overridden preference for MIME: iso-ir-123                             *)

  if UName='CSA_Z243.4-1985-GR'      then result := csISO123CSAZ24341985gr else
  if UName='ISO-IR-123'              then result := csISO123CSAZ24341985gr else
  if UName='CSISO123CSAZ24341985GR'  then result := csISO123CSAZ24341985gr else

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

  (* additional alias: ISO6                                                *)

  if UName='ISO_8859-6:1987'         then result := csISOLatinArabic       else
  if UName='ISO-IR-127'              then result := csISOLatinArabic       else
  if UName='ISO_8859-6'              then result := csISOLatinArabic       else
  if UName='ISO-8859-6'              then result := csISOLatinArabic       else
  if UName='ECMA-114'                then result := csISOLatinArabic       else
  if UName='ASMO-708'                then result := csISOLatinArabic       else
  if UName='ARABIC'                  then result := csISOLatinArabic       else
  if UName='CSISOLATINARABIC'        then result := csISOLatinArabic       else
  if UName='ISO6'                    then result := csISOLatinArabic       else

(* Name: ISO_8859-6-E                                       [RFC1556,IANA]    *)
(* MIBenum: 81                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596E                                                         *)
(* Alias: ISO-8859-6-E (preferred MIME name)                                  *)

  if UName='ISO_8859-6-E'            then result := csISO88596E            else
  if UName='CSISO88596E'             then result := csISO88596E            else
  if UName='ISO-8859-6-E'            then result := csISO88596E            else

(* Name: ISO_8859-6-I                                       [RFC1556,IANA]    *)
(* MIBenum: 82                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88596I                                                         *)
(* Alias: ISO-8859-6-I (preferred MIME name)                                  *)

  if UName='ISO_8859-6-I'            then result := csISO88596I            else
  if UName='CSISO88596I'             then result := csISO88596I            else
  if UName='ISO-8859-6-I'            then result := csISO88596I            else

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

  (* additional alias: ISO7                                                *)

  if UName='ISO_8859-7:1987'         then result := csISOLatinGreek        else
  if UName='ISO-IR-126'              then result := csISOLatinGreek        else
  if UName='ISO_8859-7'              then result := csISOLatinGreek        else
  if UName='ISO-8859-7'              then result := csISOLatinGreek        else
  if UName='ELOT_928'                then result := csISOLatinGreek        else
  if UName='ECMA-118'                then result := csISOLatinGreek        else
  if UName='GREEK'                   then result := csISOLatinGreek        else
  if UName='GREEK8'                  then result := csISOLatinGreek        else
  if UName='CSISOLATINGREEK'         then result := csISOLatinGreek        else
  if UName='ISO7'                    then result := csISOLatinGreek        else

(* Name: T.101-G2                                            [RFC1345,KXS2]   *)
(* MIBenum: 83                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-128                                                          *)
(* Alias: csISO128T101G2                                                      *)

  (* overridden preference for MIME: iso-ir-128                             *)

  if UName='T.101-G2'                then result := csISO128T101G2         else
  if UName='ISO-IR-128'              then result := csISO128T101G2         else
  if UName='CSISO128T101G2'          then result := csISO128T101G2         else

(* Name: ISO_8859-8:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 11                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-138                                                          *)
(* Alias: ISO_8859-8                                                          *)
(* Alias: ISO-8859-8 (preferred MIME name)                                    *)
(* Alias: hebrew                                                              *)
(* Alias: csISOLatinHebrew                                                    *)

  (* additional alias: ISO8                                                *)

  if UName='ISO_8859-8:1988'         then result := csISOLatinHebrew       else
  if UName='ISO-IR-138'              then result := csISOLatinHebrew       else
  if UName='ISO_8859-8'              then result := csISOLatinHebrew       else
  if UName='ISO-8859-8'              then result := csISOLatinHebrew       else
  if UName='HEBREW'                  then result := csISOLatinHebrew       else
  if UName='CSISOLATINHEBREW'        then result := csISOLatinHebrew       else
  if UName='ISO8'                    then result := csISOLatinHebrew       else

(* Name: ISO_8859-8-E                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 84                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598E                                                         *)
(* Alias: ISO-8859-8-E (preferred MIME name)                                  *)

  if UName='ISO_8859-8-E'            then result := csISO88598E            else
  if UName='CSISO88598E'             then result := csISO88598E            else
  if UName='ISO-8859-8-E'            then result := csISO88598E            else

(* Name: ISO_8859-8-I                                  [RFC1556,Nussbacher]   *)
(* MIBenum: 85                                                                *)
(* Source: RFC1556                                                            *)
(* Alias: csISO88598I                                                         *)
(* Alias: ISO-8859-8-I (preferred MIME name)                                  *)

  if UName='ISO_8859-8-I'            then result := csISO88598I            else
  if UName='CSISO88598I'             then result := csISO88598I            else
  if UName='ISO-8859-8-I'            then result := csISO88598I            else

(* Name: CSN_369103                                          [RFC1345,KXS2]   *)
(* MIBenum: 86                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-139                                                          *)
(* Alias: csISO139CSN369103                                                   *)

  (* overridden preference for MIME: iso-ir-139                             *)

  if UName='CSN_369103'              then result := csISO139CSN369103      else
  if UName='ISO-IR-139'              then result := csISO139CSN369103      else
  if UName='CSISO139CSN369103'       then result := csISO139CSN369103      else

(* Name: JUS_I.B1.002                                        [RFC1345,KXS2]   *)
(* MIBenum: 87                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-141                                                          *)
(* Alias: ISO646-YU                                                           *)
(* Alias: js                                                                  *)
(* Alias: yu                                                                  *)
(* Alias: csISO141JUSIB1002                                                   *)

  (* overridden preference for MIME: ISO646-YU                              *)

  if UName='JUS_I.B1.002'            then result := csISO141JUSIB1002      else
  if UName='ISO-IR-141'              then result := csISO141JUSIB1002      else
  if UName='ISO646-YU'               then result := csISO141JUSIB1002      else
  if UName='JS'                      then result := csISO141JUSIB1002      else
  if UName='YU'                      then result := csISO141JUSIB1002      else
  if UName='CSISO141JUSIB1002'       then result := csISO141JUSIB1002      else

(* Name: ISO_6937-2-add                                      [RFC1345,KXS2]   *)
(* MIBenum: 14                                                                *)
(* Source: ECMA registry and ISO 6937-2:1983                                  *)
(* Alias: iso-ir-142                                                          *)
(* Alias: csISOTextComm                                                       *)

  (* overridden preference for MIME: iso-ir-142                             *)

  if UName='ISO_6937-2-ADD'          then result := csISOTextComm          else
  if UName='ISO-IR-142'              then result := csISOTextComm          else
  if UName='CSISOTEXTCOMM'           then result := csISOTextComm          else

(* Name: IEC_P27-1                                           [RFC1345,KXS2]   *)
(* MIBenum: 88                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-143                                                          *)
(* Alias: csISO143IECP271                                                     *)

  (* overridden preference for MIME: iso-ir-143                             *)

  if UName='IEC_P27-1'               then result := csISO143IECP271        else
  if UName='ISO-IR-143'              then result := csISO143IECP271        else
  if UName='CSISO143IECP271'         then result := csISO143IECP271        else

(* Name: ISO_8859-5:1988                                     [RFC1345,KXS2]   *)
(* MIBenum: 8                                                                 *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-144                                                          *)
(* Alias: ISO_8859-5                                                          *)
(* Alias: ISO-8859-5 (preferred MIME name)                                    *)
(* Alias: cyrillic                                                            *)
(* Alias: csISOLatinCyrillic                                                  *)

  (* additional alias: ISO5                                                *)

  if UName='ISO_8859-5:1988'         then result := csISOLatinCyrillic     else
  if UName='ISO-IR-144'              then result := csISOLatinCyrillic     else
  if UName='ISO_8859-5'              then result := csISOLatinCyrillic     else
  if UName='ISO-8859-5'              then result := csISOLatinCyrillic     else
  if UName='CYRILLIC'                then result := csISOLatinCyrillic     else
  if UName='CSISOLATINCYRILLIC'      then result := csISOLatinCyrillic     else
  if UName='ISO5'                    then result := csISOLatinCyrillic     else

(* Name: JUS_I.B1.003-serb                                   [RFC1345,KXS2]   *)
(* MIBenum: 89                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-146                                                          *)
(* Alias: serbian                                                             *)
(* Alias: csISO146Serbian                                                     *)

  (* overridden preference for MIME: iso-ir-146                             *)

  if UName='JUS_I.B1.003-SERB'       then result := csISO146Serbian        else
  if UName='ISO-IR-146'              then result := csISO146Serbian        else
  if UName='SERBIAN'                 then result := csISO146Serbian        else
  if UName='CSISO146SERBIAN'         then result := csISO146Serbian        else

(* Name: JUS_I.B1.003-mac                                    [RFC1345,KXS2]   *)
(* MIBenum: 90                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: macedonian                                                          *)
(* Alias: iso-ir-147                                                          *)
(* Alias: csISO147Macedonian                                                  *)

  (* overridden preference for MIME: iso-ir-147                             *)

  if UName='JUS_I.B1.003-MAC'        then result := csISO147Macedonian     else
  if UName='MACEDONIAN'              then result := csISO147Macedonian     else
  if UName='ISO-IR-147'              then result := csISO147Macedonian     else
  if UName='CSISO147MACEDONIAN'      then result := csISO147Macedonian     else

(* Name: ISO_8859-9:1989                                     [RFC1345,KXS2]   *)
(* MIBenum: 12                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-148                                                          *)
(* Alias: ISO_8859-9                                                          *)
(* Alias: ISO-8859-9 (preferred MIME name)                                    *)
(* Alias: latin5                                                              *)
(* Alias: l5                                                                  *)
(* Alias: csISOLatin5                                                         *)

  (* additional alias: ISO9                                                *)
  (* additional alias: LATIN-5                                             *)

  if UName='ISO_8859-9:1989'         then result := csISOLatin5            else
  if UName='ISO-IR-148'              then result := csISOLatin5            else
  if UName='ISO_8859-9'              then result := csISOLatin5            else
  if UName='ISO-8859-9'              then result := csISOLatin5            else
  if UName='LATIN5'                  then result := csISOLatin5            else
  if UName='L5'                      then result := csISOLatin5            else
  if UName='CSISOLATIN5'             then result := csISOLatin5            else
  if UName='ISO9'                    then result := csISOLatin5            else
  if UName='LATIN-5'                 then result := csISOLatin5            else

(* Name: greek-ccitt                                         [RFC1345,KXS2]   *)
(* MIBenum: 91                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-150                                                          *)
(* Alias: csISO150                                                            *)
(* Alias: csISO150GreekCCITT                                                  *)

  (* overridden preference for MIME: iso-ir-150                             *)

  if UName='GREEK-CCITT'             then result := csISO150               else
  if UName='ISO-IR-150'              then result := csISO150               else
  if UName='CSISO150'                then result := csISO150               else
  if UName='CSISO150GREEKCCITT'      then result := csISO150               else

(* Name: NC_NC00-10:81                                       [RFC1345,KXS2]   *)
(* MIBenum: 92                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: cuba                                                                *)
(* Alias: iso-ir-151                                                          *)
(* Alias: ISO646-CU                                                           *)
(* Alias: csISO151Cuba                                                        *)

  (* overridden preference for MIME: ISO646-CU                              *)

  if UName='NC_NC00-10:81'           then result := csISO151Cuba           else
  if UName='CUBA'                    then result := csISO151Cuba           else
  if UName='ISO-IR-151'              then result := csISO151Cuba           else
  if UName='ISO646-CU'               then result := csISO151Cuba           else
  if UName='CSISO151CUBA'            then result := csISO151Cuba           else

(* Name: ISO_6937-2-25                                       [RFC1345,KXS2]   *)
(* MIBenum: 93                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-152                                                          *)
(* Alias: csISO6937Add                                                        *)

  (* overridden preference for MIME: iso-ir-152                             *)

  if UName='ISO_6937-2-25'           then result := csISO6937Add           else
  if UName='ISO-IR-152'              then result := csISO6937Add           else
  if UName='CSISO6937ADD'            then result := csISO6937Add           else

(* Name: GOST_19768-74                                       [RFC1345,KXS2]   *)
(* MIBenum: 94                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: ST_SEV_358-88                                                       *)
(* Alias: iso-ir-153                                                          *)
(* Alias: csISO153GOST1976874                                                 *)

  (* overridden preference for MIME: iso-ir-153                             *)

  if UName='GOST_19768-74'           then result := csISO153GOST1976874    else
  if UName='ST_SEV_358-88'           then result := csISO153GOST1976874    else
  if UName='ISO-IR-153'              then result := csISO153GOST1976874    else
  if UName='CSISO153GOST1976874'     then result := csISO153GOST1976874    else

(* Name: ISO_8859-supp                                       [RFC1345,KXS2]   *)
(* MIBenum: 95                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-154                                                          *)
(* Alias: latin1-2-5                                                          *)
(* Alias: csISO8859Supp                                                       *)

  (* overridden preference for MIME: iso-ir-154                             *)

  if UName='ISO_8859-SUPP'           then result := csISO8859Supp          else
  if UName='ISO-IR-154'              then result := csISO8859Supp          else
  if UName='LATIN1-2-5'              then result := csISO8859Supp          else
  if UName='CSISO8859SUPP'           then result := csISO8859Supp          else

(* Name: ISO_10367-box                                       [RFC1345,KXS2]   *)
(* MIBenum: 96                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-155                                                          *)
(* Alias: csISO10367Box                                                       *)

  (* overridden preference for MIME: iso-ir-155                             *)

  if UName='ISO_10367-BOX'           then result := csISO10367Box          else
  if UName='ISO-IR-155'              then result := csISO10367Box          else
  if UName='CSISO10367BOX'           then result := csISO10367Box          else

(* Name: ISO-8859-10 (preferred MIME name)                   [RFC1345,KXS2]   *)
(* MIBenum: 13                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: iso-ir-157                                                          *)
(* Alias: l6                                                                  *)
(* Alias: ISO_8859-10:1992                                                    *)
(* Alias: csISOLatin6                                                         *)
(* Alias: latin6                                                              *)

  (* additional alias: ISO10                                               *)
  (* additional alias: LATIN-6                                             *)
  (* overridden preference for MIME: ISO-8859-10                            *)

  if UName='ISO-8859-10'             then result := csISOLatin6            else
  if UName='ISO-IR-157'              then result := csISOLatin6            else
  if UName='L6'                      then result := csISOLatin6            else
  if UName='ISO_8859-10:1992'        then result := csISOLatin6            else
  if UName='CSISOLATIN6'             then result := csISOLatin6            else
  if UName='LATIN6'                  then result := csISOLatin6            else
  if UName='ISO10'                   then result := csISOLatin6            else
  if UName='LATIN-6'                 then result := csISOLatin6            else

(* Name: latin-lap                                           [RFC1345,KXS2]   *)
(* MIBenum: 97                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: lap                                                                 *)
(* Alias: iso-ir-158                                                          *)
(* Alias: csISO158Lap                                                         *)

  (* overridden preference for MIME: iso-ir-158                             *)

  if UName='LATIN-LAP'               then result := csISO158Lap            else
  if UName='LAP'                     then result := csISO158Lap            else
  if UName='ISO-IR-158'              then result := csISO158Lap            else
  if UName='CSISO158LAP'             then result := csISO158Lap            else

(* Name: JIS_X0212-1990                                      [RFC1345,KXS2]   *)
(* MIBenum: 98                                                                *)
(* Source: ECMA registry                                                      *)
(* Alias: x0212                                                               *)
(* Alias: iso-ir-159                                                          *)
(* Alias: csISO159JISX02121990                                                *)

  (* overridden preference for MIME: iso-ir-159                             *)

  if UName='JIS_X0212-1990'          then result := csISO159JISX02121990   else
  if UName='X0212'                   then result := csISO159JISX02121990   else
  if UName='ISO-IR-159'              then result := csISO159JISX02121990   else
  if UName='CSISO159JISX02121990'    then result := csISO159JISX02121990   else

(* Name: DS_2089                                             [RFC1345,KXS2]   *)
(* MIBenum: 99                                                                *)
(* Source: Danish Standard, DS 2089, February 1974                            *)
(* Alias: DS2089                                                              *)
(* Alias: ISO646-DK                                                           *)
(* Alias: dk                                                                  *)
(* Alias: csISO646Danish                                                      *)

  (* overridden preference for MIME: ISO646-DK                              *)

  if UName='DS_2089'                 then result := csISO646Danish         else
  if UName='DS2089'                  then result := csISO646Danish         else
  if UName='ISO646-DK'               then result := csISO646Danish         else
  if UName='DK'                      then result := csISO646Danish         else
  if UName='CSISO646DANISH'          then result := csISO646Danish         else

(* Name: us-dk                                               [RFC1345,KXS2]   *)
(* MIBenum: 100                                                               *)
(* Alias: csUSDK                                                              *)

  if UName='US-DK'                   then result := csUSDK                 else
  if UName='CSUSDK'                  then result := csUSDK                 else

(* Name: dk-us                                               [RFC1345,KXS2]   *)
(* MIBenum: 101                                                               *)
(* Alias: csDKUS                                                              *)

  if UName='DK-US'                   then result := csDKUS                 else
  if UName='CSDKUS'                  then result := csDKUS                 else

(* Name: JIS_X0201                                           [RFC1345,KXS2]   *)
(* MIBenum: 15                                                                *)
(* Source: JIS X 0201-1976.   One byte only, this is equivalent to            *)
(*         JIS/Roman (similar to ASCII) plus eight-bit half-width             *)
(*         Katakana                                                           *)
(* Alias: X0201                                                               *)
(* Alias: csHalfWidthKatakana                                                 *)

  (* additional alias: KATAKANA                                            *)

  if UName='JIS_X0201'               then result := csHalfWidthKatakana    else
  if UName='X0201'                   then result := csHalfWidthKatakana    else
  if UName='CSHALFWIDTHKATAKANA'     then result := csHalfWidthKatakana    else
  if UName='KATAKANA'                then result := csHalfWidthKatakana    else

(* Name: KSC5636                                             [RFC1345,KXS2]   *)
(* MIBenum: 102                                                               *)
(* Alias: ISO646-KR                                                           *)
(* Alias: csKSC5636                                                           *)

  (* overridden preference for MIME: ISO646-KR                              *)

  if UName='KSC5636'                 then result := csKSC5636              else
  if UName='ISO646-KR'               then result := csKSC5636              else
  if UName='CSKSC5636'               then result := csKSC5636              else

(* Name: ISO-10646-UCS-2                                                      *)
(* MIBenum: 1000                                                              *)
(* Source: the 2-octet Basic Multilingual Plane, aka Unicode                  *)
(*         this needs to specify network byte order: the standard             *)
(*         does not specify (it is a 16-bit integer space)                    *)
(* Alias: csUnicode                                                           *)

  if UName='ISO-10646-UCS-2'         then result := csUnicode              else
  if UName='CSUNICODE'               then result := csUnicode              else

(* Name: ISO-10646-UCS-4                                                      *)
(* MIBenum: 1001                                                              *)
(* Source: the full code space. (same comment about byte order,               *)
(*         these are 31-bit numbers.                                          *)
(* Alias: csUCS4                                                              *)

  if UName='ISO-10646-UCS-4'         then result := csUCS4                 else
  if UName='CSUCS4'                  then result := csUCS4                 else

(* Name: DEC-MCS                                             [RFC1345,KXS2]   *)
(* MIBenum: 2008                                                              *)
(* Source: VAX/VMS User's Manual,                                             *)
(*         Order Number: AI-Y517A-TE, April 1986.                             *)
(* Alias: dec                                                                 *)
(* Alias: csDECMCS                                                            *)

  if UName='DEC-MCS'                 then result := csDECMCS               else
  if UName='DEC'                     then result := csDECMCS               else
  if UName='CSDECMCS'                then result := csDECMCS               else

(* Name: hp-roman8                                  [HP-PCL5,RFC1345,KXS2]    *)
(* MIBenum: 2004                                                              *)
(* Source: LaserJet IIP Printer User's Manual,                                *)
(*         HP part no 33471-90901, Hewlet-Packard, June 1989.                 *)
(* Alias: roman8                                                              *)
(* Alias: r8                                                                  *)
(* Alias: csHPRoman8                                                          *)

  if UName='HP-ROMAN8'               then result := csHPRoman8             else
  if UName='ROMAN8'                  then result := csHPRoman8             else
  if UName='R8'                      then result := csHPRoman8             else
  if UName='CSHPROMAN8'              then result := csHPRoman8             else

(* Name: macintosh                                           [RFC1345,KXS2]   *)
(* MIBenum: 2027                                                              *)
(* Source: The Unicode Standard ver1.0, ISBN 0-201-56788-1, Oct 1991          *)
(* Alias: mac                                                                 *)
(* Alias: csMacintosh                                                         *)

  (* additional alias: X-MAC-ROMAN                                         *)

  if UName='MACINTOSH'               then result := csMacintosh            else
  if UName='MAC'                     then result := csMacintosh            else
  if UName='CSMACINTOSH'             then result := csMacintosh            else
  if UName='X-MAC-ROMAN'             then result := csMacintosh            else

(* Name: IBM037                                              [RFC1345,KXS2]   *)
(* MIBenum: 2028                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp037                                                               *)
(* Alias: ebcdic-cp-us                                                        *)
(* Alias: ebcdic-cp-ca                                                        *)
(* Alias: ebcdic-cp-wt                                                        *)
(* Alias: ebcdic-cp-nl                                                        *)
(* Alias: csIBM037                                                            *)

  if UName='IBM037'                  then result := csIBM037               else
  if UName='CP037'                   then result := csIBM037               else
  if UName='EBCDIC-CP-US'            then result := csIBM037               else
  if UName='EBCDIC-CP-CA'            then result := csIBM037               else
  if UName='EBCDIC-CP-WT'            then result := csIBM037               else
  if UName='EBCDIC-CP-NL'            then result := csIBM037               else
  if UName='CSIBM037'                then result := csIBM037               else

(* Name: IBM038                                              [RFC1345,KXS2]   *)
(* MIBenum: 2029                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-INT                                                          *)
(* Alias: cp038                                                               *)
(* Alias: csIBM038                                                            *)

  if UName='IBM038'                  then result := csIBM038               else
  if UName='EBCDIC-INT'              then result := csIBM038               else
  if UName='CP038'                   then result := csIBM038               else
  if UName='CSIBM038'                then result := csIBM038               else

(* Name: IBM273                                              [RFC1345,KXS2]   *)
(* MIBenum: 2030                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP273                                                               *)
(* Alias: csIBM273                                                            *)

  if UName='IBM273'                  then result := csIBM273               else
  if UName='CP273'                   then result := csIBM273               else
  if UName='CSIBM273'                then result := csIBM273               else

(* Name: IBM274                                              [RFC1345,KXS2]   *)
(* MIBenum: 2031                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-BE                                                           *)
(* Alias: CP274                                                               *)
(* Alias: csIBM274                                                            *)

  if UName='IBM274'                  then result := csIBM274               else
  if UName='EBCDIC-BE'               then result := csIBM274               else
  if UName='CP274'                   then result := csIBM274               else
  if UName='CSIBM274'                then result := csIBM274               else

(* Name: IBM275                                              [RFC1345,KXS2]   *)
(* MIBenum: 2032                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-BR                                                           *)
(* Alias: cp275                                                               *)
(* Alias: csIBM275                                                            *)

  if UName='IBM275'                  then result := csIBM275               else
  if UName='EBCDIC-BR'               then result := csIBM275               else
  if UName='CP275'                   then result := csIBM275               else
  if UName='CSIBM275'                then result := csIBM275               else

(* Name: IBM277                                              [RFC1345,KXS2]   *)
(* MIBenum: 2033                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: EBCDIC-CP-DK                                                        *)
(* Alias: EBCDIC-CP-NO                                                        *)
(* Alias: csIBM277                                                            *)

  if UName='IBM277'                  then result := csIBM277               else
  if UName='EBCDIC-CP-DK'            then result := csIBM277               else
  if UName='EBCDIC-CP-NO'            then result := csIBM277               else
  if UName='CSIBM277'                then result := csIBM277               else

(* Name: IBM278                                              [RFC1345,KXS2]   *)
(* MIBenum: 2034                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP278                                                               *)
(* Alias: ebcdic-cp-fi                                                        *)
(* Alias: ebcdic-cp-se                                                        *)
(* Alias: csIBM278                                                            *)

  if UName='IBM278'                  then result := csIBM278               else
  if UName='CP278'                   then result := csIBM278               else
  if UName='EBCDIC-CP-FI'            then result := csIBM278               else
  if UName='EBCDIC-CP-SE'            then result := csIBM278               else
  if UName='CSIBM278'                then result := csIBM278               else

(* Name: IBM280                                              [RFC1345,KXS2]   *)
(* MIBenum: 2035                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP280                                                               *)
(* Alias: ebcdic-cp-it                                                        *)
(* Alias: csIBM280                                                            *)

  if UName='IBM280'                  then result := csIBM280               else
  if UName='CP280'                   then result := csIBM280               else
  if UName='EBCDIC-CP-IT'            then result := csIBM280               else
  if UName='CSIBM280'                then result := csIBM280               else

(* Name: IBM281                                              [RFC1345,KXS2]   *)
(* MIBenum: 2036                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: EBCDIC-JP-E                                                         *)
(* Alias: cp281                                                               *)
(* Alias: csIBM281                                                            *)

  if UName='IBM281'                  then result := csIBM281               else
  if UName='EBCDIC-JP-E'             then result := csIBM281               else
  if UName='CP281'                   then result := csIBM281               else
  if UName='CSIBM281'                then result := csIBM281               else

(* Name: IBM284                                              [RFC1345,KXS2]   *)
(* MIBenum: 2037                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP284                                                               *)
(* Alias: ebcdic-cp-es                                                        *)
(* Alias: csIBM284                                                            *)

  if UName='IBM284'                  then result := csIBM284               else
  if UName='CP284'                   then result := csIBM284               else
  if UName='EBCDIC-CP-ES'            then result := csIBM284               else
  if UName='CSIBM284'                then result := csIBM284               else

(* Name: IBM285                                              [RFC1345,KXS2]   *)
(* MIBenum: 2038                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP285                                                               *)
(* Alias: ebcdic-cp-gb                                                        *)
(* Alias: csIBM285                                                            *)

  if UName='IBM285'                  then result := csIBM285               else
  if UName='CP285'                   then result := csIBM285               else
  if UName='EBCDIC-CP-GB'            then result := csIBM285               else
  if UName='CSIBM285'                then result := csIBM285               else

(* Name: IBM290                                              [RFC1345,KXS2]   *)
(* MIBenum: 2039                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: cp290                                                               *)
(* Alias: EBCDIC-JP-kana                                                      *)
(* Alias: csIBM290                                                            *)

  if UName='IBM290'                  then result := csIBM290               else
  if UName='CP290'                   then result := csIBM290               else
  if UName='EBCDIC-JP-KANA'          then result := csIBM290               else
  if UName='CSIBM290'                then result := csIBM290               else

(* Name: IBM297                                              [RFC1345,KXS2]   *)
(* MIBenum: 2040                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp297                                                               *)
(* Alias: ebcdic-cp-fr                                                        *)
(* Alias: csIBM297                                                            *)

  if UName='IBM297'                  then result := csIBM297               else
  if UName='CP297'                   then result := csIBM297               else
  if UName='EBCDIC-CP-FR'            then result := csIBM297               else
  if UName='CSIBM297'                then result := csIBM297               else

(* Name: IBM420                                              [RFC1345,KXS2]   *)
(* MIBenum: 2041                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990,                          *)
(*         IBM NLS RM p 11-11                                                 *)
(* Alias: cp420                                                               *)
(* Alias: ebcdic-cp-ar1                                                       *)
(* Alias: csIBM420                                                            *)

  if UName='IBM420'                  then result := csIBM420               else
  if UName='CP420'                   then result := csIBM420               else
  if UName='EBCDIC-CP-AR1'           then result := csIBM420               else
  if UName='CSIBM420'                then result := csIBM420               else

(* Name: IBM423                                              [RFC1345,KXS2]   *)
(* MIBenum: 2042                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp423                                                               *)
(* Alias: ebcdic-cp-gr                                                        *)
(* Alias: csIBM423                                                            *)

  if UName='IBM423'                  then result := csIBM423               else
  if UName='CP423'                   then result := csIBM423               else
  if UName='EBCDIC-CP-GR'            then result := csIBM423               else
  if UName='CSIBM423'                then result := csIBM423               else

(* Name: IBM424                                              [RFC1345,KXS2]   *)
(* MIBenum: 2043                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp424                                                               *)
(* Alias: ebcdic-cp-he                                                        *)
(* Alias: csIBM424                                                            *)

  if UName='IBM424'                  then result := csIBM424               else
  if UName='CP424'                   then result := csIBM424               else
  if UName='EBCDIC-CP-HE'            then result := csIBM424               else
  if UName='CSIBM424'                then result := csIBM424               else

(* Name: IBM437                                              [RFC1345,KXS2]   *)
(* MIBenum: 2011                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp437                                                               *)
(* Alias: 437                                                                 *)
(* Alias: csPC8CodePage437                                                    *)

  (* additional alias: IBMPC                                               *)

  if UName='IBM437'                  then result := csPC8CodePage437       else
  if UName='CP437'                   then result := csPC8CodePage437       else
  if UName='437'                     then result := csPC8CodePage437       else
  if UName='CSPC8CODEPAGE437'        then result := csPC8CodePage437       else
  if UName='IBMPC'                   then result := csPC8CodePage437       else

(* Name: IBM500                                              [RFC1345,KXS2]   *)
(* MIBenum: 2044                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP500                                                               *)
(* Alias: ebcdic-cp-be                                                        *)
(* Alias: ebcdic-cp-ch                                                        *)
(* Alias: csIBM500                                                            *)

  if UName='IBM500'                  then result := csIBM500               else
  if UName='CP500'                   then result := csIBM500               else
  if UName='EBCDIC-CP-BE'            then result := csIBM500               else
  if UName='EBCDIC-CP-CH'            then result := csIBM500               else
  if UName='CSIBM500'                then result := csIBM500               else

(* Name: IBM775                                                   [HP-PCL5]   *)
(* MIBenum: 2087                                                              *)
(* Source: HP PCL 5 Comparison Guide (P/N 5021-0329) pp B-13, 1996            *)
(* Alias: cp775                                                               *)
(* Alias: csPC775Baltic                                                       *)

  if UName='IBM775'                  then result := csPC775Baltic          else
  if UName='CP775'                   then result := csPC775Baltic          else
  if UName='CSPC775BALTIC'           then result := csPC775Baltic          else

(* Name: IBM850                                              [RFC1345,KXS2]   *)
(* MIBenum: 2009                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp850                                                               *)
(* Alias: 850                                                                 *)
(* Alias: csPC850Multilingual                                                 *)

  if UName='IBM850'                  then result := csPC850Multilingual    else
  if UName='CP850'                   then result := csPC850Multilingual    else
  if UName='850'                     then result := csPC850Multilingual    else
  if UName='CSPC850MULTILINGUAL'     then result := csPC850Multilingual    else

(* Name: IBM851                                              [RFC1345,KXS2]   *)
(* MIBenum: 2045                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp851                                                               *)
(* Alias: 851                                                                 *)
(* Alias: csIBM851                                                            *)

  if UName='IBM851'                  then result := csIBM851               else
  if UName='CP851'                   then result := csIBM851               else
  if UName='851'                     then result := csIBM851               else
  if UName='CSIBM851'                then result := csIBM851               else

(* Name: IBM852                                              [RFC1345,KXS2]   *)
(* MIBenum: 2010                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp852                                                               *)
(* Alias: 852                                                                 *)
(* Alias: csPCp852                                                            *)

  if UName='IBM852'                  then result := csPCp852               else
  if UName='CP852'                   then result := csPCp852               else
  if UName='852'                     then result := csPCp852               else
  if UName='CSPCP852'                then result := csPCp852               else

(* Name: IBM855                                              [RFC1345,KXS2]   *)
(* MIBenum: 2046                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp855                                                               *)
(* Alias: 855                                                                 *)
(* Alias: csIBM855                                                            *)

  if UName='IBM855'                  then result := csIBM855               else
  if UName='CP855'                   then result := csIBM855               else
  if UName='855'                     then result := csIBM855               else
  if UName='CSIBM855'                then result := csIBM855               else

(* Name: IBM857                                              [RFC1345,KXS2]   *)
(* MIBenum: 2047                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp857                                                               *)
(* Alias: 857                                                                 *)
(* Alias: csIBM857                                                            *)

  if UName='IBM857'                  then result := csIBM857               else
  if UName='CP857'                   then result := csIBM857               else
  if UName='857'                     then result := csIBM857               else
  if UName='CSIBM857'                then result := csIBM857               else

(* Name: IBM860                                              [RFC1345,KXS2]   *)
(* MIBenum: 2048                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp860                                                               *)
(* Alias: 860                                                                 *)
(* Alias: csIBM860                                                            *)

  if UName='IBM860'                  then result := csIBM860               else
  if UName='CP860'                   then result := csIBM860               else
  if UName='860'                     then result := csIBM860               else
  if UName='CSIBM860'                then result := csIBM860               else

(* Name: IBM861                                              [RFC1345,KXS2]   *)
(* MIBenum: 2049                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp861                                                               *)
(* Alias: 861                                                                 *)
(* Alias: cp-is                                                               *)
(* Alias: csIBM861                                                            *)

  if UName='IBM861'                  then result := csIBM861               else
  if UName='CP861'                   then result := csIBM861               else
  if UName='861'                     then result := csIBM861               else
  if UName='CP-IS'                   then result := csIBM861               else
  if UName='CSIBM861'                then result := csIBM861               else

(* Name: IBM862                                              [RFC1345,KXS2]   *)
(* MIBenum: 2013                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp862                                                               *)
(* Alias: 862                                                                 *)
(* Alias: csPC862LatinHebrew                                                  *)

  if UName='IBM862'                  then result := csPC862LatinHebrew     else
  if UName='CP862'                   then result := csPC862LatinHebrew     else
  if UName='862'                     then result := csPC862LatinHebrew     else
  if UName='CSPC862LATINHEBREW'      then result := csPC862LatinHebrew     else

(* Name: IBM863                                              [RFC1345,KXS2]   *)
(* MIBenum: 2050                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp863                                                               *)
(* Alias: 863                                                                 *)
(* Alias: csIBM863                                                            *)

  if UName='IBM863'                  then result := csIBM863               else
  if UName='CP863'                   then result := csIBM863               else
  if UName='863'                     then result := csIBM863               else
  if UName='CSIBM863'                then result := csIBM863               else

(* Name: IBM864                                              [RFC1345,KXS2]   *)
(* MIBenum: 2051                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp864                                                               *)
(* Alias: csIBM864                                                            *)

  if UName='IBM864'                  then result := csIBM864               else
  if UName='CP864'                   then result := csIBM864               else
  if UName='CSIBM864'                then result := csIBM864               else

(* Name: IBM865                                              [RFC1345,KXS2]   *)
(* MIBenum: 2052                                                              *)
(* Source: IBM DOS 3.3 Ref (Abridged), 94X9575 (Feb 1987)                     *)
(* Alias: cp865                                                               *)
(* Alias: 865                                                                 *)
(* Alias: csIBM865                                                            *)

  if UName='IBM865'                  then result := csIBM865               else
  if UName='CP865'                   then result := csIBM865               else
  if UName='865'                     then result := csIBM865               else
  if UName='CSIBM865'                then result := csIBM865               else

(* Name: IBM866                                                     [Pond]    *)
(* MIBenum: 2086                                                              *)
(* Source: IBM NLDG Volume 2 (SE09-8002-03) August 1994                       *)
(* Alias: cp866                                                               *)
(* Alias: 866                                                                 *)
(* Alias: csIBM866                                                            *)

  if UName='IBM866'                  then result := csIBM866               else
  if UName='CP866'                   then result := csIBM866               else
  if UName='866'                     then result := csIBM866               else
  if UName='CSIBM866'                then result := csIBM866               else

(* Name: IBM868                                              [RFC1345,KXS2]   *)
(* MIBenum: 2053                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP868                                                               *)
(* Alias: cp-ar                                                               *)
(* Alias: csIBM868                                                            *)

  if UName='IBM868'                  then result := csIBM868               else
  if UName='CP868'                   then result := csIBM868               else
  if UName='CP-AR'                   then result := csIBM868               else
  if UName='CSIBM868'                then result := csIBM868               else

(* Name: IBM869                                              [RFC1345,KXS2]   *)
(* MIBenum: 2054                                                              *)
(* Source: IBM Keyboard layouts and code pages, PN 07G4586 June 1991          *)
(* Alias: cp869                                                               *)
(* Alias: 869                                                                 *)
(* Alias: cp-gr                                                               *)
(* Alias: csIBM869                                                            *)

  if UName='IBM869'                  then result := csIBM869               else
  if UName='CP869'                   then result := csIBM869               else
  if UName='869'                     then result := csIBM869               else
  if UName='CP-GR'                   then result := csIBM869               else
  if UName='CSIBM869'                then result := csIBM869               else

(* Name: IBM870                                              [RFC1345,KXS2]   *)
(* MIBenum: 2055                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP870                                                               *)
(* Alias: ebcdic-cp-roece                                                     *)
(* Alias: ebcdic-cp-yu                                                        *)
(* Alias: csIBM870                                                            *)

  if UName='IBM870'                  then result := csIBM870               else
  if UName='CP870'                   then result := csIBM870               else
  if UName='EBCDIC-CP-ROECE'         then result := csIBM870               else
  if UName='EBCDIC-CP-YU'            then result := csIBM870               else
  if UName='CSIBM870'                then result := csIBM870               else

(* Name: IBM871                                              [RFC1345,KXS2]   *)
(* MIBenum: 2056                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP871                                                               *)
(* Alias: ebcdic-cp-is                                                        *)
(* Alias: csIBM871                                                            *)

  if UName='IBM871'                  then result := csIBM871               else
  if UName='CP871'                   then result := csIBM871               else
  if UName='EBCDIC-CP-IS'            then result := csIBM871               else
  if UName='CSIBM871'                then result := csIBM871               else

(* Name: IBM880                                              [RFC1345,KXS2]   *)
(* MIBenum: 2057                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp880                                                               *)
(* Alias: EBCDIC-Cyrillic                                                     *)
(* Alias: csIBM880                                                            *)

  if UName='IBM880'                  then result := csIBM880               else
  if UName='CP880'                   then result := csIBM880               else
  if UName='EBCDIC-CYRILLIC'         then result := csIBM880               else
  if UName='CSIBM880'                then result := csIBM880               else

(* Name: IBM891                                              [RFC1345,KXS2]   *)
(* MIBenum: 2058                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp891                                                               *)
(* Alias: csIBM891                                                            *)

  if UName='IBM891'                  then result := csIBM891               else
  if UName='CP891'                   then result := csIBM891               else
  if UName='CSIBM891'                then result := csIBM891               else

(* Name: IBM903                                              [RFC1345,KXS2]   *)
(* MIBenum: 2059                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp903                                                               *)
(* Alias: csIBM903                                                            *)

  if UName='IBM903'                  then result := csIBM903               else
  if UName='CP903'                   then result := csIBM903               else
  if UName='CSIBM903'                then result := csIBM903               else

(* Name: IBM904                                              [RFC1345,KXS2]   *)
(* MIBenum: 2060                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: cp904                                                               *)
(* Alias: 904                                                                 *)
(* Alias: csIBBM904                                                           *)

  if UName='IBM904'                  then result := csIBBM904              else
  if UName='CP904'                   then result := csIBBM904              else
  if UName='904'                     then result := csIBBM904              else
  if UName='CSIBBM904'               then result := csIBBM904              else

(* Name: IBM905                                              [RFC1345,KXS2]   *)
(* MIBenum: 2061                                                              *)
(* Source: IBM 3174 Character Set Ref, GA27-3831-02, March 1990               *)
(* Alias: CP905                                                               *)
(* Alias: ebcdic-cp-tr                                                        *)
(* Alias: csIBM905                                                            *)

  if UName='IBM905'                  then result := csIBM905               else
  if UName='CP905'                   then result := csIBM905               else
  if UName='EBCDIC-CP-TR'            then result := csIBM905               else
  if UName='CSIBM905'                then result := csIBM905               else

(* Name: IBM918                                              [RFC1345,KXS2]   *)
(* MIBenum: 2062                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP918                                                               *)
(* Alias: ebcdic-cp-ar2                                                       *)
(* Alias: csIBM918                                                            *)

  if UName='IBM918'                  then result := csIBM918               else
  if UName='CP918'                   then result := csIBM918               else
  if UName='EBCDIC-CP-AR2'           then result := csIBM918               else
  if UName='CSIBM918'                then result := csIBM918               else

(* Name: IBM1026                                             [RFC1345,KXS2]   *)
(* MIBenum: 2063                                                              *)
(* Source: IBM NLS RM Vol2 SE09-8002-01, March 1990                           *)
(* Alias: CP1026                                                              *)
(* Alias: csIBM1026                                                           *)

  if UName='IBM1026'                 then result := csIBM1026              else
  if UName='CP1026'                  then result := csIBM1026              else
  if UName='CSIBM1026'               then result := csIBM1026              else

(* Name: EBCDIC-AT-DE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2064                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csIBMEBCDICATDE                                                     *)

  if UName='EBCDIC-AT-DE'            then result := csIBMEBCDICATDE        else
  if UName='CSIBMEBCDICATDE'         then result := csIBMEBCDICATDE        else

(* Name: EBCDIC-AT-DE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2065                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICATDEA                                                       *)

  if UName='EBCDIC-AT-DE-A'          then result := csEBCDICATDEA          else
  if UName='CSEBCDICATDEA'           then result := csEBCDICATDEA          else

(* Name: EBCDIC-CA-FR                                        [RFC1345,KXS2]   *)
(* MIBenum: 2066                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICCAFR                                                        *)

  if UName='EBCDIC-CA-FR'            then result := csEBCDICCAFR           else
  if UName='CSEBCDICCAFR'            then result := csEBCDICCAFR           else

(* Name: EBCDIC-DK-NO                                        [RFC1345,KXS2]   *)
(* MIBenum: 2067                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNO                                                        *)

  if UName='EBCDIC-DK-NO'            then result := csEBCDICDKNO           else
  if UName='CSEBCDICDKNO'            then result := csEBCDICDKNO           else

(* Name: EBCDIC-DK-NO-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2068                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICDKNOA                                                       *)

  if UName='EBCDIC-DK-NO-A'          then result := csEBCDICDKNOA          else
  if UName='CSEBCDICDKNOA'           then result := csEBCDICDKNOA          else

(* Name: EBCDIC-FI-SE                                        [RFC1345,KXS2]   *)
(* MIBenum: 2069                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISE                                                        *)

  if UName='EBCDIC-FI-SE'            then result := csEBCDICFISE           else
  if UName='CSEBCDICFISE'            then result := csEBCDICFISE           else

(* Name: EBCDIC-FI-SE-A                                      [RFC1345,KXS2]   *)
(* MIBenum: 2070                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFISEA                                                       *)

  if UName='EBCDIC-FI-SE-A'          then result := csEBCDICFISEA          else
  if UName='CSEBCDICFISEA'           then result := csEBCDICFISEA          else

(* Name: EBCDIC-FR                                           [RFC1345,KXS2]   *)
(* MIBenum: 2071                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICFR                                                          *)

  if UName='EBCDIC-FR'               then result := csEBCDICFR             else
  if UName='CSEBCDICFR'              then result := csEBCDICFR             else

(* Name: EBCDIC-IT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2072                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICIT                                                          *)

  if UName='EBCDIC-IT'               then result := csEBCDICIT             else
  if UName='CSEBCDICIT'              then result := csEBCDICIT             else

(* Name: EBCDIC-PT                                           [RFC1345,KXS2]   *)
(* MIBenum: 2073                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICPT                                                          *)

  if UName='EBCDIC-PT'               then result := csEBCDICPT             else
  if UName='CSEBCDICPT'              then result := csEBCDICPT             else

(* Name: EBCDIC-ES                                           [RFC1345,KXS2]   *)
(* MIBenum: 2074                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICES                                                          *)

  if UName='EBCDIC-ES'               then result := csEBCDICES             else
  if UName='CSEBCDICES'              then result := csEBCDICES             else

(* Name: EBCDIC-ES-A                                         [RFC1345,KXS2]   *)
(* MIBenum: 2075                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESA                                                         *)

  if UName='EBCDIC-ES-A'             then result := csEBCDICESA            else
  if UName='CSEBCDICESA'             then result := csEBCDICESA            else

(* Name: EBCDIC-ES-S                                         [RFC1345,KXS2]   *)
(* MIBenum: 2076                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICESS                                                         *)

  if UName='EBCDIC-ES-S'             then result := csEBCDICESS            else
  if UName='CSEBCDICESS'             then result := csEBCDICESS            else

(* Name: EBCDIC-UK                                           [RFC1345,KXS2]   *)
(* MIBenum: 2077                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUK                                                          *)

  if UName='EBCDIC-UK'               then result := csEBCDICUK             else
  if UName='CSEBCDICUK'              then result := csEBCDICUK             else

(* Name: EBCDIC-US                                           [RFC1345,KXS2]   *)
(* MIBenum: 2078                                                              *)
(* Source: IBM 3270 Char Set Ref Ch 10, GA27-2837-9, April 1987               *)
(* Alias: csEBCDICUS                                                          *)

  if UName='EBCDIC-US'               then result := csEBCDICUS             else
  if UName='CSEBCDICUS'              then result := csEBCDICUS             else

(* Name: UNKNOWN-8BIT                                             [RFC1428]   *)
(* MIBenum: 2079                                                              *)
(* Alias: csUnknown8BiT                                                       *)

  if UName='UNKNOWN-8BIT'            then result := csUnknown8BiT          else
  if UName='CSUNKNOWN8BIT'           then result := csUnknown8BiT          else

(* Name: MNEMONIC                                            [RFC1345,KXS2]   *)
(* MIBenum: 2080                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+38"                        *)
(* Alias: csMnemonic                                                          *)

  if UName='MNEMONIC'                then result := csMnemonic             else
  if UName='CSMNEMONIC'              then result := csMnemonic             else

(* Name: MNEM                                                [RFC1345,KXS2]   *)
(* MIBenum: 2081                                                              *)
(* Source: RFC 1345, also known as "mnemonic+ascii+8200"                      *)
(* Alias: csMnem                                                              *)

  if UName='MNEM'                    then result := csMnem                 else
  if UName='CSMNEM'                  then result := csMnem                 else

(* Name: VISCII                                                   [RFC1456]   *)
(* MIBenum: 2082                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVISCII                                                            *)

  if UName='VISCII'                  then result := csVISCII               else
  if UName='CSVISCII'                then result := csVISCII               else

(* Name: VIQR                                                     [RFC1456]   *)
(* MIBenum: 2083                                                              *)
(* Source: RFC 1456                                                           *)
(* Alias: csVIQR                                                              *)

  if UName='VIQR'                    then result := csVIQR                 else
  if UName='CSVIQR'                  then result := csVIQR                 else

(* Name: KOI8-R  (preferred MIME name)                            [RFC1489]   *)
(* MIBenum: 2084                                                              *)
(* Source: RFC 1489, based on GOST-19768-74, ISO-6937/8,                      *)
(*         INIS-Cyrillic, ISO-5427.                                           *)
(* Alias: csKOI8R                                                             *)

  if UName='KOI8-R'                  then result := csKOI8R                else
  if UName='CSKOI8R'                 then result := csKOI8R                else

(* Name: KOI8-U                                                   [RFC2319]   *)
(* MIBenum: 2088                                                              *)
(* Source: RFC 2319                                                           *)

  if UName='KOI8-U'                  then result := csKOI8_U               else

(* Name: IBM00858                                                             *)
(* MIBenum: 2089                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM00858)    [Mahdi] *)
(* Alias: CCSID00858                                                          *)
(* Alias: CP00858                                                             *)
(* Alias: PC-Multilingual-850+euro                                            *)

  (* additional alias: CP858                                               *)
  (* additional alias: IBM858                                              *)
  (* overridden preference for MIME: PC-Multilingual-850+euro               *)

  if UName='IBM00858'                then result := csPC_Multilingual_850_euro else
  if UName='CCSID00858'              then result := csPC_Multilingual_850_euro else
  if UName='CP00858'                 then result := csPC_Multilingual_850_euro else
  if UName='PC-MULTILINGUAL-850+EURO' then result := csPC_Multilingual_850_euro else
  if UName='CP858'                   then result := csPC_Multilingual_850_euro else
  if UName='IBM858'                  then result := csPC_Multilingual_850_euro else

(* Name: IBM00924                                                             *)
(* MIBenum: 2090                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM00924)    [Mahdi] *)
(* Alias: CCSID00924                                                          *)
(* Alias: CP00924                                                             *)
(* Alias: ebcdic-Latin9--euro                                                 *)

  if UName='IBM00924'                then result := csIBM00924             else
  if UName='CCSID00924'              then result := csIBM00924             else
  if UName='CP00924'                 then result := csIBM00924             else
  if UName='EBCDIC-LATIN9--EURO'     then result := csIBM00924             else

(* Name: IBM01140                                                             *)
(* MIBenum: 2091                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01140)    [Mahdi] *)
(* Alias: CCSID01140                                                          *)
(* Alias: CP01140                                                             *)
(* Alias: ebcdic-us-37+euro                                                   *)

  if UName='IBM01140'                then result := csIBM01140             else
  if UName='CCSID01140'              then result := csIBM01140             else
  if UName='CP01140'                 then result := csIBM01140             else
  if UName='EBCDIC-US-37+EURO'       then result := csIBM01140             else

(* Name: IBM01141                                                             *)
(* MIBenum: 2092                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01141)    [Mahdi] *)
(* Alias: CCSID01141                                                          *)
(* Alias: CP01141                                                             *)
(* Alias: ebcdic-de-273+euro                                                  *)

  if UName='IBM01141'                then result := csIBM01141             else
  if UName='CCSID01141'              then result := csIBM01141             else
  if UName='CP01141'                 then result := csIBM01141             else
  if UName='EBCDIC-DE-273+EURO'      then result := csIBM01141             else

(* Name: IBM01142                                                             *)
(* MIBenum: 2093                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01142)    [Mahdi] *)
(* Alias: CCSID01142                                                          *)
(* Alias: CP01142                                                             *)
(* Alias: ebcdic-dk-277+euro                                                  *)
(* Alias: ebcdic-no-277+euro                                                  *)

  if UName='IBM01142'                then result := csIBM01142             else
  if UName='CCSID01142'              then result := csIBM01142             else
  if UName='CP01142'                 then result := csIBM01142             else
  if UName='EBCDIC-DK-277+EURO'      then result := csIBM01142             else
  if UName='EBCDIC-NO-277+EURO'      then result := csIBM01142             else

(* Name: IBM01143                                                             *)
(* MIBenum: 2094                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01143)    [Mahdi] *)
(* Alias: CCSID01143                                                          *)
(* Alias: CP01143                                                             *)
(* Alias: ebcdic-fi-278+euro                                                  *)
(* Alias: ebcdic-se-278+euro                                                  *)

  if UName='IBM01143'                then result := csIBM01143             else
  if UName='CCSID01143'              then result := csIBM01143             else
  if UName='CP01143'                 then result := csIBM01143             else
  if UName='EBCDIC-FI-278+EURO'      then result := csIBM01143             else
  if UName='EBCDIC-SE-278+EURO'      then result := csIBM01143             else

(* Name: IBM01144                                                             *)
(* MIBenum: 2095                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01144)    [Mahdi] *)
(* Alias: CCSID01144                                                          *)
(* Alias: CP01144                                                             *)
(* Alias: ebcdic-it-280+euro                                                  *)

  if UName='IBM01144'                then result := csIBM01144             else
  if UName='CCSID01144'              then result := csIBM01144             else
  if UName='CP01144'                 then result := csIBM01144             else
  if UName='EBCDIC-IT-280+EURO'      then result := csIBM01144             else

(* Name: IBM01145                                                             *)
(* MIBenum: 2096                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01145)    [Mahdi] *)
(* Alias: CCSID01145                                                          *)
(* Alias: CP01145                                                             *)
(* Alias: ebcdic-es-284+euro                                                  *)

  if UName='IBM01145'                then result := csIBM01145             else
  if UName='CCSID01145'              then result := csIBM01145             else
  if UName='CP01145'                 then result := csIBM01145             else
  if UName='EBCDIC-ES-284+EURO'      then result := csIBM01145             else

(* Name: IBM01146                                                             *)
(* MIBenum: 2097                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01146)    [Mahdi] *)
(* Alias: CCSID01146                                                          *)
(* Alias: CP01146                                                             *)
(* Alias: ebcdic-gb-285+euro                                                  *)

  if UName='IBM01146'                then result := csIBM01146             else
  if UName='CCSID01146'              then result := csIBM01146             else
  if UName='CP01146'                 then result := csIBM01146             else
  if UName='EBCDIC-GB-285+EURO'      then result := csIBM01146             else

(* Name: IBM01147                                                             *)
(* MIBenum: 2098                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01147)    [Mahdi] *)
(* Alias: CCSID01147                                                          *)
(* Alias: CP01147                                                             *)
(* Alias: ebcdic-fr-297+euro                                                  *)

  if UName='IBM01147'                then result := csIBM01147             else
  if UName='CCSID01147'              then result := csIBM01147             else
  if UName='CP01147'                 then result := csIBM01147             else
  if UName='EBCDIC-FR-297+EURO'      then result := csIBM01147             else

(* Name: IBM01148                                                             *)
(* MIBenum: 2099                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01148)    [Mahdi] *)
(* Alias: CCSID01148                                                          *)
(* Alias: CP01148                                                             *)
(* Alias: ebcdic-international-500+euro                                       *)

  if UName='IBM01148'                then result := csIBM01148             else
  if UName='CCSID01148'              then result := csIBM01148             else
  if UName='CP01148'                 then result := csIBM01148             else
  if UName='EBCDIC-INTERNATIONAL-500+EURO' then result := csIBM01148       else

(* Name: IBM01149                                                             *)
(* MIBenum: 2100                                                              *)
(* Source: IBM See (http://www.iana.org/assignments/charset-reg/IBM01149)    [Mahdi] *)
(* Alias: CCSID01149                                                          *)
(* Alias: CP01149                                                             *)
(* Alias: ebcdic-is-871+euro                                                  *)

  if UName='IBM01149'                then result := csIBM01149             else
  if UName='CCSID01149'              then result := csIBM01149             else
  if UName='CP01149'                 then result := csIBM01149             else
  if UName='EBCDIC-IS-871+EURO'      then result := csIBM01149             else

(* Name: Big5-HKSCS                                                  [Yick]   *)
(* MIBenum: 2101                                                              *)
(* Source:   See (http://www.iana.org/assignments/charset-reg/Big5-HKSCS)     *)

  if UName='BIG5-HKSCS'              then result := csBig5_HKSCS           else

(* Name: IBM1047                                                [Robrigado]   *)
(* MIBenum: 2102                                                              *)
(* Source: IBM1047 (EBCDIC Latin 1/Open Systems)                              *)
(* http://www-1.ibm.com/servers/eserver/iseries/software/globalization/pdf/cp01047z.pdf *)
(* Alias: IBM-1047                                                            *)

  if UName='IBM1047'                 then result := csIBM1047              else
  if UName='IBM-1047'                then result := csIBM1047              else

(* Name: PTCP154                                                    [Uskov]   *)
(* MIBenum: 2103                                                              *)
(* Source: See (http://www.iana.org/assignments/charset-reg/PTCP154)          *)
(* Alias: csPTCP154                                                           *)
(* Alias: PT154                                                               *)
(* Alias: CP154                                                               *)
(* Alias: Cyrillic-Asian                                                      *)

  if UName='PTCP154'                 then result := csPTCP154              else
  if UName='CSPTCP154'               then result := csPTCP154              else
  if UName='PT154'                   then result := csPTCP154              else
  if UName='CP154'                   then result := csPTCP154              else
  if UName='CYRILLIC-ASIAN'          then result := csPTCP154              else

(* Name: UNICODE-1-1                                              [RFC1641]   *)
(* MIBenum: 1010                                                              *)
(* Source: RFC 1641                                                           *)
(* Alias: csUnicode11                                                         *)

  if UName='UNICODE-1-1'             then result := csUnicode11            else
  if UName='CSUNICODE11'             then result := csUnicode11            else

(* Name: SCSU                                                                 *)
(* MIBenum: 1011                                                              *)
(* Source: SCSU See (http://www.iana.org/assignments/charset-reg/SCSU)     [Scherer] *)

  if UName='SCSU'                    then result := csSCSU                 else

(* Name: UTF-7                                                    [RFC2152]   *)
(* MIBenum: 1012                                                              *)
(* Source: RFC 2152                                                           *)

  if UName='UTF-7'                   then result := csUTF_7                else

(* Name: UTF-16BE                                                 [RFC2781]   *)
(* MIBenum: 1013                                                              *)
(* Source: RFC 2781                                                           *)

  (* additional alias: UNICODEFFFE                                         *)

  if UName='UTF-16BE'                then result := csUTF_16BE             else
  if UName='UNICODEFFFE'             then result := csUTF_16BE             else

(* Name: UTF-16LE                                                 [RFC2781]   *)
(* MIBenum: 1014                                                              *)
(* Source: RFC 2781                                                           *)

  if UName='UTF-16LE'                then result := csUTF_16LE             else

(* Name: UTF-16                                                   [RFC2781]   *)
(* MIBenum: 1015                                                              *)
(* Source: RFC 2781                                                           *)

  (* additional alias: UNICODE                                             *)

  if UName='UTF-16'                  then result := csUTF_16               else
  if UName='UNICODE'                 then result := csUTF_16               else

(* Name: CESU-8                                                    [Phipps]   *)
(* MIBenum: 1016                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr26>                      *)
(* Alias: csCESU-8                                                            *)

  if UName='CESU-8'                  then result := csCESU_8               else
  if UName='CSCESU-8'                then result := csCESU_8               else

(* Name: UTF-32                                                     [Davis]   *)
(* MIBenum: 1017                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)

  if UName='UTF-32'                  then result := csUTF_32               else

(* Name: UTF-32BE                                                   [Davis]   *)
(* MIBenum: 1018                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)

  if UName='UTF-32BE'                then result := csUTF_32BE             else

(* Name: UTF-32LE                                                   [Davis]   *)
(* MIBenum: 1019                                                              *)
(* Source: <http://www.unicode.org/unicode/reports/tr19/>                     *)

  if UName='UTF-32LE'                then result := csUTF_32LE             else

(* Name: BOCU-1                                                   [Scherer]   *)
(* MIBenum: 1020                                                              *)
(* Source: http://www.unicode.org/notes/tn6/                                  *)
(* Alias: csBOCU-1                                                            *)

  if UName='BOCU-1'                  then result := csBOCU_1               else
  if UName='CSBOCU-1'                then result := csBOCU_1               else

(* Name: UNICODE-1-1-UTF-7                                        [RFC1642]   *)
(* MIBenum: 103                                                               *)
(* Source: RFC 1642                                                           *)
(* Alias: csUnicode11UTF7                                                     *)

  if UName='UNICODE-1-1-UTF-7'       then result := csUnicode11UTF7        else
  if UName='CSUNICODE11UTF7'         then result := csUnicode11UTF7        else

(* Name: UTF-8                                                    [RFC-draft-yergeau-rfc2279bis-05.txt] *)
(* MIBenum: 106                                                               *)
(* Source: RFC draft-yergeau-rfc2279bis-05.txt                                *)

  if UName='UTF-8'                   then result := csUTF_8                else

(* Name: ISO-8859-13                                                          *)
(* MIBenum: 109                                                               *)
(* Source: ISO See (http://www.iana.org/assignments/charset-reg/iso-8859-13)[Tumasonis]  *)

  (* additional alias: LATIN-7                                             *)
  (* additional alias: ISO13                                               *)
  (* additional alias: LATIN7                                              *)
  (* additional alias: L7                                                  *)
  (* overridden preference for MIME: ISO-8859-13                            *)

  if UName='ISO-8859-13'             then result := csISO_8859_13          else
  if UName='LATIN-7'                 then result := csISO_8859_13          else
  if UName='ISO13'                   then result := csISO_8859_13          else
  if UName='LATIN7'                  then result := csISO_8859_13          else
  if UName='L7'                      then result := csISO_8859_13          else

(* Name: ISO-8859-14                                                          *)
(* MIBenum: 110                                                               *)
(* Source: ISO See (http://www.iana.org/assignments/charset-reg/iso-8859-14) [Simonsen] *)
(* Alias: iso-ir-199                                                          *)
(* Alias: ISO_8859-14:1998                                                    *)
(* Alias: ISO_8859-14                                                         *)
(* Alias: latin8                                                              *)
(* Alias: iso-celtic                                                          *)
(* Alias: l8                                                                  *)

  (* additional alias: LATIN-8                                             *)
  (* additional alias: ISO14                                               *)
  (* overridden preference for MIME: ISO-8859-14                            *)

  if UName='ISO-8859-14'             then result := csISO_8859_14          else
  if UName='ISO-IR-199'              then result := csISO_8859_14          else
  if UName='ISO_8859-14:1998'        then result := csISO_8859_14          else
  if UName='ISO_8859-14'             then result := csISO_8859_14          else
  if UName='LATIN8'                  then result := csISO_8859_14          else
  if UName='ISO-CELTIC'              then result := csISO_8859_14          else
  if UName='L8'                      then result := csISO_8859_14          else
  if UName='LATIN-8'                 then result := csISO_8859_14          else
  if UName='ISO14'                   then result := csISO_8859_14          else

(* Name: ISO-8859-15                                                          *)
(* MIBenum: 111                                                               *)
(* Source: ISO                                                                *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/ISO-8859-15> *)
(* Alias: ISO_8859-15                                                         *)
(* Alias: Latin-9                                                             *)

  (* additional alias: L9                                                  *)
  (* additional alias: LATIN9                                              *)
  (* overridden preference for MIME: ISO-8859-15                            *)

  if UName='ISO-8859-15'             then result := csISO_8859_15          else
  if UName='ISO_8859-15'             then result := csISO_8859_15          else
  if UName='LATIN-9'                 then result := csISO_8859_15          else
  if UName='L9'                      then result := csISO_8859_15          else
  if UName='LATIN9'                  then result := csISO_8859_15          else

(* Name: ISO-8859-16                                                          *)
(* MIBenum: 112                                                               *)
(* Source: ISO                                                                *)
(* Alias: iso-ir-226                                                          *)
(* Alias: ISO_8859-16:2001                                                    *)
(* Alias: ISO_8859-16                                                         *)
(* Alias: latin10                                                             *)
(* Alias: l10                                                                 *)

  (* additional alias: ISO16                                               *)
  (* overridden preference for MIME: ISO-8859-16                            *)

  if UName='ISO-8859-16'             then result := csISO_8859_16          else
  if UName='ISO-IR-226'              then result := csISO_8859_16          else
  if UName='ISO_8859-16:2001'        then result := csISO_8859_16          else
  if UName='ISO_8859-16'             then result := csISO_8859_16          else
  if UName='LATIN10'                 then result := csISO_8859_16          else
  if UName='L10'                     then result := csISO_8859_16          else
  if UName='ISO16'                   then result := csISO_8859_16          else

(* Name: GBK                                                                  *)
(* MIBenum: 113                                                               *)
(* Source: Chinese IT Standardization Technical Committee                     *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/GBK>      *)
(* Alias: CP936                                                               *)
(* Alias: MS936                                                               *)
(* Alias: windows-936                                                         *)

  if UName='GBK'                     then result := csGBK                  else
  if UName='CP936'                   then result := csGBK                  else
  if UName='MS936'                   then result := csGBK                  else
  if UName='WINDOWS-936'             then result := csGBK                  else

(* Name: GB18030                                                              *)
(* MIBenum: 114                                                               *)
(* Source: Chinese IT Standardization Technical Committee                     *)
(*         Please see: <http://www.iana.org/assignments/charset-reg/GB18030>  *)

  if UName='GB18030'                 then result := csGB18030              else

(* Name: JIS_Encoding                                                         *)
(* MIBenum: 16                                                                *)
(* Source: JIS X 0202-1991.  Uses ISO 2022 escape sequences to                *)
(*         shift code sets as documented in JIS X 0202-1991.                  *)
(* Alias: csJISEncoding                                                       *)

  if UName='JIS_ENCODING'            then result := csJISEncoding          else
  if UName='CSJISENCODING'           then result := csJISEncoding          else

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

  (* additional alias: SHIFT-JIS                                           *)
  (* additional alias: X-SJIS                                              *)

  if UName='SHIFT_JIS'               then result := csShiftJIS             else
  if UName='MS_KANJI'                then result := csShiftJIS             else
  if UName='CSSHIFTJIS'              then result := csShiftJIS             else
  if UName='SHIFT-JIS'               then result := csShiftJIS             else
  if UName='X-SJIS'                  then result := csShiftJIS             else

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

  if UName='EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE' then result := csEUCPkdFmtJapanese else
  if UName='CSEUCPKDFMTJAPANESE'     then result := csEUCPkdFmtJapanese    else
  if UName='EUC-JP'                  then result := csEUCPkdFmtJapanese    else

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

  if UName='EXTENDED_UNIX_CODE_FIXED_WIDTH_FOR_JAPANESE' then result := csEUCFixWidJapanese else
  if UName='CSEUCFIXWIDJAPANESE'     then result := csEUCFixWidJapanese    else

(* Name: ISO-10646-UCS-Basic                                                  *)
(* MIBenum: 1002                                                              *)
(* Source: ASCII subset of Unicode.  Basic Latin = collection 1               *)
(*         See ISO 10646, Appendix A                                          *)
(* Alias: csUnicodeASCII                                                      *)

  if UName='ISO-10646-UCS-BASIC'     then result := csUnicodeASCII         else
  if UName='CSUNICODEASCII'          then result := csUnicodeASCII         else

(* Name: ISO-10646-Unicode-Latin1                                             *)
(* MIBenum: 1003                                                              *)
(* Source: ISO Latin-1 subset of Unicode. Basic Latin and Latin-1             *)
(*          Supplement  = collections 1 and 2.  See ISO 10646,                *)
(*          Appendix A.  See RFC 1815.                                        *)
(* Alias: csUnicodeLatin1                                                     *)
(* Alias: ISO-10646                                                           *)

  if UName='ISO-10646-UNICODE-LATIN1' then result := csUnicodeLatin1       else
  if UName='CSUNICODELATIN1'         then result := csUnicodeLatin1        else
  if UName='ISO-10646'               then result := csUnicodeLatin1        else

(* Name: ISO-10646-J-1                                                        *)
(* Source: ISO 10646 Japanese, see RFC 1815.                                  *)

  if UName='ISO-10646-J-1'           then result := csISO_10646_J_1        else

(* Name: ISO-Unicode-IBM-1261                                                 *)
(* MIBenum: 1005                                                              *)
(* Source: IBM Latin-2, -3, -5, Extended Presentation Set, GCSGID: 1261       *)
(* Alias: csUnicodeIBM1261                                                    *)

  if UName='ISO-UNICODE-IBM-1261'    then result := csUnicodeIBM1261       else
  if UName='CSUNICODEIBM1261'        then result := csUnicodeIBM1261       else

(* Name: ISO-Unicode-IBM-1268                                                 *)
(* MIBenum: 1006                                                              *)
(* Source: IBM Latin-4 Extended Presentation Set, GCSGID: 1268                *)
(* Alias: csUnicodeIBM1268                                                    *)

  if UName='ISO-UNICODE-IBM-1268'    then result := csUnicodeIBM1268       else
  if UName='CSUNICODEIBM1268'        then result := csUnicodeIBM1268       else

(* Name: ISO-Unicode-IBM-1276                                                 *)
(* MIBenum: 1007                                                              *)
(* Source: IBM Cyrillic Greek Extended Presentation Set, GCSGID: 1276         *)
(* Alias: csUnicodeIBM1276                                                    *)

  if UName='ISO-UNICODE-IBM-1276'    then result := csUnicodeIBM1276       else
  if UName='CSUNICODEIBM1276'        then result := csUnicodeIBM1276       else

(* Name: ISO-Unicode-IBM-1264                                                 *)
(* MIBenum: 1008                                                              *)
(* Source: IBM Arabic Presentation Set, GCSGID: 1264                          *)
(* Alias: csUnicodeIBM1264                                                    *)

  if UName='ISO-UNICODE-IBM-1264'    then result := csUnicodeIBM1264       else
  if UName='CSUNICODEIBM1264'        then result := csUnicodeIBM1264       else

(* Name: ISO-Unicode-IBM-1265                                                 *)
(* MIBenum: 1009                                                              *)
(* Source: IBM Hebrew Presentation Set, GCSGID: 1265                          *)
(* Alias: csUnicodeIBM1265                                                    *)

  if UName='ISO-UNICODE-IBM-1265'    then result := csUnicodeIBM1265       else
  if UName='CSUNICODEIBM1265'        then result := csUnicodeIBM1265       else

(* Name: ISO-8859-1-Windows-3.0-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2000                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.0.                       *)
(*         PCL Symbol Set id: 9U                                              *)
(* Alias: csWindows30Latin1                                                   *)

  if UName='ISO-8859-1-WINDOWS-3.0-LATIN-1' then result := csWindows30Latin1 else
  if UName='CSWINDOWS30LATIN1'       then result := csWindows30Latin1      else

(* Name: ISO-8859-1-Windows-3.1-Latin-1                           [HP-PCL5]   *)
(* MIBenum: 2001                                                              *)
(* Source: Extended ISO 8859-1 Latin-1 for Windows 3.1.                       *)
(*         PCL Symbol Set id: 19U                                             *)
(* Alias: csWindows31Latin1                                                   *)

  if UName='ISO-8859-1-WINDOWS-3.1-LATIN-1' then result := csWindows31Latin1 else
  if UName='CSWINDOWS31LATIN1'       then result := csWindows31Latin1      else

(* Name: ISO-8859-2-Windows-Latin-2                               [HP-PCL5]   *)
(* MIBenum: 2002                                                              *)
(* Source: Extended ISO 8859-2.  Latin-2 for Windows 3.1.                     *)
(*         PCL Symbol Set id: 9E                                              *)
(* Alias: csWindows31Latin2                                                   *)

  if UName='ISO-8859-2-WINDOWS-LATIN-2' then result := csWindows31Latin2   else
  if UName='CSWINDOWS31LATIN2'       then result := csWindows31Latin2      else

(* Name: ISO-8859-9-Windows-Latin-5                               [HP-PCL5]   *)
(* MIBenum: 2003                                                              *)
(* Source: Extended ISO 8859-9.  Latin-5 for Windows 3.1                      *)
(*         PCL Symbol Set id: 5T                                              *)
(* Alias: csWindows31Latin5                                                   *)

  if UName='ISO-8859-9-WINDOWS-LATIN-5' then result := csWindows31Latin5   else
  if UName='CSWINDOWS31LATIN5'       then result := csWindows31Latin5      else

(* Name: Adobe-Standard-Encoding                                    [Adobe]   *)
(* MIBenum: 2005                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 10J                                             *)
(* Alias: csAdobeStandardEncoding                                             *)

  if UName='ADOBE-STANDARD-ENCODING' then result := csAdobeStandardEncoding else
  if UName='CSADOBESTANDARDENCODING' then result := csAdobeStandardEncoding else

(* Name: Ventura-US                                               [HP-PCL5]   *)
(* MIBenum: 2006                                                              *)
(* Source: Ventura US.  ASCII plus characters typically used in               *)
(*         publishing, like pilcrow, copyright, registered, trade mark,       *)
(*         section, dagger, and double dagger in the range A0 (hex)           *)
(*         to FF (hex).                                                       *)
(*         PCL Symbol Set id: 14J                                             *)
(* Alias: csVenturaUS                                                         *)

  if UName='VENTURA-US'              then result := csVenturaUS            else
  if UName='CSVENTURAUS'             then result := csVenturaUS            else

(* Name: Ventura-International                                    [HP-PCL5]   *)
(* MIBenum: 2007                                                              *)
(* Source: Ventura International.  ASCII plus coded characters similar        *)
(*         to Roman8.                                                         *)
(*         PCL Symbol Set id: 13J                                             *)
(* Alias: csVenturaInternational                                              *)

  if UName='VENTURA-INTERNATIONAL'   then result := csVenturaInternational else
  if UName='CSVENTURAINTERNATIONAL'  then result := csVenturaInternational else

(* Name: PC8-Danish-Norwegian                                     [HP-PCL5]   *)
(* MIBenum: 2012                                                              *)
(* Source: PC Danish Norwegian                                                *)
(*         8-bit PC set for Danish Norwegian                                  *)
(*         PCL Symbol Set id: 11U                                             *)
(* Alias: csPC8DanishNorwegian                                                *)

  if UName='PC8-DANISH-NORWEGIAN'    then result := csPC8DanishNorwegian   else
  if UName='CSPC8DANISHNORWEGIAN'    then result := csPC8DanishNorwegian   else

(* Name: PC8-Turkish                                              [HP-PCL5]   *)
(* MIBenum: 2014                                                              *)
(* Source: PC Latin Turkish.  PCL Symbol Set id: 9T                           *)
(* Alias: csPC8Turkish                                                        *)

  if UName='PC8-TURKISH'             then result := csPC8Turkish           else
  if UName='CSPC8TURKISH'            then result := csPC8Turkish           else

(* Name: IBM-Symbols                                             [IBM-CIDT]   *)
(* MIBenum: 2015                                                              *)
(* Source: Presentation Set, CPGID: 259                                       *)
(* Alias: csIBMSymbols                                                        *)

  if UName='IBM-SYMBOLS'             then result := csIBMSymbols           else
  if UName='CSIBMSYMBOLS'            then result := csIBMSymbols           else

(* Name: IBM-Thai                                                [IBM-CIDT]   *)
(* MIBenum: 2016                                                              *)
(* Source: Presentation Set, CPGID: 838                                       *)
(* Alias: csIBMThai                                                           *)

  if UName='IBM-THAI'                then result := csIBMThai              else
  if UName='CSIBMTHAI'               then result := csIBMThai              else

(* Name: HP-Legal                                                 [HP-PCL5]   *)
(* MIBenum: 2017                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 1U                                              *)
(* Alias: csHPLegal                                                           *)

  if UName='HP-LEGAL'                then result := csHPLegal              else
  if UName='CSHPLEGAL'               then result := csHPLegal              else

(* Name: HP-Pi-font                                               [HP-PCL5]   *)
(* MIBenum: 2018                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 15U                                             *)
(* Alias: csHPPiFont                                                          *)

  if UName='HP-PI-FONT'              then result := csHPPiFont             else
  if UName='CSHPPIFONT'              then result := csHPPiFont             else

(* Name: HP-Math8                                                 [HP-PCL5]   *)
(* MIBenum: 2019                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 8M                                              *)
(* Alias: csHPMath8                                                           *)

  if UName='HP-MATH8'                then result := csHPMath8              else
  if UName='CSHPMATH8'               then result := csHPMath8              else

(* Name: Adobe-Symbol-Encoding                                      [Adobe]   *)
(* MIBenum: 2020                                                              *)
(* Source: PostScript Language Reference Manual                               *)
(*         PCL Symbol Set id: 5M                                              *)
(* Alias: csHPPSMath                                                          *)

  if UName='ADOBE-SYMBOL-ENCODING'   then result := csHPPSMath             else
  if UName='CSHPPSMATH'              then result := csHPPSMath             else

(* Name: HP-DeskTop                                               [HP-PCL5]   *)
(* MIBenum: 2021                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 7J                                              *)
(* Alias: csHPDesktop                                                         *)

  if UName='HP-DESKTOP'              then result := csHPDesktop            else
  if UName='CSHPDESKTOP'             then result := csHPDesktop            else

(* Name: Ventura-Math                                             [HP-PCL5]   *)
(* MIBenum: 2022                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6M                                              *)
(* Alias: csVenturaMath                                                       *)

  if UName='VENTURA-MATH'            then result := csVenturaMath          else
  if UName='CSVENTURAMATH'           then result := csVenturaMath          else

(* Name: Microsoft-Publishing                                     [HP-PCL5]   *)
(* MIBenum: 2023                                                              *)
(* Source: PCL 5 Comparison Guide, Hewlett-Packard,                           *)
(*         HP part number 5961-0510, October 1992                             *)
(*         PCL Symbol Set id: 6J                                              *)
(* Alias: csMicrosoftPublishing                                               *)

  if UName='MICROSOFT-PUBLISHING'    then result := csMicrosoftPublishing  else
  if UName='CSMICROSOFTPUBLISHING'   then result := csMicrosoftPublishing  else

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

  if UName='WINDOWS-31J'             then result := csWindows31J           else
  if UName='CSWINDOWS31J'            then result := csWindows31J           else

(* Name: GB2312  (preferred MIME name)                                        *)
(* MIBenum: 2025                                                              *)
(* Source: Chinese for People's Republic of China (PRC) mixed one byte,       *)
(*         two byte set:                                                      *)
(*           20-7E = one byte ASCII                                           *)
(*           A1-FE = two byte PRC Kanji                                       *)
(*         See GB 2312-80                                                     *)
(*         PCL Symbol Set Id: 18C                                             *)
(* Alias: csGB2312                                                            *)

  (* additional alias: HANZI                                               *)

  if UName='GB2312'                  then result := csGB2312               else
  if UName='CSGB2312'                then result := csGB2312               else
  if UName='HANZI'                   then result := csGB2312               else

(* Name: Big5  (preferred MIME name)                                          *)
(* MIBenum: 2026                                                              *)
(* Source: Chinese for Taiwan Multi-byte set.                                 *)
(*         PCL Symbol Set Id: 18T                                             *)
(* Alias: csBig5                                                              *)

  (* additional alias: X-BIG5                                              *)
  (* additional alias: X-X-BIG5                                            *)

  if UName='BIG5'                    then result := csBig5                 else
  if UName='CSBIG5'                  then result := csBig5                 else
  if UName='X-BIG5'                  then result := csBig5                 else
  if UName='X-X-BIG5'                then result := csBig5                 else

(* Name: windows-1250                                                         *)
(* MIBenum: 2250                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1250) [Lazhintseva] *)

  if UName='WINDOWS-1250'            then result := cswindows_1250         else

(* Name: windows-1251                                                         *)
(* MIBenum: 2251                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1251) [Lazhintseva] *)

  if UName='WINDOWS-1251'            then result := cswindows_1251         else

(* Name: windows-1252                                                         *)
(* MIBenum: 2252                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1252)       [Wendt] *)

  if UName='WINDOWS-1252'            then result := cswindows_1252         else

(* Name: windows-1253                                                         *)
(* MIBenum: 2253                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1253) [Lazhintseva] *)

  if UName='WINDOWS-1253'            then result := cswindows_1253         else

(* Name: windows-1254                                                         *)
(* MIBenum: 2254                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1254) [Lazhintseva] *)

  if UName='WINDOWS-1254'            then result := cswindows_1254         else

(* Name: windows-1255                                                         *)
(* MIBenum: 2255                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1255) [Lazhintseva] *)

  if UName='WINDOWS-1255'            then result := cswindows_1255         else

(* Name: windows-1256                                                         *)
(* MIBenum: 2256                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1256) [Lazhintseva] *)

  if UName='WINDOWS-1256'            then result := cswindows_1256         else

(* Name: windows-1257                                                         *)
(* MIBenum: 2257                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1257) [Lazhintseva] *)

  if UName='WINDOWS-1257'            then result := cswindows_1257         else

(* Name: windows-1258                                                         *)
(* MIBenum: 2258                                                              *)
(* Source: Microsoft  (http://www.iana.org/assignments/charset-reg/windows-1258) [Lazhintseva] *)

  if UName='WINDOWS-1258'            then result := cswindows_1258         else

(* Name: TIS-620                                                              *)
(* MIBenum: 2259                                                              *)
(* Source: Thai Industrial Standards Institute (TISI)           [Tantsetthi]  *)

  if UName='TIS-620'                 then result := csTIS_620              else

(* Name: HZ-GB-2312                                                           *)
(* MIBenum: 2085                                                              *)
(* Source: RFC 1842, RFC 1843                              [RFC1842, RFC1843] *)


  if UName='HZ-GB-2312'              then result := csHZ_GB_2312           else

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






