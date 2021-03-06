(* $Id: Makefile.PL 6397 2003-09-29 20:47:21Z cl )
(* generated from IANA charset list -- do not edit *)

type
  TMimeCharset = {$IFDEF FPC} 0..2999; const {$ELSE} ( {$ENDIF}
  csASCII                    {$IFDEF FPC} : TMimeCharset =    3; {$ELSE} =    3,{$ENDIF}
  csISO10646UTF1             {$IFDEF FPC} : TMimeCharset =   27; {$ELSE} =   27,{$ENDIF}
  csISO646basic1983          {$IFDEF FPC} : TMimeCharset =   28; {$ELSE} =   28,{$ENDIF}
  csINVARIANT                {$IFDEF FPC} : TMimeCharset =   29; {$ELSE} =   29,{$ENDIF}
  csISO2IntlRefVersion       {$IFDEF FPC} : TMimeCharset =   30; {$ELSE} =   30,{$ENDIF}
  csISO4UnitedKingdom        {$IFDEF FPC} : TMimeCharset =   20; {$ELSE} =   20,{$ENDIF}
  csNATSSEFI                 {$IFDEF FPC} : TMimeCharset =   31; {$ELSE} =   31,{$ENDIF}
  csNATSSEFIADD              {$IFDEF FPC} : TMimeCharset =   32; {$ELSE} =   32,{$ENDIF}
  csNATSDANO                 {$IFDEF FPC} : TMimeCharset =   33; {$ELSE} =   33,{$ENDIF}
  csNATSDANOADD              {$IFDEF FPC} : TMimeCharset =   34; {$ELSE} =   34,{$ENDIF}
  csISO10Swedish             {$IFDEF FPC} : TMimeCharset =   35; {$ELSE} =   35,{$ENDIF}
  csISO11SwedishForNames     {$IFDEF FPC} : TMimeCharset =   21; {$ELSE} =   21,{$ENDIF}
  csKSC56011987              {$IFDEF FPC} : TMimeCharset =   36; {$ELSE} =   36,{$ENDIF}
  csISO2022KR                {$IFDEF FPC} : TMimeCharset =   37; {$ELSE} =   37,{$ENDIF}
  csEUCKR                    {$IFDEF FPC} : TMimeCharset =   38; {$ELSE} =   38,{$ENDIF}
  csISO2022JP                {$IFDEF FPC} : TMimeCharset =   39; {$ELSE} =   39,{$ENDIF}
  csISO2022JP2               {$IFDEF FPC} : TMimeCharset =   40; {$ELSE} =   40,{$ENDIF}
  csISO_2022_CN              {$IFDEF FPC} : TMimeCharset =  104; {$ELSE} =  104,{$ENDIF}
  csISO_2022_CN_EXT          {$IFDEF FPC} : TMimeCharset =  105; {$ELSE} =  105,{$ENDIF}
  csISO13JISC6220jp          {$IFDEF FPC} : TMimeCharset =   41; {$ELSE} =   41,{$ENDIF}
  csISO14JISC6220ro          {$IFDEF FPC} : TMimeCharset =   42; {$ELSE} =   42,{$ENDIF}
  csISO15Italian             {$IFDEF FPC} : TMimeCharset =   22; {$ELSE} =   22,{$ENDIF}
  csISO16Portuguese          {$IFDEF FPC} : TMimeCharset =   43; {$ELSE} =   43,{$ENDIF}
  csISO17Spanish             {$IFDEF FPC} : TMimeCharset =   23; {$ELSE} =   23,{$ENDIF}
  csISO18Greek7Old           {$IFDEF FPC} : TMimeCharset =   44; {$ELSE} =   44,{$ENDIF}
  csISO19LatinGreek          {$IFDEF FPC} : TMimeCharset =   45; {$ELSE} =   45,{$ENDIF}
  csISO21German              {$IFDEF FPC} : TMimeCharset =   24; {$ELSE} =   24,{$ENDIF}
  csISO25French              {$IFDEF FPC} : TMimeCharset =   46; {$ELSE} =   46,{$ENDIF}
  csISO27LatinGreek1         {$IFDEF FPC} : TMimeCharset =   47; {$ELSE} =   47,{$ENDIF}
  csISO5427Cyrillic          {$IFDEF FPC} : TMimeCharset =   48; {$ELSE} =   48,{$ENDIF}
  csISO42JISC62261978        {$IFDEF FPC} : TMimeCharset =   49; {$ELSE} =   49,{$ENDIF}
  csISO47BSViewdata          {$IFDEF FPC} : TMimeCharset =   50; {$ELSE} =   50,{$ENDIF}
  csISO49INIS                {$IFDEF FPC} : TMimeCharset =   51; {$ELSE} =   51,{$ENDIF}
  csISO50INIS8               {$IFDEF FPC} : TMimeCharset =   52; {$ELSE} =   52,{$ENDIF}
  csISO51INISCyrillic        {$IFDEF FPC} : TMimeCharset =   53; {$ELSE} =   53,{$ENDIF}
  csiso_ir_54                {$IFDEF FPC} : TMimeCharset =   54; {$ELSE} =   54,{$ENDIF}
  csISO5428Greek             {$IFDEF FPC} : TMimeCharset =   55; {$ELSE} =   55,{$ENDIF}
  csISO57GB1988              {$IFDEF FPC} : TMimeCharset =   56; {$ELSE} =   56,{$ENDIF}
  csISO58GB231280            {$IFDEF FPC} : TMimeCharset =   57; {$ELSE} =   57,{$ENDIF}
  csISO60DanishNorwegian     {$IFDEF FPC} : TMimeCharset =   25; {$ELSE} =   25,{$ENDIF}
  csISO61Norwegian2          {$IFDEF FPC} : TMimeCharset =   58; {$ELSE} =   58,{$ENDIF}
  csISO69French              {$IFDEF FPC} : TMimeCharset =   26; {$ELSE} =   26,{$ENDIF}
  csISO70VideotexSupp1       {$IFDEF FPC} : TMimeCharset =   59; {$ELSE} =   59,{$ENDIF}
  csISO84Portuguese2         {$IFDEF FPC} : TMimeCharset =   60; {$ELSE} =   60,{$ENDIF}
  csISO85Spanish2            {$IFDEF FPC} : TMimeCharset =   61; {$ELSE} =   61,{$ENDIF}
  csISO86Hungarian           {$IFDEF FPC} : TMimeCharset =   62; {$ELSE} =   62,{$ENDIF}
  csISO87JISX0208            {$IFDEF FPC} : TMimeCharset =   63; {$ELSE} =   63,{$ENDIF}
  csISO88Greek7              {$IFDEF FPC} : TMimeCharset =   64; {$ELSE} =   64,{$ENDIF}
  csISO89ASMO449             {$IFDEF FPC} : TMimeCharset =   65; {$ELSE} =   65,{$ENDIF}
  csISO90                    {$IFDEF FPC} : TMimeCharset =   66; {$ELSE} =   66,{$ENDIF}
  csISO91JISC62291984a       {$IFDEF FPC} : TMimeCharset =   67; {$ELSE} =   67,{$ENDIF}
  csISO92JISC62991984b       {$IFDEF FPC} : TMimeCharset =   68; {$ELSE} =   68,{$ENDIF}
  csISO93JIS62291984badd     {$IFDEF FPC} : TMimeCharset =   69; {$ELSE} =   69,{$ENDIF}
  csISO94JIS62291984hand     {$IFDEF FPC} : TMimeCharset =   70; {$ELSE} =   70,{$ENDIF}
  csISO95JIS62291984handadd  {$IFDEF FPC} : TMimeCharset =   71; {$ELSE} =   71,{$ENDIF}
  csISO96JISC62291984kana    {$IFDEF FPC} : TMimeCharset =   72; {$ELSE} =   72,{$ENDIF}
  csISO2033                  {$IFDEF FPC} : TMimeCharset =   73; {$ELSE} =   73,{$ENDIF}
  csISO99NAPLPS              {$IFDEF FPC} : TMimeCharset =   74; {$ELSE} =   74,{$ENDIF}
  csISOLatin1                {$IFDEF FPC} : TMimeCharset =    4; {$ELSE} =    4,{$ENDIF}
  csISOLatin2                {$IFDEF FPC} : TMimeCharset =    5; {$ELSE} =    5,{$ENDIF}
  csISO102T617bit            {$IFDEF FPC} : TMimeCharset =   75; {$ELSE} =   75,{$ENDIF}
  csISO103T618bit            {$IFDEF FPC} : TMimeCharset =   76; {$ELSE} =   76,{$ENDIF}
  csISOLatin3                {$IFDEF FPC} : TMimeCharset =    6; {$ELSE} =    6,{$ENDIF}
  csISOLatin4                {$IFDEF FPC} : TMimeCharset =    7; {$ELSE} =    7,{$ENDIF}
  csISO111ECMACyrillic       {$IFDEF FPC} : TMimeCharset =   77; {$ELSE} =   77,{$ENDIF}
  csa7_1                     {$IFDEF FPC} : TMimeCharset =   78; {$ELSE} =   78,{$ENDIF}
  csa7_2                     {$IFDEF FPC} : TMimeCharset =   79; {$ELSE} =   79,{$ENDIF}
  csISO123CSAZ24341985gr     {$IFDEF FPC} : TMimeCharset =   80; {$ELSE} =   80,{$ENDIF}
  csISOLatinArabic           {$IFDEF FPC} : TMimeCharset =    9; {$ELSE} =    9,{$ENDIF}
  csISO88596E                {$IFDEF FPC} : TMimeCharset =   81; {$ELSE} =   81,{$ENDIF}
  csISO88596I                {$IFDEF FPC} : TMimeCharset =   82; {$ELSE} =   82,{$ENDIF}
  csISOLatinGreek            {$IFDEF FPC} : TMimeCharset =   10; {$ELSE} =   10,{$ENDIF}
  csISO128T101G2             {$IFDEF FPC} : TMimeCharset =   83; {$ELSE} =   83,{$ENDIF}
  csISOLatinHebrew           {$IFDEF FPC} : TMimeCharset =   11; {$ELSE} =   11,{$ENDIF}
  csISO88598E                {$IFDEF FPC} : TMimeCharset =   84; {$ELSE} =   84,{$ENDIF}
  csISO88598I                {$IFDEF FPC} : TMimeCharset =   85; {$ELSE} =   85,{$ENDIF}
  csISO139CSN369103          {$IFDEF FPC} : TMimeCharset =   86; {$ELSE} =   86,{$ENDIF}
  csISO141JUSIB1002          {$IFDEF FPC} : TMimeCharset =   87; {$ELSE} =   87,{$ENDIF}
  csISOTextComm              {$IFDEF FPC} : TMimeCharset =   14; {$ELSE} =   14,{$ENDIF}
  csISO143IECP271            {$IFDEF FPC} : TMimeCharset =   88; {$ELSE} =   88,{$ENDIF}
  csISOLatinCyrillic         {$IFDEF FPC} : TMimeCharset =    8; {$ELSE} =    8,{$ENDIF}
  csISO146Serbian            {$IFDEF FPC} : TMimeCharset =   89; {$ELSE} =   89,{$ENDIF}
  csISO147Macedonian         {$IFDEF FPC} : TMimeCharset =   90; {$ELSE} =   90,{$ENDIF}
  csISOLatin5                {$IFDEF FPC} : TMimeCharset =   12; {$ELSE} =   12,{$ENDIF}
  csISO150                   {$IFDEF FPC} : TMimeCharset =   91; {$ELSE} =   91,{$ENDIF}
  csISO151Cuba               {$IFDEF FPC} : TMimeCharset =   92; {$ELSE} =   92,{$ENDIF}
  csISO6937Add               {$IFDEF FPC} : TMimeCharset =   93; {$ELSE} =   93,{$ENDIF}
  csISO153GOST1976874        {$IFDEF FPC} : TMimeCharset =   94; {$ELSE} =   94,{$ENDIF}
  csISO8859Supp              {$IFDEF FPC} : TMimeCharset =   95; {$ELSE} =   95,{$ENDIF}
  csISO10367Box              {$IFDEF FPC} : TMimeCharset =   96; {$ELSE} =   96,{$ENDIF}
  csISOLatin6                {$IFDEF FPC} : TMimeCharset =   13; {$ELSE} =   13,{$ENDIF}
  csISO158Lap                {$IFDEF FPC} : TMimeCharset =   97; {$ELSE} =   97,{$ENDIF}
  csISO159JISX02121990       {$IFDEF FPC} : TMimeCharset =   98; {$ELSE} =   98,{$ENDIF}
  csISO646Danish             {$IFDEF FPC} : TMimeCharset =   99; {$ELSE} =   99,{$ENDIF}
  csUSDK                     {$IFDEF FPC} : TMimeCharset =  100; {$ELSE} =  100,{$ENDIF}
  csDKUS                     {$IFDEF FPC} : TMimeCharset =  101; {$ELSE} =  101,{$ENDIF}
  csHalfWidthKatakana        {$IFDEF FPC} : TMimeCharset =   15; {$ELSE} =   15,{$ENDIF}
  csKSC5636                  {$IFDEF FPC} : TMimeCharset =  102; {$ELSE} =  102,{$ENDIF}
  csUnicode                  {$IFDEF FPC} : TMimeCharset = 1000; {$ELSE} = 1000,{$ENDIF}
  csUCS4                     {$IFDEF FPC} : TMimeCharset = 1001; {$ELSE} = 1001,{$ENDIF}
  csDECMCS                   {$IFDEF FPC} : TMimeCharset = 2008; {$ELSE} = 2008,{$ENDIF}
  csHPRoman8                 {$IFDEF FPC} : TMimeCharset = 2004; {$ELSE} = 2004,{$ENDIF}
  csMacintosh                {$IFDEF FPC} : TMimeCharset = 2027; {$ELSE} = 2027,{$ENDIF}
  csIBM037                   {$IFDEF FPC} : TMimeCharset = 2028; {$ELSE} = 2028,{$ENDIF}
  csIBM038                   {$IFDEF FPC} : TMimeCharset = 2029; {$ELSE} = 2029,{$ENDIF}
  csIBM273                   {$IFDEF FPC} : TMimeCharset = 2030; {$ELSE} = 2030,{$ENDIF}
  csIBM274                   {$IFDEF FPC} : TMimeCharset = 2031; {$ELSE} = 2031,{$ENDIF}
  csIBM275                   {$IFDEF FPC} : TMimeCharset = 2032; {$ELSE} = 2032,{$ENDIF}
  csIBM277                   {$IFDEF FPC} : TMimeCharset = 2033; {$ELSE} = 2033,{$ENDIF}
  csIBM278                   {$IFDEF FPC} : TMimeCharset = 2034; {$ELSE} = 2034,{$ENDIF}
  csIBM280                   {$IFDEF FPC} : TMimeCharset = 2035; {$ELSE} = 2035,{$ENDIF}
  csIBM281                   {$IFDEF FPC} : TMimeCharset = 2036; {$ELSE} = 2036,{$ENDIF}
  csIBM284                   {$IFDEF FPC} : TMimeCharset = 2037; {$ELSE} = 2037,{$ENDIF}
  csIBM285                   {$IFDEF FPC} : TMimeCharset = 2038; {$ELSE} = 2038,{$ENDIF}
  csIBM290                   {$IFDEF FPC} : TMimeCharset = 2039; {$ELSE} = 2039,{$ENDIF}
  csIBM297                   {$IFDEF FPC} : TMimeCharset = 2040; {$ELSE} = 2040,{$ENDIF}
  csIBM420                   {$IFDEF FPC} : TMimeCharset = 2041; {$ELSE} = 2041,{$ENDIF}
  csIBM423                   {$IFDEF FPC} : TMimeCharset = 2042; {$ELSE} = 2042,{$ENDIF}
  csIBM424                   {$IFDEF FPC} : TMimeCharset = 2043; {$ELSE} = 2043,{$ENDIF}
  csPC8CodePage437           {$IFDEF FPC} : TMimeCharset = 2011; {$ELSE} = 2011,{$ENDIF}
  csIBM500                   {$IFDEF FPC} : TMimeCharset = 2044; {$ELSE} = 2044,{$ENDIF}
  csPC775Baltic              {$IFDEF FPC} : TMimeCharset = 2087; {$ELSE} = 2087,{$ENDIF}
  csPC850Multilingual        {$IFDEF FPC} : TMimeCharset = 2009; {$ELSE} = 2009,{$ENDIF}
  csIBM851                   {$IFDEF FPC} : TMimeCharset = 2045; {$ELSE} = 2045,{$ENDIF}
  csPCp852                   {$IFDEF FPC} : TMimeCharset = 2010; {$ELSE} = 2010,{$ENDIF}
  csIBM855                   {$IFDEF FPC} : TMimeCharset = 2046; {$ELSE} = 2046,{$ENDIF}
  csIBM857                   {$IFDEF FPC} : TMimeCharset = 2047; {$ELSE} = 2047,{$ENDIF}
  csIBM860                   {$IFDEF FPC} : TMimeCharset = 2048; {$ELSE} = 2048,{$ENDIF}
  csIBM861                   {$IFDEF FPC} : TMimeCharset = 2049; {$ELSE} = 2049,{$ENDIF}
  csPC862LatinHebrew         {$IFDEF FPC} : TMimeCharset = 2013; {$ELSE} = 2013,{$ENDIF}
  csIBM863                   {$IFDEF FPC} : TMimeCharset = 2050; {$ELSE} = 2050,{$ENDIF}
  csIBM864                   {$IFDEF FPC} : TMimeCharset = 2051; {$ELSE} = 2051,{$ENDIF}
  csIBM865                   {$IFDEF FPC} : TMimeCharset = 2052; {$ELSE} = 2052,{$ENDIF}
  csIBM866                   {$IFDEF FPC} : TMimeCharset = 2086; {$ELSE} = 2086,{$ENDIF}
  csIBM868                   {$IFDEF FPC} : TMimeCharset = 2053; {$ELSE} = 2053,{$ENDIF}
  csIBM869                   {$IFDEF FPC} : TMimeCharset = 2054; {$ELSE} = 2054,{$ENDIF}
  csIBM870                   {$IFDEF FPC} : TMimeCharset = 2055; {$ELSE} = 2055,{$ENDIF}
  csIBM871                   {$IFDEF FPC} : TMimeCharset = 2056; {$ELSE} = 2056,{$ENDIF}
  csIBM880                   {$IFDEF FPC} : TMimeCharset = 2057; {$ELSE} = 2057,{$ENDIF}
  csIBM891                   {$IFDEF FPC} : TMimeCharset = 2058; {$ELSE} = 2058,{$ENDIF}
  csIBM903                   {$IFDEF FPC} : TMimeCharset = 2059; {$ELSE} = 2059,{$ENDIF}
  csIBBM904                  {$IFDEF FPC} : TMimeCharset = 2060; {$ELSE} = 2060,{$ENDIF}
  csIBM905                   {$IFDEF FPC} : TMimeCharset = 2061; {$ELSE} = 2061,{$ENDIF}
  csIBM918                   {$IFDEF FPC} : TMimeCharset = 2062; {$ELSE} = 2062,{$ENDIF}
  csIBM1026                  {$IFDEF FPC} : TMimeCharset = 2063; {$ELSE} = 2063,{$ENDIF}
  csIBMEBCDICATDE            {$IFDEF FPC} : TMimeCharset = 2064; {$ELSE} = 2064,{$ENDIF}
  csEBCDICATDEA              {$IFDEF FPC} : TMimeCharset = 2065; {$ELSE} = 2065,{$ENDIF}
  csEBCDICCAFR               {$IFDEF FPC} : TMimeCharset = 2066; {$ELSE} = 2066,{$ENDIF}
  csEBCDICDKNO               {$IFDEF FPC} : TMimeCharset = 2067; {$ELSE} = 2067,{$ENDIF}
  csEBCDICDKNOA              {$IFDEF FPC} : TMimeCharset = 2068; {$ELSE} = 2068,{$ENDIF}
  csEBCDICFISE               {$IFDEF FPC} : TMimeCharset = 2069; {$ELSE} = 2069,{$ENDIF}
  csEBCDICFISEA              {$IFDEF FPC} : TMimeCharset = 2070; {$ELSE} = 2070,{$ENDIF}
  csEBCDICFR                 {$IFDEF FPC} : TMimeCharset = 2071; {$ELSE} = 2071,{$ENDIF}
  csEBCDICIT                 {$IFDEF FPC} : TMimeCharset = 2072; {$ELSE} = 2072,{$ENDIF}
  csEBCDICPT                 {$IFDEF FPC} : TMimeCharset = 2073; {$ELSE} = 2073,{$ENDIF}
  csEBCDICES                 {$IFDEF FPC} : TMimeCharset = 2074; {$ELSE} = 2074,{$ENDIF}
  csEBCDICESA                {$IFDEF FPC} : TMimeCharset = 2075; {$ELSE} = 2075,{$ENDIF}
  csEBCDICESS                {$IFDEF FPC} : TMimeCharset = 2076; {$ELSE} = 2076,{$ENDIF}
  csEBCDICUK                 {$IFDEF FPC} : TMimeCharset = 2077; {$ELSE} = 2077,{$ENDIF}
  csEBCDICUS                 {$IFDEF FPC} : TMimeCharset = 2078; {$ELSE} = 2078,{$ENDIF}
  csUnknown8BiT              {$IFDEF FPC} : TMimeCharset = 2079; {$ELSE} = 2079,{$ENDIF}
  csMnemonic                 {$IFDEF FPC} : TMimeCharset = 2080; {$ELSE} = 2080,{$ENDIF}
  csMnem                     {$IFDEF FPC} : TMimeCharset = 2081; {$ELSE} = 2081,{$ENDIF}
  csVISCII                   {$IFDEF FPC} : TMimeCharset = 2082; {$ELSE} = 2082,{$ENDIF}
  csVIQR                     {$IFDEF FPC} : TMimeCharset = 2083; {$ELSE} = 2083,{$ENDIF}
  csKOI8R                    {$IFDEF FPC} : TMimeCharset = 2084; {$ELSE} = 2084,{$ENDIF}
  csKOI8_U                   {$IFDEF FPC} : TMimeCharset = 2088; {$ELSE} = 2088,{$ENDIF}
  csPC_Multilingual_850_euro {$IFDEF FPC} : TMimeCharset = 2089; {$ELSE} = 2089,{$ENDIF}
  csIBM00924                 {$IFDEF FPC} : TMimeCharset = 2090; {$ELSE} = 2090,{$ENDIF}
  csIBM01140                 {$IFDEF FPC} : TMimeCharset = 2091; {$ELSE} = 2091,{$ENDIF}
  csIBM01141                 {$IFDEF FPC} : TMimeCharset = 2092; {$ELSE} = 2092,{$ENDIF}
  csIBM01142                 {$IFDEF FPC} : TMimeCharset = 2093; {$ELSE} = 2093,{$ENDIF}
  csIBM01143                 {$IFDEF FPC} : TMimeCharset = 2094; {$ELSE} = 2094,{$ENDIF}
  csIBM01144                 {$IFDEF FPC} : TMimeCharset = 2095; {$ELSE} = 2095,{$ENDIF}
  csIBM01145                 {$IFDEF FPC} : TMimeCharset = 2096; {$ELSE} = 2096,{$ENDIF}
  csIBM01146                 {$IFDEF FPC} : TMimeCharset = 2097; {$ELSE} = 2097,{$ENDIF}
  csIBM01147                 {$IFDEF FPC} : TMimeCharset = 2098; {$ELSE} = 2098,{$ENDIF}
  csIBM01148                 {$IFDEF FPC} : TMimeCharset = 2099; {$ELSE} = 2099,{$ENDIF}
  csIBM01149                 {$IFDEF FPC} : TMimeCharset = 2100; {$ELSE} = 2100,{$ENDIF}
  csBig5_HKSCS               {$IFDEF FPC} : TMimeCharset = 2101; {$ELSE} = 2101,{$ENDIF}
  csIBM1047                  {$IFDEF FPC} : TMimeCharset = 2102; {$ELSE} = 2102,{$ENDIF}
  csPTCP154                  {$IFDEF FPC} : TMimeCharset = 2103; {$ELSE} = 2103,{$ENDIF}
  csAmiga_1251               {$IFDEF FPC} : TMimeCharset = 2104; {$ELSE} = 2104,{$ENDIF}
  csKOI7_switched            {$IFDEF FPC} : TMimeCharset = 2105; {$ELSE} = 2105,{$ENDIF}
  csUnicode11                {$IFDEF FPC} : TMimeCharset = 1010; {$ELSE} = 1010,{$ENDIF}
  csSCSU                     {$IFDEF FPC} : TMimeCharset = 1011; {$ELSE} = 1011,{$ENDIF}
  csUTF_7                    {$IFDEF FPC} : TMimeCharset = 1012; {$ELSE} = 1012,{$ENDIF}
  csUTF_16BE                 {$IFDEF FPC} : TMimeCharset = 1013; {$ELSE} = 1013,{$ENDIF}
  csUTF_16LE                 {$IFDEF FPC} : TMimeCharset = 1014; {$ELSE} = 1014,{$ENDIF}
  csUTF_16                   {$IFDEF FPC} : TMimeCharset = 1015; {$ELSE} = 1015,{$ENDIF}
  csCESU_8                   {$IFDEF FPC} : TMimeCharset = 1016; {$ELSE} = 1016,{$ENDIF}
  csUTF_32                   {$IFDEF FPC} : TMimeCharset = 1017; {$ELSE} = 1017,{$ENDIF}
  csUTF_32BE                 {$IFDEF FPC} : TMimeCharset = 1018; {$ELSE} = 1018,{$ENDIF}
  csUTF_32LE                 {$IFDEF FPC} : TMimeCharset = 1019; {$ELSE} = 1019,{$ENDIF}
  csBOCU_1                   {$IFDEF FPC} : TMimeCharset = 1020; {$ELSE} = 1020,{$ENDIF}
  csUnicode11UTF7            {$IFDEF FPC} : TMimeCharset =  103; {$ELSE} =  103,{$ENDIF}
  csUTF_8                    {$IFDEF FPC} : TMimeCharset =  106; {$ELSE} =  106,{$ENDIF}
  csISO_8859_13              {$IFDEF FPC} : TMimeCharset =  109; {$ELSE} =  109,{$ENDIF}
  csISO_8859_14              {$IFDEF FPC} : TMimeCharset =  110; {$ELSE} =  110,{$ENDIF}
  csISO_8859_15              {$IFDEF FPC} : TMimeCharset =  111; {$ELSE} =  111,{$ENDIF}
  csISO_8859_16              {$IFDEF FPC} : TMimeCharset =  112; {$ELSE} =  112,{$ENDIF}
  csGBK                      {$IFDEF FPC} : TMimeCharset =  113; {$ELSE} =  113,{$ENDIF}
  csGB18030                  {$IFDEF FPC} : TMimeCharset =  114; {$ELSE} =  114,{$ENDIF}
  csOSD_EBCDIC_DF04_15       {$IFDEF FPC} : TMimeCharset =  115; {$ELSE} =  115,{$ENDIF}
  csOSD_EBCDIC_DF03_IRV      {$IFDEF FPC} : TMimeCharset =  116; {$ELSE} =  116,{$ENDIF}
  csOSD_EBCDIC_DF04_1        {$IFDEF FPC} : TMimeCharset =  117; {$ELSE} =  117,{$ENDIF}
  csJISEncoding              {$IFDEF FPC} : TMimeCharset =   16; {$ELSE} =   16,{$ENDIF}
  csShiftJIS                 {$IFDEF FPC} : TMimeCharset =   17; {$ELSE} =   17,{$ENDIF}
  csEUCPkdFmtJapanese        {$IFDEF FPC} : TMimeCharset =   18; {$ELSE} =   18,{$ENDIF}
  csEUCFixWidJapanese        {$IFDEF FPC} : TMimeCharset =   19; {$ELSE} =   19,{$ENDIF}
  csUnicodeASCII             {$IFDEF FPC} : TMimeCharset = 1002; {$ELSE} = 1002,{$ENDIF}
  csUnicodeLatin1            {$IFDEF FPC} : TMimeCharset = 1003; {$ELSE} = 1003,{$ENDIF}
  csISO_10646_J_1            {$IFDEF FPC} : TMimeCharset = 1004; {$ELSE} = 1004,{$ENDIF}
  csUnicodeIBM1261           {$IFDEF FPC} : TMimeCharset = 1005; {$ELSE} = 1005,{$ENDIF}
  csUnicodeIBM1268           {$IFDEF FPC} : TMimeCharset = 1006; {$ELSE} = 1006,{$ENDIF}
  csUnicodeIBM1276           {$IFDEF FPC} : TMimeCharset = 1007; {$ELSE} = 1007,{$ENDIF}
  csUnicodeIBM1264           {$IFDEF FPC} : TMimeCharset = 1008; {$ELSE} = 1008,{$ENDIF}
  csUnicodeIBM1265           {$IFDEF FPC} : TMimeCharset = 1009; {$ELSE} = 1009,{$ENDIF}
  csWindows30Latin1          {$IFDEF FPC} : TMimeCharset = 2000; {$ELSE} = 2000,{$ENDIF}
  csWindows31Latin1          {$IFDEF FPC} : TMimeCharset = 2001; {$ELSE} = 2001,{$ENDIF}
  csWindows31Latin2          {$IFDEF FPC} : TMimeCharset = 2002; {$ELSE} = 2002,{$ENDIF}
  csWindows31Latin5          {$IFDEF FPC} : TMimeCharset = 2003; {$ELSE} = 2003,{$ENDIF}
  csAdobeStandardEncoding    {$IFDEF FPC} : TMimeCharset = 2005; {$ELSE} = 2005,{$ENDIF}
  csVenturaUS                {$IFDEF FPC} : TMimeCharset = 2006; {$ELSE} = 2006,{$ENDIF}
  csVenturaInternational     {$IFDEF FPC} : TMimeCharset = 2007; {$ELSE} = 2007,{$ENDIF}
  csPC8DanishNorwegian       {$IFDEF FPC} : TMimeCharset = 2012; {$ELSE} = 2012,{$ENDIF}
  csPC8Turkish               {$IFDEF FPC} : TMimeCharset = 2014; {$ELSE} = 2014,{$ENDIF}
  csIBMSymbols               {$IFDEF FPC} : TMimeCharset = 2015; {$ELSE} = 2015,{$ENDIF}
  csIBMThai                  {$IFDEF FPC} : TMimeCharset = 2016; {$ELSE} = 2016,{$ENDIF}
  csHPLegal                  {$IFDEF FPC} : TMimeCharset = 2017; {$ELSE} = 2017,{$ENDIF}
  csHPPiFont                 {$IFDEF FPC} : TMimeCharset = 2018; {$ELSE} = 2018,{$ENDIF}
  csHPMath8                  {$IFDEF FPC} : TMimeCharset = 2019; {$ELSE} = 2019,{$ENDIF}
  csHPPSMath                 {$IFDEF FPC} : TMimeCharset = 2020; {$ELSE} = 2020,{$ENDIF}
  csHPDesktop                {$IFDEF FPC} : TMimeCharset = 2021; {$ELSE} = 2021,{$ENDIF}
  csVenturaMath              {$IFDEF FPC} : TMimeCharset = 2022; {$ELSE} = 2022,{$ENDIF}
  csMicrosoftPublishing      {$IFDEF FPC} : TMimeCharset = 2023; {$ELSE} = 2023,{$ENDIF}
  csWindows31J               {$IFDEF FPC} : TMimeCharset = 2024; {$ELSE} = 2024,{$ENDIF}
  csGB2312                   {$IFDEF FPC} : TMimeCharset = 2025; {$ELSE} = 2025,{$ENDIF}
  csBig5                     {$IFDEF FPC} : TMimeCharset = 2026; {$ELSE} = 2026,{$ENDIF}
  cswindows_1250             {$IFDEF FPC} : TMimeCharset = 2250; {$ELSE} = 2250,{$ENDIF}
  cswindows_1251             {$IFDEF FPC} : TMimeCharset = 2251; {$ELSE} = 2251,{$ENDIF}
  cswindows_1252             {$IFDEF FPC} : TMimeCharset = 2252; {$ELSE} = 2252,{$ENDIF}
  cswindows_1253             {$IFDEF FPC} : TMimeCharset = 2253; {$ELSE} = 2253,{$ENDIF}
  cswindows_1254             {$IFDEF FPC} : TMimeCharset = 2254; {$ELSE} = 2254,{$ENDIF}
  cswindows_1255             {$IFDEF FPC} : TMimeCharset = 2255; {$ELSE} = 2255,{$ENDIF}
  cswindows_1256             {$IFDEF FPC} : TMimeCharset = 2256; {$ELSE} = 2256,{$ENDIF}
  cswindows_1257             {$IFDEF FPC} : TMimeCharset = 2257; {$ELSE} = 2257,{$ENDIF}
  cswindows_1258             {$IFDEF FPC} : TMimeCharset = 2258; {$ELSE} = 2258,{$ENDIF}
  csTIS_620                  {$IFDEF FPC} : TMimeCharset = 2259; {$ELSE} = 2259,{$ENDIF}
  csHZ_GB_2312               {$IFDEF FPC} : TMimeCharset = 2085; {$ELSE} = 2085,{$ENDIF}
  csUnknown                  {$IFDEF FPC} : TMimeCharset =    0; {$ELSE} =  0 );{$ENDIF}
