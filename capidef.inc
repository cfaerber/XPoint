{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }


const  CONNECT_REQ            = $0200;
       CONNECT_CONF           = $0201;
       CONNECT_IND            = $0202;
       CONNECT_RESP           = $0203;
       CONNECT_INFO_REQ       = $0900;
       CONNECT_INFO_CONF      = $0901;
       CONNECT_ACTIVE_IND     = $0302;
       CONNECT_ACTIVE_RESP    = $0303;
       DISCONNECT_REQ         = $0400;
       DISCONNECT_CONF        = $0401;
       DISCONNECT_IND         = $0402;
       DISCONNECT_RESP        = $0403;
       LISTEN_REQ             = $0500;
       LISTEN_CONF            = $0501;
       GET_PARAMS_REQ         = $0600;
       GET_PARAMS_CONF        = $0601;
       INFO_REQ               = $0700;
       INFO_CONF              = $0701;
       INFO_IND               = $0702;
       INFO_RESP              = $0703;
       DATA_REQ               = $0800;
       DATA_CONF              = $0801;
       DATA_IND               = $0802;
       DATA_RESP              = $0803;

       SELECT_B2_PROTO_REQ    = $4000;
       SELECT_B2_PROTO_CONF   = $4001;

       SELECT_B3_PROTO_REQ    = $8000;
       SELECT_B3_PROTO_CONF   = $8001;
       LISTEN_B3_REQ          = $8100;
       LISTEN_B3_CONF         = $8101;
       CONNECT_B3_REQ         = $8200;
       CONNECT_B3_CONF        = $8201;
       CONNECT_B3_IND         = $8202;
       CONNECT_B3_RESP        = $8203;
       CONNECT_B3_ACTIVE_IND  = $8302;
       CONNECT_B3_ACTIVE_RESP = $8303;
       DISCONNECT_B3_REQ      = $8400;
       DISCONNECT_B3_CONF     = $8401;
       DISCONNECT_B3_IND      = $8402;
       DISCONNECT_B3_RESP     = $8403;
       GET_B3_PARAMS_REQ      = $8500;
       GET_B3_PARAMS_CONF     = $8501;
       DATA_B3_REQ            = $8600;
       DATA_B3_CONF           = $8601;
       DATA_B3_IND            = $8602;
       DATA_B3_RESP           = $8603;
       RESET_B3_REQ           = $0100;
       RESET_B3_CONF          = $0101;
       RESET_B3_IND           = $0102;
       RESET_B3_RESP          = $0103;

       HANDSET_IND            = $8702;
       HANDSET_RESP           = $8703;

       MANUFACTURER_REQ       = $ff00;
       MANUFACTURER_CONF      = $ff01;
       MANUFACTURER_IND       = $ff02;
       MANUFACTURER_RESP      = $ff03;

