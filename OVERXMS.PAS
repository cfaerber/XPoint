{ OVERXMS - Loads overlays in XMS.  Written by Wilbert van Leijen }
{ $Id$ }

Unit OverXMS;

{$O- }

Interface
uses Overlay;

Const
  ovrNoXMSDriver = -7;                 { No XMS driver installed }
  ovrNoXMSMemory = -8;                 { Insufficient XMS memory available }

Var 
  ovrmemsize : word;

Procedure OvrInitXMS;

Implementation

Procedure OvrInitXMS; External;
{$L OVERXMS.OBJ }

end.  { OverXMS }
{
  $Log$
  Revision 1.1.1.2  2003/04/16 13:48:20  mw
  MW: - Neue Berechnungsmethode fÅr XMS-Belegung durch Openxp/16
        (In overxms.asm wird die Grî·e des XMS-Blocks gesichert).

  Revision 1.1.1.1  2003/01/10 17:59:35  mw
  Import OVRXMS

}
 