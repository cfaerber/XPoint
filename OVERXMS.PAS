{ OVERXMS - Loads overlays in XMS.  Written by Wilbert van Leijen }
{ $Id$ }

Unit OverXMS;

{$O- }

Interface
uses Overlay;

Const
  ovrNoXMSDriver = -7;                 { No XMS driver installed }
  ovrNoXMSMemory = -8;                 { Insufficient XMS memory 
available }

Procedure OvrInitXMS;

Implementation

Procedure OvrInitXMS; External;
{$L OVERXMS.OBJ }

end.  { OverXMS }
{
  $Log$
  Revision 1.1.1.1  2003/01/10 17:59:35  mw
  Import OVRXMS

}
 