{   $Id$

    OpenXP declarations unit
    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2002 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

{ OpenXP const declarations unit }
unit xpconst;

interface

uses
  xpglobal;
  
const
{$IFDEF UnixFS }
       PufferFile  = 'puffer';        { Z-Netz-Puffer }
       XFerDir_    = 'spool';         { eingehende Mailbatches }
       XFerDir     = XFerDir_+DirSepa;
       JanusDir_   = XFerDir+'janus';
       JanusDir    = JanusDir_+DirSepa;
       InfileDir   = 'files'+DirSepa; { Default: Filerequests }
       AutoxDir    = 'autoexec'+DirSepa;    { AutoStart-Daten }
       AutoVDir    = 'autosend'+DirSepa;
       BadDir      = 'bad'+DirSepa;

       HeaderFile  = 'header.xps';     { Schablonen-Dateien }
       HeaderPriv  = 'privhead.xps';
       SignatFile  = 'signatur.xps';
       PrivSignat  = 'privsig.xps';
       QuoteMsk    = 'qbrett.xps';
       QuotePriv   = 'qpriv.xps';
       QuotePMpriv = 'qpmpriv.xps';
       QuoteToMsk  = 'quoteto.xps';
       WeiterMsk   = 'weiter.xps';
       ErneutMsk   = 'erneut.xps';
       EB_Msk      = 'empfbest.xps';
       CancelMsk   = 'cancel.xps';

       extBfg      = '.bfg';           { Boxen-Config-File }
       extQfg      = '.qfg';           { QWK-Config-File   }
       extSwap     = '.swp';
       extBatch    = '.sh';
       extBoxfile  = '.pp';
       extEBoxfile = '.epp';
       extFbl      = '.fbl';
       extBl       = '.bl';
       extRc       = '.rc';
       extMid      = '.mid';
       extFl       = '.fl';
       extGr       = '.gr';
       extInf      = '.inf';
       extUdl      = '.udl';
       extCfg      = '.cfg';
       extBbl      = '.bbl';
       extHelp     = '.hlp';
       extXps      = '.xps';
       extBak      = '.bak';
       extIn       = '.in';
       extOut      = '.out';
       extMsg      = '.msg';
{$ELSE}
       PufferFile  = 'PUFFER';        { Z-Netz-Puffer }
       XFerDir_    = 'SPOOL';         { eingehende Mailbatches }
       XFerDir     = XFerDir_+DirSepa;
       JanusDir_   = XFerDir+'JANUS';
       JanusDir    = JanusDir_+DirSepa;
       InfileDir   = 'FILES'+DirSepa; { Default: Filerequests }
       AutoxDir    = 'AUTOEXEC'+DirSepa;    { AutoStart-Daten }
       AutoVDir    = 'AUTOSEND'+DirSepa;
       BadDir      = 'BAD'+DirSepa;
       HeaderFile  = 'HEADER.XPS';     { Schablonen-Dateien }
       HeaderPriv  = 'PRIVHEAD.XPS';
       SignatFile  = 'SIGNATUR.XPS';
       PrivSignat  = 'PRIVSIG.XPS';
       QuoteMsk    = 'QBRETT.XPS';
       QuotePriv   = 'QPRIV.XPS';
       QuotePMpriv = 'QPMPRIV.XPS';
       QuoteToMsk  = 'QUOTETO.XPS';
       WeiterMsk   = 'WEITER.XPS';
       ErneutMsk   = 'ERNEUT.XPS';
       EB_Msk      = 'EMPFBEST.XPS';
       CancelMsk   = 'CANCEL.XPS';

       extBfg      = '.BFG';           { Boxen-Config-File }
       extQfg      = '.QFG';           { QWK-Config-File   }
       extSwap     = '.SWP';
       extBatch    = '.BAT';
       extBoxfile  = '.PP';
       extEBoxFile = '.EPP';
       extFbl      = '.FBL';
       extBl       = '.BL';
       extRc       = '.RC';
       extMid      = '.MID';
       extFl       = '.FL';
       extGr       = '.GR';
       extInf      = '.INF';
       extUdl      = '.UDL';
       extCfg      = '.CFG';
       extBbl      = '.BBL';
       extHelp     = '.HLP';
       extXps      = '.XPS';
       extBak      = '.BAK';
       extIn       = '.IN';
       extOut      = '.OUT';
       extMsg      = '.MSG';
{$ENDIF }

implementation

{
  $Log: xpconst.pas,v $
  Revision 1.1  2004/01/17 16:33:47  mk
  - split xp0.pas in xp0.pas and xpconst.pas to remove some dependencies
    xpconst.pas should be used for global constants (only!)

}
end.

