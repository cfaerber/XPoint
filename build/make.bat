@ECHO OFF
REM $Id$
MD DISTRI
CD XPDIR
PKZIP -rp ..\DISTRI\OXP320.ZIP *.*
CD ..
GOTO ENDE
{
  $Log$
  Revision 1.1.2.2  2003/04/08 21:56:58  mw
  MW: - Neue Dateien in DOC

}
:ENDE