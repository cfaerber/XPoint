@ECHO OFF
REM $Id$
MD DISTRI
CD XPDIR
PKZIP -rp ..\DISTRI\OXP320.ZIP *.*
PKZIP -rp ..\DISTRI\OXP320UP.ZIP @..\UPDATE.LST
CD ..
CD ..
PKZIP -rp -x@BUILD\EXCLUDE.LST BUILD\DISTRI\OXP320S.ZIP *.*
GOTO ENDE
{
  $Log$
  Revision 1.1.2.3  2003/04/08 23:36:47  mw
  MW: make.bat erstellt jetzt auch Update und Sourcepaket

  Revision 1.1.2.2  2003/04/08 21:56:58  mw
  MW: - Neue Dateien in DOC

}
:ENDE