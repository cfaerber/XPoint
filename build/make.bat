@ECHO OFF
REM $Id$
MD DISTRI
CD XPDIR
PKZIP -rp ..\DISTRI\OXP340.ZIP *.*
PKZIP -rp ..\DISTRI\OXP340UP.ZIP @..\UPDATE.LST
CD ..
CD ..
PKZIP -rp -x@BUILD\EXCLUDE.LST BUILD\DISTRI\OXP340S.ZIP *.*
GOTO ENDE
{
  $Log$
  Revision 1.1.4.2  2003/04/11 09:21:02  mw
  MW: - Neues Buildsystem f�r Openxp/16 3.40 implementiert

}
:ENDE