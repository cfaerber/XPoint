@echo off
rem $Id: configure.bat,v 1.2 2003/12/13 22:49:50 cl Exp $

if "%PPC%"=="" GOTO noenv
Testing %PPC%...
%PPC% build\configure.pas
if ERRORLEVEL 1 GOTO noenv
build\configure.exe
IF ERRORLEVEL 1 GOTO noenv
GOTO ok

:noenv

echo Testing Free PASCAL compiler...
ppc386 build\configure.pas
IF ERRORLEVEL 1 GOTO nofpc
build\configure.exe
IF ERRORLEVEL 1 GOTO nofpc
GOTO ok

:nofpc

echo Testing Delphi compiler...
dcc32 build\configure.pas
IF ERRORLEVEL 1 GOTO nodelphi
build\configure.exe
IF ERRORLEVEL 1 GOTO nodelphi
GOTO OK

:nodelphi

:error
echo Could not find working PASCAL compiler.
GOTO end

:ok
echo OpenXP is configured, run make.bat to build.

:end
