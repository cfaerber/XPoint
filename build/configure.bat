@echo off
rem $Id: configure.bat 6891 2004-11-25 17:31:53Z mkaemmerer $

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
