@echo off
rem OpenXP "make" Batchdatei: Kompiliert alle fuer XP relevanten
rem Programme und Resourcen.
rem Compiler ggf. als Kommandozeilenparameter uebergeben.
rem Es muss ein separates Verzeichnis fuer die Kompilate
rem angegeben sein (bei FPC: -fU[pfad]).
if "%comp%"=="" set comp=%1
if "%comp%"=="" set comp=ppc386
%comp% xp
if errorlevel 1 goto Fehler
%comp% objcom/objcom
if errorlevel 1 goto Fehler
%comp% xp-fm
if errorlevel 1 goto Fehler
%comp% uuz
if errorlevel 1 goto Fehler
rem %comp% maggi
rem if errorlevel 1 goto Fehler
rem %comp% pmconv
rem if errorlevel 1 goto Fehler
rem %comp% uucp-fl1
rem if errorlevel 1 goto Fehler
rem %comp% yup2pkt
rem if errorlevel 1 goto Fehler
%comp% ndiff
if errorlevel 1 goto Fehler
%comp% rc
if errorlevel 1 goto Fehler
%comp% ihs
if errorlevel 1 goto Fehler
rc xp-d.rq
if errorlevel 1 goto Fehler
rc xp-e.rq
if errorlevel 1 goto Fehler
rc xpfm-d.rq
if errorlevel 1 goto Fehler
rc xpfm-e.rq
if errorlevel 1 goto Fehler
ihs doc\xp
if errorlevel 1 goto Fehler
ihs doc\xp-e
if errorlevel 1 goto Fehler

:Ende
echo Fertig.
goto Ex

:Fehler
echo Es ist ein Fehler aufgetreten.
goto Ex

:Ex
