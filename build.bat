@echo off
rem OpenXP "make" Batchdatei: Kompiliert alle fuer XP relevanten
rem Programme und Resourcen.
rem Compiler ggf. als Kommandozeilenparameter uebergeben.
rem Es muss ein separates Verzeichnis fuer die Kompilate
rem angegeben sein (bei FPC: -fU[pfad]).
if "%comp%"=="" set comp=%1
if "%comp%"=="" set comp=ppc386
if "%target%"=="" set target=%2
if "%target%"=="" set target=win32
set arg=
if "%target%"=="dos32" set arg=-TGO32V2 -Fu.\dos32
if "%target%"=="win32" set arg=-TWin32
if not "%arg%"=="" goto ok
echo Platform must be dos32 or win32.
goto Fehler
:ok
%comp% -ddevelop %arg% -Fuxplib -Fu.\objcom -Fu.\netcall -Fi.\charsets openxp
if errorlevel 1 goto Fehler
%comp% -Fu.\objcom -Fuxplib -Fu.\netcall -Fi.\charsets ndiff
if errorlevel 1 goto Fehler
%comp% -Fu.\objcom -Fuxplib -Fu.\netcall -Fi.\charsets rc
if errorlevel 1 goto Fehler
%comp% -Fu.\objcom -Fuxplib -Fu.\netcall -Fi.\charsets ihs
if errorlevel 1 goto Fehler
rc openxp-d.rq
if errorlevel 1 goto Fehler
rc openxp-e.rq
if errorlevel 1 goto Fehler
ihs doc\openxp-d
if errorlevel 1 goto Fehler
ihs doc\openxp-e
if errorlevel 1 goto Fehler

:Ende
echo Fertig.
goto Ex

:Fehler
echo Es ist ein Fehler aufgetreten.
goto Ex

:Ex
