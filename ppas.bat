@echo off
SET THEFILE=openxp.exe
echo Linking %THEFILE%
c:\programme\freepascal\bin\win32\ldw.exe  -s   -b base.$$$ -o openxp.exe link.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
