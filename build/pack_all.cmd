@echo off

echo Versionsnummer aktualisieren
svn up >version.sfn
cd ..

echo Exportiere Sourcen
rmdir \source\openxpb /s /Q
svn export \svn\openxpb c:\source\openxpb

echo Erstelel Versions-Informationen
cd \svn\openxpb\build
\perl\bin\perl get_build_nr.pl >\temp\variables.cmd
call \temp\variables.cmd
echo Version: %OXP_VER%
echo Options: %OXP_OPTS%

cd \source\openxpb

rem echo Compilere mit FreePascal fuer DOS 32
rem del c:\xpexe\units\*.* /Q
rem ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\dos   -FU\xpexe\units -FuObjCOM;netcall -FuDOS32 -Tgo32v2 openxp.pas

echo Compilere mit FreePascal fuer Win32

del c:\xpexe\units\*.* /Q
ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\win32 -FU\xpexe\units -FuObjCOM;netcall  -TWin32 openxp.pas >!compwin.bak

echo Compiliere Doku
cd \source
xparchiv\docform openxpb\doc\fido.dq xpd_370d\doc\fido.txt 68 8
xparchiv\docform openxpb\doc\uucp.dq xpd_370d\doc\uucp.txt 68 8
copy xpd_370d\doc\fido.txt xpd_370w\doc
copy xpd_370d\doc\uucp.txt xpd_370w\doc

echo Compiliere Hilfe
xparchiv\ihs_38 openxpb\doc\openxp-d xpd_370d
xparchiv\ihs_38 openxpb\doc\openxp-e xpd_370d
copy xpd_370d\openxp*.hlp xpd_370w

echo Compiliere Hilfe
xparchiv\rc_38 openxpb\openxp-d xpd_370d
xparchiv\rc_38 openxpb\openxp-e xpd_370d
copy xpd_370d\openxp-?.res xpd_370w

echo Packe Sourcen
cd \source\openxpb
copy \svn\openxpb\build\file_id.source file_id.diz
rar a \a\openxp-%OXP_VER%.src.rar -r -s -ts- -x!com*.bak -m5 >\temp\pack

echo Packe Windows 32 Bit
cd ..\xpd_370w
copy \xpexe\win32\openxp.exe openxp.exe
copy \svn\openxpb\build\file_id.win file_id.diz
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.win.rar >\temp\pack

rem echo Packe DOS 32 Bit
rem cd ..\xpd_370d
rem copy \xpexe\dos\*.exe
rem copy \svn\openxpb\build\file_id.dos file_id.diz
rem rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.dos.rar >\temp\pack

echo L�sche alte Dateien
del c:\xpexe\units\*.* /Q
del c:\xpexe\dos\*.* /Q
del c:\xpexe\win32\*.* /Q
del \source\xpd_370d\openxp*.exe
del \source\xpd_370w\openxp*.exe
cd \svn\openxpb\build
del file_id.dos file_id.win file_id.source version.sfn openxp-*.spec