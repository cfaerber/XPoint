@echo off

echo Versionsnummer aktualisieren
svn up >version.svn
cd ..

echo Exportiere Sourcen
rmdir \source\openxp_4.00 /s /Q
svn export \svn\openxp_4.00 c:\source\openxp_4.00

echo Erstelle Versions-Informationen
cd \svn\openxp_4.00\build
\perl\bin\perl get_build_nr.pl >\temp\variables.cmd
call \temp\variables.cmd
echo Version: %OXP_VER%
echo Options: %OXP_OPTS%

cd \source\openxp_4.00

echo Compilere mit FreePascal fuer DOS 32
del c:\xpexe\units\*.* /Q
\fpc\2.0.0\bin\go32v2\ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\dos -FU\xpexe\units -FuObjCOM;netcall -FuDOS32 -Tgo32v2 openxp.pas >!compdos.bak

echo Compilere mit FreePascal fuer Win32

del c:\xpexe\units\*.* /Q
ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\win32 -FU\xpexe\units -FuObjCOM;netcall  -TWin32 openxp.pas >!compwin.bak

echo Compiliere Doku
cd \source
xparchiv\docform openxp_4.00\doc\fido.dq xpd_370d\doc\fido.txt 68 8
xparchiv\docform openxp_4.00\doc\uucp.dq xpd_370d\doc\uucp.txt 68 8
copy xpd_370d\doc\fido.txt xpd_370w\doc
copy xpd_370d\doc\uucp.txt xpd_370w\doc

echo Compiliere Hilfe
xparchiv\ihs_38 openxp_4.00\doc\openxp-d xpd_370d
xparchiv\ihs_38 openxp_4.00\doc\openxp-e xpd_370d
copy xpd_370d\openxp*.hlp xpd_370w

echo Compiliere Hilfe
xparchiv\rc_38 openxp_4.00\openxp-d xpd_370d
xparchiv\rc_38 openxp_4.00\openxp-e xpd_370d
copy xpd_370d\openxp-?.res xpd_370w

echo Packe Sourcen
cd \source\openxp_4.00
copy \svn\openxp_4.00\build\file_id.source file_id.diz
rar a \a\openxp-%OXP_VER%.src.rar -cl -r -s -ts- -mc16:128t+ -x!com*.bak -m5 >\temp\pack

echo Packe Windows 32 Bit
cd ..\xpd_370w
copy \xpexe\win32\openxp.exe openxp.exe
copy \svn\openxp_4.00\build\file_id.win file_id.diz
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.win.rar >\temp\pack

echo Packe DOS 32 Bit
cd ..\xpd_370d
copy \xpexe\dos\openxp.exe openxp.exe
copy \svn\openxp_4.00\build\file_id.dos file_id.diz
rar a -cl -r -m5 -s -md512 -ts- \a\openxp-%OXP_VER%.dos.rar >\temp\pack

echo Lösche alte Dateien
del c:\xpexe\units\*.* /Q
rem del c:\xpexe\dos\*.* /Q
del c:\xpexe\win32\*.* /Q
rem del \source\xpd_370d\openxp*.exe
del \source\xpd_370w\openxp*.exe
cd \svn\openxp_4.00\build
del file_id.dos file_id.win file_id.source version.sfn openxp-*.spec
