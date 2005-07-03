@echo off
echo Lösche alte Dateien

echo Versionsnummer aktualisieren
cd openxpb
svn up >build\version.sfn
echo exportiere Sourcen
rmdir \source\openxpb /s
svn export \svn\openxpb c:\source\openxpb

cd \svn\openxpb\build
\perl\bin\perl get_build_nr.pl >\temp\variables.cmd
call \temp\variables.cmd
echo Version: %OXP_VER%
echo Options: %OXP_OPTS%
cd \source\openxpb
echo Compilere mit FreePascal fuer DOS 32


del c:\xpexe\units\*.* /Q
ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\dos   -FU\xpexe\units -FuObjCOM;netcall -FuDOS32 -Tgo32v2 openxp.pas

echo Compilere mit FreePascal fuer Win32

del c:\xpexe\units\*.* /Q
ppc386 -B %OXP_OPTS% -Xs -XX -FE\xpexe\win32 -FU\xpexe\units -FuObjCOM;netcall  -TWin32 openxp.pas >!compwin.bak

echo Compiliere mit Delphi für Win32
rem \delphi6\bin\dcc32 -B -Ec:\xpexe\delphi -Unetcall;objcom;delphi openxp >!compwin_d.bak

cd \source
xparchiv\docform openxpb\doc\fido.dq xpd_370d\doc\fido.txt 68 8
xparchiv\docform openxpb\doc\uucp.dq xpd_370d\doc\uucp.txt 68 8
copy xpd_370d\doc\fido.txt xpd_370w\doc
copy xpd_370d\doc\uucp.txt xpd_370w\doc

xparchiv\ihs_38 openxpb\doc\openxp-d xpd_370d
xparchiv\ihs_38 openxpb\doc\openxp-e xpd_370d
copy xpd_370d\openxp*.hlp xpd_370w

xparchiv\rc_38 openxpb\openxp-d xpd_370d
xparchiv\rc_38 openxpb\openxp-e xpd_370d
copy xpd_370d\openxp-?.res xpd_370w

cd \sourc\openxpb
del icons.owr
echo Packe Sourcen
copy \source\openxpb\build\file_id.source file_id.diz
rar a \a\openxp-%OXP_VER%.src.rar -r -s -ts- -x!com*.bak -m5 >\temp\pack

echo Packe Windows 32 Bit
cd ..\xpd_370w
copy \xpexe\delphi\openxp_delphi.exe .
copy \xpexe\win32\openxp.exe openxp.exe
copy \source\openxpb\build\file_id.win file_id.diz
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.win.rar >\temp\pack

echo Packe DOS 32 Bit
cd ..\xpd_370d
copy \xpexe\dos\*.exe
copy \source\openxpb\build\file_id.dos file_id.diz
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.dos.rar >\temp\pack

echo Lösche alte Dateien
del c:\xpexe\units\*.* /Q
del c:\xpexe\dos\*.* /Q
del c:\xpexe\win32\*.* /Q
del \source\xpd_370d\openxp*.exe
del \source\xpd_370w\openxp*.exe

