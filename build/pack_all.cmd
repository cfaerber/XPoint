@echo off

cd ..

echo Exportiere Sourcen
rmdir dist_temp /s /Q
svn export . dist_temp

echo Versionsnummer aktualisieren
svn up >dist_temp\build\version.svn

cd dist_temp\build

echo Erstelle Versions-Informationen
"c:\Program Files\Perl\bin\perl" get_build_nr.pl >variables.cmd
call variables.cmd
echo Version: %OXP_VER%
echo Options: %OXP_OPTS%

cd ..

echo Packe Sourcen
rar a \a\openxp-%OXP_VER%.src.rar -cl -r -s -ts- -mc16:128t+ -x!com*.bak -m5 >>\a\openxp_build.log

echo Compilere mit FreePascal fuer DOS 32
c:\Development\FPC\bin\go32v2\ppc386 -B %OXP_OPTS% -Xs -XX -FEdist\dos  -FUdist -FuObjCOM;netcall;DOS32;xplib -Tgo32v2 openxp.pas >>\a\openxp_build.log

echo Compilere mit FreePascal fuer Win32
c:\Development\FPC\bin\i386-win32\ppc386 -B %OXP_OPTS% -Xs -XX -FuObjCOM;netcall;xplib -FEdist\win32 -FUdist -TWin32 openxp.pas >>\a\openxp_build.log
del *.ppu *.o /s
c:\Development\FPC\bin\i386-win32\ppc386 -B %OXP_OPTS% -FuObjCOM;netcall;xplib -FUdist -TWin32 rc.pas >>\a\openxp_build.log
del *.ppu *.o /s
c:\Development\FPC\bin\i386-win32\ppc386 -B %OXP_OPTS% -FuObjCOM;netcall;xplib -FUdist -TWin32 docform.pas >>\a\openxp_build.log
del *.ppu *.o /s
c:\Development\FPC\bin\i386-win32\ppc386 -B %OXP_OPTS% -FuObjCOM;netcall;xplib -FUdist -TWin32 ihs.pas >>\a\openxp_build.log

echo Compiliere Doku
docform doc\fido.dq dist\fido.txt 68 8
docform doc\uucp.dq dist\uucp.txt 68 8

echo Compiliere Hilfe
ihs doc\openxp-d dist
ihs doc\openxp-e dist

echo Compiliere Ressourcen
rc openxp-d dist
rc openxp-e dist

echo Verteile Dateien
copy dist\openxp-*.* dist\dos
copy dist\openxp-*.* dist\win32
copy dist\*.txt dist\dos\doc
copy dist\*.txt dist\win32\doc

mkdir dist\dos\beispiel
copy beispiel dist\dos\beispiel
mkdir dist\win32\beispiel
copy beispiel dist\win32\beispiel
mkdir dist\dos\samples
copy samples dist\dos\samples
mkdir dist\win32\samples
copy samples dist\win32\samples

echo Packe Windows 32 Bit
cd dist\win32
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.win.rar >>\a\openxp_build.log

echo Packe DOS 32 Bit
cd ..\dos
rar a -cl -r -s -ts- -m5 \a\openxp-%OXP_VER%.dos.rar >>\a\openxp_build.log

echo Lösche alte Dateien
cd ..\..\..
rmdir dist_temp /s /Q

