@echo off
echo Compilere mit Borland Pascal 7.0
\bp\bin\bpc xp.pas -b -e\xp -gd >!compdos.bp
\bp\bin\bpc zpr.pas -b -e\xp -gd >>!compdos.bp
\bp\bin\bpc uuz.pas -b -e\xp -gd >>!compdos.bp
\bp\bin\bpc zfido.pas -b -e\xp -gd >>!compdos.bp
\bp\bin\bpc maggi.pas -b -e\xp -gd >>!compdos.bp
\bp\bin\bpc ndiff.pas -b -e\xp -gd >>!compdos.bp
\bp\bin\bpc xp-fm.pas -b -e\xp -gd >>!compdos.bp
echo Compilere mit FreePascal für DOS 
ppc386 -B -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units xp.pas >!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units zpr.pas >>!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units uuz.pas >>!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units zfido.pas >>!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units maggi.pas >>!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units ndiff.pas >>!compdos.fpc
ppc386 -TGo32V2 -FEe:\xpexe\dos\fpc -FUe:\xpexe\dos\fpc\units xp-fm.pas >>!compdos.fpc
echo Compilere mit FreePascal für Win32
ppc386 -B -FUe:\xpexe\win32\fpc\units -TWin32 xp.pas >!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 zpr.pas >>!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 uuz.pas >>!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 zfido.pas >>!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 maggi.pas >>!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 ndiff.pas >>!compwin32.fpc
ppc386 -FUe:\xpexe\win32\fpc\units -TWin32 xp-fm.pas >>!compwin32.fpc
del e:\xpexe\win32\fpc\*.exe
mv *.exe e:\xpexe\win32\fpc
echo Compilere mit FreePascal für OS/2
ppc386 -B -FUe:\xpexe\os2\fpc\units -Tos2 xp.pas >!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 zpr.pas >>!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 uuz.pas >>!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 zfido.pas >>!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 maggi.pas >>!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 ndiff.pas >>!compos2.fpc
ppc386 -FUe:\xpexe\os2\fpc\units -Tos2 xp-fm.pas >>!compos2.fpc
del e:\xpexe\os2\fpc\*.exe
mv *.exe e:\xpexe\os2\fpc
echo Compilere mit TMT Pascal für DOS 
tmtpc -$Logo- -EXEMAX:2000000 -out:e:\xpexe\dos\tmt\ xp.pas >!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ zpr.pas >>!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ uuz.pas >>!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ zfido.pas >>!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ maggi.pas >>!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ ndiff.pas >>!compdos.tmt
tmtpc -$Logo- -out:e:\xpexe\dos\tmt\ xp-fm.pas >>!compdos.tmt
rem \delphi~1\bin\dcc32 zpr.pas -b -ee:\xpexe\win32\delphi -ne:\xpexe\win32\delphi\units >>!compdos.bp
del \xpexe\dos\tmt\*.fpd 
del \xpexe\dos\tmt\*.sym