@echo off
cd \source
xparchiv\docform openxp\doc\xpoint.dq xpredist\doc\xpoint.doc 68 8
xparchiv\docform openxp\doc\fido.dq xpredist\doc\fido.doc 68 8
xparchiv\docform openxp\doc\uucp.dq xpredist\doc\uucp.doc 68 8
xparchiv\ihs openxp\doc\xp
xparchiv\ihs openxp\doc\xp-e
xparchiv\rc openxp\xp-d
xparchiv\rc openxp\xp-e

copy xp-d.res ..\xpredist\
copy xp-d.res e:\xp
copy xp-e.res ..\xpengl\
copy \xp\xp.exe \xp\xp.ovr \xp\uuz.exe \xp\zfido.exe xpredist
copy \xp\maggi.exe \xp\ndiff.exe \xp\xp-fm.exe \xp\zpr.exe xpredist

copy \Homepage\xp\logkurz.html ..\xpredist\doc\logkurz.htm
copy \Homepage\xp\log.html doc\log.htm

cd openxp
rarnt a e:\a\oxp320ds -cl -r -s -m5 -md1024 *.txt *.pas *.asm *.inc *.dpr *.dq *.rq *.ihq *.obj *.bat file_id.diz
cd ..\xpredist
c:\packer\zipnt -add -maximum -recurse -path e:\a\oxp320db
c:\packer\zipnt -add -maximum e:\a\oxp320du xp.exe xp.ovr xp-d.res uuz.exe zpr.exe zfido.exe ndiff.exe maggi.exe xp-fm.exe xp.hlp readme.txt file_id.diz logsmall.htm
cd ..\xpengl
c:\packer\zipnt -add -maximum e:\a\oxp320de
cd e:\xpexe
c:\packer\zipnt -add -maximum -recurse -path e:\a\oxp320d! zpr.exe ndiff.exe
cd e:\xp
rarnt a e:\a\New_EXE -cl -s -m5 -md1024 xp.exe xp.ovr xp-d.res
