@echo off
cd doc
docform xpoint.dq xpoint.doc 68 8
docform fido.dq fido.doc 68 8
docform uucp.dq uucp.doc 68 8
ihs xp
ihs xp-e

cd ..
..\xparchiv\rc xp-d
..\xparchiv\rc xp-e
copy doc\*.doc ..\xpredist\doc
copy doc\xp.hlp ..\xpredist\
copy doc\xp-e.hlp ..\xpengl\
copy xp-d.res ..\xpredist\
copy xp-d.res e:\xp
copy xp-e.res ..\xpengl\
copy \xp\xp.exe ..\xpredist\
copy \xp\xp.ovr ..\xpredist\
copy \xp\uuz.exe ..\xpredist\
copy \xp\zfido.exe ..\xpredist\
copy \xp\maggi.exe ..\xpredist\
copy \xp\ndiff.exe ..\xpredist\
copy \xp\xp-fm.exe ..\xpredist\
copy \xp\zpr.exe ..\xpredist
copy \Homepage\xp\logkurz.html ..\xpredist\doc\logkurz.htm
copy \Homepage\xp\log.html doc\log.htm

rarnt a e:\a\oxp320ds -cl -r -s -m5 -md1024 *.txt *.pas *.asm *.inc *.dpr *.dq *.rq *.ihq *.obj *.bat file_id.diz
cd ..\xpredist
c:\packer\zipnt -add -maximum -recurse -path e:\a\oxp320db
rem c:\packer\rar a e:\a\oxp320db -sfxdos.sfx -r -cl -s -m5 
c:\packer\zipnt -add -maximum e:\a\oxp320du xp.exe xp.ovr xp-d.res uuz.exe zpr.exe zfido.exe ndiff.exe maggi.exe xp-fm.exe xp.hlp readme.txt file_id.diz logsmall.htm
cd ..\xpengl
c:\packer\zipnt -add -maximum e:\a\oxp320de
cd e:\xpexe
c:\packer\zipnt -add -maximum -recurse -path e:\a\oxp320d! zpr.exe ndiff.exe