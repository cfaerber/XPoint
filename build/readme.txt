From: roo@wombat.dyn.han.de (Dr. Christian Böttger?=)
Subject: OXP RPM Builds

Hallo,

anbei meine modifizierten Build-Scripte, falls sie mal jemand braucht. 
Meinetwegen könnt ihr die auch irgendwo zugänglich ablegen, passwords  
sollten unkenntlich sein.

mkdailysnap :läuft als cron job täglich einmal, sofern im dailydiff auch
was drin ist

buildxp: baut oxp 3.8 und 3.9 aus den aktuellen Quellen

makexp: die reinen compiler-Aufrufe

make-rpm.sh : werden von snapshot aufgerufen, baut die RPMs

ansonsten habe ich noch diverse Konfigs mit reingepackt (ppc386, kylix).

Das ganze liegt lokal bei mir in /home/boettger/openxp/script/
die diffs werden in /home/openxp/diff/ abgelegt
Sourcen sind in /home/boettger/openxp/<version>/openxp/
fpc output geht nach /home/boettger/openxp/<version>/output/
kylix output geht nach /home/boettger/openxp/<version>/kylixout/
(siehe scripte)
