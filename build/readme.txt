From: roo@wombat.dyn.han.de (=?ISO-8859-1?Q?Dr._Christian_B=F6ttger?=)
Subject: OXP RPM Builds

Hallo,

anbei meine modifizierten Build-Scripte, falls sie mal jemand braucht. =20
Meinetwegen k=F6nnt ihr die auch irgendwo zug=E4nglich ablegen, passwords  =

sollten unkenntlich sein.

snapshot :l=E4uft als cron job t=E4glich einmal, sofern im dailydiff auch w=
as =20
drin ist

buildxp: baut oxp 3.7.8 und 3.7.9 aus den aktuellen Quellen

makexp: die reinen compiler-Aufrufe

make-rpm* : werden von snapshot aufgerufen

ansonsten habe ich noch diverse Konfigs mit reingepackt (ppc386, kylix).

Das ganze liegt lokal bei mir in /home/boettger/openxp/openxp/
fpc output geht nach /home/boettger/openxp/openxp/output/
kylix output geht nach /home/boettger/openxp/openxp/kylixout/
(siehe scripte)
