program openxp;

{$I xpdefine.inc }

{%File 'xpmecol.inc'}
{%File 'databas2.inc'}
{%File 'database.inc'}
{%File 'editor.inc'}
{%File 'maske.inc'}
{%File 'osdbsd.inc'}
{%File 'osddos32.inc'}
{%File 'osdlinux.inc'}
{%File 'osdos2.inc'}
{%File 'osdwin32.inc'}
{%File 'xp10.inc'}
{%File 'xp10p.inc'}
{%File 'xp1menu.inc'}
{%File 'xp1s.inc'}
{%File 'xp2cfg.inc'}
{%File 'xp3o.inc'}
{%File 'xp4.inc'}
{%File 'xp4d.inc'}
{%File 'xp4o.inc'}
{%File 'xp4w.inc'}
{%File 'xp8.inc'}
{%File 'xp8fs.inc'}
{%File 'xpconfigedit-groups.inc'}
{%File 'xpconfigedit-mimetypes.inc'}
{%File 'xpconfigedit-pseudos.inc'}
{%File 'xpconfigedit-servers.inc'}
{%File 'xpconfigedit-systems.inc'}
{%File 'xpdefine.inc'}
{%File 'xpf1.inc'}
{%File 'xpfiles.inc'}
{%File 'databas1.inc'}
{%File 'netcall\ncuucp-t.inc'}
{%File 'netcall\ncfido-emsi.inc'}
{%File 'netcall\ncfido-wazoo.inc'}
{%File 'netcall\ncfido-yoohoo.inc'}
{%File 'netcall\ncuucp-e.inc'}
{%File 'netcall\ncuucp-fz.inc'}
{%File 'netcall\ncuucp-g.inc'}
{%File 'netcall\ncfido-binkp.inc'}
{%File 'ObjCOM\ocsdos.inc'}
{%File 'xpsendmessage_subs.inc'}

uses
  zpr in 'zpr.pas',
  clip in 'clip.pas',
  CRC in 'crc.pas',
  database in 'database.pas',
  databaso in 'databaso.pas',
  datadef in 'datadef.pas',
  datadef1 in 'datadef1.pas',
  Debug in 'debug.pas',
  Direct in 'direct.pas',
  eddef in 'eddef.pas',
  editor in 'editor.pas',
  encoder in 'encoder.pas',
  feiertag in 'feiertag.pas',
  fidoglob in 'fidoglob.pas',
  fileio in 'fileio.pas',
  GPLTools in 'gpltools.pas',
  help in 'help.pas',
  inout in 'inout.pas',
  IPAddr in 'ipaddr.pas',
  keys in 'keys.pas',
  lister in 'lister.pas',
  Log in 'log.pas',
  maske in 'maske.pas',
  maus2 in 'maus2.pas',
  MD5 in 'md5.pas',
  montage in 'montage.pas',
  mouse in 'mouse.pas',
  ndiff in 'ndiff.pas',
  ZFTools in 'zftools.pas',
  printerx in 'printerx.pas',
  ProgressOutput in 'progressoutput.pas',
  replytoall in 'replytoall.pas',
  resource in 'resource.pas',
  stack in 'stack.pas',
  StringTools in 'stringtools.pas',
  Timer in 'timer.pas',
  typeform in 'typeform.pas',
  Unicode in 'unicode.pas',
  UTFTools in 'utftools.pas',
  viewer in 'viewer.pas',
  win2 in 'win2.pas',
  winxp in 'winxp.pas',
  xp_des in 'xp_des.pas',
  xp_iti in 'xp_iti.pas',
  xp_pgp in 'xp_pgp.pas',
  xp_uue in 'xp_uue.pas',
  xp0 in 'xp0.pas',
  xp1 in 'xp1.pas',
  xp10 in 'xp10.pas',
  xp1help in 'xp1help.pas',
  xp1input in 'xp1input.pas',
  xp1o in 'xp1o.pas',
  xp1o2 in 'xp1o2.pas',
  xp2 in 'xp2.pas',
  xp2c in 'xp2c.pas',
  xp2db in 'xp2db.pas',
  xp2f in 'xp2f.pas',
  xp3 in 'xp3.pas',
  xp3ex in 'xp3ex.pas',
  xp3o in 'xp3o.pas',
  xp3o2 in 'xp3o2.pas',
  xp4 in 'xp4.pas',
  xp4e in 'xp4e.pas',
  xp4o in 'xp4o.pas',
  xp4o2 in 'xp4o2.pas',
  xp4o3 in 'xp4o3.pas',
  xp5 in 'xp5.pas',
  xp8 in 'xp8.pas',
  xp9bp in 'xp9bp.pas',
  xpauto in 'xpauto.pas',
  xpcc in 'xpcc.pas',
  XPCfg in 'xpcfg.pas',
  XPConfig in 'xpconfig.pas',
  xpconfigedit in 'xpconfigedit.pas',
  xpdatum in 'xpdatum.pas',
  xpdiff in 'xpdiff.pas',
  xpe in 'xpe.pas',
  XpEasy in 'xpeasy.pas',
  xpf2 in 'xpf2.pas',
  xpfido in 'xpfido.pas',
  xpfidonl in 'xpfidonl.pas',
  XPFTNAdr in 'xpftnadr.pas',
  xpglobal in 'xpglobal.pas',
  xpheader in 'xpheader.pas',
  xpimpexp in 'xpimpexp.pas',
  xpkeys in 'xpkeys.pas',
  xpmakeheader in 'xpmakeheader.pas',
  xpmaus in 'xpmaus.pas',
  xpmime in 'xpmime.pas',
  xpnt in 'xpnt.pas',
  XPProgressOutputWindow in 'xpprogressoutputwindow.pas',
  xpprogressoutputxy in 'xpprogressoutputxy.pas',
  xpreg in 'xpreg.pas',
  xpstat in 'xpstat.pas',
  xpterminal in 'xpterminal.pas',
  xpview in 'xpview.pas',
  xpx in 'xpx.pas',
  zcrfc in 'zcrfc.pas',
  OSDepend in 'osdepend.pas',
  SysUtils,
{$IFNDEF Kylix}
  xpcrt in 'xpcrt.pas',
  xpwin32 in 'xpwin32.pas',
  ZModem in 'netcall\zmodem.pas',
  ncgeneric in 'netcall\ncgeneric.pas',
  NCModem in 'netcall\ncmodem.pas',
  NCNNTP in 'netcall\ncnntp.pas',
  NCPOP3 in 'netcall\ncpop3.pas',
  NCSMTP in 'netcall\ncsmtp.pas',
  NCSocket in 'netcall\ncsocket.pas',
  ncuucp in 'netcall\ncuucp.pas',
  Netcall in 'netcall\netcall.pas',
  xpmodemscripts in 'netcall\xpmodemscripts.pas',
  xpncfido in 'netcall\xpncfido.pas',
  xpncnntp in 'netcall\xpncnntp.pas',
  xpncpop3 in 'netcall\xpncpop3.pas',
  xpncuucp in 'netcall\xpncuucp.pas',
  xpnczconnect in 'netcall\xpnczconnect.pas',
  xpnetcall in 'netcall\xpnetcall.pas',
  ncfido in 'netcall\ncfido.pas',
  dos in 'delphi\dos.pas',
  objcom in 'ObjCOM\objcom.pas',
  Ringbuff in 'ObjCOM\ringbuff.pas',
{$ELSE}
  ZModem in 'netcall/zmodem.pas',
  ncgeneric in 'netcall/ncgeneric.pas',
  NCModem in 'netcall/ncmodem.pas',
  NCNNTP in 'netcall/ncnntp.pas',
  NCPOP3 in 'netcall/ncpop3.pas',
  NCSMTP in 'netcall/ncsmtp.pas',
  NCSocket in 'netcall/ncsocket.pas',
  ncuucp in 'netcall/ncuucp.pas',
  Netcall in 'netcall/netcall.pas',
  xpmodemscripts in 'netcall/xpmodemscripts.pas',
  xpncfido in 'netcall/xpncfido.pas',
  xpncnntp in 'netcall/xpncnntp.pas',
  xpncpop3 in 'netcall/xpncpop3.pas',
  xpncuucp in 'netcall/xpncuucp.pas',
  xpnczconnect in 'netcall/xpnczconnect.pas',
  xpnetcall in 'netcall/xpnetcall.pas',
  ncfido in 'netcall/ncfido.pas',
  dos in 'delphi/dos.pas',
  objcom in 'ObjCOM/objcom.pas',
  Ringbuff in 'ObjCOM/ringbuff.pas',
{$ENDIF}
  archive in 'archive.pas',
  xpsendmessage_unsent in 'xpsendmessage_unsent.pas',
  xpsendmessage_internal in 'xpsendmessage_internal.pas',
  xpsendmessage in 'xpsendmessage.pas',
  RegExpr in 'regexpr.pas',
  main in 'main.pas',
  ncursix in 'ncursix.pas',
  xpsendmessage_attach_analyze in 'xpsendmessage_attach_analyze.pas',
  mime in 'mime.pas';

{$R *.res}
begin
  StartOpenXP;
end.

