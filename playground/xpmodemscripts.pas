{  $Id$

   OpenXP modem script unit
   Copyright (C) 2001 OpenXP team (www.openxp.de) and M.Kiesel

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC}

{ OpenXP modem script unit }
unit xpmodemscripts;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF}
  sysutils,typeform,fileio,inout,keys,datadef,database,maus2,
  resource,xpglobal,xp0,xp1,xp1o2,xp1input,ObjCOM,Modem,Debug;

function RunScript(test:boolean; scriptfile:string;
                   online,relogin:boolean; slog:textp):shortint;


implementation

uses  xp1o,xpkeys,xp9bp,xp10,winxp;

const ansimode : boolean = true;
      ansimax  = 40;       { max. LÑnge von ANSI-Codes }
      log2     : textp   = nil;

      ShellReleased : boolean = false;

      coltab : array[0..7] of byte = (0,red,green,brown,blue,magenta,cyan,7);

      ANSI_curup     = #27'[A';
      ANSI_curdown   = #27'[B';
      ANSI_curleft   = #27'[D';
      ANSI_curright  = #27'[C';
      ANSI_home      = #27'[H';
      ANSI_end       = #27'[K';

var termlines    : byte;
    IgnCD        : boolean;
    comnr        : byte;
    recs,lrecs   : string;
    display      : boolean;
    log          : boolean;
    logfile      : text;
    la           : byte;
    open_log     : boolean;
    in7e1        : boolean;

    ansichar     : boolean;       { ANSI-Sequenz aktiv }
    ansiseq      : string;
    ansilen      : byte;          { length(ansiseq) - fÅr schnellen Zugriff }
    ansifg,ansibg: byte;          { Farbattribute }
    ansihigh     : byte;          { add fÅr helle Farben }
    ansirev      : boolean;       { reverse Darstellung }

{$IFDEF FPC }
  {$HINTS OFF }
{$ENDIF }

{ Parameter nr, nn werden zwar nicht gebraucht, aber durch
  Prozedurvariable sind sie nîtig }
function TermGetfilename(nr,nn:byte):string;
begin
  rfehler(2003);    { '$FILE-Makro ist hier nicht mîglich.' }
  TermGetfilename:='';
end;

function RunScript(test:boolean; scriptfile:string;
                   online,relogin:boolean; slog:textp):shortint;

const MaxLines  = 500;
      Maxlabels = 100;
{     MaxlabelLen = 20;}

      pEndOK    = 0;      { RÅckgabewerte von RunScript      }
      pEndError = 1;      { = num. Parameter des END-Befehls }
      pEndFail  = 2;
      pEndSyntax= 3;      { Syntax Error }

      pDispOn   = 1;      { num. Parameter des DISPLAY-Befehls }
      pDispOff  = 2;
      pAnsiOn   = 1;      { num. Parameter des ANSI-Befehls }
      pAnsiOff  = 2;

      cmdWaitfor = 1;    cmdWrite   = 6;     cmdFlush = 11;    cmdANSI = 16;
      cmdSend    = 2;    cmdWriteln = 7;     cmdCls   = 12;
      cmdGoto    = 3;    cmdDisplay = 8;     cmdCall  = 13;
      cmdEnd     = 4;    cmdTimer   = 9;     cmdReturn= 14;
      cmdDelay   = 5;    cmdRead    = 10;    cmdBreak = 15;

      varPoint     = 1;   { Pointname }
      varUser      = 2;   { Username }
      varPassword  = 3;   { Netcall-Pa·wort }
      varOpassword = 4;   { Login-Pa·wort }
      varLogin     = 5;   { UUCP/QM-Login }
      varProtocol  = 6;   { MausTausch-Protokollkennung }
      varSerialNo  = 7;   { Maggi/Z-Seriennummer }

type  ScrRec   = record
                   txtline   : integer;           { Zeilennr. in .SCR   }
                   onflag    : byte;              { 0=nix, 1=ON, 2=Timeout }
                                                  { 3=Online, 4=Relogin }
                   onstr     : string;            { .. String-Parameter }
                   command   : shortint;          { Befehlsnummer       }
                   numpar    : longint;           { .. num. Parameter   }
                   cr,lf     : boolean;
                   strpar    : string;            { .. String-Parameter }
                 end;
      ScrArr   = array[0..MaxLines] of scrrec;

var   script   : ScrArr;
      lines    : integer;
      logins   : integer;       { verbleibende Login-Anzahl }
      RunScriptRes: ShortInt;


function LoadScript:boolean;
type labela = array[1..maxlabels] of record
                                       name : string;
                                       line : integer;
                                     end;
var t      : text;
    s,s0   : string;
    errlog : text;
    labels : integer;
    _label : labela;
    line   : integer;    { lfd. Zeile }
    errors : integer;
    stringflag : boolean;
    ok     : boolean;

  procedure serror(nr:word; txt:string);
  begin
    writeln(errlog,getreps2(2010,0,strs(line))+getreps2(2010,nr,txt));
    ok:=false;
    inc(errors);
{*    for i:=0 to MaxLines do script[i]:='';}
  end;

  procedure GetWord;     { nÑchstes Wort lesen }
  var p : byte;
  begin
    stringflag:=false;
    if s='' then
      serror(4,'')          { 'unerwartetes Zeilenende' }
    else if s[1]='"' then begin      { String-Konstante }
      p:=2;
      while (p<=length(s)) and (s[p]<>'"') do inc(p);
      if p>length(s) then
        serror(1,'')     { '"' fehlt }
      else begin
        s0:=copy(s,2,p-2);
        s:=trim(mid(s,p+1));
        stringflag:=true;
        p:=1;
        while p<length(s0) do begin
          inc(p);
          if s0[p-1]='^' then begin
            case s0[p] of
              '0'      : s0[p]:='^';
              ''''     : s0[p]:='"';
              'A'..'_' : s0[p]:=chr(ord(s0[p])-64);
              'a'..'z' : s0[p]:=chr(ord(s0[p])-96);
              else       delete(s0,p,1);
            end;
            delete(s0,p-1,1);
            end;
          end;
        end;
      end
    else begin
      p:=blankpos(s);
      if p=0 then p:=length(s)+1;
      s0:=LeftStr(s,p-1);
      s:=trim(mid(s,p+1));
      end;
  end;

  function comment(var s:string):boolean;
  begin
    comment:=(s='') or (s[1]='#') or (s[1]=';');
  end;

  procedure GetString;
  var s : string;
  begin
    GetWord;
    s:='*';
    if not stringflag then begin
      if comment(s0) then s:='' else
      if UpperCase(s0)='CR' then s:=#13 else
      if UpperCase(s0)='LF' then s:=#10 else
      if UpperCase(s0)='CRLF' then s:=#13#10;
      if s<>'*' then begin
        s0:=s;
        stringflag:=true;
        end;
      end;
  end;

  function SeekLabel:integer;
  var p : byte;
  begin
    p:=1;
    while (p<=labels) and (_label[p].name<>s0) do inc(p);
    if p<=labels then SeekLabel:=_label[p].line
    else SeekLabel:=0;
  end;

  procedure AddLabel;
  begin
    dellast(s0);
    LoString(s0);
    if SeekLabel<>0 then
      serror(2,s0)                 { 'Sprungmarke "%s" existiert bereits' }
    else if labels=MaxLabels then
      serror(3,strs(MaxLabels))    { 'Max. %s Sprungmarken mîglich!' }
    else begin
      inc(labels);
      _label[labels].name:=s0;
      _label[labels].line:=lines+1;
      end;
  end;

  procedure AddCommand; forward;

  procedure AddOnCommand;
  begin
    GetString;
    if ok then with script[lines+1] do begin
      if stringflag then LoString(s0)
      else UpString(s0);
      if onflag>0 then begin
        serror(6,'');     { 'verschachtelte ON-Befehle sind nicht erlaubt' }
        s:='';
        end
      else begin
        if stringflag then begin
          onflag:=1;                          { ON "..." <Command> }
          onstr:=s0
          end
        else if s0='TIMEOUT' then             { ON TIMEOUT <Command> }
          onflag:=2
        else if s0='ONLINE' then              { ON ONLINE <Command>  }
          onflag:=3
        else if s0='NETCALL' then             { ON NETCALL <Command> }
          onflag:=4
        else if s0='RELOGIN' then             { ON RELOGIN <Command> }
          onflag:=5
        else begin
          serror(5,'');     { 'ungÅltige ON-Funktion ' }
          s:='';
          end;
        if onflag<>0 then begin
          GetWord;
          AddCommand;
          end;
        end;
      end;
  end;

  procedure AddStrComm(cmd:integer);
  var ss : string;
  begin
    GetString;
    if ok then
      if not stringflag then
        if s0[1]<>'$' then
          serror(7,'')      { 'Text-Parameter erwartet' }
        else begin
          inc(lines);
          with script[lines] do begin
            command:=cmd;
            ss:=UpperCase(s0);
            if ss='$POINT'     then numpar:=varPoint else
            if ss='$USER'      then numpar:=varUser else
            if ss='$PASSWORD'  then numpar:=varPassword else
            if ss='$OPASSWORD' then numpar:=varOPassword else
            if ss='$LOGIN'     then numpar:=varLogin else
            if ss='$PROTOCOL'  then numpar:=varProtocol else
            if ss='$SERIALNO'  then numpar:=varSerialNo else
            begin
              serror(14,ss);     { 'ungÅltiges Textmakro: %s' }
              dec(lines);
              end;
            end;
          end
      else begin
        inc(lines);
        with script[lines] do begin
          command:=cmd;
          strpar:=s0
        end;
      end;
    if ok and not comment(s) then
      with script[lines] do begin
        GetWord;
        UpString(s0);
        if s0='CR' then cr:=true else
        if s0='LF' then lf:=true else
        if s0='CRLF' then begin cr:=true; lf:=true; end else
        serror(16,s0);    { 'ungÅltiger Parameter: %s' }
        end;
  end;

  procedure AddComm(cmd:integer);
  begin
    inc(lines);
    script[lines].command:=cmd;
  end;

  procedure AddDisplayCommand;
  begin
    GetWord;
    UpString(s0);
    if (s0<>'ON') and (s0<>'OFF') then
      serror(9,'')      { 'ON oder OFF erwartet' }
    else begin
      inc(lines);
      script[lines].command:=cmdDisplay;
      script[lines].numpar:=iif(s0='ON',pDispOn,pDispOff);
      end;
  end;

  procedure AddAnsiCommand;
  begin
    GetWord;
    UpString(s0);
    if (s0<>'ON') and (s0<>'OFF') then
      serror(9,'')      { 'ON oder OFF erwartet' }
    else begin
      inc(lines);
      script[lines].command:=cmdANSI;
      script[lines].numpar:=iif(s0='ON',pAnsiOn,pAnsiOff);
      end;
  end;

  procedure AddEndCommand;
  begin
    if (s='') or comment(s) then s0:=''
    else GetWord;
    UpString(s0);
    if (s0<>'') and (s0<>'ERROR') and (s0<>'FAIL') then
      serror(10,'')      { 'ERROR oder FAIL erwartet' }
    else begin
      inc(lines);
      script[lines].command:=cmdEnd;
      script[lines].numpar:=iif(s0='',pEndOk,iif(s0='ERROR',pEndError,pEndFail));
      end;
  end;

  procedure AddDelayCommand;
  begin
    GetWord;
    if rval(s0)=0 then
      serror(11,'')      { 'ungÅltiger Delay-Parameter (Zahl erwartet)' }
    else begin
      inc(lines);
      script[lines].command:=cmdDelay;
      script[lines].numpar:=system.round(rval(s0)*1000);
      if s<>'' then begin
        GetWord;
        UpString(s0);
        if s0='SHOW' then
          script[lines].strpar:=s0
        else if not comment(s0) then
          serror(8,'');
        end;
      end;
  end;

  procedure AddIntComm(cmd:integer; isint:boolean);
  begin
    GetWord;
    if stringflag then
      serror(12,'')      { 'numerischer Parameter erwartet' }
    else begin
      inc(lines);
      with script[lines] do begin
        command:=cmd;
        numpar:=ival(s0);
        if isint then
          numpar:=minmax(numpar,-maxint,maxint);
        end;
      end;
  end;

  procedure AddJump(cmd:integer);
  begin
    GetWord;
    LoString(s0);
    inc(lines);
    with script[lines] do begin
      command:=cmd;
      numpar:=SeekLabel;
      if numpar=0 then begin              { Label (noch) nicht vorhanden }
{*        getmem(strparp,length(s0)+1);}
        strpar:=s0;
        end;
      end;
  end;

  procedure AddCommand;
  begin
    UpString(s0);
    if s0='ON'      then AddOnCommand            else
    if s0='WAITFOR' then AddStrComm(cmdWaitfor)  else
    if s0='SEND'    then AddStrComm(cmdSend)     else
    if s0='WRITE'   then AddStrComm(cmdWrite)    else
    if s0='WRITELN' then if s<>'' then AddStrComm(cmdWriteln) else
                                       AddComm(cmdWriteln) else
    if s0='DISPLAY' then AddDisplayCommand       else
    if s0='END'     then AddEndCommand           else
    if s0='DELAY'   then AddDelayCommand         else
    if s0='GOTO'    then AddJump(cmdGoto)        else
    if s0='TIMER'   then AddIntComm(cmdTimer,true) else
    if s0='READ'    then AddComm(cmdRead)     else
    if s0='FLUSH'   then AddComm(cmdFlush)    else
    if s0='CLS'     then AddComm(cmdCls)      else
    if s0='CALL'    then AddJump(cmdCall)     else
    if s0='RETURN'  then AddComm(cmdReturn)   else
    if s0='BREAK'   then AddComm(cmdBreak)    else
    if s0='ANSI'    then AddAnsiCommand       else
    serror(15,s0);    { 'ungÅltiger Befehl: %s' }
    if ok and not comment(s) then
      serror(8,'');    { 'ÅberflÅssige Daten am Zeilenende' }
  end;

  procedure TestLabels;
  var i : integer;
  begin
    for i:=1 to lines do with script[i] do
      if ((command=cmdGoto) or (command=cmdCall)) and (numpar=0) then begin
        s0:=strpar;
        numpar:=SeekLabel;
        if numpar=0 then serror(13,s0);      { 'Sprungmarke fehlt: %s' }
        strpar:='';
        end;
  end;

begin
  fillchar(script,sizeof(script),0);
  labels:=0;
  assign(t,scriptfile);
  reset(t);
  assign(errlog,LogPath+ScerrLog);
  rewrite(errlog);
  lines:=0; line:=0; errors:=0;
  while not eof(t) and (lines<MaxLines) do begin
    ok:=true;
    inc(line);
    script[lines+1].txtline:=line;
    readln(t,s);
    s:=trim(s);
    if not comment(s) then begin
      GetWord;
      if ok then begin
        if s0[length(s0)]=':' then begin
          AddLabel;
          GetWord;
          end;
        AddCommand;
        end;
      end;
    end;
  TestLabels;
  freeres;
  close(errlog);
  close(t);
  LoadScript:=(errors=0);
end;


function ExecuteScript:shortint;
const maxstack = 50;
var ip   : integer;
    ende : boolean;
    par  : string;
    trace: text;
    tn   : byte;
    stack: array[1..maxstack] of integer;
    sp   : integer;
    ExecuteScriptRes: shortint;

  procedure RunError(nr:word);
  begin
    writeln;
    writeln(getres2(2011,3)+' '+getres2(2011,nr)+#7);
    logerror(getres2(2011,3)+' '+getres2(2011,nr));
    ende:=true;
   end;

  function timeout:boolean;
  begin
    timeout:=not (IgnCD or CommObj^.Carrier) or (zaehler[2]=0);
  end;

  function GetPar:string;
  var crlf : string;
  begin
    with script[ip],boxpar^ do begin
      if lf then
        if cr then crlf:=#13#10
        else crlf:=#10
      else
        if cr then crlf:=#13
        else crlf:='';
      case numpar of
        0            : if strpar='' then getpar:=strpar+crlf
                       else getpar:=strpar+crlf;
        varPoint     : GetPar:=pointname+crlf;
        varUser      : GetPar:=username+crlf;
        varPassword  : GetPar:=passwort+crlf;
        varOPassword : begin
                         GetPar:=o_passwort+crlf;
                         dec(logins);
                         if logins=0 then begin
                           RunScriptRes:=pEndError;
                           ende:=true;
                           end;
                       end;
        varLogin     : GetPar:=LoginName+crlf;
        varProtocol  : GetPar:=ProtoTyp+crlf;
        varSerialNo  : GetPar:=ZerbID+crlf;
      end;
    end;
  end;

  procedure TestKey;
  var c : char;
  begin
    if keypressed then begin
      c:=readkey;
      if c=#27 then begin
        ende:=true;
        ExecuteScriptRes:=pEndError;
        end
      else if c>#0 then
        CommObj^.SendString(c,False);
      end;
  end;

  procedure interprete;
  var doit : boolean;
  begin
    with script[ip] do begin
      case onflag of
        0 : doit:=true;
        1 : begin
              par:=onstr;
              doit:=(RightStr(lrecs,length(par))=par);
              if doit then begin
                if log2<>nil then write(log2^,recs);
                recs:=''; lrecs:='';
                end;
            end;
        2 : begin
              multi2;
              doit:=(zaehler[3]=0);
            end;
        3 : doit:=online;
        4 : doit:=not online;
        5 : doit:=relogin;
      end;
      if doit then
        case command of
          cmdWaitfor  : begin
                          par:=LowerCase(getpar);
                          repeat
                            tb;
                            testkey;
                          until timeout or (RightStr(lrecs,length(par))=par) or ende;
                          if log2<>nil then write(log2^,recs);
                          recs:=''; lrecs:='';
                        end;
          cmdSend     : CommObj^.SendString(GetPar,False);
          cmdGoto     : ip:=numpar-1;
          cmdEnd      : begin
                          ende:=true;
                          ExecuteScriptRes:=numpar;
                        end;
          cmdDelay    : mdelay(numpar,strpar='SHOW');
          cmdWrite    : begin moff;
                          write(GetPar); mon;
                        end;
          cmdWriteln  : begin moff; writeln(Getpar); mon; end;
          cmdDisplay  : Display:=(numpar=pDispOn);
          cmdTimer    : zaehler[3]:=numpar;
          cmdRead     : tb;
          cmdFlush    : CommObj^.PurgeInbuffer;
          cmdCls      : clrscr;
          cmdCall     : if sp=maxstack then begin
                          runerror(5);      { 'StapelÅberlauf' }
                          sp:=0;
                          end
                        else begin
                          inc(sp);
                          stack[sp]:=ip;
                          ip:=numpar-1;
                        end;
          cmdReturn   : if sp=0 then       { 'RETURN ohne CALL' }
                          runerror(6)
                        else begin
                          ip:=stack[sp];
                          dec(sp);
                        end;
          cmdBreak    : {*SendBreak(comnr)};
          cmdANSI     : begin
                          ansimode:=(numpar=pAnsiOn);
                          if not ansimode then ansichar:=false;
                        end;
        end;
      inc(ip);
      end;
  end;

begin    { of ExecuteScript }
  ip:=1;
  recs:=''; lrecs:='';
  zaehler[2]:=boxpar^.LoginWait;
  zaehler[3]:=0;
  logins:=boxpar^.retrylogin+1;
  ende:=false;
  display:=true;
  ansimode:=true;
  sp:=0;
  if ParTrace then begin                       { Trace-Log îffnen }
    assign(trace,LogPath+'TRACE.LOG');
    if existf(trace) then append(trace)
    else rewrite(trace);
    writeln(trace,getres2(2011,1),' ',scriptfile,' / ',date,' / ',time);
    writeln(trace);
    tn:=0;
    end;
  repeat
    if ParTrace then begin                     { Trace-Zeile schreiben }
      write(trace,script[ip].txtline,' ');
      inc(tn);
      if tn>17 then begin
        tn:=0; writeln(trace);
        end;
      end;
    interprete;
    Testkey;

    if ip>lines then begin
      ende:=true;
      ExecuteScriptRes:=pEndOk;
      end;
  until ende or timeout;
  if sp>0 then
    runerror(4);           { 'CALL ohne RETURN' }
  if ParTrace then begin
    if tn>0 then writeln(trace);
    writeln(trace);
    writeln(trace,getres2(2011,2),' ',time);
    writeln(trace);
    writeln(trace);
    close(trace);
    end;
  freeres;
  if timeout then
    ExecuteScriptRes:=pEndError;
  ExecuteScript := ExecuteScriptRes;
end;


procedure ReleaseScript;
var i : integer;
begin
  for i:=lines downto 1 do
    with script[i] do begin onstr:=''; strpar:=''; end;
end;


begin     { of RunScript }
  log2:=slog;
  if not LoadScript then
    RunScriptRes:=pEndSyntax
  else
    if not test and (lines>0) then
      RunScriptRes:=ExecuteScript
    else
      RunScriptRes:=pEndOK;
  RunScript := RunScriptRes;
  ReleaseScript;
  log2:=nil;
end;

end.

{
  $Log$
  Revision 1.1  2001/01/10 16:31:50  ma
  - todo: change to a class that can communicate with a class from
    xpterminal unit

  ------ moved to playground
  Revision 1.1  2001/01/04 16:06:04  ma
  - renamed, was xpterm.pas (partly)

}
