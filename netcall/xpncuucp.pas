{  $Id$

   OpenXP UUCP netcall routines
   (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{ OpenXP UUCP netcall unit }

{$I XPDEFINE.INC}

unit xpncuucp;

interface

uses
  {$IFDEF NCRT}xpcurses,{$ELSE}crt,{$ENDIF }
  sysutils,typeform,montage,fileio,keys,maus2,inout,lister,resource,
  maske,xpglobal,debug,xp0,xpdiff,xp1,xp1input,xpf2,fidoglob,classes,
  zcrfc,progressoutput;

function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList):shortint;

implementation

uses
  xp3,xp3o,xpmakeheader,xpprogressoutputwindow,xpmodemscripts,
  xpnt,xpnetcall,ncuucp,objcom;

function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList):shortint;
var
  UUNum         :word;         { fortlaufende 16-Bit-Nummer der UUCP-Dateien }
  CmdFile       :string;
//  DeleteFileList:TStringList;

  (* Nummer in UUNUMMER.DAT lesen/schreiben *)

  procedure ReadUU;
  var t : text;
      s : string;
  begin
    if _filesize(UUnumdat)<2 then
      uunum:=1
    else begin
      assign(t,UUnumdat); reset(t); readln(t,s);
      close(t); uunum:=minmax(ival(s),0,$ffff);
    end;
  end;

  procedure WriteUU; { Nr. in UUNUMER.DAT schreiben }
  var t : text;
  begin
    assign(t,UUnumdat); rewrite(t); writeln(t,uunum); close(t);
  end;

  (* Puffer in RFC-Files konvertieren *)

  function ProcessOutgoingFiles:boolean;
  var source,destDir:   string;
      uu:               TUUZ;
      delsource:        boolean;

    procedure CleanSpool;
    begin
      erase_mask(AddDirSepa(DestDir)+'*.BAK'); (* delete old input files  *)
      erase_mask(AddDirSepa(DestDir)+'*.OUT'); (* delete old output files *)
    end;

    function RunoutFilter:boolean;
    begin
      delsource := OutFilter(source);
      result := (errorlevel=0);
    end;

    function InitUUZ:boolean;
    begin
      uu := TUUZ.Create;
      if uu=nil then begin result:=false; exit; end;

      if boxpar^.SizeNego then uu.parsize := true;

      uu.SMTP     := BoxPar^.UUsmtp;

      if uu.SMTP and (BoxPar^.UParcer<>'') then
        if      pos('freeze',LowerCase(boxpar^.uparcer))>0 then uu.fSMTP := true
        else if pos('gzip',  LowerCase(boxpar^.uparcer))>0 then uu.zSMTP := true
        else if pos('bzip2', LowerCase(boxpar^.uparcer))>0 then uu.bSMTP := true
        else                                                    uu.cSMTP := true;

      uu.NewsMime := NewsMIME;
      uu.MakeQP   := MIMEqp;
      uu.RFC1522  := RFC1522;

      uu.MailUser := BoxPar^.UserName;
      uu.NewsUser := BoxPar^.UserName;
      uu.FileUser := BoxPar^.UserName;

      uu.Source   := source;
      uu.Dest     := DestDir;
      uu._from    := boxpar^.pointname;
      uu._to      := boxpar^.boxname;

      ReadUU;
      uu.uunumber := uunum;

      result:=true;
    end;

    function RunUUZ:boolean;
      { this whole stuff should really be part of UUZ }

      function Pack: boolean;
      var compression: (none,freeze,gzip,bzip,compress);
          cunbatchcmd: string;
          sr    : tsearchrec;
          f1,f2 : ^file;
          p,b : integer;
          s      : shortstring;
          is_news: boolean;
          rr    : word;

        procedure PackFehler;
        begin
          trfehler(713,30);    { 'Fehler beim Packen!' }
        end;

        procedure PackUndoRCSMTP(b:integer);
        var f1,f2: file;
            adr  : longint;
            p     : integer;
        begin
          adr:=0;
          assign(f1,DestDir+'X-'+hex(b,4)+'.OUT'); reset(f1,1);
          assign(f2,DestDir+'smtp.tmp'); rewrite(f2,1);

          repeat
            seek(f1,adr); blockread(f1,s[1],40,rr); s[0]:=chr(rr);
            p:=cpos(#10,s); s[0]:=chr(p-1); inc(adr,p); {!! BUG: Lines MUST NOT be longer than 255 chars !!}
            if (s='C rcsmtp') or (s='C rfsmtp') or (s='C rgsmtp') or (s='C rzsmtp') then s:='C rsmtp';
            s:=s+#10;
            blockwrite(f2,s[1],length(s));
          until adr>=filesize(f1);

          close(f1); close(f2); erase(f1);
          rename(f2,DestDir+'X-'+hex(b,4)+'.OUT');
        end;

      begin { Pack }
        if BoxPar^.uparcer='' then begin result:=true; exit; end;

             if pos('freeze',LowerCase(BoxPar^.uparcer))>0 then compression := freeze
        else if pos('gzip',  LowerCase(BoxPar^.uparcer))>0 then compression := gzip
        else if pos('bzip2', LowerCase(BoxPar^.uparcer))>0 then compression := bzip
        else compression := compress;

        case compression of
          freeze: cunbatchcmd := '#! funbatch'#10;
          gzip:   cunbatchcmd := '#! gunbatch'#10;
          bzip:   cunbatchcmd := '#! bunbatch'#10;
          else    cunbatchcmd := '#! cunbatch'#10;
        end;

        new(f1); new(f2);
        p:=pos('$PUFFER',UpperCase(boxpar^.uparcer));

        if 0=findfirst(DestDir+'D*.OUT',faAnyFile,sr) then
        repeat
          assign(f1^,DestDir+sr.name);
          reset(f1^,1);
          s[0]:=#8; blockread(f1^,s[1],8);
          close(f1^);
          is_news:=(s='#! rnews');

          if is_news or (LeftStr(s,5)='HELO ') then
          begin    { News/SMTPmail packen }
            shell(LeftStr(boxpar^.UpArcer,p-1)+DestDir+sr.name+mid(boxpar^.UpArcer,p+7),500,3);

            if not existf(f1^) then
            begin    { Datei wurde gepackt }
              {$IFDEF UnixFS} { under unix, the extension is always preserved }
              case compression of
                freeze: assign(f1^,DestDir+sr.name+'.F');
                gzip:   assign(f1^,DestDir+sr.name+'.gz');
                bzip:   assign(f1^,DestDir+sr.name+'.bz2');
                else    assign(f1^.DestDir+sr.name+'.Z');
              end;
              {$ELSE} { under DOS/Win32/OS.2, we don't know whether we've got a
               LFN compressor or what it does with the extension}

              { first, try .OUT => .XXX/.OXX/.OUX }
              case compression of
                { for some strange reason, the pure DOS freeze that comes with Crosspoint
                  uses XZ as an extension }
                freeze: assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-2)+'XZ');
                gzip:   assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-2)+'GZ');
                bzip:   assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-3)+'BZ2');
                else    assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-1)+'Z');
              end;

              { now, try .OUT => .X/.XX }
              if (compression<>bzip) and (not existf(f1^)) then case compression of
                freeze: assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-3)+'F');
                gzip:   assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-3)+'GZ');
                else    assign(f1^,DestDir+LeftStr(sr.name,length(sr.name)-3)+'Z');
              end;

              { finally, try .OUT => .OUT.X/.OUT.XX }
              if {$IFDEF DOS32} System.LFNSupport and {$ENDIF} (not existf(f1^)) then case compression of
              { Problem under DOS32: If we don't support LFN but the compressor does, we
                won't find the compressed file (compressed file is D-XXXX~N.Z, but we
                only look for e.g. D-XXXX.OUZ and D-XXXX.Z (and don't know N anyway). }
                freeze: assign(f1^,DestDir+sr.name+'.F');
                gzip:   assign(f1^,DestDir+sr.name+'.GZ');
                bzip:   assign(f1^,DestDir+sr.name+'.BZ2');
                else    assign(f1^,DestDir+sr.name+'.Z');
              end;
              {$ENDIF}

              if (errorlevel<>0) or not existf(f1^) then begin
                PackFehler; dispose(f1); dispose(f2);
                result:=false; exit;
              end;

              if is_news then begin             { '#! xxunbatch' erzeugen }
                reset(f1^,1);
                assign(f2^,DestDir+sr.name);
                rewrite(f2^,1);
                blockwrite(f2^,cunbatchcmd[1],length(cunbatchcmd));
                fmove(f1^,f2^);
                close(f1^); close(f2^);
                erase(f1^);
              end else                          { rxxsmtp -- erzeugt UUZ automatisch }
                rename(f1^,DestDir+sr.name);
            end else { Datei wurde nicht gepackt -- warum auch immer }
              if not is_news then { UUZ generiert hier rxxsmtp -- rueckgaengig machen }
              begin { rcsmtp -> rsmtp }
                b:=hexval(copy(sr.name,3,4)); if b=$ffff then b:=0 else inc(b);
                PackUndoRCSMTP(b);
              end;
          end; { is_news or 'HELO ' }
        until 0<>findnext(sr);

        dispose(f1);dispose(f2);
        result:=true;
      end;

    begin // RunUUZ
      MakeMimetypCfg;
      uu.ZtoU; {!! no error checking}
      result:=Pack;
    end;

    procedure KillUUZ;
    begin
      uunum := uu.uunumber;
      CmdFile:=uu.CommandFile;
      WriteUU;
      uu.Free;
      result:=true; {!! no error checking}
    end;

  begin { ProcessOutgoingFiles:boolean; }
    result    := false;
    delsource := false;

    source    := ppfile;
    destdir   := iifs(diskpoll,boxpar^.sysopout,XFerDir);

    CleanSpool;

    if _filesize(source) <=0 then
      result:=true      { doing nothing will hopefully succeed ;-) }
    else

    if RunOutFilter then
    begin
      if InitUUZ then begin
        if RunUUZ then
          result:=true;
        KillUUZ;
      end;
    end;

    if delsource then _era(source);
  end;

  (* RFC-Files in Eingangspuffer konvertieren *)

  function ProcessIncomingFiles:boolean;
  var source,dest:      string;
      uu:               TUUZ;

    function InitUUZ:boolean;
    begin
      uu := TUUZ.Create;
      if uu=nil then begin result:=false; exit; end;

      // uu.getrecenvemp := false;      { not needed for UUCP }
      // uu.shrinkheader := ShrinkUheaders; { UUZ-Schalter -r }

      {!! uncompressing programmes are defined in box config but compiled into unit zcrfc}

      uu.uncompress  := BoxPar^.downarcer;
      uu.unfreeze    := BoxPar^.unfreezer;
      uu.ungzip      := BoxPar^.ungzipper;
      uu.unbzip      := BoxPar^.unbzipper;

      uu.OwnSite  := BoxPar^.pointname+BoxPar^._domain;
      uu.Source   := source;
      uu.Dest     := dest;

      uu.ClearSourceFiles := DiskPoll or nDelPuffer;
//    uu.DeleteFileList:=DeleteFileList;

      result:=true;
    end;

    function RunUUZ:boolean;
    begin
//    MakeMimetypCfg;
      uu.UtoZ;
      result:=true;
    end;

    procedure KillUUZ;
    begin
      uu.Free;
      result:=true;
    end;

    function RunInFilter:boolean;
    begin
      if (boxpar^.eFilter<>'') then
        CallFilter(true,dest);
      result:=true;
    end;

  begin { ProcessIncomingFiles: boolean }
    result    := false;
    source    := AddDirSepa(iifs(diskpoll,BoxPar^.sysopinp,XFerDir))+'X*';

    dest      := 'UUbuffer.zer';

    if InitUUZ then begin
      if RunUUZ then
        if RunInFilter then
          result:=true;
      KillUUZ;
    end;

    if fileexists(dest) then
      if _filesize(dest)>=1 then IncomingFiles.Add(dest)
      else _era(dest);
  end;

  function RunUUCICO:integer;
  var UUCICO: TUUCPNetcall;
      CommInit: string;
  begin
    case BoxPar^.CONN_Mode of
      2: CommInit:='RAWIP ' +BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
      3: CommInit:='TELNET '+BoxPar^.Conn_IP+':'+StrS(BoxPar^.Conn_Port);
    else CommInit:=ComN[BoxPar^.bport].MCommInit;
    end;

    UUCICO:=TUUCPNetcall.
      CreateWithCommInitAndProgressOutput(CommInit,
      TProgressOutputWindowDialog.CreateWithSize(60,10,BoxName,True));

    if BoxPar^.conn_Mode=1 then
    begin
      with BoxPar^,ComN[BoxPar^.BPort] do
      begin
        if HayesComm and (ModemInit+MInit<>'') then begin
          if (ModemInit<>'') and (minit<>'') then
            UUCICO.CommandInit:= minit+'\\'+ModemInit
          else
            UUCICO.CommandInit:= minit+ModemInit;
          end;
        if HayesComm then begin
          UUCICO.CommandDial    := MDial;
          UUCICO.Phonenumbers   := BoxPar^.Telefon;
        end;
      end;

      UUCICO.MaxDialAttempts    := BoxPar^.RedialMax;
      UUCICO.RedialWaitTime     := BoxPar^.RedialWait;
      UUCICO.TimeoutConnectionEstablish := BoxPar^.ConnectMax;
    end;

    UUCICO.UUremote      := BoxName;
    UUCICO.UUname        := iifs(BoxPar^.UUCPname<>'',BoxPar^.UUCPName,BoxPar^.PointName);
    UUCICO.UUprotos      := BoxPar^.UUProtos;

    UUCICO.FilePath      := InFileDir;
    UUCICO.CommandFile   := CmdFile;
    UUCICO.DownSpool     := XFerDir;

    UUCICO.MaxWinSize    := BoxPar^.MaxWinSize;
    UUCICO.MaxPacketSize := BoxPar^.MaxPacketSize;
    UUCICO.VarPacketSize := BoxPar^.VarPacketSize;
    UUCICO.ForcePktSize  := BoxPar^.ForcePacketSize;

    UUCICO.SizeNego      := BoxPar^.SizeNego;
    UUCICO.MaxFSize      := BoxPar^.MaxFSize;

    UUCICO.LogFileName   := LogFile;

    if not UUCICO.Connect then
      result:=el_noconn
    else 
    begin
      UUCICO.Output(mcInfo,'Login',[0]);
      if RunScript(BoxPar,UUCICO.CommObj,UUCICO.ProgressOutput,false,BoxPar^.Script,false,false) = 0 then
      begin
        UUCICO.Output(mcInfo,'Starting UUCICO',[0]);
        result := UUCICO.PerformNetcall;
      end;
      UUCICO.Disconnect;
    end;

    UUCICO.Free;
  end;

{
function UUCPNetcall(boxname: string;
                     boxpar: boxptr;
                     ppfile: string;
                     diskpoll: boolean;
                     Logfile: String;
                     IncomingFiles: TStringList):shortint;
}
begin {function UUCPNetcall}
  Debug.DebugLog('xpncuucp','uucp netcall starting',DLInform);
  result:=el_noconn;

  ReadUU;

  if diskpoll then
  begin
    SetCurrentDir(boxpar^.sysopinp);
    Shell(boxpar^.sysopstart,500,1);
    SetCurrentDir(OwnPath);
    if (errorlevel=0) and ProcessIncomingFiles then
      if ProcessOutgoingFiles then begin
        SetCurrentDir(boxpar^.sysopout);
        Shell(boxpar^.sysopend,500,1);
        SetCurrentDir(OwnPath);
        if errorlevel=0 then result:=el_ok else result:=el_recerr;
      end
      else {!ProcessOutgoingFiles}
        result:=el_senderr
    else {errorlevel<>0 or !ProcessIncomingFiles}
      result:=el_recerr;
  end {!diskpoll}
  else begin
    if ProcessOutgoingFiles then begin
      result:=RunUUCICO;
      ProcessIncomingFiles; (* always read in files we've got *)
    end else
      result:=el_noconn;
  end; {!diskpoll}

  if result IN [el_recerr,el_ok] then begin
    Debug.DebugLog('xpncucp','sending upbuffer was successful, clearing unsent flags',DLInform);
    if FileExists(ppfile) then begin ClearUnversandt(ppfile,boxname); _era(ppfile); end;
  end;

end; { function UUCPNetcall}

end.

{
  $Log$
  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  --- import from playground
}
