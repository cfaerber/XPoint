{   $Id$

    Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus F"arber

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

{ Nachrichten versenden: Attachments                                   }

{$I xpdefine.inc}

unit xpsendmessage_attach;
{ ---------------------------} interface { --------------------------- }

uses
  classes,mime,sysutils,mime_ctype;

type
  MIME_EOL = (Eol_LF,Eol_CRLF,Eol_CR,Eol_none);
  MIME_CharsetClass = (charset_8bit,charset_utf8,charset_utf16be,charset_utf16le);

  TMIME_Part = class
  private
    function GContentCharset:String;
    procedure SContentCharset(const newvalue:string);

  public
    FileName   	: String;	(* Name of file 		*)
    IsTemp      : Boolean;	(* Created temporarily 		*)
    IsFile      : Boolean;      (* Was create from file         *)

    FileCharset : String;	(* charset of data		*)
    FileEOL	: MIME_EOL;     (* line ends                    *)

    ContentType 	: TMimeContentType;

    ContentEncoding	: MIME_Encoding;
    ContentDescription 	: String;
    ContentDisposition	: MIME_Disposition;

    AllowedCharsets     : set of MIME_CharsetClass;
    AllowedEOL          : set of MIME_EOL;
    AllowedEncoding     : set of MIME_Encoding;

    FileNameO   : TFileName;	(* Original file name		*)
    FileCreate  : TDateTime;	(* file creation date		*)
    FileModify  : TDateTime;	(* file modification date	*)
    FileAccess  : TDateTime;	(* file access date		*)

    property ContentCharset: String read GContentCharset write SContentCharset;

    constructor Create;
    destructor Destroy;
  end;

procedure SendAttach(parts:TList;Umlaute:Boolean);
procedure SendAttach_Add(parts:TList;x,y:Integer;Umlaute:Boolean);
procedure SendAttach_Delete(parts:TList;n:integer);
procedure SendAttach_Edit(parts:TList;n:integer;x,y:integer;Umlaute:Boolean);

{ ------------------------} implementation { ------------------------- }

uses
{$IFDEF unix}
  xpcurses,
{$ENDIF}
{$IFDEF Win32}
  windows,
{$ENDIF}
  fileio, inout, keys, lister, maus2, resource, typeform, winxp, xp0, xp1,
  xp1input, xp1o, xp3, xp3o, xp4e, xpe, xpglobal, xpnt,
  xpsendmessage_attach_analyze, maske;

constructor TMIME_Part.Create;
begin
  IsTemp	:= False;
  IsFile	:= False;

  ContentType := TMimeContentType.Create('');

  FileCharset := '';
  FileEOL	:= EOL_CRLF;

  ContentEncoding := mime_auto;
  ContentDescription := '';
  ContentDisposition := mime_attach;

  FileNameO	:= '';
  FileCreate	:= NaN;
  FileModify	:= NaN;
  FileAccess	:= NaN;
end;

destructor TMIME_Part.Destroy;
begin
  ContentType.Free;
end;

function TMIME_Part.GContentCharset:String;
begin
  result := ContentType.Charset;
end;

procedure TMIME_Part.SContentCharset(const newvalue:string);
begin
  ContentType.Charset := newvalue;
end;

{ SendAttach:                                                          }
{ add/delete/remove/edit content parts				       }

procedure SendAttach(parts:TList;Umlaute:Boolean);
const width = 68;
      edb   = 3;
      okb   = 6;
var p,p0      :	integer;	(* current line                 *)
    q,q0      : integer;        (* current start of window      *)
      c0      : integer;
    gl        : integer;        (* max number of lines          *)
    x,y       : Integer;        (* position of dialogue         *)
    poutside  : boolean;
    startmkey : boolean;   { beim Start war Maustaste gedrckt }
    t         : taste;
    bp,rb     : shortint;
    c         : char;
    buttons   : string;
    done      : boolean;

  procedure DispLine(i:integer);
  var 
    s1,s2,s3    : string;
    pa          : TMIME_Part;
    j		: Integer;
  begin
    pa:=TMime_Part(parts[i]);

{ +--------------------------------------------------------------------+ }
{ | FA Dateiname...				    text/plain, qp     | }

    s1:=iifs((i=0) and
      (pa.FileNameO='') and
      (pa.ContentDisposition=mime_inline) and
      (UpperCase(pa.ContentType.Verb)='TEXT/PLAIN'),'N', 
      iifs(pa.ContentDisposition=mime_inline,' ','A'))+
      iifs(pa.IsFile,'F',' ') {$IFDEF Debug}+
      iifs(pa.IsTemp,'T',' ') {$ENDIF};
    
    case pa.ContentEncoding of
      MIME_7bit:  s3:='7bit';
      MIME_8bit:  s3:='8bit';
      MIME_qp:    s3:='qp';
      MIME_binary:s3:='bin';
      MIME_Base64:s3:='b64';
      else        s3:='???';
    end;
    
    s2:=pa.ContentType.Verb;
    s2:=Trim(LeftStr(s2,PosX(';',s2)));
    
    if Length(s2)+Length(s3)>16 then
    begin
      if UpperCase(LeftStr(s2,10))='MULTIPART/'   then s2:='mul./'+Mid(s2,11) else
      if UpperCase(LeftStr(s2,12))='APPLICATION/' then s2:='app./'+Mid(s2,13) else
      if UpperCase(LeftStr(s2,6 ))='AUDIO/'	  then s2:='au./' +Mid(s2, 7) else
      if UpperCase(LeftStr(s2,6 ))='IMAGE/'	  then s2:='img./'+Mid(s2, 7) else
      if UpperCase(LeftStr(s2,8 ))='MESSAGE/'	  then s2:='msg./'+Mid(s2, 9) else
      if UpperCase(LeftStr(s2,6 ))='MODEL/'	  then s2:='mod./'+Mid(s2, 7) else
      if UpperCase(LeftStr(s2,6 ))='VIDEO/'	  then s2:='vid./'+Mid(s2, 7); 
    
      if Length(s2)+Length(s3)>16 then
        s2:=LeftStr(s2,13-Length(s3))+'...';
    end;

    s3:=s2+', '+s3;

    if 0<Length(pa.ContentDescription) then
      s2:=LeftStr(pa.ContentDescription,52-6)
    else if not pa.IsTemp then
      s2:=LeftStr(fitpath(pa.FileName ,52-6),52-6)
    else if 0<Length(pa.FileNameO) then
      s2:=LeftStr(fitpath(pa.FileNameO,52-6),52-6)
    else
      s2:=GetRes2(624,10); { '(intern)' }

    if (i=p) then attrtxt(col.colsel2bar)
    else attrtxt(col.colsel2box);

    s1:=s1+' '+s2+sp(Width-2-Length(s1)-Length(s2)-Length(s3))+' '+s3;
    mwrt(x+1,y+1+i-q,s1);
  end;

  procedure Display;
  var i : integer;
  begin
    // mwrt(x+5,y,'p='+StrS(p)+', q='+StrS(q)+', parts.count='+Strs(Parts.count));
    for i:=q to min(parts.count-1,q+gl-1) do
      DispLine(i);

    if q+gl-1>parts.count then begin
      moff;
      attrtxt(col.colsel2box);
      clwin(x+1,x+width,y+gl-(q+gl-1-parts.count),y+gl);
      mon;
    end;

    attrtxt(col.colsel2rahmen);
    mwrt(x,y+1,iifc(q<=0,'³',#30));
    mwrt(x,y+gl,iifc(q+gl>=parts.count,'³',#31));

    if parts.count<=0 then begin
      attrtxt(col.colsel2bar);
      mwrt(x+1,y+1,sp(width));
    end;

    aufbau:=false;
  end;
  
  procedure readbutt;
  begin
    rbx:=x+1; rby:=y+p-q+1;
    rb:=readbutton(x+2,y+gl+2,2,buttons,bp,false,t);
  end;
  
  procedure maus_bearbeiten;
  var ins1    : boolean;
      inside  : boolean;
      outside : boolean;
      xx,yy   : integer;
  begin
    maus_gettext(xx,yy);
    ins1:=(xx>x) and (xx<=x+width) and (yy>y);
    inside:=ins1 and (yy<=y+gl);
    outside:=not ins1 or (yy>y+gl+2);
    if inside then begin
      if (t=mausleft) or (t=mauslmoved) then
        p:=yy-y-1 else
      if (t=mausunright) or (t=mausunleft) then begin
        t:=keycr;
        poutside:=false
        end else
      if t=mausldouble then begin
        rb:=edb; t:=keycr; end
      end;
    if outside then begin
      if (t=mausleft) or (t=mausright) or (t=mauslmoved) or (t=mausrmoved) then
        poutside:=true else
      if poutside and ((t=mausunleft) or (t=mausunright)) then begin
        rb:=okb; t:=keyesc; end;
      end;
  end;

begin { SendAttach }
  gl:=min(screenlines-11,16);
  selbox(width+2,gl+4,'',x,y,false);
          
  buttons:= GetRes2(624,0); { ' ^Neu , ^L”schen , ^Edit , N. ^Oben , N. ^Unten , S^chlieáen ' }

  maus_pushinside(x+1,x+width,y+1,y+gl);
  poutside:=false;
(*
  pushhp(XXX)
*)

  attrtxt(col.colsel2rahmen);
  mwrt(x,y+gl+1,'Ã'+dup(width,'Ä')+'´');
  t:='!';    { Buttons nur anzeigen }
  readbutt;

  p:=0; p0:=p;
  q:=0; q0:=-1;

  done:=false;
  
  repeat
    if p<0 then p:=0 else
    if p>=parts.count then p:=max(parts.count-1,0);

    if q> p    then q:=p else
    if q<=p-gl then q:=p-gl+1;
    if q<0     then q:=0;

    if q<>q0 then 
    begin
      aufbau:=true;
      q0:=q;
    end;

    if aufbau then 
      display 
    else if p<>p0 then begin
      // mwrt(x+5,y,'p='+StrS(p)+', q='+StrS(q)+', parts.count='+Strs(Parts.count));
      if p0 in [q..(q+gl-1)] then DispLine(p0);
      DispLine(p);
    end;
    p0:=p;

    t:='*';
    readbutt;
    bp:=abs(rb);

    if (t>=mausfirstkey) and (t<=mauslastkey) then
      maus_bearbeiten;

    c:=UpCase(t[1]);

    case rb of
      1: begin 
           SendAttach_Add(parts,x,y+1+p-q,umlaute); 
           p:=parts.count-1; 
           if p0=p then p0:=-1; 	{ if first entry }
         end;
      2: begin
      	   SendAttach_Delete(parts,p);
	   aufbau:=true;
	 end;
      edb: begin
           SendAttach_Edit(parts,p,x,y+1+p-q,umlaute);
	   p0:=-1;
         end;
      4: if p>0 then begin
	   parts.Exchange(p,p-1);
	   dec(p);
	 end;
      5: if (p<parts.count-1) and (parts.count>1) then begin 
	   parts.Exchange(p,p+1);
	   inc(p);
         end;
      okb: break; { repeat }
      else if (not parts.count<=0) and (rb<0) then
      begin
        if t=keyup   then dec(p) else
        if t=keydown then inc(p) else
        if t=keyhome then p:=0 else
        if t=keyend  then p:=parts.count-1 else
        if t=keychom then p:=q else
        if t=keycend then p:=q+gl else
        if t=keypgup then begin dec(p,gl); dec(q,gl); end else
        if t=keypgdn then begin inc(p,gl); inc(q,gl); end;
	if t=keyesc  then break;
      end;
    end;

//    if q<=parts.count-gl then q:=parts.count-gl+1;
  until false;

  maus_popinside;
(*
  pophp;
*)
  closebox;
end;

procedure SendAttach_EditText(pa:TMIME_Part;IsNachricht,Umlaute:Boolean);
var FileName: String;
    OldTime : Longint;
    NewTime : Longint;
    NewTime2: TDateTime;
begin
  if not pa.IsTemp then begin
    FileName := TempS(_FileSize(pa.FileName));
    if not CopyFile(pa.FileName,FileName) then begin
      Fehler(GetRes2(624,100)); exit; end;
    OldTime  := FileAge(FileName);
  end else
    FileName := pa.FileName;

  EditFile(FileName,IsNachricht,true,
    iif(editvollbild,0,2),Umlaute);

  if not pa.IsTemp then begin
    NewTime := FileAge(FileName);
    if NewTime<>OldTime then
    begin		(* modified - use this file from now on *)
      if pa.FileNameO='' then pa.FileNameO := ExtractFileName(pa.FileName);
      pa.FileName := FileName;
      NewTime2 := FileDateToDateTime(NewTime);
      pa.FileModify := NewTime2;
      pa.FileAccess := NewTime2;
      if IsNaN(pa.FileCreate) then pa.FileCreate := NewTime2;
      pa.IsTemp := true;
    end
    else 		(* not modified - just delete copy 	*)
      _era(FileName);
  end;
end;

function on_contenttype_change(var content:string):boolean;
var IsTxt: Boolean;
begin 
  IsTxt := UpperCase(LeftStr(Content, 5))='TEXT/';
  SetFieldEnable(2,IsTxt);
  SetFieldEnable(3,IsTxt);
  result:=true;
end;

procedure SendAttach_Add(parts:TList;x,y:Integer;Umlaute:Boolean);
var pa: TMIME_Part;
    nn: Integer;
    uc: Boolean;
{$IFDEF Win32}
    Handle: THandle;
    FindData: TWin32FindData;
  
  function WinFileTimeToDateTime(var utc:TFILETIME):TDateTime;
  var local: Windows.TFileTime;
      wsyst: Windows.TSystemTime;
    {$IFNDEF Delphi}
      systm: SysUtils.TSystemTime;
    {$ENDIF}
  begin
    if (utc.dwLowDateTime or utc.dwHighDateTime)<>0 then
    begin
      Windows.FileTimeToLocalFileTime(utc,{$IFNDEF Delphi}@{$ENDIF}local);
      Windows.FileTimeToSystemTime(local,{$IFNDEF Delphi}@{$ENDIF}wsyst);
    {$IFNDEF Delphi}
      systm.year        := wsyst.wYear;
      systm.month       := wsyst.wMonth;
      systm.day         := wsyst.wDay;
      systm.hour        := wsyst.wHour;
      systm.minute      := wsyst.wMinute;
      systm.second      := wsyst.wSecond;
      systm.millisecond := wsyst.wMilliSeconds;
      Result:=SystemTimeToDateTime(systm);
    {$ELSE}
      Result:=SystemTimeToDateTime(wsyst);
    {$ENDIF}
    end else
      Result:=NaN;
  end;
{$ENDIF}

begin
  pa := TMIME_Part.Create;

  nn:=MiniSel(x+10,min(y+1,screenlines-3),'',GetRes2(624,1),1);

  case nn of
    1: begin
        {$ifdef UnixFS}
          pa.FileName := '*';
        {$else}
          pa.FileName := '*.*';
        {$endif}
         uc:=false;
         xp1o.ReadFileName(GetRes2(624,50),pa.FileName,true,uc);
         if (pa.FileName='') then begin 
           pa.Free; exit; 
         end;
         if not FileExists(pa.FileName) then begin
           Fehler(Getres2(624,52)); pa.Free; exit; 
         end;

         pa.IsFile := true;
         pa.IsTemp := false;
         pa.FileEOL:= eol_none;

         pa.FileNameO:=ExtractFileName(pa.FileName);

         {$IFDEF Win32}
           Handle := Windows.FindFirstFile(PChar(pa.FileName), {$IFNDEF Delphi}@{$ENDIF}FindData);
           if Handle <> INVALID_HANDLE_VALUE then
           begin
             Windows.FindClose(Handle);
             if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
             begin
               pa.FileModify := WinFileTimeToDateTime(FindData.ftLastWriteTime);
               pa.FileCreate := WinFileTimeToDateTime(FindData.ftCreationTime);
               pa.FileAccess := WinFileTimeToDateTime(FindData.ftLastAccessTime);
             end;
           end;
         {$ELSE}
           pa.FileModify := FileDateToDateTime(FileAge(pa.FileName));
           pa.FileCreate := NaN;
           pa.FileAccess := NaN;
         {$ENDIF}

	 SendAttach_Analyze(pa,true);
       end;
    2: begin
    	 pa.FileName    := TempS($FFFF);
         if pa.FileName='' then begin
           Fehler(GetRes2(624,101)); pa.Free; exit; end;

    	 pa.IsTemp	:= True;
    	 pa.FileCharset := 'CP850';
    	 pa.FileEOL	:= EOL_CRLF;
         
    	 pa.ContentType.AsString := 'text/plain';

         SendAttach_EditText(pa,false,Umlaute);
       end;
     else begin pa.Free; exit; end;
   end;

   if FileExists(pa.FileName) then
     parts.Add(pa)
   else
     pa.Free;
end;

procedure SendAttach_Delete(parts:TList;n:Integer);
var pa: TMIME_Part;
    dummy:Boolean;
    fn: string;
begin
  pa := TMIME_Part(parts[n]);

  if pa.IsTemp then
  begin 
    if not pa.IsFile then
      case ReadIt(60,GetRes2(624,4),GetRes2(624,5),1,dummy) of
        0,3: exit;	{ break; }
	1: begin
//	     fn:=pa.FileNameO;
//	
//	     if ReadFilename(GetRes2(624,3),fn,true,dummy) then
//           begin
//
//	     end else
//  	       exit;
	   end;
      end;
      
    _era(pa.FileName);
  end;

  parts.Delete(n);
  pa.Free;
end;

procedure SendAttach_EditType(pa:TMIME_Part);
var x,y  : Integer;
    brk  : Boolean;
    s    : String;
    IsTxt: Boolean;

    FileCharset	      : string;

    ContentEncoding   :	string;
    ContentType	      : string;
    ContentCharset    : string;
begin
  ContentCharset	:= pa.ContentType.Charset;
                           pa.ContentType.Charset:='';
  ContentType		:= pa.ContentType.AsString;
                           pa.ContentType.Charset:=ContentCharset;
  FileCharset		:= pa.FileCharset;

  case pa.ContentEncoding of
    mime_7bit,mime_8bit,mime_binary:	ContentEncoding:=GetRes2(624,30);
    mime_qp:				ContentEncoding:=GetRes2(624,31);
    mime_base64:			ContentEncoding:=GetRes2(624,31);
  else					ContentEncoding:=''; end;

  IsTxt := (UpperCase(LeftStr(ContentType, 5))='TEXT/');

  dialog(60,11,GetRes2(624,40),x,y);
  maddtext(2,2,GetRes2(624,41),0);
  maddtext(2,3,GetRes2(624,42),0);

  maddstring(2,5,GetRes2(624,45),ContentType,41,maxint,'');
  mappsel(false,GetRes2(624,37));
  mset1func(@on_contenttype_change);

  maddstring(2,7,GetRes2(624,46),FileCharset,12,maxint,'');
  mappsel(false,GetRes2(624,35)); If Not IsTxt then MDisable;

  maddstring(2,8,GetRes2(624,47),ContentCharset,12,maxint,'');
  mappsel(false,GetRes2(624,36)); If Not IsTxt then MDisable;

  maddstring(2,10,GetRes2(624,48),ContentEncoding,16,maxint,'');
  mappsel(true,IIfs(IsTxt,GetRes2(624,30)+'ù','')+GetRes2(624,31)+'ù'+GetRes2(624,32));

  freeres;
  readmask(brk);
  enddialog;

  if brk then exit;

  pa.ContentType.AsString := ContentType;
  if ContentCharset<>'' then
    pa.ContentType.Charset := ContentCharset;

  if (ContentEncoding=GetRes2(624,30)) then
    pa.ContentEncoding := mime_8bit else { DoSend will check whether 8bit is actually needed }
  if (ContentEncoding=GetRes2(624,31)) then
    pa.ContentEncoding := mime_qp else
    pa.ContentEncoding := mime_base64;

end;

procedure SendAttach_EditMeta(pa:TMIME_Part);
var x,y  : Integer;
    brk  : Boolean;
    s    : String;
    IsTxt: Boolean;

    ContentDisposition:	string;
    ContentDescription: string;
    FileName:		string;
    DateCreate,DateModify,DateRead:string;
   
  function dstr(const dt:TDateTime):String; begin if IsNaN(dt) then result:=''
  else DateTimeToString(Result,'dd"."mm"."yyyy"'#255'"hh":"nn":"ss',dt); end; 

  function strd(const dt:string):TDateTime;
  begin
    try

    except
      result := NaN;
    end;
  end;

begin
  FileName := pa.FileNameO;
  ContentDescription := pa.ContentDescription;

  DateCreate := dstr(pa.FileCreate);
  DateModify := dstr(pa.FileModify);
  DateRead   := dstr(pa.FileAccess);

  case pa.ContentDisposition of
    mime_inline:			ContentDisposition:=GetRes2(624,25);
    mime_attach:			ContentDisposition:=GetRes2(624,24);
  else					ContentDisposition:=''; end;

  dialog(60,11,GetRes2(624,40),x,y);
  
  maddstring(2,2,GetRes2(624,13),FileName,41,maxint,'');
  if not pa.IsTemp then
    mappsel(false,ExtractFileName(pa.FileName)+'ù'+pa.FileName);
  maddstring(2,3,GetRes2(624,14),ContentDescription,41,maxint,'');

  maddstring(2,5,GetRes2(624,12),ContentDisposition,16,maxint,'');
  mappsel(true,GetRes2(624,24)+'ù'+GetRes2(624,25));

  maddform(2,7,GetRes2(624,15),DateModify,'  .  .    '#255'  :  :  ',' 0123456789');
  maddform(2,8,GetRes2(624,16),DateCreate,'  .  .    '#255'  :  :  ',' 0123456789');
  maddform(2,9,GetRes2(624,16),DateRead  ,'  .  .    '#255'  :  :  ',' 0123456789');

  freeres;
  readmask(brk);
  enddialog;

  if brk then exit;

  pa.ContentDescription := ContentDescription;
  pa.FileName := FileName;
  
  if (ContentDisposition=GetRes2(624,25)) then
    pa.ContentDisposition := mime_inline else
    pa.ContentDisposition := mime_attach;
end;


procedure SendAttach_Edit(parts:TList;n:integer;x,y:integer;Umlaute:Boolean);
var pa: TMIME_Part;
    s:  String;
    nn: ShortInt;
    t:  Boolean;
begin
  if parts.count<=0 then exit;
  pa := TMIME_Part(parts[n]);
  
  t:=(Uppercase(pa.ContentType.MainType)='TEXT') or
     (Uppercase(pa.ContentType.MainType)='MESSAGE');

  if t then
    s:=GetRes2(624,7)+GetRes2(624,8)
  else 
    s:=GetRes2(624,8);

  nn:=MiniSel(x+10,min(y+1,screenlines-iif(t,5,4)),'',s,1);
  if not t then inc(nn);

  case nn of
    1: SendAttach_EditText(pa,(n=0) and
         (pa.FileNameO='') and
         (pa.ContentDisposition=mime_inline) and
         (UpperCase(pa.ContentType.Verb)='TEXT/PLAIN'),Umlaute);
    2: SendAttach_EditMeta(pa);
    3: SendAttach_EditType(pa);
    4: (* debug *);
  end;
end;

end.
