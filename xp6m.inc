{ ------------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.                   }
{ (c) 2002      OpenXP/16 & Michael Heydekamp, http://www.openxp16.de }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.         }
{                                                                     }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der     }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.       }
{ ------------------------------------------------------------------- }
{ $Id$ }


{ MIME-Encoding Unterroutinen fÅr XP6.PAS }


Const  Mime_Head        = 'XPMIMEHD.TMP';
       Mime_End         = 'XPMIMEFT.TMP';
       Default_Boundary = '-==_CrossPoint_Next_MIME_Part_==-';

Var    Mime_Temppath  : pathstr;         { Extrakt-Pfad fuer die MIME-Parts }
       Max_Mime_Parts : word;            { Anzahl der MimeParts }

{***************************************************************************}
{ Hilfsroutinen }
{***************************************************************************}

{ Name fuer Mime-Part generieren }
{--------------------------------}
function Mimename(i:integer):string;     
begin
  Mimename:=mime_temppath+'MIME'+formi(i,4)+'.TMP';
end;


{ MimeTyp aus Content-Type String  } 
{----------------------------------}
function mime_typ(s:string):string;      
var
  s2: string;
begin
  s2:=trim(mid(s,14));
  mime_typ:=lstr(left(s2,cposx(';',s2)-1));
end;


{ Filename aus Content-Type String }
{----------------------------------}
function mime_file(s:string):string;     
var
  a,b : byte;
begin
  mime_file:='';
  a:=pos('name=',lstr(s));
  if a=0 then exit;
  a:=iif(s[a+5]='"',a+6,a+5);
  b:=cpos('"',mid(s,a))-1;
  mime_file:=copy(s,a,b);
end;


{ Charset aus Content-Type String }
{---------------------------------}
function mime_charset(s:string):string;     
var
  a,b : byte;
begin
  mime_charset:='';
  a:=pos('charset=',lstr(s));
  if a=0 then exit;
  a:=iif(s[a+8]='"',a+9,a+8);
  b:=cpos('"',mid(s,a))-1;
  mime_charset:=lstr(copy(s,a,b));
end;


{ Extension in Mimetyp aus Mime-DB umwandeln }
{--------------------------------------------}
function getmimetyp(ext:pathstr):string;
var s:string;
begin
  if ustr(ext)='TXT' then getmimetyp:='text/plain' else
  if ustr(ext)='HTM' then getmimetyp:='text/html' else
  begin
    getmimetyp:='application/octet-stream';
    if ext='' then exit;
    dbSeek(mimebase,mtiExt,Ustr(ext));
    if dbFound then begin
      dbReadN(mimebase,mimeb_typ,s);
      s:=extmimetyp(s);
      if s<>'' then getmimetyp:=lstr(s);
      end;
    end;
end;


{ Intro- und Extro-Bloecke der Mime-Mail erstellen }
{--------------------------------------------------}
procedure MakeMimeFrame;
var OutFile  : text;
begin
  assign(Outfile,temppath+Mime_Head);
  rewrite(Outfile);
  Writeln(Outfile, 'This is a multi-part message in MIME format.');
  Writeln(Outfile,'--'+Default_Boundary);
  Writeln(Outfile, 'Content-Type: text/plain; charset=ISO-8859-1');
  Writeln(Outfile, 'Content-Transfer-Encoding: 8bit');
  Writeln(Outfile);
  Close(Outfile);
  assign(Outfile,temppath+Mime_End);
  rewrite(Outfile);
  writeln(Outfile,'--'+Default_Boundary+'--');
  Close(Outfile);
end;


{ Datei nach Charset-Angabe durchsuchen }
{---------------------------------------}
function Used_Charset(fn:pathstr):string;
var
    f1      : text;
    s       : string;
begin
  Used_Charset:=''; s:=' ';
  assign(f1,fn);
  reset(f1);
  while not eof(f1) and (ustr(left(s,13))<>'CONTENT-TYPE:') and (s<>'')
    do readln(f1,s);
  if not eof(f1) then Used_Charset:=lstr(mime_charset(s));
  close(f1);
end;


{ Mime-Part loeschen, folgende Parts umbenennen }
{-----------------------------------------------}
procedure delete_part(nr:integer);
var
  i : integer;
begin
  _era(mimename(nr));
  for i:=nr to max_mime_parts-2 do
    _rename(mimename(i+1),mimename(i));
  dec(max_mime_parts);
  if max_mime_parts=1 then max_mime_parts:=0;
end; 


{ Gesamtlaenge aller Mime-Parts ermitteln }
{-----------------------------------------}
function mime_size:longint;
var i:word;
    n:longint;
begin
  n:=0;
  if mime_attach then
    for i:=1 to max_mime_parts do
      if exist(mimename(i)) then         { Mime-Parts }
        inc(n,_filesize(mimename(i)));
  mime_size:=n;
end;


{***************************************************************************}
{ MIME-Messi zum Nacheditieren wieder in Einzelfiles splitten }
{***************************************************************************}

function Split_Mime_Parts(fn:pathstr):boolean;
VAR
    Infile, OutFile   : text;
    Line,Line2        : String;
    i                 : word;
    first_textpart    : word;
    header_end        : boolean;
    Bd_len            : byte;
    ISO_Part          : boolean;
    Part_CTE          : byte;

Const
    plain = 0;
    QP    = 1;
    B64   = 2;

begin
  moment;
  MakeMimeFrame;
  Bd_Len:=length(MimeBoundary)+2;
  assign(Infile,fn);
  reset(Infile);

  { Evtl. Vorspann von 1. MIME-Boundary trennen und oben an ersten Part haengen }

  readln(Infile,line);
  if left(line,bd_len)<>'--'+mimeboundary then
  begin
    readln(Infile,line2);
    if (left(line,Bd_Len)<>'--'+Mimeboundary)
     and (left(line2,Bd_Len)<>'--'+Mimeboundary)
    then begin
      assign(Outfile,temppath+Mime_Head);
      append(Outfile);
      while not eof(Infile) and (left(Line2,Bd_Len)<>'--'+Mimeboundary) do
      begin
        writeln(Outfile,line);
        Line:=Line2;
        Readln(Infile,line2);
        end;
      Close(Outfile);
      end;
    end;

  { MIME-Parts ausschliesslich Abschluss-Boundary nach MIMExxxx.TMP }

  i:=1; first_textpart:=0;
  while not eof(Infile) and (left(line,Bd_len+2)<>'--'+MimeBoundary+'--') do
  begin
    Assign(Outfile,mimename(i));
    Rewrite(Outfile);
    header_end:=false;
    Iso_part:=false; part_CTE:=plain;
    Writeln(Outfile,'--'+Default_Boundary);
    While not eof(infile) do
    begin
      readln(infile,line);
      if line='' then header_end:=true;
      if (not header_end) and (ustr(left(line,26))='CONTENT-TRANSFER-ENCODING:')
        then if lstr(trim(mid(line,27)))='quoted-printable' then part_CTE:=QP
          else if lstr(trim(mid(line,27)))='base64' then part_CTE:=B64;
      if (not header_end) and (ustr(left(line,13))='CONTENT-TYPE:')
        then begin
          line2:=mime_charset(line);
          iso_part:=iso_charset(line2);
          end;
      if (not header_end) and (first_textpart=0)   { Ersten Textpart merken }
       and (ustr(left(line,13))='CONTENT-TYPE:')
        and (mime_typ(line)='text/plain') and (mime_file(line)='')
         and (iso_part or (mime_charset(line)='') or ascii_charset(mime_charset(line)))
          then first_textpart:=i;
      if (first_textpart=i) and (part_CTE=B64) then first_textpart:=0;  { 1. Part: Nicht base64! }
      if left(line,Bd_Len)='--'+Mimeboundary then break;
      if part_CTE=QP then UnquotePrintable(line,true,false,false);
      if Iso_part or (first_textpart=i) then CharsetToIBM(line2,line);
      writeln(Outfile,line);
      end;
    Close(Outfile);
    inc(i);
    end;
  Max_Mime_Parts:=i;

  { Tempdatei fuer Editor gegen ersten erkannten Textpart tauschen }

  Close(Infile);
  erase(Infile);
  if first_textpart=0 then Makefile(fn)   { Dummyfile, wenn's keinen Textpart gab }
  else begin
    Assign(infile,mimename(first_textpart));
    reset(infile);
    Assign(outfile,fn);
    rewrite(outfile);
    repeat readln(infile,line) until eof(infile) or (line='');
    while not eof(infile) do
    begin
      readln(infile,line);
      writeln(outfile,line);
      end;
    close(Outfile);
    close(infile);
    delete_part(first_textpart);
    end;

  Closebox;
  MimeBoundary:=Default_Boundary;
  Split_Mime_Parts:=true;
end;



{***************************************************************************}
{ MIME-Versende-Menue }
{***************************************************************************}
var neuaufbau  : boolean;
    listerline : word;
const width  = 70;


{ Markier-Routine fuer Mime-Menue (Lister) }
{------------------------------------------}
function mime_mark_func(var s:string; block:boolean):boolean;
begin
  mime_mark_func:=false;
  if (s=first_marked) or (list_markanz=0)     { Nur eine markierte Zeile zulassen }
  then mime_mark_func:=true else pushkey(copy(getres2(647,3),4,1));  { 'P' }
end;


{ Tastaturabfrage fuer Mime-Menue (Lister) }
{------------------------------------------}
procedure Mime_Send_Keys(var t:taste); { Lister-Tastenabfrage fuer MIME-Menue }


  { Partnummer fuer "mimename" aus Listerzeile ausschneiden }
  {---------------------------------------------------------}
  function part_nr(s:string):word;
  begin
    part_nr:=ival(copy(s,width+1,10));
  end;


  { Zwei Mime-Parts miteinander vertauschen }
  {-----------------------------------------}
  procedure Swap_Mime_Parts(var t:Taste);
  var
    s  : pathstr;
  begin
    if (list_markanz=0) or (get_Selection=first_marked) 
      then begin pushkey(' '); exit; end;
    s:=temps(0);
    _rename(mimename(part_nr(first_marked)),s);
    _rename(mimename(part_nr(get_selection)),
            mimename(part_nr(first_marked)));
    _rename(s,mimename(part_nr(get_selection))); 
    neuaufbau:=true;
    t:=keyesc;
  end;  


  { Datei zu Base64-Mime-Block umformen und als MIMExxxx.TMP speichern }
  {--------------------------------------------------------------------}
  procedure MimeEncoder;
  var
      fn      : Pathstr;
      useclip : boolean;
      n       : word;
      s       : string;
      f1      : file;
      f2      : text;
      buffer  : tbytestream;
  begin
    fn:=sendpath;
    useclip:=false;
    if not readfilename(getres2(647,5),fn,true,useclip)    { 'Dateianhang auswÑhlen' }
      then exit;
    if not exist(fn) then
    begin
      rfehler(616); { 'Datei nicht vorhanden' }
      exit;
    end; 
    if _filesize(fn)*3/2>tempfree
    then Begin
      rfehler(617); { 'zu wenig Platz auf TemporÑr-Laufwerk' } 
      exit
      end;
    Moment;
    if Max_Mime_Parts=0 then inc(Max_Mime_Parts); 
    if MimeBoundary='' then MimeBoundary:=Default_Boundary;
    assign(f2,mimename(Max_Mime_Parts));
    rewrite(f2);
    Writeln(f2,'--'+MimeBoundary); 
    writeln(f2,'Content-Type: '+getMimetyp(getfileext(fn))+
               '; name="'+getfilename(fn)+'"');
    writeln(f2,'Content-Transfer-Encoding: base64');
    writeln(f2,'Content-Disposition: attachment; filename="'+getfilename(fn)+'"');
    writeln(f2); 
    assign(f1,fn);
    fm_ro; reset(f1,1); fm_rw;
    while not eof(f1) do
    begin
      blockread(f1,buffer,54,n);
      encode_base64(buffer,n,s);
      writeln(f2,s);
      end;
    writeln(f2);
    close(f1);
    close(f2);
    inc(Max_Mime_Parts);  
    Closebox;
  end;


  { Textparts erstellen/bearbeiten }
  {--------------------------------}
  procedure modify_textpart;
  var s        : Pathstr;
      s2       : String;
      t1,t2,t3 : Text;
      new      : boolean;
      uhrcol   : byte;
  begin
    new:=true;
    if kb_Ctrl or (not kb_shift and (trim(left(get_selection,27))=getres2(647,8))  { '(Textanhang)' }
      and (trim(copy(get_selection,width-17,17))=''))
    then begin
      if (kb_Ctrl) and (max_mime_parts=0) then 
        begin errsound; exit; end;
      new:=false;
      s:=mimename(part_nr(get_selection));
      pushhp(8078);
      if not (iso_charset(used_Charset(s)) or ascii_charset(used_Charset(s))
        or (used_Charset(s)='') or ReadJN(getreps2(647,6,left(used_charset(s),
                                          screenwidth-6-length(getres2(647,6))+1)),false))
        then begin pophp; exit; end;  { 'Nicht unterstÅtzter Zeichensatz: "%s" Trotzdem bearbeiten' }
      pophp; 
      assign(t1,s);
      reset(t1);
      assign(t2,tempS(0));
      rewrite(t2);
      repeat                               { Evtl. alten MIME-Part-Header ausschneiden }
        readln(t1,s2);
        writeln(t2,s2);
      until eof(t1) or (s2='');
      close(t2);
      assign(t3,TempS(0)); 
      rewrite(t3);
      while not eof(t1) do
      begin
        readln(t1,s2);
        writeln(t3,s2);
        end;
      close(t1);
      erase(t1);
      Close(t3);      
      rename(t3,s);
      end
    else s:=temps(0);
    wpush(1,80,1,2,'-');
    attrtxt(col.coledithead);
    wrt(1,1,forms(getres2(647,7),79));  { 'Textanhang bearbeiten' }
    uhrcol:=dphback; dphback:=col.coledithead;
    EditFile(s,false,false,false,1,false);
    dphback:=uhrcol;
    wpop;
    if _filesize(s)<>0 then begin
      MimeBoundary:=default_Boundary;
      if new then begin                                 { Neuer Textpart-Header...}
        if Max_Mime_Parts=0 then inc(Max_Mime_Parts);
        assign(t2,mimename(Max_Mime_Parts));
        inc(max_mime_Parts);
        rewrite(t2);
        writeln(t2,'--'+MimeBoundary);
        writeln(t2,'Content-Type: text/plain; charset=ISO-8859-1');
        writeln(t2,'Content-Transfer-Encoding: 8bit');
        writeln(t2);
      end else append(t2);
      assign(t3,s);
      reset(t3);
      while not eof(t3) do begin
        readln(t3,s2);
        writeln(t2,s2);
        end;
      close(t3);
      erase(t3);
      close(t2);
      if not new then rename(t2,s); 
      end;
  end;  


begin
  if (cpos(UpCase(t[1]),getres2(647,3))=1) or (t=keyins) then  { 'N' }
    if max_mime_parts>9999 then errsound
    else begin
      MimeEncoder;
      neuaufbau:=true;
      t:=keyesc;
    end;
  if (cpos(UpCase(t[1]),getres2(647,3))=2) or (t=^T) then      { 'T' }
    if max_mime_parts>9999 then errsound
    else begin
      modify_textpart; 
      neuaufbau:=true; 
      t:=keyesc; 
    end;
  if (cpos(UpCase(t[1]),getres2(647,3))=3) or (t=keydel) then  { 'L' }
  begin
    if (get_selection<>'') and ReadJN(getres2(647,4),false)  { 'Dateianhang entfernen' }
      then delete_part(part_nr(get_selection));
    neuaufbau:=true;
    t:=keyesc;
  end;
  if cpos(UpCase(t[1]),getres2(647,3))=4 then                  { 'P' }
    Swap_Mime_Parts(t);
  listerline:=ival(mid(get_selection,width+1+10));  { Lister Zeilennummer merken }
end;  


{---------------------------}
{ ** MIME-VERSENDE-MENUE ** }
{---------------------------}
function MimeSendMenu:boolean;
  var brk    : boolean;
      x,y,zen: byte;
      i,cnt  : word;
      line   : string;
      t      : text;
      height : byte; 
      pos,add: byte;
      s1     : string[80];

  function siz(size:longint):string;
  begin
    if size<10000 then siz:=strsn(size,5)
    else
      if size<1024*1024 then
        siz:=strsn(size div 1024,4)+'K'
      else
        if size<1024*1024*1024 then
          siz:=strsn(size div (1024*1024),3)+'MB'
        else
          siz:=strsn(size div (1024*1024*1024),3)+'GB';
  end;

  function mime_type(s:string):string;
  var 
    s2:string;
  begin
    s2:=mime_typ(s);
    if (s2='text/plain') and (mime_file(s)='')
      then s2:=getres2(647,8);  { '(Textanhang)' }
    mime_type:=s2;
  end;   

begin
  pushhp(8077);
  if not mime_attach then pushkey(copy(getres2(647,3),1,1));  { 'N' }
  repeat
    cnt:=0; 
    Neuaufbau:=false;
    if max_mime_parts=0 then height:=5
      else height:=min(10,max(5,Max_Mime_parts-1));
    selbox(width+2,height+2,getres2(647,1),x,y,true);  { 'Anhang-Typ ƒƒ Grî·e ƒƒ Dateiname' }
    zen:=((width+2)-length(getres2(647,2))) div 2;
    mwrt(x+zen,y+height+1,getres2(647,2));             { '< >eu ƒƒ < >ext ƒƒ < >îschen ƒƒ < >os ƒƒ <   >' }
    s1:=getres2(647,2);
    add:=0;
    attrtxt(col.colselhigh);
    for i:=1 to length(getres2(647,3))+1 do
    begin
      pos:=cpos('<',s1);
      if pos=0 then break;
      mwrt(x+zen+pos+add,y+height+1,                   { ' N        T         L            P        Esc ' }
          iifs(i<=length(getres2(647,3)),copy(getres2(647,3),i,1),'Esc'));
      s1:=mid(s1,pos+1);
      add:=pos+add;
    end;
    openlist(x+1,x+width,y+1,y+height,0,'/NS/SB/CR/NLR/M/');
    ListboxCol;
    listarrows(x+width+1,y+1,y+height,col.colselrahmen,col.colselrahmen,'≥');
    for i:=1 to Max_Mime_Parts do
      if Exist(mimename(i)) then
      begin
        assign(t,mimename(i));
        reset(t);
        repeat 
          readln(t,line) 
        until eof(t) or (ustr(left(line,13))='CONTENT-TYPE:');
        app_L('  '+forms(mime_type(line),25)+'     '+siz(_filesize(mimename(i)))
              +'      '+forms(mime_file(line),width-44)+' '+forms(strs(i),10)+strs(cnt));
        close(t);
        inc(cnt);
        end;
    listtp(Mime_Send_Keys);
    listVmark(Mime_Mark_Func); 
    list(brk);
    if neuaufbau then               { Wenn Lister neu gestartet wird  }
      for cnt:=1 to listerline do   { alte Cursorpos wiederherstellen }
        pushkey(keydown);
    closelist;
    closebox;
  until not brk or not Neuaufbau;
  MakeMimeFrame;  
  MimeSendMenu:=cnt>0;
  pophp;
end;


{
  $Log$
  Revision 1.1.2.1  2002/04/19 16:38:06  my
  JG[+MY]: MIME-Multipart-Versand (RFC/ZConnect) implementiert :-):
           OpenXP/16 kann jetzt standardkonforme MIME-Multipart-Nachrich-
           ten erzeugen und versenden. Es kînnen sowohl im Sendefenster
           als auch direkt im Editor (!) Dateien und Textteile beliebiger
           Anzahl und Grî·e an die aktuelle Nachricht angehÑngt werden.
           Die énderung der Reihenfolge bereits angehÑngter Nachrichten-
           teile ist mîglich, das Weiterleiten von MIME-Multipart-
           Nachrichten mittels N/W/K, N/W/O, N/W/E und N/W/R wird jetzt
           ebenfalls unterstÅtzt. Weitere Details siehe Hilfe (?/S/A).
           Kompletter Sourcecode fÅr XP entwickelt von JG, Anpassungen
           an und Einbau in OpenXP/16 durch MY.
           Spezieller Dank an HH fÅr die Vorarbeit im Rahmen der
           Entwicklung des XP-Tools XPBMIME, dessen Arbeitsweise teilweise
           als Ansto· und Vorlage fÅr die aktuelle XP-Implementation
           diente, sowie an JM fÅr seine Mitarbeit daran, speziell im
           Bereich Zeichensatzbehandlung und ZConnect-KonformitÑt.

}
