{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }
{ $Id$ }

{ Exec-Swapper }

{$I XPDEFINE.INC }

unit exxec;

interface

uses
  xpglobal, dos, typeform, fileio, debug;

const
  ExecDeutsch : boolean = true;


// Rckgabewert: ErrorLevel
function Xec(prog:string; const prompt:string):Integer;


implementation  { --------------------------------------------------- }

uses
{$ifdef Linux}Linux,{$endif}sysutils;

function Xec(prog:string; const prompt:string):Integer;
{ Gibt Errorlevel des aufgerufenen Programms zurueck, falls Aufruf
  erfolgen konnte, ansonsten (Programm nicht vorhanden etc.) Fehlercode
  mit negativem Vorzeichen }
var
    pp    : byte;
    para  : string;
    dpath : string;
    TempError: Integer;
begin
{$ifdef UnixFS}
{$ifdef Linux}
  { ToDo: Prompt Modifizieren (vielleicht) }
  Result := Shell(prog);
{$else}
  {$error Please implement this function for your OS }
{$endif}
{$else}
  pp:=pos(' ',prog);
  if pp=0 then para:=''
  else begin
    para:=' '+trim(Mid(prog,pp+1));
    prog:=left(prog,pp-1);
  end;
  prog:=FileUpperCase(prog);

  if (pos('|',para)>0) or (pos('>',para)>0) or (pos('<',para)>0) then
    dpath:=''
  else begin
    if FileExists(prog) then dpath:=prog
    else dpath:=FileUpperCase(fsearch(prog,getenv('PATH')));
    if (right(dpath,4)<>'.EXE') and (right(dpath,4)<>'.COM') then
      dpath:='';
  end;
  if (para<>'') and (para[1]<>' ') then para:=' '+para;
  if dpath='' then begin
    para:=' /c '+prog+para;
    dpath:=getenv('comspec');
  end;
  Debug.TempCloseLog(False);
  SwapVectors;
  DosError :=0;
  Exec(dpath, para);
  TempError:=DosError; if TempError=0 then Result:=DosExitCode else Result:=-TempError;
  SwapVectors;
  Debug.TempCloseLog(True);
  DosError :=0;          { Wird nicht sauber belegt, also von Hand machen }
{$ENDIF }
end;

end.
{
  $Log$
  Revision 1.29  2000/09/05 17:20:15  ma
  - Fix, DosError bei FPC 1.0 seltsam?

  Revision 1.28  2000/09/03 20:36:40  ma
  - Fix zur Errorlevel-Rueckgabe. Vorsicht: Verhalten geaendert!- Siehe
    Hinweis beim Kopf von Xec

  Revision 1.27  2000/08/20 12:22:40  mk
  - Linux fix

  Revision 1.26  2000/08/16 00:38:10  ma
  - Handling der Logdatei fuer die Unit Debug hinzugefuegt

  Revision 1.25  2000/08/14 20:41:22  mk
  - Exec-Routinen besser auf 32 Bit Bedingungen angepasst

  Revision 1.24  2000/07/20 16:49:56  mk
  - Copy(s, x, 255) in Mid(s, x) wegen AnsiString umgewandelt

  Revision 1.23  2000/07/04 12:04:15  hd
  - UStr durch UpperCase ersetzt
  - LStr durch LowerCase ersetzt
  - FUStr durch FileUpperCase ersetzt
  - Sysutils hier und da nachgetragen

  Revision 1.22  2000/06/23 15:59:10  mk
  - 16 Bit Teile entfernt

  Revision 1.21  2000/06/22 19:53:25  mk
  - 16 Bit Teile ausgebaut

  Revision 1.20  2000/06/20 18:21:17  hd
  - Xec angepasst (teilweise) / Linux

  Revision 1.19  2000/05/20 02:07:39  mk
  - 32 Bit/VP: FindFirst/FindNext aus Dos-Unit statta us SysTools verwendet

  Revision 1.18  2000/05/18 05:52:38  mk
  - UseBatch ausgeschaltet

  Revision 1.17  2000/05/13 15:12:27  oh
  -persoenliche debug-Zeile entfernt

  Revision 1.16  2000/05/07 17:50:07  mk
  - Limits fuer Dateinamen entfernt

  Revision 1.15  2000/05/06 17:27:54  mk
  - weiterer Exxec-Fix fuer lange Kommandozeilen

  Revision 1.14  2000/05/05 00:10:49  oh
  -PGP-Aufrufe ueber Batch-Datei

  Revision 1.13  2000/05/04 15:24:47  oh
  -Parameter-Stringbegrenzung aufgehoben

  Revision 1.12  2000/04/26 18:31:21  mk
  - Para auf 255 vergroessert

  Revision 1.11  2000/04/16 20:38:49  mk
  - Fixes fuer FindFirst (2)

  Revision 1.10  2000/04/16 19:50:38  mk
  - Fixes fuer FindFirst

  Revision 1.9  2000/04/13 12:48:31  mk
  - Anpassungen an Virtual Pascal
  - Fehler bei FindFirst behoben
  - Bugfixes bei 32 Bit Assembler-Routinen
  - Einige unkritische Memory Leaks beseitigt
  - Einge Write-Routinen durch Wrt/Wrt2 ersetzt
  - fehlende CVS Keywords in einigen Units hinzugefuegt
  - ZPR auf VP portiert
  - Winxp.ConsoleWrite provisorisch auf DOS/Linux portiert
  - Automatische Anpassung der Zeilenzahl an Consolengroesse in Win32

  Revision 1.8  2000/03/14 15:15:35  mk
  - Aufraeumen des Codes abgeschlossen (unbenoetigte Variablen usw.)
  - Alle 16 Bit ASM-Routinen in 32 Bit umgeschrieben
  - TPZCRC.PAS ist nicht mehr noetig, Routinen befinden sich in CRC16.PAS
  - XP_DES.ASM in XP_DES integriert
  - 32 Bit Windows Portierung (misc)
  - lauffaehig jetzt unter FPC sowohl als DOS/32 und Win/32

  Revision 1.7  2000/03/09 23:39:32  mk
  - Portierung: 32 Bit Version laeuft fast vollstaendig

  Revision 1.6  2000/03/08 22:36:33  mk
  - Bugfixes für die 32 Bit-Version und neue ASM-Routinen

  Revision 1.5  2000/02/19 11:40:07  mk
  Code aufgeraeumt und z.T. portiert

}
