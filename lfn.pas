{$F+,I-,O-,R-,S-,V-,G+,N+,X+}
{$IFDEF Ver70}
{$T-}                                                                  {!!.10}
{$ENDIF}

{---
  Vor dem Benutzen dieser Unit oder Ñndern eines DEFINE's LFN.GER lesen!
  Ein deutscher Interfaceteils ist in LFNINT.GER verfÅgbar.
  Eine deutsche FILE_ID.DIZ ist in LFN_IDG.DIZ verfÅgbar.
  Siehe WHATSNEW.GER fÅr die énderungen nach der letzten Version.

  Before integrate this unit or changing any DEFINE's read LFN.ENG!
  An english interfacepart is in LFNINT.ENG avaible.
  An english FILE_ID.DIZ is in LFN_IDE.DIZ avaible.
  See WHATSNEW.ENG for the changes after the last version.
---}

{-DEFINE's die das Verhalten dieser Unit bestimen}

{--------------------------------------------------------------------}
{-                          LFN.IN0 v1.10                           -}
{-      DEFINE's die das Verhalten von LFN.PAS v1.10 bestimmen      -}
{-          Conditional DEFINE's that affect LFN.PAS v1.10          -}
{--------------------------------------------------------------------}

{-Low Level Routinen der Object Professionals Toolbox aktivieren}
{-Enable to use the low level routines of the Object Professional Toolbox}
{.$DEFINE UseOpro}

  {+++++
  Anmerkung: Es wird immer mind. eine der Units Dos/WinDos benutzt, auch
  wenn beide DEFINEs deaktiviert sind. Siehe LFN.GER unter
  "Anmerkung zu UseDos/UseWinDos/CompatToDos"

  Note: Allways one of the 2 units Dos/WinDos is used, as well as you
  disable both following DEFINE's. See LFN.ENG section
  "Notice to UseDos/UseWinDos/CompatToDos"
  +++++}

{-Die Funktionen der Unit Dos patchen}
{-Hook into unit Dos}
{$DEFINE UseDos}

{-Die Funktionen der Unit WinDos patchen}
{-Hook into unit WinDos}
{.$DEFINE UseWinDos}

  {-Wenn UseDos und UseWinDos aktiviert sind, dann FINDFIRST kompatibel zur
    Unit Dos machen, ansonten kompatibel zur unit WinDos.}
  {-If you enable UseDos and UseWinDos, then export FINDFIRST compatible to
    unit Dos, otherwise to unit WinDos.}
{$DEFINE CompatToDos}

  {-DiskFree/DiskSize liefern einen Comp, ansonsten einen LongInt zurÅck.
    Siehe LFN.GER unter "Sonderfall DISKFREE/DISKSIZE".}
  {-DiskFree/DiskSize return the result as Comp, otherwise as LongInt.
    See LFN.ENG section "Special case DISKFREE/DISKSIZE".}
{$DEFINE DFSReturnComp}

  {-DiskFree/DiskSize liefern die Grî·e in Kilobytes, ansonsten in Bytes
    zurÅck}
{-DiskFree/DiskSize return the size in Kb, otherwise return in bytes}
{.$DEFINE DFSReturnKb}

  {+++++
  ACHTUNG: Vor Aktivierung einer der beiden folgenden DEFINE's zuerst LFN.GER
  unter "Sonderfall ASSIGN" nachlesen!

  Attention: Before enable one of the following 2 DEFINE's read in LFN.ENG
  the section "Special case ASSIGN"!
  +++++}

{-Zuweisung von Dateinamen > 79 Zeichen an ASSIGN ermîglichen}
{-Allow filenames larger than 79 chars pass to ASSIGN}
{.$DEFINE AssignLongName}

{-AssignDispose automatisch bei CLOSE aufrufen}
{-Force CLOSE to call AssignDispose automatically}
{.$DEFINE AutoAssignDispose}

  {+++++
  Vor der énderung eines der nÑchsten beiden DEFINE's bitte LFN.GER unter
  "Anmerkung zu UseXXXXSharingFlags" lesen.

  Before changing one of the 2 following defines please read LFN.ENG under
  "Notice to UseXXXXSharingFlags".
  +++++}

{-Automatisch die FileSharingFlags bei Dateien vom Typ TEXT setzen}
{-Set filesharingmode's automatically on files which typ is TEXT}
{.$DEFINE UseTEXTSharingFlags}

{-Automatisch die FileSharingFlags bei Dateien vom Typ FILE setzen}
{-Set filesharingmode's automatically on files which typ is FILE}
{.$DEFINE UseFILESharingFlags}

{-----------------------------------------------------------------------------
        Alle folgenden Zeilen gewÑhrleisten die ordnungsgemÑ·e Funktion
             dieser Unit und sollten daher NIEMALS geÑndert werden.

               All following lines ensure the proper function of
                     this unit and should NEVER be changed.
-----------------------------------------------------------------------------}

{$IFDEF Windows}
  {$UNDEF UseDos}
  {$DEFINE UseWinDos}
{$ENDIF}

{$IFDEF Ver60}
  {$UNDEF UseWinDos}
  {$DEFINE UseDos}
{$ENDIF}

{$IFNDEF UseWinDos}
  {$DEFINE UseDos}
  {$DEFINE CompatToDos}
{$ELSE}
  {$IFNDEF UseDos}
    {$UNDEF CompatToDos}
  {$ENDIF}
{$ENDIF}

{$IFNDEF AssignLongName}
  {$UNDEF AutoAssignDispose}
{$ENDIF}


{$IFDEF Dpmi}
  {$C FIXED PRELOAD PERMANENT}
{$ENDIF}

{$IFDEF Windows}
  {$C FIXED PRELOAD PERMANENT}
{$ENDIF}

unit
  LFN;

  {--------------------------------------------------------------------}
  {-                   LFN.PAS v1.10 Final Release                    -}
  {-     UnterstÅztung der langen Dateinamen unter Win95/98 fÅr       -}
  {-               TP6, TP/BP7 und kompatible Compiler.               -}
  {-           Geschrieben von Andreas Killer, Germany, NRW           -}
  {-      homepage: http://home.t-online.de/home/andreas.killer       -}
  {--------------------------------------------------------------------}

interface
uses
{$IFDEF UseOpro}
  OpString, OpInline,
{$ENDIF}
{$IFDEF Windows}
  WinProcs,
{$ENDIF}
{$IFDEF UseDos}
  Dos
  {$IFDEF UseWinDos} , {$ELSE}
    {$IFDEF Ver70} , Strings ; {$ELSE} ; {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF UseWinDos}
  WinDos, Strings;
{$ENDIF}

{$IFDEF Ver60}
type
  PChar = ^Char;                                                       {!!.07}
{$ENDIF}

const
  {-Werden unter Win95 Dateien erstellt, so kînnen diese mit einem bestimmten
    Alias (die Zahl hinter dem ~ z.B.: ABCDEF~1) versehen werden, um unter
    MSDOS die Datei noch identifizieren zu kînnen. 0 = automatisch vergeben,
    bzw. ermitteln.}
  FileAlias : Byte = 0;

{$IFDEF AssignLongName}
type
  {-Anmerkung: in "var F" wird die jeweilige Filevariable Åbergeben}
  AssignNewProc = procedure(var F; FName : PChar);                     {!!.07}
  AssignGetNameFunc = function(var F) : Pointer;
  AssignDisposeProc = procedure(var F);
var
  {-Diese Procedure wird zum Anlegen des Dateinamens auf dem Heap benutzt.}
  AssignNew : AssignNewProc;
  {-Liefert einen Zeiger auf den Dateinamen zurÅck}
  AssignGetName : AssignGetNameFunc;
  {-Gibt den durch AssignNew belegten Heap wieder frei.}
  AssignDispose : AssignDisposeProc;
{$ENDIF}

{.F-}
const
  {-Mîgliche zusÑtzliche Flags fÅr Filemode:}
  fmDenyAll    = $0010;  {-file sharing mode}
  fmDenyWrite  = $0020;  {-file sharing mode}
  fmDenyRead   = $0030;  {-file sharing mode}
  fmDenyNone   = $0040;  {-file sharing mode}
  fmNoInherit  = $0080;  {-no-inherit flag}
  fmNoBuffer   = $0100;  {-do not buffer data (requires that all reads/writes be exact physical sectors)}
  fmNoCompress = $0200;  {-do not compress file even if volume normally compresses files}
  fmUseAlias   = $0400;  {-use alias hint in DI as numeric tail for short-name alias}
  {-Anmerkung: fmUseAlias wird automatisch gesetzt wenn FileAlias einen Wert <> 0 hat.}
  fmError24    = $2000;  {-return error code instead of generating INT 24h if critical error while opening file}
  fmCommit     = $4000;  {-commit file after every write operation}
{.F+}

const
  {-Mîgliche zusÑtzliche Trennzeichen in der Kommandozeile (siehe LFN.GER)}
  ParamDelims : set of Char = ['"'];
var
  {-Zeigt auf die Kommandozeile (siehe EXREDIR.PAS)}
  ParamPtr : ^String;

function ParamLine : String;
  {-Liefert die komplette Kommandozeile zurÅck}
function ParamPosition(Index : Word) : Word;
  {-Liefert die Position eines Parameters in der Kommandozeile}

  {============================== Abschnitt 1: =============================}
  {======== Alternative Funktionen, die als Ersatz konzipiert sind: ========}
  {=========================================================================}

type
  {-Dieser Record musste erweitert werden, da DOS.SearchRec nur
    12 stellige Dateinamen ermîglicht.}
  SearchRec =
    record
      Fill : array[1..21] of Byte;
      Attr : Byte;
      Time : LongInt;
      Size : LongInt;
      {$IFDEF CompatToDos}
      Name : String;              {-String des langen Dateinamens}
      {$ELSE}
      Name : array[0..259] of Char; {-AsciiZ des langen Dateinamens}
      {$ENDIF}
      Handle : Word;              {-Filehandle fÅr Find-First,-Next,-Close}
    end;

{$IFDEF CompatToDos}
procedure FindFirst(Path : String; Attr : Word; var F : SearchRec);
  {-Sucht ein Verzeichnis nach dem ersten Vorkommen eines Dateinamens ab.}
{$ELSE}
type
  TSearchRec = SearchRec;

procedure FindFirst(Path : PChar; Attr : Word; var F : TSearchRec);
  {-Sucht ein Verzeichnis nach dem ersten Vorkommen eines Dateinamens ab.}
{$ENDIF}

procedure FindNext(var F : SearchRec);
  {-Setzt eine mit FindFirst begonnene Suche nach der dort angegebenen
    Datei fort.}
procedure FindClose(var F : SearchRec);
  {-Beendet eine mit FindFirst begonnene Suche. FindClose wird von FindNext
    automatisch aufgerufen, wenn keine weitere Datei mehr gefunden wurde.
    Achtung: Wird dieser Zustand NICHT erreicht, dann MUSS FindClose explizit
    aufgerufen werden um den von FindFirst belegten FileHandle wieder
    freizugeben. Unter DOS hat diese Procedure keine Auswirkung.}

type
  PathStr = String;                                                    {!!.05}
  DirStr = String;
  NameStr = String;
  ExtStr = String;

{$IFDEF UseDos}
procedure FSplit(Path : String; var Dir : DirStr; var Name : NameStr;
                 var Ext : ExtStr);
  {-Zerlegt einen vollstÑndigen Dateinamen in seine drei Komponenten.}
function FExpand(Path : PathStr) : PathStr;
   {-Erweitert einen unvollstÑndig angegebenen Dateinamen um den
     dazugehîrigen Suchpfad.}
{$ENDIF}

function DiskFree(Drive : Byte) :
  {$IFDEF DFSReturnComp} Comp ; {$ELSE} LongInt; {$ENDIF}
  {-Liefert die Grî·e des freien Speicherplatzes auf einem Laufwerk zurÅck.}
function DiskSize(Drive : Byte) :
  {$IFDEF DFSReturnComp} Comp ; {$ELSE} LongInt; {$ENDIF}
  {-Liefert die GesamtkapazitÑt eines Laufwerks zurÅck.}

{---
  Anmerkung: DiskFree/DiskSize rufen AX=$7303/INT $21 auf. Diese Funktion
  liefert im Programm unter Windows/DOS7.x einwandfreie Ergebnisse. Wenn man
  allerdings das Prg in der IDE (Turbo oder BP) entwickelt, dann liefern sie
  bei mir nur nach dem ersten Run das korrekte Ergebnis. Warum entzieht sich
  meiner Kenntnis.
---}

  {============================== Abschnitt 2: =============================}
  {==== Erweiterte Funktionen, auch unter DOS lauffÑhig (siehe LFN.GER): ===}
  {=========================================================================}

procedure SetCreateFTime(var F; Time : LongInt);
  {-Setzt Datum und Uhrzeit der Erstellung einer Datei.}
procedure GetCreateFTime(var F; var Time : LongInt);
  {-Ermittelt Datum und Uhrzeit der Erstellung einer Datei.}

{$IFDEF UseDos}
function TruePathName(PathName : String) : String;
  {-Liefert den kÅrzesten Pfad zu einer Datei, oder erweitert den
    Dateinamen mit dem momentanen Verzeichnis}
function GetShortName(PathName : String) : String;
  {-Wandelt einen langen Pfadnamen in das 8.3 Format}
function GetLongName(PathName : String) : String;
  {-Wandelt einen kurzen Pfadnamen (im 8.3 Format) in einen
    langen um (unter MSDOS gibt diese Funktion den 8.3 Namen zurÅck)}
{$ENDIF}

{$IFDEF UseWinDos}
function TruePathNamePChar(Dest, PathName : PChar) : PChar;
  {-Liefert den kÅrzesten Pfad zu einer Datei, oder erweitert den
    Dateinamen mit dem momentanen Verzeichnis}
function GetShortNamePChar(Dest, PathName : PChar) : PChar;
  {-Wandelt einen langen Pfadnamen in das 8.3 Format}
function GetLongNamePChar(Dest, PathName : PChar) : PChar;
  {-Wandelt einen kurzen Pfadnamen (im 8.3 Format) in einen langen um}
{$ENDIF}

procedure FlushDrive(Drive : Byte);
  {-Erzwingt das Schreiben der Buffers und des Cache.
      FÅr Drive gilt: 0 = aktuelles Laufwerk, 1 = Laufwerk A, usw.}
procedure ResetDrive(Drive : Byte);
  {-Erzwingt das Schreiben und Lîschen der Buffers und des Cache.
      FÅr Drive gilt: 0 = aktuelles Laufwerk, 1 = Laufwerk A, usw.}
function IsNetworkDrive(Drive : Char) : Boolean;
  {-True wenn Drive ein Laufwerk im Netzwerk ist}

  {========================================================================}

function Win95_Aktiv : Boolean;
  {-Ermittelt ob Win95 aktiv ist}
procedure EnableLFN;
  {-Aktiviert die UnterstÅtzung fÅr lange Dateinamen}
procedure DisableLFN;
  {-Deaktiviert die UnterstÅtzung fÅr lange Dateinamen}

  {========================================================================}

implementation
{$F+,I-}
{$IFDEF UseWinDos}
type
  Registers = TRegisters;
  FileRec = TFileRec;
  TextRec = TTextRec;
  TextBuf = TTextBuf;
const
  {-Grî·e temporÑrer lokal genutzter PChars}
  PCharSize = 512;
{$ENDIF}
type
  PatchRec =
    record
      Code : Byte;
      Addr : Pointer;
    end;

  SpecialPatchRec =
    record
      Instr : array[0..1] of Byte;
      Code : Byte;
      Addr : Pointer;
    end;

  PatchType = (
    {-Unit System}
    pMkDir, pRmDir, pChDir, pGetDir,
    pFReset, pFRewrite, pErase, pRename,
    pAssignText,
    pAssignFile,                                                       {!!.05}
    pFClose,                                                           {!!.05}
    pParamCount, pParamStr,                                            {!!.04}
  {$IFDEF UseDos}
    {-Unit Dos}
    pGetFAttr_D, pSetFAttr_D, pGetFTime_D, pSetFTime_D,
    pFSearch,                                                          {!!.08}
    pGetEnv,                                                           {!!.09}
  {$ENDIF}
    {!!.06 begin}
  {$IFDEF UsewinDos}
    {-Unit WinDos}
    pCreateDir, pRemoveDir, pSetCurDir, pGetCurDir,
    pGetFAttr_W, pSetFAttr_W, pGetFTime_W, pSetFTime_W,
    pGetArgCount, pGetArgStr,
    pFileExpand, pFileSplit,
    pFileSearch,                                                       {!!.08}
  {$ENDIF}
    {!!.06 end}
    pDummy);

const
  LowPatchType = pMkDir;
  HighPatchType = pDummy;

var
  {-Array fÅr den Originalcode der gepatchten Routinen}
  SavePatch : array[PatchType] of PatchRec;
  SpecialSavePatch : array[pAssignText..pAssignFile] of SpecialPatchRec; {!!.07}
  {-Die Adressen der gepatchten Routinen}
  PatchAddr : array[PatchType] of Pointer;
  Regs : Registers;

const
  On = True;
  Off = False;
var
  SaveHeapFunc : Pointer;

  function LFNHeapFunc(Size : Word) : Integer; Far;
  begin
    LFNHeapFunc := 1;
  end;

  procedure HeapFunc(On : Boolean);
    {-Installiert temporÑr eine HeapFunc}
  begin
    if On then begin
      SaveHeapFunc := HeapError;
      HeapError := @LFNHeapFunc;
    end
    else
      HeapError := SaveHeapFunc;
  end;

{.F-}
const
  {-Bitkonstanten der Procs dieser Unit}
  bSetCreateFTime = $0001;
  bGetCreateFTime = $0002;
  bFindFirstNext  = $0004;
  bFSplit         = $0008;
  bTruePathName   = $0010;
  bGetShortName   = $0020;
  bGetLongName    = $0040;
  bFExpand        = $0080;

  LFN_AllProcs    = bSetCreateFTime+bGetCreateFTime+bFindFirstNext+bFSplit+
                    bTruePathName+bGetShortName+bGetLongName+bFExpand;

  LFN_Proc : Word = 0;  {-Bitmerker welche Proc aktiv ist}
{.F+}

  procedure EnableLFNFunc(Which : PatchType); Forward;                 {!!.04}
  {-Aktiviert die gewÅnschte Funktion dieser Unit}
  procedure DisableLFNFunc(Which : PatchType); Forward;
  {-Deaktiviert die gewÅnschte Funktion dieser Unit}

  {-Low Level Routinen:
      Asc2Str, Str2Asc, AscSize, AddZero, POPJumpFAR, Int21file, Int21name,
      Int21namePChar, StUpcase, JustPathName, JustName, ClearFlag, FlagIsSet,
      AddBackSlash, DefaultDrive}
  {--------------------------------------------------------------------}
  {-                          LFN.IN1 v1.10                           -}
  {-               Low Level Routinen fÅr LFN.PAS v1.10               -}
  {--------------------------------------------------------------------}

{$IFNDEF Ver70}
  function StrMove(Dest, Source : PChar; Count : Word) : PChar; Assembler;
  asm
    push    ds
    cld
    lds     si,Source
    les     di,Dest
    mov     ax,di
    mov     dx,es
    mov     cx,Count
    cmp     si,di
    jae     @@1
    std
    add     si,cx
    add     di,cx
    dec     si
    dec     di
@@1: rep     movsb
    cld
    pop     ds
  end;

  function StrRScan(Str : PChar; Chr : Char) : PChar; Assembler;
  asm
    cld
    les     di,str
    mov     cx,0FFFFH
    xor     al,al
    repne   scasb
    not     cx
    std
    dec     di
    mov     al,Chr
    repne   scasb
    mov     ax,0
    cwd
    jne     @@1
    mov     ax,di
    mov     dx,es
    inc     ax
@@1: cld
  end;

  function StrLen(Str : PChar) : Word; Assembler;
  asm
    cld
    les     di,str
    mov     cx,0FFFFH
    xor     al,al
    repne   scasb
    mov     ax,0FFFEH
    sub     ax,cx
  end;

{$ENDIF}

  function Asc2Str(var Asc) : String; Assembler;
    {-Wandelt einen ASCIIZ in einen String}
  asm
    {-LÑnge des ASCIIZ ermitteln}
    les     di, Asc               {-ES:DI => Asc}
    mov     bx, di                {-Offset sichern}
    mov     cx, $FFFF             {-Maximale LÑnge suchen}
    cld                           {-VorwÑrts..}
    xor     ax, ax                {-..nach 0 suchen}
    repne   scasb
    sub     di, bx                {-LÑnge des ASCIIZ berechnen}
    dec     di
    mov     cx, di                {-In CX Åbertragen}
    cmp     cx, 255               {-LÑnger als 255 Zeichen?}
    jle     @@Copy
    {-Auf 255 Zeichen begrenzen}
    mov     cx, 255
@@Copy:
    {-ASCIIZ nach String kopieren}
    push    ds
    les     di, @Result           {-ES:DI => Asc2Str}
    mov     ax, cx
    stosb                         {-Resultierende LÑnge setzen}
    jcxz    @@Done
    lds     si, Asc               {-DS:SI => Asc}
    rep     movsb                 {-Bytes kopieren}
@@Done:
    pop     ds
  end;

  procedure Str2Asc(var S, D); Assembler;
    {-Wandelt einen String in einen ASCIIZ}
  asm
    push    ds
    les     di, D                 {-ES:DI => D}
    lds     si, S                 {-DS:SI => S}
    cld                           {-VorwÑrts kopieren}
    lodsb                         {-LÑnge des String nach AL}
    xor     ch,ch
    mov     cl, al                {-CX = LÑnge des String}
    jcxz    @@Done                {-öberhaupt was zu tun?}
    rep     movsb                 {-String 1 Byte nach links schieben}
@@Done:
    xor     al, al
    stosb                         {-ASCIIZ anfÅgen}
    pop     ds
  end;

  function AscSize(var Asc) : Word; Assembler;                         {!!.05}
    {-Gibt den benutzten Speicher eines ASCIIZ zurÅck}
  asm
    les     di, Asc               {-ES:DI => Asc}
    cld                           {-aufwÑrts suchen}
    xor     al, al                {-AL:= 0}
    mov     bx, di                {-DI in BX sichern}
    mov     cx, 0FFFFh            {-max. Bereich setzen}
    repne   scasb                 {-Nach der ersten 0 suchen}
    sub     di, bx                {-Anzahl Bytes bis 0 errechnen}
    mov     ax, di                {-In AX zurÅckgeben}
  end;

  procedure AddZero(var S);
    {-HÑngt eine 0 an den String}
  inline(
    $5F/                          {-pop    di          ;}
    $07/                          {-pop    es          ;}
    $30/$FF/                      {-xor    bh, bh      ;}
    $26/$8A/$1D/                  {-mov    bl, es:[di] ;}
    $FE/$C3/                      {-inc    bl          ;}
    $26/$88/$1D/                  {-mov    es:[di], bl ;}
    $30/$C0/                      {-xor    al, al      ;}
    $26/$88/$01);                 {-mov    es:[bx+di], al;}

  procedure POPJumpFAR(Dest : Pointer);
    {-Springt zum angegebenen Ziel}
  inline(
    $5F/                          {-pop  di            ;DI = Ofs(Dest)}
    $07/                          {-pop  es            ;ES = Seg(Dest)}
    $89/$EC/                      {-mov  sp, bp        ;Stack korrigieren}
    $5D/                          {-pop  bp            ;}
  {$IFDEF Dpmi}
    $A1/>SelectorInc/             {-mov ax, SelectorInc}
    $26/$29/$45/$02/              {-sub es:[di+2], ax}
  {$ENDIF}
    $26/$FF/$6D/$00);             {-jmp  far es:[di]   ;Sprung zum Ziel}

{.F-}
const                                                                  {!!.05}
  {-Offsets eines TextRec}
  fHandle    = 0;
  fMode      = 2;
  fBufSize   = 4;
  fPrivate   = 6;
  fBufPos    = 8;
  fBufEnd    = 10;
  fBufPtr    = 12;
  fOpenProc  = 16;
  fInOutProc = 20;
  fFlushProc = 24;
  fCloseProc = 28;
  fUserData  = 32;
  fName      = 48;
  fBuffer    = 128;
{.F+}

  function Int21file(rAX : Word; var F) : Boolean;
    {-FÅhrt einen INT 21 mit FileRec(F).Name in DS:DX durch}
  var
    P : Pointer;
  begin
    with FileRec(F), Regs do begin
      AX := rAX;
      DS := Seg(Name);
      DX := Ofs(Name);
    {$IFDEF AssignLongName}
      {!!.05 begin}
      P := AssignGetName(F);
      if P <> nil then begin
        DS := Seg(P^);
        DX := Ofs(P^);
      end;
      {!!.05 end}
    {$ENDIF}
      Flags := 0;                                                      {!!.08}
      Intr($21, Regs);                                                 {!!.06}
      Int21file := Flags and FCarry = 0;
    end;
  end;

  function Int21name(rAX : Word; Name : String) : Boolean;
    {-FÅhrt einen INT 21h mit Name in DS:SI durch}
  begin
    Str2Asc(Name, Name);
    with Regs do begin
      AX := rAX;
      DS := Seg(Name);
      SI := Ofs(Name);
      Flags := 0;                                                      {!!.08}
      Intr($21, Regs);                                                 {!!.06}
      Int21name := Flags and FCarry = 0;
    end;
  end;

{$IFDEF UseWinDos}
  function Int21namePChar(rAX : Word; Name : PChar) : Boolean;         {!!.06}
    {-FÅhrt einen INT 21h mit Name in DS:SI durch}
  begin
    with Regs do begin
      AX := rAX;
      DS := Seg(Name^);
      SI := Ofs(Name^);
      Flags:= 0;                                                       {!!.08}
      Intr($21, Regs);            
      Int21namePChar := Flags and FCarry = 0;
    end;
  end;
{$ENDIF}

{$IFNDEF UseOpro}
  function StUpcase(S : String) : String;
    {-Wandelt einen String in Gro·buchstaben}
  var
    I : Byte;
  begin
    for I := 1 to Length(S) do
      StUpcase[I] := Upcase(S[I]);
    StUpcase[0] := S[0];
  end;

  function JustPathName(PathName : String) : String;
    {-Liefert nur den Pfad zurÅck}
  var
    I : Byte;
  begin
    I := Length(PathName);
    while (I > 0) and (not(PathName[I] in ['\', ':'])) do
      Dec(I);
    Inc(I);
    if I > 0 then
      Delete(PathName, I, 255);
    JustPathName := PathName;
  end;

  function JustName(PathName : String) : String;
    {-Liefert nur den Dateinamen}
  var
    I : Byte;
  begin
    I := Length(PathName);
    while (not(PathName[I] in ['\', ':'])) and (I > 0) do              {!!.05}
      Dec(I);
    if I = 0 then                                                      {!!.05}
      JustName := PathName                                             {!!.05}
    else                                                               {!!.05}
      JustName := Copy(PathName, I+1, 255);
  end;

  procedure ClearFlag(var Flags : Word; FlagMask : Word);
    {-Clear the bit(s) specified by FlagMask in Flags}
  inline(
    $58/                          {pop ax         ;FlagMask into AX}
    $5F/                          {pop di}
    $07/                          {pop es         ;ES:DI => Flags}
    $F7/$D0/                      {not ax         ;FlagMask := not FlagMask}
    $26/$21/$05);                 {and es:[di],ax ;Flags := Flags and not FlagMask}

  function FlagIsSet(Flags, FlagMask : Word) : Boolean;
    {-Returns True if the bit specified by FlagMask is set in Flags}
  inline(
    $5A/                          {pop dx    ;FlagMask into DX}
    $58/                          {pop ax    ;Flags into AX}
    $21/$D0/                      {and ax,dx ;Mask out everything not in FlagMask}
    $74/$02/                      {jz  Exit}
    $B0/$01);                     {mov al,1  ;AL = Ord(True)}
  {Exit:}

  function AddBackSlash(S : String) : String;
    {-FÅgt ggf. einen Backslash einem Pfad hinzu}
  var
    SLen : Byte absolute S;
  begin
    if S[SLen] <> '\' then begin
      Inc(SLen);
      S[SLen] := '\';
    end;
    AddBackSlash := S;
  end;
{$ENDIF}

  function DefaultDrive : Char; Assembler;
    {-Gibt das momentane Laufwerk als Char zurÅck}
  asm
    mov     ah, 19h
    int     21h
    add     al, 'A'
  end;


  {-Ersatzfunktionen fÅr Unit System (Teil 1):
      MkDir, RmDir, ChDir, GetDir, GetFAttr, SetFAttr,
      SetFTime, SetCreateFTime, GetFTime, GetCreateFTime}
  {--------------------------------------------------------------------}
  {-                          LFN.IN2 v1.10                           -}
  {-   Ersatzfunktionen fÅr Unit System (Teil 1) fÅr LFN.PAS v1.10    -}
  {--------------------------------------------------------------------}

  procedure Win95MkDir(S : String); Far;
    {-Erzeugt ein Unterverzeichnis.}
  begin
    InOutRes := 0;

    {-String zu Verwendung als ASCIIZ erweitern}
    AddZero(S);

    with Regs do begin
      AX := $7139;
      DS := Seg(S);
      DX := Ofs(S[1]);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pMkDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pMkDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95RmDir(S : String);
    {-Lîscht ein leeres Unterverzeichnis.}
  begin
    InOutRes := 0;

    {-String zu Verwendung als ASCIIZ erweitern}
    AddZero(S);

    with Regs do begin
      AX := $713A;
      DS := Seg(S);
      DX := Ofs(S[1]);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pRmDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pRmDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95ChDir(S : String);
    {-Wechselt das aktuelle Verzeichnis.}

    function ChDrive(Drive : Char) : Boolean; Assembler;
      {-Wechselt das Laufwerk}
    asm
      mov     bx, 1               {-ChDrive:= True}
      mov     ah, 0Eh             {-Laufwerk wechseln}
      mov     dl, Drive
      and     dl, 0DFh            {-Nur Gro·buchstaben}
      sub     dl, 'A'             {-A: = 0, etc. umrechnen}
      int     21h
      mov     ah, 19h             {-Laufwerk ermitteln}
      int     21h
      cmp     al, dl              {-Ist es gesetzt worden?}
      je      @@Done
      dec     bx                  {-Nein, dann False}
@@Done:
      mov     ax, bx              {-Ergebnis nach AX}
    end;

  begin
    InOutRes := 0;

    {-Laufwerk wechseln}
    if (Length(S) >= 2) and (S[2] = ':') then begin
      if not ChDrive(S[1]) then
        {-UngÅltiges Laufwerk}
        InOutRes := 15;
      if Length(S) = 2 then
        Exit;
    end;

    {-String zu Verwendung als ASCIIZ erweitern}
    AddZero(S);

    with Regs do begin
      AX := $713B;
      DS := Seg(S);
      DX := Ofs(S[1]);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pChDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pChDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95GetDir(D : Byte; var StrP; StrLen : Word);
    {-Ermittelt das momentan gesetzte Verzeichnis eines Laufwerks.}
  var
    S : String;
  begin
    InOutRes := 0;

    S := ':\';
    with Regs do begin
      {-Das momentane Laufwerk ermitteln}
      if D = 0 then
        S[0] := DefaultDrive
      else
        S[0] := Char(D+Ord('A')-1);
      {-Das momentane Verzeichnis ermitteln}
      AX := $7147;
      DL := D;
      DS := Seg(S);
      SI := Ofs(S[3]);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pGetDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pGetDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX
      else begin
        {-ASCIIZ in String wandeln}
        S := Asc2Str(S);
        {-In die Quelle Åbertragen}
        if Length(S) < StrLen then
          StrLen := Length(S);
        Move(S, StrP, StrLen+1);
      end;
    end;
  end;

  procedure Win95GetFAttr(var F; var Attr : Word);
    {-Liefert die Attribute einer Datei zurÅck.}
  begin
    DosError := 0;
    Attr := 0;                                                         {!!.06}

    with Regs do begin
      BL := 0;                    {-retrieve attributes}
      if not Int21file($7143, F) then
        {-Fehler}
        if AX = $7100 then begin
         {$IFDEF UseDos}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pGetFAttr_D);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pGetFAttr_D]);
         {$ELSE}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pGetFAttr_W);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pGetFAttr_W]);
         {$ENDIF}
        end
        else
          {-Fehlercode setzen}
          DosError := AX
      else
        {-Attribut zurÅckliefern}
        Attr := CX;
    end;
  end;

  procedure Win95SetFAttr(var F; Attr : Word);
    {-Setzt die Attribute einer Datei.}
  begin
    DosError := 0;

    with Regs do begin
      BL := 1;                    {-set attributes}
      CX := Attr;
      if not Int21file($7143, F) then
        {-Fehler}
        if AX = $7100 then begin
         {$IFDEF UseDos}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pSetFAttr_D);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pSetFAttr_D]);
         {$ELSE}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pSetFAttr_W);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pSetFAttr_W]);
         {$ENDIF}
        end
        else
          {-Fehlercode setzen}
          DosError := AX;
    end;
  end;

  procedure Win95SetFTime(var F; Time : LongInt);
    {-Setzt Datum und Uhrzeit der letzten VerÑnderung einer Datei.}
  begin
    DosError := 0;

    with Regs do begin
      DI := Time shr 16;          {-new last-write date}
      CX := Time and $FFFF;       {-new last-write time}
      BL := 3;                    {-set last write date/time}
      if not Int21file($7143, F) then
        {-Fehler}
        if AX = $7100 then begin
         {$IFDEF UseDos}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pSetFTime_D);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pSetFTime_D]);
         {$ELSE}
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pSetFTime_W);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pSetFTime_W]);
         {$ENDIF}
        end
        else
          {-Fehlercode setzen}
          DosError := AX;
    end;
  end;

  procedure SetCreateFTime(var F; Time : LongInt);
    {-Setzt Datum und Uhrzeit der Erstellung einer Datei.}
  begin
    DosError := 0;

    if not FlagIsSet(LFN_Proc, bSetCreateFTime) then begin
      {-Funktion nicht aktiv, Original aufrufen}
      SetFTime(F, Time);
      Exit;
    end;

    with Regs do begin
      DI := Time shr 16;          {-new creation date}
      CX := Time and $FFFF;       {-new creation time}
      BL := 7;                    {-set creation date/time}
      {-SI = hundredths (10-millisecond units past time in CX, 0-199)}
      SI := 0;
      if not Int21file($7143, F) then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          ClearFlag(LFN_Proc, bSetCreateFTime);
          {-Original aufrufen}
          SetFTime(F, Time);
        end
        else
          {-Fehlercode setzen}
          DosError := AX;
    end;
  end;

type
  FileInf =                                                            {!!.07}
    record
      FileAttr : LongInt;         {-file attributes}
      CreationTime,               {-creation time (0 = unsupported)}
      LastAccessTime,             {-last access time (0 = unsupported)}
      LastWriteTime : Comp;       {-last write time}
      VolumeSerialNumber : LongInt; {-volume serial number}
      FileSize : Comp;            {-size of file}
      FileLinks : LongInt;        {-number of links to file}
      FileIdentifier : Comp;      {-unique file identifier}
      {-Note: the file identifer and volume serial number together uniquely
        identify a file while it is open; the identifier may change when the
        system is restarted or the file is first opened}
    end;

  procedure SwapComp(var C : Comp); Assembler;                         {!!.07}
    {-Vertauscht das niederwertige und das hîherwertige LongInt}
  asm
    les     di, C
    mov     ax, es:[di+4]
    mov     bx, es:[di+6]
    xchg    es:[di+0], ax
    xchg    es:[di+2], bx
    xchg    ax, es:[di+4]
    xchg    bx, es:[di+6]
  end;

  function GetFileInfoByHandle(var F; var FI : FileInf) : Word;        {!!.07}
    {-Liefert erweiterte Informationen Åber eine geîffnete Datei}
  begin
    GetFileInfoByHandle := 0;
    with Regs do begin
      AX := $71A6;
      BX := FileRec(F).Handle;
      DS := Seg(FI);
      DX := Ofs(FI);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        GetFileInfoByHandle := AX
      else
        with FI do begin
          SwapComp(FileSize);
          SwapComp(FileIdentifier);
        end;
    end;
  end;

  function MakeDosTime(FileTime : Comp; var DosTime : LongInt) : Word; {!!.07}
    {-Wandelt eine Win95 64-bit Datum/Zeitangabe in eine DOS 32-bit}
  begin
    MakeDosTime := 0;
    with Regs do begin
      AX := $71A7;
      BL := 0;
      DS := Seg(FileTime);
      SI := Ofs(FileTime);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        MakeDosTime := AX
      else
        DosTime := LongInt(DX) shl 16+CX;
    end;
  end;

  procedure Win95GetFTime(var F; var Time : LongInt);        {!!.07 rewritten}
    {-Ermittelt Datum und Uhrzeit der letzten VerÑnderung einer Datei.}
  var
    FI : FileInf;
  begin
    Time := 0;                                                         {!!.06}

    {-Dateiinformationen ermitteln}
    DosError := GetFileInfoByHandle(F, FI);
    {-Wenn kein Fehler, dann Zeitangabe konvertieren}
    if DosError = 0 then
      DosError := MakeDosTime(FI.LastWriteTime, Time);

    if DosError <> 0 then
      if Regs.AX = $7100 then begin
        {$IFDEF UseDos}
        {-Funktion wird nicht unterstÅzt, deaktivieren}
        DisableLFNFunc(pGetFTime_D);
        {-Original aufrufen}
        POPJumpFAR(@PatchAddr[pGetFTime_D]);
        {$ELSE}
        {-Funktion wird nicht unterstÅzt, deaktivieren}
        DisableLFNFunc(pGetFTime_W);
        {-Original aufrufen}
        POPJumpFAR(@PatchAddr[pGetFTime_W]);
        {$ENDIF}
      end;
  end;

  procedure GetCreateFTime(var F; var Time : LongInt);       {!!.07 rewritten}
    {-Ermittelt Datum und Uhrzeit der Erstellung einer Datei.}
  var
    FI : FileInf;
  begin
    Time := 0;                                                         {!!.06}

    if not FlagIsSet(LFN_Proc, bGetCreateFTime) then begin
      {-Funktion nicht aktiv, Original aufrufen}
      GetFTime(F, Time);
      Exit;
    end;

    {-Dateiinformationen ermitteln}
    DosError := GetFileInfoByHandle(F, FI);
    {-Wenn kein Fehler, dann Zeitangabe konvertieren}
    if DosError = 0 then
      DosError := MakeDosTime(FI.CreationTime, Time);                  {!!.10}

    if DosError <> 0 then
      if Regs.AX = $7100 then begin
        {-Funktion wird nicht unterstÅzt, deaktivieren}
        ClearFlag(LFN_Proc, bGetCreateFTime);
        {-Original aufrufen}
        GetFTime(F, Time);
      end;
  end;

  {-Ersatzfunktionen fÅr Unit System (Teil 2):
      Erase, Rename, Reset, Rewrite, Assign, Close, etc.,
      LFNAssignNew, LFNAssignGetName, LFNAssignDispose}
  {--------------------------------------------------------------------}
  {-                          LFN.IN3 v1.10                           -}
  {-   Ersatzfunktionen fÅr Unit System (Teil 2) fÅr LFN.PAS v1.10    -}
  {--------------------------------------------------------------------}

  procedure Win95Erase(var F);
    {-Lîscht eine Datei.}
  begin
    InOutRes := 0;

    with Regs do begin
{---
  SI = wildcard and attributes flag
     0000h wildcards are not allows, and search attributes are
             ignored
     0001h wildcards are allowed, and only files with matching
             names and attributes are deleted
---}
      SI := 0;
      if not Int21file($7141, F) then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pErase);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pErase]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95Rename(var F; Newname : String);
    {-Gibt einer externen Datei einen neuen Namen.}
  begin
    InOutRes := 0;

    {-String zu Verwendung als ASCIIZ erweitern}
    AddZero(Newname);

    with Regs do begin
      ES := Seg(Newname);
      DI := Ofs(Newname[1]);
      if not Int21file($7156, F) then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pRename);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pRename]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  function Win95FileOpenPrim(var F; RSize : Word; Action : Word;
                             OpenTextFile : Boolean) : Boolean;
  var
    P : Pointer;
    SaveFileMode : Word;
  begin
    InOutRes := 0;

    {-Diese Funktion liefert nur dann False, wenn die gewÅnschte Funktion
      nicht verfÅgbar ist!}
    Win95FileOpenPrim := True;

    with Regs do begin
      AX := $716C;

      {!!.07 begin}
      {$IFDEF UseFileSharingFlags}
      if not OpenTextFile then begin
        SaveFileMode := FileMode;
        {-Ein FILE soll geîffnet werden, SharingFlags lîschen}
        FileMode := FileMode and $FF8F;
        {-Und entsprechend wieder setzen}
        case FileMode and $03 of
          0 : FileMode := FileMode or fmDenyWrite;
          1 : FileMode := FileMode or fmDenyRead;
          2 : FileMode := FileMode or fmDenyAll;
        end;
      end;
      {$ENDIF}
      {!!.07 end}

      if FileAlias <> 0 then                                           {!!.03}
        BX := FileMode or fmUseAlias                                   {!!.03}
      else                                                             {!!.03}
        BX := FileMode;
      {-CX = attributes}
      CX := 0;
{---
 DX = Bitfields for Windows95 long-name open action:
   Bit(s)  Description
    0      open file (fail if file does not exist)
    1      truncate file if it already exists (fail if file does not exist)
    4      create new file if file does not already exist (fail if exists)
   Note:   the only valid combinations of multiple flags are bits 4&0 and 4&1
---}
      DX := Action;
      DS := Seg(FileRec(F).Name);
      SI := Ofs(FileRec(F).Name);
    {$IFDEF AssignLongName}
      {!!.05 begin}
      P := AssignGetName(F);
      if P <> nil then begin
        DS := Seg(P^);
        SI := Ofs(P^);
      end;
      {!!.05 end}
    {$ENDIF}
      {-DI = alias hint (number to append to short filename for
        disambiguation)}
      DI := FileAlias;
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if (Flags and FCarry <> 0) then
        if AX = $7100 then
          {-Funktion nicht verfÅgbar}
          Win95FileOpenPrim := False
        else begin
          {-Fehlercode fÅr IOResult setzen}
          {$IFDEF UseFileSharingFlags}
          if AX = 80 then                                              {!!.07}
            AX := 5;                                                   {!!.07}
          {$ENDIF}
          InOutRes := AX
        end
      else
        {-Dateizugriff erfolgreich}
        with FileRec(F) do begin
          Handle := AX;
          if OpenTextFile then                                         {!!.03}
            {Mode := FileMode and $FB+$D7B1}
            Mode := FileMode and $03+$D7B1                             {!!.07}
          else                                                         {!!.03}
            Mode := $D7B3;                                             {!!.03}
          RecSize := RSize;
        end;
    end;
    {-FileAlias wieder auf automatische Zuweisung umstellen}
    FileAlias := 0;

    {!!.07 begin}
    {$IFDEF UseFileSharingFlags}
    if not OpenTextFile then
      FileMode := SaveFileMode;
    {$ENDIF}
    {!!.07 end}
  end;

  procedure Win95FReset(var F; RSize : Word);
    {-ôffnet eine existierende Datei.}
  begin
    if FileRec(F).Mode <> fmClosed then                                {!!.09}
      Close(file(F));                                                  {!!.09}

    if not Win95FileOpenPrim(F, RSize, $01, False) then begin
      {-Funktion nicht verfÅgbar, deaktivieren}
      DisableLFNFunc(pFReset);
      {-Original anspringen}
      POPJumpFAR(@PatchAddr[pFReset]);
    end;
  end;

  procedure Win95FRewrite(var F; RSize : Word);
    {-Erzeugt und îffnet eine neue Datei.}
  var
    I : Integer;
  begin
    if FileRec(F).Mode <> fmClosed then                                {!!.09}
      Close(file(F));                                                  {!!.09}

    {-Eine bereits bestehende Datei MUSS zuerst gelîscht werden}
    Erase(file(F));
    I := IoResult;
    case I of
      {-Diese Fehler werden ignoriert}
      0 : {-File gelîscht} ;
      2 : {-File nicht gefunden} ;
      5 : {-Zugriff verweigert} ;                                      {!!.05}
    else
      {-Fehler wieder herstellen}
      InOutRes := I;
      Exit;
    end;

    if not Win95FileOpenPrim(F, RSize, $10, False) then begin
      {-Funktion nicht verfÅgbar, deaktivieren}
      DisableLFNFunc(pFRewrite);
      {-Original anspringen}
      POPJumpFAR(@PatchAddr[pFRewrite]);
    end;
  end;

  function TEXTREAD(var T : TextRec) : Word; Assembler;      {!!.05 rewritten}
    {-ErfÅllt die gleiche Funktion wie SYSTEM.FILEREAD}
  asm
    push    ds
    les     di, T
    lds     dx, es:[di].fBufPtr
    mov     cx, es:[di].fBufSize
    mov     bx, es:[di].fHandle
    mov     ah, 3Fh
    int     21h
    jc      @@2
    mov     es:[di].fBufEnd, ax
    xor     ax, ax
@@1:
    mov     word ptr es:[di].fBufPos, 0
    pop     ds
    jmp     @@Done
@@2:
    mov     word ptr es:[di].fBufEnd, 0
    jmp     @@1
@@Done:
  end;

  function TEXTWRITE(var T : TextRec) : Word; Assembler;     {!!.05 rewritten}
    {-ErfÅllt die gleiche Funktion wie SYSTEM.FILEWRITE}
  asm
    push    ds
    les     di, T
    lds     dx, es:[di].fBufPtr
    xor     cx, cx
    xchg    cx, es:[di].fBufPos
    mov     bx, es:[di].fHandle
    mov     ah, 40h
    int     21h
    jc      @@1
    sub     ax, cx
    je      @@1
    mov     ax, 101               {-RTE 101: Write error}
@@1:
    pop     ds
  end;

  function TEXTWRITEDEV(var T : TextRec) : Word; Assembler;            {!!.05}
    {-ErfÅllt die gleiche Funktion wie SYSTEM.FILEWRDEV}
  asm
    push    ds
    les     di, T
    lds     dx, es:[di].fBufPtr
    xor     cx, cx
    xchg    cx, es:[di].fBufPos
    mov     bx, es:[di].fHandle
    mov     ah, 40h
    int     21h
    jc      @@1
    xor     ax, ax
@@1:
    pop     ds
  end;

  function TEXTCLOSE(var T : TextRec) : Word; Assembler;     {!!.05 rewritten}
    {-ErfÅllt die gleiche Funktion wie SYSTEM.FILECLOSE}
  asm
    les     di, T
    mov     bx, es:[di].fHandle
    cmp     bx, 4
    jbe     @@1
    mov     ah, 3Eh
    int     21h
    jc      @@2
@@1:
    xor     ax,ax
@@2:
{$IFDEF AssignLongName}
  {$IFDEF AutoAssignDispose}
    push    ax
    les     di, T
    push    es
    push    di
    call    AssignDispose
    pop     ax
  {$ENDIF}
{$ENDIF}
  end;

  function OpenIOCTL(var T : TextRec) : Word; Assembler;               {!!.05}
    {-Testet einen GerÑtetreiber ob er ansprechbar ist}
  asm
    les     di, T
    mov     ax, 4400h             {-IOCTL: Attribut lesen}
    mov     bx, 1                 {-Handle}
    mov     es:[di].fHandle, bx
    mov     word ptr es:[di].fMode, fmOutPut
    int     21h
    jc      @@Done
    xor     ax, ax
@@Done:
  end;

  function DOSFileOpenPrim(var T : TextRec) : Word;
    {-ôffnet eine Datei vom Typ TEXT mit MSDOS-Funktionen}
  begin
    DOSFileOpenPrim := 0;
    FileMode := FileMode and $03;                                      {!!.07}
    with Regs do
      case T.Mode of
        {-Reset oder Append}
        fmInput, fmInOut :
          {-Anmerkung: Filemode wurde von FILEOPEN gesetzt}
          if Int21file($3D00+FileMode, T) then
            T.Handle := AX
          else
            DOSFileOpenPrim := AX;
        {-Rewrite}
        fmOutPut :
          begin
            Regs.CX := 0;
            if Int21file($3C00+FileMode, T) then
              T.Handle := AX
            else
              DOSFileOpenPrim := AX;
          end;
      end;
  end;

  function TEXTOPEN(var T : TextRec) : Word;
    {-Ersatz fÅr System.TextOpen}
  var
    Action : Word;
    I : Integer;
    AppendFile : Boolean;
    SaveFileMode : Word;   {!!.03}                                     {!!.07}
  begin
    AppendFile := False;
    SaveFileMode := FileMode;                                          {!!.03}

    with T do begin
      {!!.05 begin}
      if Name[0] = #0 then begin                                 
        {-FÅr den Fall: Assign(TEXT, '');}
        case Mode of
          fmInput :
            begin
              InOutFunc := @TEXTREAD;
              TEXTOPEN := 0;
            end;
          fmOutPut, fmInOut :
            begin
              InOutFunc := @TEXTWRITEDEV;
              FlushFunc := @TEXTWRITEDEV;
              TEXTOPEN := OpenIOCTL(T);
            end;
        end;
        FileMode := SaveFileMode;
        Exit;
      end;
      {!!.05 end}

      case Mode of
        fmInput :                 {-Reset}
          begin
            {FileMode:= 0;}
            FileMode := FileMode and $FFFC;                            {!!.07}
            {$IFDEF UseTextSharingFlags}
            FileMode := FileMode and $FF8F or fmDenyWrite;             {!!.07}
            {$ENDIF}
            Action := $01;
            InOutFunc := @TEXTREAD;
          end;

        fmOutPut :                {-Rewrite}
          begin
            {-Eine bereits bestehende Datei MUSS zuerst gelîscht werden}
            Erase(Text(T));
            I := IoResult;
            case I of
              {-Diese Fehler werden ignoriert}
              0 : {-File gelîscht} ;
              2 : {-File nicht gefunden} ;
              5 : {-Zugriff verweigert} ;                              {!!.05}
            else
              {-Fehler wieder herstellen}
              TEXTOPEN := I;
              FileMode := SaveFileMode;                                {!!.03}
              Exit;
            end;

            {FileMode:= 1;}
            FileMode := FileMode and $FFFC or 1;                       {!!.07}
            {$IFDEF UseTextSharingFlags}
            FileMode := FileMode and $FF8F or fmDenyRead;              {!!.07}
            {$ENDIF}
            Action := $10;
            InOutFunc := @TEXTWRITE;
          end;

        fmInOut :                 {-Append}
          begin
            AppendFile := True;
            {FileMode:= 1;}
            FileMode := FileMode and $FFFC or 1;                       {!!.07}
            {$IFDEF UseTextSharingFlags}
            FileMode := FileMode and $FF8F or fmDenyRead;              {!!.07}
            {$ENDIF}
            Action := $01;
            InOutFunc := @TEXTWRITE;
          end;
      end;

      {-Zugriff auf die Datei}
      if not Win95FileOpenPrim(T, BufSize, Action, True) then begin
        {-Funktion nicht verfÅgbar, Handling von TEXT deaktivieren}
        DisableLFNFunc(pAssignText);
        {-Datei mit MSDOS-Funktionen îffnen}
        InOutRes := DOSFileOpenPrim(T);
      end;

      if AppendFile and (InOutRes = 0) then
        {-Seek zum Ende der Datei}
        with Regs do begin
          AX := $4202;
          BX := Handle;
          CX := 0;
          DX := 0;
          Flags := 0;                                                  {!!.07}
          MsDos(Regs);
          if Flags and FCarry <> 0 then
            InOutRes := AX;
          Mode := fmOutPut;
        end;
    end;

    FileMode := SaveFileMode;                                          {!!.03}
    TEXTOPEN := InOutRes;
  end;

  function LFNAssignTestName(var P : PChar; var S : String) : Word; Assembler; {!!.07}
    {-PrÅft ob der Dateiname als String oder AsciiZ vorliegt}
  asm
    {-Wenn DX <> 0 dann wurde ein PChar Åbergeben}
    or      dx, dx
    jne     @@IsPChar
    {-Ein String befindet sich in P, nach S als PChar kopieren}
    push    ds
    lds     si, P
    lds     si, ds:[si]
    les     di, S
    cld
    lodsb
    xor     ah, ah
    mov     cx, ax
    mov     dx, ax
    rep     movsb
    xor     al, al
    stosb
    {-Adresse von P auf S Ñndern}
    lds     si, S
    les     di, P
    mov     ax, si
    stosw
    mov     ax, ds
    stosw
    {-DS, CX wieder herstellen}
    pop     ds
    mov     cx, dx
    inc     cx
    jmp     @@StoreLen
@@IsPChar:
    {-LÑnge von P ermitteln}
    les     di, P
    les     di, es:[di]
    cld
    xor     al, al
    mov     cx, 0FFFFh
    repne   scasb
    not     cx
@@StoreLen:
    mov     ax, cx
  end;

  procedure LFNAssignStoreName(var F; var P : PChar; PSize : Word);    {!!.07}
    {-Speichert den Dateinamen in der Dateivariablen}
  var
    Temp : PChar;
  begin
    with FileRec(F) do begin
      {$IFDEF AssignLongName}
      {-Muss der Dateiname gekÅrzt werden?}
      if PSize > SizeOf(Name) then begin
        {-Benutzerdefinierte Funktion aufrufen}
        AssignNew(F, P);
        {-Grî·e des geÑnderten Dateinamens neu ermitteln}
        PSize := AscSize(P^);
      end;
      {$ENDIF}

      {-Ist der Dateiname (immer noch) zu lang?}
      if PSize > SizeOf(Name) then begin
        {-öbertragung auf das Maximum begrenzen}
        PSize := SizeOf(Name)-1;
        Name[79] := #0;
        {-Fehler zurÅckgeben}
        InOutRes := 201;          {-Range check error}
      end;

      {-Den Dateinamen in die Dateivariable Åbertragen}
      Move(P^, Name, PSize);

      {$IFDEF AssignLongName}
      {-Den Åbergebenen Dateinamen ggf. restaurieren}
      Temp := AssignGetName(F);
      if Temp <> nil then
        Move(Temp^, P^, AscSize(Temp^));
      {$ENDIF}
    end;
  end;

  procedure AssignText(var T; P : PChar);                              {!!.05}
    {-Ersatz NUR fÅr Assign mit Dateien vom Typ TEXT}
  var
    PSize : Word;
    PString : String;
  begin
    PSize := LFNAssignTestName(P, PString);                            {!!.07}

    {-Felder der Dateivariablen initialisieren}
    with TextRec(T) do begin
      Handle := 0;
      Mode := fmClosed;
      BufSize := SizeOf(TextBuf);
      private := 0;
      BufPos := 0;
      BufEnd := 0;
      BufPtr := @Buffer;
      OpenFunc := @TEXTOPEN;
      InOutFunc := nil;
      FlushFunc := nil;
      CloseFunc := @TEXTCLOSE;
      FillChar(UserData, SizeOf(UserData), 0);
    end;

    LFNAssignStoreName(T, P, PSize);                                   {!!.07}
  end;

  procedure AssignFile(var F; P : PChar);                              {!!.05}
    {-Ersatz NUR fÅr Assign mit Dateien vom Typ FILE}
  var
    PSize : Word;
    PString : String;
  begin
    PSize := LFNAssignTestName(P, PString);                            {!!.07}

    with FileRec(F) do begin
      Handle := 0;
      Mode := fmClosed;
      RecSize := 0;
      FillChar(UserData, SizeOf(UserData), 0);
    end;

    LFNAssignStoreName(F, P, PSize);                                   {!!.07}
  end;

  procedure FILECLOSE(var F); Assembler;                               {!!.05}
    {-ErfÅllt dieselbe Funktion wie SYSTEM.FILECLOSE}
  asm
    les     di, F
    call    @@OpenCheck
    jne     @@Done
    mov     bx, es:[di].fHandle
    cmp     bx, 4
    jbe     @@1
    mov     ah, 3Eh
    int     21h
    jnc     @@1
    mov     InOutRes, ax
@@1:
    mov     word ptr es:[di].fMode, fmClosed
    jmp     @@Done

@@OpenCheck:
    cmp     word ptr es:[di].fMode, fmInOut
    je      @@IsOpen
    mov     InOutRes, 103
@@IsOpen:
    retn

@@Done:
{$IFDEF AssignLongName}
  {$IFDEF AutoAssignDispose}
    push    es
    push    di
    call    AssignDispose
  {$ENDIF}
{$ENDIF}
  end;

{$IFDEF AssignLongName}
  procedure LFNAssignNew(var F; N : PChar);            {!!.07 various changes}
    {-Legt einen Pointer auf dem Heap an, speichert ihn in UserData}
  const
    {-Wird an ASSIGN ein Dateiname lÑnger 79 Zeichen Åbergeben, so wird der
      Pfad entfernt und dafÅr dieser String eingesetzt. Das erste Zeichen wird
      durch den Laufwerksbezeichner ersetzt.}
    AssignDump : array[1..7] of Char = '#:\...\';
  type
    CharArrayPtr = ^CharArray;
    CharArray = array[0..1] of Char;
  var
    P, FName : PChar;
    NLen, FLen : Word;
  begin
    NLen:= AscSize(N^);
    with FileRec(F) do
      if MaxAvail < NLen then
        {-Kein Platz mehr auf dem Heap}
        {RunError(203)}                                                {!!.06}
        InOutRes := 203                                                {!!.06}
      else begin
        {-Speicher belegen}
        GetMem(P, NLen);

        {-Dateinamen auf den Heap kopieren}
        asm
          push    ds
          lds     si, N
          les     di, P
          cld
          mov     cx, NLen
          rep     movsb
          pop     ds
        end;

        {-Den Pointer in UserData speichern}
        Move(P, UserData, SizeOf(Pointer));

        {-Laufwerksbezeichner einsetzen}
        if CharArrayPtr(N)^[1] = ':' then
          AssignDump[1] := Upcase(N^)
        else
          AssignDump[1] := DefaultDrive;
        StrMove(N, @AssignDump, SizeOf(AssignDump));
        Inc(N, SizeOf(AssignDump));

        {-Den Anfang des Dateinamens suchen}
        FName := StrRScan(P, '\');
        if FName = nil then begin
          FName := StrRScan(P, ':');
          if FName = nil then
            FName := P;
        end
        else
          Inc(FName);
        FLen := StrLen(FName);

        {-LÑnge prÅfen und Dateinamen einsetzen}
        if FLen > 79-SizeOf(AssignDump) then
          FLen := 79-SizeOf(AssignDump);
        StrMove(N, FName, FLen);

        {-Sicherstellen das er mit #0 endet}
        Inc(N, FLen);
        N^ := #0;
      end;
  end;

  function LFNAssignGetName(var F) : Pointer; Assembler;               {!!.05}
    {-Liefert den Pointer in UserData zurÅck}
  asm
    les     di, F
    mov     ax, es:[di].fUserData
    mov     dx, es:[di].fUserData.2
  end;

  procedure LFNAssignDispose(var F);                                   {!!.05}
    {-Gibt den durch AssignNew belegten Heap wieder frei.}
  var
    P : Pointer;
  begin
    {-Pointer aus UserData lesen}
    P := AssignGetName(F);
    {-Ist dort ein Pointer gespeichert?}
    if P <> nil then begin
      {-Dateiname freigeben}
      FreeMem(P, AscSize(P^));
      {-Den Pointer in UserData lîschen}
      P := nil;
      Move(P, FileRec(F).UserData, SizeOf(Pointer));
    end;
  end;
{$ENDIF}

  {-Ersatzfunktionen fÅr Unit Dos:
      FExpand, FindFirst, FindNext, FindClose, FSplit, DiskFree, DiskSize
      FSearch, GetEnv}
  {--------------------------------------------------------------------}
  {-                          LFN.IN4 v1.10                           -}
  {-         Ersatzfunktionen fÅr Unit Dos fÅr LFN.PAS v1.10          -}
  {--------------------------------------------------------------------}

{$IFDEF UseDos}
  function FExpand(Path : PathStr) : PathStr;
   {-Erweitert einen unvollstÑndig angegebenen Dateinamen um den
     dazugehîrigen Suchpfad.}
  var
    PName : String;
  begin
    if not FlagIsSet(LFN_Proc, bFExpand) then begin
      FExpand := Dos.FExpand(Path);
      Exit;
    end;

    PName := JustPathName(Path);
    if PName = '' then begin
      {-Nur ein Dateiname wurde angegeben}
      GetDir(0, PName);
      if Regs.AX = $7100 then begin
        {-Function nicht verfÅgbar, deaktivieren}
        ClearFlag(LFN_Proc, bFExpand);
        {-Original aufrufen}
        FExpand := Dos.FExpand(Path);
        Exit;
      end;
      FExpand := AddBackSlash(PName)+Path;
    end
    else begin
      {-Den korrekten Pfad ermittlen}
      PName := TruePathName(PName);
      if Regs.AX = $7100 then begin
        {-Function nicht verfÅgbar, deaktivieren}
        ClearFlag(LFN_Proc, bFExpand);
        {-Original aufrufen}
        FExpand := Dos.FExpand(Path);
        Exit;
      end;
      FExpand := AddBackSlash(PName)+JustName(Path);
    end;
  end;
{$ENDIF}

type
  LongSearchRec =
    record
      Attr : LongInt;             {-Dateiattribute}
      CreationTime,               {-Datum/Zeit der Erstellung}
      AccessTime,                 {-Datum/Zeit des letzen Zugriffs}
      ModifyTime,                 {-Datum/Zeit der letzen VerÑnderung}
      Size : Comp;                {-Grî·e der Datei}
      Reserved : array[0..7] of Char;
      LongName : array[0..259] of Char; {-ASCIIZ des langen Dateinamens}
      ShortName : array[1..14] of Char; {-ASCIIZ des kurzen Dateinamens}
    end;

  procedure ConvertSearchRec(L : LongSearchRec; var S : SearchRec);
    {-Kopiert die benîtigten Felder des LongSearchRec nach SearchRec}
  type
    CompRec =
      record
        LowC, HighC : LongInt;
      end;
  var
    I : Word;
  begin
    S.Attr := L.Attr;
    S.Time := CompRec(L.ModifyTime).LowC;
    S.Size := CompRec(L.Size).HighC;
    {$IFDEF Windows}
    OemToAnsi(L.LongName, L.LongName);                                 {!!.06}
    {$ENDIF}
    {$IFDEF CompatToDos}
    S.Name := Asc2Str(L.LongName);
    {$ELSE}
    Move(L.LongName, S.Name, AscSize(L.LongName)+1);
    {$ENDIF}
  end;

{$IFDEF CompatToDos}
  procedure FindFirst(Path : String; Attr : Word; var F : SearchRec);
{$ELSE}
  procedure FindFirst(Path : PChar; Attr : Word; var F : SearchRec);
{$ENDIF}
    {-Sucht ein Verzeichnis nach dem ersten Vorkommen eines Dateinamens ab.}
  var
    LSR : LongSearchRec;
{$IFDEF CompatToDos}
    DSR : Dos.SearchRec absolute F;
{$ELSE}
    DSR : WinDos.TSearchRec absolute F;                                {!!.06}
{$ENDIF}
  begin
    {DosError := 0;}                                                   {!!.04}
    if not FlagIsSet(LFN_Proc, bFindFirstNext) then begin
      {-Funktion nicht aktiv, Original aufrufen}
    {$IFDEF CompatToDos}
      Dos.FindFirst(Path, Attr, DSR);
    {$ELSE}
      WinDos.FindFirst(Path, Attr, DSR);                               {!!.06}
    {$ENDIF}
      Exit;
    end;

    DosError := 0;                                                     {!!.04}

  {$IFDEF CompatToDos}
    {-String zu Verwendung als ASCIIZ erweitern}
    AddZero(Path);
  {$ENDIF}
  {$IFDEF Windows}
    AnsiToOem(Path, F.Name);                                           {!!.06}
  {$ENDIF}
    with Regs do begin
      AX := $714E;
      CL := Attr;
      {-CH = required-attributes mask}
      CH := 0;
{---
   SI = date/time format
     0000h  use 64-bit file time format
     0001h  use MS-DOS date/time values (see #0971,#0972) in low
            double-word of file time QWORD (date is high word, time is low
            word of double-word)
---}
      SI := 1;
    {$IFDEF CompatToDos}
      DS := Seg(Path);
      DX := Ofs(Path[1]);
    {$ELSE}
     {!!.06 begin}
     {$IFDEF Windows}
      DS := Seg(F.Name);
      DX := Ofs(F.Name);
     {$ELSE}
      DS := Seg(Path^);
      DX := Ofs(Path^);
     {$ENDIF}
     {!!.06 end}
    {$ENDIF}
      ES := Seg(LSR);
      DI := Ofs(LSR);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bFindFirstNext);
          {-Das Original aufrufen}
        {$IFDEF CompatToDos}
          Dos.FindFirst(Path, Attr, DSR);
        {$ELSE}
          WinDos.FindFirst(Path, Attr, DSR);                           {!!.06}
        {$ENDIF}
        end
        else
          {-Fehlercode setzen}
          DosError := AX
      else begin
        {-FileHandle fÅr diesen Suchvorgang merken}
        F.Handle := AX;
        {-Den LongSearchRec in den SearchRec umwandeln}
        ConvertSearchRec(LSR, F);
      end;
    end;
  end;

  procedure FindNext(var F : SearchRec);
    {-Setzt eine mit FindFirst begonnene Suche nach der dort angegebenen
      Datei fort.}
  var
    LSR : LongSearchRec;
  {$IFDEF CompatToDos}
    DSR : Dos.SearchRec absolute F;
  {$ELSE}
    DSR : WinDos.TSearchRec absolute F;                                {!!.06}
  {$ENDIF}
  begin
    {DosError := 0;}                                                   {!!.04}
    if not FlagIsSet(LFN_Proc, bFindFirstNext) then begin
      {-Funktion nicht aktiv, Orginal aufrufen}
    {$IFDEF CompatToDos}
      Dos.FindNext(DSR);
    {$ELSE}
      WinDos.FindNext(DSR);                                            {!!.06}
    {$ENDIF}
      Exit;
    end;

    DosError := 0;                                                     {!!.04}

    with Regs do begin
      AX := $714F;
      BX := F.Handle;
      {-SI = date/time format (siehe FindFirst)}
      SI := 1;
      ES := Seg(LSR);
      DI := Ofs(LSR);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bFindFirstNext);
          {-Das Original aufrufen}
        {$IFDEF CompatToDos}
          Dos.FindNext(DSR);
        {$ELSE}
          WinDos.FindNext(DSR);                                        {!!.06}
        {$ENDIF}
        end
        else begin
          {-Fehlercode setzen}
          DosError := AX;
          {-Keine weiteren Dateien gefunden?}
          if DosError = 18 then
            {-Filehandle wieder freigeben}
            FindClose(F);
        end
      else
        {-Den LongSearchRec in den SearchRec umwandeln}
        ConvertSearchRec(LSR, F);
    end;
  end;

  procedure FindClose(var F : SearchRec);
    {-Beendet eine mit FindFirst begonnene Suche. (FindClose wird von FindNext
      automatisch aufgerufen, wenn keine weitere Datei mehr gefunden wurde)}
  begin
    if not FlagIsSet(LFN_Proc, bFindFirstNext) then
      {-Funktion nicht aktiv}
      Exit;

    if F.Handle < 5 then
      {-Entweder wurde das Filehandle schon freigegeben, oder aber es
        handelt sich um ein Standardhandle von TP}
      Exit;

    {-Das Filehandle freigeben}
    with Regs do begin
      AX := $71A1;
      BX := F.Handle;
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      if Flags and FCarry = 0 then
        {-Als freigeben setzen}
        F.Handle := 0
      else
        if AX = $7100 then
          {-Funktion nicht verfÅgaber, deaktivieren}
          ClearFlag(LFN_Proc, bFindFirstNext);
    end;
  end;

{$IFDEF UseDos}
  procedure FSplit(Path : String; var Dir : DirStr; var Name : NameStr;
                   var Ext : ExtStr);
    {-Zerlegt einen vollstÑndigen Dateinamen in seine drei Komponenten.}
  var
    I, J : Word;
    dDir : Dos.DirStr absolute Dir;
    dName : Dos.NameStr absolute Name;
    dExt : Dos.ExtStr absolute Ext;
  begin
    if not FlagIsSet(LFN_Proc, bFSplit) then begin
      Dos.FSplit(Path, dDir, dName, dExt);
      Exit;
    end;

    J := Length(Path);
    Dir := '';
    Name := '';
    Ext := '';
    for I := J downto 1 do begin
      case Path[I] of
        '.' :                     {-Extension}
          if Ext = '' then begin                                       {!!.06}
            Ext := Copy(Path, I, J-I+1);
            J := I-1;
          end;
        '\', ':' :                {-Ende des Pfades}
          begin
            Name := Copy(Path, I+1, J-I);
            Dir := Copy(Path, 1, I);
            Exit;
          end;
      end;
    end;
    if J > 0 then
      Name := Copy(Path, I-1, J-I+1);
  end;
{$ENDIF}

type
  {-Format of extended free space structure:}
  ExtendedDisk =
    record
      ReturnedSize,               {- ret: size of returned structure}
      StructureVersion : Word;    {-call: structure version (0h),
                                     ret: actual structure version}
      {-with adjustment for compression:}
      SectorPerCluster,           {-number of sectors per cluster}
      BytesPerSector,             {-number of bytes per sector}
      Avaible,                    {-number of available clusters}
      Total,                      {-total number of clusters}
      {-without adjustment for compression:}
      PhysicalAvaible,            {-number of physical sectors available}
      PhysicalTotal,              {-total number of physical sectors}
      UnitsAvailble,              {-number of available allocation units}
      UnitsTotal : LongInt;       {-total allocation units}
      Reserved : array[0..127] of Byte;
    end;

  function Call7303(Drive : Byte; var ED : ExtendedDisk) : Boolean;
{---
  Anmerkung: Diese Funktion liefert im Programm unter Windows/DOS7.x
  einwandfreie Ergebnisse. Wenn man allerdings das Prg in der IDE (Turbo oder
  BP) entwickelt, dann liefern sie bei mir nur nach dem ersten Run das
  korrekte Ergebnis. Warum entzieht sich meiner Kenntnis.
---}
  const
    DriveName : String[4] = 'C:\'#0;
  begin
    if Drive = 0 then
      DriveName[1] := DefaultDrive
    else
      DriveName[1] := Char(Drive+64);
    ED.ReturnedSize := 0;
    ED.StructureVersion := 0;
    with Regs do begin
      AX := $7303;
      DS := Seg(DriveName);
      DX := Ofs(DriveName[1]);
      ES := Seg(ED);
      DI := Ofs(ED);
      CX := SizeOf(ED);
      Flags := 0;                                                      {!!.07}
      MsDos(Regs);
      Call7303 := (Flags and FCarry = 0) and
                  (ED.ReturnedSize >= SizeOf(ED)-SizeOf(ED.Reserved));
    end;
  end;

  function DiskFree(Drive : Byte) :
    {$IFDEF DFSReturnComp} Comp ; {$ELSE} LongInt; {$ENDIF}
    {-Liefert die Grî·e des freien Speicherplatzes auf einem Laufwerk zurÅck.}
  var
    ED : ExtendedDisk;
    Size : Comp;
  begin
    if not Call7303(Drive, ED) then
    {$IFDEF UseDos}
      DiskFree := Dos.DiskFree(Drive) {$IFDEF DFSReturnKb} div 1024 {$ENDIF}
    {$ELSE}
      DiskFree :=
        WinDos.DiskFree(Drive) {$IFDEF DFSReturnKb} div 1024 {$ENDIF}  {!!.06}
    {$ENDIF}
    else begin
      Size := 1;
      with ED do
        Size := Size*Avaible*SectorPerCluster*BytesPerSector
                {$IFDEF DFSReturnKb} / 1024 {$ENDIF} ;
    {$IFDEF DFSReturnComp}
      DiskFree:= Size;
    {$ELSE}
      if Size > MaxLongInt then
        DiskFree := MaxLongInt
      else
        DiskFree := Trunc(Size);
    {$ENDIF}
    end;
  end;

  function DiskSize(Drive : Byte) :
    {$IFDEF DFSReturnComp} Comp ; {$ELSE} LongInt; {$ENDIF}
    {-Liefert die GesamtkapazitÑt eines Laufwerks zurÅck.}
  var
    ED : ExtendedDisk;
    Size : Comp;
  begin
    if not Call7303(Drive, ED) then
    {$IFDEF UseDos}
      DiskSize := Dos.DiskSize(Drive) {$IFDEF DFSReturnKb} div 1024 {$ENDIF}
    {$ELSE}
      DiskSize :=
        WinDos.DiskSize(Drive) {$IFDEF DFSReturnKb} div 1024 {$ENDIF}  {!!.06}
    {$ENDIF}
    else begin
      Size := 1;
      with ED do
        Size := Size*Total*SectorPerCluster*BytesPerSector
                {$IFDEF DFSReturnKb} / 1024 {$ENDIF} ;
    {$IFDEF DFSReturnComp}
      DiskSize:= Size;
    {$ELSE}
      if Size > MaxLongInt then
        DiskSize := MaxLongInt
      else
        DiskSize := Trunc(Size);
    {$ENDIF}
    end;
  end;

  function FileSearchPrim(Dest : PChar; DestLen : Word;
                          Name, List : PChar) : Boolean; Assembler;    {!!.08}
    {-Sucht eine Liste von Verzeichnissen nach einem Dateieintrag ab.
      Das Ergebnis wird in Dest gespeichert. Ist Dest^ = #0 dann wurde die
      Datei nicht gefunden. Gibt immer True zurÅck, ausser Win9x unterstÅtzt
      die Funktion nicht.}
  asm
    push    ds
    cld
    lds     si, List
    les     di, Dest
    mov     cx, DestLen
@@1:
    push    ds
    push    si
    jcxz    @@3
    lds     si, Name
@@2:
    lodsb
    or      al, al
    je      @@3
    stosb
    loop    @@2
@@3:
    xor     al, al
    stosb
    les     di, Dest
    mov     ax, 7143h

    push    ds
    lds     dx, Dest
    xor     bx, bx
    int     21h
    pop     ds

    pop     si
    pop     ds
    jc      @@4
    test    cx, 18h
    je      @@9
@@4:
    cmp     ax, 7100h
    jne     @@4a
    mov     al, False
    jmp     @@Done
@@4a:
    cmp     ax, 7143h
    jne     @@4b
    mov     al, False
    jmp     @@Done
@@4b:

    les     di, Dest
    mov     cx, DestLen
    xor     ah, ah
    lodsb
    or      al, al
    je      @@8
@@5:
    cmp     al, ';'
    je      @@7
    jcxz    @@6
    mov     ah, al
    stosb
    dec     cx
@@6:
    lodsb
    or      al, al
    jne     @@5
    dec     si
@@7:
    jcxz    @@1
    cmp     ah, ':'
    je      @@1
    mov     al, '\'
    cmp     al, ah
    je      @@1
    stosb
    dec     cx
    jmp     @@1
@@8:
    stosb
@@9:
    mov     al, True
@@Done:
    pop     ds
  end;

{$IFDEF UseDos}                                                        {!!.09}
  function Win95FSearch(Path : PathStr; DirList : String) : PathStr;   {!!.08}
    {-Sucht eine Liste von Verzeichnissen nach einem Dateieintrag ab.}
  var
    ResultPtr : Pointer;
  begin
    {-Zeiger auf Win95FSearch holen}
    asm
      les     di, @Result
      mov     ResultPtr.word[0], di
      mov     ResultPtr.word[2], es
    end;
    {-Die Parameter zu AsciiZ's umwandeln}
    Str2Asc(Path, Path);
    Str2Asc(DirList, DirList);
    {-Die Datei suchen}
    if not FileSearchPrim(ResultPtr, 255, @Path, @DirList) then begin
      {-Funktion wird nicht unterstÅzt, deaktivieren}
      DisableLFNFunc(pFSearch);
      {-Original aufrufen}
      POPJumpFAR(@PatchAddr[pFSearch]);
    end
    else
      {-Das Ergebnis in einen String umwandeln}
      Win95FSearch := Asc2Str(ResultPtr^);
  end;

  function Win95GetEnv(EnvVar : String) : String;                      {!!.09}
    {-Liest einen Eintrag aus der Environment-Tabelle}
  var
    I, P : Integer;
    S : String;
  begin
    EnvVar := StUpcase(EnvVar);
    for I := 1 to EnvCount do begin
      S := EnvStr(I);
      P := Pos('=', S);
      if (P > 0) and (StUpcase(Copy(S, 1, P-1)) = EnvVar) then begin
        Win95GetEnv := Copy(S, P+1, 255);
        Exit;
      end;
    end;
    Win95GetEnv := '';
  end;
{$ENDIF}                                                               {!!.09}

  {-Erweiterte Funktionen:
      TruePathName, GetShortName, GetLongName, FlushDrive, ResetDrive,
      GetShortNamePChar, GetLongNamePChar, TruePathNamePChar,
      FlushDrive, ResetDrive, IsNetworkDrive}
  {--------------------------------------------------------------------}
  {-                          LFN.IN5 v1.10                           -}
  {-             Erweiterte Funktionen fÅr LFN.PAS v1.10              -}
  {--------------------------------------------------------------------}

{$IFDEF UseDos}
  function GetShortNameDOS(PathName : String) : String; {!!.06 various changes}
    {-Wandelt einen langen Pfadnamen in das 8.3 Format ohne Win95}

    function RemoveBlanks(S : String; MaxLen : Word) : String; Assembler;
      {-Entfernt die Leerstellen aus S und kÅrzt ihn auf MaxLen}
    asm
      push    ds
      les     di, @Result
      push    di
      inc     di
      lds     si, S
      cld
      lodsb
      xor     ah, ah
      mov     cx, ax
      xor     bx, bx
      jcxz    @@Done                                                   {!!.10}
@@Next:
      lodsb
      cmp     al, ' '
      je      @@IsBlank
      stosb
      inc     bx
@@IsBlank:
      dec     cx
      jcxz    @@Done
      cmp     bx, MaxLen
      jl      @@Next
@@Done:
      pop     di
      mov     ax, bx
      stosb
      pop     ds
    end;

    function GetSubDIR(S : String; var From : Word) : String;
      {-Extrahiert einen Teilabschnitt eines Pfadnamens}
    var
      I : Word;
    begin
      {-Momentane Position merken}
      I := From;
      {-Nach dem nÑchsten "\" suchen}
      while (S[From] <> '\') and (From <= Length(S)) do
        Inc(From);
      {-Teilstring extrahieren}
      GetSubDIR := Copy(S, I, From-I);
      {-Position weitersetzen fÅr den nÑchsten Durchlauf}
      Inc(From);
    end;

    procedure Fake(var S : String); Assembler;
      {-KÅrzt einen Dateinamen auf 8 Zeichen und fÅgt eine Tilde an}
    asm
      les     di, S
      cld
      mov     al, 8
      stosb
      add     di, 6
      mov     al, '~'
      stosb
      mov     al, '?'
      stosb
    end;

  var
    Work, Path : String;
    Dir : DirStr;
    Name : NameStr;
    Ext : ExtStr;
    I : Word;
    Sr : Dos.SearchRec;
  begin
    GetShortNameDOS := '';

    {-Namen splitten}
    LFN.FSplit(PathName, Dir, Name, Ext);

    {-Die Leerstellen aus dem Extension entfernen}
    Ext := RemoveBlanks(Ext, 4);

    {-Die Leerstellen aus dem Namen entfernen}
    Name := RemoveBlanks(Name, 9);
    {-Den Dateinamen ggf. kÅrzen}
    if Length(Name) > 8 then
      Fake(Name);

    {-Wurde ein Laufwerksbezeichner angegeben?}
    I := Pos(':', Dir);
    if I = 0 then begin
      {-Kein Laufwerksbezeichner}
      Work := FExpand('');
      if (Dir[1] <> '\') or (Length(Dir) = 0) then
        {-Momentanen Pfad einfÅgen}
        Insert(Work, Dir, 1)
      else begin
        {-Nur Laufwerksbezeichner einfÅgen}
        Delete(Work, 3, 255);
        Insert(Work, Dir, 1);
      end;
    end
    else begin
      {-Wurde ein Pfad ab dem Root des Laufwerkes angegeben?}
      I := Pos('\', Dir);
      if (I = 0) or (I > 3) then begin
        {-Pfad dieses Laufwerkes ermitteln}
        GetDir(Byte(Upcase(Dir[1]))-Ord('A')+1, Work);
        {-Ggf. einen BackSlash anhÑngen}
        Work := AddBackSlash(Work);
        {-Pfad einfÅgen}
        Delete(Dir, 1, 2);
        Insert(Work, Dir, 1);
      end;
    end;

    {-Die Verzeichnisnamen kÅrzen}
    I := 1;
    Path := '';
    while I <= Length(Dir) do begin
      {-Einen Teilnamen extrahieren}
      Work := GetSubDIR(Dir, I);
      {-Die Leerstellen aus dem Namen entfernen}
      Work := RemoveBlanks(Work, $FF);
      if Length(Work) > 8 then begin
        {-KÅrzen mit ? als Alias}
        Fake(Work);
        {-Danach suchen}
        Dos.FindFirst(Path+Work, AnyFile, Sr);
        if DosError <> 0 then begin
          {-"Pfad nicht gefunden" zurÅckgeben}
          DosError := 3;
          Exit;
        end;
        {-Den richtigen Namen zurÅckgeben}
        Work := Sr.Name
      end;
      {-Teilnamen anfÅgen}
      Path := Path+Work+'\';
    end;
    Dir := Path;

    {-Ermitteln ob es die Datei gibt}
    Dos.FindFirst(Dir+Name+Ext, AnyFile, Sr);
    if DosError = 0 then begin
      Name := Sr.Name;
      Ext := '';
    end
    else begin
      {-Ggf. "Datei nicht gefunden" zurÅckgeben}
      if DosError = 18 then
        DosError := 2;
      Exit;
    end;

    {-Unter DOS gibt es nur gro·geschriebene Dateinamen}
    GetShortNameDOS := StUpcase(Dir+Name+Ext);
  end;

  function TruePathNameDOS(Path : String) : String;
    {-Return the shortest Pathname for Path}
  var
    SaveDir : PathStr;
    fName : String;
  begin
    Path := GetShortNameDOS(Path);
    fName := JustName(Path);
    Path := JustPathName(Path);
    GetDir(0, SaveDir);
    if Path[Length(Path)] = '\' then
      Dec(Path[0]);
    ChDir(Path);
    if IoResult <> 0 then begin
      TruePathNameDOS := Path+'\'+fName;
      Exit;
    end;
    TruePathNameDOS := FExpand('')+fName;
    ChDir(SaveDir);
  end;

  function TruePathName(PathName : String) : String;
    {-Liefert den kÅrzesten Pfad zu einer Datei, oder erweitert den
      Dateinamen mit dem momentanen Verzeichnis}
  var
    TName : String;
  begin
    DosError := 0;
    if not FlagIsSet(LFN_Proc, bTruePathName) then begin
      TruePathName := TruePathNameDOS(PathName);
      Exit;
    end;

    with Regs do begin
      CX := 0;
      ES := Seg(TName);
      DI := Ofs(TName);
      if not Int21name($7160, PathName) then begin
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bTruePathName);
          TruePathName := TruePathNameDOS(PathName);
        end
        else begin
          DosError := AX;
          TruePathName := PathName;
        end;
      end
      else begin
        DosError := 0;
        TruePathName := Asc2Str(TName);
      end;
    end;
  end;

  function GetShortName(PathName : String) : String;
    {-Wandelt einen langen Pfadnamen in das 8.3 Format}
  var
    TName : String;
  begin
    DosError := 0;
    if not FlagIsSet(LFN_Proc, bGetShortName) then begin
      GetShortName := GetShortNameDOS(PathName);
      Exit;
    end;

    with Regs do begin
      CX := $0001;
      ES := Seg(TName);
      DI := Ofs(TName);
      if not Int21name($7160, PathName) then
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bGetShortName);
          GetShortName := GetShortNameDOS(PathName);
        end
        else begin
          DosError := AX;
          GetShortName := PathName;
        end
      else begin
        DosError := 0;
        GetShortName := Asc2Str(TName);
      end;
    end;
  end;

  function GetLongName(PathName : String) : String;
    {-Wandelt einen kurzen Pfadnamen (im 8.3 Format) in einen langen um}
  var
    TName : String;
  begin
    DosError := 0;
    if not FlagIsSet(LFN_Proc, bGetLongName) then begin
      GetLongName := GetShortNameDOS(PathName);
      Exit;
    end;

    with Regs do begin
      CX := $0002;
      ES := Seg(TName);
      DI := Ofs(TName);
      if not Int21name($7160, PathName) then begin
        if AX = $7100 then begin
          ClearFlag(LFN_Proc, bGetLongName);
          GetLongName := GetShortNameDOS(PathName);
        end
        else begin
          DosError := AX;
          GetLongName := PathName;
        end;
      end
      else begin
        DosError := 0;
        GetLongName := Asc2Str(TName);
      end;
    end;
  end;
{$ENDIF}

{$IFDEF UseWinDos}
  function Win95FileSplit(Path, Dir, Name, Ext : PChar) : Word; Forward;

  function GetShortNameDOSPChar(Dest, PathName : PChar) : PChar;       {!!.06}
    {-Wandelt einen langen Pfadnamen in das 8.3 Format ohne Win95}

    procedure RemoveBlanks(P : PChar; CurLen, MaxLen : Word); Assembler;
      {-Entfernt die Leerstellen aus P und kÅrzt ihn auf MaxLen}
    asm
      push    ds
      lds     si, P
      les     di, P
      mov     cx, CurLen
      xor     bx, bx
      cld
@@Next:
      lodsb
      cmp     al, ' '
      je      @@IsBlank
      stosb
      inc     bx
@@IsBlank:
      dec     cx
      jcxz    @@Done
      cmp     bx, MaxLen
      jl      @@Next
@@Done:
      xor     al, al
      stosb
      pop     ds
    end;

    procedure GetSubDIR(Dest : PChar; var FromP : PChar);
      {-Extrahiert einen Teilabschnitt eines Pfadnamens}
    var
      ToP : PChar;
    begin
      {-Nach dem nÑchsten "\" suchen}
      ToP := StrScan(FromP, '\');
      {-Teilstring extrahieren}
      if ToP <> nil then
        StrLCopy(Dest, FromP, ToP-FromP);
      {-Position weitersetzen fÅr den nÑchsten Durchlauf}
      FromP := ToP;
      if FromP <> nil then
        Inc(FromP);
    end;

    procedure Fake(P : PChar); Assembler;
      {-KÅrzt einen Dateinamen auf 8 Zeichen und fÅgt eine Tilde an}
    asm
      les     di, P
      add     di, 6
      mov     al, '~'
      stosb
      mov     al, '?'
      stosb
      xor     al, al
      stosb
    end;

  label
    ExitPoint;
  var
    Sr : WinDos.TSearchRec;
    Work, Temp, Save,
    Path, Dir, Name, Ext : PChar;
  begin
    GetShortNameDOSPChar := nil;
    if (Dest = nil) or (PathName = nil) then
      Exit;

    HeapFunc(On);
    GetMem(Work, PCharSize);
    GetMem(Path, PCharSize);
    GetMem(Dir, PCharSize);
    GetMem(Name, PCharSize);
    GetMem(Ext, PCharSize);
    HeapFunc(Off);
    {-Wenn Ext angelegt werden konnte, dann konnten auch alle anderen PChars
      angelegt werden, da alle gleich gro· sind. Trick 32,5 ;-)}
    if Ext = nil then
      goto ExitPoint;

    {-Namen splitten}
    Win95FileSplit(PathName, Dir, Name, Ext);

    {-Die Leerstellen aus dem Extension entfernen und auf 3 Stellen kÅrzen}
    RemoveBlanks(Ext, StrLen(Ext), 4);

    {-Die Leerstellen aus dem Namen entfernen}
    RemoveBlanks(Name, StrLen(Name), 9);
    {-Den Dateinamen ggf. kÅrzen}
    if StrLen(Name) > 8 then
      Fake(Name);

    {-Wurde ein Laufwerksbezeichner angegeben?}
    Temp := StrScan(Dir, ':');
    if Temp = nil then begin
      {-Kein Laufwerksbezeichner}
      Temp := Dir;
      FileExpand(Work, '');
      if Dir^ <> '\' then
        {-Momentanen Pfad einfÅgen}
        Inc(Temp, StrLen(Work))
      else
        {-Nur Laufwerksbezeichner einfÅgen}
        Inc(Temp, 2);
      StrMove(Temp, Dir, StrLen(Dir)+1);
      StrMove(Dir, Work, Temp-Dir);
    end
    else begin
      {-Wurde ein Pfad ab dem Root des Laufwerkes angegeben?}
      Temp := StrScan(Dir, '\');
      if (Temp = nil) or (Temp-Dir > 2) then begin
        {-Pfad dieses Laufwerkes ermitteln}
        GetCurDir(Work, Byte(Upcase(Dir^))-Ord('A')+1);
        {-Ggf. einen BackSlash anhÑngen}
        Temp := StrEnd(Work);
        if Temp^ <> '\' then
          StrCat(Work, '\');
        {-Pfad einfÅgen}
        Temp := Dir;
        Inc(Temp, StrLen(Work));
        Dec(Temp, 2);
        StrMove(Temp, Dir, StrLen(Dir)+1);
        StrMove(Dir, Work, Temp-Dir+2);
      end;
    end;

    {-Den Pfad StÅck fÅr StÅck testen}
    Path^ := #0;
    Temp := Dir;
    {-TeilstÅck extrahieren}
    GetSubDIR(Work, Temp);
    while Temp <> nil do begin
      {-Leerstellen entfernen und ggf. kÅrzen}
      RemoveBlanks(Work, StrLen(Work), $FF);
      {-Langer Pfadname?}
      if StrLen(Work) > 8 then begin
        {-In Tilde umwandlen und danach suchen}
        Fake(Work);
        Save := StrEnd(Path);
        StrCat(Path, Work);
        WinDos.FindFirst(Path, faAnyFile, Sr);
        if DosError <> 0 then
          goto ExitPoint;
        StrMove(Save, Sr.Name, 8);
      end
      else
        StrCat(Path, Work);
      {-Backslash anhÑngen und nÑchstes TeilstÅck extrahieren}
      StrCat(Path, '\');
      GetSubDIR(Work, Temp);
    end;

    {-Den kompletten Pfad zusammenkopieren}
    StrCopy(Work, Path);
    StrCat(Work, Name);
    StrCat(Work, Dir);

    {-Ermitteln ob es die Datei gibt}
    WinDos.FindFirst(Work, faAnyFile, Sr);
    if DosError = 0 then begin
      StrCopy(Name, Sr.Name);
      Ext^ := #0;
    end
    else begin
      {-Ggf. "Datei nicht gefunden" zurÅckgeben}
      if DosError = 18 then
        DosError := 2;
      goto ExitPoint;
    end;

    {-Das Ergebnis zusammenstellen}
    StrCopy(Dest, Path);
    StrCat(Dest, Name);
    StrCat(Dest, Ext);

    {-Unter DOS gibt es nur gro·geschriebene Dateinamen}
    GetShortNameDOSPChar := StrUpper(Dest);

ExitPoint:
    if Work <> nil then
      FreeMem(Work, PCharSize);
    if Path <> nil then
      FreeMem(Path, PCharSize);
    if Dir <> nil then
      FreeMem(Dir, PCharSize);
    if Name <> nil then
      FreeMem(Name, PCharSize);
    if Ext <> nil then
      FreeMem(Ext, PCharSize);
  end;

  function TruePathNameDOSPChar(Dest, Path : PChar) : PChar;           {!!.06}
    {-Return the shortest Pathname for Path}
  label
    ExitPoint;
  var
    SaveDir, Dir, Name, Ext, Temp : PChar;
  begin
    if GetShortNameDOSPChar(Dest, Path) = nil then begin
      TruePathNameDOSPChar := nil;
      Exit;
    end;

    HeapFunc(On);
    GetMem(SaveDir, PCharSize);
    GetMem(Dir, PCharSize);
    GetMem(Name, PCharSize);
    GetMem(Ext, PCharSize);
    HeapFunc(Off);
    {-Wenn Ext angelegt werden konnte, dann konnten auch alle anderen PChars
      angelegt werden, da alle gleich gro· sind. Trick 32,5 ;-)}
    if Ext = nil then
      goto ExitPoint;

    FileSplit(Dest, Dir, Name, Ext);
    Temp := StrEnd(Dir);
    if Temp^ = '\' then
      Temp^ := #0;

    GetCurDir(SaveDir, 0);
    SetCurDir(Dir);
    if IoResult <> 0 then
      TruePathNameDOSPChar := StrCopy(Dest, Path)
    else
      TruePathNameDOSPChar := FileExpand(Dest, StrCat(Name, Ext));
    SetCurDir(SaveDir);

ExitPoint:
    if SaveDir <> nil then
      FreeMem(SaveDir, PCharSize);
    if Dir <> nil then
      FreeMem(Dir, PCharSize);
    if Name <> nil then
      FreeMem(Name, PCharSize);
    if Ext <> nil then
      FreeMem(Ext, PCharSize);
  end;

  function GetShortNamePChar(Dest, PathName : PChar) : PChar;          {!!.06}
    {-Wandelt einen langen Pfadnamen in das 8.3 Format}
  begin
    GetShortNamePChar := nil;
    if (Dest = nil) or (PathName = nil) then
      Exit;

    DosError := 0;
    if not FlagIsSet(LFN_Proc, bGetShortName) then begin
      GetShortNamePChar := GetShortNameDOSPChar(Dest, PathName);
      Exit;
    end;

    with Regs do begin
      CX := $0001;
      ES := Seg(Dest^);
      DI := Ofs(Dest^);
      if not Int21namePChar($7160, PathName) then
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bGetShortName);
          GetShortNamePChar := GetShortNameDOSPChar(Dest, PathName);
        end
        else begin
          DosError := AX;
          GetShortNamePChar := nil;
        end
      else begin
        DosError := 0;
        GetShortNamePChar := Dest;
      end;
    end;
  end;

  function GetLongNamePChar(Dest, PathName : PChar) : PChar;           {!!.06}
    {-Wandelt einen kurzen Pfadnamen (im 8.3 Format) in einen langen um}
  begin
    GetLongNamePChar := nil;
    if (Dest = nil) or (PathName = nil) then
      Exit;

    DosError := 0;
    if not FlagIsSet(LFN_Proc, bGetLongName) then begin
      GetLongNamePChar := GetShortNameDOSPChar(Dest, PathName);
      Exit;
    end;

    with Regs do begin
      CX := $0002;
      ES := Seg(Dest^);
      DI := Ofs(Dest^);
      if not Int21namePChar($7160, PathName) then begin
        if AX = $7100 then begin
          ClearFlag(LFN_Proc, bGetLongName);
          GetLongNamePChar := GetShortNameDOSPChar(Dest, PathName);
        end
        else begin
          DosError := AX;
          GetLongNamePChar := nil;
        end;
      end
      else begin
        DosError := 0;
        GetLongNamePChar := Dest;
      end;
    end;
  end;

  function TruePathNamePChar(Dest, PathName : PChar) : PChar;          {!!.06}
    {-Liefert den kÅrzesten Pfad zu einer Datei, oder erweitert den
      Dateinamen mit dem momentanen Verzeichnis}
  begin
    TruePathNamePChar := nil;
    if (Dest = nil) or (PathName = nil) then
      Exit;

    DosError := 0;
    if not FlagIsSet(LFN_Proc, bTruePathName) then begin
      TruePathNamePChar := TruePathNameDOSPChar(Dest, PathName);
      Exit;
    end;

    with Regs do begin
      CX := 0;
      ES := Seg(Dest^);
      DI := Ofs(Dest^);
      if not Int21namePChar($7160, PathName) then begin
        if AX = $7100 then begin
          {-Funktion nicht verfÅgbar, deaktivieren}
          ClearFlag(LFN_Proc, bTruePathName);
          TruePathNamePChar := TruePathNameDOSPChar(Dest, PathName);
        end
        else begin
          DosError := AX;
          TruePathNamePChar := nil;
        end;
      end
      else begin
        DosError := 0;
        TruePathNamePChar := Dest;
      end;
    end;
  end;
{$ENDIF}

  procedure DOSResetDrive; Assembler;
    {-Erzwingt das Schreiben der Buffers und des Cache}
  asm
    mov     ah, 0Dh
    int     21h
  end;

  procedure FlushDrive(Drive : Byte);
    {-Erzwingt das Schreiben der Buffers und des Cache
        FÅr Drive gilt: 0 = aktuelles Laufwerk, 1 = Laufwerk A, usw.}
  begin
    if not Win95_Aktiv then
      DOSResetDrive
    else
      with Regs do begin
        AX := $710D;
        CX := 0;                  {-Values for drive reset action:
          0000h  flush filesystem buffers for drive, and reset drive
          0001h  flush filesystem buffers and cache for drive, and reset drive
          0002h  remount DriveSpace volume}
        if Drive = 0 then
          DX := Byte(DefaultDrive)-64
        else
          DX := Drive;
        Flags := 0;                                                    {!!.07}
        MsDos(Regs);
      end;
  end;

  procedure ResetDrive(Drive : Byte);
    {-Erzwingt das Schreiben und Lîschen der Buffers und des Cache
        FÅr Drive gilt: 0 = aktuelles Laufwerk, 1 = Laufwerk A, usw.}
  begin
    if not Win95_Aktiv then
      DOSResetDrive
    else
      with Regs do begin
        AX := $710D;
        CX := 1;                  {-Values for drive reset action:
          0000h  flush filesystem buffers for drive, and reset drive
          0001h  flush filesystem buffers and cache for drive, and reset drive
          0002h  remount DriveSpace volume}
        if Drive = 0 then
          DX := Byte(DefaultDrive)-64
        else
          DX := Drive;
        Flags := 0;                                                    {!!.07}
        MsDos(Regs);
      end;
  end;

  function IsNetworkDrive(Drive : Char) : Boolean; Assembler;          {!!.06}
    {-True wenn Drive ein Laufwerk im Netzwerk ist}
  asm
    xor     al, al
    mov     bl, Drive
    cmp     bl, 'A'
    jb      @@Done
    sub     bl, '@'
    mov     ax, 4409h
    int     21h
    xor     al, al
    test    dx, 1000h
    jz      @@Done
    inc     al
@@Done:
  end;

  {-Funktionen der Kommandozeilenparameter:
      ParamLine, ParamCount, ParamPosition, ParamStr}
  {--------------------------------------------------------------------}
  {-                          LFN.IN7 v1.10                           -}
  {-     Funktionen der Kommandozeilenparameter fÅr LFN.PAS v1.10     -}
  {--------------------------------------------------------------------}

  procedure GetModuleNamePrim(Dest : PChar; MaxLen : Word); Assembler; {!!.06}
    {-Liefert den Namen des aktuellen Moduls aus dem Environment}
  asm
    push    ds
    {-Zeiger in das Environment ermitteln}
    mov     ds, PrefixSeg
    mov     es, ds:word ptr 2Ch
    xor     di, di
    {-Das Ende des Environment suchen}
    xor     ax, ax
    cld
@@SearchZero:
    cmp     al, es:[di]
    je      @@EofEnv
    mov     cx, -1
    repne   scasb
    jmp     @@SearchZero
@@EofEnv:
    {-3 Stellen weiter beginnt der Filename}
    add     di, 3
    {-Das Ende des Filenamens suchen}
    mov     si, di
    push    es
    pop     ds
    mov     cx, -1
    repne   scasb
    xchg    ax, cx
    not     ax
    dec     ax
    {-Max. LÑnge prÅfen und String speichern}
    les     di, Dest
    mov     cx, MaxLen
    cmp     cx, ax
    jb      @@LenOk
    xchg    ax, cx
@@LenOk:
    push    cx
    rep     movsb
    xor     al, al
    stosb
    pop     ax
    pop     ds
  end;

  function GetModuleName : String; Assembler;                          {!!.06}
    {-Liefert den Namen des aktuellen Moduls zurÅck}
  asm
{$IFDEF Ver60}
    push    bp                                                         {!!.08}
    mov     bp, sp                                                     {!!.08}
{$ENDIF}
{$IFDEF Windows}
    push    HInstance
{$ENDIF}
    les     di, @Result
    inc     di
    push    es
    push    di
    push    255
{$IFDEF Windows}
    call    GetModuleFileName
{$ELSE}
    call    GetModuleNamePrim
{$ENDIF}
    {-LÑnge des AsciiZ ermitteln und LÑngenbyte setzen}
    les     di, @Result
    push    di
    inc     di
    xor     al, al
    cld
    mov     cx, 255
    repne   scasb
    not     cl
    dec     cl
    mov     al, cl
    pop     di
    stosb
{$IFDEF Ver60}
    leave                                                              {!!.08}
{$ENDIF}
  end;

  function GetModuleNamePChar(Dest : PChar; MaxLen : Word) : PChar; Assembler; {!!.06}
    {-Liefert den Namen des aktuellen Moduls zurÅck}
  asm
{$IFDEF Windows}
    push    HInstance
{$ENDIF}
    push    Dest.word[2]
    push    Dest.word[0]
    mov     ax, MaxLen
    inc     ax
    push    ax
{$IFDEF Windows}
    call    GetModuleFileName
{$ELSE}
    call    GetModuleNamePrim
{$ENDIF}
    mov     ax, Dest.word[0]
    mov     dx, Dest.word[2]
  end;

  function ParamLine : String;
    {-Liefert die komplette Kommandozeile zurÅck}
  begin
   {$IFDEF Windows}
    ParamLine := StrPas(CmdLine);
   {$ELSE}
    ParamLine := String(Ptr(PrefixSeg, $80)^);
   {$ENDIF}
  end;

var
  DelimStack : String[127];                                            {!!.08}
  DelimCount : Byte absolute DelimStack;                               {!!.08}

  function Win95ParamCount : Word;                     {!!.08 various changes}
    {-Liefert die Anzahl der Kommandozeilen-Parameter zurÅck}
  label
    ReStart;
  var
    I, Count : Word;
  begin
    Count := 0;
    I := 1;
    while I <= Length(ParamPtr^) do begin
      DelimCount := 0;
      {-Zuerst die Leerzeichen Åbergehen}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] = ' ') do
        Inc(I);
ReStart:
      {-Nun die Sondertrennzeichen behandeln}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] in ParamDelims) do begin
        Inc(DelimCount);
        DelimStack[DelimCount] := ParamPtr^[I];
        Inc(I);
      end;

      {-Ist I nicht au·erhalb der Zeile, dann sind wir am Beginn eines Wortes}
      if I <= Length(ParamPtr^) then
        Inc(Count);

      {-Das Ende des momentanen Wortes suchen, zuerst den Stack abarbeiten}
      while (DelimCount > 0) and (I <= Length(ParamPtr^)) do begin
        if (ParamPtr^[I] = DelimStack[DelimCount]) and (DelimCount > 0) then
          Dec(DelimCount);
        Inc(I);
      end;

      {-Dann das nÑchste Leerzeichen suchen}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] <> ' ') do begin
        if ParamPtr^[I] in ParamDelims then begin
          Dec(Count);
          goto ReStart;
        end;
        Inc(I);
      end;
    end;
    Win95ParamCount := Count;
  end;

  function ParamPosition(Index : Word) : Word;         {!!.08 various changes}
    {-Liefert die Position eines Parameters in der Kommandozeile}
  label
    ReStart;
  var
    I, Count : Word;
  begin
    ParamPosition := 0;
    Count := 0;
    I := 1;
    while (I <= Length(ParamPtr^)) and (Index <> Count) do begin
      DelimCount := 0;
      {-Zuerst die Leerzeichen Åbergehen}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] = ' ') do
        Inc(I);
ReStart:
      {-Nun die Sondertrennzeichen behandeln}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] in ParamDelims) do begin
        Inc(DelimCount);
        DelimStack[DelimCount] := ParamPtr^[I];
        Inc(I);
      end;

      {-Ist I nicht au·erhalb der Zeile, dann sind wir am Beginn eines Wortes}
      if I <= Length(ParamPtr^) then
        Inc(Count);

      if Index <> Count then begin
        {-Das Ende des momentanen Wortes suchen, zuerst den Stack abarbeiten}
        while (DelimCount > 0) and (I <= Length(ParamPtr^)) do begin
          if (ParamPtr^[I] = DelimStack[DelimCount]) and (DelimCount > 0) then
            Dec(DelimCount);
          Inc(I);
        end;

        {-Dann nach dem nÑchsten Leerzeichen suchen}
        while (I <= Length(ParamPtr^)) and (ParamPtr^[I] <> ' ') do begin
          if ParamPtr^[I] in ParamDelims then begin
            Dec(Count);
            goto ReStart;
          end;
          Inc(I);
        end;
      end
      else
        ParamPosition := I;
    end;
  end;

  function Win95ParamStr(Index : Word) : String;       {!!.08 various changes}
    {-Liefert einen Kommandozeilen-Parameter zurÅck}
  label
    ReStart;
  var
    I, Len : Word;
    Temp : PChar;
  begin
    if Index = 0 then begin
      {!!.06 rewritten begin}
     {$IFDEF UseDos}
      Win95ParamStr := GetLongName(GetModuleName);
     {$ELSE}
      GetMem(Temp, PCharSize);
      GetModuleNamePChar(Temp, 255);
      GetLongNamePChar(Temp, Temp);
      Win95ParamStr := StrPas(Temp);
      FreeMem(Temp, PCharSize);
     {$ENDIF}
      {!!.06 end}
      Exit;
    end;

    Len := 0;
    I := ParamPosition(Index);
    if I > 0 then begin
      {-Das Ende des momentanen Wortes suchen}
ReStart:
      {-Zuerst den Stack abarbeiten}
      while (DelimCount > 0) and (I <= Length(ParamPtr^)) do begin
        if (DelimCount > 0) and (ParamPtr^[I] = DelimStack[DelimCount]) then
          Dec(DelimCount)
        else
          {-Alles was kein Sonderzeichen ist gehîrt zum Parameter}
          if not(ParamPtr^[I] in ParamDelims) then begin
            Inc(Len);
            Win95ParamStr[Len] := ParamPtr^[I];
          end;
        Inc(I);
      end;

      {-Dann nach dem nÑchsten Leerzeichen suchen}
      while (I <= Length(ParamPtr^)) and (ParamPtr^[I] <> ' ') do begin
        if ParamPtr^[I] in ParamDelims then begin
          {-Sonderzeichen auf den Stack bringen}
          while (I <= Length(ParamPtr^)) and (ParamPtr^[I] in ParamDelims) do begin
            Inc(DelimCount);
            DelimStack[DelimCount] := ParamPtr^[I];
            Inc(I);
          end;
          {-Und mit der Verarbeitung derselben fortfahren}
          goto ReStart;
        end;

        Inc(Len);
        Win95ParamStr[Len] := ParamPtr^[I];
        Inc(I);
      end;
    end;

    Win95ParamStr[0] := Char(Len);
  end;

  {-Ersatzfunktionen fÅr Unit WinDos:
      CreateDir, RemoveDir, SetCurDir, GetCurDir, GetArgStr,
      FileExpand, FileSplit, FileSearch}
  {--------------------------------------------------------------------}
  {-                          LFN.IN8 v1.10                           -}
  {-       Ersatzfunktionen fÅr Unit WinDos fÅr LFN.PAS v1.10         -}
  {--------------------------------------------------------------------}

{$IFDEF UseWinDos}

  procedure Win95CreateDir(Dir : PChar);
    {-Erzeugt ein neues Unterverzeichnis.}
  begin
    if Dir = nil then
      Exit;
    InOutRes := 0;
    with Regs do begin
      AX := $7139;
      DS := Seg(Dir^);
      DX := Ofs(Dir^);
      Flags:= 0;                                                       {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pCreateDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pCreateDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95RemoveDir(Dir : PChar);
    {-Entfernt ein leeres Unterverzeichnis.}
  begin
    if Dir = nil then
      Exit;
    InOutRes := 0;
    with Regs do begin
      AX := $713A;
      DS := Seg(Dir^);
      DX := Ofs(Dir^);
      Flags:= 0;                                                       {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pRemoveDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pRemoveDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  procedure Win95SetCurDir(Dir : PChar);
    {-éndert das aktuelle Verzeichnis zum angegebenen Pfad.}

    function ChDrive(Drive : Char) : Boolean; Assembler;
      {-Wechselt das Laufwerk}
    asm
      mov     bx, 1               {-ChDrive:= True}
      mov     ah, 0Eh             {-Laufwerk wechseln}
      mov     dl, Drive
      and     dl, 0DFh            {-Nur Gro·buchstaben}
      sub     dl, 'A'             {-A: = 0, etc. umrechnen}
      int     21h
      mov     ah, 19h             {-Laufwerk ermitteln}
      int     21h
      cmp     al, dl              {-Ist es gesetzt worden?}
      je      @@Done
      dec     bx                  {-Nein, dann False}
@@Done:
      mov     ax, bx              {-Ergebnis nach AX}
    end;

  var
    Temp : PChar;
  begin
    if Dir = nil then
      Exit;
    InOutRes := 0;

    {-Laufwerk wechseln}
    Temp := Dir;
    Inc(Temp);
    if (StrLen(Dir) >= 2) and (Temp^ = ':') then begin
      if not ChDrive(Dir^) then
        {-UngÅltiges Laufwerk}
        InOutRes := 15;
      if StrLen(Dir) = 2 then
        Exit;
    end;

    with Regs do begin
      AX := $713B;
      DS := Seg(Dir^);
      DX := Ofs(Dir^);
      Flags:= 0;                                                       {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pSetCurDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pSetCurDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX;
    end;
  end;

  function Win95GetCurDir(Dir : PChar; Drive : Byte) : PChar;
    {-Gibt das aktuelle Verzeichnis des Laufwerks zurÅck.}
  var
    Temp : PChar;
  begin
    Win95GetCurDir := nil;
    if Dir = nil then
      Exit;

    InOutRes := 0;

    {-Laufwerksbuchstaben ermitteln}
    asm
      les     di, Dir
      cld
      mov     al, Drive
      or      al, al              {-Drive <> 0?}
      jne     @@DriveAsChar
      mov     ah, 19h             {-Mom. Laufwerk ermitteln}
      int     21h
      inc     al
@@DriveAsChar:
      add     al, '@'             {-In 'A'..'Z' umwandeln}
      stosb
      mov     ax, '\:'            {-':\' anhÑngen}
      stosw
    end;

    with Regs do begin
      {-Das momentane Verzeichnis ermitteln}
      AX := $7147;
      DL := Drive;
      DS := Seg(Dir^);
      Temp := Dir;
      Inc(Temp, 3);
      SI := Ofs(Temp^);
      Flags:= 0;                                                       {!!.07}
      MsDos(Regs);
      if Flags and FCarry <> 0 then
        {-Fehler}
        if AX = $7100 then begin
          {-Funktion wird nicht unterstÅzt, deaktivieren}
          DisableLFNFunc(pGetCurDir);
          {-Original aufrufen}
          POPJumpFAR(@PatchAddr[pGetCurDir]);
        end
        else
          {-Fehlercode fÅr IOResult setzen}
          InOutRes := AX
      else
        Win95GetCurDir := Dir;
    end;
  end;

  function Win95GetArgStr(Dest : PChar; Index : Integer; MaxLen : Word) : PChar;
    {-Gibt die Kommandozeilenparameter aus Index zurÅck.}
  begin
    if Dest <> nil then
      if Index = 0 then
        Win95GetArgStr := GetLongNamePChar(Dest, GetModuleNamePChar(Dest, MaxLen))
      else
        Win95GetArgStr := StrPCopy(Dest, Copy(Win95ParamStr(Index), 1, MaxLen))
    else
      Win95GetArgStr := nil;
  end;

  function Win95FileExpand(Dest, Name : PChar) : PChar;
    {-Expandiert einen Dateinamen.}
  const
    BackSlash : array[0..1] of Char = '\'#0;
    TempSize = 1024;
  var
    NamePtr, Temp : PChar;
  begin
    Win95FileExpand := nil;
    if (Dest = nil) or (Name = nil) then
      Exit;

    {-TemporÑr Platz anlegen, weil Dest=Name sein kann}
    HeapFunc(On);
    GetMem(Temp, TempSize);
    HeapFunc(Off);
    if Temp = nil then
      Exit;

    {-Wurde ein Pfad mit angegeben?}
    NamePtr := StrRScan(Name, '\');
    if NamePtr = nil then
      NamePtr := StrRScan(Name, ':');

    if NamePtr = nil then begin
      {-Momentanes Verzeichnis ermitteln}
      Temp := GetCurDir(Temp, 0);
      if Regs.AX = $7100 then begin
        {-Function nicht verfÅgbar, deaktivieren}
        DisableLFNFunc(pFileExpand);
        FreeMem(Temp, TempSize);
        POPJumpFAR(@PatchAddr[pFileExpand]);
      end;
      {-Backslash anhÑngen}
      if StrLen(Temp) > 3 then                                         {!!.07}
        Temp := StrCat(Temp, @BackSlash);
      {-Dateinamen anhÑngen}
      Temp := StrCat(Temp, Name);
    end
    else begin
      {-Den korrekten Pfad ermittlen}
      Temp := TruePathNamePChar(Temp, Name);
      if Regs.AX = $7100 then begin
        {-Function nicht verfÅgbar, deaktivieren}
        DisableLFNFunc(pFileExpand);
        FreeMem(Temp, TempSize);
        POPJumpFAR(@PatchAddr[pFileExpand]);
      end;
    end;

    {-Ergebnis Åbertragen und Temp freigeben}
    Win95FileExpand := StrCopy(Dest, Temp);
    FreeMem(Temp, TempSize);
  end;

  function Win95FileSplit(Path, Dir, Name, Ext : PChar) : Word;
    {-Zerlegt einen vollstÑndigen Dateinamen in seine drei Komponenten.}
  var
    DirPtr, NamePtr, ExtPtr : PChar;
    DirLen, NameLen, ExtLen, PathLen, Result : Word;
  begin
    Win95FileSplit := 0;
    if Path = nil then Exit;
    if Dir <> nil then Dir^ := #0;
    if Name <> nil then Name^ := #0;
    if Ext <> nil then Ext^ := #0;

    Result := 0;

    {-Nach der Extension suchen}
    ExtPtr := StrRScan(Path, '.');

    {-Nach dem Ende eines Pfades suchen}
    NamePtr := StrRScan(Path, '\');
    if NamePtr = nil then
      NamePtr := StrRScan(Path, ':');

    if (ExtPtr <> nil) and (NamePtr <> nil) then                       {!!.10}
      if ExtPtr < NamePtr then                                         {!!.10}
        {-Die Extension kann nicht lÑnger als der Name+Extension sein} {!!.10}
        ExtPtr:= nil;                                                  {!!.10}

    {-Pfad gefunden?}
    if NamePtr <> nil then begin
      {-Letztes Zeichen gehîrt nicht zum Namen}
      Inc(NamePtr);
      {-Der Anfang von Path enthÑlt das Directory}
      DirPtr := Path;
    end
    else begin
      {-Der Anfang von Path enthÑlt den Dateinamen}
      NamePtr := Path;
      {-Kein Pfad vorhanden}
      DirPtr := nil;
    end;

    {-LÑnge des ganzen Path ermitteln}
    PathLen := StrLen(Path);

    {-Extension gefunden?}
    if ExtPtr <> nil then begin
      {-LÑnge ermitteln}
      ExtLen := StrLen(ExtPtr);
      {-Kann das Ergebnis Åbertragen werden?}
      if Ext <> nil then
        StrLCopy(Ext, ExtPtr, ExtLen);
      {-RÅckgabeflag setzen}
      Inc(Result, fcExtension);
    end
    else
      {-Kein Extension}
      ExtLen := 0;

    {-Pfad gefunden?}
    if DirPtr <> nil then begin
      {-PfadlÑnge berechnen}
      DirLen := PathLen-StrLen(NamePtr);
      {-Kann das Ergebnis Åbertragen werden?}
      if Dir <> nil then
        StrLCopy(Dir, DirPtr, DirLen);
      {-RÅckgabeflag setzen}
      Inc(Result, fcDirectory);
    end
    else
      {-Kein Pfad}
      DirLen := 0;

    {-LÑnge des Dateinamens berechnen}
    NameLen := PathLen-DirLen-ExtLen;
    {-Kann das Ergebnis Åbertragen werden?}
    if (Name <> nil) and (NameLen > 0) then begin
      StrLCopy(Name, NamePtr, NameLen);
      {-RÅckgabeflag setzen}
      Inc(Result, fcFileName);
    end;

    {-Wildcards enthalten?}
    if (StrScan(Path, '*') <> nil) or (StrScan(Path, '?') <> nil) then
      Inc(Result, fcWildCards);

    Win95FileSplit := Result;
  end;

  function Win95FileSearch(Dest, Name, List : PChar) : PChar;          {!!.08}
  begin
    {-FileSearchPrim befindet sich in LFN.IN4}
    if not FileSearchPrim(Dest, 512, Name, List) then begin
      {-Funktion wird nicht unterstÅzt, deaktivieren}
      DisableLFNFunc(pFileSearch);
      {-Original aufrufen}
      POPJumpFAR(@PatchAddr[pFileSearch]);
    end
    else
      Win95FileSearch := Dest;
  end;

{$ENDIF}

  {-Funktionen zur Initalisierung:
      Initialize, EnableLFN, DisableLFN, Win95_Aktiv}
  {--------------------------------------------------------------------}
  {-                          LFN.IN6 v1.10                           -}
  {-         Funktionen zur Initalisierung fÅr LFN.PAS v1.10          -}
  {--------------------------------------------------------------------}

  procedure GetProcPtr;
    {-Ermittelt die Pointer der zu patchenden Routinen}
  label
    lMkDir, lRmDir, lChDir, lGetDir,
    lFReset, lFRewrite, lErase, lRename,
    lAssignText,
  {$IFDEF UseDos}
    lGetFAttr_D, lSetFAttr_D, lGetFTime_D, lSetFTime_D,
    lFSearch,                                                          {!!.08}
    lGetEnv,                                                           {!!.09}
  {$ENDIF}
    lParamCount, lParamStr,                                            {!!.04}
    lAssignFile,                                                       {!!.05}
    lFClose,                                                           {!!.05}
    {!!.06 begin}
  {$IFDEF UseWinDos}
    lCreateDir, lRemoveDir, lSetCurDir, lGetCurDir,
    lGetFAttr_W, lSetFAttr_W, lGetFTime_W, lSetFTime_W,
    lGetArgCount, lGetArgStr, lFileExpand, lFileSplit,
    lFileSearch,                                                       {!!.08}
  {$ENDIF}
    {!!.06 end}
    Start;
  var
    S : String[1];
    F : file;
    T : Text;
    W : Word;
    L : LongInt;
    P : PChar;
  begin
    goto Start;
    System.MkDir(''); lMkDir:
    System.RmDir(''); lRmDir:
    System.ChDir(''); lChDir:
    System.GetDir(0, S); lGetDir:
    System.Reset(F, 1); lFReset:
    System.Rewrite(F, 1); lFRewrite:
    System.Erase(F); lErase:
    System.Rename(F, S); lRename:
    System.Assign(T, S); lAssignText:
  {$IFDEF UseDos}
    Dos.GetFAttr(F, W); lGetFAttr_D:
    Dos.SetFAttr(F, W); lSetFAttr_D:
    Dos.GetFTime(F, L); lGetFTime_D:
    Dos.SetFTime(F, L); lSetFTime_D:
lFSearch: if Dos.FSearch('', '') = '' then ;                           {!!.08}
lGetEnv: if Dos.GetEnv('') = '' then ;                                 {!!.09}
  {$ENDIF}
lParamCount: if System.ParamCount = 0 then ;                           {!!.04}
lParamStr: if System.ParamStr(1) = '' then ;                           {!!.04}
    System.Assign(F, S); lAssignFile:                                  {!!.05}
    System.Close(F); lFClose:                                          {!!.05}
    {!!.06 begin}
  {$IFDEF UseWinDos}
    WinDos.CreateDir(P); lCreateDir:
    WinDos.RemoveDir(P); lRemoveDir:
    WinDos.SetCurDir(P); lSetCurDir:
lGetCurDir: P := WinDos.GetCurDir(P, 0);
    WinDos.GetFAttr(F, W); lGetFAttr_W:
    WinDos.SetFAttr(F, W); lSetFAttr_W:
    WinDos.GetFTime(F, L); lGetFTime_W:
    WinDos.SetFTime(F, L); lSetFTime_W:
lGetArgCount: if WinDos.GetArgCount = 0 then ;
lGetArgStr: P := WinDos.GetArgStr(P, 0, 0);
lFileExpand: P := WinDos.FileExpand(P, P);
lFileSplit: if WinDos.FileSplit(P, P, P, P) = 0 then ;
lFileSearch: if WinDos.FileSearch(P,P,P) = nil then;                   {!!.08}
  {$ENDIF}
    {!!.06 end}
Start:
    PatchAddr[pDummy] := @PatchAddr[pDummy];                           {!!.06}
    asm
      mov     si, pMkDir
      mov     bx, offset lMkDir
      call    @StorePtr

      mov     si, pRmDir
      mov     bx, offset lRmDir
      call    @StorePtr

      mov     si, pChDir
      mov     bx, offset lChDir
      call    @StorePtr

      mov     si, pGetDir
      mov     bx, offset lGetDir
      call    @StorePtr

      mov     si, pFReset
      mov     bx, offset lFReset
      call    @StorePtr

      mov     si, pFRewrite
      mov     bx, offset lFRewrite
      call    @StorePtr

      mov     si, pErase
      mov     bx, offset lErase
      call    @StorePtr

      mov     si, pRename
      mov     bx, offset lRename
      call    @StorePtr

      mov     si, pAssignText
      mov     bx, offset lAssignText
      call    @StorePtr

    {$IFDEF UseDos}
      mov     si, pGetFAttr_D
      mov     bx, offset lGetFAttr_D
      call    @StorePtr

      mov     si, pSetFAttr_D
      mov     bx, offset lSetFAttr_D
      call    @StorePtr

      mov     si, pGetFTime_D
      mov     bx, offset lGetFTime_D
      call    @StorePtr

      mov     si, pSetFTime_D
      mov     bx, offset lSetFTime_D
      call    @StorePtr

      {!!.08 begin}
      mov     si, pFSearch
      mov     bx, offset lFSearch
      mov     cx, offset lGetEnv
      call    @SearchStorePtr
      {!!.08 end}

      {!!.09 begin}
      mov     si, pGetEnv
      mov     bx, offset lGetEnv
      mov     cx, offset lParamCount
      call    @SearchStorePtr
      {!!.09 end}
    {$ENDIF}

      {!!.04 begin}
      mov     si, pParamCount
      mov     bx, offset lParamCount
      mov     cx, offset lParamStr
      call    @SearchStorePtr

      mov     si, pParamStr
      mov     bx, offset lParamStr
      mov     cx, offset lAssignFile                                   {!!.05}
      call    @SearchStorePtr
      {!!.04 end}

      {!!.05 begin}
      mov     si, pAssignFile
      mov     bx, offset lAssignFile
      call    @StorePtr

      mov     si, pFClose
      mov     bx, offset lFClose
      call    @StorePtr
      {!!.05 end}

      {!!.06 begin}
    {$IFDEF UseWinDos}
      mov     si, pCreateDir
      mov     bx, offset lCreateDir
      call    @StorePtr

      mov     si, pRemoveDir
      mov     bx, offset lRemoveDir
      call    @StorePtr

      mov     si, pSetCurDir
      mov     bx, offset lSetCurDir
      call    @StorePtr

      mov     si, pGetCurDir
      mov     bx, offset lGetCurDir
      mov     cx, offset lGetFAttr_W
      call    @SearchStorePtr

      mov     si, pGetFAttr_W
      mov     bx, offset lGetFAttr_W
      call    @StorePtr

      mov     si, pSetFAttr_W
      mov     bx, offset lSetFAttr_W
      call    @StorePtr

      mov     si, pGetFTime_W
      mov     bx, offset lGetFTime_W
      call    @StorePtr

      mov     si, pSetFTime_W
      mov     bx, offset lSetFTime_W
      call    @StorePtr

      mov     si, pGetArgCount
      mov     bx, offset lGetArgCount
      mov     cx, offset lGetArgStr
      call    @SearchStorePtr

      mov     si, pGetArgStr
      mov     bx, offset lGetArgStr
      mov     cx, offset lFileExpand
      call    @SearchStorePtr

      mov     si, pFileExpand
      mov     bx, offset lFileExpand
      mov     cx, offset lFileSplit
      call    @SearchStorePtr

      mov     si, pFileSplit
      mov     bx, offset lFileSplit
      mov     cx, offset lFileSearch                                   {!!.08}
      call    @SearchStorePtr

      {!!.08 begin}
      mov     si, pFileSearch
      mov     bx, offset lFileSearch
      mov     cx, offset Start
      call    @SearchStorePtr
      {!!.08 end}
    {$ENDIF}
      {!!.06 end}

      jmp     @Done

@StorePtr:
      shl     si, 2               {-Index im Array berechnen}
      sub     bx, 4               {-Position auf die Adresse des CALL FAR}
      lea     di, PatchAddr[si]   {-Adresse des Array holen}
      mov     ax, cs:[bx]         {-OFS des CALL FAR lesen}
      mov     [di], ax            {-Im Array speichern}
      mov     ax, cs:[bx+2]       {-SEG des CALL FAR lesen}
      mov     [di+2], ax          {-Im Array speichern}
      retn

      {!!.04 begin}
@SearchStorePtr:
      mov     ax, cs              {-ES:DI => Zeiger auf den Code}
      mov     es, ax
      mov     di, bx
      sub     cx, bx              {-CX = Bytes bis zum nÑchtes Label}
      mov     al, 9Ah             {-$9A = erstes Byte des CALL FAR...}
      cld                         {-...vorwÑrts...}
      repne   scasb               {-...suchen}
      mov     bx, di              {-CS:[BX] = erstes Bytes nach $9A}
      shl     si, 2               {-Index im Array berechnen}
      lea     di, PatchAddr[si]   {-Adresse des Array holen}
      mov     ax, cs:[bx]         {-OFS des CALL FAR lesen}
      mov     [di], ax            {-Im Array speichern}
      mov     ax, cs:[bx+2]       {-SEG des CALL FAR lesen}
      mov     [di+2], ax          {-Im Array speichern}
      retn
      {!!.04 end}
@Done:
    end;
  end;

type
  OS = record
         O, S : Word;
       end;

  procedure Initialize;
  var
    P : PatchType;
  begin
    {-Pointer der zu patchenden Routinen in SYSTEM und DOS ermitteln}
    GetProcPtr;

    {-Sicherung des alten Codes}
    for P := LowPatchType to Pred(HighPatchType) do
      case P of                                                        {!!.07}
        pAssignText, pAssignFile :                                     {!!.07}
          Move(PatchAddr[P]^, SpecialSavePatch[P],                     {!!.07}
               SizeOf(SpecialPatchRec));                               {!!.07}
      else
        Move(PatchAddr[P]^, SavePatch[P], SizeOf(PatchRec));
      end;

    {$IFDEF Dpmi}
    {-Korretur der Adressen fÅr Patch}
    for P:= LowPatchType to Pred(HighPatchType) do
      OS(PatchAddr[P]).S:= OS(PatchAddr[P]).S+SelectorInc;
    {$ENDIF}
  end;

  procedure EnableLFNFunc(Which : PatchType);                          {!!.04}
    {-Aktiviert die gewÅnschte Funktion dieser Unit}
  var
    Patch : PatchRec;
    SpecialPatch : SpecialPatchRec;
    Save : Pointer;
  begin
    Patch.Code := $EA;            {-JMP FAR}
    with Patch do
      case Which of
        pMkDir : Addr := @Win95MkDir;
        pRmDir : Addr := @Win95RmDir;
        pChDir : Addr := @Win95ChDir;
        pGetDir : Addr := @Win95GetDir;
        pFReset : Addr := @Win95FReset;
        pFRewrite : Addr := @Win95FRewrite;
        pErase : Addr := @Win95Erase;
        pRename : Addr := @Win95Rename;

      {$IFDEF UseDos}
        pGetFAttr_D : Addr := @Win95GetFAttr;
        pSetFAttr_D : Addr := @Win95SetFAttr;
        pGetFTime_D : Addr := @Win95GetFTime;
        pSetFTime_D : Addr := @Win95SetFTime;
        pFSearch : Addr := @Win95FSearch;
        pGetEnv : Addr := @Win95GetEnv;
      {$ENDIF}
        pParamCount : Addr := @Win95ParamCount;                        {!!.04}
        pParamStr : Addr := @Win95ParamStr;                            {!!.04}
        pFClose : Addr := @FILECLOSE;                                  {!!.05}

        {!!.06 begin}
      {$IFDEF UseWinDos}
        pGetArgCount : Addr := @Win95ParamCount;
        pGetArgStr : Addr := @Win95GetArgStr;
        pFileExpand : Addr := @Win95FileExpand;
        pFileSplit : Addr := @Win95FileSplit;
        pGetFAttr_W : Addr := @Win95GetFAttr;
        pSetFAttr_W : Addr := @Win95SetFAttr;
        pGetFTime_W : Addr := @Win95GetFTime;
        pSetFTime_W : Addr := @Win95SetFTime;
        pCreateDir : Addr := @Win95CreateDir;
        pRemoveDir : Addr := @Win95RemoveDir;
        pSetCurDir : Addr := @Win95SetCurDir;
        pGetCurDir : Addr := @Win95GetCurDir;
        pFileSearch : Addr:= @Win95FileSearch;                         {!!.08}
      {$ENDIF}
        {!!.06 end}

        {!!.07 begin}
        pAssignText, pAssignFile :
          with SpecialPatch do begin
            Instr[0] := $31; Instr[1] := $D2; {-XOR DX, DX}
            Code := $EA;          {-JMP FAR}
            case Which of
              pAssignText : Addr := @AssignText;
              pAssignFile : Addr := @AssignFile;
            end;
          end;
        {!!.07 end}

        pDummy : Exit;                                                 {!!.06}
      else
        WriteLn('Uuups.');
        Halt;
      end;

    {!!.06 begin}
  {$IFDEF Windows}
    Save := PatchAddr[Which];
    {-Datensegment-Alias fuer Codesegment holen}
    with OS(PatchAddr[Which]) do
      S := AllocCStoDSalias(S);
  {$ENDIF}
    {!!.06 end}

    case Which of                                                      {!!.07}
      pAssignText, pAssignFile :                                       {!!.07}
        Move(SpecialPatch, PatchAddr[Which]^,                          {!!.07}
             SizeOf(SpecialPatchRec));                                 {!!.07}
    else
      Move(Patch, PatchAddr[Which]^, SizeOf(PatchRec));
    end;

    {!!.06 begin}
  {$IFDEF Windows}
    {-Selector freigeben}
    if FreeSelector(OS(PatchAddr[Which]).S) = 0 then {nothing} ;
    PatchAddr[Which] := Save;
  {$ENDIF}
    {!!.06 end}
  end;

  procedure DisableLFNFunc(Which : PatchType);
    {-Deaktiviert die gewÅnschte Funktion dieser Unit}
  var
    Save : Pointer;
  begin
    {!!.06 begin}
  {$IFDEF Windows}
    Save := PatchAddr[Which];
    {-Datensegment-Alias fuer Codesegment holen}
    with OS(PatchAddr[Which]) do
      S := AllocCStoDSalias(S);
  {$ENDIF}
    {!!.06 end}

    case Which of                                                      {!!.07}
      pAssignText, pAssignFile :                                       {!!.07}
        Move(SpecialSavePatch[Which], PatchAddr[Which]^,               {!!.07}
             SizeOf(SpecialPatchRec));                                 {!!.07}
    else
      Move(SavePatch[Which], PatchAddr[Which]^, SizeOf(PatchRec));
    end;

    {!!.06 begin}
  {$IFDEF Windows}
    {-Selector freigeben}
    if FreeSelector(OS(PatchAddr[Which]).S) = 0 then ;
    PatchAddr[Which] := Save;
  {$ENDIF}
    {!!.06 end}
  end;

  procedure EnableLFN;
    {-Aktiviert die UnterstÅtzung fÅr lange Dateinamen}
  var
    P : PatchType;
  begin
    LFN_Proc := LFN_AllProcs;
    for P := LowPatchType to Pred(HighPatchType) do
      EnableLFNFunc(P);                                                {!!.04}
  end;

  procedure DisableLFN;
    {-Deaktiviert die UnterstÅtzung fÅr lange Dateinamen}
  var
    P : PatchType;
  begin
    LFN_Proc := 0;
    for P := LowPatchType to Pred(HighPatchType) do
      DisableLFNFunc(P);                                               {!!.04}
  end;

  function Win95_Aktiv : Boolean; Assembler;                 {!!.04 rewritten}
    {-Ermittelt ob Win95 aktiv ist}
  asm
    mov     ax, 160Ah
    int     2Fh
    or      ax, ax
    je      @@Supported
    {-Aufruf wird nicht unterstÅtzt}
    mov     al, False
    jmp     @@Done
@@Supported:
    cmp     bx, 0395h             {-Ist es Win95a oder Ñlter?}
    jl      @@Done
    mov     al, True
@@Done:
  end;

begin
  Initialize;
  if Win95_Aktiv then
    EnableLFN;

  {$IFDEF Windows}
  GetMem(ParamPtr, StrLen(CmdLine)+1);                                 {!!.06}
  ParamPtr^ := StrPas(CmdLine);                                        {!!.06}
  {$ELSE}
  ParamPtr := Ptr(PrefixSeg, $80);                                     {!!.05}
  {$ENDIF}

  {!!.05 begin}
  {$IFDEF AssignLongName}
  AssignNew := LFNAssignNew;
  AssignGetName := LFNAssignGetName;
  AssignDispose := LFNAssignDispose;
  {$ENDIF}
  {!!.05 end}
end.
