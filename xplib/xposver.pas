{
  determine os type and version
}

{$I xpdefine.inc }

unit xposver;

interface

function os_name:	ansistring;
function os_release:	ansistring;
function os_version:    ansistring;
function os_arch:       ansistring;

function compiler_name:         ansistring;
function compiler_release:      ansistring;
function compiler_target:       ansistring;
function compiler_arch:         ansistring;

implementation

{$IF Defined(WIN32) or Defined(WIN64)}
uses windows, sysutils;
{$ELSEIF Defined(UNIX)}
uses baseunix;
{$ELSE}
!!! Unknown target
{$IFEND}

{$IF Defined(WIN32) or Defined(WIN64)}
function os_name:	ansistring;
begin
  result := 'Windows';
end;

{$IF not Declared(OSVERSIONINFOEX)}
type OSVERSIONINFOEX = packed record
  dwOSVersionInfoSize, dwMajorVersion, dwMinorVersion, dwBuildNumber, dwPlatformId: DWORD;
  szCSDVersion: array[1..128] of char;
  wServicePackMajor,wServicePackMinor,wSuiteMask: WORD;
  wProductType, wReserved: BYTE;
end;

const VER_NT_WORKSTATION = 1;
{$IFEND}

function os_release:	ansistring;
var osv: OSVERSIONINFOEX;
begin
  osv.dwOSVersionInfoSize := Sizeof(osv);
  GetVersionEx((POSVERSIONINFO(@osv))^);

  result := '';

  case osv.dwPlatformId of
//  VER_PLATFORM_WIN32S

    VER_PLATFORM_WIN32_WINDOWS:
      case osv.dwMajorVersion of
        4: case osv.dwMinorVersion of
             0:  result := '95';
             10: result := '98';
             90: result := 'Me';
           end;
      end;

    VER_PLATFORM_WIN32_NT:
      case osv.dwMajorVersion of
        0..4:   result := 'NT';
        5:      case osv.dwMinorVersion of
                  0:    result := '2000';
                  1:    result := 'XP';
                  else  result := 'Server 2003';
                end;
        6:      if osv.wProductType = VER_NT_WORKSTATION then
                  case osv.dwMinorVersion of
                    0:  result := 'Vista';
                    1:  result := '7';
                    else result := '7+';
                  end
                else
                  case osv.dwMinorVersion of
                    0:  result := 'Server 2008';
                    1:  result := 'Server 2008 R2';
                    else result := 'Server';
                  end;
        else    if osv.wProductType <> VER_NT_WORKSTATION then
                  result := 'Server';
      end;
  end;
end;

function os_version:    ansistring;
var osv: OSVERSIONINFOEX;
begin
  osv.dwOSVersionInfoSize := Sizeof(osv);
  GetVersionEx((POSVERSIONINFO(@osv))^);

  result := IntToStr(osv.dwMajorVersion) + '.' + IntToStr(osv.dwMinorVersion) + IntToStr(osv.dwBuildNumber);
  if osv.szCSDVersion[1]<>#0 then result := result + ' ' + osv.szCSDVersion;
end;

function os_arch:       ansistring;
begin
  result := 'x86';
end;


{$ELSE}
function os_name:	ansistring;
var name: UtsName;
begin
  assert(fpUName(name)=0);
  result := name.sysname;
end;

function os_release:	ansistring;
var name: UtsName;
begin
  assert(fpUName(name)=0);
  result := name.release;
end;

function os_version:    ansistring;
var name: UtsName;
begin
  assert(fpUName(name)=0);
  result := name.version;
end;

function os_arch:       ansistring;
var name: UtsName;
begin
  assert(fpUName(name)=0);
  result := name.machine;
end;
{$IFEND}


function compiler_name: ansistring;
begin
  { These compilers are easily detectable }

  {$IF Defined(FPC) }                            result := 'FreePascal';
  {$ELSEIF Defined(__GPC__) }                    result := 'GNU Pascal';

  { Delphi, Kylix and C++ Builder are not :-( }

  {$ELSIF Defined(BCB) and Defined(VER160) }
    result := 'Borland C#Builder';
  {$ELSIF Defined(BCB) }
    result := 'Borland C++ Builder';
  {$ELSEIF Defined(VER210) or Defined(VER220) or Defined(VER230) or
           Defined(VER240) }
    result := 'Embarcadero RAD';
  {$ELSEIF Defined(VER185) or Defined(VER190) or Defined(VER200) }
    result := 'CodeGear Delphi';
  {$ELSEIF Defined(VER180) }
    result := 'Borland Developer Studio';
  {$ELSEIF Defined(VER170) or Defined(VER160) or Defined(VER150) or
           Defined(VER140) or Defined(VER130) or Defined(VER120) or
           Defined(VER100) or Defined(VER90) or Defined(VER80) }
    {$IFDEF LINUX}
      result := 'Borland Kylix';
    {$ELSE}
      result := 'Borland Delphi';
    {$ENDIF}
    
  {$ELSE} !!! Unknown pascal compiler name {$IFEND}
end;

function compiler_release: ansistring;
begin
  {$IF Defined(FPC) and Defined(VER2_0) }               result := '2.0';
  {$ELSIF Defined(FPC) and Defined(VER2_2) }            result := '2.2';
  {$ELSIF Defined(FPC) and Defined(VER2_4) }            result := '2.4';

  {$ELSEIF Defined(VER160) and Defined(BCB) }           result := '1';  // C#Builder
  {$ELSEIF Defined(VER140) and Defined(LINUX) }         result := '';   // Kylix

  {$ELSEIF Defined(VER210) }                            result := '2010';
  {$ELSEIF Defined(VER200) }                            result := '2009';
  {$ELSEIF Defined(VER190) }                            result := '2007 for .NET';
  {$ELSEIF Defined(VER185) }                            result := '2007 for Win32';
  {$ELSEIF Defined(VER180) }                            result := '2006';
  {$ELSEIF Defined(VER170) }                            result := '2005';
  {$ELSEIF Defined(VER160) }                            result := '8 for .NET';
  {$ELSEIF Defined(VER150) }                            result := '7';
  {$ELSEIF Defined(VER140) }                            result := '6';
  {$ELSEIF Defined(VER130) }                            result := '5';
  {$ELSEIF Defined(VER120) }                            result := '4';
  {$ELSEIF Defined(VER100) }                            result := '3';
  {$ELSEIF Defined(VER90) }                             result := '2';
  {$ELSEIF Defined(VER80) }                             result := '1';

  {$ELSE} !!! Unknown pascal compiler version {$IFEND}

end;

function compiler_target: ansistring;
begin
  {$IF Defined(WIN64)}                                 result := 'Win64';
  {$ELSEIF Defined(WIN32) or Defined(MSWINDOWS)}       result := 'Win32';
  {$ELSEIF Defined(WINCE) }                             result := 'WinCE';

  {$ELSEIF Defined(FREEBSD) }                           result := 'FreeBSD';
  {$ELSEIF Defined(NETBSD) }                            result := 'NetBSD';
  {$ELSEIF Defined(DARWIN) }                            result := 'Mac OS X';
  {$ELSEIF Defined(SUNOS) }                             result := 'Solaris';
  {$ELSEIF Defined(LINUX) }                             result := 'Linux';

  {$ELSEIF Defined(Haiku) }                             result := 'Haiku';
  {$ELSEIF Defined(BEOS) }                              result := 'BeOS';
  {$ELSEIF Defined(QNX) }                               result := 'QNX';

  {$ELSEIF Defined(Go32V2) }                            result := 'Go32v2';
  {$ELSEIF Defined(OS2) }                               result := 'OS/2';
  {$ELSEIF Defined(AMIGA) }                             result := 'Amiga';
  {$ELSEIF Defined(ATARI) }                             result := 'Atari';
  {$ELSEIF Defined(MAC) }                               result := 'Mac Classic';
  {$ELSEIF Defined(PALMOS) }                            result := 'PalmPS';

  {$ELSE} !!! Unknown compiler platform {$IFEND}
end;

function compiler_arch: ansistring;
begin
  {$IF Defined(CPUX86_64) or Defined(CPUAMD64) }        result := 'x86_64';
  {$ELSEIF Defined(CPU386) or Defined(CPUI386) }        result := 'x86';
  {$ELSEIF Defined(CPUSPARC) }                          result := 'SPARC';
  {$ELSEIF Defined(CPU68K) }                            result := 'm68k';
  {$ELSEIF Defined(CPUSPARC) }	                        result := 'SPARC';
  {$ELSEIF Defined(CPUALPHA) }                          result := 'Alpha';
  {$ELSEIF Defined(CPUPOWERPC64) }	                result := 'PowerPC_64';
  {$ELSEIF Defined(CPUPOWERPC) or Defined(CPUPOWERPC) } result := 'PowerPC';
  {$ELSEIF Defined(CPUIA64) }	                        result := 'IA-64';
  {$ELSEIF Defined(CPUARM) }                     	result := 'ARM';
  {$ELSEIF Defined(CPUAVR) }	                        result := 'AVR';
  {$ELSE} !!! Unknown compiler architecture {$IFEND}
end;

end.
