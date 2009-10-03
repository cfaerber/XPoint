{
** ObjCOM global compiler directives
** See files "LICENSE.TXT" and "CREDITS.TXT"
}

{ Include rangecheck and other compiler-dependend debugging code }

{.$DEFINE Debug }

{ Include linenumbers in exe file }

{$DEFINE DebugInfo }

{ -- Do not modify from this line! -- }

{$IFDEF VER130 }          { Delphi 5.0 }
  {$DEFINE Delphi }
{$ENDIF }

{$IFDEF VER140 }          { Delphi 6.0 and Kylix 1.0 }
  {$DEFINE Delphi }
  {$IFDEF Linux }
    {$DEFINE Kylix }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER150 }          { Delphi 7.0 }
  {$DEFINE Delphi }
  {$IFDEF Linux }
    {$DEFINE Kylix }
  {$ENDIF }
{$ENDIF }

{$IFDEF VER170 }          { Delphi 2005/9.0  }
  {$DEFINE Delphi }
{$ENDIF }

{$IFDEF VER180 }          { Delphi 2006/10.0  }
  {$DEFINE Delphi }
{$ENDIF }

{$IFNDEF VirtualPascal }
  {$IFNDEF FPC }
     {$IFNDEF Delphi }
       {$IFNDEF Kylix }
        !! Only Freepascal, Delphi, Kylix and Virtual Pascal is supported
        {$ENDIF }
     {$ENDIF }
  {$ENDIF }
{$ENDIF }

{$IFDEF VirtualPascal }   { Virtual Pascal >= 2.1 }
  {$DEFINE VP }
  {$B-,D-,H+,I-,J+,M-,P+,Q-,R-,S-,T-,V-,W-,X+,Z-}
  {&AlignCode+,AlignData+,AlignRec+,Asm-,Cdecl-,Delphi+,Frame+,G3+,LocInfo-}
  {&Open32-,Optimise-,OrgName-,SmartLink+,Speed+,Use32+,ZD-}
{$ENDIF }

{$IFDEF FPC }             { Freepascal >= 1.0 }
  {$DEFINE Ver32 }
  {$IFDEF GO32V2 }
    {$DEFINE DOS32 }
    {$UNDEF DPMI }
  {$ENDIF }
  {$MODE Delphi }
  {$GOTO ON }
  {$INLINE ON }
  {$PACKRECORDS 8 }       { Default: 2 }
  {$PACKENUM 1 }          { Default: 4, valid: 1, 2, 4, Normal, Default }
  {$H+,I-,M-,P-,T-,V-,X+}
  {$DEFINE Int64 }
  {$DEFINE i386 }
  // writable const noch definieren
{$ENDIF }

{$IFDEF Delphi }
  {$IFDEF GUI }
    {$APPTYPE GUI }
  {$ELSE }
    {$APPTYPE CONSOLE }
  {$ENDIF }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J-,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF Kylix }
  {$APPTYPE CONSOLE }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J-,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF DebugInfo }
  {$IFDEF Delphi }
    {$D+}                 { Debug-Informationen }
    {$L+}                 { Locale Symbole }
    {$Y+}                 { Symbol declarations }
  {$ENDIF }
  {$IFDEF Kylix }
    {$D+}                 { Debug-Informationen }
    {$L+}                 { Locale Symbole }
    {$Y+}                 { Symbol declarations }
  {$ENDIF }
  {$IFDEF VirtualPascal }
    {$D+,LocInfo+}
  {$ENDIF }
{$ENDIF }


{$IFDEF Debug }
  {$IFDEF VirtualPascal }
    {$Q+,R+,S+,W+,Optimize-}
  {$ENDIF }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S+}               { Stack Checking }
    {$ENDIF }
    {$C+}                 { Assertation support }
    {$D+}
    {$M+}                 { Typeinfo for RTTI }
    {$L+}                 { Local Symbols }
    {$Q+}                 { Overflow-Check }
    {$R+}                 { Range Check }
    {$Y+}                 { Browser Informations }
  {$ENDIF }
  {$IFDEF Delphi }
    {$O-}
    {$C+}                 { Assertation support }
    {$Q+}                 { Overflow-Check }
    {$R+}                 { Range Check }
  {$ENDIF }
{$ELSE }
  {$IFDEF FPC }
    {$IFNDEF Linux }
      {$S-}               { Stack Checking }
    {$ENDIF }
    {$C-,M-,Q-,R-,Y-}
  {$ENDIF }
{$ENDIF }

{$IFDEF Win32 }
  {$DEFINE TCP }
{$ENDIF }

{$IFDEF OS2 }
  {$IFNDEF VP }
    {$DEFINE TCP }
  {$ENDIF }
{$ENDIF }

{$IFDEF DOS32 }
  {$DEFINE Fossil }
{$ENDIF }

{$IFDEF Linux}
  {$DEFINE Unix}
  {$DEFINE TCP }
{$ENDIF}

{$IFDEF FreeBSD }
  {$DEFINE Unix }
  {$DEFINE BSD }
  {$DEFINE TCP }
  {$UNDEF Linux }
{$ENDIF }

{$IFDEF NetBSD }
  {$DEFINE Unix }
  {$DEFINE BSD }
  {$DEFINE TCP }
  {$UNDEF Linux }
{$ENDIF }

{
  $Log: ocdefine.inc,v $
  Revision 1.13  2003/08/25 07:01:30  mk
  - added RAW IP Support for OS/2

  Revision 1.12  2003/01/23 10:03:44  mk
  - changes for Kylix and Linux

  Revision 1.11  2003/01/07 11:07:50  mk
  - added some fpc and kylix defines

  Revision 1.10  2003/01/01 16:19:45  mk
  - changes to made FreeBSD-Version compilable

  Revision 1.9  2002/11/23 23:52:14  mk
  - fixed comment in last committ

  Revision 1.8  2002/11/23 23:48:42  mk
  - added Delphi 7 support

  Revision 1.7  2001/09/07 23:24:56  ml
  - Kylix compatibility stage II

  Revision 1.6  2001/01/31 20:18:19  cl
  - improved Win32 rawip implementation:
    - no own ringbuffer
    - detection of remote connection close

  Revision 1.5  2001/01/20 17:36:26  ml
  - linuxextension of telnet-obj

  Revision 1.4  2001/01/03 22:29:24  ma
  - TCP port working only with Win32

  Revision 1.3  2000/12/14 16:29:19  hd
  - Workaround: no range check under FPC
    - please, never, FPC is buggy.

  Revision 1.2  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.1  2000/10/16 12:19:06  mk
  - added ocdefine.inc

}
