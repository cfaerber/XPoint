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

{$IFDEF FPC }             { Freepascal >= 1.0.2 }
  {$DEFINE Ver32 }
  {$IFDEF GO32V2 }
    {$DEFINE DOS32 }
    {$UNDEF DPMI }
  {$ENDIF }
  {$MODE Delphi }
  {$ASMMODE Intel }
  {$PACKRECORDS Default } { Default: 2 }
  {$C-,H+,I-,M-,P-,Q-,R-,T-,V-,X+}
  {$IFNDEF Linux}
    {$S-}
  {$ENDIF}
  {$DEFINE Int64 }
  {$HINTS OFF}
{$ENDIF }

{$IFDEF DebugInfo }
  {$IFDEF VirtualPascal }
    {$D+,LocInfo+}
  {$ENDIF }
{$ENDIF }

{$IFDEF Debug }
  {$IFDEF VirtualPascal }
    {$Q+,R+,S+,W+,Optimize-}
  {$ENDIF }
  {$IFDEF FPC }
    {$Q+,R-,S+}
    {$C+}                 { Assertion support }
  {$ENDIF }
{$ENDIF }

{$IFDEF Win32 }
  {$DEFINE TCP }
{$ENDIF }

{$IFDEF OS2 }
  {$IFNDEF VP }
    {a$DEFINE TCP }
  {$ENDIF }
{$ENDIF }

{$IFDEF DOS32 }
  {$DEFINE Fossil }
{$ENDIF }

{$IFDEF Linux }
  {$DEFINE TCP }
{$ENDIF }

{
  $Log$
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
