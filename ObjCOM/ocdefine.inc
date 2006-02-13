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
  {$C-,H+,I-,J+,M-,P-,T-,V-,X+}
  {$DEFINE Int64 }
  {$DEFINE i386 }
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
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J+,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
  {$MINSTACKSIZE $00004000}
  {$MAXSTACKSIZE $00100000}
  {$IMAGEBASE $00400000}
{$ENDIF }

{$IFDEF Kylix }
  {$APPTYPE CONSOLE }
  {$DEFINE Ver32 }
  {$DEFINE Int64 }
  {$DEFINE i386}
  {$A+,B-,C-,D-,E-,F-,G-,H+,I-,J+,K-,L-,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
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
