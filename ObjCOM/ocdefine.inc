{
** ObjCOM global compiler directives
** See files "LICENSE.TXT" and "CREDITS.TXT"
}

{ Include rangecheck and other compiler-dependend debugging code }

{.$DEFINE Debug }

{ Include linenumbers in exe file }

{$DEFINE DebugInfo }

{ -- Do not modify from this line! -- }

{$IFNDEF VirtualPascal }
  {$IFNDEF FPC }
     { !! Only FreePascal and Virtual Pascal Supported }
     { !! ToDo: Delphi Support }
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
  {$C-,H+,I-,M-,P-,Q-,R-,S-,T-,V-,X+}
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
    {$Q+,R+,S+}
    {$C+}                 { Assertion support }
  {$ENDIF }
{$ENDIF }

{$IFDEF Win32 }
  {$IFNDEF VP }
    {$DEFINE TCP }
  {$ENDIF }
{$ENDIF }

{$IFDEF OS2 }
  {$IFNDEF VP }
    {$DEFINE TCP }
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
  Revision 1.2  2000/10/28 09:40:28  ma
  - introduced credits.txt

  Revision 1.1  2000/10/16 12:19:06  mk
  - added ocdefine.inc

}
