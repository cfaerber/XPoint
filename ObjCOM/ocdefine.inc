{  $Id$

   Created on October, 16st 2000 by Markus K„mmerer <mk@happyarts.de>

   This software is part of the ObjCOM project.

   Global compiler directives
}

{ Include Rangecheck and other compilerdependend Debuging-Code }

{.$DEFINE Debug }

{ Include Linenumbers in EXE-File }

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
    {$C+}                 { Assertation support }
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
  Revision 1.1  2000/10/16 12:19:06  mk
  - added ocdefine.inc

}
