(* $Id$ *)
(* Generated from source file tree - DO NOT EDIT *)

{$IFDEF Linux}
  {$I unix/linux/xpexample.inc}
{$ELSE}
  {$IFDEF FreeBSD}
    {$I unix/bsd/free/xpexample.inc}
  {$ELSE}
    {$IFDEF BSD}
      {$I unix/bsd/xpexample.inc}
    {$ELSE}
      {$IFDEF Unix}
        {$I unix/xpexample.inc}
      {$ELSE}
        {$IFDEF UnixFS}
          {$I all/xpexample.inc}
        {$ELSE}
          {$I all\xpexample.inc}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
