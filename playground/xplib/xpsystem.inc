{$IFDEF Win32}
  {$INCLUDEPATH sys/win32}
{$ENDIF}

{$IFDEF Unix}
  {$IFDEF Linux}
    {$INCLUDEPATH sys/unix}
  {$ENDIF}
  {$INCLUDEPATH sys/unix}
{$ENDIF}

{$INCLUDEPATH sys}
