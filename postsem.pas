{$ifndef virtualpascal}

!!! This program must be compiled with Virtual Pascal 2.0 or better!
!!! Dieses Programm muss mit Virtual Pascal 2.0 oder neuer kompiliert werden!

{$else}

{$ifndef os2}

!!! This program must be compiled with target OS/2!
!!! Dieses Programm muss fÅr die Zielplattform OS/2 kompiliert werden!

{$endif}

{$endif}

{ $Id$ }

uses vpsyslow, strings;

var event_sem: string;
    p_event_sem: pChar;

procedure postsem (sem: pChar);
  begin
    sempostevent (semaccessevent (sem));
  end;

begin
  if paramcount <> 1 then begin
    writeln ('Usage: POSTSEM event-semaphore');
    halt (1);
  end;
  event_sem := paramstr (1);
  getmem (p_event_sem, length (event_sem) + 1);
  p_event_sem := StrPCopy (p_event_sem, event_sem);
  postsem (p_event_sem);
  freemem (p_event_sem, length (event_sem) + 1);
end.
{
  $Log$
  Revision 1.1  2000/02/25 01:19:29  mk
  MK: * POSTSEM.PAS hinzugefuegt
 
}