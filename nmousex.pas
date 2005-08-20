{
    $Id$
    Copyright (c) 1998 by Michael Van Canneyt
    member of the Free Pascal development team

    Unit to access the ncurses library

    See the file COPYING.FPC included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Many thanks to Ken Wright for his patches !
}
unit nmousex;
interface

{$packrecords C}
{$linklib ncurses}
{$linklib c}
uses
    ncurses;

type
      mmask_t = cardinal;
      pmmask_t = ^mmask_t;

      MEVENT = record
        id:    system.smallint;
        x,y,z: longint;
        bstate: mmask_t;
      end;

    Function getmouse(var me:MEVENT):longint; cdecl; external;
    Function ungetmouse(var me:MEVENT):longint; cdecl; external;
    Function mousemask(newmask:mmask_t; pold:pmmask_t):mmask_t; cdecl; external;

    Function mouse_trafo(x,y:pinteger; to_screen:bool):bool; cdecl; external;
    Function wmouse_trafo(win: pwindow; x,y:pinteger; to_screen:bool):bool; cdecl; external;

    Const
      BUTTON1_RELEASED       = $0000001;
      BUTTON1_PRESSED        = $0000002;
      BUTTON1_CLICKED        = $0000004;
      BUTTON1_DOUBLE_CLICKED = $0000008;
      BUTTON1_TRIPLE_CLICKED = $0000010;
      BUTTON1_RESERVED_EVENT = $0000020;
      BUTTON2_RELEASED       = $0000040;
      BUTTON2_PRESSED        = $0000080;
      BUTTON2_CLICKED        = $0000100;
      BUTTON2_DOUBLE_CLICKED = $0000200;
      BUTTON2_TRIPLE_CLICKED = $0000400;
      BUTTON2_RESERVED_EVENT = $0000800;
      BUTTON3_RELEASED       = $0001000;
      BUTTON3_PRESSED        = $0002000;
      BUTTON3_CLICKED        = $0004000;
      BUTTON3_DOUBLE_CLICKED = $0008000;
      BUTTON3_TRIPLE_CLICKED = $0010000;
      BUTTON3_RESERVED_EVENT = $0020000;
      BUTTON4_RELEASED       = $0040000;
      BUTTON4_PRESSED        = $0080000;
      BUTTON4_CLICKED        = $0100000;
      BUTTON4_DOUBLE_CLICKED = $0200000;
      BUTTON4_TRIPLE_CLICKED = $0400000;
      BUTTON4_RESERVED_EVENT = $0800000;
      BUTTON_CTRL            = $1000000;
      BUTTON_SHIFT           = $2000000;
      BUTTON_ALT             = $4000000;
      ALL_MOUSE_EVENTS       = $7FFFFFF;
      REPORT_MOUSE_POSITION  = $8000000;

implementation

end.
