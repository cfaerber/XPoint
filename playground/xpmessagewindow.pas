{  $Id$

   OpenXP message window class

   Copyright (C) 2001 OpenXP team and M.Kiesel

   This is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.

   The software is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file gpl.txt. If not, write to the
   Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I XPDEFINE.INC }

{ OpenXP message window class; used by some netcall classes.
  Displays a window in which for example progress messages may
  be displayed. }
unit XPMessageWindow;

interface

uses IPCClass,Classes,SysUtils,Timer;

type
  tpTimer= ^tTimer;

  TXPMessageWindow = class(TIPC)

  protected
    FVisible,LastMsgUnimportant: Boolean;
    FPosX,FPosY,FWidth,FHeight: Byte;
    FHeadline: String;
    FLines: TStringList;
    procedure Display(RefreshContent: Boolean);
    procedure SVisible(nVisible: Boolean);

  public
    { Timer displayed in right top of window, initialized upon class creation }
    Timer: tTimer;
    { which timer to display? }
    TimerToUse: tpTimer;
    { display which time? }
    TimerDisplay: (mwElapsedTime,mwTimeout,mwNone);

    { True if visible, used also for hiding/restoring window }
    property IsVisible: Boolean read FVisible write SVisible;

    constructor CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);

    { Displays a new message in window }
    procedure WriteFmt(mc: TMsgClass; fmt: string; args: array of const); override;

    destructor Destroy; override;
  end;

implementation  { ------------------------------------------------- }

uses
  {$IFDEF Unix} xpcurses,{$ELSE}crt,{$ENDIF}
  typeform,winxp,xp0,xp1,debug;

constructor TXPMessageWindow.CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
begin
  Timer.Init; TimerToUse:=@Timer;
  FWidth:=iw; FHeight:=ih;
  FHeadline:=Headline;
  FLines:=TStringList.Create;
  LastMsgUnimportant:=False;
  FVisible:=False; IsVisible:=Visible;
end;

procedure TXPMessageWindow.SVisible(nVisible: Boolean);
begin
  if nVisible<>FVisible then begin
    FVisible:=nVisible;
    if nVisible then begin
      diabox(FWidth+3,FHeight+2,FHeadline,FPosX,FPosY);
      Display(true);
      end
    else
      closebox;
  end;
end;

procedure TXPMessageWindow.Display(RefreshContent: Boolean);
var iLine: Integer; s: string;
begin
  if not IsVisible then exit;
  case TimerDisplay of
    mwElapsedTime: s:=FormatDateTime('hh":"nn":"ss',
                      TimerToUse^.ElapsedSec/60/60/24);
    mwTimeout: s:=FormatDateTime('hh":"nn":"ss',
                      TimerToUse^.SecsToTimeout/60/60/24);
    mwNone: s:='';
  end;
  if s<>'' then FWrt(FPosX+2+FWidth-9,FPosY,FormS(s,8));
  if RefreshContent then
    for iLine:=0 to FHeight-1 do
      if iLine>=FLines.Count then
        FWrt(FPosX+2,FPosY+iLine+1,Sp(FWidth))
      else begin
        if iLine=(FLines.Count-1)then TextAttr:=col.colmailerhigh else TextAttr:=col.colmailer;
        FWrt(FPosX+2,FPosY+iLine+1,FormS(FLines[iLine],FWidth));
        TextAttr:=col.colmailer;
        end
end;

{ fmt='': only update timer }
procedure TXPMessageWindow.WriteFmt(mc: TMsgClass; fmt: string; args: array of const);
var s: String;
begin
  s:=Format(fmt,args);

  if fmt<>'' then begin
    Debug.DebugLog('xpmewi','Display: '+s,DLDebug);
    // if last message was "not important", it may be overwritten
    if LastMsgUnimportant then
      FLines.Delete(FLines.Count-1)
    else
      if FLines.Count>=FHeight then FLines.Delete(0);
    LastMsgUnimportant:=(mc=mcDebug)or(mc=mcVerbose);
    FLines.Add(s);
    end;
  Display(fmt<>'');
end;

destructor TXPMessageWindow.Destroy;
begin
  IsVisible:=false;
  FLines.Free;
  Timer.Done;
end;

end.

{
  $Log$
  Revision 1.6  2001/02/19 12:18:28  ma
  - simplified ncmodem usage
  - some small improvements

  Revision 1.5  2001/02/18 16:20:06  ma
  - BinkP's working! :-) - had to cope with some errors in BinkP protocol
    specification...

  Revision 1.4  2001/02/09 17:31:07  ma
  - added timer to xpmessagewindow
  - did some work on AKA handling in xpncfido

  Revision 1.3  2001/02/02 20:59:57  ma
  - moved log routines to ncmodem

  Revision 1.2  2001/02/02 17:14:01  ma
  - new Fidomailer polls :-)

  Revision 1.1  2001/01/19 21:19:09  ma
  - will be used in (xp)ncfido, (xp)ncuucp...
  - compiles, but untested yet

}
