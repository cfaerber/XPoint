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
    FHeadline,LastTime: String;
    FLines: TStringList;
    procedure Display(RefreshContent: Boolean); virtual;
    procedure SVisible(nVisible: Boolean);      virtual;
    procedure SHeadline(strHeadline: String);   virtual;

  public
    { Timer displayed in right top of window, initialized upon class creation }
    Timer: tTimer;
    { which timer to display? }
    TimerToUse: tpTimer;
    { display which time? }
    TimerDisplay: (mwElapsedTime,mwTimeout,mwNone);

    { True if visible, used also for hiding/restoring window }
    property IsVisible: Boolean read FVisible write SVisible;
    property Headline: string read FHeadline write SHeadline;

    constructor CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
    destructor Destroy; override;

    { Displays a new message in window }
    procedure WriteFmt(mc: TMsgClass; fmt: string; args: array of const); override;

    { Resizes and the window _and_ _makes_ _it_ _visible_ }
    procedure Resize(Width,Height:integer); virtual;
  end;

  TXPMessageWindowDialog = class(TXPMessageWindow)
  protected
    FPosY2:   Byte;
//  procedure SVisible(nVisible: Boolean);      override;
    procedure Display(RefreshContent: Boolean); override;
  public
    constructor CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);

    { Write status texts }
    procedure WrtText(x,y:integer;txt:string);
    procedure WrtData(x,y:integer;txt:string;len:integer;ralign:boolean);

    { Resizes and the window _and_ _makes_ _it_ _visible_ }
    procedure Resize(Width,Height:integer); override;
    { Resize window and split it into several regions }
    procedure ResizeSplit(Width:Integer;Heights:Array of Integer);
  end;

implementation  { ------------------------------------------------- }

uses
  {$IFDEF Unix} xpcurses,{$ELSE}crt,{$ENDIF}
  typeform,winxp,xp0,xp1,debug;

constructor TXPMessageWindow.CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
begin
  Timer.Init; TimerToUse:=@Timer;
  FWidth:=iw; FHeight:=ih;
  FHeadline:=Headline; LastTime:='';
  FLines:=TStringList.Create;
  LastMsgUnimportant:=False;
  FVisible:=False; IsVisible:=Visible;
end;

procedure TXPMessageWindow.SVisible(nVisible: Boolean);
begin
  if nVisible<>FVisible then begin
    FVisible:=nVisible;
    if nVisible then begin
      diabox(FWidth+3,FHeight+2,LeftStr(FHeadline,FWidth-11),FPosX,FPosY);
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
  if(s<>'')and(s<>LastTime)then begin
    FWrt(FPosX+2+FWidth-9,FPosY,FormS(s,8));
    LastTime:=s;
    end;
  if RefreshContent then
    for iLine:=0 to FHeight-1 do
      if iLine>=FLines.Count then
        FWrt(FPosX+2,FPosY+iLine+1,Sp(FWidth))
      else begin
        if iLine=(FLines.Count-1)then TextAttr:=col.colmailerhi2 else TextAttr:=col.colmailer;
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
    else if FLines.Count >= FHeight then
      FLines.Delete(0);
    LastMsgUnimportant:=(mc=mcDebug)or(mc=mcVerbose);
    FLines.Add(s);
    end;
  Display(fmt<>'');
end;

procedure TXPMessageWindow.SHeadline(strHeadline:string);
begin
//  Start bei: FPosX+3
//  Stop bei:  FPosX+2+FWidth-9 -1
//  => maximale Länge: FWidth-11

  if IsVisible then begin
    FWrt(FPosX+3,FPosY,LeftStr(strHeadline,FWidth-11)+' ');
    if Min(FWidth-11,Length(strHeadline)) < Min(FWidth-11,Length(FHeadline)) then
      FWrt(FPosX+3+Length(strHeadline)+1,FPosY,Dup(FWidth-11-Length(strHeadline),#$C4));
  end;
  FHeadline:=strHeadLine;
end;

procedure TXPMessageWindow.Resize(Width,Height:Integer);
var i,j:Integer;
begin
  IsVisible:=false; (* hide window *)
  FWidth:=Width;
  FHeight:=Height;
  IsVisible:=true;  (* will display window *)
end;

destructor TXPMessageWindow.Destroy;
begin
  IsVisible:=false;
  FLines.Free;
  Timer.Done;
end;

{-----------------------------------------------------------------------------}

constructor TXPMessageWindowDialog.CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
begin
  FPosY2:=0;
  inherited CreateWithSize(iw,ih,headline,visible);
end;


procedure TXPMessageWindowDialog.Resize(Width,Height:integer);
begin
  FPosY2:=0;
  inherited Resize(Width,Height);
end;

procedure TXPMessageWindowDialog.ResizeSplit(Width:Integer;Heights:Array of Integer);
var i,j:Integer;
begin
  IsVisible:=false;  (* hide window *)

  (* set size variables *)
  FWidth:=Width;
  FHeight:=0; for i:=Low(Heights) to High(Heights) do
    FHeight:=FHeight+Heights[i]+1;

  (* create dialogue *)
  DiaBox(FWidth+3,FHeight+2,FHeadline,FPosX,FPosY);
  FVisible:=true;

  (* show split lines *)
  FPosY2:=FPosY; for i:=Low(Heights) to (High(Heights)-1) do begin
    FPosY2:=FPosY2+Heights[i]+1;
    FWrt(FPosX,FPosY2,HBar(FWidth+3));
  end;

  Display(true);
end;

procedure TXPMessageWindowDialog.Display(RefreshContent: Boolean);
var ILine: Integer;
    SPos:  Integer;
begin
  if not IsVisible then exit;
  if FPosY2<FPosY then FPosY2:=FPosY;
  inherited Display(false);
  if not RefreshContent then exit;

  SPos:=min(FHeight-FLines.Count,FPosY2-FPosY);
				
  for iLine:= FPosY2-FPosY to FHeight-1 do
    if iLine-spos >= FLines.Count then
      FWrt(FPosX+2,FPosY+iLine+1,Sp(FWidth) )
    else begin
      if (iLine-spos)=(FLines.Count-1) then
        TextAttr:=col.colmailerhi2 else
        TextAttr:=col.colmailer;
      FWrt(FPosX+2,FPosY+iLine+1,FormS(FLines[iLine-spos],FWidth));
      TextAttr:=col.colmailer;
    end;
end;

procedure TXPMessageWindowDialog.WrtText(x,y:integer;txt:string);
begin
  if not IsVisible then exit;
  FWrt(FPosX+x,FPosY+y,txt);
end;

procedure TXPMessageWindowDialog.WrtData(x,y:integer;txt:string;len:integer;ralign:boolean);
begin
  if not IsVisible then exit;
  if Length(txt)>(len-2) then txt:=LeftStr(txt,len-2);
  if ralign then txt:=sp(len-length(txt)-1)+txt+' '
  else   txt:=' '+txt+sp(len-length(txt)-1);
  TextAttr:=col.colmailerhigh;
  FWrt(FPosX+x,FPosY+y,txt);
  TextAttr:=col.colmailer;
end;

{-----------------------------------------------------------------------------}

end.

{
  $Log$
  Revision 1.9  2001/03/20 12:07:08  ma
  - various fixes and improvements

  Revision 1.8  2001/03/16 23:01:27  cl
  - fixed bug w/ field size in TXPMessageWindowDialog.WrtData

  Revision 1.7  2001/03/16 17:17:04  cl
  - TXPMessageWindow can be resized
  - TXPMessageWindow's title can be changed
  - TXPMessageWindowDialog:
    - can be split into several regions
    - allows writes at arbitrary positions

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
