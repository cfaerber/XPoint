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
unit XPProgressOutputWindow;

interface

uses ProgressOutput,Classes,SysUtils,Timer;

type
  tpTimer= ^tTimer;

  TProgressOutputWindow = class(TProgressOutput)

  protected
    FVisible,LastMsgUnimportant,ErrorOccurred: Boolean;
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

  TProgressOutputWindowDialog = class(TProgressOutputWindow)
  protected
    FPosY2:   Byte;
//  procedure SVisible(nVisible: Boolean);      override;
    procedure Display(RefreshContent: Boolean); override;
  public
    constructor CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
//    destructor Destroy; override;

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
  typeform,winxp,xp0,xp1,inout;

constructor TProgressOutputWindow.CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
begin
  Timer.Init; TimerToUse:=@Timer;
  FWidth:=iw; FHeight:=ih;
  FHeadline:=Headline; LastTime:='';
  FLines:=TStringList.Create;
  LastMsgUnimportant:=False; ErrorOccurred:=False;
  FVisible:=False; IsVisible:=Visible;
end;

procedure TProgressOutputWindow.SVisible(nVisible: Boolean);
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

procedure TProgressOutputWindow.Display(RefreshContent: Boolean);
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
procedure TProgressOutputWindow.WriteFmt(mc: TMsgClass; fmt: string; args: array of const);
var s: String;
begin
  s:=Format(fmt,args);

  if fmt<>'' then begin
    // if last message was "not important" and current message is no error,
    // overwrite last message
    if LastMsgUnimportant and (mc<mcError)then
      FLines.Delete(FLines.Count-1)
    else if FLines.Count >= FHeight then
      FLines.Delete(0);
    LastMsgUnimportant:=(mc=mcDebug)or(mc=mcVerbose);
    ErrorOccurred:=ErrorOccurred or(mc>=mcError);
    FLines.Add(s);
    end;
  Display(fmt<>'');
end;

procedure TProgressOutputWindow.SHeadline(strHeadline:string);
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

procedure TProgressOutputWindow.Resize(Width,Height:Integer);
begin
  IsVisible:=false; (* hide window *)
  FWidth:=Width;
  FHeight:=Height;
  IsVisible:=true;  (* will display window *)
end;

destructor TProgressOutputWindow.Destroy;
begin
  WriteFmt(mcInfo,' ',[0]); // well... just to get off msg "hanging up" :-)
  if ErrorOccurred then mdelay(4000)else mdelay(1000);
  IsVisible:=false;
  FLines.Free;
  Timer.Done;
  inherited Destroy;
end;

{-----------------------------------------------------------------------------}

constructor TProgressOutputWindowDialog.CreateWithSize(iw,ih: Integer; Headline: String; Visible: Boolean);
begin
  FPosY2:=0;
  inherited CreateWithSize(iw,ih,headline,visible);
end;


procedure TProgressOutputWindowDialog.Resize(Width,Height:integer);
begin
  FPosY2:=0;
  inherited Resize(Width,Height);
end;

procedure TProgressOutputWindowDialog.ResizeSplit(Width:Integer;Heights:Array of Integer);
var i:Integer;
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

procedure TProgressOutputWindowDialog.Display(RefreshContent: Boolean);
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

procedure TProgressOutputWindowDialog.WrtText(x,y:integer;txt:string);
begin
  if not IsVisible then exit;
  FWrt(FPosX+x,FPosY+y,txt);
end;

procedure TProgressOutputWindowDialog.WrtData(x,y:integer;txt:string;len:integer;ralign:boolean);
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
  Revision 1.3  2001/04/16 18:13:28  ma
  - ProgOutWin now pauses a bit on closing
    (some seconds if an error occured, one second if not)
  - removed other delays

  Revision 1.2  2001/04/16 13:50:45  ma
  - last msg will never be overwritten if new message is error msg

  Revision 1.1  2001/03/21 19:17:08  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
