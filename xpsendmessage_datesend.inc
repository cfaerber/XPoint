{ $Id$

  Copyright (C) 2003 OpenXP/32 Team <www.openxp.de> 
  see CVS log below for authors

  This file is part of OpenXP.

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

function test_senddate(var s:string):boolean;
begin
  if smdl(ixdispdat(s),min_send) then begin
    rfehler(601);    { 'Rueckdatieren nicht moeglich.' }
    s:=fdat(longdat(min_send));
    test_senddate:=false;
    end
  else
    test_senddate:=true;
end;

function DateSend(var sdate: TDateTime):boolean;
var brk      : boolean;
    x,y      : Integer;
    mind     : fdate;
    dd,mm,yy : smallword;
    senddate : string;
begin
  decodedate(now,yy,mm,dd);
  mind.t:=dd; mind.m:=mm; mind.j:=yy;
  incd(mind);
  senddate:=formi(mind.t,2)+'.'+formi(mind.m,2)+'.'+formi(mind.j mod 100,2);
  min_send:=ixdispdat(senddate);
  dialog(27,3,'',x,y);
  madddate(3,2,getres(607),senddate,false,false);   { 'absenden am ' }
  msetvfunc(test_senddate);
  readmask(brk);
  if not brk then sdate := DateTimeToLongDate(ixdispdat(senddate));
  decodedate(sdate,dd,mm,yy);
  enddialog;
  DateSend:=not brk;
end;

procedure OnDateButton(var cont: string);
var SendDate: TDateTime;
   i, n: Integer;
begin
  n := 0;
  for i := 0 to List.Count -1 do
    if not List[i].Empty then
      inc(n);

  if n<>1 then
    fehler(GetRes2(610,67))
  else
  if (GSData.SigningMethod <> sigNone) or (GSData.EncryptionMethod <> secNone) then
    fehler(GetRes2(610,68))
  else
  if DateSend(SendDate) then
  begin
    GSData.SendDate := SendDate;
    MQuit(1);
  end;
end;

//
// $Log: xpsendmessage_datesend.inc,v $
// Revision 1.1  2003/09/06 23:03:08  cl
// - send window - time-shifted sending of message
//   cLOSES task #76792: Sendefenster: Datum
//
