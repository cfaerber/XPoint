{   $Id$

    Copyright (C) 1991-2001 Peter Mandrella
    Copyright (C) 2000-2001 OpenXP team (www.openxp.de)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

unit encoder;

{$I XPDEFINE.INC }

interface

type str90=string[90];
     tbytestream=array[0..63] of byte;

function EncodeBase64(var source; len:word): string;
procedure encode_UU(var bytestream:tbytestream;len:word;
                    var encoded:str90);

implementation

type tbase64alphabet=array[0..63] of char;

const cbase64alphabet:tbase64alphabet=
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function EncodeBase64(var source; len:word): string;
  var i,j,l:word;
      b:array[0..3] of byte;
      bytestream: tbytestream;
  begin
    result:='';
    if len=0 then exit;
    fillchar(bytestream,sizeof(bytestream),0);
    move(source,bytestream,len);

    l:=0;
    for i:=0 to (len-1) div 3 do begin
      inc(l,3);
      if l>len then l:=len;
      b[0]:=(bytestream[i*3] and $fc) shr 2;
      b[1]:=((bytestream[i*3] and $03) shl 4)
            or ((bytestream[i*3+1] and $f0) shr 4);
      b[2]:=((bytestream[i*3+1] and $0f) shl 2)
            or ((bytestream[i*3+2] and $c0) shr 6);
      b[3]:=bytestream[i*3+2] and $3f;
      for j:=0 to (l-1) mod 3+1 do
       result:=result+cbase64alphabet[b[j]];
      for j:=1 to 2-(l-1) mod 3 do
       result:=result+'=';
    end;
  end;

procedure encode_UU(var bytestream:tbytestream;len:word;
                    var encoded:str90);
  var i,j:word;
      b:array[0..3] of byte;
  begin
    encoded:='';
    if len=0 then exit;
    for i:=len to sizeof(tbytestream)-1 do bytestream[i]:=0;
    for i:=0 to (len-1) div 3 do begin
      b[0]:=(bytestream[i*3] and $fc) shr 2;
      b[1]:=((bytestream[i*3] and $03) shl 4)
            or ((bytestream[i*3+1] and $f0) shr 4);
      b[2]:=((bytestream[i*3+1] and $0f) shl 2)
            or ((bytestream[i*3+2] and $c0) shr 6);
      b[3]:=bytestream[i*3+2] and $3f;
      for j:=0 to 3 do begin
        if b[j]=0 then b[j]:=64;
        encoded:=encoded+char(b[j]+32);
      end;
    end;
    encoded:=char(len+32)+encoded;
  end;

{
  $Log$
  Revision 1.7  2001/09/08 16:29:28  mk
  - use FirstChar/LastChar/DeleteFirstChar/DeleteLastChar when possible
  - some AnsiString fixes

  Revision 1.6  2001/05/27 14:23:34  ma
  - cleaned up a bit

  Revision 1.5  2001/03/13 19:24:55  ma
  - added GPL headers, PLEASE CHECK!
  - removed unnecessary comments

  Revision 1.4  2000/04/04 21:01:20  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
end.

