{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1998, 2000 by Robert B”ck                                   }
{ (c) 2000 OpenXP Team & Markus K„mmerer, http://www.openxp.de    }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{ --------------------------------------------------------------- }
{ $Id$ }

unit encoder;

{$I XPDEFINE.INC }

interface

type str90=string[90];
     tbytestream=array[0..63] of byte;

procedure encode_base64(var bytestream:tbytestream;len:word;
                        var encoded:str90);
procedure encode_UU(var bytestream:tbytestream;len:word;
                    var encoded:str90);

implementation

type tbase64alphabet=array[0..63] of char;

const cbase64alphabet:tbase64alphabet=
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

procedure encode_base64(var bytestream:tbytestream;len:word;
                        var encoded:str90);
  var i,j,l:word;
      b:array[0..3] of byte;
  begin
    encoded:='';
    if len=0 then exit;
    for i:=len to sizeof(tbytestream)-1 do bytestream[i]:=0;
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
       encoded:=encoded+cbase64alphabet[b[j]];
      for j:=1 to 2-(l-1) mod 3 do
       encoded:=encoded+'=';
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

end.
{
  $Log$
  Revision 1.4  2000/04/04 21:01:20  mk
  - Bugfixes für VP sowie Assembler-Routinen an VP angepasst

  Revision 1.3  2000/02/17 16:14:19  mk
  MK: * ein paar Loginfos hinzugefuegt

}
