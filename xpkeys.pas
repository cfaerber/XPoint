{ --------------------------------------------------------------- }
{ Dieser Quelltext ist urheberrechtlich geschuetzt.               }
{ (c) 1991-1999 Peter Mandrella                                   }
{ CrossPoint ist eine eingetragene Marke von Peter Mandrella.     }
{                                                                 }
{ Die Nutzungsbedingungen fuer diesen Quelltext finden Sie in der }
{ Datei SLIZENZ.TXT oder auf www.crosspoint.de/srclicense.html.   }
{ --------------------------------------------------------------- }

{ (Funktions)tasten & Makros }

{$I XPDEFINE.INC }
{$F+}

unit xpkeys;

interface

uses
  xpglobal, dos,typeform,fileio,inout,keys,resource,xp0,xp1;


type  getf_func   = function(nr,nn:byte):pathstr;
var   getfilename : getf_func;
const FuncExternal: boolean = false;    { *-Funktionen gesperrt }
      PreExtProc  : procedure = nil;


procedure prog_call(nr,nn:byte);
function  test_fkeys(var t:taste):boolean;
procedure Xmakro(var t:taste; flags:byte);


const k0_S  : char = 'S';      { Spezial-Mode         }
      k0_A  : char = 'A';      { Alle Bretter         }
      k0_H  : char = 'H';      { Brett hinzufÅgen     }
      k0_cH : char = ^H;       { aus Maps-Liste anlg. }
      k0_L  : char = 'L';      { Brett lîschen        }
      k0_E  : char = 'E';      { Brett editieren      }
      k0_V  : char = 'V';      { Bretter verknÅpfen   }
      k0_cT : char = ^T;       { Trennzeile einfÅgen  }
      k0_P  : char = 'P';      { Bretter verschieben  }
      k0_Le : char = 'L';      { Lesemode setzen      }
      k0_B  : char = 'b';      { Brettbrief           }
      k0_SB : char = 'B';      { Brett-Crossposting   }
      k0_I  : char = 'I';      { BinÑrdatei senden    }
      k0_TE : char = 'E';      { Textdatei senden     }
      k0_cG : char = ^G;       { Brettgruppe mark.    }
      k0_cE : char = ^E;       { Bretter entmark.     }
      k0_cW : char = ^W;       { Brettweiterschalter  }
      k0_cF : char = ^F;       { Satznummer           }
      k0_Ac : char = 'A';      { Spezial / Zugriff    }

      k1_S  : char = 'S';      { Spezialmode          }
      k1_A  : char = 'A';      { Adre·buch-Umschalter }
      k1_H  : char = 'H';      { User hinzufÅgen      }
      k1_V  : char = 'V';      { Verteiler anlegen    }
      k1_L  : char = 'L';      { User lîschen         }
      k1_E  : char = 'E';      { User editieren       }
      k1_cV : char = ^V;       { Userbretter verkn.   }
      k1_B  : char = 'b';      { Userbrief            }
      k1_SB : char = 'B';      { User-Crossposting    }
      k1_I  : char = 'I';      { BinÑrdatei senden    }
      k1_TE : char = 'E';      { Textdatei senden     }
      k1_R  : char = 'R';      { Adre·buch ein/austr. }
      k1_P  : char = 'P';      { Pa·wort Ñndern       }
      k1_cE : char = ^E;       { User entmarkieren    }
      k1_cW : char = ^W;       { Userweiterschalter   }
      k1_U  : char = 'U';      { Usersuche            }

      k2_S  : char = 'S';      { Spezial-MenÅ         }
      k2_cR : char = ^R;       { Rot13                }
      k2_cH : char = ^H;       { Hex-Dump             }
      k2_I  : char = 'I';      { Info-Fenster         }
      k2_O  : char = 'O';      { Originalheader       }
      k2_H  : char = 'H';      { Halten               }
      k2_L  : char = 'L';      { Lîschen              }
      k2_K  : char = 'K';      { Kill                 }
      k2_cU : char = ^U;       { UserEdit             }
      k2_V  : char = 'V';      { Wiedervorlage        }
      k2_cE : char = ^E;       { Nachrichten entmark. }
      k2_U  : char = 'U';      { User-Fenster         }
      k2_cF : char = ^F;       { Textdatei senden     }
      k2_cI : char = ^I;       { BinÑrdatei senden    }
      k2_G  : char = 'G';      { Bezugssuche          }
      k2_cA : char = ^A;       { Fido-Anzeige umsch.  }
      k2_KA : char = 'A';      { Adre·anzeige im Baum }
      k2_EA : char = 'A';      { EmpfÑngeranzeige     }
      k2_cW : char = ^W;       { Weiterschalter       }
      k2_cD : char = ^D;       { Datumsanzeige        }
      k2_R  : char = 'R';      { Drucken              }
      k2_cN : char = ^N;       { Realnameanzeige      }
      k2_BB : char = '#';      { Kommentarbaum        }
      k2_A  : char = 'A';      { unversandt <-> Alle  }
      k2_b  : char = 'b';      { Brettbrief           }
      k2_cB : char = ^B;       { Quote-Brettbrief     }
      k2_SB : char = 'B';      { Multiquote           }
      k2_p  : char = 'p';      { PM                   }
      k2_cP : char = ^P;       { Quote-PM             }
      k2_SP : char = 'P';      { Multiquote-PM        }
      k2_cT : char = ^T;       { User-Pa·wort-Edit    }
      k2_cQ : char = ^Q;       { indirekter Fido-Quote}

      k3_H  : char = 'H';      { neuer AV-Eintrag     }
      k3_E  : char = 'E';      { AutoVersand Edit     }
      k3_L  : char = 'L';      { lîschen              }
      k3_A  : char = 'A';      { aktiv                }
      k3_T  : char = 'T';      { TextEdit             }
      k3_I  : char = 'I';      { Fileinfo             }
      k3_S  : char = 'S';      { Autoversand Senden   }
      k3_K  : char = 'K';      { Eintrag kopieren     }

      k4_D  : char = 'D';      { Drucken im Lister    }
      k4_W  : char = 'W';      { in Datei schreiben   }
      k4_L  : char = 'L';      { Nachricht lîschen    }
      k4_cL : char = ^L;       { .. und drinbleiben   }
      k4_H  : char = 'H';      { Nachricht halten     }
      k4_F  : char = 'F';      { Farbumschaltung      }


implementation  { -------------------------------------------------- }

uses xp4o,xp7,xp9,xpauto;


{ Funktionstaste in Hauptfenster oder ArcViewer }

procedure prog_call(nr,nn:byte);
var s      : string;
    p0     : byte;
    fn,fn2 : pathstr;
    brk    : boolean;
    dummy  : boolean;
    auto   : boolean;
begin
  with fkeys[nr]^[nn] do begin
    s:=prog;
    auto:=autoexec;
    if s[1]='*' then begin
      if funcexternal then exit;
      s:=ustr(trim(s));
      if copy(s,2,7)='NETCALL' then
        EinzelNetcall(trim(copy(s,10,BoxNameLen)))
      else if copy(s,2,8)='RNETCALL' then
        dummy:=netcall(true,trim(copy(s,11,BoxNameLen)),false,true,false)
      else if mid(s,2)='REORG' then begin
        MsgReorgScan(true,false,brk);
        if not brk then MsgReorg;
        end
      else if copy(s,2,7)='SETUSER' then
        SetUsername(mid(trim(prog),10))    { in XP9 }
      else if copy(s,2,4)='LIST' then begin
        fn:=trim(mid(s,7));
        if exist(fn) then
          if listfile(fn,fn,true,false,0)<>0 then
          else
        else
          rfehler(20);   { '*LIST: Datei nicht vorhanden!' }
        end
      else if copy(s,2,4)='EDIT' then begin
        fn:=trim(mid(s,7));
        if exist(fn) then
          editfile(fn,false,false,0,false)
        else
          rfehler(23)    { '*EDIT: Datei nicht vorhanden!' }
        end
      else if mid(s,2)='AUTOEXEC' then
        auto:=true
      else if s<>'*' then
        rfehler1(21,left(s,50));   { 'UngÅltige Funktion:  %s' }
      end
    else if s<>'' then begin
      p0:=pos('$FILE',ustr(s));
      if p0>0 then begin
        fn:=getfilename(nr,nn);
        if (fn='') or not exist(fn) then exit;
        s:=copy(s,1,p0-1)+fn+copy(s,p0+5,120);
        end;
      if @preextproc<>nil then begin
        preextproc;
        preextproc:=nil;
        end;
      shellkey:=warten;
      if listout then begin
        fn2:=TempFile(TempPath);
        shell(s+'>'+fn2,speicher,0);
        if listfile(fn2,'',true,false,0)<>0 then;
        if exist(fn2) then era(fn2);
        end
      else
        shell(s,speicher,iif(vollbild,1,3));
      if (p0>0) and not bname and (exist(fn)) then
        era(fn);
      end;
    if auto then
      if (s='') and not exist(AutoxDir+'\*.*') then
        hinweis(getres(150))   { 'keine Dateien im AUTOEXEC-Verzeichnis vorhanden' }
      else
        xpauto.AutoExec(false);
    end;
end;

function test_fkeys(var t:taste):boolean;
var nr,n : shortint;
begin
  test_fkeys:=false;
  nr:=0;
  if (t>=keysf1) and (t<=keysf10) then nr:=1
  else if (t>=keycf1) and (t<=keycf10) then nr:=2
  else if (t>=keyaf1) and (t<=keyaf10) then nr:=3;
  if nr>0 then begin
    n:=(ord(t[2])-84)mod 10+1;
    if fkeys[nr]^[n].prog<>'' then begin
      prog_call(nr,n);
      test_fkeys:=true;
      end;
    end;
end;


{ flags:  1 = Bretter, 2 = User, 4 = Msgs, 8=Lister, 16=ArcViewer, 32=Editor,
          64 = Terminal }

procedure Xmakro(var t:taste; flags:byte);
var i : integer;
    s : string;
begin
  i:=1;
  while (i<=keymacros) and ((macroflags[i] and flags=0) or (t<>macrokey[i])) do
    inc(i);
  if i<=keymacros then begin
    s:=macrodef[i]^;
    if s[1]=#0 then t:=left(s,2)
    else t:=s[1];
    insert(mid(s,length(t)+1),forwardkeys,1);
    end;
end;


end.

