{   $Id$

    OpenXP Statistical SPAM Classifier

    Copyright (C) 2003 OpenXP team (www.openxp.de)
    Copyright (C) 2003 Claus F"arber.

    Derieved from parts of bogofilter
    Copyright (C) 2002 Eric S. Raymond et. al.

    Credits:
    Eric S. Raymond: author of bogofilter, parts of which have been translated
      to PASACL and used herein.
    Gary Robinson: proposed the algorithm used (in a modified version) here in
      http://radio.weblogs.com/0101454/stories/2002/09/16/spamDetection.html.
    Paul Graham: Original idea of a statistical SPAM filter in his article
      ``A plan for Spam'' (http://www.paulgraham.com/spam.html).

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 2 of the License, or (at your
    option) any later version.

    This program is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc}

unit xpspam;

interface

uses
  xpheader,
  datadef,
  classes,
  xpglobal;

type
  TSpamStatsWord = packed record
    Word: String;
    GoodCount,BadCount: Integer;
    Prob: Double;
  end;

  TSpamStats = packed record
    Spamicity:  Double;
    ROBX: Double; ROBX_CNT: Integer;
    MostInteresting: array[0..11] of TSpamStatsWord;
    LeastInteresting: array [0..7] of TSpamStatsWord;
  end;

  TSpamStatus = (
    stHam,
    stSpam,
    stUnknown
  );

  TSpamicityCalculator = class
  private
    d: DB;
    GoodFld, BadFld: Integer;
    ROBX:    Double;
    ROBX_CNT:Integer;
    FStats:  TSpamStats;

  private
    procedure OpenDatabase;
    procedure CloseDatabase;

  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

  public
    function CalculateMessageSpamicity(Content:TStream): Double;

  private
    function GetSpamStatus: TSpamStatus;
    function GetIsSpam: boolean;
    function GetIsHam: boolean;
    function GetIsUnknown: boolean;

  public
    property Statistics: TSpamStats read FStats;
    property Spamicity: Double read FStats.Spamicity;
    property Status: TSpamStatus read GetSpamStatus;

    property IsSpam: boolean read GetIsSpam;
    property IsHam: boolean read GetIsHam;
    property IsUnknown: boolean read GetIsUnknown;
  end;

procedure register_message_as_spam(Content: TStream;
  SpamStatus, OldSpamStatus: TSpamStatus);

function calc_message_spamicity(Content: TStream): Double; overload;
function calc_message_spamicity(Content: TStream; var Status: TSpamStats): Double; overload;

implementation

{$IFDEF Delphi]
{$DEFINE HAS_UNIT_HASHES}
{$ENDIF}

uses
  math,
//xp2db,
  xp0,
  sysutils,
{$IFDEF HAS_UNIT_HASHES}
  hashes,
{$ENDIF}
  database;

type xfloat = record
  mant: double;
  exp:  integer;
end;

{$IFDEF HAS_UNIT_HASHES}
type TWordList = TIntegerHash;
{$ELSE}
type 
  PTWordListData = ^TWordListData;
  TWordListData = record
    Next:       PTWordListData;
    Used:       Integer;
    Data:       array [0..8191] of integer;
  end;

  TWordList = class(TStringList)
  private
    FList: TStringList;
    FVals: PTWordListData;
    FIterPos: integer;

    function GetCurrentKey: string;
    function GetWords(const Key: string): integer;
    procedure SetWords(const Key: string; NewValue: integer);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Restart;
    function Next: boolean;
    function Exists(const Key: string): boolean;
    property CurrentKey: string read GetCurrentKey;
    property Values[const Key: string]: integer read GetWords write SetWords; default;
  end;

constructor TWordList.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := true; 

  GetMem(FVals, Sizeof(TWordListData));
  FVals.Next := nil;
  FVals.Used := 0;
end;

destructor TWordList.Destroy;
var dd: PTWordListData;
begin
  while assigned(FVals) do
  begin
    dd := FVals^.Next;
    FreeMem(FVals);
    FVals := dd;
  end;  

  FList.Free;
end;

function TWordList.GetCurrentKey: string;
begin
  result := FList[FIterPos];
end;

function TWordList.GetWords(const Key: string): integer;
var i: integer;
begin
  if FList.Find(Key, i) then
    result := PInteger(FList.Objects[i])^
  else
    raise ERangeError.Create('key not found');
end;

procedure TWordList.SetWords(const Key: string; NewValue: integer);
var dd: PTWordListData;
    pt: PInteger;
    i:  Integer;
begin
  if Find(Key, i) then
    PInteger(FList.Objects[i])^ := NewValue
  else begin
    if FVals^.Used >= High(FVals^.Data)-Low(FVals^.Data)+1 then
    begin
      GetMem(dd,sizeof(TWordlistdata));
      dd^.Next := FVals;
      dd^.Used := 0;
      FVals := dd;
    end;
    pt := @(FVals^.Data[FVals^.Used+Low(FVals^.Data)]);
    Inc(FVals^.Used);
    pt^ := NewValue;
    FList.AddObject(Key,TObject(pointer(pt)));
  end;
end;

procedure TWordList.Restart;
begin
  FIterPos := -1;
end;

function TWordList.Next: boolean;
begin
  Inc(FIterPos);
  result := FIterPos < FList.Count;
end;

function TWordList.Exists(const Key: string): boolean;
var i: integer;
begin
  result :=  FList.Find(Key,i);
end;
{$ENDIF}

const
  EVEN_ODDS = 0.5;
  MIN_DEV = 0.1;

  MAX_WORDS = 2;

  ROBS = 0.001; 	        (* Robinson's s *)
  ROBX_DEFAULT = 0.415; 	(* Robinson's x *)

  ROBX_CNT_START = 500000;
  ROBX_SCALE = 100000000;

function make_wordlist(Content: TStream): TWordList;
var
  Words: TWordList;
  Word: String;
  Buffer: array[1..8192] of Char;
  BufPos,BufCnt: Integer;
  Done: boolean;
  Bytes: Integer;

  procedure add_word(const word_src:string);
  var Word: string;
  begin
    Word := Uppercase(Word_src);
    if Length(Word)>7 then SetLength(Word,7);

    if Words.Exists(Word) then
      Words[Word] := Min(MAX_WORDS,Words[Word]+1)
    else
      Words[Word] := 1;
  end;

begin
  Words := TWordList.Create;

  Word := '';
  Bytes := 0;
  BufPos := 1;
  BufCnt := 0;
  Done := false;

  repeat
    if BufPos >= BufCnt then
    begin
      BufPos := Low(Buffer);
      BufCnt := Content.Read(Buffer,High(Buffer)-Low(Buffer)+1)+Low(Buffer);
      if BufCnt <= Low(Buffer) then
        done := true;
    end;

    if done or (Buffer[BufPos]in[#0..#14,#$12,#$14,#$17..#$20,
      '!','"','''','(',')',',','.',':',';','?','@','[',']','{','}',
      '\','|','`','<','=','>','/']) then
    begin
      if Length(Word) > 2 then add_word(Word);
      Word := '';
    end else
      Word := Word+Buffer[BufPos];
    Inc(BufPos);
    Inc(Bytes);
  until done or (Bytes>20000);

  Result := Words;
end;

procedure clear_spam_stats(var status: TSpamStats);
var i: integer;
begin
  status.Spamicity := EVEN_ODDS;
  status.ROBX := ROBX_DEFAULT;
  status.ROBX_CNT := ROBX_CNT_START;

  for i := Low(status.MostInteresting) to High(status.MostInteresting) do
  begin
    status.MostInteresting[i].Word := '';
    status.MostInteresting[i].GoodCount := 0;
    status.MostInteresting[i].BadCount := 0;
    status.MostInteresting[i].Prob := EVEN_ODDS
  end;

  for i := Low(status.LeastInteresting) to High(status.LeastInteresting) do
  begin
    status.LeastInteresting[i].Word := '';
    status.LeastInteresting[i].GoodCount := 0;
    status.LeastInteresting[i].BadCount := 0;
    status.LeastInteresting[i].Prob := 1.0;
  end;
end;

procedure register_message_as_spam(Content: TStream;
  SpamStatus, OldSpamStatus: TSpamStatus);
var
  Words:   TWordList;
  Word:    string;
  Count:   integer;
  GoodFld,BadFld,WordFld,DatFld: integer;
  d:       DB;
  GoodCount,GoodOld: integer;
  BadCount, BadOld: integer;
  Dat, DatOld: integer;

var t,m,j   : smallword;
    h,mm,s,ss: smallword;
    
begin
  if OldSpamStatus=SpamStatus then exit;
  dbOpen(d,SpamFltFile,1);
  try
  GoodFld := dbGetFeldNr(d,'goodcnt');
  BadFld  := dbGetFeldNr(d,'badcnt');
  WordFld  := dbGetFeldNr(d,'word');
  DatFld  := dbGetFeldNr(d,'datum');
  Words := make_wordlist(Content);
  try
    Words.Restart;
    while Words.Next do
    begin
      Word := Words.CurrentKey;
      Count := Min(Words[Word], MaxInt div 2);
      dbSeek(d,spamiword,Word);
      if dbFound then
      begin
        GoodCount := dbReadIntN(d,GoodFld); GoodOld := GoodCount;
        BadCount := dbReadIntN(d,BadFld);   BadOld  := BadCount;
      end else
      begin
        GoodCount := 0; GoodOld := -1;
        BadCount := 0;  BadOld  := -1;
        dbAppend(d);
        dbWriteNStr(d,WordFld,word);
      end;

      if OldSpamStatus=stSpam then
        BadCount := BadCount - Count else
      if OldSpamStatus=stHam then
        GoodCount := GoodCount - Count;

      if SpamStatus=stSpam then
        BadCount := BadCount + Count else
      if SpamStatus=stHam then
        GoodCount := GoodCount + Count;

      if GoodCount < 0 then GoodCount := 0;
      if BadCount  < 0 then BadCount  := 0;

      if GoodCount > MaxInt div 2 then GoodCount := MaxInt div 2;
      if BadCount  > MaxInt div 2 then BadCount  := MaxInt div 2;

      DatOld := dbReadIntN(d,DatFld);
      Dat := Trunc(Double(Now) * 24); // date + hours only
      if (DatOld < Dat)and(DatOld<>0) then Dat := (DatOld + Dat +1) div 2;

      if GoodCount<>GoodOld then dbWriteN(d,GoodFld,GoodCount);
      if BadCount <>BadOld  then dbWriteN(d,BadFld, BadCount);
      if Dat      <>DatOld  then dbWriteN(d,DatFld, Dat);
    end;

  finally
    Words.Free;
  end;
  finally
    dbClose(d);
  end;
end;

type PTSpamStats = ^TSpamStats;

function calc_spam_stats_dowork(d: DB;
  Content: TStream;
  GoodFld,BadFld: integer;
  var ROBX:    Double;
  var ROBX_CNT:Integer;
  Status: PTSpamStats): Double;

var token:   string;
    prob:    Double;
    dev:     Double;
    P,Q:     xFloat;
    robn:    integer;
    Words:   TWordlist;
    i,j:     integer;

  function compute_probability(const token: string): Double;
  var GoodCount: integer;
      BadCount: integer;
      prob: Double;
      robx_add: integer;

    procedure add_word_stat;
    var i: integer;
        j: integer;
       lp: Double;
       pp: Double;
    begin
      pp := abs(EVEN_ODDS-Prob);

      lp := 1.0;
      j := -1;

      for i := Low(status^.MostInteresting) to High(status^.MostInteresting) do
        if (abs(status^.MostInteresting[i].Prob-EVEN_ODDS) < lp) then
        begin
          lp := abs(status^.MostInteresting[i].Prob-EVEN_ODDS);
          j := i;
        end;
      if (j >= 0) and (abs(status^.MostInteresting[j].Prob-EVEN_ODDS)<pp) then
      begin
        status^.MostInteresting[j].Word := token;
        status^.MostInteresting[j].GoodCount := GoodCount;
        status^.MostInteresting[j].BadCount := BadCount;
        status^.MostInteresting[j].Prob := Prob;
      end;

      lp := 0.0;
      j := -1;
      for i := Low(status^.LeastInteresting) to High(status^.LeastInteresting) do
        if (abs(status^.LeastInteresting[i].Prob-EVEN_ODDS) > lp) then
        begin
          lp := abs(status^.LeastInteresting[i].Prob-EVEN_ODDS);
          j := i;
        end;
      if (j >= 0) and (abs(status^.LeastInteresting[j].Prob-EVEN_ODDS)>pp) then
      begin
        status^.LeastInteresting[j].Word := token;
        status^.LeastInteresting[j].GoodCount := GoodCount;
        status^.LeastInteresting[j].BadCount := BadCount;
        status^.LeastInteresting[j].Prob := Prob;
      end;

    end;

  begin
    dbSeek(d,spamiword,token);
    if dbFound then
    begin
      GoodCount := dbReadIntN(d,GoodFld);
      BadCount := dbReadIntN(d,BadFld);
    end else
    begin
      GoodCount := 0;
      BadCount := 0;
    end;

    Prob := (ROBS * ROBX + GoodCount) / (ROBS + GoodCount + BadCount);

    if assigned(status) then add_word_stat;

    ROBX_ADD := Min(MaxInt div 2,Trunc(log2(Max(1,GoodCount+BadCount)))+1);
    ROBX_CNT := Max(1,Min(MaxInt div 2,ROBX_CNT+ROBX_ADD));
    ROBX := ((ROBX*(ROBX_CNT-ROBX_ADD)) + Prob*(ROBX_ADD)) / ROBX_CNT;

    result := prob;
  end;

  function get_spamicity(robn: Integer; const p,q: xFLOAT): double;
  const
    ln10: double = 2.302585093;			(* log(10) - 2.3025850929940459  *)
  var
    r: double;
    p_pr,q_pr: double;

  begin
    r := 1.0 / robn;

  //  stats.p_ln = log(P.mant) + P.exp * ln10;	/* invlogsum */
  //  stats.q_ln = log(Q.mant) + Q.exp * ln10;	/* logsum    */

    p_pr := 1.0 - power(P.mant, r) * power(10.0, P.exp * r);	(* Robinson's P *)
    q_pr := 1.0 - power(Q.mant, r) * power(10.0, Q.exp * r);	(* Robinson's Q *)

    result := 1.0 - (1.0 + (p_pr - q_pr) / (p_pr + q_pr)) / 2.0;
  end;

  procedure sort_status(var Status: TSpamStats);
  var i,j: integer;
     save: TSpamStatsWord;
     
  begin
      for i := Low(status.MostInteresting) to High(status.MostInteresting)-1 do
        for j := i+1 to High(status.MostInteresting) do 
          if abs(Status.MostInteresting[i].Prob-EVEN_ODDS) < abs(Status.MostInteresting[j].Prob-EVEN_ODDS) then
          begin
            save := Status.MostInteresting[i];
            Status.MostInteresting[i] := Status.MostInteresting[j];
            Status.MostInteresting[j] := save;
          end;
      
      for i := Low(status.LeastInteresting) to High(status.LeastInteresting)-1 do
        for j := i+1 to High(status.LeastInteresting) do 
          if abs(Status.LeastInteresting[i].Prob-EVEN_ODDS) > abs(Status.LeastInteresting[j].Prob-EVEN_ODDS) then
          begin
            save := Status.LeastInteresting[i];
            Status.LeastInteresting[i] := Status.LeastInteresting[j];
            Status.LeastInteresting[j] := save;
          end;
  end;

begin
  Words := make_wordlist(Content);
  try
    RobN := 0;

    p.mant := 1.0; p.exp := 0;
    q.mant := 1.0; q.exp := 0;

    Words.Restart;
    while Words.Next do
    begin
    (* Robinson's P and Q; accumulation step *)
    (* P = 1 - ((1-p1)*(1-p2)*...*(1-pn))^(1/n)            [spamminess] *)
    (* Q = 1 - (p1*p2*...*pn)^(1/n)                    [non-spamminess] *)
      prob := compute_probability(Words.CurrentKey);
      dev :=  abs(EVEN_ODDS - prob);

      if dev >= min_dev then
      begin

        P.mant := P.mant * Power((1-prob),dev);
        if (P.mant < 1.0e-200) then
        begin
          P.mant := P.mant * 1.0e200;
          P.exp  := P.Exp - 200;
        end;

        Q.mant := Q.Mant * Power(prob,dev);
        if (Q.mant < 1.0e-200) then
        begin
       	  Q.mant := Q.mant * 1.0e200;
      	  Q.exp  := Q.exp - 200;
        end;
        inc(robn);
      end;
    end;

    (* S = (P - Q) / (P + Q)                       [combined indicator] *)

    if (robn>0) then
    begin
      result := get_spamicity(robn, P, Q);
    end else
      result := robx;


    if assigned(status) then
    begin
      status^.Spamicity := Result;
      status^.RobX := ROBX;
      status^.Robx_Cnt := ROBX_CNT;
      sort_status(status^);
    end;
  finally
    Words.Free;
  end;
end;

function calc_spam_stats(Content: TStream; Status: PTSpamStats): Double;
var Calculator: TSpamicityCalculator;
begin
  Calculator := TSpamicityCalculator.Create;
  try
    Calculator.CalculateMessageSpamicity(Content);
    if Assigned(Status) then Status^ := Calculator.Statistics;
  finally
    Calculator.Free;
  end;
end;


function calc_message_spamicity(Content: TStream): Double; overload;
begin
  Result := calc_spam_stats(Content,nil);
end;

function calc_message_spamicity(Content: TStream; var Status: TSpamStats): Double; overload;
begin
  Result := calc_spam_stats(Content,@Status);
end;

(*
  private
    d: DB;
    GoodFld, BadFld: Integer;
    ROBX:    Double;
    ROBX_CNT:Integer;
    FStats:  TSpamStats;
*)

procedure TSpamicityCalculator.OpenDatabase;
begin
  if assigned(d) then exit;

  dbOpen(d,SpamfltFile,1);
  try
    GoodFld := dbGetFeldNr(d,'goodcnt');
    BadFld  := dbGetFeldNr(d,'badcnt');

    dbSeek(d,spamiword,'!ROBX');
    if dbFound then begin
      ROBX     := dbReadIntN(d,GoodFld) / ROBX_SCALE;
      ROBX_CNT := dbReadIntN(d,BadFld);
    end else
    begin
      ROBX     := ROBX_DEFAULT;
      ROBX_CNT := ROBX_CNT_START;
    end;
  except
    dbClose(d);
    d := nil;
    raise;
  end;
end;

procedure TSpamicityCalculator.CloseDatabase;
var i: Integer;
begin
  try
    dbSeek(d,spamiword,'!ROBX');
    if not dbFound then begin
      dbAppend(d);
      dbWriteStr(d,'word','!ROBX');
    end;
    i := Trunc(ROBX*ROBX_SCALE);
    dbWriteN(d,GoodFld,i);
    dbWriteN(d,BadFld, ROBX_CNT);
  finally
    dbClose(d);
  end;
end;

constructor TSpamicityCalculator.Create;
begin
  d := nil;
end;

destructor TSpamicityCalculator.Destroy;
begin
  CloseDatabase;
end;

function TSpamicityCalculator.CalculateMessageSpamicity(Content:TStream): Double;
begin
  OpenDatabase;
  clear_spam_stats(FStats);

  Result := calc_spam_stats_dowork(d,Content,GoodFld,BadFld,
    ROBX,ROBX_CNT,@FStats);
end;

function TSpamicityCalculator.GetSpamStatus: TSpamStatus;
begin
  if IsHam then
    Result := stHam
  else
  if IsSpam then
    Result := stSpam
  else
    Result := stUnknown;
end;

function TSpamicityCalculator.GetIsSpam: boolean;
begin
  Result := Spamicity > 0.75;
end;

function TSpamicityCalculator.GetIsHam: boolean;
begin
  Result := Spamicity < 0.25;
end;

function TSpamicityCalculator.GetIsUnknown: boolean;
begin
  Result := (not IsSpam) and (not IsHam);
end;

//
// $Log$
// Revision 1.2  2003/02/07 16:13:02  cl
// - added field ``DATUM'' to SPAMFLT.DB1
//
// Revision 1.1  2003/01/28 10:42:25  cl
// - Added statistical SPAM filter
//
end.
