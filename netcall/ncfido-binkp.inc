{  $Id$

   OpenXP fido netcall BinkP protocol implementation include file

   Copyright (C) 2001 by OpenXP team and M.Kiesel

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

   This software is part of the OpenXP project (www.openxp.de).
}

{ OpenXP fido netcall BinkP protocol implementation include file }

type
  tFrameType= (M_NUL,M_ADR,M_PWD,M_FILE,M_OK,M_EOB,M_GOT,M_ERR,M_BSY,M_GET,M_SKIP,M_DATA);
  tBinkpFrame= class
    protected
      FDataSize,DataRead: SmallWord; FCommObj: tpCommObj;
      IsOutgoing: Boolean;
      function SFrameType: tFrameType;
      function SDataSize: SmallWord;
      function SCommandPar: string;
      function SIsReady: boolean;
    public
      { The frame's data part }
      Data: array[1..32767]of byte;

      // Properties are used for incoming frames
      { Returns frame type }
      property FrameType: tFrameType read SFrameType;
      { Returns frame data size (note: one byte for command included if command frame) }
      property DataSize: SmallWord read SDataSize;
      { Gets parameter if frame is a command frame }
      property CommandPar: string read SCommandPar;
      { Returns true is packet is received completely }
      property IsReady: boolean read SIsReady;

      constructor CreateWithCommObj(aCommObj: tpCommObj);
      { Reads a frame from incoming byte stream. Will block until read entiry if Blocking is true. }
      procedure ReadFromStream(Blocking: Boolean);
      { Sends an outgoing command frame.  }
      procedure SendCommandFrame(aCommand: tFrameType; Par: string);
      { Sets up an outgoing data frame.  }
      procedure SetupDataFrame(aDataSize: SmallWord);
      { Sends the frame if outgoing }
      procedure Send;
    end;

const
  stFrameType: array[0..10]of string= ('M_NUL','M_ADR','M_PWD','M_FILE','M_OK','M_EOB','M_GOT','M_ERR','M_BSY','M_GET','M_SKIP');

// ************************** Binkp frame class implementation ************************

function tBinkpFrame.SFrameType: tFrameType;
begin
  if FDataSize>=$8000 then
    result:=tFrameType(Data[1])
  else
    result:=M_DATA;
end;

function tBinkpFrame.SDataSize: SmallWord;
begin
  result:=FDataSize and $7fff;
end;

function tBinkpFrame.SCommandPar: string;
begin
  if FDataSize>=$8000 then begin
    SetLength(result,(FDataSize and $7fff)-1);
    move(Data[2],result[1],length(result));
    end else result:='';
end;

function tBinkpFrame.SIsReady: boolean;
var reads,readthistime,avail: longint;
begin
  result:=false; if IsOutgoing then exit;
  if FDataSize=0 then begin // FDataSize not yet read
    if FCommObj^.CharCount>=2 then begin
      FCommObj^.ReadBlock(FDataSize,2,reads); FDataSize:=Swap(FDataSize);
      end;
    end else begin // FDataSize already read
    result:=DataRead>=(FDataSize and $7fff);
    if not result then begin
      reads:=(FDataSize and $7fff)-DataRead;
      avail:=FCommObj^.CharCount; if avail<reads then reads:=avail;
      while reads>0 do begin
        if reads>1024 then readthistime:=1024 else readthistime:=reads;
        FCommObj^.ReadBlock(Data[DataRead+1],readthistime,readthistime);
        dec(reads,readthistime); inc(DataRead,readthistime);
        end;
      result:=DataRead>=(FDataSize and $7fff);
      if result then // now it's ready
        if FDataSize>$7fff then
          Debug.DebugLog('ncfido','got binkp frame '+stFrameType[Data[1]]+' '+SCommandPar,DLDebug)
        else
          Debug.DebugLog('ncfido','got binkp data frame, size '+Strs(FDataSize),DLDebug)
      end;
    end;
end;

constructor tBinkpFrame.CreateWithCommObj(aCommObj: tpCommObj);
begin inherited create; FCommObj:=aCommObj; end;

procedure tBinkpFrame.ReadFromStream(Blocking: Boolean);
begin
  IsOutgoing:=false; DataRead:=0; FDataSize:=0;
  if Blocking then repeat until SIsReady;
end;

procedure tBinkpFrame.SendCommandFrame(aCommand: tFrameType; Par: string);
begin
  IsOutgoing:=true;
  Data[1]:=ord(aCommand); move(Par[1],Data[2],length(Par));
  FDataSize:=length(Par)+1+$8000; Send;
end;

procedure tBinkpFrame.SetupDataFrame(aDataSize: SmallWord);
begin
  IsOutgoing:=true; FDataSize:=aDataSize;
end;

procedure tBinkpFrame.Send;
var i,written: longint; FrameSize: smallword;
begin
  if IsOutgoing then begin
    FrameSize:=Swap(FDataSize);
    FCommObj^.SendBlock(FrameSize,2,written);
    i:=0;
    while i<(FDataSize and $7fff)do begin
      written:=(FDataSize and $7fff)-i; if written>1024 then written:=1024;
      FCommObj^.SendBlock(Data[i+1],written,written);
      inc(i,written);
      end;
    if FDataSize<=$7fff then
      Debug.DebugLog('ncfido','sent binkp data frame, size '+Strs(FDataSize),DLDebug)
    else
      Debug.DebugLog('ncfido','sent binkp command frame '+stFrameType[Data[1]]+
                     ' '+SCommandPar+', size '+strs(FDataSize and $7fff),DLDebug);
    end;
end;

// ************************** Binkp protocol implementation ************************

{ Binkp session setup procedure. Results are 0: OK, 1: Busy, 2: Error }
function TFidomailer.BinkPSessionSetup: byte;
type tState= (WaitConn,WaitAddr,SendPasswd,AuthRemote,IfSecure,WaitOk,Done);
var
  State,LastState: tState;
  BpF: tBinkpFrame;
  Timer: tTimer;
  s,CRAM_MD5_Challenge: String;

  function DecodeHex(s: string): string;
  begin
    result:='';
    while s<>'' do begin
      result:=result+Char(HexVal(Copy(s,1,2)));
      Delete(s,1,2);
      end;
  end;

begin
  State:=WaitConn; LastState:=WaitConn; BpF:=tBinkpFrame.CreateWithCommObj(FCommObj);
  CRAM_MD5_Challenge:='';
  Timer.Init; Timer.SetTimeout(60);
  Output(mcVerbose,'Authenticating...',[0]);
  repeat
    if LastState<>State then begin
      Debug.DebugLog('ncfido','binkp session setup state '+Strs(Ord(State)),DLDebug);
      LastState:=State;
      end;
    case State of
      WaitConn: begin
        if SysName='' then
          BpF.SendCommandFrame(M_NUL,'SYS OpenXP system')
        else
          BpF.SendCommandFrame(M_NUL,'SYS '+SysName);
        BpF.SendCommandFrame(M_NUL,'ZYZ '+Username);
        BpF.SendCommandFrame(M_ADR,AKAs);
        State:=WaitAddr;
        BpF.ReadFromStream(False);
        end;
      WaitAddr: begin // WaitAddr BEFORE SendPasswd because we're waiting for md5 challenge
        if BpF.IsReady then begin
          case BpF.FrameType of
            M_ADR: State:=SendPasswd;
//** see if answering side presented the address we've called
            M_BSY: begin Output(mcError,'Remote is busy',[0]); result:=1; State:=Done; end;
            M_ERR: begin Output(mcError,'Remote refuses connection',[0]); result:=2; State:=Done; end;
            M_NUL: begin
                     s:=BpF.CommandPar;
                     Log(lcInfo,s);
                     // see if we got an md5 challenge: format "OPT xxx CRAM-MD5/SHA1-challenge yyy"
                     if(Copy(s,1,3)='OPT')and(Pos(' CRAM-',s)<>0) then begin
                       Delete(s,1,Pos(' CRAM-',s)+5);
                       if Pos(' ',s)<>0 then s:=LeftStr(s,Pos(' ',s)-1);
                       if Pos('MD5',s)<>0 then begin
                         Delete(s,1,Pos('-',s));
                         CRAM_MD5_Challenge:=s;
                         end;
                       end;
                     end;
            end;
          if State=WaitAddr then BpF.ReadFromStream(False);
          end;
        end;
      SendPasswd: begin
        Debug.DebugLog('ncfido','Challenge: '+CRAM_MD5_Challenge,DLInform);
        if Password='' then Password:='-';
        if CRAM_MD5_Challenge<>'' then
          Password:='CRAM-MD5-'+LowerCase(CRAM_MD5(Password,DecodeHex(CRAM_MD5_Challenge)));
        BpF.SendCommandFrame(M_PWD,Password);
        State:=AuthRemote;
        end;
      AuthRemote: begin
//** report
        State:=IfSecure;
        end;
      IfSecure: begin
        if Password='-' then begin
          result:=0; State:=Done;
          Output(mcInfo,'Authentication successful',[0]);
        end else begin
          BpF.ReadFromStream(False);
          State:=WaitOk;
          end;
        end;
      WaitOk: begin
        if BpF.IsReady then begin
          case BpF.FrameType of
            M_OK: begin
                    if CRAM_MD5_Challenge<>'' then
                      Output(mcInfo,'Secure authentication successful',[0])
                    else
                      Output(mcInfo,'Plain text authentication successful',[0]);
                    result:=0; State:=Done;
                    end;
            M_BSY: begin Output(mcError,'Remote is busy',[0]); result:=1; State:=Done; end;
            M_ERR: begin Output(mcError,'Remote refuses connection',[0]); result:=2; State:=Done; end;
            M_NUL: Log(lcInfo,BpF.CommandPar);
            end;
          if State<>Done then BpF.ReadFromStream(False);
          end;
        end;
      end;
    Output(mcVerbose,'',[0]);
  until(State=Done)or KeyPressed or Timer.Timeout;
  if Timer.Timeout then begin Output(mcError,'Timeout',[0]); Log(lcError,'timeout'); result:=2; end;
  if Keypressed then begin Output(mcError,'User break',[0]); Log(lcError,'user break'); result:=2; end;
  while KeyPressed do ReadKey;
  BpF.Destroy; Timer.Done;
end;

{ Binkp file transfer procedure. Results are 0: OK, 1: at least sent all files, 2: not all files sent }
function TFidomailer.BinkPFileTransfer: byte;
type
  tState= (WaitConn,InitTransfer,Switch,Receive,Transmit,Done,ConnLost);
  tRxState= (RxWaitF,RxAccF,RxReceD,RxWriteD,RxEOB,RxDone);
  tTxState= (TxGNF,TxTryR,TxReads,TxWLA,TxDone);
  tResult= (OK,Continue,Failure);
const
  sRxState: array[tRxState]of string= ('RxWaitF','RxAccF','RxReceD','RxWriteD','RxEOB','RxDone');
  sTxState: array[tTxState]of string= ('TxGNF','TxTryR','TxReads','TxWLA','TxDone');
var
  State: tState; RxState,LastRxState: tRxState; TxState,LastTxState: tTxState;
  InBpF,OutBpF: tBinkpFrame; TheQueue: TList; PendingFiles,PendingFilesWithPath: TStringList;
  RxFilename: string; RxFileHandle,RxResult: Longint; RxSize,RxTime,RxOffset: Cardinal;
  TxFilename,TxFilenameWithPath: string; TxFileHandle,TxResult: Longint; TxSize,TxTime: Cardinal;

  function GetPar(var s: string): string;
  var p: integer;
  begin p:=pos(' ',s); if p<=0 then p:=length(s)+1; result:=copy(s,1,p-1); delete(s,1,p); end;

  function BpReceive: tResult;
  var s: string; RxFilePos: Cardinal;
  begin
    result:=OK;
    if LastRxState<>RxState then begin
      Debug.DebugLog('ncfido','binkp rxstate '+sRxState[RxState],DLDebug);
      LastRxState:=RxState;
      end;
    case RxState of
      RxWaitF: begin
        if not InBpF.IsReady then exit;
        case InBpF.FrameType of
          M_DATA: ; // ignore
          M_ERR: begin Output(mcError,'Remote reported error',[0]); RxState:=RxDone; result:=Failure; exit; end;
          M_GET,M_GOT,M_SKIP: begin
            TheQueue.Add(InBpF); InBpF:=tBinkpFrame.CreateWithCommObj(FCommObj);
            end;
          M_NUL: Log(lcInfo,InBpF.CommandPar);
          M_EOB: begin Output(mcInfo,'Finished receiving files',[0]); RxState:=RxDone; end; //** protocol violation
          M_FILE: begin
            s:=InBpF.CommandPar; RxFilename:=GetPar(s); RxSize:=StrToIntDef(GetPar(s),0);
            RxTime:=StrToIntDef(GetPar(s),0); RxOffset:=StrToIntDef(GetPar(s),0);
            RxState:=RxAccF; result:=Continue;
            end;
          end;
        InBpF.ReadFromStream(False);
        end;
      RxAccF: begin
        Output(mcVerbose,'receiving %s',[RxFilename]);
        if FileExists(IncomingDir+RxFilename)then begin // file exists
          RxFileHandle:=FileOpen(IncomingDir+RxFilename,fmOpenReadWrite);
          if RxFileHandle>0 then begin // opened ok
            RxOffset:=FileSeek(RxFileHandle,0,fsFromEnd);
            OutBpF.SendCommandFrame(M_GET,RxFilename+' '+IntToStr(RxSize)+' '+IntToStr(RxTime)+' '+IntToStr(RxOffset));
            RxState:=RxReceD;
            end else begin // open failed
            OutBpF.SendCommandFrame(M_SKIP,RxFilename+' '+IntToStr(RxSize)+' '+IntToStr(RxTime));
            RxState:=RxWaitF;
            end;
          end else begin // file does not exist
          RxFileHandle:=FileCreate(IncomingDir+RxFilename);
          if RxFileHandle>0 then begin // opened ok
            RxOffset:=0;
            RxState:=RxReceD;
          end else begin // open failed
            OutBpF.SendCommandFrame(M_SKIP,RxFilename+' '+IntToStr(RxSize)+' '+IntToStr(RxTime));
            RxState:=RxWaitF;
            end;
          RxState:=RxReceD;
          end; // ...of "file does not exist"
        end;
      RxReceD: begin
        if not InBpF.IsReady then exit;
        case InBpF.FrameType of
          M_DATA: begin
            RxState:=RxWriteD;
            RxResult:=FileWrite(RxFileHandle,InBpF.Data[1],InBpF.DataSize);
            result:=Continue
            end;
          M_ERR: begin RxState:=RxDone; result:=Failure; exit; end;
          M_GET,M_GOT,M_SKIP: begin
            TheQueue.Add(InBpF); InBpF:=tBinkpFrame.CreateWithCommObj(FCommObj);
            end;
          M_NUL: Log(lcInfo,InBpF.CommandPar);
          M_FILE: begin RxState:=RxAccF; FileClose(RxFileHandle); end; //** report partially recv file
          end;
        InBpF.ReadFromStream(False);
        end;
      RxWriteD: begin
        if RxResult<0 then begin RxState:=RxDone; result:=failure; exit; end;
        RxFilePos:=FileSeek(RxFileHandle,0,fsFromCurrent);
        Output(mcVerbose,'receiving %s %.0f%%',[RxFilename,100.0*RxFilePos/RxSize]);
        if RxFilePos>RxSize then begin RxState:=RxDone; result:=Failure; exit; end;
        if RxFilePos=RxSize then begin
          RxState:=RxWaitF; //** set file time
          FileClose(RxFileHandle);
          OutBpF.SendCommandFrame(M_GOT,RxFilename+' '+IntToStr(RxSize)+' '+IntToStr(RxTime));
          Output(mcInfo,'%s received',[RxFilename]);
          IncomingFiles.Add(IncomingDir+RxFilename);
          LogRxFile(IncomingDir+RxFilename);
          end;
        if RxFilePos<RxSize then RxState:=RxReceD;
        end;
      RxEOB,RxDone: begin // process incoming packets even if in RxDone
        if TxState=TxDone then RxState:=RxDone;
        if not InBpF.IsReady then exit;
        case InBpF.FrameType of
          M_ERR: begin RxState:=RxDone; result:=Failure; exit; end;
          M_GET,M_GOT,M_SKIP: begin
            TheQueue.Add(InBpF); InBpF:=tBinkpFrame.CreateWithCommObj(FCommObj);
            end;
          M_NUL: Log(lcInfo,InBpF.CommandPar);
          end;
        InBpF.ReadFromStream(False);
        end;
      end;
  end;

  procedure ProcessTheQueue;
  var QBpF: tBinkpFrame; s: String; iPending: Integer;
  begin
    QBpF:=TheQueue[TheQueue.Count-1];
    case QBpF.FrameType of
      M_GET: begin
        //** implement
        end;
      M_GOT: begin
        s:=QBpF.CommandPar; s:=GetPar(s);
        if s=TxFilename then begin
          FileClose(TxFileHandle);
          Output(mcError,'Remote refused file %s',[TxFilename]);
          TxState:=TxGNF;
          end else begin
          iPending:=PendingFiles.IndexOf(s);
          if iPending<>-1 then begin
            LogTxFile(PendingFilesWithPath[iPending]);
            PendingFiles.Delete(iPending);
            PendingFilesWithPath.Delete(iPending);
            Output(mcInfo,'sent %s',[s]);
            end;
          end;
        end;
      M_SKIP: begin
        s:=QBpF.CommandPar; s:=GetPar(s);
        if s=TxFilename then begin
          FileClose(TxFileHandle);
          Output(mcInfo,'Remote will accept file %s later',[TxFilename]);
          TxState:=TxGNF;
          end;
        //** add: M_SKIP file not currently transmitting
        end;
      end;
    QBpF.Destroy;
    TheQueue.Delete(TheQueue.Count-1);
  end;

  function BpTransmit: tResult;
  const BufferSize= 8192;
  begin
    result:=OK;
    if LastTxState<>TxState then begin
      Debug.DebugLog('ncfido','binkp txstate '+sTxState[TxState],DLDebug);
      LastTxState:=TxState;
      end;
    case TxState of
      TxGNF: begin
        if OutgoingFiles.Count<=0 then begin // no more files to send
          OutBpF.SendCommandFrame(M_EOB,'');
          TxState:=TxWLA; result:=Continue;
          end else begin                     // still files to send
          TxFilename:=ExtractFilename(OutgoingFiles[0]);
          TxFilenameWithPath:=OutgoingFiles[0];
          Output(mcInfo,'Sending %s',[TxFilename]);
          TxFileHandle:=FileOpen(OutgoingFiles[0],fmOpenRead);
          OutgoingFiles.Delete(0);
          if TxFileHandle>0 then begin       // opened ok
            TxSize:=FileSeek(TxFileHandle,0,fsFromEnd);
            FileSeek(TxFileHandle,0,fsFromBeginning);
            TxTime:=FileGetDate(TxFileHandle);
            OutBpF.SendCommandFrame(M_FILE,TxFilename+' '+IntToStr(TxSize)+' '+IntToStr(TxTime)+' 0');
            TxState:=TxTryR; result:=Continue;
            end else begin                   // failed to open
            TxState:=TxDone; result:=Failure;
            end;
          end;
        end;
      TxTryR: begin
        result:=continue;
        if TheQueue.Count>0 then
          ProcessTheQueue
        else
          TxState:=TxReadS;
        end;
      TxReadS: begin
        TxResult:=FileRead(TxFileHandle,OutBpF.Data[1],BufferSize);
        if TxResult<0 then begin TxState:=TxDone; result:=Failure; exit end;
        OutBpF.SetupDataFrame(TxResult); OutBpF.Send;
        if TxResult<BufferSize then begin // file end reached
          FileClose(TxFileHandle);
          TxState:=TxGNF;
          PendingFiles.Add(TxFilename);
          PendingFilesWithPath.Add(TxFilenameWithPath);
          TxFilename:=''; TxFilenameWithPath:='';
          end else begin // still data to read
          TxState:=TxTryR;
          end;
        end;
      TxWLA,TxDone: begin // pay attention to TheQueue even if in TxDone
        if TheQueue.Count>0 then begin
          ProcessTheQueue; result:=continue;
          end else
          if PendingFiles.Count<=0 then TxState:=TxDone;
        end;
      end;
  end;

  function BreakTransfer: integer;
  begin
    State:=Done;
    DebugLog('ncfido','BreakTransfer: '+IntToStr(OutgoingFiles.Count)+'!'+IntToStr(Pendingfiles.Count),DLDebug);
    if(OutgoingFiles.Count<=0)and(PendingFiles.Count<=0)then
      result:=1
    else
      result:=2;
  end;

begin
  State:=WaitConn;
  InBpF:=tBinkpFrame.CreateWithCommObj(FCommObj); OutBpF:=tBinkpFrame.CreateWithCommObj(FCommObj);
  TheQueue:=TList.Create; PendingFiles:=TStringList.Create; PendingFilesWithPath:=TStringList.Create;
  repeat
    case State of
      WaitConn: begin
        Output(mcInfo,'Initiating file transfer',[0]);
        RxState:=RxWaitF; TxState:=TxGNF;
        LastRxState:=RxWaitF; LastTxState:=TxGNF;
        State:=Switch; //** set timer
        InBpF.ReadFromStream(False);
        end;
      Switch: begin
        if (RxState=RxDone)and(TxState=TxDone)then begin
          Output(mcInfo,'File transfer complete',[0]);
          result:=0; State:=Done;
          end else if FCommObj^.CharAvail then
            State:=Receive
          else if FCommObj^.ReadyToSend(1024) or
                  // Ensure transmit routine calls ProcessTheQueue - even if connection already down
                  ((TxState=TxWLA)and(TheQueue.Count>0))then
            State:=Transmit
          else if not FCommObj^.Carrier then
            State:=ConnLost;
          //** if timeout...
        end;
      Receive: begin
          case BpReceive of
            OK: State:=Switch; //** set timer
            Failure: result:=BreakTransfer;
            Continue: ;
          end;
        end;
      Transmit: begin
          case BpTransmit of
            OK: State:=Switch; //** set timer
            Failure: result:=BreakTransfer;
            Continue: ;
          end;
        end;
      ConnLost: begin
        Output(mcError,'Connection lost',[0]);
        Log(lcError,'Connection lost');
        State:=Done; result:=BreakTransfer;
        end;
    end;
    if (KeyPressed and(ReadKey=#27))then begin
      Output(mcError,'User break',[0]);
      Log(lcError,'User break');
      result:=BreakTransfer;
      end;
  until State=Done;
  InBpF.Destroy; OutBpF.Destroy; TheQueue.Destroy; PendingFiles.Destroy; PendingFilesWithPath.Destroy;
end;


{
  $Log$
  Revision 1.4  2001/05/19 16:20:25  ma
  - implemented secure BinkP CRAM-MD5 authentication

  Revision 1.3  2001/05/09 19:37:53  ma
  - connection termination recognition changed again

  Revision 1.2  2001/04/18 11:02:13  ma
  - using StrgList.IndexOf instead of Find, Find only works on sorted
    Strglists.

  Revision 1.1  2001/03/21 19:17:09  ma
  - using new netcall routines now
  - renamed IPC to Progr.Output

}
