program openxp;

{$I xpdefine.inc }

uses uuzng,
  xpcharset,
  uuzng_maus,
  uuzng_zc;

var   
  InSpool : TNetcallSpoolIn;
  OutSpool: TNetcallSpoolOut;

begin
  OutSpool := TMausTauschSpoolOut.Create('TESTBUF.OUT');

  InSpool := TZCSpoolIn.Create('TESTBUF.ZER');
  InSpool.CopyTo(OutSpool);

  OutSpool.Free;
  InSpool.Free;
end.

