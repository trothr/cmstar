/*
 *        Name: TARREADC REXX
 *              read a tar "deck" in the reader
 *              Copyright 1992, Richard M. Troth
 */

/* get the name of the file if any */
Parse Arg tf . '(' . ')' .

/* if tarfile is a URL then punt to 'curl' */
If POS("://",tf) > 0 Then Do
  'CALLPIPE curl --binary' tf '| *:'
  Exit rc
End

/* define a disposable virtual reader */
rdr = "DC0"
Address "COMMAND" 'GETFMADR 200'
If rc = 0 Then Parse Pull . . rdr .
Parse Value DiagRC(08,'DEFINE READER' rdr) With 1 rc 10 . 17 rs '15'x .
If rc ^= 0 Then Do
  If rs ^= "" Then Say rs
  Exit rc
End

/* put a hold on this reader so that the file is not consumed */
Call Diag 08, 'SPOOL' rdr 'HOLD'
Call Diag 08, 'CLOSE' rdr

If tf ^= "-" Then Do
  'CALLPIPE CP ORDER READER' tf '| VAR RS'
  If rc ^= 0 Then Do
    orc = rc
    'OUTPUT' rs
    Call Diag 08, 'CLOSE' rdr
    Call Diag 08, 'DETACH' rdr
    Exit orc
  End /* If .. Do */
End /* If .. Do */

/* read the spool file and deliver to output */
'CALLPIPE READER' rdr ,
  '| NLOCATE 1-1 /' || '03'x || '/' ,
  '| SPEC 2-* 1' ,
  '| PAD 80 40 ' ,
  '| *:'

/* close and detach the temporary reader */
Call Diag 08, 'CLOSE' rdr
Call Diag 08, 'DETACH' rdr

Exit rc


