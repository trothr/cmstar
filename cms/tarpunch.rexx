/* © Copyright 1995-2026, Richard M. Troth, all rights reserved. <plaintext>
 *
 *        Name: TARPUNCH REXX (CMS Pipelines "gem" in Rexx)
 *              This program is part of the CMS TAR package.
 *              "punch" a tar deck to another user
 *              Copyright 1992, 1995, Richard M. Troth
 */

Parse Arg target nameit . '(' . ')' .

/* if target is a dash, feed to punch w/o closing */
If target = '-' Then Do
  'CALLPIPE *.INPUT: | FBLOCK 80 00 | PUNCH'
  Exit rc
End /* If .. Do */

/* okay, so we really want to send this */
'CALLPIPE COMMAND IDENTIFY | VAR IDENTITY'
Parse Var identity userid . hostid . rscsid . '15'x .
Parse Var target user '@' host
If user = "" Then user = userid
If host = "" Then host = hostid

/* careful! this is a raw UFT "batch" job */
Address "COMMAND" 'STATE UFTCHOST REXX *'
If rc = 0 Then Do
  'ADDPIPE *.OUTPUT: | UFTCHOST' host '| *.OUTPUT:'
  If rc ^= 0 Then Exit rc
  'CALLPIPE VAR USERID | XLATE LOWER | VAR USERID'
  'OUTPUT FILE 0' userid '-'
  'OUTPUT USER' user
  'OUTPUT TYPE I'
  If nameit ^= "" Then 'OUTPUT NAME' nameit
  'OUTPUT DATA'
  'CALLPIPE *: | *:'
End /* If .. Do */

/* if that didn't work then punt to RSCS */
Else Do
  Address "COMMAND" 'GETFMADR'
  If rc ^= 0 Then Exit rc
  Parse Pull . . tmp .
  Call Diag 08, 'DEFINE PUNCH' tmp
  If rc ^= 0 Then Exit rc
  Call Diag 08, 'TAG DEV' tmp host user '50'
  Call Diag 08, 'SPOOL' tmp 'TO' rscsid
  'CALLPIPE *.INPUT: | FBLOCK 80 00 | SPEC x41 1 1-* NEXT | URO' tmp
  If nameit ^= "" Then Do
    Parse Upper Var nameit fn "." ft "."
    Call Diag 08, 'CLOSE' tmp 'NAME' fn ft
  End ; Else Call Diag 08, 'CLOSE' tmp
  Call Diag 08, 'DETACH' tmp
End /* Else Do */

Exit


