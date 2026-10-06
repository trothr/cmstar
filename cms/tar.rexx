/* © Copyright 1992, 1995, Richard M. Troth, all rights reserved.
 *              (casita sourced) <plaintext>
 *
 *        Name: TAR REXX
 *              a from-scratch replacement for CMS 'tar' v1
 *      Author: Rick Troth, Houston, Texas, USA
 */

vrm = "2.4.1"
Numeric Digits 16

/* ASCII non-printables */
a_nprint = '00010203040506'x
/* EBCDIC non-printables */
e_nprint = '0001020304'x

/* some defaults */
tar.uid = 1
tar.gid = 1

Parse Source . . . . . arg0 .
argo = arg0 || ':'

tc = ""     /*  primary operation  (tar command)  */
tf = ""     /*  archive file  (tar file)  */
td = ""     /*  archive device  (disk, tape, or SPOOL)  */

verbose = 0
modtime = 1
prmopt = 0
include = ""
tarlist = 0
skip = 0
peek = 0
once = 0
replace = 0

/* parse command-line options */
Parse Arg args "(" opts ")" .

Parse Var args arg1 .
Do While Left(arg1,2) = "--"
  Parse Var args . args
  Select
    When Abbrev("--version",arg1,5) Then Do
/*    Say "CMS TAR - Version" vrm "(piped)"    */
/*    Say "tar (CMS tar)" vrm    */
      Say "CMS TAR" vrm "(piped)"
      Exit
    End
    Otherwise Do
      Address "COMMAND" 'XMITMSG 3 ARG1 (ERRMSG'
      Exit 24
    End
  End /* Select */
  Parse Var args arg1 .
End

Parse Var args cmd args
Upper cmd

Do While cmd ^= ""
    Parse Var cmd 1 c 2 cmd
    Select  /*  c  */
        When c = '-' Then nop
        When c = 'C' Then Do
            If tc ^= "" Then Do
                Address "COMMAND" 'XMITMSG 66 TC C (ERRMSG'
                Say argo "multiple primary operations."
                Exit 24
                End  /*  If  ..  Do  */
            tc = c
            End  /*  When  Do  */
        When c = 'X' Then Do
            If tc ^= "" Then Do
                Say argo "multiple primary operations."
                Exit 24
                End  /*  If  ..  Do  */
            tc = c
            End  /*  When  Do  */
        When c = 'T' Then Do
            If tc ^= "" Then Do
                Say argo "multiple primary operations."
                Exit 24
                End  /*  If  ..  Do  */
            tc = c
            End  /*  When  Do  */
        When c = 'R' Then Do
            If tc ^= "" Then Do
                Say argo "multiple primary operations."
                Exit 24
                End  /*  If  ..  Do  */
            tc = c
            End  /*  When  Do  */
        When c = 'F' Then Do
            If tf ^= "" Then Do
                Say argo "multiple archives specified."
                Exit 24
                End  /*  If  ..  Do  */
            Parse Var args tf args
            td = 'F'
            End  /*  When  Do  */
        When c = 'S' Then Do
            If tf ^= "" Then Do
                Say argo "multiple archives specified."
                Exit 24
                End  /*  If  ..  Do  */
            Parse Var args tf args
            td = 'S'
            End  /*  When  Do  */
        When c = '0' | c = '1' | c = '2' | c = '3' ,
             c = '4' | c = '5' | c = '6' | c = '7' Then Do
            If tf ^= "" Then Do
                Say argo "multiple archives specified."
                Exit 24
                End  /*  If  ..  Do  */
            tf = "TAP" || c
            td = 'T'
            End  /*  When  Do  */
        When c = 'V' Then Do
          verbose = 1
          Say "CMS TAR - Version" vrm "(piped)"
        End  /*  When  Do  */
        When c = 'M' Then modtime = 0
        When c = 'W' Then prompt = 1
        Otherwise Do
          Address "COMMAND" 'XMITMSG 3 C (ERRMSG'
          Say argo "unrecognized command token" c
          Exit 24
        End  /*  Otherwise  Do  */
        End  /*  Select  c  */
    End

If tf = "" Then tf = "TAP1"
If td = "" Then td = "T"

If Left(tf,1) = "*" Then Do
  Address "COMMAND" 'GLOBALV SELECT TARLIST GET TARFILE'
  If rc = 0 & tarfile ^= "" Then tf = tarfile
End

/* if tarfile is a URL then handle it like a spool file */
If POS("://",tf) > 0 Then td = 'S'

Do While opts ^= ""
  Parse Var opts op opts
  Upper op
  Select /* op */
    When Abbrev("TARLIST",op,4)     Then tarlist = 1
    When Abbrev("NOTARLIST",op,3)   Then tarlist = 0
    When Abbrev("INCLUDE",op,3)     Then Parse Var opts include opts
    When Abbrev("SKIP",op,1)        Then Parse Var opts skip opts
    When Abbrev("PEEK",op,2)        Then Do; peek = 1; once = 1; End
    When Abbrev("ONCE",op,1)        Then once = 1
    When Abbrev("VERBOSE",op,1)     Then verbose = 1
    When Abbrev("TERSE",op,5)       Then verbose = 0
    When Abbrev("MODTIME",op,1)     Then modtime = 1
    When Abbrev("NOMODTIME",op,3)   Then modtime = 0
    When Abbrev("PROMPT",op,2)      Then prompt = 1
    When Abbrev("NOPROMPT",op,3)    Then prompt = 0
    When Abbrev("REPLACE",op,3)     Then replace = 1
    When Abbrev("NOREPLACE",op,3)   Then replace = 0
    Otherwise Do
      Address "COMMAND" 'XMITMSG 3 OP (ERRMSG'
      Exit 24
    End /* Otherwise Do */
  End /* Select op */
End /* Do While */

Select  /*  tc  */

    When tc = 'C' Then Do
        Select  /*  td  */
            When td = 'F' Then Do
                If tf ^= "-" Then Do
                    Parse Var tf tfn '.' tft '.' tfm '.' .
                    If tft = "" Then tft = "TAR"
                    If tfm = "" Then tfm = "A"
                    'ADDPIPE *.OUTPUT: | >' tfn tft tfm 'F 512'
                    End  /*  If  ..  Do  */
                Call CREATE
                End  /*  When  ..  Do  */
            When td = 'T' Then Do
                'ADDPIPE *.OUTPUT: | TAPE' tf
                Call CREATE
                End  /*  When  ..  Do  */
            When td = 'S' Then Do
                'ADDPIPE *.OUTPUT: | TARPUNCH' tf
                Call CREATE
                End  /*  When  ..  Do  */
            Otherwise Do
                Say argo "internal error: unknown TAR target" td tf
                End  /*  Otherwise  Do  */
            End  /*  Select  td  */
        End  /*  When  ..  Do  */

    When tc = 'X' Then Do
        Select  /*  td  */
            When td = 'F' Then Do
                If tf ^= "-" Then Do
                    Parse Var tf tfn '.' tft '.' tfm '.' .
                    If tft = "" Then tft = "TAR"
                    'ADDPIPE <' tfn tft tfm '| *.INPUT:'
                    End  /*  If  ..  Do  */
                Call XTRACT
                End  /*  When  ..  Do  */
            When td = 'T' Then Do
                'CALLPIPE CMS TAPE REW (' tf    /*  not quite right  */
                'ADDPIPE TAPE' tf '| *.INPUT:'
                Call XTRACT
                'CALLPIPE CMS TAPE REW (' tf    /*  not quite right  */
                End  /*  When  ..  Do  */
            When td = 'S' Then Do
                'ADDPIPE TARREADC' tf '| *.INPUT:'
                Call XTRACT
                End  /*  When  ..  Do  */
            Otherwise Do
                Say argo "internal error: unknown TAR source" td tf
                End  /*  Otherwise  Do  */
            End  /*  Select  td  */
        End  /*  When  ..  Do  */

    When tc = 'T' Then Do
        Select  /*  td  */
            When td = 'F' Then Do
                If tf ^= "-" Then Do
                    Parse Var tf tfn '.' tft '.' tfm '.' .
                    If tft = "" Then tft = "TAR"
                    'ADDPIPE <' tfn tft tfm '| *.INPUT:'
                    End  /*  If  ..  Do  */
                Call LISTOC
                End  /*  When  ..  Do  */
            When td = 'T' Then Do
                'CALLPIPE CMS TAPE REW (' tf    /*  not quite right  */
                'ADDPIPE TAPE' tf '| *.INPUT:'
                Call LISTOC
                'CALLPIPE CMS TAPE REW (' tf    /*  not quite right  */
                End  /*  When  ..  Do  */
            When td = 'S' Then Do
                'ADDPIPE TARREADC' tf '| *.INPUT:'
                Call LISTOC
                End  /*  When  ..  Do  */
            Otherwise Do
                Say argo "internal error: unknown TAR source" td tf
                End  /*  Otherwise  Do  */
            End  /*  Select  td  */
        End  /*  When  ..  Do  */

    End  /*  Select  tc  */

Exit rc * (rc ^= 12)


/* ---------------------------------------------------------------------
 *  create or update
 */
CREATE:

If include = "" Then 'ADDPIPE TARINDEX' args '| *.INPUT:'
                Else 'ADDPIPE <' include 'FILELIST | *.INPUT:'

userid = Userid()
groupid = "nogroup" /* vmgroup(userid) */
'CALLPIPE VAR USERID   | XLATE LOWER | VAR USERID'
'CALLPIPE VAR GROUPID  | XLATE LOWER | VAR GROUPID'

Do Forever

    'READTO RECORD'
    If rc ^= 0 Then Leave
    If Strip(record) = "" Then Iterate
    If Left(record,1) = '*' Then Iterate

    Parse Upper Var record fn ft fm  .   '(' opts ')' .
    Parse       Var record .  .  .  name '('  .   ')' .
    Parse Var name '"' . "'" . name
    name = Strip(name)
    If name = "" Then ,
        'CALLPIPE LITERAL' Strip(fn) || '.' || Strip(ft) ,
            '| XLATE LOWER | STRIP | VAR NAME'

    'CALLPIPE COMMAND LISTFILE' fn ft fm ,
        '(FULLDATE | DROP | VAR FILESPEC'
    If rc /= 0 Then Iterate

    Parse Var filespec . . fmode recfm lrecl . . date time .
    fmode = Right(fmode,1)
    Select  /*  fmode  */
        When fmode = 0 Then perm = '600'
        When fmode = 1 Then perm = '644'
        When fmode = 2 Then perm = '644'
        When fmode = 3 Then perm = '444'
        When fmode = 4 Then perm = '644'
        When fmode = 5 Then perm = '644'
        When fmode = 6 Then perm = '666'
        Otherwise           perm = '644'
        End  /*  Select  fmode  */

    If recfm = 'V' Then lrecl = 0

    'CALLPIPE <' fn ft fm '| TAKE FIRST 1 | VAR SAMPLE'
    If Verify(sample,e_nprint,'M') = 0 Then trans = 't'
                                       Else trans = 'b'

    Select
        When trans = 't' Then
            pipe = '| STRIP TRAILING | E2A | SPEC 1-* 1 .0A. X2C NEXT'
        When lrecl = 0 Then
            pipe = '| BLOCK 512 CMS'
        Otherwise
            pipe = ""
        End  /*  Select  */

    'CALLPIPE <' fn ft fm pipe '| COUNT BYTES | VAR SIZE'

    Call MKTARENT

    'CALLPIPE VAR TARENT | E2A | *:'
    'CALLPIPE <' fn ft fm pipe '| FBLOCK 512 00 | *:'
    If verbose Then Say "a" name || "," size "bytes," trans lrecl

    End  /*  Do  Forever  */

/*  a trailer of nulls  */
'OUTPUT' Copies('00'x,512)
'OUTPUT' Copies('00'x,512)

Return


/* ---------------------------------------------------------------------
 *  extract
 */
XTRACT:

'ADDPIPE *: | FBLOCK 512 | *.INPUT:'
'CALLPIPE *: | TAKE' skip '| HOLE'

Parse Var args xtf xfn xft xfm .
If xfn = "" Then xfn = "="
If xft = "" Then xft = "="
If xfm = "" Then xfm = "A"

Do Forever

    'PEEKTO'
    If rc ^= 0 Then Leave

    'CALLPIPE *: | TAKE 1 | A2E | VAR RECORD'
    Call EXTARENT
    If size = 0 & name = "" Then Leave
    If size = 0 Then Iterate
    If name ^= xtf & xtf ^= '*' & xtf ^= '' Then Do
        'CALLPIPE *: | TARTAKE' size '| HOLE'
        Iterate
        End  /*  If  ..  Do  */

    Parse Value Reverse(name) With basename '/' .
    basename = Reverse(basename)
    Parse Upper Var basename fn '.' ft '.' .
    If fn = "" Then fn = Userid()
    If xfn ^= "=" Then fn = xfn
    If ft = "" Then ft = "$"
    If xft ^= "=" Then ft = xft
    fm = xfm
    filespec = fn ft fm

    If ^peek Then Do
        'CALLPIPE STATE' filespec
        If rc = 0 Then Do
            If ^replace Then Do
                If verbose Then ,
                    Say "x" name || "," size "bytes," ,
                        (size+511)%512 "tape blocks"
                Address "COMMAND" 'XMITMSG 24 FILESPEC (CALLER TAR'
                Leave
                End  /*  If  ..  Do  */
            Else Address "COMMAND" 'ERASE' filespec
            End  /*  If  ..  Do  */
        End  /*  If  ..  Do  */

    If trans ^= 'T' & trans ^= 'B' Then Do
        'PEEKTO RECORD'
        If size < 512 Then record = Left(record,size)
        If Verify(record,a_nprint,'M') = 0 Then Do
            trans = 'T'
            lrecl = 0
            End  /*  If  ..  Do  */
        Else Do
            trans = 'B'
            If size < 65536 Then lrecl = size
                            Else lrecl = 512
            End  /*  Else  Do  */
        End  /*  If  ..  Do  */

    Select
        When trans = 'T' & lrecl = 0 Then
            pipe = 'DEBLOCK LINEND 0A | STRIP TRAILING 0D' ,
                '| DROP LAST | A2E | PAD 1'
        When trans = 'T' & lrecl > 0 Then
            pipe = 'DEBLOCK LINEND 0A | STRIP TRAILING 0D' ,
                '| DROP LAST | A2E | PAD' lrecl
        When trans = 'B' & lrecl = 0 Then
            pipe = 'DEBLOCK CMS'
        When trans = 'B' & lrecl > 0 Then
            pipe = 'FBLOCK' lrecl '00'
        End  /*  Select  */

    If lrecl = 0 Then fix = ""
                 Else fix = "FIXED" lrecl

    If verbose Then ,
        Say "x" name || "," size "bytes," ,
                (size+511)%512 "tape blocks, as" ,
                    filespec || fmode trans fix
    'CALLPIPE' ,
        '*: | TARTAKE' size '|' pipe '| > TAR CMSUT1' fm || '3' fix

    If peek Then Do
        Address "COMMAND" 'MAKEBUF'
        Push "COMMAND MSG x" name || "," size "bytes," ,
                (size+511)%512 "tape blocks"
        Push "COMMAND SET FN" fn
        Push "COMMAND SET FT" ft
        Push "COMMAND SET FM" fm || fmode
        Address "COMMAND" 'XEDIT TAR CMSUT1' fm
        Address "COMMAND" 'DROPBUF'
        End  /*  If  ..  Do  */
    Else Do
        Address "COMMAND" 'RENAME TAR CMSUT1' fm filespec || fmode
        Address "COMMAND" 'DMSPLU' filespec date time
        End  /*  Else  Do  */

    If once Then Leave
    If xtf ^= '*' & xtf ^= '' Then Leave

    End  /*  Do  Forever  */

Return


/* ---------------------------------------------------------------------
 *  list table of contents
 */
LISTOC:

'ADDPIPE *: | FBLOCK 512 | *.INPUT:'
'CALLPIPE *: | TAKE' skip '| HOLE'

/* if no other output, attach console */
'STREAMSTATE OUTPUT'
If rc = 4 Then rc = 0
If rc = 8 Then rc = 0
If rc = 12 Then 'ADDPIPE *.OUTPUT: | CONSOLE'
If rc /= 0 Then Exit rc

Do Forever

    'PEEKTO'
    If rc ^= 0 Then Leave

    'CALLPIPE *: | TAKE 1 | A2E | VAR RECORD'
    Call EXTARENT
    If size = 0 & name = "" Then Leave
    If POS("/",date) > 0 Then date = plu2std(date)

    If size > 0 Then Do
      Select
        When tarlist  Then 'OUTPUT' "      " || Left(name,44) ,
          Right(size,8) Right(date,10) Right(time,8) ,
          Right(skip,8) name /* trans recfm lrecl fmode */
        When verbose  Then 'OUTPUT' Left(name,42) '-' ,
          Right(date,10) Right(time,8) Right(size,8) "bytes."
        Otherwise          'OUTPUT' name
      End /* Select */

        take = Trunc((size + 511) / 512)
        'CALLPIPE *: | TAKE' take '| HOLE'
        skip = skip + take
        End  /*  If  ..  Do  */
    skip = skip + 1

    If args ^= "" Then Leave

    End  /*  Do  Forever  */

Return


/* ------------------------------------------------------------ EXTARENT
 *  Extract TAR entry (directory info) values.
 *  Sets: size, and other variables.
 */
EXTARENT:
Parse Var record 1 name '00'x .
record = Translate(record,' ','00'x)
Parse Upper Var record 101 perm . ,
                       125 size date chksum . trans lrecl fmode . ,
                       257 . ,
                       385 .
size = o2d(size)                /* convert to decimal */
If size > 0 Then Do
    Parse Value sysdate(o2d(date)) With date time .
    chksum = o2d(chksum)        /* convert to decimal */
    lrecl = o2d(lrecl)          /* convert to decimal */
    If lrecl = 0 Then recfm = 'V'
                 Else recfm = 'F'
    If ^Datatype(fmode,'N') Then fmode = '1'
    End  /*  If  ..  Do  */

Return


/* ------------------------------------------------------------------ */
O2D:        Procedure   /*  Octal to Decimal conversion  */
Parse Arg o
d = 0
Do While o ^= ""
    Parse Var o 1 c 2 o
    If Datatype(c,'N') Then d = d * 8 + c
    End  /*  Do  While  */
Return d


/* ------------------------------------------------------------------ */
D2O:        Procedure   /*  Decimal to Octal conversion  */
Parse Arg d
/* Say "D2O:" d */
If ^Datatype(d,'N') Then d = 0
d = trunc(d)
If d < 1 Then Return 0
o = ""
Do While d ^= 0
    o = d // 8 || o
    d = d % 8
    End  /*  Do While  */
Return o


/* ------------------------------------------------------------- TARDATE
 *  Convert local CMS time and date stamp to POSIX time value.
 *  This routine originally contained its own time logic
 *  but now punts to the 'DATECONVERT' stage in CMS Pipelines.
 */
TARDATE:  Procedure
Parse Arg date time . , .
If POS("-",date) > 0 Then idfmt = "ISODATE"
                     Else idfmt = "FULLDATE"

/* convert to POSIX for easier arithmetic */
idate = date time
'CALLPIPE VAR IDATE | DATECONVERT' idfmt 'POSIX | VAR PDATE'
If rc /= 0 Then Return 0

/* return POSIX time value */
Return C2D(pdate) /* - tzoffset("S") */


/* ------------------------------------------------------------- SYSDATE
 *  Convert POSIX time in archive to local CMS time and date stamp.
 *  This routine originally contained its own time logic
 *  but now punts to the 'DATECONVERT' stage in CMS Pipelines.
 */
SYSDATE:  Procedure
Parse Arg base . , .

/* render it in binary form */
zdate = D2C(base,8)

/* convert back to FULLDATE and include time */
'CALLPIPE VAR ZDATE | DATECONVERT POSIX FULLDATE TIMEOUT | VAR RS'
If rc /= 0 Then Do
  If rs /= "" & rs /= "RS" Then Say rs
  Return Date("S") Time()
End

/* slice off fractional time and return a usable stamp */
Parse Var rs rd rt .
Parse Var rt rt "." .
Return rd rt


/* ------------------------------------------------------------ MKTARENT
 *  Create a TAR entry (directory info) from values.
 */
MKTARENT:

tarent   =  Left(name,100,'00'x) || ,
        Right(perm,6,"0") || " " || '00'x || ,
     Right(tar.uid,6,"0") || " " || '00'x || ,
     Right(tar.gid,6,"0") || " " || '00'x || ,
  Right(d2o(size),11,"0") || " " || ,
Right(d2o(tardate(date time)),11,"0") || " " || ,
                      "        " || ,
                             "0" || ,
                             '00'x

/*
            Right(trans,2) ,
            Right(d2o(lrecl),8) ,
            Right(fmode,1) || '00'x
 */

'CALLPIPE VAR TARENT | E2A | VAR TARENT'
chksum = 0
Do i = 1 to Length(tarent)
  chksum = chksum + c2d(Substr(tarent,i,1))
End
/* chksum = chksum + 16 */

tarent   =  Left(name,100,'00'x) || ,
        Right(perm,6,"0") || " " || '00'x || ,
     Right(tar.uid,6,"0") || " " || '00'x || ,
     Right(tar.gid,6,"0") || " " || '00'x || ,
  Right(d2o(size),11,"0") || " " || ,
Right(d2o(tardate(date time)),11,"0") || " " || ,
 Right(d2o(chksum),6,"0") || '00'x || " " || ,
                             "0" || ,
                             '00'x

/*
            Right(trans,2) ,
            Right(d2o(lrecl),8) ,
            Right(fmode,1) || '00'x
 */

tarent = Left(tarent,512,'00'x)

Return

tarent02 = '00'x || "ustar" || '00'x || "00" || ,
    Left(userid,32,'00'x) || Left(groupid,32,'00'x) || ,
           "0000000" || '00'x || "0000000" || '00'x

Return

/*

struct posix_header
{                               /* byte offset */
# char name[100];               /*   0 */
# char mode[8];                 /* 100 */
# char uid[8];                  /* 108 */
# char gid[8];                  /* 116 */
# char size[12];                /* 124 */
# char mtime[12];               /* 136 */
# char chksum[8];               /* 148 */
  char typeflag;                /* 156 */
  char linkname[100];           /* 157 */
  char magic[6];                /* 257 */
  char version[2];              /* 263 */
  char uname[32];               /* 265 */
  char gname[32];               /* 297 */
  char devmajor[8];             /* 329 */
  char devminor[8];             /* 337 */
  char prefix[155];             /* 345 */
                                /* 500 */
};

 */






/* ------------------------------------------------------------ TZOFFSET
 *  Compute timezone offset based on timezone string from 'CP Q TIME'.
 *  (we probably have a CSL routine to do this ... but maybe not)
 */
tzoffset: Procedure

Parse Upper Arg denom . ',' . , .
Parse Upper Value Diag(08,'QUERY TIME') With . . . tz .

Select /* tz */
  When tz = "PST" Then zo = -8
  When tz = "PDT" Then zo = -7
  When tz = "MST" Then zo = -7
  When tz = "MDT" Then zo = -6
  When tz = "CST" Then zo = -6
  When tz = "CDT" Then zo = -5
  When tz = "EST" Then zo = -5
  When tz = "EDT" Then zo = -4
  When tz = "CET"  Then zo = 1
  When tz = "CEDT" Then zo = 2
  When tz = "CEST" Then zo = 2
  When tz = "EET"  Then zo = 2
  When tz = "EEDT" Then zo = 3
  When tz = "EEST" Then zo = 3
  When tz = "WET"  Then zo = 0
  When tz = "WEDT" Then zo = 1
  When tz = "WEST" Then zo = 1
  /* FEED ME: need more timezones, duh */
  When tz = "GMT"  Then zo = 0
  When tz = "UTC"  Then zo = 0
  Otherwise zo = 0
End /* Select tz */

denom = Left(denom,1)
Select /* denom */
  When denom = "S" Then Return zo * 60 * 60      /* offset in seconds */
  When denom = "M" Then Return zo * 60           /* offset in minutes */
  Otherwise Return zo
End /* Select denom */


/* ---------------------------------------------------------------------
 */
plu2std: Procedure
Parse Arg d . , .
Parse Var d mon "/" day "/" year
Return year || "-" || mon || "-" || day


