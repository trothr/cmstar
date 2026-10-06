# Commands in CMS TAR

CMS TAR mimics Traditional `tar` and recognizes a single-token "command"
followed by one or more arguments. Some refer to this style as a cluster
of *option* letters.

As of the current release, CMS TAR does *not* support separation of the
"components" in the so-called cluster. CMS TAR also does not support the
long-option style (double dash style) found in TAR implementations
such as Gnu TAR. CMS TAR also does not support compression because
there is, as yet, no reliable way to get compression in CMS Pipelines.

A leading dash is not required and is ignored if present.

## Detailing CMS TAR Commands

* create

The "c" command invokes a create operation.

    tar cf bdisk * * b

This will create `BDISK TAR` on filemode "A"
containing all files found on your "B" disk.

* extract

The "x" command invokes an extract operation.

    tar xf imported

This will extract the files from `IMPORTED TAR` (on any filemode).
The files will be placed at filemode "A".

* list (table of contents)

The "t" command invokes a list operation (of the "table of contents").

    tar tf arcfile

This will list the files in `ARCFILE TAR`.
`ARCFILE TAR` is presumably RECFM=F and LRECL=512, but `tar` does not
restrict files which it reads to any particular format, only that they
are binary.

* "f" for file

An "f" in the command means that a filename follows.
This is illustrated in the examples above.

CMS TAR uses a single blank-delimited token for the archive filename.
The filetype is `tar`. If the name is a single numeric digit then it
indicates a tape device.

* "v" for verbose

A "v" in the command makes the operation verbose.
Creation and extraction will then list the files archived or extracted.

* "s" for spooled or spooling

An "s" in the command is a special function unique to CMS TAR.
Files will come from z/VM spool space (thus the "name" is a spool ID).
When creating an archive, "s" means "send" and the resulting archive
will be sent to other users (via spool space or via UFT protocol).

## Special CMS TAR Features

* spooling

When creating an archive,
"s" indicates that what follows is a target in "user@node" format.
If the `uftchost` stage is available, `tar` will attempt to use it.
Otherwise, `tar` will "punch" the archive and will route it via RSCS
if appropriate.

When extracting or listing an archive,
"s" indicates that what follows is a spool file (the "spool ID",
a one-to-four digit number).

* tarlist

The `tarlist` command presents a list of files in the archive
in similar fashion to `filelist` for ordinary CMS files and
`rdrlist` for spool files. Function keys PF9 and PF11 are set to
"receive" and "peek" respectively.


