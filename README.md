emacs-24.3.d
============

RHEL7 and derivative Linux distributions like CentOS7 ship with Emacs
24.3.

My other Emacs configuration repository moved forward without concern
about maintaining backwards compatibility with Emacs 24.3 because, at
the time, it was convenient enough for me to compile more a more
recent version of Emacs and run that from a central network location.

Lately I've had to work on machines without a networked home drive,
and copying around my own build of Emacs is inconvenient.

So, this is a minimal configuration I can quickly clone onto a RHEL7
machine.

Git submodules are used instead of ELPA/MELPA in this configuration.
To check them out, after cloning, run:

    git submodule update --init

