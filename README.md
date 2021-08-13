emacs-24.3.d
============

About
-----

RHEL7 and derivative Linux distributions like CentOS7 ship with Emacs
24.3.

My other Emacs configuration repository moved forward without concern
about maintaining backwards compatibility with Emacs 24.3 because, at
the time, it was convenient enough for me to compile a more recent
version of Emacs and run that from a central network location.

Lately I've had to work on machines without a networked home drive,
and copying around my own build of Emacs is inconvenient.

So, this is a minimal configuration I can quickly clone onto a RHEL7
machine.

Submodules
----------

Git submodules are used instead of ELPA/MELPA in this configuration.
To check them out, after cloning, run:

    git submodule update --init

Running
-------

Running the script `emacs24.sh` will change the value of `$HOME` to
the parent directory of your checkout before running /usr/bin/emacs.
There are other ways to change the source of your init files, but this
serves well enough for my purposes.  See also: [How to start emacs
with a custom
user-emacs-directory](https://emacs.stackexchange.com/questions/4253)

To use this repository, you might do something like:

    mkdir -p $HOME/emacs24/master
    git clone https://github.com/jlucas/emacs-24.3.d $HOME/emacs24/master/.emacs.d

Then run:

    $HOME/emacs24/master/.emacs.d/emacs24.sh

To run without having to type the full path, add an alias to your
shell initialization files (e.g. `~/.bashrc`) like:

    alias e24=$HOME/emacs24/master/.emacs.d/emacs24.sh

After which you could just run:

    e24

