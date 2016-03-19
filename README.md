opam-compiler-conf
==================

A small script to create short-lived OPAM compiler switches from experimental OCaml branches

Usage:
```
bash opam-compiler-conf.sh <command>
```

Or, after `make BINDIR=~/.local/bin install`:
```
opam compiler-conf <command>
```

Here is the output of the 'help' command:
```
available commands: get-switch get-conf check-conf get-descr configure install switch reinstall remove|uninstall help

You should run this script from the directory of an OCaml compiler
source that you wish to install as an OPAM switch. It was designed for
short-lived experiments with experimental branches of the compiler, so
it infers the switch name from DCVS information (only git is
currently supported).

The workflow should be as follows, from the root directory of an
OCaml source tree:

    opam compiler-conf configure
    make world.opt # or any way to build the compiler
    opam compiler-conf install # gives you a new switch

If you already have a switch for this compiler branch installed, then
you should note that

    opam compiler-conf reinstall

will recompile the OPAM packages in this switch, while just running

    make install

will simply overwrite the compiler's installation. This can save time
when you know the compiler change will not affect package compilation
in any way.


The full list of commands is the following:

get-switch: returns the name of the OPAM switch inferred from DCVS information

get-conf:   returns the OPAM configuration file inferred

get-descr:  returns the OPAM description file inferred

configure:  runs the ./configure script of the OCaml distribution
            (OCaml needs to be told at ./configure time where it will
            be installed, and we handle this for you, knowing where
            OPAM will expect its stuff); you can pass it arguments
            expected by the configure script (eg. --no-tk)

install:    setups the OPAM switch and install it (will run 'make install')
            you need to have compiled the distribution first

switch:     switches to this new OPAM compiler (you'll still need to setup env)

reinstall:  reinstall the OPAM switch (useful if you changed the compiler source)

uninstall:  removes the OPAM switch and its configuration

check-conf: checks that the last configured switch agrees with the
            current DCVS state (branch). This is useful if you have
            played with other branches (and thus other switches) and
            don't remember whether you should reconfigure before
            recompiling.

get-paths:  returns inferred paths (for debugging purposes)

help:       this message


For configuration, the script supports the following environment
variables:

BASE_PACKAGES: Specifies the list of base-packages that a compiler
  description should list. It is a space-separated list of words.

  Default: "base-unix base-bigarray base-threads".

FORCE_BRANCH: Overrides the detection of the branch name to specify
  any name you want (it should be an alphanumeric word with
  no spaces). For example, this is useful when you are checked out of
  your branch but still want to reinstall the current state at the
  same switch.

FORCE_VERSION: Overrides the compiler-version detection.

OPAM: the opam command to use.

  Default: "opam".

```