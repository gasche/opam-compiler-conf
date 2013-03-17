opam-compiler-conf
==================

A small script to create short-lived OPAM compiler switches from experimental OCaml branches

Usage:
```
sh opam-compiler-conf.sh <command>
```

Here is the output from the 'help' command:
```
available commands: get-switch get-conf configure install switch reinstall remove|uninstall help

You should run this script from the directory of an OCaml compiler
source that you wish to install as an OPAM switch. It was designed for
short-lived experiments with experimental branches of the compiler, so
it infers the switch name from DCVS information (only git is
currently supported).

get-switch: returns the name of the OPAM switch inferred from DCVS information

get-conf:   returns the OPAM configuration file inferred

configure:  runs the ./configure script of the OCaml distribution
            (OCaml needs to be told at ./configure time where it will
             be installed, and we handle this for you, knowing where
             OPAM will expect its stuff)

install:    setups the OPAM switch and install it (will run 'make install')
            you need to have compiled the distribution first 

switch:     switches to this new OPAM compiler (you'll still need to setup env)

reinstall:  reinstall the OPAM switch (useful if you changed the compiler source)

uninstall:  removes the OPAM switch and its configuration

help:       this message
```