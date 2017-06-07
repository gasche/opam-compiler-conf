#!/bin/bash

#the "opam" command to use
if [ -z "$OPAM" ]
then
    OPAM=opam
fi

# the path to the OPAM installation
OPAMDIR=$(opam config var root)
if [ -z "$OPAMDIR" ]
then
   echo
   echo "Error: could not determine the opam root using 'opam config var root'."
   echo "This may happen if you are on an switch that does not exist or was removed,"
   echo "for example a switch created by this script then uninstalled."
   echo "You should switch back to a working opam state before running this script."
   echo "Exiting."
   exit 2
fi

USAGE="available commands:\
  {get,show}-switch {get,show}-conf {get,show}-descr {get,show}-paths\
  check-conf\
  configure install switch reinstall {remove,uninstall}\
  help"

if [ -z "$BASE_PACKAGES" ]
then
    BASE_PACKAGES="base-unix base-bigarray base-threads"
fi

case $1 in
    ""|help)
        echo $USAGE
        echo
        cat <<EOF
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

show-switch:
get-switch: returns the name of the OPAM switch inferred from DCVS information

show-conf:
get-conf:   returns the OPAM configuration file inferred

show-descr:
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

show-paths:
get-paths:  show inferred paths (for debugging purposes)

help:       this message


For configuration, the script supports the following environment
variables:

BASE_PACKAGES: Specifies the list of base-packages that a compiler
  description should list. It is a space-separated list of words.

  Default: "base-unix base-bigarray threads".

FORCE_BRANCH: Overrides the detection of the branch name to specify
  any name you want (it should be an alphanumeric word with
  no spaces). For example, this is useful when you are checked out of
  your branch but still want to reinstall the current state at the
  same switch.

FORCE_VERSION: Overrides the compiler-version detection.

OPAM: the opam command to use.

  Default: "opam".

EOF
        exit 0
        ;;
esac

# OPAM will need version information to accept package installation
# this is useful if you try to install packages that only support
# specific OCaml versions, eg. ">= 3.12": the version here needs to
# give a more-or-less valid indication of which version compatibility
# you will support, and in practice it does the right thing.
#
# For example, the SVN trunk has his VERSION file set to
# 4.01.0+dev... which, when stripped of the +dev part, reasonably
# indicates a high-enough OCaml version.
#
# OPAM packagers make use of this fact: for example, if 4.01.0 is the
# name of the current development version, they will set "< 4.01.0" as
# a version constraint for packages that are known to fail on trunk.

if [ ! -f VERSION ]
then
    cat <<EOF
Error: No VERSION file detected in the current working directory.

This script relies on being run from the directory of the OCaml
distribution you wish to compile and use through OPAM. In absence of
a VERSION file, we assume you are not in this directory and stop right
here.

If you really are in the source directory of an OCaml compiler, you
may add a VERSION file yourself; its first line should be a reasonable
approximation of the OCaml version it implements (eg. 4.00.1),
optionally followed by a '+' and whatever additional information. For
example: 4.01.0+dev-foo-bar

This script will derive the OPAM switch name from the content of this
VERSION file. Pass it the command 'get-switch' to see the inferred switch name.


EOF
    exit 1
fi

VERSION=`head -n 1 VERSION | sed "s/+.*//g"`

# some DCVS-specific logic to infer the branch name
#   I have only implemented the git logic, please feel free
#   to send me code for, for example, a SVN equivalent
if [ ! -z "$FORCE_BRANCH" ]
then BRANCH=$FORCE_BRANCH
else BRANCH=`git symbolic-ref --short -q HEAD`
fi

if [ ! -z "$FORCE_VERSION" ]
then VERSION_OPAM=$FORCE_VERSION
else VERSION_OPAM="${VERSION}"
fi

# the name of the corresponding OPAM switch
SWITCH=${VERSION_OPAM}+local-git-${BRANCH}

# the prefix passed to the ocaml distribution's ./configure, inside the opam repo
PREFIX=$OPAMDIR/$SWITCH

# create a correponding OPAM compiler
if [[ "$($OPAM --version)" < "1.1.0" ]] ; then
OPAM_COMP_DIR=$OPAMDIR/compilers
else
OPAM_COMP_DIR=$OPAMDIR/compilers/${VERSION_OPAM}/$SWITCH
fi
OPAM_COMP_PATH=$OPAM_COMP_DIR/$SWITCH.comp
OPAM_DESCR_PATH=$OPAM_COMP_DIR/$SWITCH.descr

# "src: $PATH" is the standard way to indicate the compiler source,
# but recent OPAM versions are too clever at finding that this is
# a git-versioned directory and clone it in a way that breaks
# opam-compiler-conf.sh. On recent-enough version we therefore use
# "local: $PATH" instead, which explicitly specifies that this must be
# used as local source rather than a git-versioned resource.
if [[ "$($OPAM --version)" < "1.2.0" ]] ; then
SRC_KEY=src
else
SRC_KEY=local
fi

PWD=`pwd`

output_comp_data() {
    echo "opam-version: \"1\""
    echo "version: \"${VERSION_OPAM}\""
    echo "$SRC_KEY: \"$PWD\""
    echo "build: ["
    echo "  [\"%{make}%\" \"install\"]"
    echo "]"
    echo "packages: ["
    for b in $BASE_PACKAGES; do echo "  \"$b\""; done
    echo "]"
    echo "env: ["
    echo "  [ CAML_LD_LIBRARY_PATH = \"%{lib}%/stublibs\" ]"
    echo "]"
}

output_descr_data() {
    echo "Local checkout of ${VERSION} at ${PWD}"
}

check_is_configured() {
    if [ ! -f "config/Makefile" ]
    then
        echo "Error: You must run the 'configure' command first."
        exit 1
    fi
    CONF_PREFIX=`grep "^PREFIX=" config/Makefile | head -n1`
    if [ ! "$CONF_PREFIX" = "PREFIX=$PREFIX" ]
    then
       cat <<EOF
Error: the ./configure script appear to not have been run by this script:
       its PREFIX setting
         $CONF_PREFIX
       does not match the current state of the directory
         PREFIX=$PREFIX
       Setting up OPAM in this way would prevent its installation from working
       properly.

       Note: if the wrong prefix comes from a wrong detection of the
       git branch (eg. during a bisection), you can set the
       FORCE_BRANCH variable to force a given value to be used as the
       branch name during PREFIX inference.
EOF
        exit 1
    fi
}

check_is_installed() {
    if [ ! -f "$OPAM_COMP_PATH" ]
    then
        echo "Error: You must run the 'install' command first."
        exit 1
    fi
}

do_install() {
    # configure the .opam switch
    mkdir -p $OPAM_COMP_DIR
    # we previously used 'echo -e', but OSX does not support it
    (output_comp_data) > $OPAM_COMP_PATH
    (output_descr_data) > $OPAM_DESCR_PATH
    #will run 'make install'
    $OPAM switch install $SWITCH
}

do_reinstall() {
    check_is_installed
    $OPAM switch reinstall $SWITCH
}

do_uninstall() {
    if [ "$SWITCH" = $(opam switch show) ]
    then
        echo "You are still on the switch '$SWITCH', switching to 'system' to uninstall."
        $OPAM switch system
    fi
    $OPAM switch remove $SWITCH
    # issue #8: if users get a yes/no choice and choose no, the remove
    # command above will return (with no particular exit code), yet
    # the switch is not uninstalled; do not remove
    # $OPAM_COMP_PATH in this case.
    if [ ! -d $PREFIX ]
    then
        rm $OPAM_COMP_PATH
    fi
}

# main :: IO ()   ;-)
case "$1" in
    show-switch|get-switch)
        echo $SWITCH
        ;;
    check-conf)
        check_is_configured
        echo "Your opam compiler switch is correctly configured."
        echo "You can build the compiler, then run the 'install' command."
        ;;
    show-conf|get-conf)
        output_comp_data
        ;;
    show-descr|get-descr)
        output_descr_data
        ;;
    show-paths|get-paths)
        echo "PREFIX=$PREFIX"
        echo "OPAM_COMP_DIR=$OPAM_COMP_DIR"
        echo "OPAM_COMP_PATH=$OPAM_COMP_PATH"
        echo "OPAM_DESCR_PATH=$OPAM_DESCR_PATH"
        ;;
    configure)
        # configure the ocaml distribution for compilation
        shift
        ./configure --prefix $PREFIX "$@"
        ;;
    install)
        # check that ./configure was run
        check_is_configured
        do_install
        ;;
    switch)
        check_is_configured
        check_is_installed
        $OPAM switch $SWITCH
        ;;
    reinstall)
        check_is_configured
        if [ ! -f "$OPAM_COMP_PATH" ]
        then
            do_install
        else
            do_reinstall
        fi
        ;;
    remove|uninstall)
        check_is_installed
        do_uninstall
        ;;
    *)
        echo $USAGE
        ;;
esac
