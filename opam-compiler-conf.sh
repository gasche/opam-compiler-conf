# the path to the OPAM installation
OPAM=~/.opam

USAGE="available commands: get-switch get-conf configure\
                           install switch reinstall remove|uninstall\
                           help"

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

VERSION=`head -n 1 VERSION | sed s/+.*//g`

# some DCVS-specific logic to infer the branch name
#   I have only implemented the git logic, please feel free
#   to send me code for, for example, a SVN equivalent
BRANCH=`git branch | grep "^*" | cut -d' ' -f2`

# the name of the corresponding OPAM switch
SWITCH=${VERSION}+git-${BRANCH}

# the prefix passed to the ocaml distribution's ./configure, inside the opam repo
PREFIX=$OPAM/$SWITCH

# create a correponding OPAM compiler
OPAM_COMP_DIR=$OPAM/compilers
OPAM_COMP_PATH=$OPAM_COMP_DIR/$SWITCH.comp

PWD=`pwd`
OPAM_COMP_DATA="
opam-version: \"1\"\n\
version: \"$VERSION\"\n\
src: \"$PWD\"\n\
build: [\n\
  [\"%{make}%\" \"install\"]\n\
]\n\
env: [\n\
  [ CAML_LD_LIBRARY_PATH = \"%{lib}%/stublibs\" ]\n\
]\n\
"


check_is_configured() {
    if [ ! -f "config/Makefile" ]
    then
        echo "Error: You must run the 'configure' command first."
        exit 1
    fi
    CONF_PREFIX=`grep "^PREFIX=" config/Makefile | head -n1`
    if [  "$CONF_PREFIX" = "PREFIX=$PREFIX" ]
    then
        echo
    else
        cat <<EOF
Error: the ./configure script appear to not have been run by this script:
       its PREFIX setting
         $CONF_PREFIX
       does not match the current state of the directory
         PREFIX=$PREFIX
       Setting up OPAM in this way would prevent its installation from working
       properly.
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

# main :: IO ()   ;-)
case "$1" in
    get-switch)
        echo $SWITCH
        ;;
    get-conf)
        echo -e $OPAM_COMP_DATA
        ;;
    configure)
        # configure the ocaml distribution for compilation
        ./configure --prefix $PREFIX --no-tk
        ;;
    install)
        # check that ./configure was run
        check_is_configured
        # configure the .opam switch
        mkdir -p $OPAM_COMP_DIR
        echo -e $OPAM_COMP_DATA > $OPAM_COMP_PATH
        #will run 'make install'
        opam switch install $SWITCH
        ;;
    switch)
        check_is_configured
        check_is_installed
        opam switch $SWITCH
        ;;
    reinstall)
        check_is_configured
        check_is_installed
        opam switch reinstall $SWITCH
        ;;
    remove|uninstall)
        check_is_installed
        if [ "$SWITCH" = `opam switch show` ]
        then
            echo "You are still on the switch '$SWITCH', switching to 'system' to uninstall."
            opam switch system
        fi
        opam switch remove $SWITCH
        rm $OPAM_COMP_PATH
        ;;
    *)
        echo $USAGE
        ;;
esac
