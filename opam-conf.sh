# the path of the local OPAM development repository
#   (to create it, just use
#      mkdir -p $REPO/compilers
#      opam repo add dev-repo $REPO
#   )
REPO=~/.opam

#opam will need version information to accept package installation
VERSION=`head -n 1 VERSION | sed s/+.*//g`

# some DCVS-specific logic to infer the branch name
BRANCH=`git branch | grep "^*" | cut -d' ' -f2`

# the name of the corresponding OPAM switch
SWITCH=${VERSION}+git-${BRANCH}

# the prefix passed to the ocaml distribution's ./configure, inside the opam repo
PREFIX=$REPO/$SWITCH

# configure the ocaml distribution for compilation
./configure --prefix $PREFIX --no-tk --no-camlp4 --no-graph

# create a correponding OPAM compiler
mkdir -p $REPO/compilers
OPAMCONF=$REPO/compilers/$SWITCH.comp
PWD=`pwd`
cat > $OPAMCONF <<EOF
opam-version: "1"
version: "$VERSION"
preinstalled: true
EOF

# it is important that we call `opam switch install $SWITCH' before we
# actually install the compiler distribution (make install), because otherwise
# opam will launch the 'install' command the first time you call
# 'opam switch SWITCH', which has the side-effect of deleting the directory
# $PREFIX as a safety m$easure (oh, something is already there at
# install time, surely the safest thing to do is to blast it!), removing
# the binaries installed by 'make install'...
opam switch install $SWITCH

echo "OPAM configuration created for switch '$SWITCH'"


# you're done, now you can compile OCaml as you wish, that is probably
#   make world.opt
# then install it,
#   make install
# and it will be installed in the right place for OPAM to recognize it
# as a new compiler that you can instantly use to install new
# packages, etc. Use
#   opam switch dev-$BRANCH
# to have opam switch to the new compiler version
