#!/usr/bin/env bash
set -o errexit
set -o nounset

### (new with TeemV2)
### This is the new script for regenerating NrrdIO from Teem sources.
### Previously this existed as a set of commented-out commands near
### the top of pre-GNUmakefile, but (because of comments) not in a
### useful copy-and-paste-able format. This is a more reliable way
### to run everything to re-generate NrrdIO, including for ITK.

# set DOPULL
DOPULL=''
if [[ $# -ge 1 ]]; then
    if [[ $1 == "-p" || $1 == "--pull" ]]; then
        DOPULL='yup'
        shift
    fi
fi

# set ITK and/or show usage info
if [[ $# -eq 0 ]]; then
    ITK=''
elif [[ $# -eq 1 && $1 == "itk" ]]; then
    ITK='yup'
else
    >&2 echo "Usage: $0 [-p|--pull] [itk]"
    exit 1
fi

# make sure TEEM_SRC_ROOT is set, and set correctly
if [ -z ${TEEM_SRC_ROOT+x} ]; then
    >&2 echo "$0: Oops: need TEEM_SRC_ROOT set to path to Teem source, the full path"
    >&2 echo "$0: of the directory containing \"src\" (with \"src/air\", \"src/biff\","
    >&2 echo "$0: \"src/nrrd\" subidrectories)."
    exit 1
else
    if [[ ! -d "$TEEM_SRC_ROOT/src" ||
          ! -d "$TEEM_SRC_ROOT/src/air" ||
          ! -d "$TEEM_SRC_ROOT/src/biff" ||
          ! -d "$TEEM_SRC_ROOT/src/nrrd" ]]; then
        >&2 echo "$0: TEEM_SRC_ROOT is set to $TEEM_SRC_ROOT"
        >&2 echo "$0: but don't see \"src\" subdirectory with further"
        >&2 echo "$0: \"src/air\", \"src/biff\", \"src/nrrd\" subdirs"
        exit 1
    fi
fi

# turn on echoing
set -o xtrace
if [[ $DOPULL ]]; then
    (cd $TEEM_SRC_ROOT; git pull)
    git pull
fi
if [[ $ITK ]]; then
    # regenerate itk_NrrdIO_mangle.h.in
    make -f pre-GNUmakefile clean
    make -f pre-GNUmakefile
    make -f sample-GNUmakefile # to make libNrrdIO.a
    # runs "nm libNrrdIO.a" to generate #define renamings
    ./mangle.py @MANGLE_PREFIX@ itk > itk_NrrdIO_mangle.h.in
    # can now delete libNrrdIO.a and its pre-reqs
    # restart making NrrdIO
    make -f sample-GNUmakefile clean
    make -f pre-GNUmakefile clean
    # with ITK_NRRDIO this generates NrrdIO.h.in (as well as NrrdIO.h)
    ITK_NRRDIO= make -f pre-GNUmakefile
else
    make -f pre-GNUmakefile clean
    make -f pre-GNUmakefile
    make -f sample-GNUmakefile clean
    make -f sample-GNUmakefile
fi
# turn off echoing
set +o xtrace
