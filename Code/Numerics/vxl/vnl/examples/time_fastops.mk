#! /bin/sh
#
DSTDIR=`make echovar-RELOBJDIR|grep -v \ |tail -1`
test -f $DSTDIR/time_fastops.o || exit 1
echo Running time_fastops in $DSTDIR
#
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make DEFINES="-DMETHOD=1" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.10 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make DEFINES="-DMETHOD=2" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.20 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make DEFINES="-DMETHOD=3" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.30 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make DEFINES="-DMETHOD=4" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.40 2>&1
#
DSTDIR=`make BUILD=noshared echovar-RELOBJDIR|grep -v \ |tail -1`
test -f $DSTDIR/time_fastops.o || exit 0
echo Running time_fastops in $DSTDIR
#
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make BUILD=noshared DEFINES="-DOPTIMIZED -DMETHOD=1" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.10 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make BUILD=noshared DEFINES="-DOPTIMIZED -DMETHOD=2" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.20 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make BUILD=noshared DEFINES="-DOPTIMIZED -DMETHOD=3" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.30 2>&1
rm -f $DSTDIR/time_fastops.o $DSTDIR/time_fastops
make BUILD=noshared DEFINES="-DOPTIMIZED -DMETHOD=4" $DSTDIR/time_fastops
$DSTDIR/time_fastops > $DSTDIR/time_fastops.40 2>&1
