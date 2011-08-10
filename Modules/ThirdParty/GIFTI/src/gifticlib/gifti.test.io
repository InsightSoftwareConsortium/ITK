#!/bin/tcsh

# either include the names of the gifti files on the command line,
# or be sitting in the directory with them

set skip_ts = 0
set prog = gifti_tool   # was gifti_test

# if the user specifies -nots, skip the time series dataset
set narg = $#argv
set base = 1
set gt_comp = 0
if ( $narg >= 1 ) then
    if ( "$argv[1]" == "-help" ) then
        echo "usage: gifti.test.io [-nots] [-gt_compare] [files...]"
        echo ""
        echo "option:"
        echo "    -nots         : do not process *time_series*.gii"
        echo ""
        echo "examples:"
        echo "    gifti.test.io"
        echo "    gifti.test.io *.gii"
        echo "    gifti.test.io -nots"
        echo "    gifti.test.io -nots *.gii"

        exit
    else if ( "$argv[1]" == "-gt_compare" ) then
        @ narg --
        @ base ++
        set gt_comp = 1
    else if ( "$argv[1]" == "-nots" ) then
        @ narg --
        @ base ++
        set skip_ts = 1
    endif
endif

echo using `which $prog` ...

if ( $narg >= 1 ) then
    set files = ( $argv[$base-] )
else
    echo applying default gifti files...
    set files = ( gifti.*.gii )
endif

echo ""

foreach file ( $files )
    echo -n "$file : "

    if ( $skip_ts && "$file" =~ *time_series*.gii ) then
        echo "skipping ..."
        continue
    endif

    $prog -infile $file -no_updates -write_gifti new.gii
    if( $status ) then
        echo "** FAILURE : $prog -infile $file -no_updates -write_gifti new.gii"
    else
        if ( $gt_comp ) then
            gifti_tool -compare_gifti -compare_data -infiles $file new.gii  \
                >& /dev/null
            set result = $status
            if ( $result ) echo '** differs **'
        else
            cmp $file new.gii
            set result = $status
        endif
        if( ! $result ) echo okay
    endif
end
