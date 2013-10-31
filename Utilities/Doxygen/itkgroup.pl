# if regular doxycomment add a @{
# at next empty line add a //@}

$ingroup = 0;
$semicount =0;
$endbracecount = 0;
$endparencount = 0;
while(<>)
{
    chomp;
    $line = $_;
    # if the line is not an empty line
    if( $line =~ /\S+/ )
    {
        if ( /\/\*\*(.*)/ )
        {
            # I guess it was not a group, dump savebuffer
            if($ingroup)
            {
                print "/**" . $savebuffer . "\n";
            }
            # if it is a class or brief then output the line but
            # do not start a group
            if ( /(\\class|\\brief)/ )
            {
                print $line . "\n";
            }
            # must be a group so start saving
            else
            {
                $savebuffer = "$1" . "\n";
                $ingroup = 1;
                $semicount = 0;
                $endbracecount = 0;
                $endparencount = 0;
            }
        }
        else
        {
            # add to save buffer if in group
            if($ingroup)
            {
                $savebuffer = $savebuffer . $_ . "\n";
            }
            else
            {
                # non empty line that is not the start of a doxy comment
                print $_ . "\n";
            }
        }
        if($line =~ /;/ )
        {
            $semicount = $semicount + 1;
        }
        if($line =~ /\}/ )
        {
            $endbracecount = $endbracecount + 1;
        }
        if($line =~ /\)/ )
        {
            $endparencount = $endparencount + 1;
        }
    }
    else
    {
        if($ingroup)
        {
            if($endparencount > 1 && ($semicount > 1 || $endbracecount > 1))
            {
                print "/**@\{" . $savebuffer . "//@}\n\n";
            }
            else
            {
                print "/**" . $savebuffer . "\n";
            }
            $savebuffer = "";
            $ingroup = 0;
        }
        else
        {
            print $line . "\n";
        }
    }
}
