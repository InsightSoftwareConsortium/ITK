# if regular doxycomment add a @{
# at next empty line add a //@}

$ingroup = 0;
while(<>)
{
    chomp;
    $line = $_;
    if( $line =~ /\S+/ )
    {
	if ( /\/\*\*(.*)/ )
	{
	    if ( /(\\class|\\brief)/ )
	    {
		print $line . "\n";
	    }
	    else
	    {
		$ingroup = 1;
		print "/**@\{" . $1 . "\n";
	    }
	}
	else
	{
	    # non empty line that is not the start of a doxy comment
	    print $_ . "\n";
	}
    }
    else
    {
	if($ingroup)
	{
	    print "//@}\n";
	    $ingroup = 0;
	}
	else
	{
	    print $line . "\n";
	}
    }
}
