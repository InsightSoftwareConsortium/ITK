#!/usr/bin/perl -ws
#
# Script that will move testing results from /ftp/incoming to the 
# appropriate location for dashboard summarization.  
#
# Installation:
#    Place this script in your cgi-bin area. 
#    Change the name of the script to match the "TriggerSite" that Build.tcl.in
#        uses.  This is currently defined in a toplevel CMakeLists.txt file.
#    Change the variables "dropLocation" and "destination" to match your 
#        installation. "destination" probably needs to end in 
#        "Testing/HTML/TestingResults/Sites" for the XSLT scripts to run.
#
###

use CGI;
use File::Basename;
use File::Copy;
use File::Path;


#
# Change these two variables to suit your own installation
#
$dropLocation = "/ftp/incoming";
$destination = "/insight/TestingTree/Insight-TestingResults/Testing/HTML/TestingResults/Sites";


#
# Nothing should need to be changed below this point
#
$query = CGI::new();
$xmlfile = $query->param("xmlfile");
$xmlfile =~ s/%(..)/sprintf("%c", hex($1))/g;   # unquote %-quoted

print $query->header();

$fullPathToIncomingXMLFile = $dropLocation . "/" . $xmlfile;
if (-e $fullPathToIncomingXMLFile) {
    # file exists, so lets move it

    # first, translate the xml filename to a directory path
    $xmlfile =~ s|___|/|g;

    # for security reasons, disallow any file with ".." in the user 
    # specified path to keep people from storing files just anywhere in
    # the host directory structure
    $securityCheck = $xmlfile;
    if ( ($securityCheck =~ /\.\./) ) {
	print $query->p("For security reasons, $xmlfile cannot be accepted.");
	exit;
    }
    
    # construct destination path and filename
    $fullPathToDestinationXMLFile = $destination . "/" . $xmlfile;
    mkpath( dirname( $fullPathToDestinationXMLFile ) );

    # now copy the file to destination
    move( $fullPathToIncomingXMLFile, $fullPathToDestinationXMLFile);

    print $query->p("$xmlfile submission successful.");
  }
else {
    # specified file does not exist
    print $query->p("$xmlfile submission failed. $xmlfile is not in the dropbox.");
  }

