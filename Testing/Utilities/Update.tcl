# Unlike the other tcl scripts in this directory,
# Update.tcl is run in the source directory.
# To avoid boot straping problems, this script
# writes to stdout and assums that cvs is in your path.


# Cheat, source in some utilities.  No actual substitutions happen.
source [file join Testing Utilities Utility.tcl.in]

set Model Experimental
set DateStamp ""
if { $argc == 2 } \
{
  set Model [lindex $argv 0]
  set DateStamp [lindex $argv 1]
  set Year [string range $DateStamp 0 3]
  set Month [string range $DateStamp 4 5]
  set Day [string range $DateStamp 6 7]
}

# Begin the XML output
set Out stdout

set cvs cvs

puts $Out {<?xml version="1.0" encoding="UTF-8"?>}
puts $Out "<Update>"
puts $Out "\t<StartDateTime>[clock format [clock seconds]]</StartDateTime>"


proc GetLog { File } \
{
  global cvs
  return "[exec $cvs log -N -r $File | grep -v "access list:" | grep -v "RCS File:" | grep -v "Working File:" | grep -v "head:" | grep -v "branch:" | grep -v "locks:" | grep -v "keyword substitution:" | grep -v "total revisions:" | grep -v "description:" | grep -v "\\-\\-\\-\\-\\-\\-\\-\\-\\-\\-" | grep -v ^$ ]";
}
  
# Do the update
# Update to a particular time, but to do later.
# clock format [clock scan today] -format "%Y-%m-%d 23:00 %Z" -gmt 1

set UpdateCommand "$cvs update -d -P -A"

if { $Model == "Nightly" } \
{
  # For the moment, just get latest source
  # set UpdateCommand "$UpdateCommand -D \"$Year-$Month-$Day 23:00 GMT\""
}
  
set UpdateStatus [catch { eval exec $UpdateCommand >& update.tmp } result]

puts $Out "\t<UpdateCommand>$UpdateCommand</UpdateCommand>"
puts $Out "\t<UpdateReturnStatus>[XMLSafeString $result]</UpdateReturnStatus>"

set Update [open update.tmp r]

catch { unset AuthorList }
catch { unset DirectoryList }
set Files ""
set Line 0
while { ![eof $Update] } \
{
  set Line [gets $Update]
  if { [regexp "^U " $Line] || [regexp "^P " $Line] } \
  {
    set FileStatus([lindex $Line 1]) Updated
    lappend Files [lindex $Line 1]
  }
  if { [regexp "^C " $Line] } \
  {
    set FileStatus([lindex $Line 1]) Conflicting
    lappend Files [lindex $Line 1]
  }
  if { [regexp "^M " $Line] } \
  {
    set FileStatus([lindex $Line 1]) Modified
    lappend Files [lindex $Line 1]
  }
  
}
close $Update
catch { file delete -force update.tmp }

# Get a little bit of info for each file
foreach File $Files \
{
  set lastRevision [lindex [exec cvs log -N -r $File | grep "revision "] 1]
  set allRevisions  [exec /usr/tmp/local/bin/cvs log -N -r:$lastRevision  $File | grep "revision "]
  set priorRevision [lindex $allRevisions 3]
  set Log [GetLog $File]

  puts $Out "\t<$FileStatus($File)>"
  puts $Out "\t\t<File Directory=\"[file dir $File]\">[file tail $File]</File>"
  puts $Out "\t\t<FullName>$File</FullName>"

  set Log [GetLog $File]
  # Try to get autor and date
  set Date ""
  set Author ""
  regexp "date:(\[^;\]*)" $Log a Date
  regexp "author:(\[^;\]*)" $Log a Author
  set Author [string trim $Author]
  set Date [string trim $Date]
  lappend AuthorList($Author) $File

  lappend DirectoryList([file dir $File]) [file tail $File]
  
  puts $Out "\t\t<CheckinDate>[XMLSafeString [string trim $Date]]</CheckinDate>"
  puts $Out "\t\t<Author>[XMLSafeString [string trim $Author]]</Author>"
  
  
  puts $Out "\t\t<Log>[XMLSafeString $Log]</Log>"
  puts $Out "\t\t<Revision>$lastRevision</Revision>"
  if { $priorRevision != "" } \
  {
    puts $Out "\t\t<PriorRevision>$priorRevision</PriorRevision>"
  }
  puts $Out "\t</$FileStatus($File)>"

}

foreach Dir [array names DirectoryList] \
{
  puts $Out "\t<Directory>"
  puts $Out "\t\t<Name>[XMLSafeString $Dir]</Name>"
  foreach File $DirectoryList($Dir) \
  {
    puts $Out "\t\t<File Directory=\"$Dir\">$File</File>"
  }
  puts $Out "\t</Directory>"
}

foreach Author [array names AuthorList] \
{
  puts $Out "\t<Author>"
  puts $Out "\t\t<Name>[XMLSafeString $Author]</Name>"
  foreach File $AuthorList($Author) \
  {
    puts $Out "\t\t<File Directory=\"[file dir $File]\">[file tail $File]</File>"
  }
  puts $Out "\t</Author>"
}
  

puts $Out "\t<EndDateTime>[clock format [clock seconds]]</EndDateTime>"

puts $Out "</Update>"
close $Out
exit

