# =========================================================================
# 
#   Program:   Insight Segmentation & Registration Toolkit
#   Module:    Update.tcl
#   Language:  Tcl
#   Date:      $Date$
#   Version:   $Revision$
# 

# Copyright (c) 2001 Insight Consortium
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#  * Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the following disclaimer.

#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.

#  * The name of the Insight Consortium, nor the names of any consortium members,
#    nor of any contributors, may be used to endorse or promote products derived
#    from this software without specific prior written permission.

#   * Modified source versions must be plainly marked as such, and must not be
#     misrepresented as being the original software.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


# Unlike the other tcl scripts in this directory,
# Update.tcl is run in the source directory.
# To avoid boot straping problems, this script
# writes to stdout and assums that cvs is in your path.


# Cheat, source in some utilities.  No actual substitutions happen.
source [file join Testing Utilities Utility.tcl.in]

# Read in the user list
set f [open [file join Documentation UserList.txt]]
while { ![eof $f] } \
{
  set l [gets $f]
  set l [split $l ":"]
  if { [llength $l] == 3 } \
  {
    set Users([lindex $l 0]) [lindex $l 2]
  }
}
close $f

set Model Experimental
set DateStamp ""
if { $argc >= 1 } \
{
  set Model [lindex $argv 0]
  # set DateStamp [lindex $argv 1]
  # set Year [string range $DateStamp 0 3]
  # set Month [string range $DateStamp 4 5]
  # set Day [string range $DateStamp 6 7]
}

# Begin the XML output
set Out stdout

set cvs cvs

puts $Out {<?xml version="1.0" encoding="UTF-8"?>}
puts $Out "<Update>"
puts $Out "\t<StartDateTime>[clock format [clock seconds]]</StartDateTime>"


proc SplitLog { Log } \
{
  set Result ""
  set Buffer ""
  foreach Line [split $Log "\n"] \
  {
    switch -glob -- $Line \
    {
      "=========================================================================*" - \
      "--------------------------*" \
      {
	lappend Result $Buffer
	set Buffer ""
      }
      default \
      {
	append Buffer "$Line\n"
      }
    }
  }
  return [lrange $Result 1 end]
}

proc LoadCVSInformation { File } \
{
  global UseDates Yesterday Today cvs FileStatus YesterdayTS Model Users

  #  puts stderr $YesterdayTS

  if { [catch { set Log [exec $cvs log -N $File] }] } { return }
  # puts stderr "Log of $File: $Log"

  regexp "head: (\[0-9.\]+)" $Log dummy FileStatus($File,Head)
  regexp "total revisions: (\[0-9\]+);\tselected revisions: (\[0-9\])" $Log dummy FileStatus($File,TotalRevisions) FileStatus($File,SelectedRevisions)

  set FileStatus($File,SelectedRevisions) 0
  
  set FileStatus($File,LastReportedRevision) $FileStatus($File,Head)
  set Logs [SplitLog $Log]
  set i 0
  set LastReported $FileStatus($File,Head)
  set HaveOne 0
  
  foreach SubLog $Logs \
  {
    set SplitLog [split $SubLog "\n"]
    regexp "date: (\[0-9\]+)/(\[0-9\]+)/(\[0-9\]+) (\[^;\]+);" [lindex $SplitLog 1] Date Year Month Day Time
    
    regexp "revision (\[0-9.\]+)" [lindex $SplitLog 0] dummy LastReported

    if { $i == 1 } \
    {
      set FileStatus($File,LastReportedRevision) $LastReported
    }

     
    
    set FileStatus($File,RevisionLog,$i,PreviousRevision) $LastReported
    
    regexp "revision (\[0-9.\]+)" [lindex $SplitLog 0] dummy FileStatus($File,RevisionLog,$i,Revision)

    if { $i != 0 } \
    {
      set FileStatus($File,RevisionLog,[expr $i - 1],PreviousRevision) $FileStatus($File,RevisionLog,$i,Revision)
    }

    #
    # Check to see if the date is still today
    # Always capture at least one revision...
#     if { [clock scan "$Time $Month/$Day/$Year"] < $YesterdayTS && $HaveOne} \
#     {
#       break
#     }
    set HaveOne 1


    regexp "date: (\[^;\]+);  author: (\[^;\]+);" [lindex $SplitLog 1] dummy FileStatus($File,RevisionLog,$i,Date) FileStatus($File,RevisionLog,$i,Author)

    if { [info exists Users($FileStatus($File,RevisionLog,$i,Author))] } \
    {
      set FileStatus($File,RevisionLog,$i,Email) $Users($FileStatus($File,RevisionLog,$i,Author))
    } \
    else \
    {
      set FileStatus($File,RevisionLog,$i,Email) ""
    }
    

    foreach l [lrange $SplitLog 2 end] \
    {
      if { $l != {} } \
      {
	append FileStatus($File,RevisionLog,$i,Comment) "$l\n"
      }
    }
    incr i
    incr FileStatus($File,SelectedRevisions)
    if { $Model == "Experimental" && $i > 1 } \
    {
      break
    }
    # Break out if not today or yesterday...
    if { $Model == "Nightly" && $i > 1 } \
    {
      if { ![string match "$Today*" $Date] && ![string match "$Yesterday*" $Date] } \
      {
	break;
      }
    }
    
  }
  
}
  
# Do the update
# Update to a particular time, but to do later.
# clock format [clock scan today] -format "%Y-%m-%d 23:00 %Z" -gmt 1

if { [info exists env(NOUPDATE)] } \
{
  set UpdateCommand "$cvs -n update -d -P -A"
} \
else \
{
  set UpdateCommand "$cvs update -d -P -A"
}

set UseDates 0
if { $Model == "Nightly" } \
{
  # For the moment, just get latest source
  # set UpdateCommand "$UpdateCommand -D \"$Year-$Month-$Day 23:00 GMT\""

  set t [GetNightlySeconds]

  set Date [clock format $t -format "%Y-%m-%d 3:00:00 EST"]
  set UpdateCommand "$UpdateCommand -D \"$Date\""

  set Today [clock format $t -format "%Y/%m/%d"]
  set Yesterday [clock format [expr $t - 24 * 60 * 60 ] -format "%Y/%m/%d"]
  
}
if { $Model == "Experimental" } \
{
  set t [clock seconds]
  set Today [clock format $t -format "%Y/%m/%d"]
  set Yesterday [clock format [expr $t - 24 * 60 * 60 ] -format "%Y/%m/%d"]
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
    set FileStatus([lindex $Line 1],Status) Updated
    lappend Files [lindex $Line 1]
  }
  if { [regexp "^C " $Line] } \
  {
    set FileStatus([lindex $Line 1],Status) Conflicting
    lappend Files [lindex $Line 1]
  }
  if { [regexp "^M " $Line] } \
  {
    set FileStatus([lindex $Line 1],Status) Modified
    lappend Files [lindex $Line 1]
  }
  
}
close $Update
# catch { file delete -force update.tmp }

# Get a little bit of info for each file
foreach File $Files \
{
  LoadCVSInformation $File
  # parray FileStatus "*$File*"
  lappend DirectoryList([file dir $File]) $File
  lappend AuthorList($FileStatus($File,RevisionLog,0,Author)) $File
}

foreach Dir [array names DirectoryList] \
{
  puts $Out "\t<Directory>"
  puts $Out "\t\t<Name>[XMLSafeString $Dir]</Name>"
  foreach File $DirectoryList($Dir) \
  {
    puts $Out "\t<$FileStatus($File,Status)>"
    puts $Out "\t\t<File Directory=\"[file dir $File]\">[file tail $File]</File>"
    puts $Out "\t\t<Directory>[file dir $File]</Directory>"
    puts $Out "\t\t<FullName>$File</FullName>"
    
    puts $Out "\t\t<CheckinDate>[XMLSafeString $FileStatus($File,RevisionLog,0,Date)]</CheckinDate>"
    puts $Out "\t\t<Author>[XMLSafeString $FileStatus($File,RevisionLog,0,Author)]</Author>"
    puts $Out "\t\t<Email>[XMLSafeString $FileStatus($File,RevisionLog,0,Email)]</Email>"
  
  
    puts $Out "\t\t<Log>[XMLSafeString $FileStatus($File,RevisionLog,0,Comment)]</Log>"
    puts $Out "\t\t<Revision>$FileStatus($File,Head)</Revision>"
    puts $Out "\t\t<PriorRevision>$FileStatus($File,LastReportedRevision)</PriorRevision>"

    for { set i 0 } { $i < $FileStatus($File,SelectedRevisions) } { incr i } \
    {
      puts $Out "\t\t<Revisions>"
      foreach Field [list Revision PreviousRevision Author Date Comment Email] \
      {
	puts $Out "\t\t\t<$Field>[XMLSafeString $FileStatus($File,RevisionLog,$i,$Field)]</$Field>"
      }    
      puts $Out "\t\t</Revisions>"
    }
    puts $Out "\t</$FileStatus($File,Status)>"
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

