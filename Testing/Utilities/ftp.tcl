# ftp.tcl --
#
#	FTP library package for Tcl 8.2+.  Originally written by Steffen
#	Traeger (Steffen.Traeger@t-online.de); modified by Peter MacDonald
#	(peter@pdqi.com) to support multiple simultaneous FTP sessions.
#
# Copyright (c) 1996-1999 by Steffen Traeger <Steffen.Traeger@t-online.de>
# Copyright (c) 2000 by Ajuba Solutions
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) Id
#
#   core ftp support: 	ftp::Open <server> <user> <passwd> <?options?>
#			ftp::Close <s>
#			ftp::Cd <s> <directory>
#			ftp::Pwd <s>
#			ftp::Type <s> <?ascii|binary?>	
#			ftp::List <s> <?directory?>
#			ftp::NList <s> <?directory?>
#			ftp::FileSize <s> <file>
#			ftp::ModTime <s> <from> <to>
#			ftp::Delete <s> <file>
#			ftp::Rename <s> <from> <to>
#			ftp::Put <s> <local> <?remote?>
#			ftp::Append <s> <local> <?remote?>
#			ftp::Get <s> <remote> <?local?>
#			ftp::Reget <s> <remote> <?local?>
#			ftp::Newer <s> <remote> <?local?>
#			ftp::MkDir <s> <directory>
#			ftp::RmDir <s> <directory>
#			ftp::Quote <s> <arg1> <arg2> ...
#

package provide ftp [lindex {Revision: 2.0 } 1]

namespace eval ftp {

namespace export DisplayMsg Open Close Cd Pwd Type List NList FileSize ModTime\
		 Delete Rename Put Append Get Reget Newer Quote MkDir RmDir 
	
set serial 0
set VERBOSE 0
set DEBUG 0
}

#############################################################################
#
# DisplayMsg --
#
# This is a simple procedure to display any messages on screen.
# Can be intercepted by the -output option to Open
#
#	namespace ftp {
#		proc DisplayMsg {msg} {
#			......
#		}
#	}
#
# Arguments:
# msg - 		message string
# state -		different states {normal, data, control, error}
#
proc ftp::DisplayMsg {s msg {state ""}} {

    upvar ::ftp::ftp$s ftp
    variable VERBOSE 
    
    if { ([info exists ftp(Output)]) && ($ftp(Output) != "") } {
        eval [concat $ftp(Output) {$s $msg $state}]
        return
    }
        
    switch -exact -- $state {
        data {
            if { $VERBOSE } {
                puts $msg
            }
        }
        control	{
            if { $VERBOSE } {
                puts $msg
            }
        }
        error {
            puts stderr "ERROR: $msg"
        }
        default	{
            if { $VERBOSE } {
                puts $msg
            }
        }
    }
    return
}

#############################################################################
#
# Timeout --
#
# Handle timeouts
# 
# Arguments:
#  -
#
proc ftp::Timeout {s} {
    upvar ::ftp::ftp$s ftp

    after cancel $ftp(Wait)
    set ftp(state.control) 1

    DisplayMsg "" "Timeout of control connection after $ftp(Timeout) sec.!" error
    return
}

#############################################################################
#
# WaitOrTimeout --
#
# Blocks the running procedure and waits for a variable of the transaction 
# to complete. It continues processing procedure until a procedure or 
# StateHandler sets the value of variable "finished". 
# If a connection hangs the variable is setting instead of by this procedure after 
# specified seconds in $ftp(Timeout).
#  
# 
# Arguments:
#  -		
#

proc ftp::WaitOrTimeout {s} {
    upvar ::ftp::ftp$s ftp

    set retvar 1

    if { [info exists ftp(state.control)] } {

        set ftp(Wait) [after [expr {$ftp(Timeout) * 1000}] [list [namespace current]::Timeout $s]]

        vwait ::ftp::ftp${s}(state.control)
        set retvar $ftp(state.control)
    }
    return $retvar
}

#############################################################################
#
# WaitComplete --
#
# Transaction completed.
# Cancel execution of the delayed command declared in procedure WaitOrTimeout.
# 
# Arguments:
# value -	result of the transaction
#			0 ... Error
#			1 ... OK
#

proc ftp::WaitComplete {s value} {
    upvar ::ftp::ftp$s ftp

    if { [info exists ftp(state.data)] } {
        vwait ::ftp::ftp${s}(state.data)
    }

    after cancel $ftp(Wait)
    set ftp(state.control) $value
    return $ftp(state.control)
}

#############################################################################
#
# PutsCtrlSocket --
#
# Puts then specified command to control socket,
# if DEBUG is set than it logs via DisplayMsg
# 
# Arguments:
# command - 		ftp command
#

proc ftp::PutsCtrlSock {s {command ""}} {
    upvar ::ftp::ftp$s ftp
    variable DEBUG
	
    if { $DEBUG } {
        DisplayMsg $s "---> $command"
    }

    puts $ftp(CtrlSock) $command
    flush $ftp(CtrlSock)
    return
}

#############################################################################
#
# StateHandler --
#
# Implements a finite state handler and a fileevent handler
# for the control channel
# 
# Arguments:
# sock - 		socket name
#			If called from a procedure than this argument is empty.
# 			If called from a fileevent than this argument contains
#			the socket channel identifier.

proc ftp::StateHandler {s {sock ""}} {
    upvar ::ftp::ftp$s ftp
    variable DEBUG 
    variable VERBOSE

    # disable fileevent on control socket, enable it at the and of the state machine
    # fileevent $ftp(CtrlSock) readable {}
		
    # there is no socket (and no channel to get) if called from a procedure

    set rc "   "

    if { $sock != "" } {

        set number [gets $sock bufline]

        if { $number > 0 } {

            # get return code, check for multi-line text
            
            regexp "(^\[0-9\]+)( |-)?(.*)$" $bufline all rc multi_line msgtext
            set buffer $bufline
			
            # multi-line format detected ("-"), get all the lines
            # until the real return code

            while { [string equal $multi_line "-"] } {
                set number [gets $sock bufline]	
                if { $number > 0 } {
                    append buffer \n "$bufline"
                    regexp "(^\[0-9\]+)( |-)?(.*)$" $bufline all rc multi_line
                }
            }
        } elseif { [eof $ftp(CtrlSock)] } {
            # remote server has closed control connection
            # kill control socket, unset State to disable all following command
            
            set rc 421
            if { $VERBOSE } {
                DisplayMsg $s "C: 421 Service not available, closing control connection." control
            }
            DisplayMsg $s "Service not available!" error
            CloseDataConn $s
            WaitComplete $s 0
            catch {unset ftp(State)}
            catch {close $ftp(CtrlSock); unset ftp(CtrlSock)}
            return
        }
	
    } 
	
    if { $DEBUG } {
        DisplayMsg $s "-> rc=\"$rc\"\n-> state=\"$ftp(State)\""
    }
	
    # system status replay
    if { [string equal $rc "211"] } {
        return
    }

    # use only the first digit 
    regexp "^\[0-9\]?" $rc rc
	
    switch -exact -- $ftp(State) {
        user { 
            switch -exact -- $rc {
                2 {
                    PutsCtrlSock $s "USER $ftp(User)"
                    set ftp(State) passwd
                }
                default {
                    set errmsg "Error connecting! $msgtext"
                    set complete_with 0
                }
            }
        }
        passwd {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                3 {
                    PutsCtrlSock $s "PASS $ftp(Passwd)"
                    set ftp(State) connect
                }
                default {
                    set errmsg "Error connecting! $msgtext"
                    set complete_with 0
                }
            }
        }
        connect {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error connecting! $msgtext"
                    set complete_with 0
                }
            }
        }   
        quit {
            PutsCtrlSock $s "QUIT"
            set ftp(State) quit_sent
        }
        quit_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error disconnecting! $msgtext"
                    set complete_with 0
                }
            }
        }
        quote {
            PutsCtrlSock $s $ftp(Cmd)
            set ftp(State) quote_sent
        }
        quote_sent {
            set complete_with 1
            set ftp(Quote) $buffer
        }
        type {
            if { [string equal $ftp(Type) "ascii"] } {
                PutsCtrlSock $s "TYPE A"
            } else {
                PutsCtrlSock $s "TYPE I"
            }
            set ftp(State) type_sent
        }
        type_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error setting type \"$ftp(Type)\"!"
                    set complete_with 0
                }
            }
        }
        nlist_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) nlist_open
            } else {
                set errmsg "Error setting port!"
            }
        }
        nlist_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) nlist_open
        }
        nlist_open {
            switch -exact -- $rc {
                2 {
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    }   
                    PutsCtrlSock $s "NLST$ftp(Dir)"
                    set ftp(State) list_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        list_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) list_open
            } else {
                set errmsg "Error setting port!"
            }
        }
        list_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) list_open
        }
        list_open {
            switch -exact -- $rc {
                2 {
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    }   
                    PutsCtrlSock $s "LIST$ftp(Dir)"
                    set ftp(State) list_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        list_sent {
            switch -exact -- $rc {
                1 {
                    set ftp(State) list_close
                }
                default {  
                    if { [string equal $ftp(Mode) "passive"] } {
                        unset ftp(state.data)
                    }    
                    set errmsg "Error getting directory listing!"
                    set complete_with 0
                }
            }
        }
        list_close {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error receiving list!"
                    set complete_with 0
                }
            }
        }
        size {
            PutsCtrlSock $s "SIZE $ftp(File)"
            set ftp(State) size_sent
        }
        size_sent {
            switch -exact -- $rc {
                2 {
                    regexp "^\[0-9\]+ (.*)$" $buffer all ftp(FileSize)
                    set complete_with 1
                }
                default {
                    set errmsg "Error getting file size!"
                    set complete_with 0
                }
            }
        } 
        modtime {
            PutsCtrlSock $s "MDTM $ftp(File)"
            set ftp(State) modtime_sent
        }  
        modtime_sent {
            switch -exact -- $rc {
                2 {
                    regexp "^\[0-9\]+ (.*)$" $buffer all ftp(DateTime)
                    set complete_with 1
                }
                default {
                    set errmsg "Error getting modification time!"
                    set complete_with 0
                }
            }
        } 
        pwd {
            PutsCtrlSock $s "PWD"
            set ftp(State) pwd_sent
        }
        pwd_sent {
            switch -exact -- $rc {
                2 {
                    regexp "^.*\"(.*)\"" $buffer temp ftp(Dir)
                    set complete_with 1
                }
                default {
                    set errmsg "Error getting working dir!"
                    set complete_with 0
                }
            }
        }
        cd {
            PutsCtrlSock $s "CWD$ftp(Dir)"
            set ftp(State) cd_sent
        }
        cd_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error changing directory!"
                    set complete_with 0
                }
            }
        }
        mkdir {
            PutsCtrlSock $s "MKD $ftp(Dir)"
            set ftp(State) mkdir_sent
        }
        mkdir_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error making dir \"$ftp(Dir)\"!"
                    set complete_with 0
                }
            }
        }
        rmdir {
            PutsCtrlSock $s "RMD $ftp(Dir)"
            set ftp(State) rmdir_sent
        }
        rmdir_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error removing directory!"
                    set complete_with 0
                }
            }
        }
        delete {
            PutsCtrlSock $s "DELE $ftp(File)"
            set ftp(State) delete_sent
        }
        delete_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error deleting file \"$ftp(File)\"!"
                    set complete_with 0
                }
            }
        }
        rename {
            PutsCtrlSock $s "RNFR $ftp(RenameFrom)"
            set ftp(State) rename_to
        }
        rename_to {
            switch -exact -- $rc {
                3 {
                    PutsCtrlSock $s "RNTO $ftp(RenameTo)"
                    set ftp(State) rename_sent
                }
                default {
                    set errmsg "Error renaming file \"$ftp(RenameFrom)\"!"
                    set complete_with 0
                }
            }
        }
        rename_sent {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error renaming file \"$ftp(RenameFrom)\"!"
                    set complete_with 0
                }
            }
        }
        put_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) put_open
            } else {
                set errmsg "Error setting port!"
            }
        }
        put_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) put_open
        }
        put_open {
            switch -exact -- $rc {
                2 {
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    } 
                    PutsCtrlSock $s "STOR $ftp(RemoteFilename)"
                    set ftp(State) put_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        put_sent {
            switch -exact -- $rc {
                1 {
                    set ftp(State) put_close
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        # close already opened DataConnection
                        unset ftp(state.data)
                    }  
                    set errmsg "Error opening connection!"
                    set complete_with 0
                }
            }
        }
        put_close {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error storing file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        append_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) append_open
            } else {
                set errmsg "Error setting port!"
            }
        }
        append_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) append_open
        }
        append_open {
            switch -exact -- $rc {
                2 {
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    }   
                    PutsCtrlSock $s "APPE $ftp(RemoteFilename)"
                    set ftp(State) append_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        append_sent {
            switch -exact -- $rc {
                1 {
                    set ftp(State) append_close
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        # close already opened DataConnection
                        unset ftp(state.data)
                    }  
                    set errmsg "Error opening connection!"
                    set complete_with 0
                }
            }
        }
        append_close {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error storing file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        reget_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) reget_restart
            } else {
                set errmsg "Error setting port!"
            }
        }
        reget_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) reget_restart
        }
        reget_restart {
            switch -exact -- $rc {
                2 { 
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    }   
                    if { $ftp(FileSize) != 0 } {
                        PutsCtrlSock $s "REST $ftp(FileSize)"
                        set ftp(State) reget_open
                    } else {
                        PutsCtrlSock $s "RETR $ftp(RemoteFilename)"
                        set ftp(State) reget_sent
                    } 
                }
                default {
                    set errmsg "Error restarting filetransfer of \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        reget_open {
            switch -exact -- $rc {
                2 -
                3 {
                    PutsCtrlSock $s "RETR $ftp(RemoteFilename)"
                    set ftp(State) reget_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        reget_sent {
            switch -exact -- $rc {
                1 {
                    set ftp(State) reget_close
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        # close already opened DataConnection
                        unset ftp(state.data)
                    }  
                    set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        reget_close {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        get_active {
            if { [OpenActiveConn $s] } {
                PutsCtrlSock $s "PORT $ftp(LocalAddr),$ftp(DataPort)"
                set ftp(State) get_open
            } else {
                set errmsg "Error setting port!"
            }
        } 
        get_passive {
            PutsCtrlSock $s "PASV"
            set ftp(State) get_open
        }
        get_open {
            switch -exact -- $rc {
                2 -
                3 {
                    if { [string equal $ftp(Mode) "passive"] } {
                        if { ![OpenPassiveConn $s $buffer] } {
                            set errmsg "Error setting PASSIVE mode!"
                            set complete_with 0
                        }
                    }   
                    PutsCtrlSock $s "RETR $ftp(RemoteFilename)"
                    set ftp(State) get_sent
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        set errmsg "Error setting PASSIVE mode!"
                    } else {
                        set errmsg "Error setting port!"
                    }  
                    set complete_with 0
                }
            }
        }
        get_sent {
            switch -exact -- $rc {
                1 {
                    set ftp(State) get_close
                }
                default {
                    if { [string equal $ftp(Mode) "passive"] } {
                        # close already opened DataConnection
                        unset ftp(state.data)
                    }  
                    set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
        get_close {
            switch -exact -- $rc {
                2 {
                    set complete_with 1
                }
                default {
                    set errmsg "Error retrieving file \"$ftp(RemoteFilename)\"!"
                    set complete_with 0
                }
            }
        }
    }

    # finish waiting 
    if { [info exists complete_with] } {
        WaitComplete $s $complete_with
    }

    # display control channel message
    if { [info exists buffer] } {
        if { $VERBOSE } {
            foreach line [split $buffer \n] {
                DisplayMsg $s "C: $line" control
            }
        }
    }
	
    # display error message
    if { [info exists errmsg] } {
        set ftp(Error) $errmsg
        DisplayMsg $s $errmsg error
    }

    # enable fileevent on control socket again
    #fileevent $ftp(CtrlSock) readable [list ::ftp::StateHandler $ftp(CtrlSock)]

}

#############################################################################
#
# Type --
#
# REPRESENTATION TYPE - Sets the file transfer type to ascii or binary.
# (exported)
#
# Arguments:
# type - 		specifies the representation type (ascii|binary)
# 
# Returns:
# type	-		returns the current type or {} if an error occurs

proc ftp::Type {s {type ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return {}
    }

    # return current type
    if { $type == "" } {
        return $ftp(Type)
    }

    # save current type
    set old_type $ftp(Type) 
	
    set ftp(Type) $type
    set ftp(State) type
    StateHandler $s
	
    # wait for synchronization
    set rc [WaitOrTimeout $s]
    if { $rc } {
        return $ftp(Type)
    } else {
        # restore old type
        set ftp(Type) $old_type
        return {}
    }
}

#############################################################################
#
# NList --
#
# NAME LIST - This command causes a directory listing to be sent from
# server to user site.
# (exported)
# 
# Arguments:
# dir - 		The $dir should specify a directory or other system 
#			specific file group descriptor; a null argument 
#			implies the current directory. 
#
# Arguments:
# dir - 		directory to list 
# 
# Returns:
# sorted list of files or {} if listing fails

proc ftp::NList {s { dir ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return {}
    }

    set ftp(List) {}
    if { $dir == "" } {
        set ftp(Dir) ""
    } else {
        set ftp(Dir) " $dir"
    }

    # save current type and force ascii mode
    set old_type $ftp(Type)
    if { $ftp(Type) != "ascii" } {
        Type $s ascii
    }

    set ftp(State) nlist_$ftp(Mode)
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]

    # restore old type
    if { [Type $s] != $old_type } {
        Type $s $old_type
    }

    unset ftp(Dir)
    if { $rc } { 
        return [lsort $ftp(List)]
    } else {
        CloseDataConn $s
        return {}
    }
}

#############################################################################
#
# List --
#
# LIST - This command causes a list to be sent from the server
# to user site.
# (exported)
# 
# Arguments:
# dir - 		If the $dir specifies a directory or other group of 
#			files, the server should transfer a list of files in 
#			the specified directory. If the $dir specifies a file
#			then the server should send current information on the
# 			file.  A null argument implies the user's current 
#			working or default directory.  
# 
# Returns:
# list of files or {} if listing fails

proc ftp::List {s {dir ""}} {

    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return {}
    }

    set ftp(List) {}
    if { $dir == "" } {
        set ftp(Dir) ""
    } else {
        set ftp(Dir) " $dir"
    }

    # save current type and force ascii mode

    set old_type $ftp(Type)
    if { ![string equal "$ftp(Type)" "ascii"] } {
        Type $s ascii
    }

    set ftp(State) list_$ftp(Mode)
    StateHandler $s

    # wait for synchronization

    set rc [WaitOrTimeout $s]

    # restore old type

    if { ![string equal "[Type $s]" "$old_type"] } {
        Type $s $old_type
    }

    unset ftp(Dir)
    if { $rc } { 
		
        # clear "total"-line

        set l [split $ftp(List) "\n"]
        set index [lsearch -regexp $l "^total"]
        if { $index != "-1" } { 
            set l [lreplace $l $index $index]
        }

        # clear blank line

        set index [lsearch -regexp $l "^$"]
        if { $index != "-1" } { 
            set l [lreplace $l $index $index]
        }

        return $l
    } else {
        CloseDataConn $s
        return {}
    }
}

#############################################################################
#
# FileSize --
#
# REMOTE FILE SIZE - This command gets the file size of the
# file on the remote machine. 
# ATTENTION! Doesn't work properly in ascci mode!
# (exported)
# 
# Arguments:
# filename - 		specifies the remote file name
# 
# Returns:
# size -		files size in bytes or {} in error cases

proc ftp::FileSize {s {filename ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return {}
    }
	
    if { $filename == "" } {
        return {}
    } 

    set ftp(File) $filename
    set ftp(FileSize) 0
	
    set ftp(State) size
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
	
    unset ftp(File)
		
    if { $rc } {
        return $ftp(FileSize)
    } else {
        return {}
    }
}


#############################################################################
#
# ModTime --
#
# MODIFICATION TIME - This command gets the last modification time of the
# file on the remote machine.
# (exported)
# 
# Arguments:
# filename - 		specifies the remote file name
# 
# Returns:
# clock -		files date and time as a system-depentend integer
#			value in seconds (see tcls clock command) or {} in 
#			error cases

proc ftp::ModTime {s {filename ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        } 
        return {}
    }
	
    if { $filename == "" } {
        return {}
    } 

    set ftp(File) $filename
    set ftp(DateTime) ""
	
    set ftp(State) modtime
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
	
    unset ftp(File)
		
    if { $rc } {
        scan $ftp(DateTime) "%4s%2s%2s%2s%2s%2s" year month day hour min sec
        set clock [clock scan "$month/$day/$year $hour:$min:$sec" -gmt 1]
        unset year month day hour min sec
        return $clock
    } else {
        return {}
    }
}

#############################################################################
#
# Pwd --
#
# PRINT WORKING DIRECTORY - Causes the name of the current working directory.
# (exported)
# 
# Arguments:
# None.
# 
# Returns:
# current directory name

proc ftp::Pwd {s } {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return {}
    }

    set ftp(Dir) {}

    set ftp(State) pwd
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
	
    if { $rc } {
        return $ftp(Dir)
    } else {
        return {}
    }
}

#############################################################################
#
# Cd --
#   
# CHANGE DIRECTORY - Sets the working directory on the server host.
# (exported)
# 
# Arguments:
# dir -			pathname specifying a directory
#
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::Cd {s {dir ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        if { ![string is digit -strict $s] } {
            DisplayMsg $s "Bad connection name \"$s\"" error
        } else {
            DisplayMsg $s "Not connected!" error
        }
        return 0
    }

    if { $dir == "" } {
        set ftp(Dir) ""
    } else {
        set ftp(Dir) " $dir"
    }

    set ftp(State) cd
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(Dir)
	
    if { $rc } {
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# MkDir --
#
# MAKE DIRECTORY - This command causes the directory specified in the $dir
# to be created as a directory (if the $dir is absolute) or as a subdirectory
# of the current working directory (if the $dir is relative).
# (exported)
# 
# Arguments:
# dir -			new directory name
#
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::MkDir {s dir} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(Dir) $dir

    set ftp(State) mkdir
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(Dir)
	
    if { $rc } {
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# RmDir --
#
# REMOVE DIRECTORY - This command causes the directory specified in $dir to 
# be removed as a directory (if the $dir is absolute) or as a 
# subdirectory of the current working directory (if the $dir is relative).
# (exported)
#
# Arguments:
# dir -			directory name
#
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::RmDir {s dir} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(Dir) $dir

    set ftp(State) rmdir
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(Dir)
	
    if { $rc } {
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# Delete --
#
# DELETE - This command causes the file specified in $file to be deleted at 
# the server site.
# (exported)
# 
# Arguments:
# file -			file name
#
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::Delete {s file} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(File) $file

    set ftp(State) delete
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(File)
	
    if { $rc } {
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# Rename --
#
# RENAME FROM TO - This command causes the file specified in $from to be 
# renamed at the server site.
# (exported)
# 
# Arguments:
# from -			specifies the old file name of the file which 
#				is to be renamed
# to -				specifies the new file name of the file 
#				specified in the $from agument
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::Rename {s from to} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(RenameFrom) $from
    set ftp(RenameTo) $to

    set ftp(State) rename

    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(RenameFrom)
    unset ftp(RenameTo)
	
    if { $rc } {
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# ElapsedTime --
#
# Gets the elapsed time for file transfer
# 
# Arguments:
# stop_time - 		ending time

proc ftp::ElapsedTime {s stop_time} {
    upvar ::ftp::ftp$s ftp

    set elapsed [expr {$stop_time - $ftp(Start_Time)}]
    if { $elapsed == 0 } {
        set elapsed 1
    }
    set persec [expr {$ftp(Total) / $elapsed}]
    DisplayMsg $s "$ftp(Total) bytes sent in $elapsed seconds ($persec Bytes/s)"
    return
}

#############################################################################
#
# PUT --
#
# STORE DATA - Causes the server to accept the data transferred via the data 
# connection and to store the data as a file at the server site.  If the file
# exists at the server site, then its contents shall be replaced by the data
# being transferred.  A new file is created at the server site if the file
# does not already exist.
# (exported)
#
# Arguments:
# source -			local file name
# dest -			remote file name, if unspecified, ftp assigns
#				the local file name.
# Returns:
# 0 -			file not stored
# 1 - 			OK

proc ftp::Put {s source {dest ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    if { ![file exists $source] } {
        DisplayMsg $s "File \"$source\" not exist" error
        return 0
    }
		
    if { $dest == "" } {
        set dest $source
    }

    set ftp(LocalFilename) $source
    set ftp(RemoteFilename) $dest

    set ftp(SourceCI) [open $ftp(LocalFilename) r]
    if { [string equal $ftp(Type) "ascii"] } {
        fconfigure $ftp(SourceCI) -buffering line -blocking 1
    } else {
        fconfigure $ftp(SourceCI) -buffering line -translation binary -blocking 1
    }

    set ftp(State) put_$ftp(Mode)
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
    if { $rc } {
        ElapsedTime $s [clock seconds]
        return 1
    } else {
        CloseDataConn $s
        return 0
    }
}

#############################################################################
#
# APPEND --
#
# APPEND DATA - Causes the server to accept the data transferred via the data 
# connection and to store the data as a file at the server site.  If the file
# exists at the server site, then the data shall be appended to that file; 
# otherwise the file specified in the pathname shall be created at the
# server site.
# (exported)
#
# Arguments:
# source -			local file name
# dest -			remote file name, if unspecified, ftp assigns
#				the local file name.
# Returns:
# 0 -			file not stored
# 1 - 			OK

proc ftp::Append {s source {dest ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    if { ![file exists $source] } {
        DisplayMsg $s "File \"$source\" not exist" error
        return 0
    }
			
    if { $dest == "" } {
        set dest $source
    }

    set ftp(LocalFilename) $source
    set ftp(RemoteFilename) $dest

    set ftp(SourceCI) [open $ftp(LocalFilename) r]
    if { [string equal $ftp(Type) "ascii"] } {
        fconfigure $ftp(SourceCI) -buffering line -blocking 1
    } else {
        fconfigure $ftp(SourceCI) -buffering line -translation binary -blocking 1
    }

    set ftp(State) append_$ftp(Mode)
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
    if { $rc } {
        ElapsedTime $s [clock seconds]
        return 1
    } else {
        CloseDataConn $s
        return 0
    }
}


#############################################################################
#
# Get --
#
# RETRIEVE DATA - Causes the server to transfer a copy of the specified file
# to the local site at the other end of the data connection.
# (exported)
#
# Arguments:
# source -			remote file name
# dest -			local file name, if unspecified, ftp assigns
#				the remote file name.
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc ftp::Get {s source {dest ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    if { $dest == "" } {
        set dest $source
    }

    set ftp(RemoteFilename) $source
    set ftp(LocalFilename) $dest

    set ftp(State) get_$ftp(Mode)
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
    if { $rc } {
        ElapsedTime $s [clock seconds]
        return 1
    } else {
        CloseDataConn $s
        return 0
    }
}

#############################################################################
#
# Reget --
#
# RESTART RETRIEVING DATA - Causes the server to transfer a copy of the specified file
# to the local site at the other end of the data connection like get but skips over 
# the file to the specified data checkpoint. 
# (exported)
#
# Arguments:
# source -			remote file name
# dest -			local file name, if unspecified, ftp assigns
#				the remote file name.
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc ftp::Reget {s source {dest ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    if { $dest == "" } {
        set dest $source
    }

    set ftp(RemoteFilename) $source
    set ftp(LocalFilename) $dest

    if { [file exists $ftp(LocalFilename)] } {
        set ftp(FileSize) [file size $ftp(LocalFilename)]
    } else {
        set ftp(FileSize) 0
    }
	
    set ftp(State) reget_$ftp(Mode)
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s]
    if { $rc } {
        ElapsedTime $s [clock seconds]
        return 1
    } else {
        CloseDataConn $s
        return 0
    }
}

#############################################################################
#
# Newer --
#
# GET NEWER DATA - Get the file only if the modification time of the remote 
# file is more recent that the file on the current system. If the file does
# not exist on the current system, the remote file is considered newer.
# Otherwise, this command is identical to get. 
# (exported)
#
# Arguments:
# source -			remote file name
# dest -			local file name, if unspecified, ftp assigns
#				the remote file name.
#
# Returns:
# 0 -			file not retrieved
# 1 - 			OK

proc ftp::Newer {s source {dest ""}} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    if { $dest == "" } {
        set dest $source
    }

    set ftp(RemoteFilename) $source
    set ftp(LocalFilename) $dest

    # get remote modification time
    set rmt [ModTime $s $ftp(RemoteFilename)]
    if { $rmt == "-1" } {
        return 0
    }

    # get local modification time
    if { [file exists $ftp(LocalFilename)] } {
        set lmt [file mtime $ftp(LocalFilename)]
    } else {
        set lmt 0
    }
	
    # remote file is older than local file
    if { $rmt < $lmt } {
        return 0
    }

    # remote file is newer than local file or local file doesn't exist
    # get it
    set rc [Get $s $ftp(RemoteFilename) $ftp(LocalFilename)]
    return $rc
		
}

#############################################################################
#
# Quote -- 
#
# The arguments specified are sent, verbatim, to the remote ftp server.     
#
# Arguments:
# 	arg1 arg2 ...
#
# Returns:
#  string sent back by the remote ftp server or null string if any error
#

proc ftp::Quote {s args} {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(Cmd) $args

    set ftp(State) quote
    StateHandler $s

    # wait for synchronization
    set rc [WaitOrTimeout $s] 

    unset ftp(Cmd)

    if { $rc } {
        return $ftp(Quote)
    } else {
        return {}
    }
}


#############################################################################
#
# Abort -- 
#
# ABORT - Tells the server to abort the previous ftp service command and 
# any associated transfer of data. The control connection is not to be 
# closed by the server, but the data connection must be closed.
# 
# NOTE: This procedure doesn't work properly. Thus the ftp::Abort command
# is no longer available!
#
# Arguments:
# None.
#
# Returns:
# 0 -			ERROR
# 1 - 			OK
#
# proc Abort {} {
#
# }

#############################################################################
#
# Close -- 
#
# Terminates a ftp session and if file transfer is not in progress, the server
# closes the control connection.  If file transfer is in progress, the 
# connection will remain open for result response and the server will then
# close it. 
# (exported)
# 
# Arguments:
# None.
#
# Returns:
# 0 -			ERROR
# 1 - 			OK

proc ftp::Close {s } {
    upvar ::ftp::ftp$s ftp

    if { ![info exists ftp(State)] } {
        DisplayMsg $s "Not connected!" error
        return 0
    }

    set ftp(State) quit
    StateHandler $s

    # wait for synchronization
    WaitOrTimeout $s

    catch {close $ftp(CtrlSock)}
    catch {unset ftp}
}

#############################################################################
#
# Open --
#
# Starts the ftp session and sets up a ftp control connection.
# (exported)
# 
# Arguments:
# server - 		The ftp server hostname.
# user -		A string identifying the user. The user identification 
#			is that which is required by the server for access to 
#			its file system.  
# passwd -		A string specifying the user's password.
# options -		-blocksize size		writes "size" bytes at once
#						(default 4096)
#			-timeout seconds	if non-zero, sets up timeout to
#						occur after specified number of
#						seconds (default 120)
#			-progress proc		procedure name that handles callbacks
#						(no default)  
#			-output proc		procedure name that handles output
#						(no default)  
#			-mode mode		switch active or passive file transfer
#						(default active)
#			-port number		alternative port (default 21)
# 
# Returns:
# 0 -			Not logged in
# 1 - 			User logged in

proc ftp::Open {server user passwd args} {
    variable DEBUG 
    variable VERBOSE
    variable serial

    set s $serial
    incr serial
    upvar ::ftp::ftp$s ftp
#    if { [info exists ftp(State)] } {
#        DisplayMsg $s "Mmh, another attempt to open a new connection? There is already a hot wire!" error
#        return 0
#    }

    # default NO DEBUG
    if { ![info exists DEBUG] } {
        set DEBUG 0
    }

    # default NO VERBOSE
    if { ![info exists VERBOSE] } {
        set VERBOSE 0
    }
	
    if { $DEBUG } {
        DisplayMsg $s "Starting new connection with: "
    }
	
    set ftp(User) 		$user
    set ftp(Passwd) 	$passwd
    set ftp(RemoteHost) 	$server
    set ftp(LocalHost) 	[info hostname]
    set ftp(DataPort) 	0
    set ftp(Type) 		{}
    set ftp(Error) 		{}
    set ftp(Progress) 	{}
    set ftp(Output) 	{}
    set ftp(Blocksize) 	4096	
    set ftp(Timeout) 	600	
    set ftp(Mode) 		active	
    set ftp(Port) 		21	

    set ftp(State) 		user
	
    # set state var
    set ftp(state.control) ""
	
    # Get and set possible options
    set options {-blocksize -timeout -mode -port -progress -output}
    foreach {option value} $args {
        if { [lsearch -exact $options $option] != "-1" } {
            if { $DEBUG } {
                DisplayMsg $s "  $option = $value"
            }
            regexp {^-(.?)(.*)$} $option all first rest
            set option "[string toupper $first]$rest"
            set ftp($option) $value
        } 
    }
    if { $DEBUG && ([llength $args] == 0) } {
        DisplayMsg $s "  no option"
    }

    # No call of StateHandler is required at this time.
    # StateHandler at first time is called automatically
    # by a fileevent for the control channel.

    # Try to open a control connection
    if { ![OpenControlConn $s] } {
        return -1
    }

    # waits for synchronization
    #   0 ... Not logged in
    #   1 ... User logged in
    if { [WaitOrTimeout $s] } {
        # default type is binary
        Type $s binary
        return $s
    } else {
        # close connection if not logged in
        Close $s
        return -1
    }
    return $s;
}

#############################################################################
#
# CopyNext --
#
# recursive background copy procedure for ascii/binary file I/O
# 
# Arguments:
# bytes - 		indicates how many bytes were written on $ftp(DestCI)

proc ftp::CopyNext {s bytes {error {}}} {
    upvar ::ftp::ftp$s ftp
    variable DEBUG
    variable VERBOSE

    # summary bytes

    incr ftp(Total) $bytes

    # callback for progress bar procedure
    
    if { ([info exists ftp(Progress)]) && ([info commands [lindex $ftp(Progress) 0]] != "") } { 
        eval $ftp(Progress) $ftp(Total)
    }

    # setup new timeout handler

    after cancel $ftp(Wait)
    set ftp(Wait) [after [expr {$ftp(Timeout) * 1000}] [namespace current]::Timeout $s]

    if { $DEBUG } {
        DisplayMsg $s "-> $ftp(Total) bytes $ftp(SourceCI) -> $ftp(DestCI)" 
    }

    if { $error != "" } {
        catch {close $ftp(DestCI)}
        catch {close $ftp(SourceCI)}
        unset ftp(state.data)
        DisplayMsg $s $error error

    } elseif { [eof $ftp(SourceCI)] } {
        close $ftp(DestCI)
        close $ftp(SourceCI)
        unset ftp(state.data)
        if { $VERBOSE } {
            DisplayMsg $s "D: Port closed" data
        }

    } else {
        fcopy $ftp(SourceCI) $ftp(DestCI) -command [list [namespace current]::CopyNext $s] -size $ftp(Blocksize)

    }
    return
}

#############################################################################
#
# HandleList --
#
# Handles ascii/binary data transfer for Put and Get 
# 
# Arguments:
# sock - 		socket name (data channel)

proc ftp::HandleData {s sock} {
    upvar ::ftp::ftp$s ftp

    # Turn off any fileevent handlers

    fileevent $sock writable {}		
    fileevent $sock readable {}

    # create local file for ftp::Get 

    if { [regexp "^get" $ftp(State)] } {
        set rc [catch {set ftp(DestCI) [open $ftp(LocalFilename) w]} msg]
        if { $rc != 0 } {
            DisplayMsg $s "$msg" error
            return 0
        }
        if { [string equal $ftp(Type) "ascii"] } {
            fconfigure $ftp(DestCI) -buffering line -blocking 1
        } else {
            fconfigure $ftp(DestCI) -buffering line -translation binary -blocking 1
        }
    }	

    # append local file for ftp::Reget 

    if { [regexp "^reget" $ftp(State)] } {
        set rc [catch {set ftp(DestCI) [open $ftp(LocalFilename) a]} msg]
        if { $rc != 0 } {
            DisplayMsg $s "$msg" error
            return 0
        }
        if { [string equal $ftp(Type) "ascii"] } {
            fconfigure $ftp(DestCI) -buffering line -blocking 1
        } else {
            fconfigure $ftp(DestCI) -buffering line -translation binary -blocking 1
        }
    }	

    # perform fcopy

    set ftp(Total) 0
    set ftp(Start_Time) [clock seconds]
    fcopy $ftp(SourceCI) $ftp(DestCI) -command [list [namespace current]::CopyNext $s] -size $ftp(Blocksize)
    return
}

#############################################################################
#
# HandleList --
#
# Handles ascii data transfer for list commands
# 
# Arguments:
# sock - 		socket name (data channel)

proc ftp::HandleList {s sock} {
    upvar ::ftp::ftp$s ftp
    variable VERBOSE

    if { ![eof $sock] } {
        set buffer [read $sock]
        if { $buffer != "" } {
            set ftp(List) [append ftp(List) $buffer]
        }	
    } else {
        close $sock
        unset ftp(state.data)
        if { $VERBOSE } {
            DisplayMsg $s "D: Port closed" data
        }
    }
    return
}

############################################################################
#
# CloseDataConn -- 
#
# Closes all sockets and files used by the data conection
#
# Arguments:
# None.
#
# Returns:
# None.
#
proc ftp::CloseDataConn {s } {
    upvar ::ftp::ftp$s ftp

    catch {after cancel $ftp(Wait)}
    catch {fileevent $ftp(DataSock) readable {}}
    catch {close $ftp(DataSock); unset ftp(DataSock)}
    catch {close $ftp(DestCI); unset ftp(DestCI)} 
    catch {close $ftp(SourceCI); unset ftp(SourceCI)}
    catch {close $ftp(DummySock); unset ftp(DummySock)}
    return
}

#############################################################################
#
# InitDataConn --
#
# Configures new data channel for connection to ftp server 
# ATTENTION! The new data channel "sock" is not the same as the 
# server channel, it's a dummy.
# 
# Arguments:
# sock -		the name of the new channel
# addr -		the address, in network address notation, 
#			of the client's host,
# port -		the client's port number

proc ftp::InitDataConn {s sock addr port} {
    upvar ::ftp::ftp$s ftp
    variable VERBOSE

    # If the new channel is accepted, the dummy channel will be closed

    catch {close $ftp(DummySock); unset ftp(DummySock)}

    set ftp(state.data) 0

    # Configure translation mode

    if { [string equal $ftp(Type) "ascii"] } {
        fconfigure $sock -buffering line -blocking 1
    } else {
        fconfigure $sock -buffering line -translation binary -blocking 1
    }

    # assign fileevent handlers, source and destination CI (Channel Identifier)

    switch -regexp -- $ftp(State) {
        list {
            fileevent $sock readable [list [namespace current]::HandleList $s $sock]
            set ftp(SourceCI) $sock		  
        }
        get {
            fileevent $sock readable [list [namespace current]::HandleData $s $sock]
            set ftp(SourceCI) $sock			  
        }
        append -
        put {
            fileevent $sock writable [list [namespace current]::HandleData $s $sock]
            set ftp(DestCI) $sock			  
        }
    }

    if { $VERBOSE } {
        DisplayMsg $s "D: Connection from $addr:$port" data
    }
    return
}

#############################################################################
#
# OpenActiveConn --
#
# Opens a ftp data connection
# 
# Arguments:
# None.
# 
# Returns:
# 0 -			no connection
# 1 - 			connection established

proc ftp::OpenActiveConn {s } {
    upvar ::ftp::ftp$s ftp
    variable VERBOSE

    # Port address 0 is a dummy used to give the server the responsibility 
    # of getting free new port addresses for every data transfer.
    
    set rc [catch {set ftp(DummySock) [socket -server [list [namespace current]::InitDataConn $s] 0]} msg]
    if { $rc != 0 } {
        DisplayMsg $s "$msg" error
        return 0
    }

    # get a new local port address for data transfer and convert it to a format
    # which is useable by the PORT command

    set p [lindex [fconfigure $ftp(DummySock) -sockname] 2]
    if { $VERBOSE } {
        DisplayMsg $s "D: Port is $p" data
    }
    set ftp(DataPort) "[expr {$p / 256}],[expr {$p % 256}]"

    return 1
}

#############################################################################
#
# OpenPassiveConn --
#
# Opens a ftp data connection
# 
# Arguments:
# buffer - returned line from server control connection 
# 
# Returns:
# 0 -			no connection
# 1 - 			connection established

proc ftp::OpenPassiveConn {s buffer} {
    upvar ::ftp::ftp$s ftp

    if { [regexp {([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)} $buffer all a1 a2 a3 a4 p1 p2] } {
        set ftp(LocalAddr) "$a1.$a2.$a3.$a4"
        set ftp(DataPort) "[expr {$p1 * 256 + $p2}]"

        # establish data connection for passive mode

        set rc [catch {set ftp(DataSock) [socket $ftp(LocalAddr) $ftp(DataPort)]} msg]
        if { $rc != 0 } {
            DisplayMsg $s "$msg" error
            return 0
        }

        InitDataConn $s $ftp(DataSock) $ftp(LocalAddr) $ftp(DataPort)
        return 1
    } else {
        return 0
    }
}

#############################################################################
#
# OpenControlConn --
#
# Opens a ftp control connection
# 
# Arguments:
# None.
# 
# Returns:
# 0 -			no connection
# 1 - 			connection established

proc ftp::OpenControlConn {s } {
    upvar ::ftp::ftp$s ftp
    variable DEBUG
    variable VERBOSE

    # open a control channel

    set rc [catch {set ftp(CtrlSock) [socket $ftp(RemoteHost) $ftp(Port)]} msg]
    if { $rc != 0 } {
        if { $VERBOSE } {
            DisplayMsg $s "C: No connection to server!" error
        }
        if { $DEBUG } {
            DisplayMsg $s "[list $msg]" error
        }
        unset ftp(State)
        return 0
    }

    # configure control channel

    fconfigure $ftp(CtrlSock) -buffering line -blocking 1 -translation {auto crlf}
    fileevent $ftp(CtrlSock) readable [list [namespace current]::StateHandler $s $ftp(CtrlSock)]
	
    # prepare local ip address for PORT command (convert pointed format
    # to comma format)

    set ftp(LocalAddr) [lindex [fconfigure $ftp(CtrlSock) -sockname] 0]
    regsub -all "\[.\]" $ftp(LocalAddr) "," ftp(LocalAddr) 

    # report ready message

    set peer [fconfigure $ftp(CtrlSock) -peername]
    if { $VERBOSE } {
        DisplayMsg $s "C: Connection from [lindex $peer 0]:[lindex $peer 2]" control
    }
	
    return 1
}

# ?????? Hmm, how to do multithreaded for tkcon?
# added TkCon support
# TkCon is (c) 1995-1999 Jeffrey Hobbs, http://www.purl.org/net/hobbs/tcl/script/tkcon/
# started with: tkcon -load ftp
if { [string equal [uplevel "#0" {info commands tkcon}] "tkcon"] } {

    # new ftp::List proc makes the output more readable
    proc ::ftp::__ftp_ls {args} {
        foreach i [::ftp::List_org $args] {
            puts $i
        }
    }

    # rename the original ftp::List procedure
    rename ::ftp::List ::ftp::List_org

    alias ::ftp::List	::ftp::__ftp_ls
    alias bye		catch {::ftp::Close; exit}	

    set ::ftp::VERBOSE 1
    set ::ftp::DEBUG 0
}

