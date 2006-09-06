package require InsightToolkit

if {$argc == 0} {
    set fileid stdout
} else {
    set fileid [open [lindex $argv 0] w]
}

set b [itkStringStream]
set allClasses [info command itk*]

foreach class $allClasses {
    puts $fileid "--------------- $class ---------------"
}

exit

