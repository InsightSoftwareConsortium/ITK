package require InsightToolkit
#
# Exercise the PrintSelf of each class
#
if {$argc == 0} {
    set fileid stdout
} else {
    set fileid [open [lindex $argv 0] w]
}

set b [itkStringStream]
set allClasses [info command itk*_New]

foreach class $allClasses {
    puts $fileid "--------------- $class ---------------"
    catch {set a [$class]; $a Print [$b GetStream]; puts $fileid "[$b GetString]"; $b Reset}
}

exit

