package require InsightToolkit
#
# Exersize the PrintSelf of each class
#
set b [itkStringStream]
set allClasses [info command itk*_New]

foreach class $allClasses {
    puts "--------------- $class ---------------"
    catch {set a [$class]; $a Print [$b GetStream]; puts "[$b GetString]"}
}

exit

