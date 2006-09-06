package require InsightToolkit
set d [itkDirectory_New]
$d Load "."
set n [$d GetNumberOfFiles]
for {set i 1} {$i < $n} {incr i} {
   puts [$d GetFile $i]
}




