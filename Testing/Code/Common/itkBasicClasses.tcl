package require InsightToolkit
package require itktesting

set dir [itkDirectory_New]
$dir Load "."
set numFiles [$dir GetNumberOfFiles]
for { set i 1 } { $i < $numFiles } { incr i } {
   puts [$dir GetFile $i]
}

set loader [itkDynamicLoader_New]
set o [itkDynamicLoader_OpenLibrary "/lib/libc.so"]
itkDynamicLoader_CloseLibrary $o
puts $o
set o [itkDynamicLoader_OpenLibrary  "/windows/SYSTEM32/mfc70.dll"]
itkDynamicLoader_CloseLibrary $o
puts $o
exit
