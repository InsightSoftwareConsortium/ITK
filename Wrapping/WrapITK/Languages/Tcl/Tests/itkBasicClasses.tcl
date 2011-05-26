#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

package require InsightToolkit
package require itktesting

itkStringStream stream

set dir [itkDirectory_New]
$dir Load "."
set numFiles [$dir GetNumberOfFiles]
for { set i 1 } { $i < $numFiles } { incr i } {
   puts [$dir GetFile $i]
}
$dir Print [stream GetStream]
puts [stream GetString]
set loader [itkDynamicLoader_New]
set o [itkDynamicLoader_OpenLibrary "/lib/libc.so"]
itkDynamicLoader_CloseLibrary $o
stream Reset
puts $o
set o [itkDynamicLoader_OpenLibrary  "/windows/SYSTEM32/mfc70.dll"]
itkDynamicLoader_CloseLibrary $o
puts $o
exit
