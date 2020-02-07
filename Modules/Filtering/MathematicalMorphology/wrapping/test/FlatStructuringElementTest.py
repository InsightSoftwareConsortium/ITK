#==========================================================================
#
#   Copyright NumFOCUS
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

import itk
from sys import argv, exit
itk.auto_progress(2)

if argv[1] == "Ball":
    print("Ball")
    strel = itk.FlatStructuringElement[2].Ball(int(argv[2]))
elif argv[1] == "Box":
    print("Box")
    strel = itk.FlatStructuringElement[2].Box(int(argv[2]))
else:
    print("invalid arguement: " + argv[1])
    exit(1)
