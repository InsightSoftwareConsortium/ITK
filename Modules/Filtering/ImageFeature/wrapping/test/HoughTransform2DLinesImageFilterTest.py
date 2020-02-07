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

#
#  Example on the use of the LaplacianImageFilter
#

import itk
from sys import argv
itk.auto_progress(2)

edges = itk.imread(argv[1], itk.F)
houghF = itk.HoughTransform2DLinesImageFilter[itk.F, itk.F].New()
houghF.SetInput(edges)
houghF.SetAngleResolution(100)
houghF.SetNumberOfLines(2)
houghF.Update()
detected_lines = houghF.GetLines()

# Check that we detected 2 lines as we requested.
assert len(detected_lines) == 2

# Check that we can access the line by index.
line1 = detected_lines[0]

# Check that we can access the points of the line
assert len(line1.GetPoints()) == 2
