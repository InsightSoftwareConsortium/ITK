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

#
#  Example on the use of the MedianImageFilter
#

import itk
from sys import argv

input_filename = argv[1]
output_filename = argv[2]
radius = int(argv[3])

reader = itk.ImageFileReader.IUC2.New(FileName=input_filename)

# test the deduction of the template parameter from the input
filt = itk.MedianImageFilter.New(reader, Radius=radius)
filt.Update()
result = filt.GetOutput()
watcher = itk.XMLFilterWatcher(filt, "filter")

# test the update of the filter with the snake case function
# and the setting of parameter inside it
result = itk.median_image_filter(reader, radius=radius)

# test the write method
itk.imwrite(result, output_filename)
