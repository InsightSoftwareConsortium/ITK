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

import itk
import sys
itk.auto_progress(2)

# first argument is the pixel type
PType = getattr(itk, sys.argv[1])
# second arguement the image dimension
dim = int(sys.argv[2])

# get the image type
IType = itk.Image[PType, dim]

# create the reader
reader = itk.ImageFileReader[IType].New(FileName=sys.argv[3])
# and the writer
writer = itk.ImageFileWriter[IType].New(reader, FileName=sys.argv[4])

# execute the filters in the pipeline !
writer.Update()
