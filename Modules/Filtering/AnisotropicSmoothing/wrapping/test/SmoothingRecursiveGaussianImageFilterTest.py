# ==========================================================================
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
# ==========================================================================*/

#
#  Example on the use of the SmoothingRecursiveGaussianImageFilter
#

import itk
from sys import argv

itk.auto_progress(2)

reader = itk.ImageFileReader.IUC2.New(FileName=argv[1])
filter = itk.SmoothingRecursiveGaussianImageFilter.New(reader, Sigma=eval(argv[3]))
itk.imwrite(filter, argv[2])
