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
#  Example on the use of the CannyEdgeDetectionImageFilter
#

from __future__ import print_function

import itk
from sys import argv, stderr, exit
itk.auto_progress(2)

if len(argv) < 3:
    print((
        "Usage: CannyEdgeDetectionImageFilter.py inputImage outputImage "
        "[variance]"), file=stderr)
    exit(1)

variance = 2.0
if len(argv) > 3:
    variance = float(argv[3])
    print(variance)

reader = itk.ImageFileReader.IF2.New(FileName=argv[1])
filter = itk.CannyEdgeDetectionImageFilter.IF2IF2.New(
    reader,
    Variance=variance)
outputCast = itk.RescaleIntensityImageFilter.IF2IUC2.New(
    filter,
    OutputMinimum=0,
    OutputMaximum=255)
itk.write(outputCast, argv[2])
