# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

import itk
from sys import argv, stderr, exit

itk.auto_progress(2)


if len(argv) < 2:
    print(
        (
            "Missing Parameters \n Usage: IntensityWindowingImageFilter.py inputImageFile"
        ),
        file=stderr,
    )
    exit(1)

image = itk.imread(argv[1], itk.F)
# Verifies that ITK supports getting tuples for filter parameters. Not testing the filter
# results.
intensity_filter = itk.IntensityWindowingImageFilter.New(image, WindowLevel=[255, 127])
