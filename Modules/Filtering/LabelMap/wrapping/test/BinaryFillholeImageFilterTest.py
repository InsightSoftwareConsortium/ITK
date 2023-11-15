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

if len(argv) < 5:
    print(
        (
            "Missing Parameters \n Usage: BinaryFillholeImageFilterTest.py inputImageFile "
            "outputImageFile fullyConnected foregroundValue"
        ),
        file=stderr,
    )
    exit(1)


mask = itk.imread(argv[1])
fully_connected = (int(argv[3]) > 0)
foreground_value = int(argv[4])

mask_filled = itk.binary_fillhole_image_filter(
    mask, fully_connected=fully_connected, foreground_value=foreground_value)
itk.imwrite(mask_filled, argv[2])
