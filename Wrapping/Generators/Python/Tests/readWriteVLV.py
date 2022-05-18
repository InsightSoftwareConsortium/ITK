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
import sys

if len(sys.argv) < 3:
    print("Usage: " + sys.argv[0] + " in.seg.nrrd out.seg.nrrd")
    sys.exit(1)

itk.auto_progress(2)

test_image = itk.imread(sys.argv[1], pixel_type=itk.VariableLengthVector[itk.UC])
for i in range(len(sys.argv) - 3):
    assert test_image.shape[i] == int(sys.argv[i + 3])
itk.imwrite(test_image, sys.argv[2], compression=True)
