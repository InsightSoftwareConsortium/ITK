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
import numpy as np

image = itk.GetImageFromArray(np.full((200, 200, 200), 1.0))
interp = itk.LinearInterpolateImageFunction.New(image)

# From the doc, the three versions of IsInsideBuffer appear
print(interp.IsInsideBuffer.__doc__)

# The following line calls the "point version"
print(interp.IsInsideBuffer([1, 2, 3]))

# Force the function to be called with a continuous index
cindex = itk.ContinuousIndex[itk.D, 3]()
cindex[0] = 1.5
cindex[1] = 2.5
cindex[2] = 3.5
print(interp.IsInsideBuffer(cindex))

# Force the function to be called with a point
point = itk.Point[itk.D, 3]()
point[0] = 1.5
point[1] = 2.5
point[2] = 3.5
print(interp.IsInsideBuffer(point))

# TODO: Which variant will this call?
print(interp.IsInsideBuffer([1.5, 2.5, 3.5]))
