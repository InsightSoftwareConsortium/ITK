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
# ==========================================================================
"""GradientImageFilter.OverrideBoundaryCondition takes ownership of its argument.

Without a DISOWN typemap, Python and the filter's unique_ptr member both free the
boundary condition, and the interpreter aborts at shutdown.  A non-zero exit code
from this script is itself part of the regression check.
"""

import itk

itk.auto_progress(2)

ImageType = itk.Image[itk.F, 2]

filt = itk.GradientImageFilter[ImageType, itk.F, itk.F].New()
boundaryCondition = itk.PeriodicBoundaryCondition[ImageType]()

assert (
    boundaryCondition.thisown
), "Python should own a freshly constructed boundary condition"

filt.OverrideBoundaryCondition(boundaryCondition)

assert (
    not boundaryCondition.thisown
), "OverrideBoundaryCondition must transfer ownership away from Python"

# The filter must remain usable with the adopted boundary condition.
image = ImageType.New()
region = itk.ImageRegion[2]()
region.SetSize([8, 8])
image.SetRegions(region)
image.Allocate()
image.FillBuffer(1.0)

filt.SetInput(image)
filt.Update()

assert filt.GetOutput().GetLargestPossibleRegion().GetSize()[0] == 8

print("Test finished.")
