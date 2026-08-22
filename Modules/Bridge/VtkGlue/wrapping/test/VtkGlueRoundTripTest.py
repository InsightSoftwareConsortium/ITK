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

"""Round-trip itk.Image -> vtkImageData -> itk.Image through the VtkGlue filters.

This exercises the SWIG typemaps in VtkGlue.i directly: ImageToVTKImageFilter
returns `vtkImageData *` (the `out` typemap) and VTKImageToImageFilter accepts
`vtkImageData *` (the `in` typemap). The pure-numpy helpers in itk.support.extras
bypass both, so they are deliberately not used here.
"""

import sys

import numpy as np
from vtkmodules.vtkCommonDataModel import vtkImageData

import itk

Dimension = 2
PixelType = itk.F
ImageType = itk.Image[PixelType, Dimension]

reference_array = np.arange(6 * 4, dtype=np.float32).reshape((4, 6))
image = itk.image_from_array(reference_array)
image.SetSpacing([0.5, 2.0])
image.SetOrigin([-3.0, 7.0])

to_vtk = itk.ImageToVTKImageFilter[ImageType].New()
to_vtk.SetInput(image)
to_vtk.Update()
vtk_image = to_vtk.GetOutput()

if not isinstance(vtk_image, vtkImageData):
    print(
        f"out typemap returned {type(vtk_image)!r}, expected vtkImageData",
        file=sys.stderr,
    )
    sys.exit(1)

if vtk_image.GetDimensions()[:2] != (6, 4):
    print(f"unexpected VTK dimensions {vtk_image.GetDimensions()}", file=sys.stderr)
    sys.exit(1)

from_vtk = itk.VTKImageToImageFilter[ImageType].New()
from_vtk.SetInput(vtk_image)
from_vtk.Update()
result = from_vtk.GetOutput()

failures = []

result_array = itk.array_from_image(result)
if not np.array_equal(result_array, reference_array):
    failures.append(
        f"pixel buffer differs:\n{result_array}\nexpected:\n{reference_array}"
    )

if not np.allclose(list(result.GetSpacing()), [0.5, 2.0]):
    failures.append(f"spacing {list(result.GetSpacing())}, expected [0.5, 2.0]")

if not np.allclose(list(result.GetOrigin()), [-3.0, 7.0]):
    failures.append(f"origin {list(result.GetOrigin())}, expected [-3.0, 7.0]")

direction = itk.array_from_matrix(result.GetDirection())
if not np.allclose(direction, np.identity(Dimension)):
    failures.append(f"direction {direction}, expected identity")


# The `in` typemap must reject bad input rather than dereference it. The forged
# case matters most: without an isinstance() gate a crafted `__this__` would be
# cast to a native pointer and segfault instead of raising.
class ForgedVtkImageData:
    __this__ = "_00000000deadbeef_p_vtkImageData"


for bad, description in (
    ("not a vtkImageData", "a str"),
    (ForgedVtkImageData(), "an object with a forged __this__"),
    (vtkImageData, "the class rather than an instance"),
):
    try:
        itk.VTKImageToImageFilter[ImageType].New().SetInput(bad)
    except TypeError:
        pass
    except Exception as exception:  # noqa: BLE001 - anything but TypeError is a defect
        failures.append(
            f"in typemap raised {exception!r} for {description}, expected TypeError"
        )
    else:
        failures.append(f"in typemap accepted {description}; expected TypeError")

if failures:
    for failure in failures:
        print(f"  - {failure}", file=sys.stderr)
    sys.exit(1)

print("itk.Image <-> vtkImageData round trip preserved geometry and pixels.")
