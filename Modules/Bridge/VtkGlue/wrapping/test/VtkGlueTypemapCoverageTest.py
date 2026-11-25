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
"""Exercise every VtkGlue SWIG typemap reachable from Python.

VtkGlueRoundTripTest covers the vtkImageData pair. This covers the
remaining reachable typemaps and the cases where a typemap can succeed
on the happy path while mishandling geometry, pixel type, dimension,
object identity or bad input.

Deliberately free of numpy so the test runs wherever ITKVtkGlue is
wrapped, without requiring ITKBridgeNumPy.
"""

import sys

import itk
from vtkmodules.vtkCommonDataModel import vtkImageData
from vtkmodules.vtkCommonCore import vtkPoints
from vtkmodules.util.vtkConstants import VTK_DOUBLE, VTK_FLOAT

failures = []


def check(condition, message):
    if not condition:
        failures.append(message)


def make_image(image_type, size, spacing, origin, marks):
    """An image with distinct spacing/origin and known sparse pixel values."""
    image = image_type.New()
    region = itk.ImageRegion[len(size)]()
    region.SetSize(size)
    region.SetIndex([0] * len(size))
    image.SetRegions(region)
    image.Allocate()
    image.FillBuffer(0)
    image.SetSpacing(spacing)
    image.SetOrigin(origin)
    for index, value in marks.items():
        image.SetPixel(list(index), value)
    return image


def round_trip(image_type, image):
    to_vtk = itk.ImageToVTKImageFilter[image_type].New()
    to_vtk.SetInput(image)
    to_vtk.Update()
    vtk_image = to_vtk.GetOutput()

    from_vtk = itk.VTKImageToImageFilter[image_type].New()
    from_vtk.SetInput(vtk_image)
    from_vtk.Update()
    return to_vtk, vtk_image, from_vtk, from_vtk.GetOutput()


# --------------------------------------------------------------------------
# 1. The accessor typemaps: vtkImageImport* and vtkImageExport*.
#    VtkGlueRoundTripTest never calls these, so their `out` typemaps are
#    otherwise unexercised from Python.
# --------------------------------------------------------------------------
image_type = itk.Image[itk.F, 2]
image = make_image(image_type, [6, 4], [2.0, 3.0], [10.0, 20.0], {(2, 1): 7.5})
to_vtk, vtk_image, from_vtk, result = round_trip(image_type, image)

importer = to_vtk.GetImporter()
check(
    importer is not None and importer.GetClassName() == "vtkImageImport",
    f"GetImporter returned {type(importer).__name__}, expected vtkImageImport",
)

exporter = from_vtk.GetExporter()
check(
    exporter is not None and exporter.GetClassName() == "vtkImageExport",
    f"GetExporter returned {type(exporter).__name__}, expected vtkImageExport",
)

# --------------------------------------------------------------------------
# 2. Geometry and pixels survive the 2D round trip.
# --------------------------------------------------------------------------
check(
    tuple(vtk_image.GetDimensions()[:2]) == (6, 4),
    f"vtkImageData dimensions {vtk_image.GetDimensions()} != (6, 4, 1)",
)
check(
    tuple(vtk_image.GetSpacing()[:2]) == (2.0, 3.0),
    f"vtkImageData spacing {vtk_image.GetSpacing()} lost the ITK spacing",
)
check(result.GetPixel([2, 1]) == 7.5, "marked pixel did not survive the round trip")
check(list(result.GetSpacing()) == [2.0, 3.0], "spacing not restored on the way back")
check(list(result.GetOrigin()) == [10.0, 20.0], "origin not restored on the way back")

# --------------------------------------------------------------------------
# 3. The typemaps are templated over pixel type and dimension. A cast that
#    is correct for float/2D can still be wrong elsewhere.
# --------------------------------------------------------------------------
for pixel_type, name in ((itk.UC, "unsigned char"), (itk.SS, "signed short")):
    try:
        typed = itk.Image[pixel_type, 2]
    except KeyError:
        continue  # not wrapped in this configuration
    marked = make_image(typed, [5, 3], [1.5, 2.5], [-4.0, 8.0], {(1, 2): 9})
    _, vtk_typed, _, back = round_trip(typed, marked)
    check(
        back.GetPixel([1, 2]) == 9,
        f"{name} pixel value lost in round trip",
    )
    check(
        tuple(vtk_typed.GetDimensions()[:2]) == (5, 3),
        f"{name} dimensions wrong: {vtk_typed.GetDimensions()}",
    )

try:
    image_type_3d = itk.Image[itk.F, 3]
except KeyError:
    image_type_3d = None
if image_type_3d is not None:
    volume = make_image(
        image_type_3d, [4, 3, 2], [1.0, 2.0, 4.0], [0.0, 0.0, 5.0], {(3, 2, 1): -2.25}
    )
    _, vtk_volume, _, back_3d = round_trip(image_type_3d, volume)
    check(
        tuple(vtk_volume.GetDimensions()) == (4, 3, 2),
        f"3D dimensions wrong: {vtk_volume.GetDimensions()}",
    )
    check(
        tuple(vtk_volume.GetSpacing()) == (1.0, 2.0, 4.0),
        f"3D spacing wrong: {vtk_volume.GetSpacing()}",
    )
    check(back_3d.GetPixel([3, 2, 1]) == -2.25, "3D pixel lost in round trip")

# --------------------------------------------------------------------------
# 4. Object identity. The `out` typemap reconstructs a Python wrapper from a
#    raw address; two calls must yield the same underlying VTK object rather
#    than distinct wrappers over the same memory.
# --------------------------------------------------------------------------
first = to_vtk.GetOutput()
second = to_vtk.GetOutput()
check(
    first.GetAddressAsString("vtkImageData")
    == second.GetAddressAsString("vtkImageData"),
    "repeated GetOutput() returned different underlying vtkImageData addresses",
)


# --------------------------------------------------------------------------
# 5. Lifetime. The wrapper must keep the VTK object alive after the producing
#    filter goes out of scope, and repeated conversions must not corrupt state.
# --------------------------------------------------------------------------
def detached_vtk_image():
    local = itk.ImageToVTKImageFilter[image_type].New()
    local.SetInput(image)
    local.Update()
    return local.GetOutput()


detached = detached_vtk_image()
check(
    tuple(detached.GetDimensions()[:2]) == (6, 4),
    "vtkImageData became invalid after its producing filter was released",
)

for iteration in range(10):
    _, _, _, repeated = round_trip(image_type, image)
    if repeated.GetPixel([2, 1]) != 7.5:
        failures.append(f"round trip {iteration} did not preserve the marked pixel")
        break

# --------------------------------------------------------------------------
# 6. The `in` typemap must reject what it cannot convert instead of
#    reinterpreting an unrelated pointer as vtkImageData.
# --------------------------------------------------------------------------
for bad, description in (
    (vtkPoints(), "a vtkPoints instance"),
    ("Addr=0x0", "a string that looks like VTK's address encoding"),
    (42, "an integer"),
):
    try:
        rejecting = itk.VTKImageToImageFilter[image_type].New()
        rejecting.SetInput(bad)
    except Exception:
        pass  # any exception is acceptable; silent acceptance is not
    else:
        failures.append(f"SetInput accepted {description} as vtkImageData")

# --------------------------------------------------------------------------
# 7. Degenerate but valid input must convert; a scalar type that disagrees
#    with the ITK pixel type must be reported rather than reinterpreted.
# --------------------------------------------------------------------------
minimal_input = vtkImageData()
minimal_input.SetDimensions(1, 1, 1)
minimal_input.AllocateScalars(VTK_FLOAT, 1)
try:
    minimal = itk.VTKImageToImageFilter[image_type].New()
    minimal.SetInput(minimal_input)
    minimal.Update()
    check(
        minimal.GetOutput() is not None,
        "conversion of a 1x1x1 vtkImageData produced no output",
    )
except Exception as error:  # noqa: BLE001 - report rather than abort
    failures.append(f"1x1x1 vtkImageData raised {error!r}")

mismatched = vtkImageData()
mismatched.SetDimensions(2, 2, 1)
mismatched.AllocateScalars(VTK_DOUBLE, 1)
try:
    wrong_scalar = itk.VTKImageToImageFilter[image_type].New()
    wrong_scalar.SetInput(mismatched)
    wrong_scalar.Update()
except Exception:
    pass  # expected: the scalar type disagrees with itk.F
else:
    failures.append("double-valued vtkImageData silently accepted for a float image")


if failures:
    print(f"{len(failures)} VtkGlue typemap check(s) failed:", file=sys.stderr)
    for failure in failures:
        print(f"  - {failure}", file=sys.stderr)
    sys.exit(1)

print("All reachable VtkGlue typemaps behaved correctly.")
