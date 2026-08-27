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

"""ITK's half of the SimpleITK interop contract, without SimpleITK.

SimpleITK builds ITK, so an ITK test that imports SimpleITK would close a
cycle in the ecosystem build graph. The contract ITK owns is duck-typed --
"read xyz-ordered geometry from an image-like object" -- so it is exercised
here with a stub. Round-trip fidelity between two independently built ITK
libraries is the business of a standalone suite that depends on neither
project's build.
"""

import itk
import numpy as np

from itk.support.extras import _spatial_from_order_explicit

SPACING_XYZ = (1.0, 2.0, 3.0)


class ImageLike:
    """Stands in for a SimpleITK Image: Get*() accessors, optional keys."""

    def __init__(self, spacing_xyz=SPACING_XYZ, keys=None):
        self._spacing = spacing_xyz
        self._keys = {} if keys is None else keys

    def __getitem__(self, key):
        try:
            return self._keys[key]
        except KeyError:
            raise KeyError(f'"{key}" not in meta-data dictionary') from None

    def GetSpacing(self):
        return self._spacing


# --------------------------------------------------------------------------
# The order-explicit key is preferred when present
# --------------------------------------------------------------------------
explicit = ImageLike(keys={"spacing_xyz": (7.0, 8.0, 9.0)})
assert _spatial_from_order_explicit(explicit, "spacing") == (
    7.0,
    8.0,
    9.0,
), "the _xyz key must win over the accessor when both are available"
print("order-explicit key preferred over the accessor")


# --------------------------------------------------------------------------
# Fall back to the accessor: SimpleITK exposes no _xyz keys today
# --------------------------------------------------------------------------
# Verified against SimpleITK 2.5.4: image['spacing_xyz'] raises KeyError and
# the class has no keys(). The accessor path therefore carries every real
# conversion, so it is not a decorative fallback.
accessor_only = ImageLike()
assert (
    _spatial_from_order_explicit(accessor_only, "spacing") == SPACING_XYZ
), "must fall back to GetSpacing() when no order-explicit key exists"
print("accessor fallback used when no order-explicit key is present")


# --------------------------------------------------------------------------
# The bare key is never consulted, even when it is the only key
# --------------------------------------------------------------------------
# This is the #6706 conflict: a bare 'spacing' means (z,y,x) on an itk.Image
# and (x,y,z) on a SimpleITK Image. Reading it would silently reverse the
# spacing. Give the stub a bare key that disagrees with its accessor and
# require the accessor to win.
trap = ImageLike(keys={"spacing": SPACING_XYZ[::-1]})
assert (
    _spatial_from_order_explicit(trap, "spacing") == SPACING_XYZ
), "the order-ambiguous bare key must never be read (#6706)"
print("order-ambiguous bare key is never read")


# --------------------------------------------------------------------------
# Absent entirely: report nothing rather than guess a default
# --------------------------------------------------------------------------
class Empty:
    def __getitem__(self, key):
        raise KeyError(key)


assert (
    _spatial_from_order_explicit(Empty(), "spacing") is None
), "must return None when neither the key nor the accessor exists"
print("missing geometry reported as None rather than defaulted")


# --------------------------------------------------------------------------
# A non-zero buffered region, readable in both key orders
# --------------------------------------------------------------------------
offset_image = itk.Image[itk.F, 3].New()
region = itk.ImageRegion[3]()
region.SetIndex([2, 3, 4])
region.SetSize([4, 5, 6])
offset_image.SetRegions(region)
offset_image.Allocate(True)
assert tuple(offset_image["index_xyz"]) == (2, 3, 4), "index_xyz must be xyz"
assert tuple(offset_image["index_zyx"]) == (4, 3, 2), "index_zyx must be zyx"
print("non-zero start index is readable in both orders (#6710)")


# --------------------------------------------------------------------------
# ITK's own order-explicit keys, which the converter writes against
# --------------------------------------------------------------------------
image = itk.Image[itk.F, 3].New()
image.SetRegions([4, 5, 6])
image.Allocate(True)
image.SetSpacing(SPACING_XYZ)
assert np.allclose(image["spacing_xyz"], SPACING_XYZ), "itk spacing_xyz is not xyz"
assert np.allclose(
    image["spacing_zyx"], SPACING_XYZ[::-1]
), "itk spacing_zyx is not zyx"
print("itk.Image exposes both spatial key orders (#6710)")

print("simpleitk_protocol test passed")
