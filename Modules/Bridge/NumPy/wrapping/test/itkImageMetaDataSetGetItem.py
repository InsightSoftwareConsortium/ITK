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

import sys
import numpy as np
from distutils.version import StrictVersion as VS

if VS(np.__version__) < VS("1.15.0"):
    print("NumPy 1.15.0 or greater is required")
    sys.exit(0)

import itk

if len(sys.argv) < 2:
    print("Usage: " + sys.argv[0] + " <input_image>")
    sys.exit(1)
filename = sys.argv[1]

image = itk.imread(filename)

meta_data = dict(image)
assert meta_data["0008|0008"] == "ORIGINAL\\PRIMARY\\AXIAL"
assert meta_data["0008|0016"] == "1.2.840.10008.5.1.4.1.1.2"
assert (
    meta_data["0010|0010"]
    == "LIVER DONOR PHANTOM 2                                           "
)
assert np.allclose(meta_data["origin"], np.array([-836.0, -144.0, -122.0]))
assert np.allclose(meta_data["spacing"], np.array([1.0, 0.484375, 0.484375]))
assert np.allclose(
    meta_data["direction"],
    np.array([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]),
)

assert image["0008|0008"] == "ORIGINAL\\PRIMARY\\AXIAL"
assert image["0008|0016"] == "1.2.840.10008.5.1.4.1.1.2"
assert (
    image["0010|0010"]
    == "LIVER DONOR PHANTOM 2                                           "
)
assert np.allclose(image["origin"], np.array([-836.0, -144.0, -122.0]))
assert np.allclose(image["spacing"], np.array([1.0, 0.484375, 0.484375]))
assert np.allclose(
    image["direction"], np.array([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]])
)

new_origin = np.array([1.0, 5.0, 8.0])
image["origin"] = new_origin
assert np.allclose(image["origin"], new_origin)
# NumPy indexing order is reverse of ITK
assert np.allclose(np.array(image.GetOrigin()), np.array([8.0, 5.0, 1.0]))

# __getitem__
assert image[0, 5, 8] == -999
# __setitem__
image[0, 5, 8] = 123
assert image[0, 5, 8] == 123
