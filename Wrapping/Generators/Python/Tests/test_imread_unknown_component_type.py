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

"""
itk.imread() raises a KeyError that names the component type the ImageIO
reported and points at pixel_type=. With fallback_only=True the retry
fails too, because no wrapped reader can decode pixel data whose
component type the ImageIO could not identify.
"""

import os
import tempfile

import itk


def _write_mha_with_unrecognized_element_type(path):
    # MetaIO reports an ElementType it cannot interpret as component and pixel type "unknown".
    header = (
        "ObjectType = Image\n"
        "NDims = 2\n"
        "DimSize = 2 2\n"
        "ElementType = MET_NOT_A_REAL_TYPE\n"
        "ElementDataFile = LOCAL\n"
    )
    with open(path, "wb") as f:
        f.write(header.encode("ascii"))
        f.write(bytes(16))


def test_imread_names_the_unrecognized_component_type():
    with tempfile.TemporaryDirectory() as temp_dir:
        filename = os.path.join(temp_dir, "unrecognized_element_type.mha")
        _write_mha_with_unrecognized_element_type(filename)

        try:
            itk.imread(filename)
            raise AssertionError("expected a KeyError")
        except KeyError as e:
            assert "unknown" in str(e)
            assert "pixel_type" in str(e)


def test_imread_fallback_only_still_fails_on_the_same_file():
    with tempfile.TemporaryDirectory() as temp_dir:
        filename = os.path.join(temp_dir, "unrecognized_element_type.mha")
        _write_mha_with_unrecognized_element_type(filename)

        try:
            itk.imread(filename, itk.F, fallback_only=True)
            raise AssertionError("expected a RuntimeError")
        except RuntimeError:
            pass


if __name__ == "__main__":
    test_imread_names_the_unrecognized_component_type()
    test_imread_fallback_only_still_fails_on_the_same_file()
    print("OK")
