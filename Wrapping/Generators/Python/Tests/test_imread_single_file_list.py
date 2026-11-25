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

"""Test for imread with a single-element list.

This tests the fix for the issue where imread fails when provided with a list
containing a single file path. The issue was that SetFileNames was receiving
an unpacked string instead of a list, causing it to iterate over characters.
"""

import sys
import os
import itk
import numpy as np
import tempfile


def test_imread_single_element_list():
    """Test that imread works with a list containing a single file path."""
    # Create a temporary image file
    temp_dir = tempfile.mkdtemp()
    filename = os.path.join(temp_dir, "test_image.png")

    try:
        # Create and save a test image
        image_size = (8, 10)
        arr = np.zeros(image_size, dtype=np.uint8)
        image = itk.image_from_array(arr)
        itk.imwrite(image, filename)

        # Test 1: imread with a string (baseline - should always work)
        img_from_string = itk.imread(filename)
        assert img_from_string is not None
        # NumPy shape (8, 10) becomes ITK size [10, 8] due to row/column-major ordering
        assert itk.size(img_from_string)[0] == image_size[1]
        assert itk.size(img_from_string)[1] == image_size[0]

        # Test 2: imread with a single-element list (the bug case)
        img_from_list = itk.imread([filename])
        assert img_from_list is not None
        # When reading as a series, dimension is increased by 1
        assert img_from_list.GetImageDimension() == 3
        assert itk.size(img_from_list)[0] == image_size[1]
        assert itk.size(img_from_list)[1] == image_size[0]
        assert itk.size(img_from_list)[2] == 1

    finally:
        # Cleanup
        if os.path.exists(filename):
            os.remove(filename)
        if os.path.exists(temp_dir):
            os.rmdir(temp_dir)


def test_imread_multi_element_list():
    """Test that imread still works with a list containing multiple file paths."""
    # Create temporary image files
    temp_dir = tempfile.mkdtemp()
    filename1 = os.path.join(temp_dir, "test_image1.png")
    filename2 = os.path.join(temp_dir, "test_image2.png")

    try:
        # Create and save test images
        image_size = (8, 10)
        arr = np.zeros(image_size, dtype=np.uint8)
        image = itk.image_from_array(arr)
        itk.imwrite(image, filename1)
        itk.imwrite(image, filename2)

        # Test: imread with a multi-element list (should work as before)
        img_from_list = itk.imread([filename1, filename2])
        assert img_from_list is not None
        # When reading as a series, dimension is increased by 1
        assert img_from_list.GetImageDimension() == 3
        # NumPy shape (8, 10) becomes ITK size [10, 8] due to row/column-major ordering
        assert itk.size(img_from_list)[0] == image_size[1]
        assert itk.size(img_from_list)[1] == image_size[0]
        assert itk.size(img_from_list)[2] == 2

    finally:
        # Cleanup
        if os.path.exists(filename1):
            os.remove(filename1)
        if os.path.exists(filename2):
            os.remove(filename2)
        if os.path.exists(temp_dir):
            os.rmdir(temp_dir)


if __name__ == "__main__":
    test_imread_single_element_list()
    test_imread_multi_element_list()
