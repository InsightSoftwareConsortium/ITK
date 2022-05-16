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
import unittest
import datetime as dt

import itk
import numpy as np


class TestNumpyVectorContainerMemoryviewInterface(unittest.TestCase):
    """This tests numpy array <-> ITK VectorContainer conversion."""

    def setUp(self):
        pass

    def test_NumPyBridge_VectorContainer(self):
        "Try to convert a itk.VectorContainer into a Numpy array and back."

        if not (
            hasattr(itk.VectorContainer, "ULLF")
            and hasattr(itk.PyVectorContainer, "ULLF")
            and hasattr(itk.Point, "F3")
            and hasattr(itk.VectorContainer, "ULLPF3")
            and hasattr(itk.Point, "F2")
            and hasattr(itk.VectorContainer, "ULLPF2")
        ):
            # There is insufficient wrapping to perform this test; skip it.
            print("Insufficient wrapping to perform itkPyVectorContainerTest")
            return

        v1 = itk.VectorContainer[itk.ULL, itk.F].New()
        v1.Reserve(4)
        v1.SetElement(0, 1.2)
        v1.SetElement(1, 2)
        v1.SetElement(2, 4)
        v1.SetElement(3, 5)
        arr = itk.array_view_from_vector_container(v1)
        assert arr.dtype == np.float32
        v2 = itk.vector_container_from_array(arr)
        self.assertEqual(v1.Size(), arr.shape[0])
        self.assertEqual(v1.Size(), v2.Size())
        # Compute difference between the original vector and numpy array view
        diff = 0.0
        for ii in range(0, v1.Size()):
            diff += abs(v1.GetElement(ii) - arr[ii])
        self.assertEqual(0, diff)
        # Compute difference between the two vectors
        diff = 0.0
        for ii in range(0, v1.Size()):
            diff += abs(v1.GetElement(ii) - v2.GetElement(ii))
        self.assertEqual(0, diff)
        # Test view
        v1.SetElement(0, 1)
        self.assertEqual(v1.GetElement(0), arr[0])
        # Test deep copy
        arr_cp = itk.array_from_vector_container(v1)
        self.assertEqual(v1.GetElement(0), arr_cp[0])
        v1.SetElement(0, 0)
        self.assertNotEqual(v1.GetElement(0), arr_cp[0])
        v2_cp = itk.vector_container_from_array(arr_cp)
        arr_cp[0] = 2
        self.assertNotEqual(v2_cp.GetElement(0), arr_cp[0])

        PointType = itk.Point[itk.F, 3]
        v_point = itk.VectorContainer[itk.ULL, PointType].New()
        v_point.Reserve(2)
        point = PointType()
        point[0] = 1.0
        point[1] = 2.0
        point[2] = 4.0
        v_point.SetElement(0, point)
        point[0] = 6.0
        point[1] = 8.0
        point[2] = 9.0
        v_point.SetElement(1, point)
        arr = itk.array_view_from_vector_container(v_point)
        self.assertTrue(
            np.array_equal(arr, np.array([[1.0, 2.0, 4.0], [6.0, 8.0, 9.0]]))
        )

        PointType = itk.Point[itk.F, 2]
        v_point = itk.VectorContainer[itk.ULL, PointType].New()
        v_point.Reserve(2)
        point = PointType()
        point[0] = 1.0
        point[1] = 2.0
        v_point.SetElement(0, point)
        point[0] = 6.0
        point[1] = 8.0
        v_point.SetElement(1, point)
        arr = itk.array_view_from_vector_container(v_point)
        self.assertTrue(np.array_equal(arr, np.array([[1.0, 2.0], [6.0, 8.0]])))


if __name__ == "__main__":
    unittest.main()
