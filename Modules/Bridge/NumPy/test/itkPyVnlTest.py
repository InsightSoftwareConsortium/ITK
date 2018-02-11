#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/
from __future__ import print_function
import sys
import unittest
import datetime as dt

import itk
import numpy as np

class TestNumpyVnlMemoryviewInterface(unittest.TestCase):
    """ This tests numpy array <-> ITK Scalar Image conversion. """

    def setUp(self):
        pass

    def test_NumPyBridge_VnlVector(self):
        "Try to convert a vnl vector into a Numpy array and back."
        v1 = itk.vnl_vector[itk.F]()
        v1.set_size(4)
        v1.put(0, 1.3)
        v1.put(1, 2)
        v1.put(2, 4)
        v1.put(3, 5)
        arr = itk.PyVnl.F.GetArrayViewFromVnlVector(v1)
        v2 = itk.PyVnl.F.GetVnlVectorFromArray(arr)
        self.assertEqual(v1.size(), arr.shape[0])
        self.assertEqual(v1.size(), v2.size())
        # Compute difference between the original vector and numpy array view
        diff = 0.0
        for ii in range(0, v1.size()):
          diff += abs(v1.get(ii) - arr[ii])
        self.assertEqual(0, diff)
        # Compute difference between the two vectors
        diff = 0.0
        for ii in range(0, v1.size()):
          diff += abs(v1.get(ii) - v2.get(ii))
        self.assertEqual(0, diff)
        # Test view
        v1.put(0, 1)
        self.assertEqual(v1.get(0), arr[0])
        # Test deep copy
        arr_cp = itk.PyVnl.F.GetArrayFromVnlVector(v1)
        self.assertEqual(v1.get(0), arr_cp[0])
        v1.put(0, 0)
        self.assertNotEqual(v1.get(0), arr_cp[0])
        v2_cp=itk.PyVnl.F.GetVnlVectorFromArray(arr_cp)
        arr_cp[0] = 2
        self.assertNotEqual(v2_cp.get(0), arr_cp[0])

    def test_NumPyBridge_VnlMatrix(self):
        "Try to convert a vnl matrix into a Numpy array and back."
        m1 = itk.vnl_matrix[itk.F]()
        m1.set_size(2, 3)
        m1.fill(0)
        m1.put(1, 2, 1.3)
        m1.put(1, 0, 2)
        arr = itk.PyVnl.F.GetArrayViewFromVnlMatrix(m1)
        m2 = itk.PyVnl.F.GetVnlMatrixFromArray(arr)
        # Check that matrices have the same numer of elements
        self.assertEqual(m1.size(), m2.size())
        self.assertEqual(m1.size(), arr.size)
        # Check that the matrices axes dimensions have not been flipped or changed
        self.assertEqual(m1.rows(), arr.shape[0])
        self.assertEqual(m1.columns(), arr.shape[1])
        self.assertEqual(m1.rows(), m2.rows())
        self.assertEqual(m1.columns(), m2.columns())
        # Compute any difference between the original matrix and the numpy array view
        diff = 0.0
        for ii in range(m1.rows()):
          for jj in range(m1.cols()):
            diff += abs(m1.get(ii, jj) - arr[ii, jj])
        self.assertEqual(0, diff)
        # Compute any difference between the two matrices
        diff = 0.0
        for ii in range(m1.rows()):
          for jj in range(m1.cols()):
            diff += abs(m1.get(ii, jj) - m2.get(ii, jj))
        self.assertEqual(0, diff)
        # Test view
        m1.put(0, 0, 1)
        self.assertEqual(m1.get(0, 0), arr[0, 0])
        # Test deep copy
        arr_cp = itk.PyVnl.F.GetArrayFromVnlMatrix(m1)
        self.assertEqual(m1.get(0, 0), arr_cp[0, 0])
        m1.put(0, 0, 2)
        self.assertNotEqual(m1.get(0, 0), arr_cp[0, 0])
        m2 = itk.PyVnl.F.GetVnlMatrixFromArray(arr_cp)
        arr_cp[0, 0] = 2
        self.assertNotEqual(m2.get(0, 0), arr_cp[0, 0])



if __name__ == '__main__':
    unittest.main()
