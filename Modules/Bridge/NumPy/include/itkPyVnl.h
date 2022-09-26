/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkPyVnl_h
#define itkPyVnl_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include <Python.h>

namespace itk
{

/**
 * \class PyVnl
 *
 *  \brief Helper class get views of VNL data buffers in python arrays and back.
 *
 *  This class will either receive a VNL data structure and create the equivalent
 *  Python array view or will receive a Python array and create a copy of it in a
 *  VNL data structure. This permits passing VNL data structures into python arrays
 *  from the NumPy python package.
 *
 *  \ingroup ITKBridgeNumPy
 */
template <typename TElement>
class PyVnl
{
public:
  /** Standard "Self" type alias. */
  using Self = PyVnl;

  /** Type of the data from which the buffer will be converted */
  using DataType = TElement;
  using VectorType = vnl_vector<TElement>;
  using MatrixType = vnl_matrix<TElement>;

  /**
   * Get an Array with the content of the vnl vector
   */
  static PyObject *
  _GetArrayViewFromVnlVector(VectorType * vector);

  /**
   * Get a vnl vector from a Python array
   */
  static const VectorType
  _GetVnlVectorFromArray(PyObject * arr, PyObject * shape);

  /**
   * Get an Array with the content of the vnl matrix
   */
  static PyObject *
  _GetArrayViewFromVnlMatrix(MatrixType * matrix);

  /**
   * Get a vnl matrix from a Python array
   */
  static const MatrixType
  _GetVnlMatrixFromArray(PyObject * arr, PyObject * shape);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPyVnl.hxx"
#endif

#endif // _itkPyVnl_h
