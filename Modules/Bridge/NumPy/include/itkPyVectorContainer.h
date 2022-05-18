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

#ifndef itkPyVectorContainer_h
#define itkPyVectorContainer_h

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include <Python.h>

#include "itkVectorContainer.h"

namespace itk
{

/** \class PyVectorContainer
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
template <typename TElementIdentifier, typename TElement>
class PyVectorContainer
{
public:
  /** Standard "Self" type alias. */
  using Self = PyVectorContainer;

  /** Type of the data from which the buffer will be converted */
  using ElementIdentiferType = TElementIdentifier;
  using DataType = TElement;
  using VectorContainerType = VectorContainer<TElementIdentifier, TElement>;

  /**
   * Get an Array with the content of the vnl vector
   */
  static PyObject *
  _array_view_from_vector_container(VectorContainerType * vector);

  /**
   * Get a vnl vector from a Python array
   */
  static const typename VectorContainerType::Pointer
  _vector_container_from_array(PyObject * arr, PyObject * shape);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPyVectorContainer.hxx"
#endif

#endif // _itkPyVectorContainer_h
