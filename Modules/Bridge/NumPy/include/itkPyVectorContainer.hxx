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
#ifndef itkPyVectorContainer_hxx
#define itkPyVectorContainer_hxx

#include <stdexcept>

namespace itk
{

template <typename TElementIdentifier, typename TElement>
PyObject *
PyVectorContainer<TElementIdentifier, TElement>::_array_view_from_vector_container(VectorContainerType * vector)
{
  Py_buffer pyBuffer{};

  if (!vector)
  {
    throw std::runtime_error("Input vector is null");
  }

  DataType * buffer = vector->CastToSTLContainer().data();

  void * vectorBuffer = buffer;

  // Computing the length of data
  Py_ssize_t len = vector->Size();
  len *= sizeof(DataType);

  const int        res = PyBuffer_FillInfo(&pyBuffer, nullptr, vectorBuffer, len, 0, PyBUF_CONTIG);
  PyObject * const memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template <typename TElementIdentifier, typename TElement>
auto
PyVectorContainer<TElementIdentifier, TElement>::_vector_container_from_array(PyObject * arr, PyObject * shape) -> const
  typename VectorContainerType::Pointer
{
  Py_buffer pyBuffer{};

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    PyBuffer_Release(&pyBuffer);
    return nullptr;
  }

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  PyObject * const   obj = shape;
  PyObject * const   shapeseq = PySequence_Fast(obj, "expected sequence");
  const unsigned int dimension = PySequence_Size(obj);

  PyObject *   item = PySequence_Fast_GET_ITEM(shapeseq, 0); // Only one dimension
  const size_t numberOfElements = static_cast<size_t>(PyInt_AsLong(item));

  const size_t len = numberOfElements * sizeof(DataType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of vector and Buffer.");
    PyBuffer_Release(&pyBuffer);
    return nullptr;
  }
  const auto * const data = static_cast<const DataType *>(buffer);
  auto               output = VectorContainerType::New();
  output->resize(numberOfElements);
  for (size_t ii = 0; ii < numberOfElements; ++ii)
  {
    output->SetElement(ii, data[ii]);
  }
  PyBuffer_Release(&pyBuffer);

  return output;
}

} // namespace itk

#endif
