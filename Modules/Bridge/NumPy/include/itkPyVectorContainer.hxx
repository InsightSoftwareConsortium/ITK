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

#include <memory> // For unique_ptr.
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

  // Computing the length of data
  const auto len = static_cast<Py_ssize_t>(vector->size() * sizeof(DataType));

  PyBuffer_FillInfo(&pyBuffer, nullptr, buffer, len, 0, PyBUF_CONTIG);
  return PyMemoryView_FromBuffer(&pyBuffer);
}

template <typename TElementIdentifier, typename TElement>
auto
PyVectorContainer<TElementIdentifier, TElement>::_vector_container_from_array(PyObject * arr, PyObject * const shape) ->
  typename VectorContainerType::Pointer
{
  Py_buffer pyBuffer{};

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    return nullptr;
  }

  [[maybe_unused]] const std::unique_ptr<Py_buffer, decltype(&PyBuffer_Release)> bufferScopeGuard(&pyBuffer,
                                                                                                  &PyBuffer_Release);

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  PyObject * const   shapeseq = PySequence_Fast(shape, "expected sequence");
  const unsigned int dimension = PySequence_Size(shape);

  PyObject *   item = PySequence_Fast_GET_ITEM(shapeseq, 0); // Only one dimension
  const size_t numberOfElements = static_cast<size_t>(PyInt_AsLong(item));

  const size_t len = numberOfElements * sizeof(DataType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of vector and Buffer.");
    return nullptr;
  }
  const auto * const data = static_cast<const DataType *>(buffer);
  auto               output = VectorContainerType::New();
  output->assign(data, data + numberOfElements);

  return output;
}

} // namespace itk

#endif
