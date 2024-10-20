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
#ifndef itkPyVnl_hxx
#define itkPyVnl_hxx

#include <memory> // For unique_ptr.
#include <stdexcept>

namespace itk
{

template <class TElement>
PyObject *
PyVnl<TElement>::_GetArrayViewFromVnlVector(VectorType * vector)
{
  Py_buffer pyBuffer{};

  if (!vector)
  {
    throw std::runtime_error("Input vector is null");
  }

  DataType * buffer = vector->data_block();

  // Computing the length of data
  const auto len = static_cast<Py_ssize_t>(vector->size() * sizeof(DataType));

  PyBuffer_FillInfo(&pyBuffer, nullptr, buffer, len, 0, PyBUF_CONTIG);
  return PyMemoryView_FromBuffer(&pyBuffer);
}

template <class TElement>
auto
PyVnl<TElement>::_GetVnlVectorFromArray(PyObject * arr, PyObject * const shape) -> VectorType
{
  Py_buffer pyBuffer{};

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    return VectorType();
  }

  [[maybe_unused]] const std::unique_ptr<Py_buffer, decltype(&PyBuffer_Release)> bufferScopeGuard(&pyBuffer,
                                                                                                  &PyBuffer_Release);

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  PyObject * const   shapeseq = PySequence_Fast(shape, "expected sequence");
  const unsigned int dimension = PySequence_Size(shape);

  PyObject * const item = PySequence_Fast_GET_ITEM(shapeseq, 0); // Only one dimension
  const size_t     numberOfElements = static_cast<size_t>(PyInt_AsLong(item));

  const size_t len = numberOfElements * sizeof(DataType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of vector and Buffer.");
    return VectorType();
  }
  const auto * const data = static_cast<const DataType *>(buffer);
  return VectorType(data, numberOfElements);
}

template <class TElement>
PyObject *
PyVnl<TElement>::_GetArrayViewFromVnlMatrix(MatrixType * matrix)
{
  Py_buffer pyBuffer{};

  if (!matrix)
  {
    throw std::runtime_error("Input matrix is null");
  }

  DataType * buffer = matrix->data_block();

  // Computing the length of data
  const auto len = static_cast<Py_ssize_t>(matrix->size() * sizeof(DataType));

  PyBuffer_FillInfo(&pyBuffer, nullptr, buffer, len, 0, PyBUF_CONTIG);
  return PyMemoryView_FromBuffer(&pyBuffer);
}

template <class TElement>
auto
PyVnl<TElement>::_GetVnlMatrixFromArray(PyObject * arr, PyObject * const shape) -> MatrixType
{
  Py_buffer pyBuffer{};

  size_t numberOfElements = 1;

  unsigned int size[2];

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    return MatrixType();
  }

  [[maybe_unused]] const std::unique_ptr<Py_buffer, decltype(&PyBuffer_Release)> bufferScopeGuard(&pyBuffer,
                                                                                                  &PyBuffer_Release);

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  PyObject * const   shapeseq = PySequence_Fast(shape, "expected sequence");
  const unsigned int dimension = PySequence_Size(shape);

  for (unsigned int i = 0; i < 2; ++i)
  {
    PyObject * const item = PySequence_Fast_GET_ITEM(shapeseq, i);
    size[i] = static_cast<unsigned int>(PyInt_AsLong(item));
    numberOfElements *= size[i];
  }

  const size_t len = numberOfElements * sizeof(DataType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of matrix and Buffer.");
    return MatrixType();
  }

  const auto * const data = static_cast<const DataType *>(buffer);
  return MatrixType(data, size[0], size[1]);
}


} // namespace itk

#endif
