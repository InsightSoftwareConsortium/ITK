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

#include <stdexcept>

namespace itk
{

template <class TElement>
PyObject *
PyVnl<TElement>::_GetArrayViewFromVnlVector(VectorType * vector)
{
  PyObject * memoryView = nullptr;
  Py_buffer  pyBuffer{};

  int res = 0;

  if (!vector)
  {
    throw std::runtime_error("Input vector is null");
  }

  DataType * buffer = vector->data_block();

  void * vectorBuffer = buffer;

  // Computing the length of data
  Py_ssize_t len = vector->size();
  len *= sizeof(DataType);

  res = PyBuffer_FillInfo(&pyBuffer, nullptr, vectorBuffer, len, 0, PyBUF_CONTIG);
  memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template <class TElement>
auto
PyVnl<TElement>::_GetVnlVectorFromArray(PyObject * arr, PyObject * shape) -> const VectorType
{
  PyObject * obj = nullptr;
  PyObject * shapeseq = nullptr;
  PyObject * item = nullptr;

  Py_buffer pyBuffer{};

  size_t numberOfElements = 1;

  unsigned int dimension = 0;

  size_t len = 1;

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    PyBuffer_Release(&pyBuffer);
    return VectorType();
  }

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  obj = shape;
  shapeseq = PySequence_Fast(obj, "expected sequence");
  dimension = PySequence_Size(obj);

  item = PySequence_Fast_GET_ITEM(shapeseq, 0); // Only one dimension
  numberOfElements = static_cast<size_t>(PyInt_AsLong(item));

  len = numberOfElements * sizeof(DataType);
  if (bufferLength != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of vector and Buffer.");
    PyBuffer_Release(&pyBuffer);
    return VectorType();
  }
  const auto * const data = static_cast<const DataType *>(buffer);
  VectorType         output(data, numberOfElements);
  PyBuffer_Release(&pyBuffer);

  return output;
}

template <class TElement>
PyObject *
PyVnl<TElement>::_GetArrayViewFromVnlMatrix(MatrixType * matrix)
{
  PyObject * memoryView = nullptr;
  Py_buffer  pyBuffer{};

  int res = 0;

  if (!matrix)
  {
    throw std::runtime_error("Input matrix is null");
  }

  DataType * buffer = matrix->data_block();

  void * matrixBuffer = buffer;

  // Computing the length of data
  Py_ssize_t len = matrix->size();
  len *= sizeof(DataType);

  res = PyBuffer_FillInfo(&pyBuffer, nullptr, matrixBuffer, len, 0, PyBUF_CONTIG);
  memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template <class TElement>
auto
PyVnl<TElement>::_GetVnlMatrixFromArray(PyObject * arr, PyObject * shape) -> const MatrixType
{
  PyObject * obj = nullptr;
  PyObject * shapeseq = nullptr;
  PyObject * item = nullptr;

  Py_buffer pyBuffer{};

  size_t numberOfElements = 1;

  unsigned int dimension = 0;

  size_t       len = 1;
  unsigned int size[2];

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    PyBuffer_Release(&pyBuffer);
    return MatrixType();
  }

  const Py_ssize_t   bufferLength = pyBuffer.len;
  const void * const buffer = pyBuffer.buf;

  obj = shape;
  shapeseq = PySequence_Fast(obj, "expected sequence");
  dimension = PySequence_Size(obj);

  for (unsigned int i = 0; i < 2; ++i)
  {
    item = PySequence_Fast_GET_ITEM(shapeseq, i);
    size[i] = static_cast<unsigned int>(PyInt_AsLong(item));
    numberOfElements *= size[i];
  }

  len = numberOfElements * sizeof(DataType);
  if (bufferLength != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of matrix and Buffer.");
    PyBuffer_Release(&pyBuffer);
    return MatrixType();
  }

  const auto * const data = static_cast<const DataType *>(buffer);
  MatrixType         output(data, size[0], size[1]);
  PyBuffer_Release(&pyBuffer);

  return output;
}


} // namespace itk

#endif
