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
  PyObject * memoryView = NULL;
  Py_buffer  pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t elementSize = sizeof(DataType);
  int    res = 0;

  if (!vector)
  {
    throw std::runtime_error("Input vector is null");
  }

  DataType * buffer = vector->CastToSTLContainer().data();

  void * vectorBuffer = (void *)(buffer);

  // Computing the length of data
  Py_ssize_t len = vector->Size();
  len *= elementSize;

  res = PyBuffer_FillInfo(&pyBuffer, NULL, (void *)vectorBuffer, len, 0, PyBUF_CONTIG);
  memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template <typename TElementIdentifier, typename TElement>
auto
PyVectorContainer<TElementIdentifier, TElement>::_vector_container_from_array(PyObject * arr, PyObject * shape) -> const
  typename VectorContainerType::Pointer
{
  PyObject * obj = NULL;
  PyObject * shapeseq = NULL;
  PyObject * item = NULL;

  Py_ssize_t bufferLength;
  Py_buffer  pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t numberOfElements = 1;

  const void * buffer;

  unsigned int dimension = 0;


  size_t elementSize = sizeof(DataType);
  size_t len = 1;

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
    PyBuffer_Release(&pyBuffer);
    return nullptr;
  }
  else
  {
    bufferLength = pyBuffer.len;
    buffer = pyBuffer.buf;
  }

  obj = shape;
  shapeseq = PySequence_Fast(obj, "expected sequence");
  dimension = PySequence_Size(obj);

  item = PySequence_Fast_GET_ITEM(shapeseq, 0); // Only one dimension
  numberOfElements = (size_t)PyInt_AsLong(item);

  len = numberOfElements * elementSize;
  if (bufferLength != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of vector and Buffer.");
    PyBuffer_Release(&pyBuffer);
    return nullptr;
  }
  DataType * data = (DataType *)buffer;
  auto       output = VectorContainerType::New();
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
