/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkPyVnl.h"
#include <stdexcept>

namespace itk
{

template<class TElement>
PyObject *
PyVnl<TElement>
::_GetArrayViewFromVnlVector( VectorType * vector)
{
  PyObject *                  memoryView    = NULL;
  Py_buffer                   pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t                      elementSize   = sizeof(DataType);
  int                         res           = 0;

  if( !vector )
    {
    throw std::runtime_error("Input vector is null");
    }

  DataType *buffer = vector->data_block();

  void * vectorBuffer = (void *)( buffer );

  // Computing the length of data
  Py_ssize_t len = vector->size();
  len *= elementSize;

  res = PyBuffer_FillInfo(&pyBuffer, NULL, (void*)vectorBuffer, len, 0, PyBUF_CONTIG);
  memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template<class TElement>
const typename PyVnl<TElement>::VectorType
PyVnl<TElement>
::_GetVnlVectorFromArray( PyObject *arr, PyObject *shape)
{
  PyObject *                  obj           = NULL;
  PyObject *                  shapeseq      = NULL;
  PyObject *                  item          = NULL;

  Py_ssize_t                  bufferLength;
  Py_buffer                   pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t numberOfElements = 1;

  const void *                buffer;

  unsigned int                dimension     = 0;


  size_t                      elementSize   = sizeof(DataType);
  size_t                      len           = 1;

  if(PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
    {
    PyErr_SetString( PyExc_RuntimeError, "Cannot get an instance of NumPy array." );
    PyBuffer_Release(&pyBuffer);
    return VectorType();
    }
  else
    {
    bufferLength = pyBuffer.len;
    buffer = pyBuffer.buf;
    }

  obj        = shape;
  shapeseq   = PySequence_Fast(obj, "expected sequence");
  dimension  = PySequence_Size(obj);

  item = PySequence_Fast_GET_ITEM(shapeseq,0);// Only one dimension
  numberOfElements = (size_t)PyInt_AsLong(item);

  len = numberOfElements*elementSize;
  if ( bufferLength != len )
    {
    PyErr_SetString( PyExc_RuntimeError, "Size mismatch of vector and Buffer." );
    PyBuffer_Release(&pyBuffer);
    return VectorType();
    }
  DataType * data = (DataType *)buffer;
  VectorType output(data, numberOfElements);
  PyBuffer_Release(&pyBuffer);

  return output;
}

template<class TElement>
PyObject *
PyVnl<TElement>
::_GetArrayViewFromVnlMatrix( MatrixType * matrix)
{
  PyObject *                  memoryView    = NULL;
  Py_buffer                   pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t                      elementSize   = sizeof(DataType);
  int                         res           = 0;

  if( !matrix )
    {
    throw std::runtime_error("Input matrix is null");
    }

  DataType *buffer =  matrix->data_block();

  void * matrixBuffer = (void *)( buffer );

  // Computing the length of data
  Py_ssize_t len = matrix->size();
  len *= elementSize;

  res = PyBuffer_FillInfo(&pyBuffer, NULL, (void*)matrixBuffer, len, 0, PyBUF_CONTIG);
  memoryView = PyMemoryView_FromBuffer(&pyBuffer);

  PyBuffer_Release(&pyBuffer);

  return memoryView;
}

template<class TElement>
const typename PyVnl<TElement>::MatrixType
PyVnl<TElement>
::_GetVnlMatrixFromArray( PyObject *arr, PyObject *shape)
{
  PyObject *                  obj           = NULL;
  PyObject *                  shapeseq      = NULL;
  PyObject *                  item          = NULL;

  Py_ssize_t                  bufferLength;
  Py_buffer                   pyBuffer;
  memset(&pyBuffer, 0, sizeof(Py_buffer));

  size_t numberOfElements = 1;

  const void *                buffer;

  unsigned int                dimension     = 0;

  size_t                      elementSize   = sizeof(DataType);
  size_t                      len           = 1;
  unsigned int                size[2];

  if(PyObject_GetBuffer(arr, &pyBuffer, PyBUF_CONTIG) == -1)
    {
    PyErr_SetString( PyExc_RuntimeError, "Cannot get an instance of NumPy array." );
    PyBuffer_Release(&pyBuffer);
    return MatrixType();
    }
  else
    {
    bufferLength = pyBuffer.len;
    buffer = pyBuffer.buf;
    }

  obj        = shape;
  shapeseq   = PySequence_Fast(obj, "expected sequence");
  dimension  = PySequence_Size(obj);

  for( unsigned int i = 0; i < 2; ++i )
    {
    item = PySequence_Fast_GET_ITEM(shapeseq,i);
    size[i] = (unsigned int)PyInt_AsLong(item);
    numberOfElements *= size[i];
    }

  len = numberOfElements*elementSize;
  if ( bufferLength != len )
    {
    PyErr_SetString( PyExc_RuntimeError, "Size mismatch of matrix and Buffer." );
    PyBuffer_Release(&pyBuffer);
    return MatrixType();
    }

  DataType * data = (DataType *)buffer;
  MatrixType output(data, size[0], size[1]);
  PyBuffer_Release(&pyBuffer);

  return output;
}


} // namespace itk

#endif
