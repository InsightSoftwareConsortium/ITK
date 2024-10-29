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
#ifndef itkPyBuffer_hxx
#define itkPyBuffer_hxx


#include "itkImportImageContainer.h"
#include <algorithm> // For reverse.
#include <memory>    // For unique_ptr.

namespace itk
{

template <class TImage>
PyObject *
PyBuffer<TImage>::_GetArrayViewFromImage(ImageType * image)
{
  Py_buffer pyBuffer{};

  if (!image)
  {
    throw std::runtime_error("Input image is null");
  }

  image->Update();

  void * const buffer = image->GetBufferPointer();

  // Computing the length of data
  const unsigned int  numberOfComponents = image->GetNumberOfComponentsPerPixel();
  const SizeValueType numberOfPixels = image->GetBufferedRegion().GetNumberOfPixels();
  const auto          len = static_cast<Py_ssize_t>(numberOfPixels * numberOfComponents * sizeof(ComponentType));

  PyBuffer_FillInfo(&pyBuffer, nullptr, buffer, len, 0, PyBUF_CONTIG);
  return PyMemoryView_FromBuffer(&pyBuffer);
}

template <class TImage>
auto
PyBuffer<TImage>::_get_image_view_from_contiguous_array(PyObject * arr, PyObject * shape, PyObject * numOfComponent)
  -> OutputImagePointer
{
  Py_buffer pyBuffer{};

  SizeType size;

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_ANY_CONTIGUOUS) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get a contiguous buffer from the specified NumPy array.");
    return nullptr;
  }

  [[maybe_unused]] const std::unique_ptr<Py_buffer, decltype(&PyBuffer_Release)> bufferScopeGuard(&pyBuffer,
                                                                                                  &PyBuffer_Release);

  const Py_ssize_t bufferLength = pyBuffer.len;
  void * const     buffer = pyBuffer.buf;

  PyObject * const   shapeseq = PySequence_Fast(shape, "expected sequence");
  const unsigned int dimension = PySequence_Size(shape);

  const long numberOfComponents = PyInt_AsLong(numOfComponent);

  for (unsigned int i = 0; i < dimension; ++i)
  {
    PyObject * const item = PySequence_Fast_GET_ITEM(shapeseq, i);
    size[i] = static_cast<SizeValueType>(PyInt_AsLong(item));
  }

  const SizeValueType numberOfPixels = size.CalculateProductOfElements();

  const size_t len = numberOfPixels * numberOfComponents * sizeof(ComponentType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of image and Buffer.");
    SWIG_Py_DECREF(shapeseq);
    return nullptr;
  }

  if (PyBuffer_IsContiguous(&pyBuffer, 'C') == 0)
  {
    if (PyBuffer_IsContiguous(&pyBuffer, 'F') == 1)
    {
      // The buffer is Fortran contiguous (and not C-contiguous), so reverse the size elements.
      std::reverse(size.begin(), size.end());
    }
    else
    {
      // The buffer is neither Fortran nor C-contiguous. This is unlikely to happen, because PyBUF_ANY_CONTIGUOUS was
      // specified as argument, when retrieving the buffer. But if it _does_ happen, it's obviously an error.
      PyErr_SetString(PyExc_RuntimeError, "The buffer should be contiguous, but it is not!");
      return nullptr;
    }
  }

  using InternalPixelType = typename TImage::InternalPixelType;
  using ImporterType = ImportImageContainer<SizeValueType, InternalPixelType>;
  auto           importer = ImporterType::New();
  constexpr bool importImageFilterWillOwnTheBuffer = false;
  auto * const   data = static_cast<InternalPixelType *>(buffer);
  importer->SetImportPointer(data, numberOfPixels, importImageFilterWillOwnTheBuffer);

  OutputImagePointer output = TImage::New();
  output->SetRegions(size);
  output->SetPixelContainer(importer);
  output->SetNumberOfComponentsPerPixel(numberOfComponents);

  SWIG_Py_DECREF(shapeseq);

  return output;
}

} // namespace itk

#endif
