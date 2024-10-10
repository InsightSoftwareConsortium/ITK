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
#include <memory> // For unique_ptr.

namespace itk
{

template <class TImage>
PyObject *
PyBuffer<TImage>::_GetArrayViewFromImage(ImageType * image)
{
  Py_buffer pyBuffer{};

  Py_ssize_t len = 1;

  if (!image)
  {
    throw std::runtime_error("Input image is null");
  }

  image->Update();

  ComponentType * buffer =
    const_cast<ComponentType *>(reinterpret_cast<const ComponentType *>(image->GetBufferPointer()));

  void * itkImageBuffer = buffer;

  // Computing the length of data
  const int numberOfComponents = image->GetNumberOfComponentsPerPixel();
  SizeType  size = image->GetBufferedRegion().GetSize();

  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    len *= size[dim];
  }

  len *= numberOfComponents;
  len *= sizeof(ComponentType);

  PyBuffer_FillInfo(&pyBuffer, nullptr, itkImageBuffer, len, 0, PyBUF_CONTIG);
  return PyMemoryView_FromBuffer(&pyBuffer);
}

template <class TImage>
auto
PyBuffer<TImage>::_GetImageViewFromArray(PyObject * arr, PyObject * shape, PyObject * numOfComponent)
  -> OutputImagePointer
{
  Py_buffer pyBuffer{};

  SizeType      size;
  SizeType      sizeFortran;
  SizeValueType numberOfPixels = 1;

  if (PyObject_GetBuffer(arr, &pyBuffer, PyBUF_ND | PyBUF_ANY_CONTIGUOUS) == -1)
  {
    PyErr_SetString(PyExc_RuntimeError, "Cannot get an instance of NumPy array.");
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
    sizeFortran[dimension - 1 - i] = static_cast<SizeValueType>(PyInt_AsLong(item));
    numberOfPixels *= size[i];
  }

  bool isFortranContiguous = false;
  if (pyBuffer.strides != nullptr && pyBuffer.itemsize == pyBuffer.strides[0])
  {
    isFortranContiguous = true;
  }

  const size_t len = numberOfPixels * numberOfComponents * sizeof(ComponentType);
  if (bufferLength < 0 || static_cast<size_t>(bufferLength) != len)
  {
    PyErr_SetString(PyExc_RuntimeError, "Size mismatch of image and Buffer.");
    SWIG_Py_DECREF(shapeseq);
    return nullptr;
  }

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);
  if (isFortranContiguous)
  {
    region.SetSize(sizeFortran);
  }
  else
  {
    region.SetSize(size);
  }

  PointType origin;
  origin.Fill(0.0);

  SpacingType spacing;
  spacing.Fill(1.0);

  using InternalPixelType = typename TImage::InternalPixelType;
  using ImporterType = ImportImageContainer<SizeValueType, InternalPixelType>;
  auto           importer = ImporterType::New();
  constexpr bool importImageFilterWillOwnTheBuffer = false;
  auto * const   data = static_cast<InternalPixelType *>(buffer);
  importer->SetImportPointer(data, numberOfPixels, importImageFilterWillOwnTheBuffer);

  OutputImagePointer output = TImage::New();
  output->SetRegions(region);
  output->SetOrigin(origin);
  output->SetSpacing(spacing);
  output->SetPixelContainer(importer);
  output->SetNumberOfComponentsPerPixel(numberOfComponents);

  SWIG_Py_DECREF(shapeseq);

  return output;
}

} // namespace itk

#endif
