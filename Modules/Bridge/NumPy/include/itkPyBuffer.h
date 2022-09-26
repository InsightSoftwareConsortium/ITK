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

#ifndef itkPyBuffer_h
#define itkPyBuffer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkDefaultConvertPixelTraits.h"


// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#undef _XOPEN_SOURCE
#include <Python.h>

namespace itk
{

/**
 * \class PyBuffer
 *
 *  \brief Helper class to get ITK image views into python arrays and back.
 *
 *  This class will receive a C buffer and create the equivalent python
 *  array view. This permits passing image buffers into python arrays from
 *  the NumPy python package.
 *
 *  \ingroup ITKBridgeNumPy
 */
template <typename TImage>
class PyBuffer
{
public:
  /** Standard "Self" type alias. */
  using Self = PyBuffer;

  /** Type of the image from which the buffer will be converted */
  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using SizeType = typename ImageType::SizeType;
  using IndexType = typename ImageType::IndexType;
  using RegionType = typename ImageType::RegionType;
  using PointType = typename ImageType::PointType;
  using SpacingType = typename ImageType::SpacingType;
  using ImagePointer = typename ImageType::Pointer;
  using ComponentType = typename DefaultConvertPixelTraits<PixelType>::ComponentType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  using OutputImagePointer = typename TImage::Pointer;

  /**
   * Get an Array with the content of the image buffer
   */
  static PyObject *
  _GetArrayViewFromImage(ImageType * image);

  /**
   * Get an ITK image from a Python array
   */
  static const OutputImagePointer
  _GetImageViewFromArray(PyObject * arr, PyObject * shape, PyObject * numOfComponent);
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPyBuffer.hxx"
#endif

#endif // _itkPyBuffer_h
