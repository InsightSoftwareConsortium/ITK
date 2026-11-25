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
#ifndef itkPocketFFTCommon_h
#define itkPocketFFTCommon_h

#include "itk_pocketfft.h"

namespace itk
{
/** \brief Helpers shared by the PocketFFT image filters.
 * \ingroup ITKFFT
 */
namespace PocketFFTCommon
{

/** pocketfft shape is listed slowest-varying axis first; the ITK buffer is
 * x-fastest, so ITK dimension i maps to shape index (Dimension - 1 - i). */
template <typename TSize>
inline itk::detail::pocketfft::shape_t
MakeShape(const TSize & itkSize, const unsigned int dimension)
{
  itk::detail::pocketfft::shape_t shape(dimension);
  for (unsigned int i = 0; i < dimension; ++i)
  {
    shape[dimension - 1 - i] = itkSize[i];
  }
  return shape;
}

/** Contiguous-buffer strides in bytes for a pixel of size pixelBytes. */
template <typename TSize>
inline itk::detail::pocketfft::stride_t
MakeStride(const TSize & itkSize, const unsigned int dimension, const size_t pixelBytes)
{
  itk::detail::pocketfft::stride_t stride(dimension);
  ptrdiff_t                        byteStride = static_cast<ptrdiff_t>(pixelBytes);
  for (unsigned int i = 0; i < dimension; ++i)
  {
    stride[dimension - 1 - i] = byteStride;
    byteStride *= static_cast<ptrdiff_t>(itkSize[i]);
  }
  return stride;
}

/** All axes, ordered so axes.back() is the ITK x dimension (the axis
 * pocketfft halves in r2c/c2r). */
inline itk::detail::pocketfft::shape_t
MakeAxes(const unsigned int dimension)
{
  itk::detail::pocketfft::shape_t axes(dimension);
  for (unsigned int i = 0; i < dimension; ++i)
  {
    axes[i] = i;
  }
  return axes;
}

/** In-place 1D complex transform of a contiguous line buffer. */
template <typename TValue>
inline void
Transform1D(std::complex<TValue> * data, const size_t lineLength, const bool forward, const TValue scale)
{
  const itk::detail::pocketfft::shape_t  shape{ lineLength };
  const itk::detail::pocketfft::stride_t stride{ static_cast<ptrdiff_t>(sizeof(std::complex<TValue>)) };
  itk::detail::pocketfft::c2c(shape, stride, stride, { 0 }, forward, data, data, scale);
}

} // namespace PocketFFTCommon
} // namespace itk

#endif
