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
#ifndef itkVnlFFTCommon_h
#define itkVnlFFTCommon_h

#include "itkIntTypes.h"

#include "vnl/algo/vnl_fft_base.h"

namespace itk
{

/** \class VnlFFTCommon
 * \brief Common routines related to Vnl's FFT implementation.
 *
 * \ingroup ITKFFT
 */
struct VnlFFTCommon
{

  /** Vnl's FFT supports discrete Fourier transforms for images whose
  sizes have a prime factorization consisting of 2's, 3's, and 5's. */
  template< typename TSizeValue >
  static bool IsDimensionSizeLegal(TSizeValue n);

  static ITK_CONSTEXPR_VAR SizeValueType GREATEST_PRIME_FACTOR = 5;

  /** Convenience struct for computing the discrete Fourier
  Transform. */
  template< typename TImage >
  struct VnlFFTTransform:
    public vnl_fft_base< TImage::ImageDimension, typename TImage::PixelType >
  {
    typedef vnl_fft_base< TImage::ImageDimension, typename TImage::PixelType > Base;

    //: constructor takes size of signal.
    VnlFFTTransform(const typename TImage::SizeType & s);
  };

};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTCommon.hxx"
#endif

#endif // itkVnlFFTCommon_h
