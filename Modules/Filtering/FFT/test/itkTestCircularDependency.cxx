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
#include "itkImage.h"
#include "itkPocketFFTComplexToComplexFFTImageFilter.h"
#include "itkPocketFFTRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkPocketFFTInverseFFTImageFilter.h"
#include "itkPocketFFTHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkPocketFFTForwardFFTImageFilter.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#  include "itkFFTWComplexToComplexFFTImageFilter.h"
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkFFTWInverseFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#  include "itkFFTWForwardFFTImageFilter.h"

template <typename T>
void
FFTW()
{
  using PixelType = T;
  using CplxPixelType = std::complex<PixelType>;
  using RealImageType = itk::Image<PixelType, 3>;
  using CplxImageType = itk::Image<CplxPixelType, 3>;

  using FFTWComplexToComplexFilterType = itk::FFTWComplexToComplexFFTImageFilter<CplxImageType>;
  auto fftwCplxToCplxFFT = FFTWComplexToComplexFilterType::New();

  using FFTWRealToHalfHermitianForwardFFTImageFilterType =
    itk::FFTWRealToHalfHermitianForwardFFTImageFilter<RealImageType, CplxImageType>;
  const typename FFTWRealToHalfHermitianForwardFFTImageFilterType::Pointer fRlToHlfHrmtnFwrdFFT =
    FFTWRealToHalfHermitianForwardFFTImageFilterType::New();

  using FFTWInverseFFTImageFilterType = itk::FFTWInverseFFTImageFilter<CplxImageType, RealImageType>;
  auto fNvrsFFT = FFTWInverseFFTImageFilterType::New();

  using FFTWHalfHermitianToRealInverseFFTImageFilterType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>;
  const typename FFTWHalfHermitianToRealInverseFFTImageFilterType::Pointer hlfHrmtnToRlnvrs =
    FFTWHalfHermitianToRealInverseFFTImageFilterType::New();

  using FFTWForwardFFTImageFilterType = itk::FFTWForwardFFTImageFilter<RealImageType, CplxImageType>;
  auto fFrwrdFFT = FFTWForwardFFTImageFilterType::New();
}
#endif

template <typename T>
void
PocketFFT()
{
  using PixelType = T;
  using CplxPixelType = std::complex<PixelType>;
  using RealImageType = itk::Image<PixelType, 3>;
  using CplxImageType = itk::Image<CplxPixelType, 3>;

  using PocketFFTComplexToComplexFilterType = itk::PocketFFTComplexToComplexFFTImageFilter<CplxImageType>;
  auto vnlCplxToCplxFFT = PocketFFTComplexToComplexFilterType::New();

  using PocketFFTRealToHalfHermitianForwardFFTImageFilterType =
    itk::PocketFFTRealToHalfHermitianForwardFFTImageFilter<RealImageType, CplxImageType>;
  const typename PocketFFTRealToHalfHermitianForwardFFTImageFilterType::Pointer vRlToHlfHrmtnFwrdFFT =
    PocketFFTRealToHalfHermitianForwardFFTImageFilterType::New();

  using PocketFFTInverseFFTImageFilterType = itk::PocketFFTInverseFFTImageFilter<CplxImageType, RealImageType>;
  auto vNvrsFFT = PocketFFTInverseFFTImageFilterType::New();

  using PocketFFTHalfHermitianToRealInverseFFTImageFilterType =
    itk::PocketFFTHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>;
  const typename PocketFFTHalfHermitianToRealInverseFFTImageFilterType::Pointer fHlfHrmtnToRlnvrs =
    PocketFFTHalfHermitianToRealInverseFFTImageFilterType::New();

  using PocketFFTForwardFFTImageFilterType = itk::PocketFFTForwardFFTImageFilter<RealImageType, CplxImageType>;
  auto vFrwrdFFT = PocketFFTForwardFFTImageFilterType::New();
}

int
main()
{
#if defined(ITK_USE_FFTWF)
  FFTW<float>();
#endif
#if defined(ITK_USE_FFTWD)
  FFTW<double>();
#endif
  PocketFFT<float>();
  PocketFFT<double>();
  return 0;
}
