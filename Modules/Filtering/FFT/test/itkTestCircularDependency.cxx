/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkImage.h"
#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkVnlForwardFFTImageFilter.h"

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
  typename FFTWComplexToComplexFilterType::Pointer fftwCplxToCplxFFT = FFTWComplexToComplexFilterType::New();

  using FFTWRealToHalfHermitianForwardFFTImageFilterType =
    itk::FFTWRealToHalfHermitianForwardFFTImageFilter<RealImageType, CplxImageType>;
  typename FFTWRealToHalfHermitianForwardFFTImageFilterType::Pointer fRlToHlfHrmtnFwrdFFT =
    FFTWRealToHalfHermitianForwardFFTImageFilterType::New();

  using FFTWInverseFFTImageFilterType = itk::FFTWInverseFFTImageFilter<CplxImageType, RealImageType>;
  typename FFTWInverseFFTImageFilterType::Pointer fNvrsFFT = FFTWInverseFFTImageFilterType::New();

  using FFTWHalfHermitianToRealInverseFFTImageFilterType =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>;
  typename FFTWHalfHermitianToRealInverseFFTImageFilterType::Pointer hlfHrmtnToRlnvrs =
    FFTWHalfHermitianToRealInverseFFTImageFilterType::New();

  using FFTWForwardFFTImageFilterType = itk::FFTWForwardFFTImageFilter<RealImageType, CplxImageType>;
  typename FFTWForwardFFTImageFilterType::Pointer fFrwrdFFT = FFTWForwardFFTImageFilterType::New();
}
#endif

template <typename T>
void
Vnl()
{
  using PixelType = T;
  using CplxPixelType = std::complex<PixelType>;
  using RealImageType = itk::Image<PixelType, 3>;
  using CplxImageType = itk::Image<CplxPixelType, 3>;

  using VnlComplexToComplexFilterType = itk::VnlComplexToComplexFFTImageFilter<CplxImageType>;
  typename VnlComplexToComplexFilterType::Pointer vnlCplxToCplxFFT = VnlComplexToComplexFilterType::New();

  using VnlRealToHalfHermitianForwardFFTImageFilterType =
    itk::VnlRealToHalfHermitianForwardFFTImageFilter<RealImageType, CplxImageType>;
  typename VnlRealToHalfHermitianForwardFFTImageFilterType::Pointer vRlToHlfHrmtnFwrdFFT =
    VnlRealToHalfHermitianForwardFFTImageFilterType::New();

  using VnlInverseFFTImageFilterType = itk::VnlInverseFFTImageFilter<CplxImageType, RealImageType>;
  typename VnlInverseFFTImageFilterType::Pointer vNvrsFFT = VnlInverseFFTImageFilterType::New();

  using VnlHalfHermitianToRealInverseFFTImageFilterType =
    itk::VnlHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>;
  typename VnlHalfHermitianToRealInverseFFTImageFilterType::Pointer fHlfHrmtnToRlnvrs =
    VnlHalfHermitianToRealInverseFFTImageFilterType::New();

  using VnlForwardFFTImageFilterType = itk::VnlForwardFFTImageFilter<RealImageType, CplxImageType>;
  typename VnlForwardFFTImageFilterType::Pointer vFrwrdFFT = VnlForwardFFTImageFilterType::New();
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
  Vnl<float>();
  Vnl<double>();
  return 0;
}
