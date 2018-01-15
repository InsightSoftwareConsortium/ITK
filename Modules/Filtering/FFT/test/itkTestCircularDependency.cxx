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
#include "itkImage.h"
#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkVnlForwardFFTImageFilter.h"

#if defined( ITK_USE_FFTWF ) || defined( ITK_USE_FFTWD )
#include "itkFFTWComplexToComplexFFTImageFilter.h"
#include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTWInverseFFTImageFilter.h"
#include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkFFTWForwardFFTImageFilter.h"

template<typename T>
void FFTW()
{
  typedef T                             PixelType;
  typedef std::complex<PixelType>       CplxPixelType;
  typedef itk::Image<PixelType, 3>      RealImageType;
  typedef itk::Image< CplxPixelType, 3> CplxImageType;

  typedef itk::FFTWComplexToComplexFFTImageFilter<CplxImageType> FFTWComplexToComplexFilterType;
  typename FFTWComplexToComplexFilterType::Pointer fftwCplxToCplxFFT = FFTWComplexToComplexFilterType::New();

  typedef itk::FFTWRealToHalfHermitianForwardFFTImageFilter<RealImageType,CplxImageType>
    FFTWRealToHalfHermitianForwardFFTImageFilterType;
  typename FFTWRealToHalfHermitianForwardFFTImageFilterType::Pointer fRlToHlfHrmtnFwrdFFT =
    FFTWRealToHalfHermitianForwardFFTImageFilterType::New();

  typedef itk::FFTWInverseFFTImageFilter<CplxImageType,RealImageType> FFTWInverseFFTImageFilterType;
  typename FFTWInverseFFTImageFilterType::Pointer fNvrsFFT = FFTWInverseFFTImageFilterType::New();

  typedef itk::FFTWHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>
    FFTWHalfHermitianToRealInverseFFTImageFilterType;
  typename FFTWHalfHermitianToRealInverseFFTImageFilterType::Pointer hlfHrmtnToRlnvrs =
    FFTWHalfHermitianToRealInverseFFTImageFilterType::New();

  typedef itk::FFTWForwardFFTImageFilter<RealImageType, CplxImageType> FFTWForwardFFTImageFilterType;
  typename FFTWForwardFFTImageFilterType::Pointer fFrwrdFFT = FFTWForwardFFTImageFilterType::New();
}
#endif

template<typename T>
void Vnl()
{
  typedef T                             PixelType;
  typedef std::complex<PixelType>       CplxPixelType;
  typedef itk::Image<PixelType, 3>      RealImageType;
  typedef itk::Image< CplxPixelType, 3> CplxImageType;

  typedef itk::VnlComplexToComplexFFTImageFilter<CplxImageType> VnlComplexToComplexFilterType;
  typename VnlComplexToComplexFilterType::Pointer vnlCplxToCplxFFT = VnlComplexToComplexFilterType::New();

  typedef itk::VnlRealToHalfHermitianForwardFFTImageFilter<RealImageType,CplxImageType>
    VnlRealToHalfHermitianForwardFFTImageFilterType;
  typename VnlRealToHalfHermitianForwardFFTImageFilterType::Pointer vRlToHlfHrmtnFwrdFFT =
    VnlRealToHalfHermitianForwardFFTImageFilterType::New();

  typedef itk::VnlInverseFFTImageFilter<CplxImageType,RealImageType> VnlInverseFFTImageFilterType;
  typename VnlInverseFFTImageFilterType::Pointer vNvrsFFT = VnlInverseFFTImageFilterType::New();

  typedef itk::VnlHalfHermitianToRealInverseFFTImageFilter<CplxImageType, RealImageType>
    VnlHalfHermitianToRealInverseFFTImageFilterType;
  typename VnlHalfHermitianToRealInverseFFTImageFilterType::Pointer fHlfHrmtnToRlnvrs =
    VnlHalfHermitianToRealInverseFFTImageFilterType::New();

  typedef itk::VnlForwardFFTImageFilter<RealImageType, CplxImageType> VnlForwardFFTImageFilterType;
  typename VnlForwardFFTImageFilterType::Pointer vFrwrdFFT = VnlForwardFFTImageFilterType::New();

}

int main()
{
  #if defined( ITK_USE_FFTWF )
  FFTW<float>();
  #endif
  #if defined( ITK_USE_FFTWD )
  FFTW<double>();
  #endif
  Vnl<float>();
  Vnl<double>();
  return 0;
}
