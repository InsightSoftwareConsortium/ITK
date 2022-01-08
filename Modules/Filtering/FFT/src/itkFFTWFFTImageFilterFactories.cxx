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
#include "itkConfigure.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#  include "ITKFFTExport.h"

#  include "itkFFTWComplexToComplex1DFFTImageFilter.h"
#  include "itkFFTWComplexToComplexFFTImageFilter.h"
#  include "itkFFTWForward1DFFTImageFilter.h"
#  include "itkFFTWForwardFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#  include "itkFFTWInverse1DFFTImageFilter.h"
#  include "itkFFTWInverseFFTImageFilter.h"
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"

#  include "itkCreateObjectFunction.h"
#  include "itkVersion.h"
#  include "itkObjectFactoryBase.h"

namespace itk
{
// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKFFT_EXPORT
     FFTWComplexToComplex1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWComplexToComplex1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWComplexToComplexFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWComplexToComplexFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWForward1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWForward1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWForwardFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWForwardFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWHalfHermitianToRealInverseFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWHalfHermitianToRealInverseFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWInverse1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWInverse1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWInverseFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWInverseFFTImageFilter>>();
}

void ITKFFT_EXPORT
     FFTWRealToHalfHermitianForwardFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<FFTWRealToHalfHermitianForwardFFTImageFilter>>();
}
} // end namespace itk

#endif // defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
