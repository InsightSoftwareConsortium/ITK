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
#include "itkConfigure.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#  include "itkFFTWFFTImageFilterInitFactory.h"

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
FFTWFFTImageFilterInitFactory::FFTWFFTImageFilterInitFactory()
{
  FFTWFFTImageFilterInitFactory::RegisterFactories();
}

FFTWFFTImageFilterInitFactory::~FFTWFFTImageFilterInitFactory() = default;

void
FFTWFFTImageFilterInitFactory::RegisterFactories()
{
  FFTImageFilterFactory<FFTWComplexToComplex1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWComplexToComplexFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWForward1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWForwardFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWHalfHermitianToRealInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWInverse1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<FFTWRealToHalfHermitianForwardFFTImageFilter>::RegisterOneFactory();
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
// TODO CMake parsing currently does not allow "InitFactory"
void ITKFFT_EXPORT
     FFTWFFTImageFilterInitFactoryRegister__Private()
{
  FFTWFFTImageFilterInitFactory::RegisterFactories();
}

} // end namespace itk

#endif // defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
