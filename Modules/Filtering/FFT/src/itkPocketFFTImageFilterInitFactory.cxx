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
#include "itkPocketFFTImageFilterInitFactory.h"

#include "itkPocketFFTComplexToComplex1DFFTImageFilter.h"
#include "itkPocketFFTComplexToComplexFFTImageFilter.h"
#include "itkPocketFFTForward1DFFTImageFilter.h"
#include "itkPocketFFTForwardFFTImageFilter.h"
#include "itkPocketFFTHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkPocketFFTInverse1DFFTImageFilter.h"
#include "itkPocketFFTInverseFFTImageFilter.h"
#include "itkPocketFFTRealToHalfHermitianForwardFFTImageFilter.h"

#include "itkCreateObjectFunction.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
PocketFFTImageFilterInitFactory::PocketFFTImageFilterInitFactory()
{
  PocketFFTImageFilterInitFactory::RegisterFactories();
}

PocketFFTImageFilterInitFactory::~PocketFFTImageFilterInitFactory() = default;

void
PocketFFTImageFilterInitFactory::RegisterFactories()
{
  FFTImageFilterFactory<PocketFFTComplexToComplex1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTComplexToComplexFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTForward1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTForwardFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTHalfHermitianToRealInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTInverse1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<PocketFFTRealToHalfHermitianForwardFFTImageFilter>::RegisterOneFactory();
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
// TODO CMake parsing currently does not allow "InitFactory"
void ITKFFT_EXPORT
PocketFFTImageFilterInitFactoryRegister__Private()
{
  PocketFFTImageFilterInitFactory::RegisterFactories();
}

} // end namespace itk
