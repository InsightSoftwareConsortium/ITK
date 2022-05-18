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
#include "itkVnlFFTImageFilterInitFactory.h"

#include "itkVnlComplexToComplex1DFFTImageFilter.h"
#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkVnlForward1DFFTImageFilter.h"
#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkVnlInverse1DFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"

#include "itkCreateObjectFunction.h"
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{
VnlFFTImageFilterInitFactory::VnlFFTImageFilterInitFactory()
{
  VnlFFTImageFilterInitFactory::RegisterFactories();
}

VnlFFTImageFilterInitFactory::~VnlFFTImageFilterInitFactory() = default;

void
VnlFFTImageFilterInitFactory::RegisterFactories()
{
  FFTImageFilterFactory<VnlComplexToComplex1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlComplexToComplexFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlForward1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlForwardFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlHalfHermitianToRealInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlInverse1DFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlInverseFFTImageFilter>::RegisterOneFactory();
  FFTImageFilterFactory<VnlRealToHalfHermitianForwardFFTImageFilter>::RegisterOneFactory();
}

// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
// TODO CMake parsing currently does not allow "InitFactory"
void ITKFFT_EXPORT
     VnlFFTImageFilterInitFactoryRegister__Private()
{
  VnlFFTImageFilterInitFactory::RegisterFactories();
}

} // end namespace itk
