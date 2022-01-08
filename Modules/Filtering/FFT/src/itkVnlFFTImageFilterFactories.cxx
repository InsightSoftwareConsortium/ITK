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
#include "ITKFFTExport.h"

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
// Undocumented API used to register during static initialization.
// DO NOT CALL DIRECTLY.
void ITKFFT_EXPORT
     VnlComplexToComplex1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlComplexToComplex1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlComplexToComplexFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlComplexToComplexFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlForward1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlForward1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlForwardFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlForwardFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlHalfHermitianToRealInverseFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlHalfHermitianToRealInverseFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlInverse1DFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlInverse1DFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlInverseFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlInverseFFTImageFilter>>();
}

void ITKFFT_EXPORT
     VnlRealToHalfHermitianForwardFFTFactoryRegister__Private()
{
  ObjectFactoryBase::RegisterInternalFactoryOnce<FFTImageFilterFactory<VnlRealToHalfHermitianForwardFFTImageFilter>>();
}
} // end namespace itk
