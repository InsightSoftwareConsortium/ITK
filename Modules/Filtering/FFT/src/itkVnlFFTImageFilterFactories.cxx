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

static bool VnlComplexToComplex1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlComplexToComplex1DFFTFactoryRegister__Private()
{
  if (!VnlComplexToComplex1DFFTHasBeenRegistered)
  {
    VnlComplexToComplex1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlComplexToComplex1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlComplexToComplexFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlComplexToComplexFFTFactoryRegister__Private()
{
  if (!VnlComplexToComplexFFTHasBeenRegistered)
  {
    VnlComplexToComplexFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlComplexToComplexFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlForward1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlForward1DFFTFactoryRegister__Private()
{
  if (!VnlForward1DFFTHasBeenRegistered)
  {
    VnlForward1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlForward1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlForwardFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlForwardFFTFactoryRegister__Private()
{
  if (!VnlForwardFFTHasBeenRegistered)
  {
    VnlForwardFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlForwardFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlHalfHermitianToRealInverseFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlHalfHermitianToRealInverseFFTFactoryRegister__Private()
{
  if (!VnlHalfHermitianToRealInverseFFTHasBeenRegistered)
  {
    VnlHalfHermitianToRealInverseFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlHalfHermitianToRealInverseFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlInverse1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlInverse1DFFTFactoryRegister__Private()
{
  if (!VnlInverse1DFFTHasBeenRegistered)
  {
    VnlInverse1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlInverse1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlInverseFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlInverseFFTFactoryRegister__Private()
{
  if (!VnlInverseFFTHasBeenRegistered)
  {
    VnlInverseFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlInverseFFTImageFilter>::RegisterOneFactory();
  }
}

static bool VnlRealToHalfHermitianForwardFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            VnlRealToHalfHermitianForwardFFTFactoryRegister__Private()
{
  if (!VnlRealToHalfHermitianForwardFFTHasBeenRegistered)
  {
    VnlRealToHalfHermitianForwardFFTHasBeenRegistered = true;
    FFTImageFilterFactory<VnlRealToHalfHermitianForwardFFTImageFilter>::RegisterOneFactory();
  }
}
} // end namespace itk
