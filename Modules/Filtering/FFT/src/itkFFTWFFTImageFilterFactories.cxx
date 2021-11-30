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

static bool FFTWComplexToComplex1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWComplexToComplex1DFFTFactoryRegister__Private()
{
  if (!FFTWComplexToComplex1DFFTHasBeenRegistered)
  {
    FFTWComplexToComplex1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWComplexToComplex1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWComplexToComplexFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWComplexToComplexFFTFactoryRegister__Private()
{
  if (!FFTWComplexToComplexFFTHasBeenRegistered)
  {
    FFTWComplexToComplexFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWComplexToComplexFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWForward1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWForward1DFFTFactoryRegister__Private()
{
  if (!FFTWForward1DFFTHasBeenRegistered)
  {
    FFTWForward1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWForward1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWForwardFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWForwardFFTFactoryRegister__Private()
{
  if (!FFTWForwardFFTHasBeenRegistered)
  {
    FFTWForwardFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWForwardFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWHalfHermitianToRealInverseFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWHalfHermitianToRealInverseFFTFactoryRegister__Private()
{
  if (!FFTWHalfHermitianToRealInverseFFTHasBeenRegistered)
  {
    FFTWHalfHermitianToRealInverseFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWHalfHermitianToRealInverseFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWInverse1DFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWInverse1DFFTFactoryRegister__Private()
{
  if (!FFTWInverse1DFFTHasBeenRegistered)
  {
    FFTWInverse1DFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWInverse1DFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWInverseFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWInverseFFTFactoryRegister__Private()
{
  if (!FFTWInverseFFTHasBeenRegistered)
  {
    FFTWInverseFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWInverseFFTImageFilter>::RegisterOneFactory();
  }
}

static bool FFTWRealToHalfHermitianForwardFFTHasBeenRegistered;
void        ITKFFT_EXPORT
            FFTWRealToHalfHermitianForwardFFTFactoryRegister__Private()
{
  if (!FFTWRealToHalfHermitianForwardFFTHasBeenRegistered)
  {
    FFTWRealToHalfHermitianForwardFFTHasBeenRegistered = true;
    FFTImageFilterFactory<FFTWRealToHalfHermitianForwardFFTImageFilter>::RegisterOneFactory();
  }
}
} // end namespace itk

#endif // defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
