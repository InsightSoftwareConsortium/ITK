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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkConfigure.h"
#include "vnl/vnl_sample.h"
#include "itkTestMain.h"


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkVnlFFTTest);
#if defined(USE_FFTWF)
  REGISTER_TEST(itkFFTWF_FFTTest);
  REGISTER_TEST(itkVnlFFTWF_FFTTest);
#endif
#if defined(USE_FFTWD)
  REGISTER_TEST(itkFFTWD_FFTTest);
  REGISTER_TEST(itkVnlFFTWD_FFTTest);
#endif
#if defined(USE_FFTWD)
  REGISTER_TEST(itkCurvatureRegistrationFilterTest);
#endif
}
