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


#include "itkFFTTest.h"

#if defined(ITK_USE_FFTWD)
// Compare FFT using VNL and FFTW Libraries. The test is performed for 2 3d
// array one of them having the same dimension(4,4,4) and the other having
// different dimensions (3,4,5). Images are created with different dimensions
// in the test function based on the second template argument   and  the size
// of these dimensions are taken from the array.The data types used are float
// and double.
int
itkVnlFFTWD_FFTTest(int, char *[])
{
  using ImageD1 = itk::Image<double, 1>;
  using ImageD2 = itk::Image<double, 2>;
  using ImageD3 = itk::Image<double, 3>;

#  ifndef ITK_USE_CUFFTW
  std::cout << "WriteWisdomCache  " << itk::FFTWGlobalConfiguration::GetWriteWisdomCache() << std::endl;
  std::cout << "ReadWisdomCache  " << itk::FFTWGlobalConfiguration::GetReadWisdomCache() << std::endl;
  std::cout << "PlanRigor  " << itk::FFTWGlobalConfiguration::GetPlanRigor() << std::endl;
  std::cout << "WisdomCacheBase " << itk::FFTWGlobalConfiguration::GetWisdomCacheBase() << std::endl;
  std::cout << "WisdomeFile     " << itk::FFTWGlobalConfiguration::GetWisdomFileDefaultBaseName() << std::endl;
#  endif

  unsigned int SizeOfDimensions1[] = { 4, 4, 4 };
  unsigned int SizeOfDimensions2[] = { 3, 5, 4 };
  int          rval = 0;
  std::cerr << "VnlFFTWD:double,1 (4,4,4)" << std::endl;
  if ((test_fft_rtc<double, 1, itk::VnlForwardFFTImageFilter<ImageD1>, itk::FFTWForwardFFTImageFilter<ImageD1>>(
        SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,2 (4,4,4)" << std::endl;
  if ((test_fft_rtc<double, 2, itk::VnlForwardFFTImageFilter<ImageD2>, itk::FFTWForwardFFTImageFilter<ImageD2>>(
        SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,3 (4,4,4)" << std::endl;
  if ((test_fft_rtc<double, 3, itk::VnlForwardFFTImageFilter<ImageD3>, itk::FFTWForwardFFTImageFilter<ImageD3>>(
        SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,1 (3,5,4)" << std::endl;
  if ((test_fft_rtc<double, 1, itk::VnlForwardFFTImageFilter<ImageD1>, itk::FFTWForwardFFTImageFilter<ImageD1>>(
        SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,2 (3,5,4)" << std::endl;
  if ((test_fft_rtc<double, 2, itk::VnlForwardFFTImageFilter<ImageD2>, itk::FFTWForwardFFTImageFilter<ImageD2>>(
        SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,3 (3,5,4)" << std::endl;
  if ((test_fft_rtc<double, 3, itk::VnlForwardFFTImageFilter<ImageD3>, itk::FFTWForwardFFTImageFilter<ImageD3>>(
        SizeOfDimensions2)) != 0)
    rval++;

  return (rval == 0) ? 0 : -1;
}
#endif
