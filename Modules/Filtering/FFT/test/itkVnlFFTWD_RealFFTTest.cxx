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


#include "itkRealFFTTest.h"

#if defined(ITK_USE_FFTWD)
// Compare FFT using VNL and FFTW Libraries. The test is performed for
// two 3D arrays, one of them having the same dimension(4,4,4) and the
// other having different dimensions (3,4,5). Images are created with
// different dimensions in the test function based on the second
// template argument and the size of these dimensions are taken from
// the array. The data types used are float and double.
int itkVnlFFTWD_RealFFTTest(int, char *[])
{
  typedef itk::Image< double, 1>               ImageD1;
  typedef itk::Image< double, 2>               ImageD2;
  typedef itk::Image< double, 3>               ImageD3;

  std::cout << "WriteWisdomCache  " << itk::FFTWGlobalConfiguration::GetWriteWisdomCache() << std::endl;
  std::cout << "ReadWisdomCache  " << itk::FFTWGlobalConfiguration::GetReadWisdomCache() << std::endl;
  std::cout << "PlanRigor  " << itk::FFTWGlobalConfiguration::GetPlanRigor() << std::endl;
  std::cout << "WisdomCacheBase " << itk::FFTWGlobalConfiguration::GetWisdomCacheBase()  << std::endl;
  std::cout << "WisdomeFile     " << itk::FFTWGlobalConfiguration::GetWisdomFileDefaultBaseName() << std::endl;

  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "VnlFFTWD:double,1 (4,4,4)" << std::endl;
  if((test_fft_rtc<double,1,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD1> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD1> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,2 (4,4,4)" << std::endl;
  if((test_fft_rtc<double,2,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD2> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD2> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,3 (4,4,4)" << std::endl;
  if((test_fft_rtc<double,3,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD3> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD3> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,1 (3,5,4)" << std::endl;
  if((test_fft_rtc<double,1,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD1> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD1> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,2 (3,5,4)" << std::endl;
  if((test_fft_rtc<double,2,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD2> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD2> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "VnlFFTWD:double,3 (3,5,4)" << std::endl;
  if((test_fft_rtc<double,3,
      itk::VnlRealToHalfHermitianForwardFFTImageFilter<ImageD3> ,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD3> >(SizeOfDimensions2)) != 0)
    rval++;

  return (rval == 0) ? 0 : -1;
}
#endif
