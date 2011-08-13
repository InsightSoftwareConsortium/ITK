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

#if defined(USE_FFTWD)
int itkFFTWD_FFTTest(int, char *[])
{
  typedef itk::Image< double, 1>               ImageD1;
  typedef itk::Image< std::complex<double>, 1> ImageCD1;
  typedef itk::Image< double, 2>               ImageD2;
  typedef itk::Image< std::complex<double>, 2> ImageCD2;
  typedef itk::Image< double, 3>               ImageD3;
  typedef itk::Image< std::complex<double>, 3> ImageCD3;

  std::cout << "WriteWisdomCache  " << itk::FFTWGlobalConfiguration::GetWriteWisdomCache() << std::endl;
  std::cout << "ReadWisdomCache  " << itk::FFTWGlobalConfiguration::GetReadWisdomCache() << std::endl;
  std::cout << "PlanRigor  " << itk::FFTWGlobalConfiguration::GetPlanRigor() << std::endl;
  std::cout << "WisdomCacheBase " << itk::FFTWGlobalConfiguration::GetWisdomCacheBase()  << std::endl;
  std::cout << "WisdomeFile     " << itk::FFTWGlobalConfiguration::GetWisdomFileDefaultBaseName() << std::endl;

  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;

  std::cerr << "FFTWD:double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD1> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD1> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD2> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD2> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD3> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD3> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD1> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD1> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWD:double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD2> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD2> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWD:double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<ImageD3> ,
      itk::FFTWComplexConjugateToRealImageFilter<ImageCD3> >(SizeOfDimensions2)) != 0)
    rval++;
  return (rval == 0) ? 0 : -1;
}


#endif
