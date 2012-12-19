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
// Test FFT using FFTW Libraries. The test is performed for two 3D
// arrays, one of them having the same dimension(4,4,4) and the other
// having different dimensions (3,4,5).  Images are created with
// different dimensions in the test function based on the second
// template argument and the size of these dimensions are taken from
// the array.The data types used are float and double.
int itkFFTWD_RealFFTTest(int, char *[])
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

  std::cerr << "FFTWD:double,1 (4,4,4)" << std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD1> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD1> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,2 (4,4,4)" << std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD2> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD2> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,3 (4,4,4)" << std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD3> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD3> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWD:double,1 (3,5,4)" << std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD1> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD1> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWD:double,2 (3,5,4)" << std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD2> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD2> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWD:double,3 (3,5,4)" << std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToHalfHermitianForwardFFTImageFilter<ImageD3> ,
      itk::FFTWHalfHermitianToRealInverseFFTImageFilter<ImageCD3> >(SizeOfDimensions2)) != 0)
    rval++;

  // Exercise the plan rigor methods
  itk::FFTWRealToHalfHermitianForwardFFTImageFilter< ImageD3 >::Pointer fft =
    itk::FFTWRealToHalfHermitianForwardFFTImageFilter< ImageD3 >::New();
  fft->SetPlanRigor( FFTW_ESTIMATE );
  if ( fft->GetPlanRigor() != FFTW_ESTIMATE )
    {
    std::cerr << "Plan rigor read from FFT filter is not FFTW_ESTIMATE." << std::endl;
    return 0;
    }
  fft->SetPlanRigor( FFTW_MEASURE );

  itk::FFTWHalfHermitianToRealInverseFFTImageFilter< ImageCD3 >::Pointer ifft =
    itk::FFTWHalfHermitianToRealInverseFFTImageFilter< ImageCD3 >::New();
  ifft->SetPlanRigor( FFTW_ESTIMATE );
  if ( ifft->GetPlanRigor() != FFTW_ESTIMATE )
    {
    std::cerr << "Plan rigor read from FFT filter is not FFTW_ESTIMATE." << std::endl;
    return 0;
    }
  ifft->SetPlanRigor( FFTW_MEASURE );

  fft->Print(std::cout);
  ifft->Print(std::cout);

  return (rval == 0) ? 0 : -1;
}


#endif
