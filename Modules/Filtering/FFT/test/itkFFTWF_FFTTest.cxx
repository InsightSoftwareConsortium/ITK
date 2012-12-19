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


#if defined(ITK_USE_FFTWF)
// Test FFT using FFTW Libraries. The test is performed for 2 3d array one of
// them having the same dimension(4,4,4) and the other having different
// dimensions (3,4,5).  Images are created with different dimensions in the
// test function based on the second template argument   and  the size of these
// dimensions are taken from the array.The data types used are float and
// double.
int itkFFTWF_FFTTest(int argc, char *argv[])
{
  typedef itk::Image< float, 1>               ImageF1;
  typedef itk::Image< std::complex<float>, 1> ImageCF1;
  typedef itk::Image< float, 2>               ImageF2;
  typedef itk::Image< std::complex<float>, 2> ImageCF2;
  typedef itk::Image< float, 3>               ImageF3;
  typedef itk::Image< std::complex<float>, 3> ImageCF3;

  // exercise the name-value conversion methods
  itk::FFTWGlobalConfiguration::GetPlanRigorValue("FFTW_EXHAUSTIVE");
  itk::FFTWGlobalConfiguration::GetPlanRigorName(FFTW_EXHAUSTIVE);

  itk::FFTWGlobalConfiguration::SetPlanRigor(FFTW_EXHAUSTIVE);
  itk::FFTWGlobalConfiguration::SetReadWisdomCache(true);
  itk::FFTWGlobalConfiguration::SetWriteWisdomCache(true);
  if(argc>1)
    {
    itk::FFTWGlobalConfiguration::SetWisdomCacheBase(argv[1]);
    }
  std::cout << "WriteWisdomCache  " << itk::FFTWGlobalConfiguration::GetWriteWisdomCache() << std::endl;
  std::cout << "ReadWisdomCache  " << itk::FFTWGlobalConfiguration::GetReadWisdomCache() << std::endl;
  std::cout << "PlanRigor  " << itk::FFTWGlobalConfiguration::GetPlanRigor() << std::endl;
  std::cout << "WisdomCacheBase " << itk::FFTWGlobalConfiguration::GetWisdomCacheBase()  << std::endl;
  std::cout << "WisdomeFile     " << itk::FFTWGlobalConfiguration::GetWisdomFileDefaultBaseName() << std::endl;

  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "FFTWF:float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWForwardFFTImageFilter<ImageF1> ,
      itk::FFTWInverseFFTImageFilter<ImageCF1> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWF:float,2 (4,4,4)" << std::endl;
  if((test_fft<float,2,
      itk::FFTWForwardFFTImageFilter<ImageF2> ,
      itk::FFTWInverseFFTImageFilter<ImageCF2> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWF:float,3 (4,4,4)" << std::endl;
  if((test_fft<float,3,
      itk::FFTWForwardFFTImageFilter<ImageF3> ,
      itk::FFTWInverseFFTImageFilter<ImageCF3> >(SizeOfDimensions1)) != 0)
    rval++;
  std::cerr << "FFTWF:float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWForwardFFTImageFilter<ImageF1> ,
      itk::FFTWInverseFFTImageFilter<ImageCF1> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWF:float,2 (3,5,4)" << std::endl;
  if((test_fft<float,2,
      itk::FFTWForwardFFTImageFilter<ImageF2> ,
      itk::FFTWInverseFFTImageFilter<ImageCF2> >(SizeOfDimensions2)) != 0)
    rval++;
  std::cerr << "FFTWF:float,3 (3,5,4)" << std::endl;
  if((test_fft<float,3,
      itk::FFTWForwardFFTImageFilter<ImageF3> ,
      itk::FFTWInverseFFTImageFilter<ImageCF3> >(SizeOfDimensions2)) != 0)
    rval++;

  // Exercise the plan rigor methods
  itk::FFTWForwardFFTImageFilter< ImageF3 >::Pointer fft =
    itk::FFTWForwardFFTImageFilter< ImageF3 >::New();
  fft->SetPlanRigor( FFTW_ESTIMATE );
  if ( fft->GetPlanRigor() != FFTW_ESTIMATE )
    {
    std::cerr << "Plan rigor read from FFT filter is not FFTW_ESTIMATE." << std::endl;
    return 0;
    }
  fft->SetPlanRigor( FFTW_MEASURE );

  itk::FFTWInverseFFTImageFilter< ImageCF3 >::Pointer ifft =
    itk::FFTWInverseFFTImageFilter< ImageCF3 >::New();
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
