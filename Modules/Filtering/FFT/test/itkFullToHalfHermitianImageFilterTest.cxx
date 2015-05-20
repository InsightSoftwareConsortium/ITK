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

#include <iostream>

#include "itkChangeInformationImageFilter.h"
#include "itkHalfToFullHermitianImageFilter.h"
#include "itkFullToHalfHermitianImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkRealToHalfHermitianForwardFFTImageFilter.h"

int itkFullToHalfHermitianImageFilterTest(int argc, char *argv[])
{
  // Print usage information.
  if ( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <test image size x> <test image size y>"
              << std::endl;
    return EXIT_FAILURE;
    }

  // Read in image.
  typedef itk::Image< float, 2 >                 ImageType;
  typedef itk::Image< std::complex< float >, 2 > ComplexImageType;
  typedef itk::RandomImageSource< ImageType >    RandomSourceType;

  RandomSourceType::Pointer source = RandomSourceType::New();
  RandomSourceType::SizeType size;
  size[0] = atoi( argv[1] );
  size[1] = atoi( argv[2] );
  source->SetMin( 0.0f );
  source->SetMax( 1.0f );
  source->SetSize( size );
  source->Update();

  // Change the index of the image's largest possible region to test
  // generality of the filters.
  typedef itk::ChangeInformationImageFilter< ImageType > ChangeFilterType;
  ChangeFilterType::Pointer changer = ChangeFilterType::New();
  changer->ChangeRegionOn();
  ChangeFilterType::OutputImageOffsetValueType indexShift[2];
  indexShift[0] = -3;
  indexShift[1] =  5;
  changer->SetOutputOffset( indexShift );
  changer->SetInput( source->GetOutput() );

  // Compute frequency image, yielding the non-redundant half of the
  // full complex image.
  typedef itk::RealToHalfHermitianForwardFFTImageFilter< ImageType, ComplexImageType > FFTFilterType;
  FFTFilterType::Pointer fft = FFTFilterType::New();
  fft->SetInput( changer->GetOutput() );

  // Expand the non-redundant half to the full complex image.
  typedef itk::HalfToFullHermitianImageFilter< ComplexImageType > HalfToFullFilterType;
  HalfToFullFilterType::Pointer halfToFullFilter = HalfToFullFilterType::New();
  halfToFullFilter->SetActualXDimensionIsOddInput( fft->GetActualXDimensionIsOddOutput() );
  halfToFullFilter->SetInput( fft->GetOutput() );

  typedef itk::FullToHalfHermitianImageFilter< ComplexImageType > FullToHalfFilterType;
  FullToHalfFilterType::Pointer fullToHalfFilter = FullToHalfFilterType::New();
  fullToHalfFilter->SetInput( halfToFullFilter->GetOutput() );
  fullToHalfFilter->Update();
  fullToHalfFilter->Print(std::cout);

  // Check that the output of the full-to-half filter has the same
  // size as the output of the FFT filter.
  ComplexImageType::RegionType fftRegion = fft->GetOutput()->GetLargestPossibleRegion();
  if ( fullToHalfFilter->GetOutput()->GetLargestPossibleRegion() != fftRegion )
    {
    std::cerr << "Output size of full-to-half filter is not the same as the output size "
              << "of the FFT filter." << std::endl;
    return EXIT_FAILURE;
    }

  // Check that the output of the full-to-half filter is equal to the
  // output of the FFT filter.
  typedef itk::ImageRegionConstIterator< ComplexImageType > IteratorType;
  IteratorType fftIt( fft->GetOutput(), fftRegion );
  IteratorType f2hIt( fullToHalfFilter->GetOutput(), fftRegion );

  for (fftIt.GoToBegin(), f2hIt.GoToBegin(); !fftIt.IsAtEnd(); ++fftIt, ++f2hIt)
    {
    if ( fftIt.Get() != f2hIt.Get() )
      {
      std::cerr << "Pixel at index " << fftIt.GetIndex() << " does not match!" << std::endl;
      std::cerr << "FFT output: " << fftIt.Get() << ", full-to-half output: " << f2hIt.Get()
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
