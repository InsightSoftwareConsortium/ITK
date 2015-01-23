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
#ifndef itkForwardInverseFFTTest_h
#define itkForwardInverseFFTTest_h

#include "itkAbsoluteValueDifferenceImageFilter.h"
#include "itkImageFileReader.h"

template< typename TForwardFFT, typename TInverseFFT >
bool ForwardInverseFullFFTTest(const char * inputFileName)
{
  double tolerance = 1.e-3;
  typedef typename TForwardFFT::InputImageType ImageType;
  typedef itk::ImageFileReader< ImageType >    ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );

  typename TForwardFFT::Pointer fft = TForwardFFT::New();
  fft->SetInput( reader->GetOutput() );

  typename TInverseFFT::Pointer ifft = TInverseFFT::New();
  ifft->SetInput( fft->GetOutput() );

  typedef itk::AbsoluteValueDifferenceImageFilter< ImageType, ImageType, ImageType >
    AbsDiffFilterType;
  typename AbsDiffFilterType::Pointer diffFilter = AbsDiffFilterType::New();
  diffFilter->SetInput1( reader->GetOutput() );
  diffFilter->SetInput2( ifft->GetOutput() );
  diffFilter->UpdateLargestPossibleRegion();

  bool success = true;
  typedef itk::ImageRegionConstIteratorWithIndex< ImageType > IteratorType;
  IteratorType it( diffFilter->GetOutput(),
                   diffFilter->GetOutput()->GetLargestPossibleRegion() );
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() > tolerance )
      {
      std::cerr << "Error found at pixel " << it.GetIndex() << "." << std::endl;
      std::cerr << "Absolute difference is " << it.Get()
                << ", which is greater than the acceptable tolerance of"
                << tolerance << "." << std::endl;
      success = false;
      break;
      }
    ++it;
    }

  return success;
}

template< typename TForwardFFT, typename TInverseFFT >
bool ForwardInverseHalfFFTTest(const char * inputFileName)
{
  double tolerance = 1.e-3;
  typedef typename TForwardFFT::InputImageType ImageType;
  typedef itk::ImageFileReader< ImageType >    ReaderType;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );
  reader->UpdateLargestPossibleRegion();

  typename TForwardFFT::Pointer fft = TForwardFFT::New();
  fft->SetInput( reader->GetOutput() );

  typename TInverseFFT::Pointer ifft = TInverseFFT::New();
  bool xIsOdd = reader->GetOutput()->GetLargestPossibleRegion().GetSize()[0] % 2 == 1;
  ifft->SetActualXDimensionIsOdd( xIsOdd );
  ifft->SetInput( fft->GetOutput() );

  typedef itk::AbsoluteValueDifferenceImageFilter< ImageType, ImageType, ImageType >
    AbsDiffFilterType;
  typename AbsDiffFilterType::Pointer diffFilter = AbsDiffFilterType::New();
  diffFilter->SetInput1( reader->GetOutput() );
  diffFilter->SetInput2( ifft->GetOutput() );
  diffFilter->UpdateLargestPossibleRegion();

  bool success = true;
  typedef itk::ImageRegionConstIteratorWithIndex< ImageType > IteratorType;
  IteratorType it( diffFilter->GetOutput(),
                   diffFilter->GetOutput()->GetLargestPossibleRegion() );
  while ( !it.IsAtEnd() )
    {
    if ( it.Get() > tolerance )
      {
      std::cerr << "ForwardInverseHalfFFTTest: Error found at pixel " << it.GetIndex() << "." << std::endl;
      std::cerr << "Absolute difference is " << it.Get()
                << ", which is greater than the acceptable tolerance of"
                << tolerance << "." << std::endl;
      success = false;
      break;
      }
    ++it;
    }

  return success;
}

#endif
