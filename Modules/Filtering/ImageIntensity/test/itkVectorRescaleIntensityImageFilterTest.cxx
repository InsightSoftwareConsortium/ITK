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

#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkVectorRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

#include <iostream>

int itkVectorRescaleIntensityImageFilterTest( int, char* [] )
{
  const unsigned int VectorDimension = 3;

  typedef itk::Vector< int,   VectorDimension > InputPixelType;
  typedef itk::Vector< float, VectorDimension > OutputPixelType;

  const unsigned int ImageDimension = 3;

  typedef itk::Image< InputPixelType, ImageDimension > InputImageType;
  typedef itk::Image< OutputPixelType,ImageDimension > OutputImageType;

  InputImageType::Pointer    inputImage  = InputImageType::New();
  InputImageType::RegionType region;
  InputImageType::SizeType   size;
  InputImageType::IndexType  index;

  size.Fill( 20 );
  index.Fill( 0 );

  region.SetIndex( index );
  region.SetSize( size );

  InputPixelType pixelValue;
  pixelValue[0] = 10;
  pixelValue[1] = 20;
  pixelValue[2] = 30;

  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer( pixelValue );

  typedef itk::VectorRescaleIntensityImageFilter<
                                     InputImageType,
                                     OutputImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, VectorRescaleIntensityImageFilter,
    UnaryFunctorImageFilter );

  filter->SetInput( inputImage );

  const double desiredMaximum = 2.0;
  filter->SetOutputMaximumMagnitude( desiredMaximum );
  TEST_SET_GET_VALUE( desiredMaximum, filter->GetOutputMaximumMagnitude() );

  filter->SetFunctor( filter->GetFunctor() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  FilterType::InputRealType scale = filter->GetScale();
  std::cout << "Input scale value: " <<
    static_cast< itk::NumericTraits< FilterType::InputRealType >::PrintType > (scale) << std::endl;

  FilterType::InputRealType shift = filter->GetShift();
  std::cout << "Input scale value: " <<
    static_cast< itk::NumericTraits< FilterType::InputRealType >::PrintType > (shift) << std::endl;

  FilterType::InputRealType inputMaximumMagnitude =
    filter->GetInputMaximumMagnitude();
  std::cout << "Input maximum magnitude: " <<
    static_cast< itk::NumericTraits< FilterType::InputRealType >::PrintType > (inputMaximumMagnitude)
    << std::endl;

  OutputImageType::ConstPointer outputImage = filter->GetOutput();

  typedef itk::ImageRegionConstIterator< OutputImageType > IteratorType;

  IteratorType ot( outputImage, outputImage->GetBufferedRegion() );

  ot.GoToBegin();

  const double tolerance = 1e-3;

  const double factor = desiredMaximum / static_cast< double >( pixelValue.GetNorm() );

  while( !ot.IsAtEnd() )
    {
    const OutputPixelType outputValue = ot.Get();
    for( unsigned int k = 0; k < VectorDimension; k++ )
      {
      if( itk::Math::NotAlmostEquals( outputValue[k],
        itk::NumericTraits< itk::NumericTraits< OutputPixelType >::ValueType >::ZeroValue() ) )
        {
        if( !itk::Math::FloatAlmostEqual( static_cast< double > ( outputValue[k] ),
          static_cast< double >( pixelValue[k] * factor ), 10, tolerance ) )
          {
          std::cerr << "Test FAILED !" << std::endl;
          std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
          std::cerr << "Input  Pixel Value = " << pixelValue  << std::endl;
          std::cerr << "Output Pixel Value = " << outputValue << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    ++ot;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
