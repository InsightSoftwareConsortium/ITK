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


#include "itkVectorRescaleIntensityImageFilter.h"

int itkVectorRescaleIntensityImageFilterTest(int, char* [] )
{
  std::cout << "itkVectorRescaleIntensityImageFilterTest Start" << std::endl;

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

  inputImage->SetRegions(region);
  inputImage->Allocate();
  inputImage->FillBuffer( pixelValue );

  typedef itk::VectorRescaleIntensityImageFilter<
                                     InputImageType,
                                     OutputImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( inputImage );

  const double desiredMaximum =  1.0;

  filter->SetOutputMaximumMagnitude( desiredMaximum );

  try
    {
    filter->Update();
    filter->SetFunctor(filter->GetFunctor());
    }

  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }

  OutputImageType::ConstPointer outputImage = filter->GetOutput();

  typedef itk::ImageRegionConstIterator< OutputImageType > IteratorType;

  IteratorType ot( outputImage, outputImage->GetBufferedRegion() );

  ot.GoToBegin();

  const double tolerance = 1e-3;

  const double factor = desiredMaximum / static_cast< double >( pixelValue.GetNorm() );

  while( !ot.IsAtEnd() )
    {
    const OutputPixelType outputValue = ot.Get();
    for(unsigned int k=0; k < VectorDimension; k++)
      {
      if ( itk::Math::NotAlmostEquals( outputValue[k],
           itk::NumericTraits< itk::NumericTraits< OutputPixelType >::ValueType >::ZeroValue() ) )
        {
        if( std::fabs( outputValue[k] - pixelValue[k] * factor ) / outputValue[k] - 1.0 > tolerance )
          {
          std::cerr << "Test FAILED !" << std::endl;
          std::cerr << "Input  Pixel Value = " << pixelValue  << std::endl;
          std::cerr << "Output Pixel Value = " << outputValue << std::endl;
          }
        }
      }
    ++ot;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
