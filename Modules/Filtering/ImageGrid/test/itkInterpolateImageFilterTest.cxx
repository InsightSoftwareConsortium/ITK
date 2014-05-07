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

#include "itkInterpolateImageFilter.h"

int itkInterpolateImageFilterTest(int, char* [] )
{
  enum { ImageDimension = 2 };
  typedef unsigned long                              InputPixelType;
  typedef unsigned long                              OutputPixelType;
  typedef itk::Image<InputPixelType,ImageDimension>  InputImageType;
  typedef itk::Image<OutputPixelType,ImageDimension> OutputImageType;

  // fill images
  typedef InputImageType::SizeType SizeType;
  SizeType size;
  size.Fill( 5 );

  InputImageType::Pointer image1 = InputImageType::New();
  image1->SetRegions( size );
  image1->Allocate();
  image1->FillBuffer( 100 );

  InputImageType::Pointer image2 = InputImageType::New();
  image2->SetRegions( size );
  image2->Allocate();


  itk::ImageRegionIteratorWithIndex<InputImageType> inIter2( image2,
    image2->GetBufferedRegion() );

  unsigned long temp = 0;
  while( !inIter2.IsAtEnd() )
    {
    inIter2.Set( temp );
    temp += 2;
    ++inIter2;
    }

  typedef itk::InterpolateImageFilter<InputImageType,OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image1 );
  filter->SetInput2( image2 );
  filter->Print( std::cout );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught unexpected exception: " << err << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // walk the output and check the results
  itk::ImageRegionIteratorWithIndex<InputImageType> inIter1( image1,
    image1->GetBufferedRegion() );
  inIter2.GoToBegin();

  itk::ImageRegionIteratorWithIndex<OutputImageType> outIter( filter->GetOutput(),
    filter->GetOutput()->GetBufferedRegion() );

  temp = 50;
  while( !inIter1.IsAtEnd() )
    {

    std::cout << " " << inIter1.Get()
              << " " << inIter2.Get()
              << " " << outIter.Get()
              << std::endl;

    if ( outIter.Get() != temp )
      {
      std::cout << "Expected output " << temp
                << " got " << outIter.Get()
                << " instead. " << std::endl;
      std::cout << "Test failed. " << std::endl;
      return EXIT_FAILURE;
      }

    ++temp;
    ++inIter1;
    ++inIter2;
    ++outIter;
    }

  // excerise other methods
  filter->GetInput1();
  filter->GetInput2();
  filter->GetInterpolator();
  filter->SetDistance( 0.2 );
  filter->GetDistance();

  // set the interpolator to be null
  filter->SetInterpolator( ITK_NULLPTR );
  bool pass = false;

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught expected exception: " << err << std::endl;
    pass = true;
    }

  if ( !pass )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
