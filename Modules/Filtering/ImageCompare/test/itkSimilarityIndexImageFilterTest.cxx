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

#include "itkSimilarityIndexImageFilter.h"
#include "itkMath.h"

int itkSimilarityIndexImageFilterTest(int, char* [] )
{

  typedef unsigned char Pixel1Type;
  typedef float         Pixel2Type;
  enum { ImageDimension = 2 };

  typedef itk::Image<Pixel1Type,ImageDimension> Image1Type;
  typedef itk::Image<Pixel2Type,ImageDimension> Image2Type;

  Image1Type::Pointer image1 = Image1Type::New();
  Image2Type::Pointer image2 = Image2Type::New();

  Image1Type::SizeType size;
  size.Fill( 8 );

  image1->SetRegions( size );
  image2->SetRegions( size );

  image1->Allocate();
  image2->Allocate();

  unsigned long numOfPixels = image1->GetBufferedRegion().GetNumberOfPixels();
  unsigned long lower1 = 0;
  unsigned long upper1 = (unsigned long) ( (double)numOfPixels * 0.75 ) - 1;
  unsigned long lower2 = (unsigned long) ( (double)numOfPixels * 0.25 );
  unsigned long upper2 = numOfPixels - 1;

  itk::ImageRegionIterator<Image1Type> it1( image1, image1->GetBufferedRegion() );
  itk::ImageRegionIterator<Image2Type> it2( image2, image2->GetBufferedRegion() );
  unsigned long count = 0;

  while( !it1.IsAtEnd() || !it2.IsAtEnd() )
    {

    if ( !it1.IsAtEnd() )
      {
      if ( lower1 <= count && count <= upper1 )
        {
        it1.Set( 5 );
        }
      else
        {
        it1.Set( 0 );
        }
      ++it1;
      }

    if ( !it2.IsAtEnd() )
      {
      if ( lower2 <= count && count <= upper2 )
        {
        it2.Set( 7.2 );
        }
      else
        {
        it2.Set( 0 );
        }
      ++it2;
      }

    ++count;

    }

  typedef itk::SimilarityIndexImageFilter<Image1Type,Image2Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image1 );
  filter->SetInput2( image2 );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "Caught an unexpected exception" << std::endl;
    std::cout << err;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  filter->Print(std::cout);

  // check results
  FilterType::RealType trueOverlap = 0.5 / 0.75;
  FilterType::RealType overlap = filter->GetSimilarityIndex();

  std::cout << " True index: " << trueOverlap << std::endl;
  std::cout << " Computed index: " << overlap << std::endl;

  if ( itk::Math::abs( trueOverlap - overlap ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // test case where both images are zero
 Image1Type::Pointer image3 = Image1Type::New();
 Image2Type::Pointer image4 = Image2Type::New();

 image3->SetRegions( image1->GetBufferedRegion() );
 image3->Allocate();
 image3->FillBuffer( 0 );

 image4->SetRegions( image2->GetBufferedRegion() );
 image4->Allocate();
 image4->FillBuffer( 0 );

 filter->SetInput1( image3 );
 filter->SetInput2( image4 );
 filter->Update();

 if ( itk::Math::NotExactlyEquals(filter->GetSimilarityIndex(), 0) )
    {
    std::cout << "Overlap: " << filter->GetSimilarityIndex() << std::endl;
    std::cout << "Zero overlap expected." << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;

}
