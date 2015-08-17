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

#include "itkImageAlgorithm.h"

#include "itkTestingMacros.h"

#include "itkExtractImageFilter.h"
#include "itkTestingComparisonImageFilter.h"

#include "itkImageAdaptor.h"
#include "itkAbsImageAdaptor.h"
#include "itkMath.h"

namespace {

template < typename TImageType >
bool CheckBuffer( const TImageType* image, typename TImageType::PixelType p )
{
  typedef itk::ImageRegionConstIterator<TImageType> ImageIterator;

  ImageIterator iter( image, image->GetBufferedRegion() );

  while( !iter.IsAtEnd() )
    {
    if (itk::Math::NotExactlyEquals(iter.Get(), p))
      return false;
    ++iter;
    }
  return true;

}

}

int itkImageAlgorithmCopyTest2( int, char *[] )
{

  typedef itk::Image<float, 3> Float3DImageType;
  typedef itk::Image<short, 3> Short3DImageType;

  typedef itk::Image<std::vector<float>, 3>               STDVectorImageType;
  typedef itk::AbsImageAdaptor< Float3DImageType, short > AbsImageType;

  typedef itk::ImageRegion<3> RegionType;


  RegionType::IndexType index;
  index.Fill(0);
  RegionType::SizeType size;
  size.Fill(64);

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);


  Short3DImageType::Pointer image1 = Short3DImageType::New();
  image1->SetRegions( region );
  image1->Allocate();
  image1->FillBuffer( 13 );

  Short3DImageType::Pointer image2 = Short3DImageType::New();
  image2->SetRegions( region );
  image2->Allocate(true); // initialize buffer to zero


  Float3DImageType::Pointer image3 = Float3DImageType::New();
  image3->SetRegions( region );
  image3->Allocate(true); // initialize buffer to zero

  std::cout << "Copying two images of same type"  << std::endl;
  itk::ImageAlgorithm::Copy( image1.GetPointer(), image2.GetPointer(), region, region );

  TEST_EXPECT_TRUE( CheckBuffer( image2.GetPointer(), 13 ) );

  std::cout << "Copying images of different types" << std::endl;
  itk::ImageAlgorithm::Copy( image1.GetPointer(), image3.GetPointer(), region, region );

  TEST_EXPECT_TRUE( CheckBuffer( image3.GetPointer(), 13 ) );

  image1->FillBuffer(0);

  itk::ImageAlgorithm::Copy( image3.GetPointer(), image1.GetPointer(), region, region );

  TEST_EXPECT_TRUE( CheckBuffer( image1.GetPointer(), 13 ) );


  AbsImageType::Pointer absimage = AbsImageType::New();
  absimage->SetImage( image3 );
  image2->FillBuffer( 0 );

  std::cout << "Copying from adaptor" << std::endl;
  itk::ImageAlgorithm::Copy( absimage.GetPointer(), image2.GetPointer(), region, region );

  TEST_EXPECT_TRUE( CheckBuffer( image2.GetPointer(), 13 ) );


  STDVectorImageType::Pointer image4 = STDVectorImageType::New();
  image4->SetRegions( region );
  image4->Allocate();
  image4->FillBuffer( std::vector<float>( 10, 3.14) );

  STDVectorImageType::Pointer image5 = STDVectorImageType::New();
  image5->SetRegions( region );
  image5->Allocate();

  std::cout << "Copying Non-POD pixels" << std::endl;
  itk::ImageAlgorithm::Copy( image4.GetPointer(), image5.GetPointer(), region, region );

  TEST_EXPECT_TRUE( CheckBuffer( image5.GetPointer(), std::vector<float>( 10, 3.14) ) );

  return EXIT_SUCCESS;
}
