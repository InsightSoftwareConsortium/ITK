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

#include "itkHausdorffDistanceImageFilter.h"
#include "itkFilterWatcher.h"

int itkHausdorffDistanceImageFilterTest(int, char* [] )
{

  typedef unsigned int Pixel1Type;
  typedef float        Pixel2Type;
  enum { ImageDimension = 3 };

  typedef itk::Image<Pixel1Type,ImageDimension> Image1Type;
  typedef itk::Image<Pixel2Type,ImageDimension> Image2Type;

  Image1Type::Pointer image1 = Image1Type::New();
  Image2Type::Pointer image2 = Image2Type::New();

  Image1Type::SizeType size;
  size.Fill( 50 );

  image1->SetRegions( size );
  image2->SetRegions( size );

  image1->Allocate();
  image2->Allocate();

  image1->FillBuffer( itk::NumericTraits<Pixel1Type>::ZeroValue() );
  image2->FillBuffer( itk::NumericTraits<Pixel2Type>::ZeroValue() );

  typedef Image1Type::RegionType RegionType;
  RegionType region1;
  RegionType region2;

  typedef Image1Type::IndexType IndexType;
  IndexType index;

  size.Fill( 20 );
  index.Fill( 10 );
  region1.SetSize( size );
  region1.SetIndex( index );

  size.Fill( 15 );
  index.Fill( 20 );
  region2.SetSize( size );
  region2.SetIndex( index );

  itk::ImageRegionIterator<Image1Type> it1( image1, region1 );
  Pixel1Type count = itk::NumericTraits<Pixel1Type>::ZeroValue();
  while ( !it1.IsAtEnd() )
    {
    it1.Set( ++count );
    ++it1;
    }

  itk::ImageRegionIterator<Image2Type> it2( image2, region2 );
  while ( !it2.IsAtEnd() )
    {
    it2.Set( 7.2 );
    ++it2;
    }

  int exit_status=EXIT_SUCCESS; //If no failures detected, then EXIT_SUCCESS
  // compute the directed Hausdorff distance h(image1,image2)
  {
  typedef itk::DirectedHausdorffDistanceImageFilter<Image1Type,Image2Type> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter, "filter");

  filter->SetInput1( image1 );
  filter->SetInput2( image2 );
  filter->Update();
  filter->Print( std::cout );

  // check results
  const FilterType::RealType trueDistance = 10 * std::sqrt( static_cast<double>(ImageDimension) );
  const FilterType::RealType distance = filter->GetDirectedHausdorffDistance();

  std::cout << " True distance: " << trueDistance << std::endl;
  std::cout << " Computed computed: " << distance << std::endl;
  std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;
  if ( itk::Math::abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  if ( itk::Math::abs( 6.5 - filter->GetAverageHausdorffDistance() ) > 0.1 )
    {
    std::cout << "Test failed, average distance too great. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  }

  // compute the directed Hausdorff distance h(image2,image1)
  {
  typedef itk::DirectedHausdorffDistanceImageFilter<Image2Type,Image1Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image2 );
  filter->SetInput2( image1 );
  filter->Update();

  // check results
  const FilterType::RealType trueDistance = 5 * std::sqrt( static_cast<double>(ImageDimension) );
  const FilterType::RealType distance = filter->GetDirectedHausdorffDistance();

  std::cout << " True distance: " << trueDistance << std::endl;
  std::cout << " Computed computed: " << distance << std::endl;
  std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;

  if ( itk::Math::abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  if ( itk::Math::abs( 2.5 - filter->GetAverageHausdorffDistance() ) > 0.1 )
    {
    std::cout << "Test failed, average distance too great. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  }

  // compute the Hausdorff distance H(image1,image2)
  {
  typedef itk::HausdorffDistanceImageFilter<Image1Type,Image2Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image1 );
  filter->SetInput2( image2 );
  filter->Update();

  // check results
  const FilterType::RealType trueDistance = 10 * std::sqrt( static_cast<double>(ImageDimension) );
  const FilterType::RealType distance = filter->GetHausdorffDistance();

  std::cout << " True distance: " << trueDistance << std::endl;
  std::cout << " Computed computed: " << distance << std::endl;
  std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;

  if ( itk::Math::abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  if ( itk::Math::abs( 4.5 - filter->GetAverageHausdorffDistance() ) > 0.1 )
    {
    std::cout << "Test failed, average distance too great. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  }

  // compute the Hausdorff distance H(image2,image1)
  {
  typedef itk::HausdorffDistanceImageFilter<Image2Type,Image1Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image2 );
  filter->SetInput2( image1 );
  filter->Update();

  // check results
  const FilterType::RealType trueDistance = 10 * std::sqrt( static_cast<double>(ImageDimension) );
  const FilterType::RealType distance = filter->GetHausdorffDistance();

  std::cout << " True distance: " << trueDistance << std::endl;
  std::cout << " Computed computed: " << distance << std::endl;
  std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;

  if ( itk::Math::abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  if ( itk::Math::abs( 4.5 - filter->GetAverageHausdorffDistance() ) > 0.1 )
    {
    std::cout << "Test failed, average distance too great. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  }

  // compute the Hausdorff distance H(image2,image1)
  {
  Image1Type::SpacingType spacing1 = image1->GetSpacing();
  spacing1[0]=spacing1[0]/2;
  spacing1[1]=spacing1[1]/2;
  spacing1[2]=spacing1[2]/2;
  image1->SetSpacing(spacing1);
  Image2Type::SpacingType spacing2 = image2->GetSpacing();
  spacing2[0]=spacing2[0]/2;
  spacing2[1]=spacing2[1]/2;
  spacing2[2]=spacing2[2]/2;
  image2->SetSpacing(spacing2);
  typedef itk::HausdorffDistanceImageFilter<Image2Type,Image1Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image2 );
  filter->SetInput2( image1 );
  filter->SetUseImageSpacing(true);
  filter->Update();

  // check results
  const FilterType::RealType trueDistance = 10 * std::sqrt( spacing1[0]*spacing1[0]+spacing1[1]*spacing1[1]+spacing1[2]*spacing1[2]);
  const FilterType::RealType trueAverageDistance = 4.5 * spacing1[0];
  const FilterType::RealType distance = filter->GetHausdorffDistance();

  std::cout << " True distance: " << trueDistance << std::endl;
  std::cout << " Computed computed: " << distance << std::endl;
  std::cout << " Average distance: " << filter->GetAverageHausdorffDistance() << std::endl;

  if ( itk::Math::abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  if ( itk::Math::abs( trueAverageDistance - filter->GetAverageHausdorffDistance() ) > 0.1 )
    {
    std::cout << "Test failed, average distance too great. " << std::endl;
    exit_status = EXIT_FAILURE;
    }
  }

  if ( exit_status == EXIT_SUCCESS )
    {
    std::cout << "All Test passed. " << std::endl;
    }
  else
    {
    std::cout << "All Test failed. " << std::endl;
    }
  return exit_status;

}
