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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"


template< typename TRegion >
static bool RunTest(const TRegion & region, const TRegion & exclusionRegion)
{
  const unsigned int ImageDimension = TRegion::ImageDimension;

  typedef itk::Index< ImageDimension >                  IndexPixelType;
  typedef unsigned char                                 ValuePixelType;

  typedef itk::Image< IndexPixelType, ImageDimension >  IndexImageType;
  typedef itk::Image< ValuePixelType, ImageDimension >  ValueImageType;

  typename IndexImageType::Pointer myIndexImage = IndexImageType::New();

  myIndexImage->SetLargestPossibleRegion( region );
  myIndexImage->SetBufferedRegion( region );
  myIndexImage->SetRequestedRegion( region );
  myIndexImage->Allocate();

  typename ValueImageType::Pointer  myValueImage  = ValueImageType::New();

  myValueImage->SetLargestPossibleRegion( region );
  myValueImage->SetBufferedRegion( region );
  myValueImage->SetRequestedRegion( region );
  myValueImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ValueImageType >  ValueIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< IndexImageType >  IndexIteratorType;

  const unsigned char normalRegionValue    = 100;
  const unsigned char exclusionRegionValue = 200;

  // Initialize the Image
  IndexIteratorType ii( myIndexImage, region );
  ValueIteratorType iv( myValueImage, region );

  ii.GoToBegin();
  iv.GoToBegin();

  while( !ii.IsAtEnd() )
    {
    ii.Set( ii.GetIndex() );
    iv.Set( normalRegionValue );
    ++ii;
    ++iv;
    }

  // Set a different value inside the exclusion region
  TRegion croppedExclusionRegion( exclusionRegion );
  if ( !croppedExclusionRegion.Crop( region ) )
    {
    // Exclusion region is completely outside the region. Set it to
    // have size 0.
    typename TRegion::IndexType exclusionStart = region.GetIndex();
    croppedExclusionRegion.SetIndex( exclusionStart );

    typename TRegion::SizeType exclusionSize = croppedExclusionRegion.GetSize();
    exclusionSize.Fill( 0 );
    croppedExclusionRegion.SetSize( exclusionSize );
    }

  ValueIteratorType ive( myValueImage, croppedExclusionRegion );

  ive.GoToBegin();
  while( !ive.IsAtEnd() )
    {
    ive.Set( exclusionRegionValue );
    ++ive;
    }

  typedef itk::ImageRegionExclusionIteratorWithIndex< IndexImageType > ExclusionIndexIteratorType;
  typedef itk::ImageRegionExclusionIteratorWithIndex< ValueImageType > ExclusionValueIteratorType;

  ExclusionValueIteratorType ev( myValueImage, region );
  ExclusionIndexIteratorType ei( myIndexImage, region );

  ev.SetExclusionRegion( exclusionRegion );
  ei.SetExclusionRegion( exclusionRegion );

  unsigned int numberOfPixelsVisited = 0;
  const unsigned int pixelsToVisit  = region.GetNumberOfPixels() -
                                      croppedExclusionRegion.GetNumberOfPixels();

  ev.GoToBegin();
  ei.GoToBegin();
  while( !ev.IsAtEnd() )
    {
    if( ei.Get() != ei.GetIndex() )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return false;
      }

    if( ev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
      }
    ++numberOfPixelsVisited;
    ++ei;
    ++ev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
    }

  numberOfPixelsVisited = 0;
  ev.GoToReverseBegin();
  ei.GoToReverseBegin();
  while( !ev.IsAtReverseEnd() )
    {
    if( ei.Get() != ei.GetIndex() )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return false;
      }

    if( ev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
      }
    ++numberOfPixelsVisited;
    --ei;
    --ev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion iterator" << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
    }

  typedef itk::ImageRegionExclusionConstIteratorWithIndex< IndexImageType > ExclusionIndexConstIteratorType;
  typedef itk::ImageRegionExclusionConstIteratorWithIndex< ValueImageType > ExclusionValueConstIteratorType;

  ExclusionValueConstIteratorType cev( myValueImage, region );
  ExclusionIndexConstIteratorType cei( myIndexImage, region );

  cev.SetExclusionRegion( exclusionRegion );
  cei.SetExclusionRegion( exclusionRegion );

  numberOfPixelsVisited = 0;

  cev.GoToBegin();
  cei.GoToBegin();
  while( !cev.IsAtEnd() )
    {
    if( cei.Get() != cei.GetIndex() )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return false;
      }

    if( cev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return false;
      }
    ++numberOfPixelsVisited;
    ++cei;
    ++cev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
    }

  numberOfPixelsVisited = 0;
  cev.GoToReverseBegin();
  cei.GoToReverseBegin();
  while( !cev.IsAtReverseEnd() )
    {
    if( cei.Get() != cei.GetIndex() )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return false;
      }

    if( cev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << cev.GetIndex() << std::endl;
      return false;
      }
    ++numberOfPixelsVisited;
    --cei;
    --cev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return false;
    }

  return true;
}


int itkImageRegionExclusionIteratorWithIndexTest(int, char* [] )
{
  const unsigned int                    Dimension = 3;
  typedef itk::Size< Dimension >        SizeType;
  typedef itk::Index< Dimension >       IndexType;
  typedef itk::ImageRegion< Dimension > RegionType;

  SizeType   regionSize;
  IndexType  regionStart;
  RegionType region;

  regionStart.Fill( 0 );
  regionSize.Fill( 7 );

  region.SetIndex( regionStart );
  region.SetSize( regionSize );

  SizeType::SizeValueType size[2] = {4, 7};

  unsigned int count = 0;
  for (SizeType::SizeValueType s = 0; s < 2; ++s)
    {
    for (IndexType::IndexValueType k = -2; k < 6; ++k)
      {
      for (IndexType::IndexValueType j = -2; j < 6; ++j)
        {
        for (IndexType::IndexValueType i = -2; i < 6; ++i)
          {
          IndexType exclusionStart;
          exclusionStart[0] = i;
          exclusionStart[1] = j;
          exclusionStart[2] = k;

          SizeType exclusionSize;
          exclusionSize.Fill( size[s] );

          RegionType exclusionRegion( exclusionStart, exclusionSize );

          count++;

          if ( !RunTest( region, exclusionRegion ) )
            {
            std::cerr << "Test failed for exclusion region: " << exclusionRegion;
            return EXIT_FAILURE;
            }
          }
        }
      }
    }

  // Test exclusion region completely outside the region.
  IndexType exclusionStart;
  exclusionStart.Fill( -3 );
  SizeType exclusionSize;
  exclusionSize.Fill( 2 );
  RegionType exclusionRegion( exclusionStart, exclusionSize );

  if ( !RunTest( region, exclusionRegion ) )
    {
    std::cerr << "Test failed for exclusion region: " << exclusionRegion;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
