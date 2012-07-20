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

#include "itkImageRegionConstIteratorWithOnlyIndex.h"

template <typename TImage>
class itkImageRegionConstIteratorWithOnlyIndexTestIteratorTester
{

  public:
    typedef TImage                          ImageType;
    typedef typename ImageType::IndexType   IndexType;

    typedef itk::ImageRegionConstIteratorWithOnlyIndex<ImageType > ConstIteratorType;

    itkImageRegionConstIteratorWithOnlyIndexTestIteratorTester( )
    {
      m_Image = ImageType::New();

      typename ImageType::SizeType size;
      size.Fill(100);

      typename ImageType::IndexType start;
      start.Fill(0);

      typename ImageType::RegionType region;
      region.SetSize( size );
      region.SetIndex( start );

      m_Image->SetRegions( region );

      // Setup a smaller requested region
      size.Fill( 50 );
//      size[0] = 40;
      start.Fill ( 10 );
//      start[0] = 8;
      region.SetSize( size );
      region.SetIndex( start );
      m_Image->SetRequestedRegion( region );
    }

    bool TestConstIterator()
    {
      typename ImageType::RegionType region = m_Image->GetBufferedRegion();
      if( TestByRegion( region ) == false )
        {
        std::cout << "Failed testing buffered region." << std::endl;
        return false;
        }

      region = m_Image->GetRequestedRegion();
      if( TestByRegion( region ) == false )
        {
        std::cout << "Failed testing requested region." << std::endl;
        return false;
        }

      return true;
    }

    bool TestByRegion( typename ImageType::RegionType & region )
    {
      ConstIteratorType it( m_Image, region );
      it.GoToBegin();
      typename ImageType::IndexValueType step = 0;

      while( !it.IsAtEnd() )
        {
        IndexType index = it.GetIndex();
        // Check to see if the index is within allowed bounds
        bool isInside = region.IsInside(index);
        if( !isInside )
          {
          std::cout << "Index is not inside region! - " << index << std::endl;
          return false;
          }
        // check repeatibility
        if( index != it.GetIndex() )
          {
          std::cout << "Failed to repeat GetIndex." << std::endl;
          return false;
          }
        // increment and test index
        ++it;
        IndexType truthIndex;
        truthIndex[0] = step % region.GetSize()[0] + region.GetIndex()[0];
        truthIndex[1] = step / region.GetSize()[0] + region.GetIndex()[1];
        if( ImageType::GetImageDimension() > 2 )
          {
          truthIndex[1] = ( step / region.GetSize()[0] ) % region.GetSize()[1] + region.GetIndex()[1];
          truthIndex[2] = step / ( region.GetSize()[0] * region.GetSize()[1] ) + region.GetIndex()[2];
          }

        if( index != truthIndex  )
          {
          std::cout << "Failed single increment. step: " << step << " index: " << index << " truthIndex: " << truthIndex << std::endl;
          return false;
          }
        ++step;
        // check repeatibility after decrement
        --it;
        if( index != it.GetIndex() )
          {
          std::cout << "Failed to increment and decrement." << std::endl;
          return false;
          }
        ++it;
        }

      // Test iterating fwd by line
      IndexType index;
      it.GoToBegin();
      index = it.GetIndex();
      for( unsigned int i=0; i < region.GetSize()[0]; i++ )
        {
        ++it;
        }
      if( index[0] != it.GetIndex()[0] || index[1] != it.GetIndex()[1] - 1 )
        {
        std::cout << "Failed iterating forward by line." << std::endl;
        }

      // iterate back
      for( unsigned int i=0; i < region.GetSize()[0]; i++ )
        {
        --it;
        }
      if( index != it.GetIndex() )
        {
        std::cout << "Failed iterating back by line." << std::endl;
        }

      // Test iterating fwd by slice
      if( ImageType::GetImageDimension() > 2 )
        {
        it.GoToBegin();
        index = it.GetIndex();
        for( unsigned int i=0; i < region.GetSize()[0] * region.GetSize()[1]; i++ )
          {
          ++it;
          }
        ++it; //extra step
        if( index[0] != it.GetIndex()[0] - 1 || index[1] != it.GetIndex()[1] || index[2] != it.GetIndex()[2] - 1 )
          {
          std::cout << "Failed iterating forward by slice." << std::endl;
          }

        // iterate back
        for( unsigned int i=0; i < region.GetSize()[0] * region.GetSize()[1]; i++ )
          {
          --it;
          }
        --it;
        if( index != it.GetIndex() )
          {
          std::cout << "Failed iterating back by slice." << std::endl;
          }
        }

      return true;
    }

  private:

    typename ImageType::Pointer m_Image;

};

int itkImageRegionConstIteratorWithOnlyIndexTest(int, char* [] )
{
  bool testPassed = true; // let's be optimistic

  // Instantiate image of various types and
  // test the iterators on them

  {
  std::cout << "Testing with Image< char, 3 >... " << std::endl;
  itkImageRegionConstIteratorWithOnlyIndexTestIteratorTester< itk::Image< char, 3 > > Tester;
  if( Tester.TestConstIterator() == false )
    {
    testPassed = false;
    }
  }

  {
  std::cout << "Testing with ImageBase< 2 >... " << std::endl;
  itkImageRegionConstIteratorWithOnlyIndexTestIteratorTester< itk::ImageBase< 2 > > Tester;
  if( Tester.TestConstIterator() == false )
    {
    testPassed = false;
    }
  }

  {
  std::cout << "Testing with ImageBase< 3 >... " << std::endl;
  itkImageRegionConstIteratorWithOnlyIndexTestIteratorTester< itk::ImageBase< 3 > > Tester;
  if( Tester.TestConstIterator() == false )
    {
    testPassed = false;
    }
  }

  if ( !testPassed )
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Success" << std::endl;
  return EXIT_SUCCESS;

}
