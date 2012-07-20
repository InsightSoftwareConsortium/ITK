/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License" << std::endl;
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

#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkConstNeighborhoodIteratorWithOnlyIndex.h"

template <typename TImage>
typename TImage::Pointer itkConstNeighborhoodIteratorWithOnlyIndexTestGetTestImage(int d1, int d2, int d3, int d4)
{
  itk::Size<4>  sizeND;
   sizeND[0] = d1;
   sizeND[1] = d2;
   sizeND[2] = d3;
   sizeND[3] = d4;

  itk::Index<4> origND;
   origND.Fill(0);

  itk::ImageRegion<4> RegionND;
   RegionND.SetSize(sizeND);
   RegionND.SetIndex(origND);

  typename TImage::Pointer imageND = TImage::New();
   imageND->SetLargestPossibleRegion(RegionND);
   imageND->SetBufferedRegion(RegionND);
   imageND->SetRequestedRegion(RegionND);

  return  imageND;
}

template< typename TImage >
int itkConstNeighborhoodIteratorWithOnlyIndexTestRun()
{
  typename TImage::Pointer img = itkConstNeighborhoodIteratorWithOnlyIndexTestGetTestImage<TImage>(10, 10, 5, 3);

  typedef TImage                                                    ImageType;
  typedef itk::ConstNeighborhoodIteratorWithOnlyIndex< ImageType >  ConstNeighborhoodIteratorType;
  typedef typename ConstNeighborhoodIteratorType::IndexType         IndexType;

  IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  typename ConstNeighborhoodIteratorType::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  typename ConstNeighborhoodIteratorType::RegionType reg;
  typename ConstNeighborhoodIteratorType::SizeType sz;
  IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx);
  reg.SetSize(sz);

  std::cout << "Creating ConstNeighborhoodIterator" << std::endl;
  ConstNeighborhoodIteratorType it(radius, img, reg);

  std::cout << "Moving iterator using SetLocation() to loc: " << loc << std::endl;
  it.SetLocation(loc);
  it.Print(std::cout);
  if( it.GetIndex() != loc )
    {
    std::cerr << "Error with SetLocation." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test GetIndex( NeighborhoodIndex )" << std::endl;
  if( it.GetIndex( it.GetCenterNeighborhoodIndex() ) != loc )
    {
    std::cerr << "Error getting index from center nhood index. Returned " << it.GetIndex( it.GetCenterNeighborhoodIndex() ) << std::endl;
    return EXIT_FAILURE;
    }
  IndexType truthIndex;
  for( unsigned int i=0; i < 4; i++ )
    {
    truthIndex[i] = loc[i] - radius[i];
    }
  if( it.GetIndex( 0 ) != truthIndex )
    {
    std::cerr << "Error getting index from nhood index 0. Returned " << it.GetIndex( 0 ) << std::endl;
    }

  std::cout << "Testing GoToBegin()" << std::endl;
  it.GoToBegin();
  it.Print(std::cout);
  if( it.GetIndex() != idx )
    {
    std::cerr << "Error with GoToBegin. Expected: " << idx << ", GetIndex: " << it.GetIndex() << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Testing IsAtBegin()" << std::endl;
  std::cout << it.IsAtBegin() << std::endl;
  if( ! it.IsAtBegin() )
    {
    std::cerr << "Error with IsAtBegin." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Testing GoToEnd()" << std::endl;
  it.GoToEnd();
  it.Print(std::cout);

  std::cout << "Testing IsAtEnd()" << std::endl;
  std::cout << it.IsAtEnd() << std::endl;
  if( ! it.IsAtEnd() )
    {
    std::cerr << "Error with either IsAtEnd or GoToEnd." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Testing forward iteration" << std::endl;
  it.GoToBegin();
  IndexType index = it.GetIndex();
  ++it;
  if( it.GetIndex()[0] != index[0]+1 )
    {
    std::cerr << "Error with fwd iteration" << std::endl;
    return EXIT_FAILURE;
    }
  while (! it.IsAtEnd())
    {
    ++it;
    }

  // fwd iterate by a line
  it.GoToBegin();
  index = it.GetIndex();
  for( unsigned int i = 0; i < sz[0]; i++ )
    {
    ++it;
    }
  if( it.GetIndex()[0] != index[0] || it.GetIndex()[1] != index[1]+1 )
    {
    std::cerr << "Error with fwd iteration by one line." << std::endl;
    return EXIT_FAILURE;
    }

  // fwd iterate by a slice
  index = it.GetIndex();
  for( unsigned int i = 0; i < sz[0]*sz[1]; i++ )
    {
    ++it;
    }
  if( it.GetIndex()[0] != index[0] || it.GetIndex()[1] != index[1] || it.GetIndex()[2] != index[2] + 1 )
    {
    std::cerr << "Error with fwd iteration by one slice." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Testing reverse iteration" << std::endl;
  it.GoToEnd();
  index = it.GetIndex();
  --it;
  if( it.GetIndex()[3] != index[3]-1 )
    {
    std::cerr << "Error with reverse iteration" << std::endl;
    return EXIT_FAILURE;
    }

  index = it.GetIndex();
  for( unsigned int i = 0; i < sz[0]; i++ )
    {
    --it;
    }
  if( it.GetIndex()[0] != index[0] || it.GetIndex()[1] != index[1] - 1 )
    {
    std::cerr << "Error with reverse iteration by one line." << std::endl;
    return EXIT_FAILURE;
    }

  index = it.GetIndex();
  for( unsigned int i = 0; i < sz[0]*sz[1]; i++ )
    {
    --it;
    }
  if( it.GetIndex()[0] != index[0] || it.GetIndex()[1] != index[1] || it.GetIndex()[2] != index[2] - 1 )
    {
    std::cerr << "Error with reverse iteration by one slice." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Moving iterator using SetLocation()" << std::endl;
  it.SetLocation(loc);
  it.Print(std::cout);

  std::cout << "Testing GetIndex()" << std::endl;
  std::cout << it.GetIndex() << std::endl;

  std::cout << "Testing GetBoundingBoxAsImageRegion" << std::endl;
  std::cout << it.GetBoundingBoxAsImageRegion() << std::endl;

  ////

  std::cout << "Testing random access iteration" << std::endl;

  typename ImageType::Pointer ra_img = itkConstNeighborhoodIteratorWithOnlyIndexTestGetTestImage<TImage>(10, 10, 5, 3);
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;

  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  std::cout << "Creating ConstNeighborhoodIterator" << std::endl;
  ConstNeighborhoodIteratorType ra_it(radius, ra_img, ra_img->GetRequestedRegion());
  ConstNeighborhoodIteratorType copy_it;

  std::cout << "Test copying." << std::endl;
  copy_it = ra_it;
  if( copy_it != ra_it || ! (copy_it == ra_it) )
    {
    std::cerr << "Failure with copying or equality comparison." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! (copy_it >= ra_it) || ! (copy_it <= ra_it) )
    {
    std::cerr << "Failure with copying or >= or <= comparison." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Testing random access" << std::endl;
  ra_it.SetLocation( loc );

  std::cout << "Adding [1, 1, 1, 1]" << std::endl;
  OffsetType a_off;
  a_off.Fill(1);
  ra_it += a_off;
  for( unsigned int i=0; i < 4; i++ )
    {
    IndexType ind = ra_it.GetIndex();
    if( ind[i] != loc[i] + 1 )
      {
      std::cerr << "Error with adding offset " << a_off << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test iterator comparisons" << std::endl;
  if( ! (copy_it < ra_it) || ! (copy_it <= ra_it) )
    {
    std::cerr << "Error with < or <=." << std::endl;
    return EXIT_FAILURE;
    }
  if( ! (copy_it < ra_it) || ! (copy_it <= ra_it) )
    {
    std::cerr << "Error with < or <=." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Subtracting [1, 1, 1, 1]" << std::endl;
  ra_it -= a_off;
  for( unsigned int i=0; i < 4; i++ )
    {
    IndexType ind = ra_it.GetIndex();
    if( ind[i] != loc[i] )
      {
      std::cerr << "Error with substracting offset " << a_off << std::endl;
      return EXIT_FAILURE;
      }
    }

  copy_it = ra_it;

  std::cout << "Adding [0 0 0 2]" << std::endl;
  a_off.Fill(0);
  a_off[3] = 2;
  ra_it += a_off;
  IndexType truth = loc;
  truth += a_off;
  for( unsigned int i=0; i < 4; i++ )
    {
    IndexType ind = ra_it.GetIndex();
    if( ind[i] != truth[i] )
      {
      std::cerr << "Error with adding offset " << a_off << std::endl;
      return EXIT_FAILURE;
      }
    }

  if( ra_it < copy_it || ra_it <= copy_it || copy_it > ra_it || copy_it >= ra_it )
    {
    std::cerr << "Error with > or >= comparison after adding offset." << std::endl;
    return EXIT_FAILURE;
    }

  copy_it = ra_it;

  std::cout << "Adding [0 8 0 0]" << std::endl;
  a_off.Fill(0);
  a_off[1] = 8;
  ra_it += a_off;
  truth += a_off;
  for( unsigned int i=0; i < 4; i++ )
    {
    IndexType ind = ra_it.GetIndex();
    if( ind[i] != truth[i] )
      {
      std::cerr << "Error with adding offset " << a_off << std::endl;
      return EXIT_FAILURE;
      }
    }

  if( ra_it < copy_it || ra_it <= copy_it || copy_it > ra_it || copy_it >= ra_it )
    {
    std::cerr << "Error with comparison after adding offset " << a_off << std::endl;
    std::cerr << (ra_it < copy_it) << ", " << (ra_it <= copy_it) << ", " << (copy_it > ra_it) << ", " << (copy_it >= ra_it) << std::endl;
    return EXIT_FAILURE;
    }

  copy_it = ra_it;

  std::cout << "Adding [5 -3 2 -1]" << std::endl;
  a_off[0] = 5;
  a_off[1] = -3;
  a_off[2] = 2;
  a_off[3] = -1;
  ra_it += a_off;
  truth += a_off;
  for( unsigned int i=0; i < 4; i++ )
    {
    IndexType ind = ra_it.GetIndex();
    if( ind[i] != truth[i] )
      {
      std::cerr << "Error with adding offset " << a_off << std::endl;
      return EXIT_FAILURE;
      }
    }

  if( ra_it > copy_it || ra_it >= copy_it || copy_it < ra_it || copy_it <= ra_it )
    {
    std::cerr << "Error with comparison after adding offset." << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Test IndexInBounds
  //
  std::cout << "Testing IndexInBounds" << std::endl;
  int dims[4] = {13,11,9,7};
  typename ImageType::Pointer iib_img = itkConstNeighborhoodIteratorWithOnlyIndexTestGetTestImage<TImage>(dims[0], dims[1], dims[2], dims[3]);
  radius[0] = 4;
  radius[1] = 3;
  radius[2] = 2;
  radius[3] = 1;

  std::cout << "Creating ConstNeighborhoodIterator" << std::endl;
  typedef ConstNeighborhoodIteratorType IteratorType;
  IteratorType iib_it(radius, iib_img, iib_img->GetRequestedRegion());
  typename IteratorType::OffsetType resultOffset;
  typename IteratorType::OffsetType internalIndex;

  typename IteratorType::IndexType centerLoc;
  centerLoc[0] = static_cast<typename IteratorType::IndexType::IndexValueType>(dims[0] / 2);
  centerLoc[1] = static_cast<typename IteratorType::IndexType::IndexValueType>(dims[1] / 2);
  centerLoc[2] = static_cast<typename IteratorType::IndexType::IndexValueType>(dims[2] / 2);
  centerLoc[3] = static_cast<typename IteratorType::IndexType::IndexValueType>(dims[3] / 2);

  iib_it.GoToBegin();
  bool inside = iib_it.IndexInBounds( 0, internalIndex, resultOffset );
  if( inside )
    {
    std::cerr << "IndexInBounds failed for index 0, expected false." << std::endl;
    return EXIT_FAILURE;
    }
  for( unsigned int n = 0; n < 4; n++ )
    {
    if( resultOffset[n] != static_cast<itk::OffsetValueType>( radius[n] ) )
      {
      std::cerr << "IndexInBounds failed. Expected resultOffset of " << radius << ", but got " << resultOffset << std::endl;
      return EXIT_FAILURE;
      }
    }
  inside = iib_it.IndexInBounds( iib_it.Size()-1, internalIndex, resultOffset );
  if( ! inside )
    {
    std::cerr << "IndexInBounds failed for index size-1, expected true." << std::endl;
    return EXIT_FAILURE;
    }
  for( unsigned int n = 0; n < 4; n++ )
    {
    if( resultOffset[n] != 0 )
      {
      std::cerr << "IndexInBounds failed. Expected resultOffset of 0, but got " << resultOffset << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Test min boundary by dimension
  int result = EXIT_SUCCESS;
  for( unsigned int n = 0; n < 4; n++ )
    {
    typename IteratorType::IndexType boundaryLoc = centerLoc;
    boundaryLoc[n] = 0;
    iib_it.SetLocation( boundaryLoc );
    if( iib_it.IndexInBounds( 0, internalIndex, resultOffset ) )
      {
      std::cerr << "IndexInBounds failed for min boundaryLoc: " << boundaryLoc << " and dimension: " << n << ". Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }

  // Test max boundary by dimension
  for( unsigned int n = 0; n < 4; n++ )
    {
    typename IteratorType::IndexType boundaryLoc = centerLoc;
    boundaryLoc[n] = dims[n]-1;
    iib_it.SetLocation( boundaryLoc );
    if( iib_it.IndexInBounds( iib_it.Size()-1, internalIndex, resultOffset ) )
      {
      std::cerr << "IndexInBounds failed for max boundaryLoc: " << boundaryLoc << " and dimension: " << n << ". Expected false." << std::endl;
      result = EXIT_FAILURE;
      }
    }

  // Test center
  iib_it.SetLocation( centerLoc );
  inside = iib_it.IndexInBounds( 0, internalIndex, resultOffset );
  if( ! inside )
    {
    std::cerr << "IndexInBounds failed for index 0, expected true." << std::endl;
    result = EXIT_FAILURE;
    }

  return result;

}

int itkConstNeighborhoodIteratorWithOnlyIndexTest(int, char* [] )
{
  std::cout << "*** Testing with itk::Image" << std::endl << std::endl;
  if( itkConstNeighborhoodIteratorWithOnlyIndexTestRun< itk::Image<char,4> >() == EXIT_FAILURE )
    {
    std::cerr << "XXX Failed with itk::Image XXX" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << std::endl << "*** Testing with itk::ImageBase" << std::endl << std::endl;
  if ( itkConstNeighborhoodIteratorWithOnlyIndexTestRun< itk::ImageBase<4> >() == EXIT_FAILURE )
    {
    std::cerr << "XXX Failed with itk::ImageBase XXX" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
