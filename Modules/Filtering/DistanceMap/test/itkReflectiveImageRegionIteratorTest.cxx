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
#include "itkReflectiveImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkReflectiveImageRegionIteratorTest(int, char* [] )
{
  std::cout << "Creating an image" << std::endl;
  const unsigned int Dimension = 4;
  typedef itk::Index<Dimension>             PixelType;
  typedef itk::Image<PixelType,Dimension>   ImageType;
  typedef itk::Image<int,Dimension>         ImageVisitsType;

  typedef itk::ImageRegionIteratorWithIndex<ImageType>       IteratorType;
  typedef itk::ImageRegionIteratorWithIndex<ImageVisitsType> IteratorVisitsType;

  ImageType::Pointer myImage = ImageType::New();

  ImageType::SizeType size = {{4,4,4,4}};

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  ImageVisitsType::Pointer visitImage = ImageVisitsType::New();

  visitImage->SetLargestPossibleRegion( region );
  visitImage->SetRequestedRegion( region );
  visitImage->SetBufferedRegion( region );
  visitImage->Allocate();

  IteratorType        nit( myImage,    region );
  IteratorVisitsType  vit( visitImage, region );

  // Store information on the Image
  std::cout << "Storing data in the image ... " << std::endl;
  nit.GoToBegin();
  vit.GoToBegin();
  while( !nit.IsAtEnd() )
    {
    // set the pixel index as value
    nit.Set( nit.GetIndex() );
    // Set the number of visits to zero
    vit.Set( itk::NumericTraits< ImageVisitsType::PixelType >::ZeroValue() );
    ++nit;
    ++vit;
    }


  typedef itk::ReflectiveImageRegionConstIterator< ImageType >
                                                  ReflectiveIteratorType;
  ReflectiveIteratorType rit( myImage, region );

  typedef itk::ReflectiveImageRegionIterator< ImageVisitsType >
                                                  ReflectiveVisitsIteratorType;

  ReflectiveVisitsIteratorType rvt( visitImage, region );

  // Verification
  std::cout << "Verifying the reflective iterator... " << std::endl;

  rit.GoToBegin();
  rvt.GoToBegin();
  while( !rit.IsAtEnd() )
    {
    PixelType value = rit.Get();
    ImageType::IndexType index = rit.GetIndex();
    rvt.Set( rvt.Get() + 1 );
    if( value != index )
      {
      std::cerr << "Error :  at Index " << index << std::endl;
      std::cerr << "It is pointing to " << value << std::endl;
      }
    ++rit;
    ++rvt;
    }


  // Each element should be visited 2 ^ # of dimensions
  // each left shift = multiply by 2
  int visits = ( 1 << (ImageType::ImageDimension));
  int failed = 0;

  // Verify the number of visits
  vit.GoToBegin();
  while( !vit.IsAtEnd() )
    {
    if (vit.Get() != visits)
      {
      std::cout << vit.GetIndex() << " should not = " << vit.Get() << std::endl;
      failed++;
      }
    ++vit;
    }

  std::cout << std::endl;

  // Test the value at a pixel using both the iterator and the image given the index pointed
  // to by the iterator.  These should obviously always be the same.
  // But this test exposes a bug in the code that has been fixed.
  ImageType::OffsetType voffset;
  for ( unsigned int dim = 0; dim < Dimension; dim++ )
    {
    if ( region.GetSize()[dim] > 1 )
      {
      voffset[dim] = 1;
      }
    else
      {
      voffset[dim] = 0;
      }
    }
  rit.SetBeginOffset(voffset);
  rit.SetEndOffset(voffset);

  for( rit.GoToBegin(); !rit.IsAtEnd(); ++rit )
  {
    if( rit.Get() != myImage->GetPixel( rit.GetIndex() ) )
    {
      std::cerr << "Error: pixel value returned by iterator is "
          << rit.Get()
          << ", but pixel value defined by image at the same index is "
          << myImage->GetPixel( rit.GetIndex() )
          << std::endl;
      failed = 1;
    }
  }

  if ( failed )
    {
    std::cout << "      FAILED !" << std::endl << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "      PASSED !" << std::endl << std::endl;
  return EXIT_SUCCESS;

}
