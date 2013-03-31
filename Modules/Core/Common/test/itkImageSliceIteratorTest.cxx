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

#include "itkImageSliceIteratorWithIndex.h"

int itkImageSliceIteratorTest(int, char* [] )
{
  std::cout << "Creating an image of indices" << std::endl;

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;


  ImageType::Pointer myImage = ImageType::New();

  ImageType::SizeType size;

  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageSliceIteratorWithIndex< ImageType > IteratorType;

  typedef itk::ImageSliceConstIteratorWithIndex< ImageType > ConstIteratorType;

  IteratorType it( myImage, region );

  it.GoToBegin();
  it.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  it.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  ImageType::IndexType index;

  while( !it.IsAtEnd() )
  {
    while( !it.IsAtEndOfSlice() )
    {
      while( !it.IsAtEndOfLine() )
      {
        index = it.GetIndex();
        it.Set( index );
        ++it;
      }
      it.NextLine();
    }
    it.NextSlice();
  }

  {
  // Verification
  std::cout << "Verifying for iterator...";
  IteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  ot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !ot.IsAtEnd() )
  {
    while( !ot.IsAtEndOfSlice() )
    {
      while( !ot.IsAtEndOfLine() )
      {
        index = ot.GetIndex();
        if( ot.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          std::cerr << "It should be ";
          std::cerr << ot.Get() << std::endl;
          return EXIT_FAILURE;
        }
        ++ot;
      }
      ot.NextLine();
    }
    ot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;


  // Verification
  std::cout << "Verifying for const iterator...";
  ConstIteratorType cot( myImage, region );

  cot.GoToBegin();
  cot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  cot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !cot.IsAtEnd() )
  {
    while( !cot.IsAtEndOfSlice() )
    {
      while( !cot.IsAtEndOfLine() )
      {
        index = cot.GetIndex();
        if( cot.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          std::cerr << "It should be ";
          std::cerr << cot.Get() << std::endl;
          return EXIT_FAILURE;
        }
        ++cot;
      }
      cot.NextLine();
    }
    cot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;

  // Verification
  std::cout << "Verifying iterator in reverse direction... ";

  IteratorType ior( myImage, region );

  ior.GoToReverseBegin();
  ior.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  ior.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !ior.IsAtReverseEnd() )
  {
    while( !ior.IsAtReverseEndOfSlice() )
    {
      while( !ior.IsAtReverseEndOfLine() )
      {
        index = ior.GetIndex();
        if( ior.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << " value is " << ior.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
        --ior;
      }
      ior.PreviousLine();
    }
    ior.PreviousSlice();
  }

  std::cout << "   Done ! " << std::endl;

  // Verification
  std::cout << "Verifying const iterator in reverse direction... ";

  ConstIteratorType cor( myImage, region );

  cor.GoToReverseBegin();
  cor.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  cor.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !cor.IsAtReverseEnd() )
  {
    while( !cor.IsAtReverseEndOfSlice() )
    {
      while( !cor.IsAtReverseEndOfLine() )
      {
        index = cor.GetIndex();
        if( cor.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << " value is " << cor.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
        --cor;
      }
      cor.PreviousLine();
    }
    cor.PreviousSlice();
  }
  std::cout << "   Done ! " << std::endl;

  }

  {
  // Verification
  std::cout << "Test in a region < LargestPossibleRegion" << std::endl;
  std::cout << "Verifying for iterator...";

  size[0]  = 50;
  size[1]  = 50;
  size[2]  = 50;

  start[0] = 25;
  start[1] = 25;
  start[2] = 25;

  region.SetIndex( start );
  region.SetSize( size );

  IteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  ot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !ot.IsAtEnd() )
  {
    while( !ot.IsAtEndOfSlice() )
    {
      while( !ot.IsAtEndOfLine() )
      {
        index = ot.GetIndex();
        if( ot.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          std::cerr << "It should be ";
          std::cerr << ot.Get() << std::endl;
          return EXIT_FAILURE;
        }
        ++ot;
      }
      ot.NextLine();
    }
    ot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;


  // Verification
  std::cout << "Verifying for const iterator...";
  ConstIteratorType cot( myImage, region );

  cot.GoToBegin();
  cot.SetFirstDirection( 0 ); // 0=x, 1=y, 2=z
  cot.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !cot.IsAtEnd() )
  {
    while( !cot.IsAtEndOfSlice() )
    {
      while( !cot.IsAtEndOfLine() )
      {
        index = cot.GetIndex();
        if( cot.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << std::endl;
          std::cerr << "It should be ";
          std::cerr << cot.Get() << std::endl;
          return EXIT_FAILURE;
        }
        ++cot;
      }
      cot.NextLine();
    }
    cot.NextSlice();
  }
  std::cout << "  Done !" << std::endl;

  // Verification
  std::cout << "Verifying iterator in reverse direction... ";

  IteratorType ior( myImage, region );

  ior.GoToReverseBegin();
  ior.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  ior.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !ior.IsAtReverseEnd() )
  {
    while( !ior.IsAtReverseEndOfSlice() )
    {
      while( !ior.IsAtReverseEndOfLine() )
      {
        index = ior.GetIndex();
        if( ior.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << " value is " << ior.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
        --ior;
      }
      ior.PreviousLine();
    }
    ior.PreviousSlice();
  }

  std::cout << "   Done ! " << std::endl;

  // Verification
  std::cout << "Verifying const iterator in reverse direction... ";

  ConstIteratorType cor( myImage, region );

  cor.GoToReverseBegin();
  cor.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  cor.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  while( !cor.IsAtReverseEnd() )
  {
    while( !cor.IsAtReverseEndOfSlice() )
    {
      while( !cor.IsAtReverseEndOfLine() )
      {
        index = cor.GetIndex();
        if( cor.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
            << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << " value is " << cor.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
        --cor;
      }
      cor.PreviousLine();
    }
    cor.PreviousSlice();
  }
  std::cout << "   Done ! " << std::endl;

  }

  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;

}
