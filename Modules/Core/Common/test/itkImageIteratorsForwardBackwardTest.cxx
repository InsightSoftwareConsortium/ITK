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

int itkImageIteratorsForwardBackwardTest(int, char* [] )
{

  std::cout << "Creating an image" << std::endl;
  typedef itk::Image<unsigned short,3> ImageType;

  ImageType::Pointer myImage = ImageType::New();

  ImageType::SizeType size;

  size[0] = 4;
  size[1] = 4;
  size[2] = 4;

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;

  typedef itk::ImageRegionConstIteratorWithIndex< ImageType > ConstIteratorType;

  IteratorType it( myImage, region );

  ImageType::PixelType value;

  value = itk::NumericTraits< ImageType::PixelType >::ZeroValue();

  // Store information on the Image
  std::cout << "Storing data on the image ... " << std::endl;

  while( !it.IsAtEnd() )
  {
    value++;
    it.Set( value );
    ++it;
  }


  // Verification
  IteratorType ot( myImage, region );
  std::cout << "Verifying the data forwards... ";

  value = itk::NumericTraits< ImageType::PixelType >::ZeroValue();

  while( !ot.IsAtEnd() )
  {
    value++;

    if( ot.Get() != value )
    {
      std::cerr << "Error in forward pass" << std::endl;
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << ot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    ++ot;
  }

  std::cout << "      PASSED !" << std::endl;

  // Verification
  std::cout << "Verifying the data backwards... ";

  ot.GoToReverseBegin();
  --ot;
  --value;
  while( !ot.IsAtReverseEnd() )
  {

    if( ot.Get() != value )
    {
      std::cerr << "Error in backwards pass" << std::endl;
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << ot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << ot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    value--;
    --ot;
  }

  std::cout << "      PASSED !" << std::endl;

  // Verification
  ConstIteratorType cot( myImage, region );
  std::cout << "Const Iterator: Verifying the data forwards... ";

  value = itk::NumericTraits< ImageType::PixelType >::ZeroValue();

  while( !cot.IsAtEnd() )
  {
    value++;

    if( cot.Get() != value )
    {
      std::cerr << "Error in forward pass" << std::endl;
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << cot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << cot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    ++cot;
  }

  std::cout << "      PASSED !" << std::endl;

  // Verification
  std::cout << "Const Iterator : Verifying the data backwards... ";

  cot.GoToReverseBegin();
  --cot;
  --value;
  while( !cot.IsAtReverseEnd() )
  {

    if( cot.Get() != value )
    {
      std::cerr << "Error in backwards pass" << std::endl;
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << cot.GetIndex() << std::endl;
      std::cerr << "Value stored is = " << cot.Get() << std::endl;
      std::cerr << "Value should be = " << value    << std::endl;
      return EXIT_FAILURE;
    }
    value--;
    --cot;
  }

  std::cout << "      PASSED !" << std::endl;

  std::cout << std::endl << "Test passed" << std::endl;

  return EXIT_SUCCESS;

}
