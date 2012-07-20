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
#include "itkImageRandomConstIteratorWithOnlyIndex.h"

int itkImageRandomConstIteratorWithOnlyIndexTest(int, char* [] )
{
  std::cout << "Creating an image of indices" << std::endl;

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;

  ImageType::Pointer myImage = ImageType::New();
  ImageType::ConstPointer myConstImage = myImage.GetPointer();

  ImageType::SizeType size0;

  size0[0] = 100;
  size0[1] = 100;
  size0[2] = 100;

  unsigned long numberOfSamples = 10;

  ImageType::IndexType start0;
  start0.Fill(0);

  ImageType::RegionType region0;
  region0.SetIndex( start0 );
  region0.SetSize( size0 );

  myImage->SetLargestPossibleRegion( region0 );
  myImage->SetBufferedRegion( region0 );
  myImage->SetRequestedRegion( region0 );
  myImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ImageType >              IteratorType;
  typedef itk::ImageRandomConstIteratorWithOnlyIndex< ImageType >     RandomConstIteratorType;

  IteratorType it( myImage, region0 );

  it.GoToBegin();
  ImageType::IndexType index0;

  // Fill an image with indices
  while( !it.IsAtEnd() )
    {
    index0 = it.GetIndex();
    it.Set( index0 );
    ++it;
    }

  // Sample the image
  RandomConstIteratorType cot( myConstImage, region0 );
  cot.SetNumberOfSamples( numberOfSamples );
  cot.GoToBegin();

  std::cout << "Verifying const iterator... ";
  std::cout << "Random walk of the Iterator over the image " << std::endl;

  while( !cot.IsAtEnd() )
    {
    index0 = cot.GetIndex();
    it.SetIndex( index0 );
    if( it.Get() != index0 )
      {
      std::cerr << "Values don't correspond to what was stored " << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << index0 << " value is " << it.Get() <<  std::endl;
      return EXIT_FAILURE;
      }
    std::cout << index0 << std::endl;
    ++cot;
    }
  std::cout << "   Done ! " << std::endl;

  // Verification of reverse iteration
  std::cout << "Verifying const iterator in reverse direction... ";

  RandomConstIteratorType cor( myImage, region0 );
  cor.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
  cor.GoToEnd();

  --cor; // start at the end position

  while( !cor.IsAtBegin() )
    {
    index0 = cor.GetIndex();
    it.SetIndex( index0 );
    if( it.Get() != index0 )
      {
      std::cerr << "Values don't correspond to what was stored " << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << index0 << " value is " << it.Get() <<  std::endl;
      return EXIT_FAILURE;
      }
    std::cout << index0 << std::endl;
    --cor;
    }
  std::cout << index0 << std::endl; // print the value at the beginning index
  std::cout << "   Done ! " << std::endl;

  // Verification with ImageBase type and both directions of iteration
  std::cout << "Verifying const iterator with ImageBase type and in both directions... ";

  typedef itk::ImageBase< ImageDimension > ImageBaseType;
  typedef itk::ImageRandomConstIteratorWithOnlyIndex< ImageBaseType >     ImageBaseRandomConstIteratorType;

  ImageBaseType::Pointer myImageBase = ImageBaseType::New();
  myImageBase->CopyInformation( myImage );
  ImageBaseRandomConstIteratorType dor( myImageBase, region0 );
  dor.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
  dor.GoToEnd();

  --dor; // start at the last valid pixel position

  for (unsigned int counter = 0; ! dor.IsAtEnd(); ++counter)
    {
      index0 = dor.GetIndex();
      it.SetIndex( index0 );
      if( it.Get() != index0 )
        {
          std::cerr << "Values don't correspond to what was stored " << std::endl;
          std::cerr << "Test failed at index " << index0 << " value is " << it.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
      std::cout << index0 << std::endl;
      if (counter < 6)
        {
        --dor;
        }
      else
        {
        ++dor;
        }
    }
  std::cout << index0 << std::endl; // print the value at the beginning index
  std::cout << "   Done ! " << std::endl;

  // Verification of the Const Iterator in a subregion of the image
  {
    std::cout << "Verifying Const Iterator in a Region smaller than the whole image... " << std::endl;

    ImageType::IndexType start;
    start[0] = 10;
    start[1] = 12;
    start[2] = 14;

    ImageType::SizeType size;
    size[0] = 11;
    size[1] = 12;
    size[2] = 13;

    ImageType::RegionType region;
    region.SetIndex( start );
    region.SetSize( size );

    RandomConstIteratorType cbot( myImage, region );

    cbot.SetNumberOfSamples( numberOfSamples );
    cbot.GoToBegin();

    while( !cbot.IsAtEnd() )
      {
      ImageType::IndexType index =  cbot.GetIndex();
      it.SetIndex( index );
      ImageType::PixelType pixel =  it.Get();

      if( index != pixel )
        {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << pixel << " should be" << index << std::endl;
        return EXIT_FAILURE;
        }
      if( !region.IsInside( index ) )
        {
        std::cerr << "Iterator in region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region << std::endl;
        return EXIT_FAILURE;
        }
      std::cout << index << std::endl;

      ++cbot;
      }

    std::cout << "   Done ! " << std::endl;
  }

  std::cout << "Test passed" << std::endl;

  return EXIT_SUCCESS;
}
