/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkImageRandomConstIteratorWithIndex.h"




int itkImageRandomIteratorTest(int, char* [] )
{
  std::cout << "Creating an image of indices" << std::endl;

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;

  ImageType::Pointer myImage = ImageType::New();
  ImageType::ConstPointer myConstImage = myImage.GetPointer();
  
  ImageType::SizeType size;

  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

  unsigned long numberOfSamples = 10;

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ImageType >            IteratorType;

  typedef itk::ImageRandomIteratorWithIndex< ImageType >      RandomIteratorType;

  typedef itk::ImageRandomConstIteratorWithIndex< ImageType > RandomConstIteratorType;

  IteratorType it( myImage, region );


  it.GoToBegin();
  ImageType::IndexType index;
  
  // Fill an image with indices
  while( !it.IsAtEnd() )
  {
    index = it.GetIndex();
    it.Set( index );
    ++it;
  }

  
  // Sample the image 
  RandomIteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetNumberOfSamples( numberOfSamples ); 
 
  std::cout << "Verifying non-const iterator... ";
  std::cout << "Random walk of the Iterator over the image " << std::endl;
  while( !ot.IsAtEnd() )
    {
    index = ot.GetIndex();
    if( ot.Get() != index )
      {
        std::cerr << "Values don't correspond to what was stored "
          << std::endl;
        std::cerr << "Test failed at index ";
        std::cerr << index << std::endl;
        return EXIT_FAILURE;
      }
    std::cout << index << std::endl;
    ++ot;
    }
  std::cout << "   Done ! " << std::endl;

  
  // Verification 
  RandomConstIteratorType cot( myConstImage, region );

  cot.GoToBegin();
  cot.SetNumberOfSamples( numberOfSamples ); 
 
  std::cout << "Verifying const iterator... ";
  std::cout << "Random walk of the Iterator over the image " << std::endl;

  while( !cot.IsAtEnd() )
  {
    index = cot.GetIndex();
    if( cot.Get() != index )
      {
      std::cerr << "Values don't correspond to what was stored "
        << std::endl;
      std::cerr << "Test failed at index ";
      std::cerr << index << " value is " << cot.Get() <<  std::endl;
      return EXIT_FAILURE;
      }
    std::cout << index << std::endl;
    ++cot;
  }
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying iterator in reverse direction... ";
  std::cout << "Should be a random walk too (a different one)" << std::endl;

  RandomIteratorType ior( myImage, region );

  ior.GoToEnd();
  ior.SetNumberOfSamples( numberOfSamples ); 
 

  while( !ior.IsAtBegin() )
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
    std::cout << index << std::endl;
    --ior;
  }
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying const iterator in reverse direction... ";

  RandomConstIteratorType cor( myImage, region );

  cor.GoToEnd();
  cor.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
 

  while( !cor.IsAtBegin() )
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
    std::cout << index << std::endl;
    --cor;
    }
  std::cout << "   Done ! " << std::endl;



  // Verification of the Iterator in a subregion of the image
  {
  std::cout << "Verifying Iterator in a Region smaller than the whole image... ";

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

    RandomIteratorType cbot( myImage, region );

    cbot.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    while( !cbot.IsAtEnd() )
      {
      ImageType::IndexType index =  cbot.GetIndex();
      ImageType::PixelType pixel =  cbot.Get();

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



  // Verification of the Const Iterator in a subregion of the image
  {
  std::cout << "Verifying Const Iterator in a Region smaller than the whole image... ";

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
      ImageType::PixelType pixel =  cbot.Get();

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



