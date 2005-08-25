/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomNonRepeatingIteratorWithIndexTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This tests the classes ImageRandomNonRepeatingIteratorWithIndex and
  ImageRandomNonRepeatingConstIteratorWithIndex.  This was contributed 
  by Rupert Brooks, McGill Centre for Intelligent
  Machines, Montreal, Canada.  It is heavily based on the
  ImageRandomIterator test program. 

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingConstIteratorWithIndex.h"

int itkImageRandomNonRepeatingIteratorWithIndexTest(int, char* [] )
{

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;

  typedef unsigned long             PriorityPixelType;

  typedef itk::Image< PriorityPixelType, ImageDimension >  PriorityImageType;

  typedef itk::ImageRegionIteratorWithIndex< ImageType >            IteratorType;

  typedef itk::ImageRegionIteratorWithIndex< PriorityImageType >            PriorityIteratorType;
  typedef itk::ImageRandomNonRepeatingIteratorWithIndex< ImageType >      RandomIteratorType;

  typedef itk::ImageRandomNonRepeatingConstIteratorWithIndex< ImageType > RandomConstIteratorType;

  std::cout << "Creating images" << std::endl;


  ImageType::Pointer myImage = ImageType::New();
  ImageType::ConstPointer myConstImage = myImage.GetPointer();
  
  ImageType::SizeType size;

  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

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

  // Make the priority image
  PriorityImageType::Pointer priorityImage = PriorityImageType::New();
  PriorityImageType::SizeType prioritySize;

  prioritySize[0] = 50;
  prioritySize[1] = 50;
  prioritySize[2] = 50;

  PriorityImageType::IndexType priorityStart;
  priorityStart.Fill(0);

  PriorityImageType::RegionType priorityRegion;
  priorityRegion.SetIndex( priorityStart );
  priorityRegion.SetSize( prioritySize );

  priorityImage->SetLargestPossibleRegion( priorityRegion );
  priorityImage->SetBufferedRegion( priorityRegion );
  priorityImage->SetRequestedRegion( priorityRegion );
  priorityImage->Allocate();

  // we will make most of this image ones, with a small region of
  // zeros.  Then pixels from the zero region should be selected
  // preferentially.
  std::cout << "Building Priority image" << std::endl;
  PriorityIteratorType pit( priorityImage, priorityRegion );
  pit.GoToBegin();
  while( !pit.IsAtEnd() )
  {
    pit.Set( 1 );
    ++pit;
  }

  PriorityImageType::IndexType substart;
  substart[0] = 15;
  substart[1] = 16;
  substart[2] = 17;
  
  PriorityImageType::SizeType subsize;
  subsize[0] = 3;
  subsize[1] = 4;
  subsize[2] = 5;
  
  PriorityImageType::RegionType subregion;
  subregion.SetIndex( substart );
  subregion.SetSize( subsize );
  
  PriorityIteratorType subit( priorityImage, subregion );
  subit.GoToBegin();
  while( !subit.IsAtEnd() )
    {
      subit.Set( 0 );
      ++subit;
    }
  


  //********
  std::cout << "Filling image with indices" << std::endl;


  RandomIteratorType it( myImage, region );
  it.SetNumberOfSamples(size[0]*size[1]*size[2]);
  it.GoToBegin();

  ImageType::IndexType index;
  // Because the random iterator does not repeat, this should
  // fill the image with indices
  while( !it.IsAtEnd() )
  {
    index = it.GetIndex();
    it.Set( index );
    ++it;
  }

  
  // Sample the image 
  IteratorType ot( myImage, region );
  ot.GoToBegin();

  // if it repeated its going to have missed a few.
  std::cout << "Verifying iterators... ";
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
    //std::cout <<".";
    //std::cout << index << std::endl;
    ++ot;
    }
  std::cout << std::endl<<"   Done ! " << std::endl;

  
  // Verification 
  RandomConstIteratorType cot( myConstImage, region );
  cot.SetNumberOfSamples( numberOfSamples ); 
  cot.GoToBegin();

 
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
  std::cout << "Verifying iterator in reverse direction... " << std::endl;
  std::cout << "Should be a random walk too (a different one)" << std::endl;

  RandomIteratorType ior( myImage, region );
  ior.SetNumberOfSamples( numberOfSamples ); 
  ior.GoToEnd();

  --ior;
 

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
  std::cout << index << std::endl; // print the value at the beginning index
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying const iterator in reverse direction... ";

  RandomConstIteratorType cor( myImage, region );
  cor.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
  cor.GoToEnd();

  --cor; // start at the end position 

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
  std::cout << index << std::endl; // print the value at the beginning index
  std::cout << "   Done ! " << std::endl;

 // Verification 
  std::cout << "Verifying const iterator in both directions... ";

  RandomConstIteratorType dor( myImage, region );
  dor.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
  dor.GoToEnd();

  --dor; // start at the last valid pixel position 

  for (unsigned int counter = 0; ! dor.IsAtEnd(); ++counter)
    {
      index = dor.GetIndex();
      if( dor.Get() != index )
        {
          std::cerr << "Values don't correspond to what was stored "
                    << std::endl;
          std::cerr << "Test failed at index ";
          std::cerr << index << " value is " << dor.Get() <<  std::endl;
          return EXIT_FAILURE;
        }
      std::cout << index << std::endl;
      if (counter < 6)  { --dor; }
      else { ++dor; }
    }
  std::cout << index << std::endl; // print the value at the beginning index
  std::cout << "   Done ! " << std::endl;
  

  // Verification of the Iterator in a subregion of the image
  {
    std::cout << "Verifying Iterator in a Region smaller than the whole image... "
              << std::endl;

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
    std::cout << "Verifying Const Iterator in a Region smaller than the whole image... "
              << std::endl;

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


 

  // Verifying iterator works with  the priority image
  

  {
    std::cout << "Verifying Iterator with respect to priority image... "
              << std::endl;



    RandomIteratorType cbot( myImage, region );
    cbot.SetPriority(priorityImage);

    cbot.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count=0;
    while( !cbot.IsAtEnd() && count<(subsize[0]*subsize[1]*subsize[2]))
      {
      ImageType::IndexType index =  cbot.GetIndex();

      if( !subregion.IsInside( index ) )
        {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region << std::endl;
        return EXIT_FAILURE;
        }
      std::cout << index << std::endl;
      ++cbot;
      ++count;
      }
    // Now we have walked through all the pixels of low priority, next
    // one should be outside the region.
      if( subregion.IsInside( index ) )
        {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region (should be in)" << region << std::endl;
        return EXIT_FAILURE;
        }

    std::cout << "   Done ! " << std::endl;
  }

  {
    std::cout << "Verifying const Iterator with respect to priority image... "
              << std::endl;



    RandomConstIteratorType cbot( myImage, region );
    cbot.SetPriority(priorityImage);

    cbot.SetNumberOfSamples( numberOfSamples ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();
    unsigned int count=0;
    while( !cbot.IsAtEnd() && count<(subsize[0]*subsize[1]*subsize[2]))
      {
      ImageType::IndexType index =  cbot.GetIndex();

      if( !subregion.IsInside( index ) )
        {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region " << region << std::endl;
        return EXIT_FAILURE;
        }
      std::cout << index << std::endl;
      ++cbot;
      ++count;
      }
    // Now we have walked through all the pixels of low priority, next
    // one should be outside the region.
      if( subregion.IsInside( index ) )
        {
        std::cerr << "Iterator in priority region test failed" << std::endl;
        std::cerr << index << " is outside the region (should be in)" << region << std::endl;
        return EXIT_FAILURE;
        }

    std::cout << "   Done ! " << std::endl;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
