/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageLinearIteratorTest.cxx
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
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"




int itkImageLinearIteratorTest(int, char* [] )
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

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  typedef itk::ImageLinearIteratorWithIndex< ImageType > IteratorType;

  typedef itk::ImageLinearConstIteratorWithIndex< ImageType > ConstIteratorType;

  IteratorType it( myImage, region );


  it.GoToBegin();
  it.SetDirection( 0 ); // 0=x, 1=y, 2=z

  ImageType::IndexType index;
  
  while( !it.IsAtEnd() )
  {
    while( !it.IsAtEndOfLine() )
    {
      index = it.GetIndex();
      it.Set( index );
      ++it;
    }
    it.NextLine();
  }

  
  // Verification 
  IteratorType ot( myImage, region );

  ot.GoToBegin();
  ot.SetDirection( 0 ); // 0=x, 1=y, 2=z
 
  std::cout << "Verifying iterator... ";

  while( !ot.IsAtEnd() )
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
        return EXIT_FAILURE;
      }
      ++ot;
    }
    ot.NextLine();
  }
  std::cout << "   Done ! " << std::endl;

  
  // Verification 
  ConstIteratorType cot( myConstImage, region );

  cot.GoToBegin();
  cot.SetDirection( 0 ); // 0=x, 1=y, 2=z
 
  std::cout << "Verifying const iterator... ";

  while( !cot.IsAtEnd() )
  {
    while( !cot.IsAtEndOfLine() )
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
      ++cot;
    }
    cot.NextLine();
  }
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying iterator in reverse direction... ";

  IteratorType ior( myImage, region );

  ior.GoToReverseBegin();
  ior.SetDirection( 0 ); // 0=x, 1=y, 2=z
 

  while( !ior.IsAtReverseEnd() )
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
  std::cout << "   Done ! " << std::endl;



  // Verification 
  std::cout << "Verifying const iterator in reverse direction... ";

  ConstIteratorType cor( myImage, region );

  cor.GoToReverseBegin();
  cor.SetDirection( 0 ); // 0=x, 1=y, 2=z
 

  while( !cor.IsAtReverseEnd() )
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
  std::cout << "   Done ! " << std::endl;



  // Verification of GotoBeginOfLine() in the iterator
  {
  std::cout << "Verifying  iterator GoToBeginOfLine()... ";

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

      IteratorType bot( myImage, region );

      bot.SetDirection( 0 ); // 0=x, 1=y, 2=z
      bot.GoToBegin();

      ImageType::IndexType testIndex;
      testIndex = start;
      testIndex[1] += 2; // advance two lines in Y

      bot.NextLine(); // advance two lines in Y
      bot.NextLine();

      ++bot; // advance a bit along the line
      ++bot;
      ++bot;
      ++bot;

      bot.GoToBeginOfLine(); // go back to the begin of the line

      if( bot.GetIndex() != testIndex )
        {
        std::cerr << "GoToBeginOfLine() test failed" << std::endl;
        std::cerr << bot.GetIndex() << " should be" << testIndex << std::endl;
        return EXIT_FAILURE;
        }

      std::cout << "   Done ! " << std::endl;
    }





  // Verification of GotoBeginOfLine() in the const iterator
  {
  std::cout << "Verifying const iterator GoToBeginOfLine()... ";

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

    ConstIteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    ImageType::IndexType testIndex;
    testIndex = start;
    testIndex[1] += 2; // advance two lines in Y

    cbot.NextLine(); // advance two lines in Y
    cbot.NextLine();

    ++cbot; // advance a bit along the line
    ++cbot;
    ++cbot;
    ++cbot;

    cbot.GoToBeginOfLine(); // go back to the begin of the line

    if( cbot.GetIndex() != testIndex )
      {
      std::cerr << "GoToBeginOfLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "   Done ! " << std::endl;
  }




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

    IteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    while( !cbot.IsAtEnd() )
      {
      while( !cbot.IsAtEndOfLine() )
        {
        ImageType::IndexType index =  cbot.GetIndex();
        ImageType::PixelType pixel =  cbot.Get();

        if( index != pixel )
          {
          std::cerr << "Iterator in region test failed" << std::endl;
          std::cerr << pixel << " should be" << index << std::endl;
          return EXIT_FAILURE;
          }

        ++cbot;
        }
      cbot.NextLine(); 
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

    ConstIteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    while( !cbot.IsAtEnd() )
      {
      while( !cbot.IsAtEndOfLine() )
        {
        ImageType::IndexType index =  cbot.GetIndex();
        ImageType::PixelType pixel =  cbot.Get();

        if( index != pixel )
          {
          std::cerr << "Iterator in region test failed" << std::endl;
          std::cerr << pixel << " should be" << index << std::endl;
          return EXIT_FAILURE;
          }

        ++cbot;
        }
      cbot.NextLine(); 
      }

    std::cout << "   Done ! " << std::endl;
  }



 // Verification of the Iterator NextLine() in the middle of a line
  {
  std::cout << "Verifying Iterator NextLine() in the middle of a line";

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

    IteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the first line
    for(unsigned int i=0; i<size[0]/2; i++)
      {
      ++cbot;
      }

    // go to next line
    cbot.NextLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if( cbot.GetIndex() != testIndex )
      {
      std::cerr << "NextLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "   Done ! " << std::endl;

  }


 // Verification of the Iterator PreviousLine() in the middle of a line
  {
  std::cout << "Verifying Iterator PreviousLine() in the middle of a line";

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

    IteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the second line
    for(unsigned int i=0; i<size[0]+size[0]/2; i++)
      {
      ++cbot;
      }

    // go to previous line
    cbot.PreviousLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if( cbot.GetIndex() != testIndex )
      {
      std::cerr << "PreviousLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "   Done ! " << std::endl;

  }




 // Verification of the ConstIterator NextLine() in the middle of a line
  {
  std::cout << "Verifying ConstIterator NextLine() in the middle of a line";

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

    ConstIteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the first line
    for(unsigned int i=0; i<size[0]/2; i++)
      {
      ++cbot;
      }

    // go to next line
    cbot.NextLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if( cbot.GetIndex() != testIndex )
      {
      std::cerr << "NextLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "   Done ! " << std::endl;

  }


 // Verification of the ConstIterator PreviousLine() in the middle of a line
  {
  std::cout << "Verifying ConstIterator PreviousLine() in the middle of a line";

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

    ConstIteratorType cbot( myImage, region );

    cbot.SetDirection( 0 ); // 0=x, 1=y, 2=z
    cbot.GoToBegin();

    // go to the middle of the second line
    for(unsigned int i=0; i<size[0]+size[0]/2; i++)
      {
      ++cbot;
      }

    // go to previous line
    cbot.PreviousLine();

    const ImageType::IndexType testIndex = cbot.Get();
    if( cbot.GetIndex() != testIndex )
      {
      std::cerr << "PreviousLine() test failed" << std::endl;
      std::cerr << cbot.GetIndex() << " should be" << testIndex << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "   Done ! " << std::endl;

  }



  std::cout << "Test passed" << std::endl;




    return EXIT_SUCCESS;

  }



