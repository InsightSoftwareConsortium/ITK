/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageTest.cxx
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
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkExtractImageFilter.h"
#include "itkFileOutputWindow.h"
#include "itkStreamingImageFilter.h"

int itkExtractImageTest(int, char* [] )
{
  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance(fow);

  int nextVal;

  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   SimpleImage;
  SimpleImage::Pointer simpleImage = SimpleImage::New();
  std::cout << "Simple image spacing: " << simpleImage->GetSpacing()[0] << ", "
            << simpleImage->GetSpacing()[1] << std::endl;
  
  // typedefs to simplify the syntax
  typedef itk::Image<short, 2>   ShortImage;
  typedef itk::Image<short, 1>   LineImage;
  
  // Test the creation of an image with native type
  ShortImage::Pointer if2 = ShortImage::New();
  
  // fill in an image
  ShortImage::IndexType  index = {{0, 0}};
  ShortImage::SizeType   size = {{8, 12}};
  ShortImage::RegionType region;
  int row, column;
  region.SetSize( size );
  region.SetIndex( index );
  if2->SetLargestPossibleRegion( region );
  if2->SetBufferedRegion( region );
  if2->Allocate();
  
  itk::ImageRegionIterator<ShortImage> iterator(if2, region);
  
  short i=0;
  for (; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  std::cout << "Input Image: " << if2 << std::endl;

  // Create a filter
  itk::ExtractImageFilter< ShortImage, ShortImage >::Pointer extract;
  extract = itk::ExtractImageFilter< ShortImage, ShortImage >::New();
  extract->SetInput( if2 );
  
  // fill in an image
  ShortImage::IndexType  extractIndex = {{0, 0}};
  ShortImage::SizeType   extractSize = {{8, 12}};
  ShortImage::RegionType extractRegion;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);
  extract->UpdateLargestPossibleRegion();

  std::cout << extract << std::endl;
  std::cout << "Input spacing: " << if2->GetSpacing()[0] << ", "
            << if2->GetSpacing()[1] << std::endl;
  std::cout << "Output spacing: " << extract->GetOutput()->GetSpacing()[0]
            << ", "
            << extract->GetOutput()->GetSpacing()[1] << std::endl;
  
  
  ShortImage::RegionType requestedRegion;
  bool passed;
  
  // CASE 1
  extractIndex[0] = 1; extractIndex[1] = 2; 
  extractSize[0] = 5; extractSize[1] = 6;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);
  extract->UpdateLargestPossibleRegion();
  requestedRegion = extract->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<ShortImage>
    iteratorIn1(extract->GetOutput(), requestedRegion);
  
  nextVal = 0;
  passed = true; 
  size = requestedRegion.GetSize();
  index = requestedRegion.GetIndex();

  if ((index[0] != extractIndex[0])
      || (index[1] != extractIndex[1])
      || (size[0] != extractSize[0]) 
      || (size[1] != extractSize[1])) 
    {
      passed = false;
    } else {
    
      for (; !iteratorIn1.IsAtEnd(); ++iteratorIn1)
        {
          row = iteratorIn1.GetIndex()[0];
          column = iteratorIn1.GetIndex()[1];
          if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
            if ( iteratorIn1.Get() != 13 )
              {
                passed = false;
              }
          } else {
            nextVal = 8*column+row;
            if (iteratorIn1.Get() != nextVal)
              {
                std::cout << "Error: (" << row << ", " << column 
                          << "), expected " << nextVal << " got " 
                          << iteratorIn1.Get() << std::endl;
                passed = false;
              }
          }
        }
    }
  
  if (passed)
    {
      std::cout << "ExtractImageFilter case 1 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 1 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  
  // CASE 2
  extractIndex[0] = 1; extractIndex[1] = 1; 
  extractSize[0] = 7; extractSize[1] = 11;
  extractRegion.SetSize( extractSize );
  extractRegion.SetIndex( extractIndex );
  extract->SetExtractionRegion(extractRegion);

  // Create a stream
  itk::StreamingImageFilter< ShortImage, ShortImage >::Pointer stream;
  stream = itk::StreamingImageFilter< ShortImage, ShortImage >::New();
  stream->SetInput( extract->GetOutput() );
  stream->SetNumberOfStreamDivisions(2);
  
  ShortImage::RegionType setRegion = extract->GetExtractionRegion();
  size = setRegion.GetSize();
  index = setRegion.GetIndex();

  if ((index[0] != extractIndex[0])
      || (index[1] != extractIndex[1])
      || (size[0] != extractSize[0]) 
      || (size[1] != extractSize[1]))
    {
      passed = false;
    } 
  else 
    {
      stream->UpdateLargestPossibleRegion();
      requestedRegion = stream->GetOutput()->GetRequestedRegion();
      
      itk::ImageRegionIterator<ShortImage>
  iteratorIn2(stream->GetOutput(), requestedRegion);
      
      nextVal = 0;
      passed = true; 
      size = requestedRegion.GetSize();
      index = requestedRegion.GetIndex();
      if ((index[0] != extractIndex[0])
          || (index[1] != extractIndex[1])
          || (size[0] != extractSize[0]) 
          || (size[1] != extractSize[1]))
        {
          passed = false;
        } else {
          for (; !iteratorIn2.IsAtEnd(); ++iteratorIn2)
            {
              row = iteratorIn2.GetIndex()[0];
              column = iteratorIn2.GetIndex()[1];
              if ((row < 0) || (row>7) || (column < 0) || (column > 11)) {
                if ( iteratorIn2.Get() != 13 )
                  {
                    passed = false;
                  }
              } else {
                nextVal = 8*column+row;
                if (iteratorIn2.Get() != nextVal)
                  { 
                    std::cout << "Error: (" << row << ", " << column 
                              << "), expected " << nextVal << " got " 
                              << iteratorIn2.Get() << std::endl;
                    passed = false;
                  }
              }
            }
        }
    }


  // need to put in code to check whether the proper region was extracted.
  //
  
  if (passed)
    {
      std::cout << "ExtractImageFilter case 2 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 2 failed." << std::endl;
      return EXIT_FAILURE;
    }

  //Case 3: Try extracting a single row
  itk::ExtractImageFilter<ShortImage, LineImage>::Pointer lineExtract;
  lineExtract = itk::ExtractImageFilter<ShortImage, LineImage>::New();
  lineExtract->SetInput( if2 );

  extractIndex[0] = 2;
  extractIndex[1] = 0;
  extractSize[0] = 0;
  extractSize[1] = 3;
  extractRegion.SetIndex( extractIndex );
  extractRegion.SetSize( extractSize );

  lineExtract->SetExtractionRegion( extractRegion );
  lineExtract->UpdateLargestPossibleRegion();

  std::cout << "After 1D extraction. " << std::endl;
  
  //test the dimension collapse
  LineImage::RegionType requestedLineRegion;
  
  requestedLineRegion = lineExtract->GetOutput()->GetRequestedRegion();
  
  itk::ImageRegionIterator<LineImage>
    iteratorLineIn(lineExtract->GetOutput(), requestedLineRegion);

  ShortImage::IndexType testIndex;
  for (; !iteratorLineIn.IsAtEnd(); ++iteratorLineIn)
    {
    LineImage::PixelType linePixelValue = iteratorLineIn.Get();
    testIndex[0] = extractIndex[0];
    testIndex[1] = iteratorLineIn.GetIndex()[0];
    if (linePixelValue != if2->GetPixel(testIndex))
      {
      passed = false;
      }
    }
  if (passed)
    {
      std::cout << "ExtractImageFilter case 3 passed." << std::endl;
    }
  else
    {
      std::cout << "ExtractImageFilter case 3 failed." << std::endl;
      return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;
}
