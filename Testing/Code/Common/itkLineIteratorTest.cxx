/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Testcode for the itk::LineIterator.

#include <iostream>
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkLineIterator.h"
#include "itkTimeProbe.h"


int itkLineIteratorTest(int argc, char*argv[])
{
  const int Dimension = 2;
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef ImageType::RegionType::IndexType IndexType;
  typedef IndexType::IndexValueType IndexValueType;
  
 if (argc < 2)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  outputfilename" << std::endl;
    return 1;
    }

  // Set up a test image
  ImageType::RegionType::IndexType index;
  index.Fill(0);

  ImageType::RegionType::SizeType size;
  size.Fill(200);

  ImageType::RegionType region;
  region.SetIndex(index);
  region.SetSize(size);

  ImageType::Pointer output = ImageType::New();
  output->SetRegions(region);
  output->Allocate();
  output->FillBuffer(0);

  // First test: empty line
  IndexType startIndex;
  IndexType endIndex;

  startIndex[0] = 11;
  startIndex[1] = 13;
  endIndex[0] = 11;
  endIndex[1] = 13;

  typedef itk::LineIterator<ImageType> LineIteratorType;
  LineIteratorType across(output, startIndex, endIndex);

  // First test: currentIndex initialization
  if (startIndex != across.GetIndex())
    {
    std::cerr << "Error! Index should be startIndex.\n";
    return EXIT_FAILURE;
    }

  // Second test: IsAtEnd() is one pixel past the endIndex
  if (across.IsAtEnd())
    {
    std::cerr << "Error! Iterator should not be at end.\n";
    return EXIT_FAILURE;
    }

  ++across;
  if (!across.IsAtEnd())
    {
    std::cerr << "Error! Iterator should be at end.\n";
    return EXIT_FAILURE;
    }

  // Third test: draw some lines. 
  itk::TimeProbe timer;
  timer.Start();

  startIndex.Fill(10);
  endIndex.Fill(189);
  LineIteratorType it(output, startIndex, endIndex);
  while (!it.IsAtEnd())
    {
    it.Set(255);
    ++it;
    }

  startIndex.Fill(50);
  endIndex[0] = 150;
  endIndex[1] = startIndex[1];
  it = LineIteratorType(output, startIndex, endIndex);
  while (!it.IsAtEnd())
    {
    it.Set(150);
    ++it;
    }

  startIndex.Fill(120);
  endIndex[0] = 50;
  endIndex[1] = 100;
  it = LineIteratorType(output, startIndex, endIndex);
  while (!it.IsAtEnd())
    {
    it.Set(150);
    ++it;
    }
  
  timer.Stop();
  std::cerr << "Line drawing took " << timer.GetMeanTime() << " seconds.\n";

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(output);
  writer->SetFileName(argv[1]);
  writer->Update();

  return EXIT_SUCCESS;
}
