/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomIteratorTest2.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkImageFileWriter.h"


int itkImageRandomIteratorTest2( int argc, char * argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing arguments " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  outputImageFile" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;

  typedef unsigned long  PixelType;

  typedef itk::Image< PixelType, ImageDimension >  ImageType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ImageType::Pointer image = ImageType::New();

  WriterType::Pointer writer = WriterType::New();
  
  ImageType::SizeType size;

  size[0] = 1000;
  size[1] = 1000;

  unsigned long numberOfSamples = size[0] * size[1];

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  typedef itk::ImageRandomIteratorWithIndex< ImageType >      RandomIteratorType;

  RandomIteratorType it( image, region );

  it.SetNumberOfSamples( numberOfSamples );

  it.GoToBegin();
  
  PixelType counter = 0;
  
  //
  // Write down the order in which pixels are visited
  //
  while( !it.IsAtEnd() )
    {
    it.Set( counter );
    ++it;
    ++counter;
    }

  writer->SetInput( image );
  writer->SetFileName( argv[1] );
  writer->Update();
  
  return EXIT_SUCCESS;

  }



