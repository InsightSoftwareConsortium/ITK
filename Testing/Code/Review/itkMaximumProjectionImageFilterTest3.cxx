/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaximumProjectionImageFilterTest3.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkMaximumProjectionImageFilter.h"
#include "itkExtractImageFilter.h"


int itkMaximumProjectionImageFilterTest3(int argc, char * argv[])
{
  if( argc < 4 )
    {
    std::cerr << "Missing parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "Dimension Inputimage Outputimage " << std::endl;
    return EXIT_FAILURE;
    }

  int dim = atoi(argv[1]);

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, 3 > ImageType;
  typedef itk::Image< PixelType, 2 > Image2DType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::MaximumProjectionImageFilter< 
    ImageType, Image2DType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetProjectionDimension( dim );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< Image2DType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    } 
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Set ProjectionDimension to a bad value
  bool caught = false;
  try
    {
    filter->SetProjectionDimension(100);
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << std::endl << "Caught expected exception!";
    std::cerr << excp << std::endl;
    caught = true;
    }
  if (!caught)
    {
    std::cerr << "Failed to catch expected exception!" << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
