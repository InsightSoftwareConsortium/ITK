/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDownsampleImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>

#include "itkImage.h"
#include "itkBSplineDownsampleImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkBSplineDownsampleImageFilterTest( int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "Error: Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputImage outputImage splineOrder" << std::endl; 
    return EXIT_FAILURE;
    }

  typedef unsigned char   PixelType;
  const unsigned int      Dimension = 3;

  typedef itk::Image< PixelType, Dimension >   ImageType;

  typedef itk::BSplineDownsampleImageFilter< ImageType, ImageType > DownsamplerFilterType;

  DownsamplerFilterType::Pointer filter = DownsamplerFilterType::New();

  FilterWatcher watcher(filter, "BSplineDownsampleImageFilter");

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const unsigned int splineOrder = atoi( argv[3] );

  filter->SetSplineOrder( splineOrder );

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
