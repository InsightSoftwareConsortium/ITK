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

  typedef unsigned char PType;
  typedef itk::Image< PType, 3 > IType;
  typedef itk::Image< PType, 2 > IType2;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::MaximumProjectionImageFilter< IType, IType2 > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetProjectionDimension( dim );
  // to be sure that the result is ok with several threads, even on a single
  // proc computer
  filter->SetNumberOfThreads( 2 );

//   itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType2 > WriterType;
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

  return EXIT_SUCCESS;
}

