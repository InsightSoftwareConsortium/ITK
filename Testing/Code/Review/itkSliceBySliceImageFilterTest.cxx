/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceBySliceImageFilterTest.cxx
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkMedianImageFilter.h"
#include "itkSliceBySliceImageFilter.h"


int itkSliceBySliceImageFilterTest(int argc, char * argv[])
{

  if( argc != 3 )
    {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    exit(1);
    }

  const int                 Dimension = 3;
  typedef unsigned char     PixelType;

  typedef itk::Image< PixelType, Dimension >      ImageType;
 
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::SliceBySliceImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );

  typedef itk::MedianImageFilter< FilterType::InternalInputImageType,
                                  FilterType::InternalOutputImageType > MedianType;

  MedianType::Pointer median = MedianType::New();
  MedianType::InputSizeType radius;
  radius.Fill( 5 );
  median->SetRadius( radius );

  filter->SetFilter( median );
  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
    
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Exercise PrintSelf()
  //
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
