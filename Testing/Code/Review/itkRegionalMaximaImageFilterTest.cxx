/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionalMaximaImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// a test routine for regional extrema using flooding
#include "itkRegionalMaximaImageFilter.h"
#include "itkHConvexImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkInvertIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"


int itkRegionalMaximaImageFilterTest(int, char * argv[])
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " Connection InputImage  OutputImageFile  " 
              << "OutputImageFile2" << std::endl;
    return EXIT_FAILURE;
    }
 
  const int dim = 3;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::RegionalMaximaImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetFullyConnected( atoi(argv[1]) );
  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();


  // produce the same output with other filters
  typedef itk::HConvexImageFilter< IType, IType > ConvexType;
  ConvexType::Pointer convex = ConvexType::New();
  convex->SetInput( reader->GetOutput() );
  convex->SetFullyConnected( atoi(argv[1]) );
  convex->SetHeight( 1 );

  // convex gives maxima with value=1 and others with value=0
  // rescale the image so we have maxima=255 other=0
  typedef itk::RescaleIntensityImageFilter< IType, IType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( convex->GetOutput() );
  rescale->SetOutputMaximum( 255 );
  rescale->SetOutputMinimum( 0 );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( rescale->GetOutput() );
  writer2->SetFileName( argv[4] );
  writer2->Update();

  return 0;
}
