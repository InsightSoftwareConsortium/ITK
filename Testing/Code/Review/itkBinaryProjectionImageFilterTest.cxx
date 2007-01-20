/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryProjectionImageFilterTest.cxx
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
#include "itkThresholdLabelerImageFilter.h"
#include "itkChangeLabelImageFilter.h"

#include "itkBinaryProjectionImageFilter.h"
#include "itkNumericTraits.h"

int itkBinaryProjectionImageFilterTest(int argc, char * argv[])
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage Foreground Background" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 3;
  
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  
  // produce an image with 3 labels: 0 (background), 100 and 200
  
  typedef itk::ThresholdLabelerImageFilter< ImageType, ImageType > LabelerType;
  LabelerType::Pointer labeler = LabelerType::New();
  labeler->SetInput( reader->GetOutput() );
  LabelerType::RealThresholdVector thresholds;
  thresholds.push_back( 100 );
  thresholds.push_back( 200 );
  labeler->SetRealThresholds( thresholds );
  
  typedef itk::ChangeLabelImageFilter< ImageType, ImageType > ChangeType;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput( labeler->GetOutput() );
  change->SetChange( 1, 100 );
  change->SetChange( 2, 200 );

  typedef itk::BinaryProjectionImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( change->GetOutput() );

  //Exercise Set/Get methods for Foreground Value
  filter->SetForegroundValue( 255 );
  
  if ( filter->GetForegroundValue( ) != 255 )
    {
    std::cerr << "Set/Get Foreground value problem: " << filter->GetForegroundValue() << std::endl; 
    return EXIT_FAILURE;
    }

  filter->SetForegroundValue( atoi(argv[3]) );

  //Exercise Set/Get methods for Background Value
  filter->SetBackgroundValue( 0 );
  
  if ( filter->GetBackgroundValue( ) != 0 )
    {
    std::cerr << "Set/Get Background value problem" << std::endl; 
    return EXIT_FAILURE;
    }

  filter->SetBackgroundValue( atoi(argv[4]) );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

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

