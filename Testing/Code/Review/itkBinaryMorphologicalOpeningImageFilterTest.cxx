/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologicalOpeningImageFilterTest.cxx
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

#include "itkBinaryMorphologicalOpeningImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

int itkBinaryMorphologicalOpeningImageFilterTest(int argc, char * argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage Radius Background Foreground" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 2;
  
  typedef unsigned char                PixelType;
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryBallStructuringElement< PixelType, dim > KernelType;
  KernelType ball;
  KernelType::SizeType ballSize;
  ballSize.Fill( atoi( argv[3] ) );
  ball.SetRadius( ballSize );
  ball.CreateStructuringElement();
   
  typedef itk::BinaryMorphologicalOpeningImageFilter< ImageType, ImageType, KernelType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetKernel( ball );
  if( filter->GetBackgroundValue() != 0 )
    {
    std::cerr << "Wrong Background default value" << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetBackgroundValue( atoi( argv[4] ) );

  if( filter->GetForegroundValue() != 255 )
    {
    std::cerr << "Wrong Foreground default value" << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetForegroundValue( atoi( argv[5] ) );

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
