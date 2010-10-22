/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryReconstructionLabelMapFilterTest1.cxx
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
#include "itkSimpleFilterWatcher.h"

#include "itkBinaryReconstructionByDilationImageFilter.h"
#include "itkTestingMacros.h"

int itkBinaryReconstructionByDilationImageFilterTest(int argc, char * argv[])
{
  if( argc != 6 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input marker output";
    std::cerr << " fg bg";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::BinaryReconstructionByDilationImageFilter< ImageType > LabelReconstructionType;
  LabelReconstructionType::Pointer reconstruction = LabelReconstructionType::New();

  //testing get and set macros for Lambda
  int fg = atoi( argv[4] );
  reconstruction->SetForegroundValue( fg );
  TEST_SET_GET_VALUE( fg , reconstruction->GetForegroundValue() );

  int bg = atoi( argv[5] );
  reconstruction->SetBackgroundValue( bg );
  TEST_SET_GET_VALUE( bg , reconstruction->GetBackgroundValue() );

  reconstruction->SetMaskImage( reader->GetOutput() );
  reconstruction->SetMarkerImage( reader2->GetOutput() );

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reconstruction->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
