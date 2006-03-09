/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologicalGradientImageFilterTest.cxx
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
#include "itkMorphologicalGradientImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"

int itkMorphologicalGradientImageFilterTest(int argc, char * argv[])
{
  if( argc < 3 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const int dim = 2;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::BinaryBallStructuringElement< PType, dim  > StructuringElementType;
  StructuringElementType  structuringElement;
  structuringElement.SetRadius( 2 );
  structuringElement.CreateStructuringElement();

  typedef itk::MorphologicalGradientImageFilter< IType, IType, StructuringElementType > GradientType;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput(reader->GetOutput());
  gradient->SetKernel(  structuringElement );

  itk::SimpleFilterWatcher watcher(gradient);

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(gradient->GetOutput());
  writer->SetFileName(argv[2]);

  try
    {
  writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

