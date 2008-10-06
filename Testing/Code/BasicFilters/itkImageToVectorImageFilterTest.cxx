/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToVectorImageFilterTest.cxx
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
#include "itkImageToVectorImageFilter.h"
#include "itkImage.h"
#include "itkVectorImage.h"

#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"

int itkImageToVectorImageFilterTest(int argc, char *argv[] )
{

  typedef unsigned char PixelType;

  typedef itk::Image<PixelType,2>        ScalarImageType;
  typedef itk::VectorImage<PixelType,2>  VectorImageType;

  typedef itk::ImageFileReader<ScalarImageType>                ReaderType;
  typedef itk::ImageFileWriter<VectorImageType>                WriterType; 

  typedef itk::ImageToVectorImageFilter<ScalarImageType> FilterType;

  if (argc < 3)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  input1 input2 ... inputn output" << std::endl;
    return EXIT_FAILURE;
    }

  FilterType::Pointer filter = FilterType::New();
  int f = 0;
  for (int i=1; i < argc - 1; i++)
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName (argv[i]);
    reader->Update();
    filter->SetInput(f++,reader->GetOutput());
    }

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName (  argv[argc-1] );

  try
    {
    writer->SetInput(filter->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the file" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
