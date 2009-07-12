/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIOTest.cxx
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
#include <fstream>
#include <iostream>
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkStimulateImageIO.h"

int itkStimulateImageIOTest(int argc, char* argv[] )
{
  typedef itk::Image<float,2> FloatImageType;

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output1 output2 " << std::endl;
    return EXIT_FAILURE;
    }

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  FloatImageType::SizeValueType size[2];
  size[0]=128; size[1]=64;
  
  itk::RandomImageSource<FloatImageType>::Pointer random;
  random = itk::RandomImageSource<FloatImageType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::StimulateImageIO::Pointer sprIO;
  sprIO = itk::StimulateImageIO::New();

  // Write out the image
  itk::ImageFileWriter<FloatImageType>::Pointer writer;
  writer = itk::ImageFileWriter<FloatImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(sprIO);
  writer->Write();

  if ( !sprIO->CanReadFile(argv[1]) )
    {
    return EXIT_FAILURE;
    }

  try
    {
    // Create a source object (in this case a reader)
    itk::ImageFileReader<FloatImageType>::Pointer reader;
    reader = itk::ImageFileReader<FloatImageType>::New();
    reader->SetImageIO(sprIO);
    reader->SetFileName(argv[1]);
    reader->Update();

    writer->SetInput(reader->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Write();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}



