/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest.cxx
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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkDiffusionTensor3D.h"

int itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest( int ac, char* av[] )
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Input Output\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::DiffusionTensor3D<double> InPixelType;
  typedef itk::DiffusionTensor3D<float> OutPixelType;
  typedef itk::Image<InPixelType, 3> InImage;
  typedef itk::Image<OutPixelType, 3> OutImage;

  itk::ImageFileReader<InImage>::Pointer reader 
                                  = itk::ImageFileReader<InImage>::New();
  reader->SetFileName(av[1]);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  
  InImage::Pointer image = reader->GetOutput();
  image->Print(std::cout );

  // Generate test image
  itk::ImageFileWriter<InImage>::Pointer writer;
  writer = itk::ImageFileWriter<InImage>::New();
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(av[2]);
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
