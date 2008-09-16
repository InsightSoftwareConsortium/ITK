/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDicomImageIOTest.cxx
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
#include <list>
#include <fstream>
#include "itkDicomImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkRescaleIntensityImageFilter.h"

int itkDicomImageIOTest(int ac, char* av[])
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputImage\n";
    return EXIT_FAILURE;
    }


  // ATTENTION THIS IS THE PIXEL TYPE FOR 
  // THE RESULTING IMAGE
  typedef short PixelType;
  
  typedef itk::Image<PixelType, 2> myImage;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  itk::DicomImageIOFactory::RegisterOneFactory();
  
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
  
  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               myImage, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( av[2] );
 
  // Software Guide : BeginCodeSnippet
  rescaler->SetInput( reader->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  return EXIT_SUCCESS;

}
