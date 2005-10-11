/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIOTest2.cxx
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
#include "itkImage.h"
#include "itkStimulateImageIO.h"
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <fstream>

int itkStimulateImageIOTest2( int argc, char* argv[] )
{
  // This test is usually run with the data file
  // Insight/Testing/Data/Input/BigEndian.spr
  if( argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " filename\n";
    return EXIT_FAILURE;
    }
  
  typedef float PixelType;
  typedef itk::Image<PixelType, 3> myImage;

  itk::StimulateImageIO::Pointer io;
  io = itk::StimulateImageIO::New();

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  
  std::cout << "Filename: " << argv[1] << std::endl;
 reader->SetFileName(argv[1]);
  reader->SetImageIO(io);
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }
  
  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout );
  
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // This is where we call all of the Get Functions to increase coverage.
  std::cout << "Display Range " << io->GetDisplayRange() << std::endl;


  return EXIT_SUCCESS;

}
