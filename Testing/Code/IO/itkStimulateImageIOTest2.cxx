/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStimulateImageIOTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkStimulateImageIO.h"

int itkStimulateImageIOTest2( int argc, char* argv[] )
{
  // This test is usually run with the data file
  // Insight/Testing/Data/Input/kim_anat.spr
  if( argc < 2)
    {
    std::cerr << "Usage: " << argv[0] << " filename\n";
    return 1;
    }
  
  typedef float PixelType;
  typedef itk::Image<PixelType, 3> myImage;

  itk::StimulateImageIO::Pointer io;
  io = itk::StimulateImageIO::New();

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  
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
  std::cerr << "Display Range " << io->GetDisplayRange() << std::endl;


  return EXIT_SUCCESS;

}
