/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTIFFImageIOTest.cxx
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
#include <fstream>

int itkTIFFImageIOTest( int ac, char* av[] )
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] 
              << " Input Output [dimensionality:default 2]"
              << "[pixeltype: 1:uchar(default) 2:short]\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::RGBPixel<unsigned char> PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  typedef itk::Image<unsigned char, 3> myImage3D;
  typedef itk::Image<short, 2> myImageShort;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();

  itk::ImageFileReader<myImage3D>::Pointer reader3D 
                                  = itk::ImageFileReader<myImage3D>::New();

  itk::ImageFileReader<myImageShort>::Pointer readerShort 
                                  = itk::ImageFileReader<myImageShort>::New();


  if((ac == 5) && (!strcmp(av[4],"2")))
    {
    readerShort->SetFileName(av[1]);
    }
  else if((ac == 4) && (!strcmp(av[3],"3")))
    {
    reader3D->SetFileName(av[1]);
    }
  else
    {
    reader->SetFileName(av[1]);
    }

  try
    { 
    if((ac == 5) && (!strcmp(av[4],"2")))
      {
      readerShort->Update();
      }
    else if((ac == 4) && (!strcmp(av[3],"3")))
      {
      reader3D->Update();
      }
    else
      {
      reader->Update();
      }
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }
  

  if((ac == 5) && (!strcmp(av[4],"2")))
    {
    myImageShort::Pointer image = readerShort->GetOutput();

    image->Print(std::cout );

    myImageShort::RegionType region = image->GetLargestPossibleRegion();
    std::cout << "region " << region;

    // Generate test image
    itk::ImageFileWriter<myImageShort>::Pointer writer;
    writer = itk::ImageFileWriter<myImageShort>::New();
    writer->SetInput( readerShort->GetOutput() );
    writer->SetFileName(av[2]);
    writer->Update();
    }
  else if((ac == 4) && (!strcmp(av[3],"3")))
    {
    myImage3D::Pointer image = reader3D->GetOutput();

    image->Print(std::cout );

    myImage3D::RegionType region = image->GetLargestPossibleRegion();
    std::cout << "region " << region;

    // Generate test image
    itk::ImageFileWriter<myImage3D>::Pointer writer;
    writer = itk::ImageFileWriter<myImage3D>::New();
    writer->SetInput( reader3D->GetOutput() );
    writer->SetFileName(av[2]);
    writer->Update();
    }
  else
    {
    myImage::Pointer image = reader->GetOutput();

    image->Print(std::cout );

    myImage::RegionType region = image->GetLargestPossibleRegion();
    std::cout << "region " << region;

    // Generate test image
    itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( reader->GetOutput() );
    writer->SetFileName(av[2]);
    writer->Update();
    }

  return EXIT_SUCCESS;

}
