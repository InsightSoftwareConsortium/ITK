/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdDiffusionTensor3DImageReadTest.cxx
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
#include "itkImage.h"
#include "itkDiffusionTensor3D.h"

int itkNrrdDiffusionTensor3DImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::DiffusionTensor3D<float> PixelType;
  typedef itk::Image<PixelType, 3> myImage;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  reader->SetFileName(av[1]);

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

  // Pick off some pixels in the test image (the first, the last, and
  // a few in between) and make sure that the values are very close to
  // what we know to be correct, given that we know exactly what
  // volume is being read.  I/O errors will generate more than tiny
  // differences near representational precision.

  float err = 0;
  myImage::IndexType coord;
  PixelType sample;
  coord[0] = 0;
  coord[1] = 0;
  coord[2] = 0;
  sample = image->GetPixel(coord);
  err += fabs(sample(0,0) - 4.0248222);
  err += fabs(sample(0,1) - -0.2367909);
  err += fabs(sample(0,2) - 0.23370844);
  err += fabs(sample(1,1) - 1.2593846);
  err += fabs(sample(1,2) - -0.042955428);
  err += fabs(sample(2,2) - -0.15239859);

  coord[0] = 4;
  coord[1] = 5;
  coord[2] = 6;
  sample = image->GetPixel(coord);
  err += fabs(sample(0,0) - 6.3746634);
  err += fabs(sample(0,1) - -0.10792637);
  err += fabs(sample(0,2) - 0.078167915);
  err += fabs(sample(1,1) - 6.2988205);
  err += fabs(sample(1,2) - 0.40197921);
  err += fabs(sample(2,2) - 4.8993769);

  coord[0] = 3;
  coord[1] = 3;
  coord[2] = 3;
  sample = image->GetPixel(coord);
  err += fabs(sample(0,0) - 4.9852095);
  err += fabs(sample(0,1) - 0.51356757);
  err += fabs(sample(0,2) - -1.1457335);
  err += fabs(sample(1,1) - 0.6538533);
  err += fabs(sample(1,2) - -0.19546235);
  err += fabs(sample(2,2) - 1.3520061);

  coord[0] = 3;
  coord[1] = 0;
  coord[2] = 5;
  sample = image->GetPixel(coord);
  err += fabs(sample(0,0) - 6.1530757);
  err += fabs(sample(0,1) - 0.40738893);
  err += fabs(sample(0,2) - 0.11354899);
  err += fabs(sample(1,1) - 5.968379);
  err += fabs(sample(1,2) - -0.22650599);
  err += fabs(sample(2,2) - 5.0935121);

  coord[0] = 0;
  coord[1] = 3;
  coord[2] = 6;
  sample = image->GetPixel(coord);
  err += fabs(sample(0,0) - 7.3227096);
  err += fabs(sample(0,1) - -0.34219909);
  err += fabs(sample(0,2) - -0.2011447);
  err += fabs(sample(1,1) - 5.6443777);
  err += fabs(sample(1,2) - 0.43205333);
  err += fabs(sample(2,2) - 5.3755102);

  double thresh = 0.00000041;
  if (err > thresh)
    {
    std::cout << "failure because err == " << err 
              << "> " << thresh << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    return EXIT_SUCCESS;
    }

}
