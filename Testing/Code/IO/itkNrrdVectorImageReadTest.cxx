/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdVectorImageReadTest.cxx
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
#include "itkVector.h"

int itkNrrdVectorImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::Vector<float, 4> PixelType;
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
  myImage::IndexType coord;
  PixelType sample;

  // The test image has been constructed so that the vector coefficients
  // coincide with sample coordinates
  double err = 0;
  unsigned int idx = 0;
  for (unsigned int zi=0; zi<5; zi++)
    {
    coord[2] = zi;
    for (unsigned int yi=0; yi<5; yi++)
      {
      coord[1] = yi;
      for (unsigned int xi=0; xi<5; xi++)
        {
        coord[0] = xi;
        sample = image->GetPixel(coord);
        err += fabs(sample[0] - coord[0]);
        err += fabs(sample[1] - coord[1]);
        err += fabs(sample[2] - coord[2]);
        err += fabs(sample[3] - idx);
        idx++;
        }
      }
    }

  if (err)
    {
    std::cout << "test FAILED because values not as expected\n";
    return EXIT_FAILURE;
    }
  else
    {
    return EXIT_SUCCESS;
    }

}
