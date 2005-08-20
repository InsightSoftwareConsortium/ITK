/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNrrdComplexImageReadTest.cxx
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
#include <complex>

int itkNrrdComplexImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
  
  typedef std::complex<float> PixelType;
  typedef itk::Image<PixelType, 2> myImage;

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
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - 27.985973);
  err += fabs(sample.imag() - 0.0);

  coord[0] = 53;
  coord[1] = 43;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - -0.94961888);
  err += fabs(sample.imag() - 0.409872);

  coord[0] = 10;
  coord[1] = 43;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - -0.096564025);
  err += fabs(sample.imag() - 0.0094992276);

  coord[0] = 10;
  coord[1] = 0;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - 0.036231704);
  err += fabs(sample.imag() - -0.016659589);

  coord[0] = 42;
  coord[1] = 42;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - -0.027012844);
  err += fabs(sample.imag() - -0.012217643);

  coord[0] = 50;
  coord[1] = 40;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - 0.44949868);
  err += fabs(sample.imag() - -0.033380687);

  coord[0] = 8;
  coord[1] = 9;
  sample = image->GetPixel(coord);
  err += fabs(sample.real() - -0.036674671);
  err += fabs(sample.imag() - -0.0061681992);

  double thresh = 0.00000038;
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
