/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToRGBPixelFunctorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkScalarToRGBPixelFunctor.h"

int itkScalarToRGBPixelFunctorTest(int, char* [] )
{
  itk::RGBPixel<unsigned char> pixel;
  
  // Test with unsigned long.
  itk::Functor::ScalarToRGBPixelFunctor<unsigned long> ulf;

  std::cout << "Testing unsigned long integers in big endian mode"
            << std::endl;
  ulf.SetBigEndian();
  unsigned long ul;
  for (ul = 0; ul < 100; ++ul)
    {
      pixel = ulf(ul);
      std::cout << ul << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  std::cout << "Testing unsigned long integers in little endian mode"
            << std::endl;
  
  ulf.SetLittleEndian();
  for (ul = 0; ul < 100; ++ul)
    {
      pixel = ulf(ul);
      std::cout << ul << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  
  // Test with unsigned char.
  itk::Functor::ScalarToRGBPixelFunctor<unsigned char> ucf;

  std::cout << "Testing unsigned char" << std::endl;
  for (char c = 0; c < 100; ++c)
    {
      pixel = ucf(c);
      std::cout << static_cast<int>(c) << "->"  << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }

  // Test with float
  itk::Functor::ScalarToRGBPixelFunctor<float> ff;
  float f;
  std::cout << "Testing float in big endian mode" << std::endl;
  ff.SetBigEndian();
  for (f = 0; f < 100; ++f)
    {
      pixel = ff(f);
      
      std::cout << f << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  std::cout << "Testing float in little endian mode"
            << std::endl;
  
  ff.SetLittleEndian();
  for (f = 0; f < 100; ++f)
    {
      pixel = ff(f);
      std::cout << f << "->" << static_cast<int>(pixel[0]) << " "
                << static_cast<int>(pixel[1]) << " " << static_cast<int>(pixel[2])
                << std::endl;
    }
  return EXIT_SUCCESS;
}
