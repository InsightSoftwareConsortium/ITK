/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBPixelTest.cxx
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

#include "itkRGBPixel.h"

int main(void)
{
  int i, j;
  
  // Test it all
  
  float val[3] = {1, 0, .5};
  itk::RGBPixel<float> pixel(val);
  itk::RGBPixel<char> pixelArray[2];
  pixelArray[0] = 255, 255, 255;
  pixelArray[1] = 255, 255, 244;
  
  std::cout << "sizeof(pixel) = " << sizeof (pixel) << std::endl;
  if (sizeof(pixel) != 3 * sizeof(itk::RGBPixel<float>::ComponentType))
    {
    std::cerr << "ERROR: sizeof(pixel) == " << sizeof(pixel) << " but is shopuld be " << 3 * sizeof(itk::RGBPixel<float>::ComponentType) << std::endl;
    return 1;
    }
  std::cout << "pixel.GetNumberOfComponents = " << pixel.GetNumberOfComponents() << std::endl;
  std::cout << "pixel.GetScalarValue() = " << pixel.GetScalarValue() << std::endl;
  std::cout << "pixel.GetNthComponent()" << std::endl;
  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }
  pixel.SetRed (11.0); pixel.SetGreen (22.0); pixel.SetBlue (33.0);
  std::cout << "pixel.SetRed (11.0); pixel.SetGreen (22.0); pixel.SetBlue (33.0);" << std::endl;
  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }
  std::cout << "pixel.GetRed() = " << pixel.GetRed() << std::endl;
  std::cout << "pixel.GetGreen() = " << pixel.GetGreen() << std::endl;
  std::cout << "pixel.GetBlue() = " << pixel.GetBlue() << std::endl;
  
  std::cout << "pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;" << std::endl;
  
  pixel[0] = 111; pixel[1] = 222; pixel = 333;
  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }
  
  std::cout << "std::cout << pixel << std::endl;" << std::endl;
  std::cout << "\t" << pixel << std::endl;
  
  for (j = 0; j < 2; j++)
    {
    std::cout << "pixelArray["<< j << "].GetNumberOfComponents = " << pixelArray[j].GetNumberOfComponents() << std::endl;
    std::cout << "pixelArray[" << j << "].GetScalarValue() = " << static_cast<int>(pixelArray[j].GetScalarValue()) << std::endl;
    std::cout << "pixelArray[" << j << "].GetNthComponent()" << std::endl;
    for (i = 0; i < pixelArray[j].GetNumberOfComponents(); i++)
      {
      std::cout << "\tpixelArray[" << j << "].GetNthComponent(" << i << ") = " << static_cast<int>(pixelArray[j].GetNthComponent(i)) << std::endl;
      }
    }

  return 0;
}
