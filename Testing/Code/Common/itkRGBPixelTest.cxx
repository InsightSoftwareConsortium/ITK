/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkRGBPixelTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
