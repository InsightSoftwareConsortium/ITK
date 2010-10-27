/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColorTableTest.cxx
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

#include <iostream>

#include "itkColorTable.h"

template<class T> void ColorTableTest(const char *name)
{
  typedef itk::ColorTable<T> ColorTableType;
  typename ColorTableType::Pointer colors = ColorTableType::New();

  std::cout << "---------- Testing for type: :" << name
            << std::endl;
  colors->UseRandomColors(16);
  std::cout << "Random Colors" << std::endl;
  colors->Print(std::cout);
  std::cout << std::endl;

  colors->UseHeatColors(16);
  std::cout << "Heat Colors" << std::endl;
  colors->Print(std::cout);
  std::cout << std::endl;

  colors->UseGrayColors(16);
  std::cout << "Gray Colors" << std::endl;
  colors->Print(std::cout);
  std::cout << std::endl;

  colors->UseDiscreteColors();
  std::cout << "Discrete Colors" << std::endl;
  colors->Print(std::cout);
  std::cout << std::endl;
}

int itkColorTableTest(int, char* [] )
{
  ColorTableTest<unsigned char> ("unsigned char");
  ColorTableTest<char>          ("char");
  ColorTableTest<unsigned short>("unsigned short");
  ColorTableTest<short>         ("short");
  ColorTableTest<unsigned int>  ("unsigned int");
  ColorTableTest<int>           ("int");
  ColorTableTest<unsigned long> ("unsigned long");
  ColorTableTest<long>          ("long");
  ColorTableTest<float>         ("float");
  ColorTableTest<double>         ("double");

  // Find the closest color for a few colors
  typedef itk::ColorTable<unsigned char> ColorTableType;
  ColorTableType::Pointer colors = ColorTableType::New();

  unsigned int id;
  itk::RGBPixel<unsigned char> pixel;
  colors->UseRandomColors(10000);
  pixel.Set(255, 0, 0);
  id = colors->GetClosestColorTableId(pixel[0], pixel[1], pixel[2]);
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor(id)
            << " and name " << colors->GetColorName(id) << std::endl;

  colors->UseDiscreteColors();
  pixel.Set(255, 0, 0);
  id = colors->GetClosestColorTableId(pixel[0], pixel[1], pixel[2]);
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor(id)
            << " and name " << colors->GetColorName(id) << std::endl;

  colors->UseGrayColors();
  pixel.Set(17, 17, 17);
  id = colors->GetClosestColorTableId(pixel[0], pixel[1], pixel[2]);
  std::cout << "Pixel : " << pixel
            << " is closest to id: " << id
            << " which has the color: " << colors->GetColor(id)
            << " and name " << colors->GetColorName(id) << std::endl;

  // Check for degenerate case
  colors->UseGrayColors(1);
  colors->Print(std::cout);

  // Exercise the SetColorMethod
  colors->UseRandomColors(4);
  colors->SetColor(0, 0, 0, 0, "Background");
  colors->SetColor(1, 255, 0, 0, "Red");
  colors->SetColor(2, 255, 255, 0, "Yellow");
  pixel.Set(255, 255, 255);
  colors->SetColor(3, pixel, "White");
  colors->Print(std::cout);

  return EXIT_SUCCESS;
}
