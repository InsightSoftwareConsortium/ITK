/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkColorTable.h"

template<typename T> void ColorTableTest(const char *name)
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

  std::cout << "Test Passed ! " << std::endl;

  return EXIT_SUCCESS;
}
