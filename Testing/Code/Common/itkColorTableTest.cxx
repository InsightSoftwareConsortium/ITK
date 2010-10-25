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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkColorTable.h"


int itkColorTableTest(int, char* [] )
{
  typedef itk::ColorTable<unsigned char> ColorTableType;
  ColorTableType::Pointer colors = ColorTableType::New();

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

  typedef itk::ColorTable<short> ShortColorTableType;
  ShortColorTableType::Pointer shortColors = ShortColorTableType::New();

  shortColors->UseRandomColors(16);
  std::cout << "Random ShortColors" << std::endl;
  shortColors->Print(std::cout);
  std::cout << std::endl;

  shortColors->UseHeatColors(16);
  std::cout << "Heat ShortColors" << std::endl;
  shortColors->Print(std::cout);
  std::cout << std::endl;

  shortColors->UseGrayColors(16);
  std::cout << "Gray ShortColors" << std::endl;
  shortColors->Print(std::cout);
  std::cout << std::endl;

  shortColors->UseDiscreteColors();
  std::cout << "Discrete ShortColors" << std::endl;
  shortColors->Print(std::cout);
  std::cout << std::endl;

  std::cout << "Test Passed ! " << std::endl;
  return EXIT_SUCCESS;
}
