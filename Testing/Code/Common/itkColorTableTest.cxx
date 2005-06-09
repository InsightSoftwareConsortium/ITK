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
