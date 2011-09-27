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

#include "itkSize.h"

int itkSizeTest(int, char* [] )
{

  typedef itk::Size<4> SizeType;

  SizeType size1 = {{10, 20, 30, 40}};
  std::cout << "  SizeType size1 = {10, 20, 30, 40}; ";
  std::cout << size1 << std::endl;

  SizeType size2 = size1;
  std::cout << "  SizeType size2 = size1; ";
  std::cout << size2 << std::endl;

  SizeType size3 = size1 + size2;
  std::cout << "  SizeType size3 = size1 + size2; ";
  std::cout << size3 << std::endl;

  size2 += size1;
  std::cout << "  size2 += size1; ";
  std::cout << size2 << std::endl;

  size3 = size2 - size1;
  std::cout << "  size3 = size2 - size1; ";
  std::cout << size3 << std::endl;

  size2 -= size1;
  std::cout << "  size2 -= size1; ";
  std::cout << size2 << std::endl;

  size1.Fill(2);  size2.Fill(4); size3 = size1 * size2;
  std::cout << "  size1.Fill(2);  size2.Fill(4); size3 = size1 * size2; ";
  std::cout << size3 << std::endl;

  size3 *= size1;
  std::cout << "  size3 *= size1; ";
  std::cout << size3 << std::endl;

  size2 = size1;
  if (size1 == size2)
    {
    std::cout << "size1 == size2" << std::endl;
    }

  size1 *= size2;
  if (size1 != size2)
    {
    std::cout << "size1 != size2" << std::endl;
    }

  std::cout << "size1.GetElement(2):" << size1.GetElement(2) << std::endl;
  return EXIT_SUCCESS;

}
