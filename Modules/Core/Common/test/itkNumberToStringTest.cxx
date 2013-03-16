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

#include "itkNumberToString.h"
#include "itkNumericTraits.h"

template<typename T> void PrintValue(const char *t, T)
{
  itk::NumberToString<T> convert;
  std::cout << t << "(min) "
            << "raw: " << itk::NumericTraits<T>::min()
            << " converted: " << convert(itk::NumericTraits<T>::min()) << std::endl;
  std::cout << t << "(NonpositiveMin) "
            << "raw: " << itk::NumericTraits<T>::NonpositiveMin()
            << " converted" << convert(itk::NumericTraits<T>::NonpositiveMin()) << std::endl;
  std::cout << t << "(max) "
            << "raw: " << itk::NumericTraits<T>::max()
            << " converted: " << convert(itk::NumericTraits<T>::max()) << std::endl;
  std::cout << t << "(round_error) "
            << "raw: " << itk::NumericTraits<T>::round_error()
            << " converted: " << convert(itk::NumericTraits<T>::round_error()) << std::endl;
}
int itkNumberToStringTest(int, char* [] )
{
  PrintValue("unsigned char", static_cast<unsigned char>(0));
  PrintValue("char", static_cast<char>(0));
  PrintValue("unsigned short", static_cast<unsigned short>(0));
  PrintValue("short", static_cast<short>(0));
  PrintValue("unsigned int", static_cast<unsigned int>(0));
  PrintValue("int", static_cast<int>(0));
  PrintValue("unsigned long", static_cast<unsigned long>(0));
  PrintValue("long", static_cast<long>(0));
  PrintValue("float", static_cast<float>(0));
  PrintValue("double", static_cast<double>(0));
  PrintValue("long double", static_cast<long double>(0));

  return EXIT_SUCCESS;

}
