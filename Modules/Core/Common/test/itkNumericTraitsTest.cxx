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
#include <vector>
#include <cstddef>

#include "itkNumericTraits.h"


template<class T> void CheckTraits(const char *name, T t)
{

  // check std::numeric_limits members
  std::cout << "itk::NumericTraits<" << name << ">" << std::endl;
  std::cout << "\tis_specialized: " << itk::NumericTraits<T>::digits << std::endl;
  std::cout << "\tdigits: " << itk::NumericTraits<T>::digits << std::endl;
  std::cout << "\tdigits10: " << itk::NumericTraits<T>::digits10 << std::endl;
  std::cout << "\tis_signed: " << itk::NumericTraits<T>::is_signed << std::endl;
  std::cout << "\tround_error(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::round_error()) << std::endl;
  std::cout << "\tdenorm_min(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::denorm_min()) << std::endl;
  std::cout << "\tepsilon(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::epsilon()) << std::endl;
  std::cout << "\tmin(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::min()) << std::endl;
  std::cout << "\tmax(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::max()) << std::endl;

  // check NumericTraits
  std::cout << "\tIsPositive( One )" << itk::NumericTraits<T>::IsPositive( itk::NumericTraits<T>::One ) << std::endl;
  std::cout << "\tIsNonpositive( One )" << itk::NumericTraits<T>::IsNonpositive( itk::NumericTraits<T>::One ) << std::endl;
  std::cout << "\tIsNegative( One )" << itk::NumericTraits<T>::IsNegative( itk::NumericTraits<T>::One ) << std::endl;
  std::cout << "\tIsNonnegative( One )" << itk::NumericTraits<T>::IsNonnegative( itk::NumericTraits<T>::One ) << std::endl;



  std::cout << "\tZero: " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::Zero) << std::endl;
  std::cout << "\tOne: " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::One) << std::endl;
  std::cout << "\tZeroValue(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::ZeroValue()) << std::endl;
  std::cout << "\tOneValue(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::OneValue()) << std::endl;
  std::cout << "\tmax(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::max(t)) << std::endl;
  std::cout << "\tmin(" << name << "): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::min(t)) << std::endl;

  std::cout << "\tNonpositiveMin(): " << static_cast<typename itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::NonpositiveMin()) << std::endl;




}


int itkNumericTraitsTest(int, char* [] )
{
  CheckTraits("char", static_cast<char>(0));
  CheckTraits("signed char", static_cast<signed char>(0));
  CheckTraits("unsigned char", static_cast<unsigned char>(0));

  CheckTraits("short", static_cast<short>(0));
  CheckTraits("signed short", static_cast<signed short>(0));
  CheckTraits("unsigned short", static_cast<unsigned short>(0));

  CheckTraits("int", static_cast<int>(0));
  CheckTraits("signed int", static_cast<signed int>(0));
  CheckTraits("unsigned int", static_cast<unsigned int>(0));

  CheckTraits("long", static_cast<long>(0));
  CheckTraits("signed long", static_cast<signed long>(0));
  CheckTraits("unsigned long", static_cast<unsigned long>(0));

  CheckTraits("long long", static_cast<long long>(0));
  CheckTraits("signed long long", static_cast<signed long long>(0));
  CheckTraits("unsigned long long", static_cast<unsigned long long>(0));

  CheckTraits("float", static_cast<float>(0));
  CheckTraits("double", static_cast<double>(0));
  CheckTraits("long double", static_cast<long double>(0));

  // Check not fundamental types which we need

  CheckTraits("size_t", static_cast<size_t>(0));
  CheckTraits("ptrdiff_t", static_cast<ptrdiff_t>(0));
  typedef std::vector<int>::size_type VectorSizeType;
  CheckTraits("std::vector<int>::size_type", static_cast<VectorSizeType>(0));

  return EXIT_SUCCESS;
}
