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
#ifndef itkNumberToString_hxx
#define itkNumberToString_hxx

#include "itkNumberToString.h"
#include "itkNumericTraits.h"

#include <sstream>

namespace itk
{

template<typename TValue>
std::string NumberToString<TValue>::operator() (TValue val)
{
  std::ostringstream output;
  output << static_cast<typename NumericTraits<TValue>::PrintType>(val);

  return output.str();
}

}
#endif
