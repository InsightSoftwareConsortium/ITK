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
#ifndef __itkStdAlgorithm_h
#define __itkStdAlgorithm_h

#include <algorithm>
namespace itk
{
namespace algorithm
{
/** if the STL library includes std::copy_n it is preferable over
 *  std::copy in many cases.
 *  It is not a standard part of STL, if it is missing, use
 *  std::copy instead.
 */
template<class InputIterator, class Size, class OutputIterator>
OutputIterator copy_n(InputIterator first, Size n, OutputIterator result)
{
#ifdef ITK_HAS_STD_COPY_N
  return std::copy_n(first,n,result);
#else
  return std::copy(first, first + n, result);
#endif
}

}
}
#endif
