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
#include "itkFixedArray.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkFixedArray);
  namespace wrappers
  {
    typedef itk::FixedArray<double, 2 >::FixedArray itkFixedArrayD2;
    typedef itk::FixedArray<double, 3 >::FixedArray itkFixedArrayD3;
    typedef itk::FixedArray<unsigned int, 2 >::FixedArray itkFixedArrayUI2;
    typedef itk::FixedArray<unsigned int, 3 >::FixedArray itkFixedArrayUI3;
    typedef itk::FixedArray<bool, 2 >::FixedArray itkFixedArrayB2;
    typedef itk::FixedArray<bool, 3 >::FixedArray itkFixedArrayB3;
  }
}
#endif
