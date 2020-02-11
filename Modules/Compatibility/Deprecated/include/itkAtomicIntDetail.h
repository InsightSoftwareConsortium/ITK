/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/*=========================================================================
 *
 * Program:   Visualization Toolkit
 * Module:    vtkAtomicInt.h
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *  All rights reserved.
 *  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.
 *
 *     This software is distributed WITHOUT ANY WARRANTY; without even
 *     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE.  See the above copyright notice for more information.
 *
 *=========================================================================*/

#ifndef itkAtomicIntDetail_h
#define itkAtomicIntDetail_h

#include "itkMacro.h"
#include "itkConceptChecking.h"

#include <atomic>

namespace itk
{

namespace Detail
{

template <size_t VSize>
struct BaseType;

template <size_t VSize>
class AtomicOps
{
public:
  using AtomicType = typename BaseType<VSize>::Type;
  using ValueType = typename BaseType<VSize>::Type;
};


template <>
struct BaseType<8>
{
  itkAlignedTypedef(8, int64_t, Type);
};

template <>
struct BaseType<4>
{
  itkAlignedTypedef(4, int32_t, Type);
};

template <typename T>
struct IsAtomicSupportedIntegralType
{
  using Self = IsAtomicSupportedIntegralType;
  struct Constraints
  {
    using TrueT = Concept::Detail::UniqueType_bool<true>;
    using SpecializedT = Concept::Detail::UniqueType_bool<NumericTraits<T>::is_specialized>;
    using IntegralT = Concept::Detail::UniqueType_bool<NumericTraits<T>::is_integer>;
    using SizeT = Concept::Detail::UniqueType_bool<sizeof(T) == 4 || sizeof(T) == 8>;
    void
    constraints()
    {
      IntegralT a = TrueT();
      IntegralT b = TrueT();
      IntegralT c = TrueT();

      IgnoreUnusedVariable(a);
      IgnoreUnusedVariable(b);
      IgnoreUnusedVariable(c);
    }
  };


  itkConceptConstraintsMacro();
};

} // end namespace Detail
} // end namespace itk

#endif
