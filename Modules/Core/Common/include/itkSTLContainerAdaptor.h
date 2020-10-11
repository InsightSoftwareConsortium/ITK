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
#ifndef itkSTLContainerAdaptor_h
#define itkSTLContainerAdaptor_h

#include "itkMacro.h"

namespace itk
{
/** \class STLContainerAdaptor
 *  \brief An adapter object that casts a itk::XxxContainer into std::xxx
 *         and enables access to the underlying data structure.
 *
 * When the STLContainerAdaptor
 * is destroyed, it automatically calls XxxContainer::Modified().
 *
 * Here's a usage example of STLContainerAdaptor:
 *
   \code
       itk::STLContainerAdaptor<itk::VectorContainer<size_t, ElementType>> vecAdaptor(aContainer);
       std::vector<ElementType> & vec = vecAdaptor.GetSTLContainerRef();
       // do things with vec ...
       // upon return from function, vecAdaptor is destroyed and aContainer is Modified()
   \endcode
 *
 * \ingroup ITKCommon
 */

template <typename TContainer>
class STLContainerAdaptor
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(STLContainerAdaptor);

  using AdapteeType = TContainer;

  using ElementType = typename AdapteeType::Element;
  using TargetType = typename AdapteeType::STLContainerType;

private:
  AdapteeType & m_AdapteeRef;

public:
  STLContainerAdaptor(AdapteeType & adaptee)
    : m_AdapteeRef(adaptee)
  {}

  STLContainerAdaptor(AdapteeType * adaptee)
    : m_AdapteeRef(*adaptee)
  {}

  ~STLContainerAdaptor() { m_AdapteeRef.Modified(); }

  TargetType &
  GetSTLContainerRef()
  {
    return m_AdapteeRef.CastToSTLContainer();
  }
};
} // end namespace itk

#endif
