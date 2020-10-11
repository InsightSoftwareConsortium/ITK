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
#ifndef itkSTLConstContainerAdaptor_h
#define itkSTLConstContainerAdaptor_h

#include <itkMacro.h>

namespace itk
{
/** \class STLConstContainerAdaptor
 *  \brief An adapter object that casts a [const itk::XxxContainer] into [const std::xxx]
 *         and enables access to the underlying data structure.
 *
 * An adapter object that casts a [const itk::XxxContainer] into [const std::xxx]
 * and enables access to the underlying data structure.
 *
 * The class is provided for interface consistency with STLContainerAdaptor
 * plus the [const] modifier. Since everything is const, there is no need to call
 * AdapteeType::Modified() in the destructor.
 *
 * Here's a usage example of STLContainerAdaptor
 *
   \code
       itk::STLConstContainerAdaptor<itk::VectorContainer<SizeValueType, ElementType>> vecAdaptor(aContainer);
       const std::vector<ElementType> & vec = vecAdaptor.GetSTLContainerRef();
       // do things with vec ...
   \endcode
 *
 * \ingroup ITKCommon
 */

template <typename TContainer>
class STLConstContainerAdaptor
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(STLConstContainerAdaptor);

  using AdapteeType = const TContainer;

  using ElementType = const typename AdapteeType::Element;
  using TargetType = const typename AdapteeType::STLContainerType;

private:
  AdapteeType & m_AdapteeRef;

public:
  STLConstContainerAdaptor(AdapteeType & adaptee)
    : m_AdapteeRef(adaptee)
  {}

  STLConstContainerAdaptor(AdapteeType * adaptee)
    : m_AdapteeRef(*adaptee)
  {}

  TargetType &
  GetSTLConstContainerRef()
  {
    return m_AdapteeRef.CastToSTLConstContainer();
  }
};
} // end namespace itk

#endif
