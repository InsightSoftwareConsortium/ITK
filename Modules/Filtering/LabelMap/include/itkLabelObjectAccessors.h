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
#ifndef itkLabelObjectAccessors_h
#define itkLabelObjectAccessors_h

#include "itkMacro.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 */

namespace itk
{
namespace Functor
{
template< typename TLabelObject >
class LabelLabelObjectAccessor
{
public:
  typedef TLabelObject                        LabelObjectType;
  typedef typename LabelObjectType::LabelType AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetLabel();
  }
};

template< typename TLabelObject >
class NumberOfLinesLabelObjectAccessor
{
public:
  typedef TLabelObject LabelObjectType;
  typedef int          AttributeValueType;

  inline AttributeValueType operator()(const LabelObjectType *labelObject) const
  {
    return labelObject->GetNumberOfLines();
  }
};

template< typename TLabelObject, typename TAttributeAccessor >
class LabelObjectComparator
{
public:
  typedef TLabelObject       LabelObjectType;
  typedef TAttributeAccessor AttributeAccessorType;
  bool operator()(const LabelObjectType *a, const LabelObjectType *b) const
  {
    return m_Accessor(a) > m_Accessor(b);
  }

  LabelObjectComparator() {}
  LabelObjectComparator(LabelObjectComparator const & from)
  {
    m_Accessor = from.m_Accessor;
  }

private:
  AttributeAccessorType m_Accessor;
};

template< typename TLabelObject, typename TAttributeAccessor >
class LabelObjectReverseComparator
{
public:
  typedef TLabelObject       LabelObjectType;
  typedef TAttributeAccessor AttributeAccessorType;
  bool operator()(const LabelObjectType *a, const LabelObjectType *b) const
  {
    return m_Accessor(a) < m_Accessor(b);
  }

  LabelObjectReverseComparator() {}
  LabelObjectReverseComparator(LabelObjectReverseComparator const & from)
  {
    m_Accessor = from.m_Accessor;
  }

private:
  AttributeAccessorType m_Accessor;
};
}
} // end namespace itk

#endif
