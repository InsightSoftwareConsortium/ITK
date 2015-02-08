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
#ifndef itkShapePositionLabelMapFilter_hxx
#define itkShapePositionLabelMapFilter_hxx

#include "itkShapePositionLabelMapFilter.h"
#include "itkLabelMapUtilities.h"
#include "itkShapeLabelObjectAccessors.h"

namespace itk {

template <typename TImage>
ShapePositionLabelMapFilter<TImage>
::ShapePositionLabelMapFilter()
{
  m_Attribute = LabelObjectType::CENTROID;
}


template <typename TImage>
void
ShapePositionLabelMapFilter<TImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  switch( m_Attribute )
    {
    case LabelObjectType::CENTROID:
      {
      typedef typename Functor::CentroidLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, true, labelObject);
      break;
      }
    default:
      itkExceptionMacro(<< "Unknown attribute type");
      break;
    }
}

template <typename TImage>
void
ShapePositionLabelMapFilter<TImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")" << std::endl;
}

}// end namespace itk
#endif
