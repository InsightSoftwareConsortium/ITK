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
#ifndef itkStatisticsPositionLabelMapFilter_hxx
#define itkStatisticsPositionLabelMapFilter_hxx

#include "itkStatisticsPositionLabelMapFilter.h"
#include "itkLabelMapUtilities.h"
#include "itkStatisticsLabelObjectAccessors.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Label object representation and manipulation with ITK"
 * by Lehmann G.
 * https://hdl.handle.net/1926/584
 * http://www.insight-journal.org/browse/publication/176
 *
 */

namespace itk {

template <typename TImage>
StatisticsPositionLabelMapFilter<TImage>
::StatisticsPositionLabelMapFilter()
{
  this->m_Attribute = LabelObjectType::CENTER_OF_GRAVITY;
}


template <typename TImage>
void
StatisticsPositionLabelMapFilter<TImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  switch( this->m_Attribute )
    {
    case LabelObjectType::MAXIMUM_INDEX:
      {
      typedef typename Functor::MaximumIndexLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, false, labelObject);
      break;
      }
    case LabelObjectType::MINIMUM_INDEX:
      {
      typedef typename Functor::MinimumIndexLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, false, labelObject);
      break;
      }
    case LabelObjectType::CENTER_OF_GRAVITY:
      {
      typedef typename Functor::CenterOfGravityLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, true, labelObject);
      break;
      }
    default:
      Superclass::ThreadedProcessLabelObject( labelObject );
      break;
    }
}

}// end namespace itk
#endif
