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
#ifndef itkStatisticsOpeningLabelMapFilter_hxx
#define itkStatisticsOpeningLabelMapFilter_hxx

#include "itkStatisticsOpeningLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkLabelMapUtilities.h"

namespace itk
{
template< typename TImage >
StatisticsOpeningLabelMapFilter< TImage >
::StatisticsOpeningLabelMapFilter()
{
  this->m_Attribute = LabelObjectType::MEAN;
  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput( 1, static_cast< TImage * >( this->MakeOutput(1).GetPointer() ) );
}

template< typename TImage >
void
StatisticsOpeningLabelMapFilter< TImage >
::GenerateData()
{
  switch ( this->m_Attribute )
    {
    itkStatisticsLabelMapFilterDispatchMacro()
    default:
      Superclass::GenerateData();
      break;
    }
}
} // end namespace itk
#endif
