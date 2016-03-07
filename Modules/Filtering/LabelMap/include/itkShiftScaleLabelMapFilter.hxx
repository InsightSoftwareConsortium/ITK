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
#ifndef itkShiftScaleLabelMapFilter_hxx
#define itkShiftScaleLabelMapFilter_hxx

#include "itkShiftScaleLabelMapFilter.h"
#include "itkProgressReporter.h"

namespace itk
{
template< typename TImage >
ShiftScaleLabelMapFilter< TImage >
::ShiftScaleLabelMapFilter()
{
  m_Shift = 0.0;
  m_Scale = 1.0;
  m_ChangeBackgroundValue = false;
}

template< typename TImage >
void
ShiftScaleLabelMapFilter< TImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType *output = this->GetOutput();

  // get the label objects
  typename ImageType::LabelObjectVectorType labelObjects = output->GetLabelObjects();

  ProgressReporter progress( this, 0, static_cast<SizeValueType>( labelObjects.size() ) );

  // change the background, if requested
  if ( m_ChangeBackgroundValue )
    {
    PixelType label = static_cast< PixelType >( m_Scale * output->GetBackgroundValue() + m_Shift );
    output->SetBackgroundValue(label);
    }

  // and put back the objects in the map
  output->ClearLabels();
  typename ImageType::LabelObjectVectorType::iterator it = labelObjects.begin();
  while ( it != labelObjects.end() )
    {
    LabelObjectType *lo = *it;
    PixelType        label = static_cast< PixelType >( m_Scale * lo->GetLabel() + m_Shift );
    lo->SetLabel(label);
    output->AddLabelObject(lo);

    // go to the next label
    progress.CompletedPixel();
    ++it;
    }
}

template< typename TImage >
void
ShiftScaleLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shift: "  << m_Shift << std::endl;
  os << indent << "Scale: "  << m_Scale << std::endl;
  os << indent << "ChangeBackgroundValue: "  << m_ChangeBackgroundValue << std::endl;
}
} // end namespace itk
#endif
