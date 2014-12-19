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
#ifndef itkAttributeOpeningLabelMapFilter_hxx
#define itkAttributeOpeningLabelMapFilter_hxx

#include "itkAttributeOpeningLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <typename TImage, typename TAttributeAccessor>
AttributeOpeningLabelMapFilter<TImage, TAttributeAccessor>
::AttributeOpeningLabelMapFilter()
{
  m_Lambda = NumericTraits< AttributeValueType >::ZeroValue();
  m_ReverseOrdering = false;
  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput(1, static_cast<TImage*>(this->MakeOutput(1).GetPointer()));
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeOpeningLabelMapFilter<TImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();
  ImageType * output2 = this->GetOutput( 1 );

  // set the background value for the second output - this is not done in the superclasses
  output2->SetBackgroundValue( output->GetBackgroundValue() );

  AttributeAccessorType accessor;

  ProgressReporter progress( this, 0, output->GetNumberOfLabelObjects() );

  typename ImageType::Iterator it( output );
  while( ! it.IsAtEnd() )
    {
    typename LabelObjectType::LabelType label = it.GetLabel();
    LabelObjectType * labelObject = it.GetLabelObject();

    if( ( !m_ReverseOrdering && accessor( labelObject ) < m_Lambda )
      || ( m_ReverseOrdering && accessor( labelObject ) > m_Lambda ) )
      {
      // must increment the iterator before removing the object to avoid invalidating the iterator
      ++it;
      output2->AddLabelObject( labelObject );
      output->RemoveLabel( label );
      }
    else
      {
      ++it;
      }

    progress.CompletedPixel();
    }
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeOpeningLabelMapFilter<TImage, TAttributeAccessor>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "Lambda: "  << static_cast<typename NumericTraits<AttributeValueType>::PrintType>(m_Lambda) << std::endl;
}

}// end namespace itk
#endif
