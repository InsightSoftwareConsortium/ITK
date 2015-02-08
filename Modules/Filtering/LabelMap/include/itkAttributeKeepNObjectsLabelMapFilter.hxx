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
#ifndef itkAttributeKeepNObjectsLabelMapFilter_hxx
#define itkAttributeKeepNObjectsLabelMapFilter_hxx

#include "itkAttributeKeepNObjectsLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <typename TImage, typename TAttributeAccessor>
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::AttributeKeepNObjectsLabelMapFilter()
{
  m_ReverseOrdering = false;
  m_NumberOfObjects = 1;
  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput(1, static_cast<TImage*>(this->MakeOutput(1).GetPointer()));
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();
  ImageType * output2 = this->GetOutput( 1 );

  // set the background value for the second output - this is not done in the superclasses
  output2->SetBackgroundValue( output->GetBackgroundValue() );

  ProgressReporter progress( this, 0, 2 * output->GetNumberOfLabelObjects() );

  // get the label objects in a vector, so they can be sorted
  typedef typename std::vector< typename LabelObjectType::Pointer > VectorType;
  VectorType labelObjects;
  labelObjects.reserve( output->GetNumberOfLabelObjects() );
  for( typename ImageType::Iterator it( output );
    ! it.IsAtEnd();
    ++it )
    {
    labelObjects.push_back( it.GetLabelObject() );
    progress.CompletedPixel();
    }

  // instantiate the comparator and sort the vector
  if( m_NumberOfObjects < output->GetNumberOfLabelObjects() )
    {
    typename VectorType::iterator end = labelObjects.begin() + m_NumberOfObjects;
    if( m_ReverseOrdering )
      {
      ReverseComparator comparator;
      std::nth_element( labelObjects.begin(), end, labelObjects.end(), comparator );
      }
    else
      {
      Comparator comparator;
      std::nth_element( labelObjects.begin(), end, labelObjects.end(), comparator );
      }
//   progress.CompletedPixel();

    // and move the last objects to the second output
    for( typename VectorType::const_iterator it = end;
      it != labelObjects.end();
      it++ )
      {
      output->RemoveLabelObject( *it );
      output2->AddLabelObject( *it );
      progress.CompletedPixel();
      }
    }
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "NumberOfObjects: "  << m_NumberOfObjects << std::endl;
}

}// end namespace itk
#endif
