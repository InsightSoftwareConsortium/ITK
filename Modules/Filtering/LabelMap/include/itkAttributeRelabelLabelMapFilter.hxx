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
#ifndef itkAttributeRelabelLabelMapFilter_hxx
#define itkAttributeRelabelLabelMapFilter_hxx

#include "itkAttributeRelabelLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <typename TImage, typename TAttributeAccessor>
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::AttributeRelabelLabelMapFilter()
{
  m_ReverseOrdering = false;
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();

  typedef typename std::vector< typename LabelObjectType::Pointer > VectorType;

  ProgressReporter progress( this, 0, 2 * output->GetNumberOfLabelObjects() );

  // get the label objects in a vector, so they can be sorted
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
  if( m_ReverseOrdering )
    {
    ReverseComparator comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
  else
    {
    Comparator comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
  //   progress.CompletedPixel();

  // and put back the objects in the map
  output->ClearLabels();
  unsigned int label = 0;
  for( typename VectorType::const_iterator it = labelObjects.begin();
    it != labelObjects.end();
    it++ )
    {
    // avoid the background label if it is used
    if( label == output->GetBackgroundValue() )
      {
      label++;
      }
    (*it)->SetLabel( label );
    output->AddLabelObject( *it );

    // go to the nex label
    label++;
    progress.CompletedPixel();
    }
}


template <typename TImage, typename TAttributeAccessor>
void
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
}

}// end namespace itk
#endif
