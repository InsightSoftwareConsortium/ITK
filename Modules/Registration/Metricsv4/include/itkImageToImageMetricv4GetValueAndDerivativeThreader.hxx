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
#ifndef itkImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define itkImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

template< typename TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageMetricv4::VirtualImageDimension >, TImageToImageMetricv4 >
::ThreadedExecution ( const DomainType & imageSubRegion,
                      const ThreadIdType threadId )
{
  typename VirtualImageType::ConstPointer virtualImage = this->m_Associate->GetVirtualImage();
  typedef ImageRegionConstIteratorWithIndex< VirtualImageType > IteratorType;
  VirtualPointType virtualPoint;
  for( IteratorType it( virtualImage, imageSubRegion ); !it.IsAtEnd(); ++it )
    {
    const VirtualIndexType & virtualIndex = it.GetIndex();
    virtualImage->TransformIndexToPhysicalPoint( virtualIndex, virtualPoint );
    this->ProcessVirtualPoint( virtualIndex, virtualPoint, threadId );
    }
  //Finalize per thread actions
  this->m_Associate->FinalizeThread( threadId );
}

template< typename TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, TImageToImageMetricv4 >
::ThreadedExecution ( const DomainType & indexSubRange,
                      const ThreadIdType threadId )
{
  typename TImageToImageMetricv4::VirtualPointSetType::ConstPointer virtualSampledPointSet = this->m_Associate->GetVirtualSampledPointSet();
  typedef typename TImageToImageMetricv4::VirtualPointSetType::MeshTraits::PointIdentifier ElementIdentifierType;
  const ElementIdentifierType begin = indexSubRange[0];
  const ElementIdentifierType end   = indexSubRange[1];
  VirtualIndexType virtualIndex;
  typename VirtualImageType::ConstPointer virtualImage = this->m_Associate->GetVirtualImage();
  for( ElementIdentifierType i = begin; i <= end; ++i )
    {
    const VirtualPointType & virtualPoint = virtualSampledPointSet->GetPoint( i );
    virtualImage->TransformPhysicalPointToIndex( virtualPoint, virtualIndex );
    this->ProcessVirtualPoint( virtualIndex, virtualPoint, threadId );
    }
  //Finalize per thread actions
  this->m_Associate->FinalizeThread( threadId );
}

} // end namespace itk

#endif
