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
#ifndef __itkImageToImageObjectMetricGetValueAndDerivativeThreader_hxx
#define __itkImageToImageObjectMetricGetValueAndDerivativeThreader_hxx

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageToImageObjectMetricGetValueAndDerivativeThreader.h"

namespace itk
{

template< class TImageToImageObjectMetric >
void
ImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedImageRegionPartitioner< TImageToImageObjectMetric::VirtualImageDimension >, TImageToImageObjectMetric >
::ThreadedExecution ( const DomainType & imageSubRegion,
                      const ThreadIdType threadId )
{
  VirtualPointType virtualPoint;
  VirtualIndexType virtualIndex;
  typename VirtualImageType::ConstPointer virtualImage = this->m_Associate->GetVirtualDomainImage();
  typedef ImageRegionConstIteratorWithIndex< VirtualImageType > IteratorType;
  IteratorType it( virtualImage, imageSubRegion );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    virtualIndex = it.GetIndex();
    virtualImage->TransformIndexToPhysicalPoint( virtualIndex, virtualPoint );
    this->ProcessVirtualPoint( virtualIndex, virtualPoint, threadId );
    }
}

template< class TImageToImageObjectMetric >
void
ImageToImageObjectMetricGetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, TImageToImageObjectMetric >
::ThreadedExecution ( const DomainType & indexSubRange,
                      const ThreadIdType threadId )
{
  VirtualPointType virtualPoint;
  VirtualIndexType virtualIndex;
  typename VirtualImageType::ConstPointer virtualImage                                                = this->m_Associate->GetVirtualDomainImage();
  typename TImageToImageObjectMetric::VirtualSampledPointSetType::ConstPointer virtualSampledPointSet = this->m_Associate->GetVirtualSampledPointSet();
  typedef typename TImageToImageObjectMetric::VirtualSampledPointSetType::MeshTraits::PointIdentifier ElementIdentifierType;
  const ElementIdentifierType begin = indexSubRange[0];
  const ElementIdentifierType end   = indexSubRange[1];
  for( ElementIdentifierType i = begin; i <= end; ++i )
    {
    virtualPoint = virtualSampledPointSet->GetPoint( i );
    virtualImage->TransformPhysicalPointToIndex( virtualPoint, virtualIndex );
    this->ProcessVirtualPoint( virtualIndex, virtualPoint, threadId );
    }
}

} // end namespace itk

#endif
