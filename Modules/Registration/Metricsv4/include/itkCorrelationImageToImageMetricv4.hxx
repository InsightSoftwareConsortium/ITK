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
#ifndef itkCorrelationImageToImageMetricv4_hxx
#define itkCorrelationImageToImageMetricv4_hxx

#include "itkCorrelationImageToImageMetricv4.h"

namespace itk
{

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
CorrelationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::CorrelationImageToImageMetricv4() :
  m_AverageFix(0.0),
  m_AverageMov(0.0)
{
  this->m_DenseGetValueAndDerivativeThreader =
    CorrelationDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader =
    CorrelationSparseGetValueAndDerivativeThreaderType::New();

  m_HelperDenseThreader = CorrelationHelperDenseThreaderType::New();
  m_HelperSparseThreader = CorrelationHelperSparseThreaderType::New();

  if( this->m_MovingTransform->GetTransformCategory() == MovingTransformType::DisplacementField )
    {
    itkExceptionMacro("does not support displacement field transforms!!");
    }
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
CorrelationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::~CorrelationImageToImageMetricv4()
{
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
CorrelationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
CorrelationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage, TInternalComputationValueType, TMetricTraits>
::InitializeForIteration() const
{

  Superclass::InitializeForIteration();

  this->m_AverageFix = NumericTraits<MeasureType>::ZeroValue();
  this->m_AverageMov = NumericTraits<MeasureType>::ZeroValue();

  // compute the average intensity of the sampled pixels
  // Invoke the pipeline in the helper threader
  // refer to DomainThreader::Execute()

  if( this->m_UseFixedSampledPointSet ) // sparse sampling
    {
    SizeValueType numberOfPoints = this->GetNumberOfDomainPoints();
    if( numberOfPoints < 1 )
      {
      itkExceptionMacro("FixedSampledPointSet must have 1 or more points.");
      }
    typename ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Self >::DomainType range;
    range[0] = 0;
    range[1] = numberOfPoints - 1;
    this->m_HelperSparseThreader->Execute( const_cast< Self* >(this), range );
    }
  else // dense sampling
    {
    this->m_HelperDenseThreader->Execute( const_cast< Self* >(this), this->GetVirtualRegion() );
    }

  /*
   * the results:
   *  this->m_AverageFix
   *  this->m_AverageMov
   * will be stored during helper::AfterThreadedExecution()
   */


}

} // end namespace itk


#endif
