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
#ifndef __itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define __itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{
template<class TDomainPartitioner, class TImageToImageMetric, class TCorrelationMetric>
void CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::BeforeThreadedExecution()
{
  TCorrelationMetric * associate = dynamic_cast<TCorrelationMetric *>(this->m_Associate);

  Superclass::BeforeThreadedExecution();

  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize = associate->GetNumberOfParameters();

  // set size
  m_InternalCumSumPerThread.resize(this->GetNumberOfThreadsUsed());
  for (ThreadIdType i = 0; i < this->GetNumberOfThreadsUsed(); i++)
    {
    itkDebugMacro("CorrelationImageToImageMetricv4::Initialize: transform does NOT have local support\n");

    this->m_InternalCumSumPerThread[i].fdm.SetSize(globalDerivativeSize);
    this->m_InternalCumSumPerThread[i].mdm.SetSize(globalDerivativeSize);

    }

  //---------------------------------------------------------------
  // Set initial values.
  for (ThreadIdType i = 0; i < this->GetNumberOfThreadsUsed(); i++)
    {
    m_InternalCumSumPerThread[i].fm = NumericTraits<InternalComputationValueType>::Zero;
    m_InternalCumSumPerThread[i].f2 = NumericTraits<InternalComputationValueType>::Zero;
    m_InternalCumSumPerThread[i].m2 = NumericTraits<InternalComputationValueType>::Zero;
    m_InternalCumSumPerThread[i].f = NumericTraits<InternalComputationValueType>::Zero;
    m_InternalCumSumPerThread[i].m = NumericTraits<InternalComputationValueType>::Zero;

    this->m_InternalCumSumPerThread[i].mdm.Fill(NumericTraits<DerivativeValueType>::Zero);
    this->m_InternalCumSumPerThread[i].fdm.Fill(NumericTraits<DerivativeValueType>::Zero);
    }

}

template<class TDomainPartitioner, class TImageToImageMetric, class TCorrelationMetric>
void
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::AfterThreadedExecution()
{

  TCorrelationMetric * associate = dynamic_cast<TCorrelationMetric *>(this->m_Associate);

  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize = associate->GetNumberOfParameters();

  /* Store the number of valid points the enclosing class \c
   * m_NumberOfValidPoints by collecting the valid points per thread. */
  associate->m_NumberOfValidPoints = NumericTraits<SizeValueType>::Zero;
  for (ThreadIdType i = 0; i < this->GetNumberOfThreadsUsed(); i++)
    {
    associate->m_NumberOfValidPoints += this->m_NumberOfValidPointsPerThread[i];
    }

  itkDebugMacro("CorrelationImageToImageMetricv4: NumberOfValidPoints: " << associate->m_NumberOfValidPoints);

  /* Accumulate the metric value from threads and store */
  associate->m_Value = NumericTraits<InternalComputationValueType>::Zero;
  InternalComputationValueType fm = NumericTraits<InternalComputationValueType>::Zero;
  InternalComputationValueType f2 = NumericTraits<InternalComputationValueType>::Zero;
  InternalComputationValueType m2 = NumericTraits<InternalComputationValueType>::Zero;
  for (size_t i = 0; i < this->m_MeasurePerThread.size(); i++)
    {
    fm += this->m_InternalCumSumPerThread[i].fm;
    m2 += this->m_InternalCumSumPerThread[i].m2;
    f2 += this->m_InternalCumSumPerThread[i].f2;
    }

  if (m2 * f2 == 0)
    {
    itkDebugMacro( "CorrelationImageToImageMetricv4: m2 * f2 == 0");
    return;
    }

  associate->m_Value = -1.0 * fm * fm / (m2 * f2);

  /* For global transforms, compute the derivatives by combining values from each region. */
  DerivativeType fdm, mdm;
  fdm.SetSize(globalDerivativeSize);
  mdm.SetSize(globalDerivativeSize);

  fdm.Fill(NumericTraits<DerivativeValueType>::Zero);
  mdm.Fill(NumericTraits<DerivativeValueType>::Zero);

  for (ThreadIdType i = 0; i < this->GetNumberOfThreadsUsed(); i++)
    {
    fdm += this->m_InternalCumSumPerThread[i].fdm;
    mdm += this->m_InternalCumSumPerThread[i].mdm;
    }

  /** There should be a minus sign of \frac{d}{dp} mathematically, which
   *  is not in the implementation to match the requirement of the metricv4
   *  optimization framework.
   *
   *  We use += instead of assignment here because for multi-variate vector,
   *  we will want to always add to the values in m_DerivativeResult so they
   *  can be efficiently accumulated between multiple metrics.
   */
  *(associate->m_DerivativeResult) += 2.0 *fm/(f2*m2)*(fdm - fm/m2*mdm);
}

template<class TDomainPartitioner, class TImageToImageMetric, class TCorrelationMetric>
bool
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::ProcessPoint( const VirtualIndexType &,
                const VirtualPointType &           virtualPoint,
                const FixedImagePointType &,
                const FixedImagePixelType &        fixedImageValue,
                const FixedImageGradientType &,
                const MovingImagePointType &,
                const MovingImagePixelType &       movingImageValue,
                const MovingImageGradientType &    movingImageGradient,
                MeasureType &,
                DerivativeType &,
                const ThreadIdType                 threadID) const
{

  /*
   * metricValueReturn and localDerivativeReturn will not be computed here
   * instead, m_InternalCumSumPerThread will store temporary results for each thread
   * and finally compute metric and derivative in overloaded AfterThreadedExecution
   */

  TCorrelationMetric * associate = dynamic_cast<TCorrelationMetric *>(this->m_Associate);

  /* Use a pre-allocated jacobian object for efficiency */
  typedef typename TImageToImageMetric::JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_MovingTransformJacobianPerThread[threadID];

  /** For dense transforms, this returns identity */
  associate->GetMovingTransform()->ComputeJacobianWithRespectToParameters(virtualPoint, jacobian);

  InternalCumSumType & cumsum = this->m_InternalCumSumPerThread[threadID];

  /* substract the average of pixels (computed during InitializeIteration) */
  InternalComputationValueType f1 = fixedImageValue - associate->m_AverageFix;
  InternalComputationValueType m1 = movingImageValue - associate->m_AverageMov;

  cumsum.f += f1;
  cumsum.m += m1;
  cumsum.f2 += f1 * f1;
  cumsum.m2 += m1 * m1;
  cumsum.fm += f1 * m1;

  for (unsigned int par = 0; par < associate->GetNumberOfLocalParameters(); par++)
    {
    InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::Zero;
    for (SizeValueType dim = 0; dim < ImageToImageMetricv4Type::MovingImageDimension; dim++)
      {
      sum += movingImageGradient[dim] * jacobian(dim, par);
      }

    cumsum.fdm[par] += f1 * sum;
    cumsum.mdm[par] += m1 * sum;
    }

  /*
   * This skippes over the following code from the original ProcessVirtualPoint()
   *
   * ImageToImageMetricv4GetValueAndDerivativeThreaderBase::ProcessVirtualPoint
   * after ProcessPoint
   * if( pointIsValid )
     {
     this->m_NumberOfValidPointsPerThread[threadId]++;
     this->m_MeasurePerThread[threadId] += metricValueResult;
     this->StorePointDerivativeResult( virtualIndex, threadId );
     }
   *
   * For efficiency, call 'this->m_NumberOfValidPointsPerThread[threadId]++' , and then return false
   * from this method to skip the code mentioned above since it will do unnecessary operations.
   */
  this->m_NumberOfValidPointsPerThread[threadID]++;
  return false;
}

} // end namespace itk

#endif
