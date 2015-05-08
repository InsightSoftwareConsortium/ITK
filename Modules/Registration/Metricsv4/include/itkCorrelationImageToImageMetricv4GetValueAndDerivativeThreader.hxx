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
#ifndef itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkCorrelationImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::CorrelationImageToImageMetricv4GetValueAndDerivativeThreader() :
  m_CorrelationMetricValueDerivativePerThreadVariables( ITK_NULLPTR ),
  m_CorrelationAssociate( ITK_NULLPTR )
{}


template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::~CorrelationImageToImageMetricv4GetValueAndDerivativeThreader()
{
  delete[] m_CorrelationMetricValueDerivativePerThreadVariables;
}


template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
void
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::BeforeThreadedExecution()
{
  Superclass::BeforeThreadedExecution();

  /* Store the casted pointer to avoid dynamic casting in tight loops. */
  this->m_CorrelationAssociate = dynamic_cast<TCorrelationMetric *>(this->m_Associate);
  if( this->m_CorrelationAssociate == ITK_NULLPTR )
    {
    itkExceptionMacro("Dynamic casting of associate pointer failed.");
    }

  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize = this->GetCachedNumberOfParameters();

  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  // set size
  delete[] m_CorrelationMetricValueDerivativePerThreadVariables;
  m_CorrelationMetricValueDerivativePerThreadVariables = new AlignedCorrelationMetricValueDerivativePerThreadStruct[ numThreadsUsed ];
  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    this->m_CorrelationMetricValueDerivativePerThreadVariables[i].fdm.SetSize(globalDerivativeSize);
    this->m_CorrelationMetricValueDerivativePerThreadVariables[i].mdm.SetSize(globalDerivativeSize);
    }

  //---------------------------------------------------------------
  // Set initial values.
  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    m_CorrelationMetricValueDerivativePerThreadVariables[i].fm = NumericTraits<InternalComputationValueType>::ZeroValue();
    m_CorrelationMetricValueDerivativePerThreadVariables[i].f2 = NumericTraits<InternalComputationValueType>::ZeroValue();
    m_CorrelationMetricValueDerivativePerThreadVariables[i].m2 = NumericTraits<InternalComputationValueType>::ZeroValue();
    m_CorrelationMetricValueDerivativePerThreadVariables[i].f = NumericTraits<InternalComputationValueType>::ZeroValue();
    m_CorrelationMetricValueDerivativePerThreadVariables[i].m = NumericTraits<InternalComputationValueType>::ZeroValue();

    this->m_CorrelationMetricValueDerivativePerThreadVariables[i].mdm.Fill(NumericTraits<DerivativeValueType>::ZeroValue());
    this->m_CorrelationMetricValueDerivativePerThreadVariables[i].fdm.Fill(NumericTraits<DerivativeValueType>::ZeroValue());
    }

}

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
void
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::AfterThreadedExecution()
{

  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize = this->GetCachedNumberOfParameters();
  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();

  /* Store the number of valid points the enclosing class \c
   * m_NumberOfValidPoints by collecting the valid points per thread. */
  this->m_CorrelationAssociate->m_NumberOfValidPoints = NumericTraits<SizeValueType>::ZeroValue();
  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    this->m_CorrelationAssociate->m_NumberOfValidPoints += this->m_GetValueAndDerivativePerThreadVariables[i].NumberOfValidPoints;
    }

  /* Check the number of valid points meets the default minimum.
   * If not, parameters will hold default return values for this case */
  if( ! this->m_CorrelationAssociate->VerifyNumberOfValidPoints( this->m_CorrelationAssociate->m_Value, *(this->m_CorrelationAssociate->m_DerivativeResult) ) )
    {
    return;
    }

  itkDebugMacro("CorrelationImageToImageMetricv4: NumberOfValidPoints: " << this->m_CorrelationAssociate->m_NumberOfValidPoints);

  /* Accumulate the metric value from threads and store */
  this->m_CorrelationAssociate->m_Value = NumericTraits<InternalComputationValueType>::ZeroValue();
  InternalComputationValueType fm = NumericTraits<InternalComputationValueType>::ZeroValue();
  InternalComputationValueType f2 = NumericTraits<InternalComputationValueType>::ZeroValue();
  InternalComputationValueType m2 = NumericTraits<InternalComputationValueType>::ZeroValue();
  for (ThreadIdType threadId = 0; threadId < numThreadsUsed; ++threadId)
    {
    fm += this->m_CorrelationMetricValueDerivativePerThreadVariables[threadId].fm;
    m2 += this->m_CorrelationMetricValueDerivativePerThreadVariables[threadId].m2;
    f2 += this->m_CorrelationMetricValueDerivativePerThreadVariables[threadId].f2;
    }

  InternalComputationValueType m2f2 = m2 * f2;
  if ( m2f2 <= NumericTraits<InternalComputationValueType>::epsilon() )
    {
    itkDebugMacro( "CorrelationImageToImageMetricv4: m2 * f2 <= epsilon");
    return;
    }

  this->m_CorrelationAssociate->m_Value = -1.0 * fm * fm / (m2f2);

  /* For global transforms, compute the derivatives by combining values from each region. */
  if( this->m_CorrelationAssociate->GetComputeDerivative() )
    {
    DerivativeType fdm, mdm;
    fdm.SetSize(globalDerivativeSize);
    mdm.SetSize(globalDerivativeSize);

    fdm.Fill(NumericTraits<DerivativeValueType>::ZeroValue());
    mdm.Fill(NumericTraits<DerivativeValueType>::ZeroValue());

    const InternalComputationValueType fc = static_cast<InternalComputationValueType>( 2.0 );

    for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
      {
      fdm += this->m_CorrelationMetricValueDerivativePerThreadVariables[i].fdm;
      mdm += this->m_CorrelationMetricValueDerivativePerThreadVariables[i].mdm;
      }

    /** There should be a minus sign of \frac{d}{dp} mathematically, which
     *  is not in the implementation to match the requirement of the metricv4
     *  optimization framework.
     *
     *  We use += instead of assignment here because for multi-variate vector,
     *  we will want to always add to the values in m_DerivativeResult so they
     *  can be efficiently accumulated between multiple metrics.
     */
    *(this->m_CorrelationAssociate->m_DerivativeResult) += fc *fm/(f2*m2)*(fdm - fm/m2*mdm);
    }

}

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
bool
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::ProcessVirtualPoint( const VirtualIndexType & virtualIndex, const VirtualPointType & virtualPoint, const ThreadIdType threadId )
{
  FixedImagePointType         mappedFixedPoint;
  FixedImagePixelType         mappedFixedPixelValue;
  FixedImageGradientType      mappedFixedImageGradient;
  MovingImagePointType        mappedMovingPoint;
  MovingImagePixelType        mappedMovingPixelValue;
  MovingImageGradientType     mappedMovingImageGradient;
  bool                        pointIsValid = false;
  MeasureType                 metricValueResult;

  /* Transform the point into fixed and moving spaces, and evaluate.
   * Different behavior with pre-warping enabled is handled transparently.
   * Do this in a try block to catch exceptions and print more useful info
   * then we otherwise get when exceptions are caught in MultiThreader. */
  try
    {
    pointIsValid = this->m_CorrelationAssociate->TransformAndEvaluateFixedPoint( virtualPoint, mappedFixedPoint, mappedFixedPixelValue );
    if( pointIsValid &&
        this->m_CorrelationAssociate->GetComputeDerivative() &&
        this->m_CorrelationAssociate->GetGradientSourceIncludesFixed() )
      {
      this->m_CorrelationAssociate->ComputeFixedImageGradientAtPoint( mappedFixedPoint, mappedFixedImageGradient );
      }
    }
  catch( ExceptionObject & exc )
    {
    //NOTE: there must be a cleaner way to do this:
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }
  if( !pointIsValid )
    {
    return pointIsValid;
    }

  try
    {
    pointIsValid = this->m_CorrelationAssociate->TransformAndEvaluateMovingPoint( virtualPoint, mappedMovingPoint, mappedMovingPixelValue );
    if( pointIsValid &&
        this->m_CorrelationAssociate->GetComputeDerivative() &&
        this->m_CorrelationAssociate->GetGradientSourceIncludesMoving() )
      {
      this->m_CorrelationAssociate->ComputeMovingImageGradientAtPoint( mappedMovingPoint, mappedMovingImageGradient );
      }
    }
  catch( ExceptionObject & exc )
    {
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }
  if( !pointIsValid )
    {
    return pointIsValid;
    }

  /* Call the user method in derived classes to do the specific
   * calculations for value and derivative. */
  try
    {
    pointIsValid = this->ProcessPoint(
                                   virtualIndex,
                                   virtualPoint,
                                   mappedFixedPoint, mappedFixedPixelValue,
                                   mappedFixedImageGradient,
                                   mappedMovingPoint, mappedMovingPixelValue,
                                   mappedMovingImageGradient,
                                   metricValueResult, this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives,
                                   threadId );
    }
  catch( ExceptionObject & exc )
    {
    //NOTE: there must be a cleaner way to do this:
    std::string msg("Exception in GetValueAndDerivativeProcessPoint:\n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }
  if( pointIsValid )
    {
    this->m_GetValueAndDerivativePerThreadVariables[threadId].NumberOfValidPoints++;
    }

  return pointIsValid;
}

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
bool
CorrelationImageToImageMetricv4GetValueAndDerivativeThreader<TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::ProcessPoint( const VirtualIndexType &           itkNotUsed(virtualIndex),
                const VirtualPointType &           virtualPoint,
                const FixedImagePointType &        itkNotUsed(mappedFixedPoint),
                const FixedImagePixelType &        fixedImageValue,
                const FixedImageGradientType &     itkNotUsed(mappedFixedImageGradient),
                const MovingImagePointType &       itkNotUsed(mappedMovingPoint),
                const MovingImagePixelType &       movingImageValue,
                const MovingImageGradientType &    movingImageGradient,
                MeasureType &                      itkNotUsed(metricValueReturn),
                DerivativeType &                   itkNotUsed(localDerivativeReturn),
                const ThreadIdType                 threadId) const
{

  /*
   * metricValueReturn and localDerivativeReturn will not be computed here.
   * Instead, m_CorrelationMetricValueDerivativePerThreadVariables will store temporary results for each thread
   * and finally compute metric and derivative in overloaded AfterThreadedExecution
   */

  /* subtract the average of pixels (computed during InitializeIteration) */
  const InternalComputationValueType & f1 = fixedImageValue - this->m_CorrelationAssociate->m_AverageFix;
  const InternalComputationValueType & m1 = movingImageValue - this->m_CorrelationAssociate->m_AverageMov;

  AlignedCorrelationMetricValueDerivativePerThreadStruct & cumsum = this->m_CorrelationMetricValueDerivativePerThreadVariables[threadId];
  cumsum.f += f1;
  cumsum.m += m1;
  cumsum.f2 += f1 * f1;
  cumsum.m2 += m1 * m1;
  cumsum.fm += f1 * m1;

  if( this->m_CorrelationAssociate->GetComputeDerivative() )
    {
    /* Use a pre-allocated jacobian object for efficiency */
    typedef typename TImageToImageMetric::JacobianType & JacobianReferenceType;
    JacobianReferenceType jacobian = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobian;
    JacobianReferenceType jacobianPositional = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobianPositional;

    /** For dense transforms, this returns identity */
    this->m_CorrelationAssociate->GetMovingTransform()->
      ComputeJacobianWithRespectToParametersCachedTemporaries(virtualPoint,
                                                              jacobian,
                                                              jacobianPositional);

    for (unsigned int par = 0; par < this->m_CorrelationAssociate->GetNumberOfLocalParameters(); par++)
      {
      InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::ZeroValue();
      for (SizeValueType dim = 0; dim < ImageToImageMetricv4Type::MovingImageDimension; dim++)
        {
        sum += movingImageGradient[dim] * jacobian(dim, par);
        }

      cumsum.fdm[par] += f1 * sum;
      cumsum.mdm[par] += m1 * sum;
      }
    }

  return true;
}

} // end namespace itk

#endif
