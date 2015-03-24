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
#ifndef itkCorrelationImageToImageMetricv4HelperThreader_hxx
#define itkCorrelationImageToImageMetricv4HelperThreader_hxx

#include "itkCorrelationImageToImageMetricv4HelperThreader.h"

namespace itk
{

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
CorrelationImageToImageMetricv4HelperThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::CorrelationImageToImageMetricv4HelperThreader() :
  m_CorrelationMetricPerThreadVariables( ITK_NULLPTR ),
  m_CorrelationAssociate( ITK_NULLPTR )
{}


template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
CorrelationImageToImageMetricv4HelperThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::~CorrelationImageToImageMetricv4HelperThreader()
{
  delete[] m_CorrelationMetricPerThreadVariables;
}


template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
void
CorrelationImageToImageMetricv4HelperThreader< TDomainPartitioner, TImageToImageMetric, TCorrelationMetric>
::BeforeThreadedExecution()
{
  Superclass::BeforeThreadedExecution();

  /* Store the casted pointer to avoid dynamic casting in tight loops. */
  this->m_CorrelationAssociate = dynamic_cast<TCorrelationMetric *>(this->m_Associate);

  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  delete[] this->m_CorrelationMetricPerThreadVariables;
  this->m_CorrelationMetricPerThreadVariables = new AlignedCorrelationMetricPerThreadStruct[ numThreadsUsed ];

    //---------------------------------------------------------------
    // Set initial values.
  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    this->m_CorrelationMetricPerThreadVariables[i].FixSum = NumericTraits<InternalComputationValueType>::ZeroValue();
    this->m_CorrelationMetricPerThreadVariables[i].MovSum = NumericTraits<InternalComputationValueType>::ZeroValue();
    }

}

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
void
CorrelationImageToImageMetricv4HelperThreader<TDomainPartitioner,
    TImageToImageMetric, TCorrelationMetric>::AfterThreadedExecution()
{

  /* Store the number of valid points the enclosing class \c
   * m_NumberOfValidPoints by collecting the valid points per thread. */
  this->m_CorrelationAssociate->m_NumberOfValidPoints = NumericTraits<SizeValueType>::ZeroValue();

  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();

  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    this->m_CorrelationAssociate->m_NumberOfValidPoints += this->m_GetValueAndDerivativePerThreadVariables[i].NumberOfValidPoints;
    }

  if (this->m_CorrelationAssociate->m_NumberOfValidPoints <= 0 )
    {
    itkWarningMacro("collected only zero points");
    return;
    }

  InternalComputationValueType sumF = NumericTraits<InternalComputationValueType>::ZeroValue();
  InternalComputationValueType sumM = NumericTraits<InternalComputationValueType>::ZeroValue();

  for (ThreadIdType threadId = 0; threadId < numThreadsUsed; ++threadId)
    {
    sumF += this->m_CorrelationMetricPerThreadVariables[threadId].FixSum;
    sumM += this->m_CorrelationMetricPerThreadVariables[threadId].MovSum;
    }

  this->m_CorrelationAssociate->m_AverageFix = sumF / this->m_CorrelationAssociate->m_NumberOfValidPoints;
  this->m_CorrelationAssociate->m_AverageMov = sumM / this->m_CorrelationAssociate->m_NumberOfValidPoints;
}

template<typename TDomainPartitioner, typename TImageToImageMetric, typename TCorrelationMetric>
bool
CorrelationImageToImageMetricv4HelperThreader<TDomainPartitioner,
TImageToImageMetric, TCorrelationMetric>
::ProcessVirtualPoint( const VirtualIndexType & itkNotUsed(virtualIndex), const VirtualPointType & virtualPoint, const ThreadIdType threadId )
{
  FixedImagePointType         mappedFixedPoint;
  FixedImagePixelType         mappedFixedPixelValue;
  MovingImagePointType        mappedMovingPoint;
  MovingImagePixelType        mappedMovingPixelValue;
  bool                        pointIsValid = false;

  /* Transform the point into fixed and moving spaces, and evaluate.
   * Different behavior with pre-warping enabled is handled transparently.
   * Do this in a try block to catch exceptions and print more useful info
   * then we otherwise get when exceptions are caught in MultiThreader. */
  try
    {
    pointIsValid = this->m_CorrelationAssociate->TransformAndEvaluateFixedPoint( virtualPoint, mappedFixedPoint, mappedFixedPixelValue );
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

  /* Do the specific calculations for values */
  try
    {
    this->m_CorrelationMetricPerThreadVariables[threadId].FixSum += mappedFixedPixelValue;
    this->m_CorrelationMetricPerThreadVariables[threadId].MovSum += mappedMovingPixelValue;
    }
  catch( ExceptionObject & exc )
    {
    std::string msg("Exception in ProcessVirtualPoint:\n");
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

} // end namespace itk

#endif
