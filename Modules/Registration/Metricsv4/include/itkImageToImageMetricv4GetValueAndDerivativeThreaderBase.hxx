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
#ifndef __itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_hxx
#define __itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_hxx

#include "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase.h"
#include "itkNumericTraits.h"

namespace itk
{

template< class TDomainPartitioner, class TImageToImageMetricv4 >
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::ImageToImageMetricv4GetValueAndDerivativeThreaderBase()
{
}

template< class TDomainPartitioner, class TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::BeforeThreadedExecution()
{
  //---------------------------------------------------------------
  // Resize the per thread memory objects.

  /* Per-thread results */
  this->m_MeasurePerThread.resize( this->GetNumberOfThreadsUsed() );
  this->m_NumberOfValidPointsPerThread.resize( this->GetNumberOfThreadsUsed() );
  this->m_DerivativesPerThread.resize( this->GetNumberOfThreadsUsed() );
  /* This one is intermediary, for getting per-point results. */
  this->m_LocalDerivativesPerThread.resize( this->GetNumberOfThreadsUsed() );
  /* Per-thread pre-allocated Jacobian objects for efficiency */
  this->m_MovingTransformJacobianPerThread.resize( this->GetNumberOfThreadsUsed() );

  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize =
    this->m_Associate->m_MovingTransform->GetNumberOfParameters();

  for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
    {
    /* Allocate intermediary per-thread storage used to get results from
     * derived classes */
    this->m_LocalDerivativesPerThread[i].SetSize(
                                          this->m_Associate->GetNumberOfLocalParameters() );
    this->m_MovingTransformJacobianPerThread[i].SetSize(
                                          this->m_Associate->VirtualImageDimension,
                                          this->m_Associate->GetNumberOfLocalParameters() );
    if ( this->m_Associate->m_MovingTransform->HasLocalSupport() )
      {
      /* For transforms with local support, e.g. displacement field,
       * use a single derivative container that's updated by region
       * in multiple threads.
       * Initialization to zero is done in main class. */
      itkDebugMacro(
        "ImageToImageMetricv4::Initialize: transform HAS local support\n");
        /* Set each per-thread object to point to m_DerivativeResul. */
        this->m_DerivativesPerThread[i].SetData(
                                      this->m_Associate->m_DerivativeResult->data_block(),
                                      this->m_Associate->m_DerivativeResult->Size(),
                                      false );
      }
    else
      {
      itkDebugMacro(
      "ImageToImageMetricv4::Initialize: transform does NOT have local support\n");
      /* Global transforms get a separate derivatives container for each thread
       * that holds the result over a particular image region. */
        this->m_DerivativesPerThread[i].SetSize( globalDerivativeSize );
      }
    }

  //---------------------------------------------------------------
  // Set initial values.
  for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
    {
    this->m_NumberOfValidPointsPerThread[i] = NumericTraits< SizeValueType >::Zero;
    this->m_MeasurePerThread[i] = NumericTraits< InternalComputationValueType >::Zero;
    if ( ! this->m_Associate->m_MovingTransform->HasLocalSupport() )
      {
      /* Be sure to init to 0 here, because the threader may not use
       * all the threads if the region is better split into fewer
       * subregions. */
      this->m_DerivativesPerThread[i].Fill( NumericTraits< DerivativeValueType >::Zero );
      }
    }
}

template< class TDomainPartitioner, class TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::AfterThreadedExecution()
{
  /* Store the number of valid points the enclosing class \c
   * m_NumberOfValidPoints by collecting the valid points per thread. */
  this->m_Associate->m_NumberOfValidPoints = NumericTraits< SizeValueType >::Zero;
  for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
    {
    this->m_Associate->m_NumberOfValidPoints += this->m_NumberOfValidPointsPerThread[i];
    }
  itkDebugMacro( "ImageToImageMetricv4: NumberOfValidPoints: "
                 << this->m_Associate->m_NumberOfValidPoints );

  /* For global transforms, sum the derivatives from each region. */
  if ( ! this->m_Associate->m_MovingTransform->HasLocalSupport() )
    {
    for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
      {
      *(this->m_Associate->m_DerivativeResult) += this->m_DerivativesPerThread[i];
      }
    }

  /* Check the number of valid points. If there aren't enough,
   * m_Value and m_DerivativeResult will get appropriate values assigned,
   * and a warning will be output. */
  if( this->m_Associate->VerifyNumberOfValidPoints( this->m_Associate->m_Value, *(this->m_Associate->m_DerivativeResult) ) )
    {
    this->m_Associate->m_Value = NumericTraits<MeasureType>::Zero;
    /* Accumulate the metric value from threads and store the average. */
    for(size_t i=0; i< this->m_MeasurePerThread.size(); i++)
      {
      this->m_Associate->m_Value += this->m_MeasurePerThread[i];
      }
    this->m_Associate->m_Value /= this->m_Associate->m_NumberOfValidPoints;

    /* For global transforms, calculate the average values */
    if ( ! this->m_Associate->m_MovingTransform->HasLocalSupport() )
      {
      *(this->m_Associate->m_DerivativeResult) /= this->m_Associate->m_NumberOfValidPoints;
      }
    }
}

template< class TDomainPartitioner, class TImageToImageMetricv4 >
bool
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::ProcessVirtualPoint( const VirtualIndexType & virtualIndex,
                       const VirtualPointType & virtualPoint,
                       const ThreadIdType threadId )
{
  FixedOutputPointType        mappedFixedPoint;
  FixedImagePixelType         mappedFixedPixelValue;
  FixedImageGradientType      mappedFixedImageGradient;
  MovingOutputPointType       mappedMovingPoint;
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
    pointIsValid = this->m_Associate->TransformAndEvaluateFixedPoint( virtualIndex,
                                      virtualPoint,
                                      this->m_Associate->GetGradientSourceIncludesFixed(),
                                      mappedFixedPoint,
                                      mappedFixedPixelValue,
                                      mappedFixedImageGradient );
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
    pointIsValid = this->m_Associate->TransformAndEvaluateMovingPoint( virtualIndex,
                                    virtualPoint,
                                    this->m_Associate->GetGradientSourceIncludesMoving(),
                                    mappedMovingPoint,
                                    mappedMovingPixelValue,
                                    mappedMovingImageGradient );
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
                                   metricValueResult, this->m_LocalDerivativesPerThread[threadId],
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
    this->m_NumberOfValidPointsPerThread[threadId]++;
    this->m_MeasurePerThread[threadId] += metricValueResult;
    this->StorePointDerivativeResult( virtualIndex, threadId );
    }

  return pointIsValid;
}

template< class TDomainPartitioner, class TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::StorePointDerivativeResult( const VirtualIndexType & virtualIndex,
                              const ThreadIdType threadId )
{
  if ( ! this->m_Associate->m_MovingTransform->HasLocalSupport() )
    {
    this->m_DerivativesPerThread[threadId] += this->m_LocalDerivativesPerThread[threadId];
    }
  else
    {
    // Update derivative at some index
    // this requires the moving image displacement field to be
    // same size as virtual image, and that VirtualImage PixelType
    // is scalar.
    try
      {
      OffsetValueType offset =
        this->m_Associate->ComputeParameterOffsetFromVirtualDomainIndex( virtualIndex, this->m_Associate->m_MovingTransform->GetNumberOfLocalParameters() );
      for (NumberOfParametersType i=0;
            i < this->m_Associate->m_MovingTransform->GetNumberOfLocalParameters(); i++)
        {
        /* Be sure to *add* here and not assign. Required for proper behavior
         * with multi-variate metric. */
        this->m_DerivativesPerThread[threadId][offset+i] += this->m_LocalDerivativesPerThread[threadId][i];
        }
      }
    catch( ExceptionObject & exc )
      {
      std::string msg("Caught exception: \n");
      msg += exc.what();
      ExceptionObject err(__FILE__, __LINE__, msg);
      throw err;
      }
    }
}

} // end namespace itk

#endif
