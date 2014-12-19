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
#ifndef itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_hxx
#define itkImageToImageMetricv4GetValueAndDerivativeThreaderBase_hxx

#include "itkImageToImageMetricv4GetValueAndDerivativeThreaderBase.h"
#include "itkNumericTraits.h"

namespace itk
{

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::ImageToImageMetricv4GetValueAndDerivativeThreaderBase():
  m_GetValueAndDerivativePerThreadVariables( ITK_NULLPTR ),
  m_CachedNumberOfParameters( 0 ),
  m_CachedNumberOfLocalParameters( 0 )
{
}

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::~ImageToImageMetricv4GetValueAndDerivativeThreaderBase()
{
  delete[] m_GetValueAndDerivativePerThreadVariables;
}

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::BeforeThreadedExecution()
{
  //---------------------------------------------------------------
  // Resize the per thread memory objects.
  //-----------------------------------------------------------------
  // Cache some values
  this->m_CachedNumberOfParameters      = this->m_Associate->GetNumberOfParameters();
  this->m_CachedNumberOfLocalParameters = this->m_Associate->GetNumberOfLocalParameters();

  /* Per-thread results */
  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  delete[] m_GetValueAndDerivativePerThreadVariables;
  this->m_GetValueAndDerivativePerThreadVariables = new AlignedGetValueAndDerivativePerThreadStruct[ numThreadsUsed ];

  if( this->m_Associate->GetComputeDerivative() )
    {
    for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
      {
      /* Allocate intermediary per-thread storage used to get results from
       * derived classes */
      this->m_GetValueAndDerivativePerThreadVariables[i].LocalDerivatives.SetSize( this->m_CachedNumberOfLocalParameters );
      this->m_GetValueAndDerivativePerThreadVariables[i].MovingTransformJacobian.SetSize(
        this->m_Associate->VirtualImageDimension, this->m_CachedNumberOfLocalParameters );
      this->m_GetValueAndDerivativePerThreadVariables[i].MovingTransformJacobianPositional.SetSize(
        this->m_Associate->VirtualImageDimension, this->m_Associate->VirtualImageDimension );
      if ( this->m_Associate->m_MovingTransform->GetTransformCategory() == MovingTransformType::DisplacementField )
        {
        /* For transforms with local support, e.g. displacement field,
         * use a single derivative container that's updated by region
         * in multiple threads.
         * Initialization to zero is done in main class. */
        itkDebugMacro( "ImageToImageMetricv4::Initialize: transform HAS local support\n" );
        /* Set each per-thread object to point to m_DerivativeResult for efficiency. */
        this->m_GetValueAndDerivativePerThreadVariables[i].Derivatives.SetData( this->m_Associate->m_DerivativeResult->data_block(),
          this->m_Associate->m_DerivativeResult->Size(), false );
        }
      else
        {
        itkDebugMacro("ImageToImageMetricv4::Initialize: transform does NOT have local support\n");
        /* This size always comes from the moving image */
        const NumberOfParametersType globalDerivativeSize = this->m_CachedNumberOfParameters;
        /* Global transforms get a separate derivatives container for each thread
         * that holds the result over a particular image region.
         * Use a CompensatedSummation value to provide for better consistency between
         * different number of threads. */
        this->m_GetValueAndDerivativePerThreadVariables[i].CompensatedDerivatives.resize( globalDerivativeSize );
        }
      }
    }

  //---------------------------------------------------------------
  // Set initial values.
  for (ThreadIdType thread = 0; thread < numThreadsUsed; ++thread)
    {
    this->m_GetValueAndDerivativePerThreadVariables[thread].NumberOfValidPoints = NumericTraits< SizeValueType >::ZeroValue();
    this->m_GetValueAndDerivativePerThreadVariables[thread].Measure = NumericTraits< InternalComputationValueType >::ZeroValue();
    if( this->m_Associate->GetComputeDerivative() )
      {
      if ( this->m_Associate->m_MovingTransform->GetTransformCategory() != MovingTransformType::DisplacementField )
        {
        /* Be sure to init to 0 here, because the threader may not use
         * all the threads if the region is better split into fewer
         * subregions. */
        for( NumberOfParametersType p = 0; p < this->m_CachedNumberOfParameters; p++ )
          {
          this->m_GetValueAndDerivativePerThreadVariables[thread].CompensatedDerivatives[p].ResetToZero();
          }
        }
      }
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::AfterThreadedExecution()
{
  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  /* Store the number of valid points the enclosing class \c
   * m_NumberOfValidPoints by collecting the valid points per thread. */
  this->m_Associate->m_NumberOfValidPoints = NumericTraits< SizeValueType >::ZeroValue();
  for (ThreadIdType i = 0; i < numThreadsUsed; ++i)
    {
    this->m_Associate->m_NumberOfValidPoints += this->m_GetValueAndDerivativePerThreadVariables[i].NumberOfValidPoints;
    }
  itkDebugMacro( "ImageToImageMetricv4: NumberOfValidPoints: " << this->m_Associate->m_NumberOfValidPoints );

  /* For global transforms, sum the derivatives from each region. */
  if( this->m_Associate->GetComputeDerivative() )
    {
    if ( this->m_Associate->m_MovingTransform->GetTransformCategory() != MovingTransformType::DisplacementField )
      {
      for (NumberOfParametersType p = 0; p < this->m_Associate->GetNumberOfParameters(); p++ )
        {
        /* Use a compensated sum to be ready for when there is a very large number of threads */
        CompensatedDerivativeValueType sum;
        sum.ResetToZero();
        for (ThreadIdType i=0; i<numThreadsUsed; i++)
          {
          sum += this->m_GetValueAndDerivativePerThreadVariables[i].CompensatedDerivatives[p].GetSum();
          }
        (*(this->m_Associate->m_DerivativeResult))[p] += sum.GetSum();
        }
      }
    }

  /* Check the number of valid points. If there aren't enough,
   * m_Value and m_DerivativeResult will get appropriate values assigned,
   * and a warning will be output. */
  if( this->m_Associate->VerifyNumberOfValidPoints( this->m_Associate->m_Value, *(this->m_Associate->m_DerivativeResult) ) )
    {
    this->m_Associate->m_Value = NumericTraits<MeasureType>::ZeroValue();
    /* Accumulate the metric value from threads and store the average. */
    for(ThreadIdType threadId = 0; threadId < numThreadsUsed; ++threadId )
      {
      this->m_Associate->m_Value += this->m_GetValueAndDerivativePerThreadVariables[threadId].Measure;
      }
    this->m_Associate->m_Value /= this->m_Associate->m_NumberOfValidPoints;

    /* For global transforms, calculate the average values */
    if( this->m_Associate->GetComputeDerivative() )
      {
      if ( this->m_Associate->m_MovingTransform->GetTransformCategory() != MovingTransformType::DisplacementField )
        {
        *(this->m_Associate->m_DerivativeResult) /= this->m_Associate->m_NumberOfValidPoints;
        }
      }
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
bool
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::ProcessVirtualPoint( const VirtualIndexType & virtualIndex,
                       const VirtualPointType & virtualPoint,
                       const ThreadIdType threadId )
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
   * Do this in a try block to catch exceptions and print more useful info
   * then we otherwise get when exceptions are caught in MultiThreader. */
  try
    {
    pointIsValid = this->m_Associate->TransformAndEvaluateFixedPoint( virtualPoint, mappedFixedPoint, mappedFixedPixelValue);
    if( pointIsValid &&
        this->m_Associate->GetComputeDerivative() &&
        this->m_Associate->GetGradientSourceIncludesFixed() )
      {
      this->m_Associate->ComputeFixedImageGradientAtPoint( mappedFixedPoint, mappedFixedImageGradient );
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
    pointIsValid = this->m_Associate->TransformAndEvaluateMovingPoint( virtualPoint, mappedMovingPoint, mappedMovingPixelValue );
    if( pointIsValid &&
        this->m_Associate->GetComputeDerivative() &&
        this->m_Associate->GetGradientSourceIncludesMoving() )
      {
      this->m_Associate->ComputeMovingImageGradientAtPoint( mappedMovingPoint, mappedMovingImageGradient );
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
                                   metricValueResult,
                                   this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives,
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
    this->m_GetValueAndDerivativePerThreadVariables[threadId].Measure += metricValueResult;
    if( this->m_Associate->GetComputeDerivative() )
      {
      this->StorePointDerivativeResult( virtualIndex, threadId );
      }
    }

  return pointIsValid;
}

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
void
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::StorePointDerivativeResult( const VirtualIndexType & virtualIndex, const ThreadIdType threadId )
{
  if ( this->m_Associate->m_MovingTransform->GetTransformCategory() != MovingTransformType::DisplacementField )
    {
    /* Global support */
    if ( this->m_Associate->GetUseFloatingPointCorrection() )
      {
      DerivativeValueType correctionResolution = this->m_Associate->GetFloatingPointCorrectionResolution();
      for (NumberOfParametersType p = 0; p < this->m_CachedNumberOfParameters; p++ )
        {
        intmax_t test = static_cast< intmax_t >( this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives[p] * correctionResolution );
        this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives[p] = static_cast<DerivativeValueType>( test / correctionResolution );
        }
      }
    for (NumberOfParametersType p = 0; p < this->m_CachedNumberOfParameters; p++ )
      {
      this->m_GetValueAndDerivativePerThreadVariables[threadId].CompensatedDerivatives[p] += this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives[p];
      }
    }
  else
    {
    // Update derivative at some index
    // this requires the moving image displacement field to be
    // same size as virtual image, and that VirtualImage PixelType
    // is scalar (which is verified during Metric initialization).
    try
      {
      OffsetValueType offset = this->m_Associate->ComputeParameterOffsetFromVirtualIndex( virtualIndex, this->m_CachedNumberOfLocalParameters );
      for (NumberOfParametersType i=0; i < this->m_CachedNumberOfLocalParameters; i++)
        {
        /* Be sure to *add* here and not assign. Required for proper behavior
         * with multi-variate metric. */
        this->m_GetValueAndDerivativePerThreadVariables[threadId].Derivatives[offset+i] += this->m_GetValueAndDerivativePerThreadVariables[threadId].LocalDerivatives[i];
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

template< typename TDomainPartitioner, typename TImageToImageMetricv4 >
bool
ImageToImageMetricv4GetValueAndDerivativeThreaderBase< TDomainPartitioner, TImageToImageMetricv4 >
::GetComputeDerivative() const
{
  return this->m_Associate->GetComputeDerivative();
}

} // end namespace itk

#endif
