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
#ifndef __itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader_hxx
#define __itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader_hxx

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader.h"

namespace itk
{

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
void
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::ThreadedExecution( const DomainType & virtualImageSubRegion,
                     const ThreadIdType threadId )
{
  TNeighborhoodCorrelationMetric * associate = dynamic_cast< TNeighborhoodCorrelationMetric * >( this->m_Associate );

  VirtualPointType     virtualPoint;
  MeasureType          metricValueResult;
  MeasureType          metricValueSum = NumericTraits< MeasureType >::Zero;
  bool                 pointIsValid;
  ScanIteratorType     scanIt;
  ScanParametersType   scanParameters;
  ScanMemType          scanMem;

  DerivativeType & localDerivativeResult = this->m_LocalDerivativesPerThread[threadId];

  /* Create an iterator over the virtual sub region */
  associate->InitializeScanning( virtualImageSubRegion, scanIt, scanMem,
      scanParameters );
  /* Iterate over the sub region */
  scanIt.GoToBegin();
  while (!scanIt.IsAtEnd())
    {
    /* Get the virtual point */
    associate->m_VirtualDomainImage->TransformIndexToPhysicalPoint(
        scanIt.GetIndex(), virtualPoint);

    /* Call the user method in derived classes to do the specific
     * calculations for value and derivative. */
    try
      {
      this->UpdateQueues(scanIt, scanMem, scanParameters, threadId);
      pointIsValid = this->ComputeInformationFromQueues(scanIt,
          scanMem, scanParameters, threadId);
      if( pointIsValid )
        {
        this->ComputeMovingTransformDerivative(scanIt, scanMem,
            scanParameters, localDerivativeResult, metricValueResult,
            threadId );
        }
      }
    catch (ExceptionObject & exc)
      {
      //NOTE: there must be a cleaner way to do this:
      std::string msg("Caught exception: \n");
      msg += exc.what();
      ExceptionObject err(__FILE__, __LINE__, msg);
      throw err;
      }

    /* Assign the results */
    if ( pointIsValid )
      {
      this->m_NumberOfValidPointsPerThread[threadId]++;
      metricValueSum -= metricValueResult;
      /* Store the result. This depends on what type of
       * transform is being used. */
      this->StorePointDerivativeResult( scanIt.GetIndex(), threadId );
      }

    //next index
    ++scanIt;
    }

  /* Store metric value result for this thread. */
  this->m_MeasurePerThread[threadId] = metricValueSum;
}

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
void
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::UpdateQueuesAtBeginningOfLine(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters, const ThreadIdType ) const
{
  TNeighborhoodCorrelationMetric * associate = dynamic_cast< TNeighborhoodCorrelationMetric * >( this->m_Associate );

  const SizeValueType numberOfFillZero = scanParameters.numberOfFillZero;
  const SizeValueType hoodlen = scanParameters.windowLength;

  InternalComputationValueType zero
    = NumericTraits<InternalComputationValueType>::ZeroValue();
  scanMem.QsumFixed2 = SumQueueType(numberOfFillZero, zero);
  scanMem.QsumMoving2 = SumQueueType(numberOfFillZero, zero);
  scanMem.QsumFixed = SumQueueType(numberOfFillZero, zero);
  scanMem.QsumMoving = SumQueueType(numberOfFillZero, zero);
  scanMem.QsumFixedMoving = SumQueueType(numberOfFillZero, zero);
  scanMem.Qcount = SumQueueType(numberOfFillZero, zero);

  typedef InternalComputationValueType LocalRealType;

  // Now add the rest of the values from each hyperplane
  SizeValueType diameter = 2 * scanParameters.radius[0];

  const LocalRealType localZero = NumericTraits<LocalRealType>::ZeroValue();
  for (SizeValueType i = numberOfFillZero;
        i < ( diameter + NumericTraits<SizeValueType>::OneValue() ); i++)
    {
    LocalRealType sumFixed2      = localZero;
    LocalRealType sumMoving2     = localZero;
    LocalRealType sumFixed       = localZero;
    LocalRealType sumMoving      = localZero;
    LocalRealType sumFixedMoving = localZero;
    LocalRealType count = localZero;

    for ( SizeValueType indct = i; indct < hoodlen;
          indct += ( diameter + NumericTraits<SizeValueType>::OneValue() ) )
      {

      bool isInBounds = true;
      scanIt.GetPixel(indct, isInBounds);

      typename VirtualImageType::IndexType index = scanIt.GetIndex(indct);

      if (!isInBounds)
        {
        // std::cout << "DEBUG: error" << std::endl;
        continue;
        }

      VirtualPointType        virtualPoint;
      FixedImagePointType     mappedFixedPoint;
      FixedImagePixelType     fixedImageValue;
      FixedImageGradientType  fixedImageGradient;
      MovingImagePointType    mappedMovingPoint;
      MovingImagePixelType    movingImageValue;
      MovingImageGradientType movingImageGradient;
      bool pointIsValid;

      associate->m_VirtualDomainImage->TransformIndexToPhysicalPoint(index,
          virtualPoint);

      try
        {
        pointIsValid = associate->TransformAndEvaluateFixedPoint( index,
                          virtualPoint,
                          false/*compute gradient*/,
                          mappedFixedPoint,
                          fixedImageValue,
                          fixedImageGradient );
        if ( pointIsValid )
          {
          pointIsValid = associate->TransformAndEvaluateMovingPoint(index,
                            virtualPoint,
                            false/*compute gradient*/,
                            mappedMovingPoint,
                            movingImageValue,
                            movingImageGradient );
          }
        }
      catch (ExceptionObject & exc)
        {
        //NOTE: there must be a cleaner way to do this:
        std::string msg("Caught exception: \n");
        msg += exc.what();
        ExceptionObject err(__FILE__, __LINE__, msg);
        throw err;
        }


      if ( pointIsValid )
        {
        sumFixed2 += fixedImageValue  * fixedImageValue;
        sumMoving2 += movingImageValue * movingImageValue;
        sumFixed += fixedImageValue;
        sumMoving += movingImageValue;
        sumFixedMoving += fixedImageValue * movingImageValue;
        count += NumericTraits<LocalRealType>::OneValue();
        }
      }//for indct

    scanMem.QsumFixed2.push_back(sumFixed2);
    scanMem.QsumMoving2.push_back(sumMoving2);
    scanMem.QsumFixed.push_back(sumFixed);
    scanMem.QsumMoving.push_back(sumMoving);
    scanMem.QsumFixedMoving.push_back(sumFixedMoving);
    scanMem.Qcount.push_back(count);
    }
}

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
void
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::UpdateQueuesToNextScanWindow(
    const ScanIteratorType &scanIt, ScanMemType &scanMem,
    const ScanParametersType &scanParameters, const ThreadIdType ) const
{
  TNeighborhoodCorrelationMetric * associate = dynamic_cast< TNeighborhoodCorrelationMetric * >( this->m_Associate );

  const SizeValueType hoodlen = scanParameters.windowLength;

  typedef InternalComputationValueType LocalRealType;

  const LocalRealType localZero = NumericTraits<LocalRealType>::ZeroValue();

  LocalRealType sumFixed2      = localZero;
  LocalRealType sumMoving2     = localZero;
  LocalRealType sumFixed       = localZero;
  LocalRealType sumMoving      = localZero;
  LocalRealType sumFixedMoving = localZero;
  LocalRealType count          = localZero;

  SizeValueType diameter = 2 * scanParameters.radius[0];

  for ( SizeValueType indct = diameter; indct < hoodlen;
    indct += (diameter + NumericTraits<SizeValueType>::OneValue()))
    {
    bool isInBounds = true;

    scanIt.GetPixel(indct, isInBounds);
    typename VirtualImageType::IndexType index = scanIt.GetIndex(indct);

    if (!isInBounds)
    {
    continue;
    }

    VirtualPointType virtualPoint;
    FixedImagePointType mappedFixedPoint;
    FixedImagePixelType fixedImageValue;
    FixedImageGradientType fixedImageGradient;
    MovingImagePointType mappedMovingPoint;
    MovingImagePixelType movingImageValue;
    MovingImageGradientType movingImageGradient;
    bool pointIsValid;

    associate->m_VirtualDomainImage->TransformIndexToPhysicalPoint(index, virtualPoint);
    try
      {
      pointIsValid = associate->TransformAndEvaluateFixedPoint( index,
            virtualPoint,
            false/*compute gradient*/,
            mappedFixedPoint,
            fixedImageValue,
            fixedImageGradient );
      if (pointIsValid)
        {
        pointIsValid = associate->TransformAndEvaluateMovingPoint( index,
              virtualPoint,
             false/*compute gradient*/,
             mappedMovingPoint,
             movingImageValue,
             movingImageGradient );
        }
      }
    catch (ExceptionObject & exc)
      {
      //NOTE: there must be a cleaner way to do this:
      std::string msg("Caught exception: \n");
      msg += exc.what();
      ExceptionObject err(__FILE__, __LINE__, msg);
      throw err;
      }
    if ( pointIsValid )
      {
      sumFixed2 += fixedImageValue  * fixedImageValue;
      sumMoving2 += movingImageValue * movingImageValue;
      sumFixed += fixedImageValue;
      sumMoving += movingImageValue;
      sumFixedMoving += fixedImageValue * movingImageValue;
      count += NumericTraits<LocalRealType>::OneValue();
      }
    }
    scanMem.QsumFixed2.push_back(sumFixed2);
    scanMem.QsumMoving2.push_back(sumMoving2);
    scanMem.QsumFixed.push_back(sumFixed);
    scanMem.QsumMoving.push_back(sumMoving);
    scanMem.QsumFixedMoving.push_back(sumFixedMoving);
    scanMem.Qcount.push_back(count);

    scanMem.QsumFixed2.pop_front();
    scanMem.QsumMoving2.pop_front();
    scanMem.QsumFixed.pop_front();
    scanMem.QsumMoving.pop_front();
    scanMem.QsumFixedMoving.pop_front();
    scanMem.Qcount.pop_front();
}

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
void
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::UpdateQueues( const ScanIteratorType &scanIt, ScanMemType &scanMem,
                const ScanParametersType &scanParameters, const ThreadIdType threadId) const
{
  if (scanIt.GetIndex()[0] == scanParameters.scanRegionBeginIndexDim0 )
    {
    this->UpdateQueuesAtBeginningOfLine(scanIt, scanMem, scanParameters, threadId);
    }
  else
    {
    this->UpdateQueuesToNextScanWindow(scanIt, scanMem, scanParameters, threadId);
    }
}

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
bool
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::ComputeInformationFromQueues( const ScanIteratorType &scanIt, ScanMemType &scanMem, const ScanParametersType &, const ThreadIdType ) const
{
  TNeighborhoodCorrelationMetric * associate = dynamic_cast< TNeighborhoodCorrelationMetric * >( this->m_Associate );

  typedef InternalComputationValueType LocalRealType;

  const LocalRealType localZero = NumericTraits<LocalRealType>::ZeroValue();

  LocalRealType count = localZero;

  typename SumQueueType::iterator itcount = scanMem.Qcount.begin();
  while (itcount != scanMem.Qcount.end())
    {
    count += *itcount;
    ++itcount;
    }

  if (count <= localZero)
    {
    // no points available in the queue, perhaps out of image region
    return false;
    }

  // If there are values, we need to calculate the different quantities
  LocalRealType sumFixed2      = localZero;
  LocalRealType sumMoving2     = localZero;
  LocalRealType sumFixed       = localZero;
  LocalRealType sumMoving      = localZero;
  LocalRealType sumFixedMoving = localZero;
  typename SumQueueType::iterator itFixed2      = scanMem.QsumFixed2.begin();
  typename SumQueueType::iterator itMoving2     = scanMem.QsumMoving2.begin();
  typename SumQueueType::iterator itFixed       = scanMem.QsumFixed.begin();
  typename SumQueueType::iterator itMoving      = scanMem.QsumMoving.begin();
  typename SumQueueType::iterator itFixedMoving = scanMem.QsumFixedMoving.begin();

  while (itFixed2 != scanMem.QsumFixed2.end())
    {
    sumFixed2 += *itFixed2;
    sumMoving2 += *itMoving2;
    sumFixed += *itFixed;
    sumMoving += *itMoving;
    sumFixedMoving += *itFixedMoving;

    ++itFixed2;
    ++itMoving2;
    ++itFixed;
    ++itMoving;
    ++itFixedMoving;
    }

  LocalRealType fixedMean  = sumFixed  / count;
  LocalRealType movingMean = sumMoving / count;

  LocalRealType sFixedFixed = sumFixed2 - fixedMean * sumFixed - fixedMean * sumFixed
    + count * fixedMean * fixedMean;
  LocalRealType sMovingMoving = sumMoving2 - movingMean * sumMoving - movingMean * sumMoving
    + count * movingMean * movingMean;
  LocalRealType sFixedMoving = sumFixedMoving - movingMean * sumFixed - fixedMean * sumMoving
    + count * movingMean * fixedMean;

  typename VirtualImageType::IndexType oindex = scanIt.GetIndex();

  VirtualPointType        virtualPoint;
  FixedImagePointType    mappedFixedPoint;
  FixedImagePixelType     fixedImageValue;
  FixedImageGradientType  fixedImageGradient;
  MovingImagePointType   mappedMovingPoint;
  MovingImagePixelType    movingImageValue;
  MovingImageGradientType movingImageGradient;
  bool pointIsValid;

  associate->m_VirtualDomainImage->TransformIndexToPhysicalPoint(oindex, virtualPoint);

  try
    {
    pointIsValid = associate->TransformAndEvaluateFixedPoint( oindex,
            virtualPoint,
            true/*compute gradient*/,
            mappedFixedPoint,
            fixedImageValue,
            fixedImageGradient );
    if ( pointIsValid )
      {
      pointIsValid = associate->TransformAndEvaluateMovingPoint( oindex,
             virtualPoint,
             true/*compute gradient*/,
             mappedMovingPoint,
             movingImageValue,
             movingImageGradient );
      }
    }
  catch (ExceptionObject & exc)
    {
    //NOTE: there must be a cleaner way to do this:
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }

  if ( pointIsValid )
    {
    scanMem.fixedA        = fixedImageValue  - fixedMean; // scanParameters.I->GetPixel(oindex) - fixedMean;
    scanMem.movingA       = movingImageValue - movingMean; // scanParameters.J->GetPixel(oindex) - movingMean;
    scanMem.sFixedMoving  = sFixedMoving;
    scanMem.sFixedFixed   = sFixedFixed;
    scanMem.sMovingMoving = sMovingMoving;

    scanMem.fixedImageGradient  = fixedImageGradient;
    scanMem.movingImageGradient = movingImageGradient;

    scanMem.mappedFixedPoint  = mappedFixedPoint;
    scanMem.mappedMovingPoint = mappedMovingPoint;
    scanMem.virtualPoint      = virtualPoint;
    }

  return pointIsValid;
}

template < class TImageToImageMetric, class TNeighborhoodCorrelationMetric >
void
ANTSNeighborhoodCorrelationImageToImageMetricv4DenseGetValueAndDerivativeThreader< TImageToImageMetric, TNeighborhoodCorrelationMetric >
::ComputeMovingTransformDerivative(
  const ScanIteratorType &, ScanMemType &scanMem,
  const ScanParametersType &, DerivativeType &deriv,
  MeasureType &localCC, const ThreadIdType threadId) const
{
  MovingImageGradientType derivWRTImage;
  localCC = NumericTraits<MeasureType>::OneValue();

  typedef InternalComputationValueType LocalRealType;

  LocalRealType sFixedFixed   = scanMem.sFixedFixed;
  LocalRealType sMovingMoving = scanMem.sMovingMoving;
  LocalRealType sFixedMoving  = scanMem.sFixedMoving;
  LocalRealType fixedI        = scanMem.fixedA;
  LocalRealType movingI       = scanMem.movingA;

  const MovingImageGradientType movingImageGradient = scanMem.movingImageGradient;

  if (sFixedFixed == NumericTraits< LocalRealType >::Zero || sMovingMoving == NumericTraits< LocalRealType >::Zero )
    {
    deriv.Fill( NumericTraits< DerivativeValueType >::Zero );
    return;
    }

  for (ImageDimensionType qq = 0; qq < TImageToImageMetric::VirtualImageDimension; qq++)
    {
    derivWRTImage[qq] = 2.0 * sFixedMoving / (sFixedFixed * sMovingMoving) * (fixedI - sFixedMoving / sMovingMoving * movingI)
      * movingImageGradient[qq];
    }

  if ( fabs(sFixedFixed * sMovingMoving) > NumericTraits< LocalRealType >::Zero )
    {
    localCC = sFixedMoving * sFixedMoving / (sFixedFixed * sMovingMoving);
    }

  /* Use a pre-allocated jacobian object for efficiency */
  JacobianType & jacobian =
    this->m_MovingTransformJacobianPerThread[threadId];

  /** For dense transforms, this returns identity */
  this->m_Associate->GetMovingTransform()->ComputeJacobianWithRespectToParameters(
    scanMem.virtualPoint, jacobian);

  NumberOfParametersType numberOfLocalParameters = this->m_Associate->GetMovingTransform()->GetNumberOfLocalParameters();

  // this correction is necessary for consistent derivatives across N threads
  DerivativeValueType floatingPointCorrectionResolution = this->m_Associate->GetFloatingPointCorrectionResolution();
  for (NumberOfParametersType par = 0; par < numberOfLocalParameters; par++)
    {
    double sum = NumericTraits< double >::Zero;
    for (ImageDimensionType dim = 0; dim < TImageToImageMetric::MovingImageDimension; dim++)
      {
      sum += derivWRTImage[dim] * jacobian(dim, par);
      }
    intmax_t floatingPointCorrection_int = static_cast< intmax_t >( sum * floatingPointCorrectionResolution );
    deriv[par] = static_cast< DerivativeValueType >( floatingPointCorrection_int / floatingPointCorrectionResolution );
    }
  return;
}

} // end namespace itk

#endif
