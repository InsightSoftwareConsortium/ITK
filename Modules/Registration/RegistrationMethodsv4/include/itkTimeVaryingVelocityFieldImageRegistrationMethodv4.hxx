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
#ifndef itkTimeVaryingVelocityFieldImageRegistrationMethodv4_hxx
#define itkTimeVaryingVelocityFieldImageRegistrationMethodv4_hxx

#include "itkTimeVaryingVelocityFieldImageRegistrationMethodv4.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkResampleImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::TimeVaryingVelocityFieldImageRegistrationMethodv4() :
  m_LearningRate( 0.25 ),
  m_ConvergenceThreshold( 1.0e-7 ),
  m_ConvergenceWindowSize( 10 )
{
  this->m_NumberOfIterationsPerLevel.SetSize( 3 );
  this->m_NumberOfIterationsPerLevel[0] = 20;
  this->m_NumberOfIterationsPerLevel[1] = 30;
  this->m_NumberOfIterationsPerLevel[2] = 40;
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::~TimeVaryingVelocityFieldImageRegistrationMethodv4()
{
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::StartOptimization()
{
  typedef ImageDuplicator<DisplacementFieldType> DisplacementFieldDuplicatorType;
  typedef DisplacementFieldTransform<RealType, ImageDimension> DisplacementFieldTransformType;
  typename DisplacementFieldType::PixelType zeroVector;
  zeroVector.Fill( 0 );

  // This transform is used for the fixed image
  typedef itk::IdentityTransform<RealType, ImageDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  typename DisplacementFieldTransformType::Pointer identityDisplacementFieldTransform = DisplacementFieldTransformType::New();

  // This transform gets used for the moving image
  typename DisplacementFieldDuplicatorType::Pointer fieldDuplicatorIdentity = DisplacementFieldDuplicatorType::New();

  TimeVaryingVelocityFieldPointer velocityField = this->m_OutputTransform->GetModifiableVelocityField();
  IndexValueType numberOfTimePoints = velocityField->GetLargestPossibleRegion().GetSize()[ImageDimension];

  SizeValueType numberOfIntegrationSteps = numberOfTimePoints + 2;

  const typename TimeVaryingVelocityFieldType::RegionType & largestRegion = velocityField->GetLargestPossibleRegion();
  const SizeValueType numberOfPixelsPerTimePoint = largestRegion.GetNumberOfPixels() / numberOfTimePoints;

  const typename TimeVaryingVelocityFieldType::SpacingType velocityFieldSpacing = velocityField->GetSpacing();

  typename VirtualImageType::ConstPointer virtualDomainImage;
  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( metricQueue.IsNotNull() )
      {
      virtualDomainImage = metricQueue->GetVirtualImage();
      }
    else
      {
      itkExceptionMacro( "ERROR: Invalid conversion from the multi metric queue." );
      }
    }
  else
    {
    typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( metric.IsNotNull() )
      {
      virtualDomainImage = metric->GetVirtualImage();
      }
    else
      {
      itkExceptionMacro( "ERROR: Invalid metric conversion." );
      }
    }

  typedef typename ImageMetricType::DerivativeType MetricDerivativeType;
  const typename MetricDerivativeType::SizeValueType metricDerivativeSize = virtualDomainImage->GetLargestPossibleRegion().GetNumberOfPixels() * ImageDimension;
  MetricDerivativeType metricDerivative( metricDerivativeSize );

  // Warp the moving image based on the composite transform (not including the current
  // time varying velocity field transform to be optimized).

  // Instantiate the update derivative for all vectors of the velocity field
  DerivativeType updateDerivative( numberOfPixelsPerTimePoint * numberOfTimePoints * ImageDimension  );
  DerivativeType lastUpdateDerivative( numberOfPixelsPerTimePoint * numberOfTimePoints * ImageDimension  );
  lastUpdateDerivative.Fill( 0 );
  updateDerivative.Fill( 0 );

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  typename ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  // m_OutputTransform is the velocity field

  IterationReporter reporter( this, 0, 1 );

  while( this->m_CurrentIteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !this->m_IsConverged )
    {
    updateDerivative.Fill( 0 );
    MeasureType value = NumericTraits<MeasureType>::ZeroValue();
    this->m_CurrentMetricValue = NumericTraits<MeasureType>::ZeroValue();

    // Time index zero brings the moving image closest to the fixed image
    for( IndexValueType timePoint = 0; timePoint < numberOfTimePoints; timePoint++ )
      {
      RealType t = NumericTraits<RealType>::ZeroValue();
      if( numberOfTimePoints > 1 )
        {
        t = static_cast<RealType>( timePoint ) / static_cast<RealType>( numberOfTimePoints - 1 );
        }

      // Get the fixed transform.  We need to duplicate the resulting
      // displacement field since it will be overwritten when we integrate
      // the velocity field to get the moving image transform.
      if( timePoint == 0 )
        {
        this->m_OutputTransform->GetModifiableDisplacementField()->FillBuffer( zeroVector );
        }
      else
        {
        this->m_OutputTransform->SetLowerTimeBound( t );
        this->m_OutputTransform->SetUpperTimeBound( 0.0 );
        this->m_OutputTransform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
        this->m_OutputTransform->IntegrateVelocityField();
        }

      typename DisplacementFieldDuplicatorType::Pointer fieldDuplicator = DisplacementFieldDuplicatorType::New();
      fieldDuplicator->SetInputImage( this->m_OutputTransform->GetDisplacementField() );
      fieldDuplicator->Update();

      typename DisplacementFieldTransformType::Pointer fixedDisplacementFieldTransform = DisplacementFieldTransformType::New();
      fixedDisplacementFieldTransform->SetDisplacementField( fieldDuplicator->GetModifiableOutput() );

      // Get the moving transform
      if( timePoint == numberOfTimePoints - 1 )
        {
        this->m_OutputTransform->GetModifiableDisplacementField()->FillBuffer( zeroVector );
        }
      else
        {
        this->m_OutputTransform->SetLowerTimeBound( t );
        this->m_OutputTransform->SetUpperTimeBound( 1.0 );
        this->m_OutputTransform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
        this->m_OutputTransform->IntegrateVelocityField();
        }

      typename DisplacementFieldTransformType::Pointer movingDisplacementFieldTransform = DisplacementFieldTransformType::New();
      movingDisplacementFieldTransform->SetDisplacementField( this->m_OutputTransform->GetModifiableDisplacementField() );

      this->m_CompositeTransform->AddTransform( movingDisplacementFieldTransform );
      this->m_CompositeTransform->SetOnlyMostRecentTransformToOptimizeOn();
      if( timePoint == 0 && this->m_CurrentIteration <= 1 )
        {
        fieldDuplicatorIdentity->SetInputImage( movingDisplacementFieldTransform->GetDisplacementField() );
        fieldDuplicatorIdentity->Update();
        fieldDuplicatorIdentity->GetModifiableOutput()->FillBuffer( zeroVector );
        identityDisplacementFieldTransform->SetDisplacementField( fieldDuplicatorIdentity->GetModifiableOutput() );
        }

      for( unsigned int n = 0; n < this->m_MovingSmoothImages.size(); n++ )
        {
        typedef ResampleImageFilter<MovingImageType, VirtualImageType, RealType> MovingResamplerType;
        typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
        movingResampler->SetTransform( this->m_CompositeTransform );
        movingResampler->SetInput( this->m_MovingSmoothImages[n] );
        movingResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
        movingResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
        movingResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
        movingResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
        movingResampler->SetDefaultPixelValue( 0 );
        movingResampler->Update();

        typedef ResampleImageFilter<FixedImageType, VirtualImageType, RealType> FixedResamplerType;
        typename FixedResamplerType::Pointer fixedResampler = FixedResamplerType::New();
        fixedResampler->SetTransform( fixedDisplacementFieldTransform );
        fixedResampler->SetInput( this->m_FixedSmoothImages[n] );
        fixedResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
        fixedResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
        fixedResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
        fixedResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
        fixedResampler->SetDefaultPixelValue( 0 );
        fixedResampler->Update();

        if( multiMetric )
          {
          typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() );
          if( metricQueue.IsNotNull() )
            {
            metricQueue->SetFixedImage( fixedResampler->GetOutput() );
            metricQueue->SetMovingImage( movingResampler->GetOutput() );
            }
          else
            {
            itkExceptionMacro("ERROR: Invalid conversion from the multi metric queue.");
            }
          }
        else
          {
          dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedImage( fixedResampler->GetOutput() );
          dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetMovingImage( movingResampler->GetOutput() );
          }
        }

      if( multiMetric )
        {
        multiMetric->SetFixedTransform( identityTransform );
        multiMetric->SetMovingTransform( identityDisplacementFieldTransform );
        }
      else
        {
        dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( identityTransform );
        dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( identityDisplacementFieldTransform );
        }
      this->m_Metric->Initialize();

      metricDerivative.Fill( NumericTraits<typename MetricDerivativeType::ValueType>::ZeroValue() );
      this->m_Metric->GetValueAndDerivative( value, metricDerivative );

      // Ensure that the size of the optimizer weights is the same as the
      // number of local transform parameters (=ImageDimension)
      if( !this->m_OptimizerWeightsAreIdentity && this->m_OptimizerWeights.Size() == ImageDimension )
        {
        typename MetricDerivativeType::iterator it;
        for( it = metricDerivative.begin(); it != metricDerivative.end(); it += ImageDimension )
          {
          for( unsigned int d = 0; d < ImageDimension; d++ )
            {
            *(it + d) *= this->m_OptimizerWeights[d];
            }
          }
        }

      // Note: we are intentionally ignoring the jacobian determinant.
      // It does not change the direction of the optimization, only
      // the scaling.  It is very expensive to compute it accurately.

      this->m_CurrentMetricValue += value;

      // Remove the temporary mapping along the geodesic
      this->m_CompositeTransform->RemoveTransform();

      // we rescale the update velocity field at each time point.
      // we first need to convert to a displacement field to look
      // at the max norm of the field.

      const bool importFilterWillReleaseMemory = false;
      DisplacementVectorType *metricDerivativeFieldPointer = reinterpret_cast<DisplacementVectorType *>( metricDerivative.data_block() );

      typedef ImportImageFilter<DisplacementVectorType, ImageDimension> ImporterType;
      typename ImporterType::Pointer importer = ImporterType::New();
      importer->SetImportPointer( metricDerivativeFieldPointer, numberOfPixelsPerTimePoint, importFilterWillReleaseMemory );
      importer->SetRegion( virtualDomainImage->GetBufferedRegion() );
      importer->SetOrigin( virtualDomainImage->GetOrigin() );
      importer->SetSpacing( virtualDomainImage->GetSpacing() );
      importer->SetDirection( virtualDomainImage->GetDirection() );
      importer->Update();

      typedef Image<RealType, ImageDimension> MagnitudeImageType;

      typedef VectorMagnitudeImageFilter<DisplacementFieldType, MagnitudeImageType> MagnituderType;
      typename MagnituderType::Pointer magnituder = MagnituderType::New();
      magnituder->SetInput( importer->GetOutput() );
      magnituder->Update();

      typedef StatisticsImageFilter<MagnitudeImageType> StatisticsImageFilterType;
      typename StatisticsImageFilterType::Pointer stats = StatisticsImageFilterType::New();
      stats->SetInput( magnituder->GetOutput() );
      stats->Update();

      RealType maxNorm = stats->GetMaximum();
      if( maxNorm <= 0.0 )
        {
        maxNorm = 1.0;
        }

      RealType scale = 1.0 / maxNorm;
      metricDerivative *= scale;
      updateDerivative.update( metricDerivative, timePoint * numberOfPixelsPerTimePoint * ImageDimension );
      } // end loop over time points

    // update the transform --- averaging with the last update reduces oscillations
    updateDerivative = ( updateDerivative + lastUpdateDerivative ) * 0.5;
    lastUpdateDerivative = updateDerivative;
    this->m_OutputTransform->UpdateTransformParameters( updateDerivative, this->m_LearningRate );

    this->m_CurrentMetricValue /= static_cast<MeasureType>( numberOfTimePoints );

    convergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
    this->m_CurrentConvergenceValue = convergenceMonitoring->GetConvergenceValue();

    if( this->m_CurrentConvergenceValue < this->m_ConvergenceThreshold )
      {
      this->m_IsConverged = true;
      }

    if( this->m_IsConverged || this->m_CurrentIteration >= this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] )
      {

      // Once we finish by convergence or exceeding number of iterations,
      // we need to reset the transform by resetting the time bounds to the
      // full range [0,1] and integrating the velocity field to get the
      // forward and inverse displacement fields.

      this->m_OutputTransform->SetLowerTimeBound( 0 );
      this->m_OutputTransform->SetUpperTimeBound( 1.0 );
      this->m_OutputTransform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
      this->m_OutputTransform->IntegrateVelocityField();

      if( this->GetDebug() )
        {
        RealType spatialNorm = NumericTraits<RealType>::ZeroValue();
        RealType spatioTemporalNorm = NumericTraits<RealType>::ZeroValue();

        typename TimeVaryingVelocityFieldType::SizeType radius;
        radius.Fill( 1 );

        typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TimeVaryingVelocityFieldType> FaceCalculatorType;
        FaceCalculatorType faceCalculator;
        typename FaceCalculatorType::FaceListType faceList = faceCalculator( velocityField, velocityField->GetLargestPossibleRegion(), radius );

        // We only iterate over the first element of the face list since
        // that contains only the interior region.

        ConstNeighborhoodIterator<TimeVaryingVelocityFieldType> ItV( radius, velocityField, faceList.front() );
        for( ItV.GoToBegin(); !ItV.IsAtEnd(); ++ItV )
          {
          RealType localSpatialNorm = NumericTraits<RealType>::ZeroValue();
          RealType localSpatioTemporalNorm = NumericTraits<RealType>::ZeroValue();
          for( unsigned int d = 0; d < ImageDimension + 1; d++ )
            {
            DisplacementVectorType vector =  ( ItV.GetNext( d ) - ItV.GetPrevious( d ) ) * 0.5 * velocityFieldSpacing[d];
            RealType vectorNorm = vector.GetNorm();
            localSpatioTemporalNorm += vectorNorm;
            if( d < ImageDimension )
              {
              localSpatialNorm += vectorNorm;
              }
            }
          spatialNorm += ( localSpatialNorm / static_cast<RealType>( ImageDimension + 1 ) );
          spatioTemporalNorm += ( localSpatioTemporalNorm / static_cast<RealType>( ImageDimension + 1 ) );
          }
        spatialNorm /= static_cast<RealType>( ( velocityField->GetLargestPossibleRegion() ).GetNumberOfPixels() );
        spatioTemporalNorm /= static_cast<RealType>( ( velocityField->GetLargestPossibleRegion() ).GetNumberOfPixels() );
        itkDebugMacro( "    spatio-temporal velocity field norm : " << spatioTemporalNorm << ", spatial velocity field norm: " << spatialNorm );
        }
      }
    reporter.CompletedStep();
    }
}

/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::GenerateData()
{

  this->AllocateOutputs();

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );

    // The base class adds the transform to be optimized at initialization.
    // However, since this class handles its own optimization, we remove it
    // to optimize separately.  We then add it after the optimization loop.

    this->m_CompositeTransform->RemoveTransform();

    this->StartOptimization();

    this->m_CompositeTransform->AddTransform( this->m_OutputTransform );
    }

  this->GetTransformOutput()->Set(this->m_OutputTransform);
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Number of levels: " << this->m_NumberOfLevels << std::endl;
  os << indent << "Smoothing sigmas: " << this->m_SmoothingSigmasPerLevel << std::endl;
  os << indent << "Number of iterations: " << this->m_NumberOfIterationsPerLevel << std::endl;
  os << indent << "Convergence threshold: " << this->m_ConvergenceThreshold << std::endl;
  os << indent << "Convergence window size: " << this->m_ConvergenceWindowSize << std::endl;
  os << indent << "Learning rate: " << this->m_LearningRate << std::endl;
}

} // end namespace itk

#endif
