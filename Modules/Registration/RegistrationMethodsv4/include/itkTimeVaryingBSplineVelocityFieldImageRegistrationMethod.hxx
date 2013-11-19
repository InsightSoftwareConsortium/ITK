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
#ifndef __itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_hxx
#define __itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_hxx

#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkPointSet.h"
#include "itkResampleImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::TimeVaryingBSplineVelocityFieldImageRegistrationMethod() :
  m_LearningRate( 0.25 ),
  m_ConvergenceThreshold( 1.0e-7 ),
  m_ConvergenceWindowSize( 10 ),
  m_NumberOfTimePointSamples( 4 )
{
  this->m_NumberOfIterationsPerLevel.SetSize( 3 );
  this->m_NumberOfIterationsPerLevel[0] = 20;
  this->m_NumberOfIterationsPerLevel[1] = 30;
  this->m_NumberOfIterationsPerLevel[2] = 40;
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::~TimeVaryingBSplineVelocityFieldImageRegistrationMethod()
{
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
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

  TimeVaryingVelocityFieldControlPointLatticePointer velocityFieldLattice = this->m_OutputTransform->GetModifiableVelocityField();

  SizeValueType numberOfIntegrationSteps = this->m_NumberOfTimePointSamples + 2;

  const typename TimeVaryingVelocityFieldControlPointLatticeType::RegionType & latticeRegion = velocityFieldLattice->GetLargestPossibleRegion();
  const typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType latticeSize = latticeRegion.GetSize();

  SizeValueType numberOfTimeControlPoints = latticeSize[ImageDimension];
  SizeValueType numberOfControlPointsPerTimePoint = static_cast<SizeValueType>( latticeRegion.GetNumberOfPixels() / numberOfTimeControlPoints );

  // Warp the moving image based on the composite transform (not including the current
  // time varying velocity field transform to be optimized).

  typename OutputTransformType::VelocityFieldPointType        sampledVelocityFieldOrigin;
  typename OutputTransformType::VelocityFieldSpacingType      sampledVelocityFieldSpacing;
  typename OutputTransformType::VelocityFieldSizeType         sampledVelocityFieldSize;
  typename OutputTransformType::VelocityFieldDirectionType    sampledVelocityFieldDirection;

  sampledVelocityFieldOrigin.Fill( 0.0 );
  sampledVelocityFieldSpacing.Fill( 1.0 );
  sampledVelocityFieldSize.Fill( this->m_NumberOfTimePointSamples );
  sampledVelocityFieldDirection.SetIdentity();

  typename VirtualImageType::ConstPointer virtualDomainImage;
  typename FixedImageMaskType::ConstPointer fixedImageMask;

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( metricQueue.IsNotNull() )
      {
      virtualDomainImage = metricQueue->GetVirtualImage();
      fixedImageMask = metricQueue->GetFixedImageMask();
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid conversion from the multi metric queue.");
      }
    }
  else
    {
    typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( metric.IsNotNull() )
      {
      virtualDomainImage = metric->GetVirtualImage();
      fixedImageMask = metric->GetFixedImageMask();
      }
    else
      {
      itkExceptionMacro("ERROR: Invalid metric conversion.");
      }
    }

  typedef typename ImageMetricType::DerivativeType MetricDerivativeType;
  const typename MetricDerivativeType::SizeValueType metricDerivativeSize = virtualDomainImage->GetLargestPossibleRegion().GetNumberOfPixels() * ImageDimension;
  MetricDerivativeType metricDerivative( metricDerivativeSize );

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    sampledVelocityFieldOrigin[i] = virtualDomainImage->GetOrigin()[i];
    sampledVelocityFieldSpacing[i] = virtualDomainImage->GetSpacing()[i];
    sampledVelocityFieldSize[i] = virtualDomainImage->GetRequestedRegion().GetSize()[i];
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      sampledVelocityFieldDirection[i][j] = virtualDomainImage->GetDirection()[i][j];
      }
    }

  this->m_OutputTransform->SetVelocityFieldOrigin( sampledVelocityFieldOrigin );
  this->m_OutputTransform->SetVelocityFieldDirection( sampledVelocityFieldDirection );
  this->m_OutputTransform->SetVelocityFieldSpacing( sampledVelocityFieldSpacing );
  this->m_OutputTransform->SetVelocityFieldSize( sampledVelocityFieldSize );
  this->m_OutputTransform->IntegrateVelocityField();

  typename TimeVaryingWeightedMaskImageType::Pointer timeVaryingFixedWeightedMaskImage = NULL;
  if( fixedImageMask )
    {
    timeVaryingFixedWeightedMaskImage = TimeVaryingWeightedMaskImageType::New();
    timeVaryingFixedWeightedMaskImage->SetOrigin( sampledVelocityFieldOrigin );
    timeVaryingFixedWeightedMaskImage->SetDirection( sampledVelocityFieldDirection );
    timeVaryingFixedWeightedMaskImage->SetSpacing( sampledVelocityFieldSpacing );
    timeVaryingFixedWeightedMaskImage->SetRegions( sampledVelocityFieldSize );
    timeVaryingFixedWeightedMaskImage->SetNumberOfComponentsPerPixel( 1 );
    timeVaryingFixedWeightedMaskImage->Allocate();
    timeVaryingFixedWeightedMaskImage->FillBuffer( 0.0 );
    }

  // Warp the moving image based on the composite transform (not including the current
  // time varying velocity field transform to be optimized).

  unsigned long sampledNumberOfPixels = sampledVelocityFieldSize[ImageDimension];
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    sampledNumberOfPixels *= sampledVelocityFieldSize[d];
    }

  // Instantiate the update derivative for all vectors of the velocity field
  DerivativeType updateDerivative( sampledNumberOfPixels * ImageDimension  );
  DerivativeType lastUpdateDerivative( sampledNumberOfPixels * ImageDimension  );
  lastUpdateDerivative.Fill( 0 );
  updateDerivative.Fill( 0 );

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  typename ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  IterationReporter reporter( this, 0, 1 );

  while( this->m_CurrentIteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !this->m_IsConverged )
    {
    updateDerivative.Fill( 0 );
    MeasureType value = NumericTraits<MeasureType>::Zero;
    this->m_CurrentMetricValue = NumericTraits<MeasureType>::Zero;

    typename PointSetType::Pointer velocityFieldPoints = PointSetType::New();
    velocityFieldPoints->Initialize();

    typename WeightsContainerType::Pointer velocityFieldWeights = WeightsContainerType::New();
    const WeightsElementType boundaryWeight = 1.0e10;

    IdentifierType numberOfVelocityFieldPoints = NumericTraits<IdentifierType>::Zero;

    for( SizeValueType timePoint = 0; timePoint < this->m_NumberOfTimePointSamples; timePoint++ )
      {
      RealType t = NumericTraits<RealType>::Zero;
      if( this->m_NumberOfTimePointSamples > 1 )
        {
        t = static_cast<RealType>( timePoint ) / static_cast<RealType>( this->m_NumberOfTimePointSamples - 1 );
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
      if( timePoint == this->m_NumberOfTimePointSamples - 1 )
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

      if( fixedImageMask )
        {
        typedef ResampleImageFilter<MaskImageType, WeightedMaskImageType, RealType> FixedMaskResamplerType;
        typename FixedMaskResamplerType::Pointer fixedMaskResampler = FixedMaskResamplerType::New();
        fixedMaskResampler->SetTransform( fixedDisplacementFieldTransform );
        fixedMaskResampler->SetInput( dynamic_cast<ImageMaskSpatialObjectType *>( const_cast<FixedImageMaskType *>( fixedImageMask.GetPointer() ) )->GetImage() );
        fixedMaskResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
        fixedMaskResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
        fixedMaskResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
        fixedMaskResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
        fixedMaskResampler->SetDefaultPixelValue( 0 );
        fixedMaskResampler->Update();

        ImageRegionIteratorWithIndex<WeightedMaskImageType> ItFM( fixedMaskResampler->GetOutput(), fixedMaskResampler->GetOutput()->GetRequestedRegion() );
        for( ItFM.GoToBegin(); !ItFM.IsAtEnd(); ++ItFM )
          {
          typename WeightedMaskImageType::PixelType weight = ItFM.Get();
          if( weight > 0.0 )
            {
            typename WeightedMaskImageType::IndexType index = ItFM.GetIndex();
            typename TimeVaryingWeightedMaskImageType::IndexType indexT;
            for( unsigned int d = 0; d < ImageDimension; d++ )
              {
              indexT[d] = index[d];
              }
            indexT[ImageDimension] = timePoint;
            timeVaryingFixedWeightedMaskImage->SetPixel( indexT, weight );
            }
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

      metricDerivative.Fill( NumericTraits<typename MetricDerivativeType::ValueType>::Zero );
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

      const SizeValueType numberOfPixels = static_cast<SizeValueType>( metricDerivative.Size() / ImageDimension );
      const bool importFilterWillReleaseMemory = false;

      DisplacementVectorType *metricDerivativeFieldPointer = reinterpret_cast<DisplacementVectorType *>( metricDerivative.data_block() );

      typename VirtualImageType::DirectionType identity;
      identity.SetIdentity();

      typedef ImportImageFilter<DisplacementVectorType, ImageDimension> DisplacementFieldImporterType;
      typename DisplacementFieldImporterType::Pointer displacementFieldImporter = DisplacementFieldImporterType::New();
      displacementFieldImporter->SetImportPointer( metricDerivativeFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
      displacementFieldImporter->SetRegion( virtualDomainImage->GetBufferedRegion() );
      displacementFieldImporter->SetOrigin( virtualDomainImage->GetOrigin() );
      displacementFieldImporter->SetSpacing( virtualDomainImage->GetSpacing() );
      displacementFieldImporter->SetDirection( identity );
      displacementFieldImporter->Update();

      typedef Image<RealType, ImageDimension> MagnitudeImageType;

      typedef VectorMagnitudeImageFilter<DisplacementFieldType, MagnitudeImageType> MagnituderType;
      typename MagnituderType::Pointer magnituder = MagnituderType::New();
      magnituder->SetInput( displacementFieldImporter->GetOutput() );
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

      updateDerivative.update( metricDerivative, timePoint * metricDerivative.size() );
      } // end loop over time points

    // update the transform --- averaging with the last update reduces oscillations
    updateDerivative = ( updateDerivative + lastUpdateDerivative ) * 0.5;
    lastUpdateDerivative = updateDerivative;

    // Here we need to convert the metric derivative to the control point derivative.

    const bool importFilterWillReleaseMemory = false;

    DisplacementVectorType *metricDerivativeFieldPointer = reinterpret_cast<DisplacementVectorType *>( updateDerivative.data_block() );

    const SizeValueType numberOfVelocityFieldPixels = static_cast<SizeValueType>( updateDerivative.Size() / ImageDimension );

    typename TimeVaryingVelocityFieldType::DirectionType identity;
    identity.SetIdentity();

    typedef ImportImageFilter<DisplacementVectorType, ImageDimension + 1> VelocityFieldImporterType;
    typename VelocityFieldImporterType::Pointer velocityFieldImporter = VelocityFieldImporterType::New();
    velocityFieldImporter->SetImportPointer( metricDerivativeFieldPointer, numberOfVelocityFieldPixels, importFilterWillReleaseMemory );
    velocityFieldImporter->SetRegion( sampledVelocityFieldSize );
    velocityFieldImporter->SetOrigin( sampledVelocityFieldOrigin );
    velocityFieldImporter->SetSpacing( sampledVelocityFieldSpacing );
    velocityFieldImporter->SetDirection( identity );
    velocityFieldImporter->Update();

    itkDebugMacro( "Extracting points from field. " )

    const typename VirtualImageType::IndexType virtualDomainIndex = virtualDomainImage->GetLargestPossibleRegion().GetIndex();
    const typename VirtualImageType::SizeType virtualDomainSize = virtualDomainImage->GetLargestPossibleRegion().GetSize();

    typename MaskImageType::ConstPointer maskImage = NULL;
    if( fixedImageMask )
      {
      typename ImageMaskSpatialObjectType::Pointer imageMask = dynamic_cast<ImageMaskSpatialObjectType *>( const_cast<FixedImageMaskType *>( fixedImageMask.GetPointer() ) );
      if( imageMask.IsNotNull() )
        {
        maskImage = imageMask->GetImage();
        }
      else
        {
        itkExceptionMacro("ERROR: Invalid maskImage conversion.");
        }
      }

    ImageRegionConstIteratorWithIndex<TimeVaryingVelocityFieldType> It( velocityFieldImporter->GetOutput(), velocityFieldImporter->GetOutput()->GetBufferedRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      typename TimeVaryingVelocityFieldType::IndexType index = It.GetIndex();

      DisplacementVectorType data = It.Get();

      WeightsElementType weight = 1.0;

      if( timeVaryingFixedWeightedMaskImage )
        {
        typename TimeVaryingWeightedMaskImageType::PixelType maskPixelValue = timeVaryingFixedWeightedMaskImage->GetPixel( index );
        if( maskPixelValue <= 0 )
           {
           continue;
           }
        else
          {
          weight = static_cast<WeightsElementType>( maskPixelValue );
          }
        }

      bool isOnBoundary = false;
      for( unsigned int d = 0; d < ImageDimension; d++ )
        {
        if( index[d] == virtualDomainIndex[d] || index[d] == virtualDomainIndex[d] + static_cast<int>( virtualDomainSize[d] ) - 1 )
          {
          isOnBoundary = true;
          break;
          }
        }
      if( isOnBoundary )
        {
        data.Fill( 0.0 );
        weight = boundaryWeight;
        }

      typename TimeVaryingVelocityFieldType::PointType point;
      velocityFieldImporter->GetOutput()->TransformIndexToPhysicalPoint( index, point );

      typename PointSetType::PointType spatioTemporalPoint;
      for( unsigned int d = 0; d < ImageDimension + 1; d++ )
        {
        spatioTemporalPoint[d] = point[d];
        }
      spatioTemporalPoint[ImageDimension] = spatioTemporalPoint[ImageDimension] / static_cast<RealType>( this->m_NumberOfTimePointSamples - 1 );

      velocityFieldPoints->SetPointData( numberOfVelocityFieldPoints, data );
      velocityFieldPoints->SetPoint( numberOfVelocityFieldPoints, spatioTemporalPoint );
      velocityFieldWeights->InsertElement( numberOfVelocityFieldPoints, weight );
      numberOfVelocityFieldPoints++;
      }

    // update the transform

    itkDebugMacro( "Calculating the B-spline field." );

    typename TimeVaryingVelocityFieldControlPointLatticeType::PointType         velocityFieldOrigin;
    typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType       velocityFieldSpacing;
    typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType          velocityFieldSize;

    for( unsigned int d = 0; d < ImageDimension; d++ )
      {
      velocityFieldOrigin[d] = virtualDomainImage->GetOrigin()[d];
      velocityFieldSpacing[d] = virtualDomainImage->GetSpacing()[d];
      velocityFieldSize[d] = virtualDomainImage->GetLargestPossibleRegion().GetSize()[d];
      }
    // provide a simple temporal domain spanning [0,1]
    velocityFieldOrigin[ImageDimension] = 0.0;
    velocityFieldSpacing[ImageDimension] = 0.1;
    velocityFieldSize[ImageDimension] = 11;

    typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();

    typename BSplineFilterType::ArrayType numberOfControlPoints;
    for( unsigned int d = 0; d < ImageDimension+1; d++ )
      {
      numberOfControlPoints[d] = latticeSize[d];
      }

    bspliner->SetOrigin( velocityFieldOrigin );
    bspliner->SetSpacing( velocityFieldSpacing );
    bspliner->SetSize( velocityFieldSize );
    bspliner->SetDirection( velocityFieldLattice->GetDirection() );
    bspliner->SetNumberOfLevels( 1 );
    bspliner->SetSplineOrder( this->m_OutputTransform->GetSplineOrder() );
    bspliner->SetNumberOfControlPoints( numberOfControlPoints );
    bspliner->SetInput( velocityFieldPoints );
    bspliner->SetPointWeights( velocityFieldWeights );
    if( this->GetDebug() )
      {
      bspliner->SetGenerateOutputImage( true );
      }
    else
      {
      bspliner->SetGenerateOutputImage( false );
      }
    bspliner->Update();

    TimeVaryingVelocityFieldControlPointLatticePointer updateControlPointLattice = bspliner->GetPhiLattice();

    TimeVaryingVelocityFieldPointer velocityField = NULL;
    if( this->GetDebug() )
      {
      velocityField = bspliner->GetOutput();
      }

    // Instantiate the update derivative for all vectors of the velocity field

    typename OutputTransformType::ScalarType * valuePointer =
      reinterpret_cast<typename OutputTransformType::ScalarType *>( updateControlPointLattice->GetBufferPointer() );
    DerivativeType updateControlPointDerivative( valuePointer, numberOfControlPointsPerTimePoint * numberOfTimeControlPoints * ImageDimension );

    this->m_OutputTransform->UpdateTransformParameters( updateControlPointDerivative, this->m_LearningRate );

    this->m_CurrentMetricValue /= static_cast<MeasureType>( this->m_NumberOfTimePointSamples );

    convergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
    this->m_CurrentConvergenceValue = convergenceMonitoring->GetConvergenceValue();

    if( this->m_CurrentConvergenceValue < this->m_ConvergenceThreshold )
      {
      this->m_IsConverged = true;

      this->m_OutputTransform->SetLowerTimeBound( 0 );
      this->m_OutputTransform->SetUpperTimeBound( 1.0 );
      this->m_OutputTransform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
      this->m_OutputTransform->IntegrateVelocityField();

      if( this->GetDebug() )
        {
        RealType spatialNorm = NumericTraits<RealType>::Zero;
        RealType spatioTemporalNorm = NumericTraits<RealType>::Zero;

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
          RealType localSpatialNorm = NumericTraits<RealType>::Zero;
          RealType localSpatioTemporalNorm = NumericTraits<RealType>::Zero;
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
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::GenerateData()
{
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

  DecoratedOutputTransformPointer transformDecorator = DecoratedOutputTransformType::New().GetPointer();
  transformDecorator->Set( this->m_OutputTransform );
  this->ProcessObject::SetNthOutput( 0, transformDecorator );
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Number of Iterations: " << this->m_NumberOfIterationsPerLevel << std::endl;
  os << indent << "Learning rate: " << this->m_LearningRate << std::endl;
  os << indent << "Convergence threshold: " << this->m_ConvergenceThreshold << std::endl;
  os << indent << "Convergence window size: " << this->m_ConvergenceWindowSize << std::endl;
}

} // end namespace itk

#endif
