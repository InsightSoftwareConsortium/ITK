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
#ifndef itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_hxx
#define itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod_hxx

#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkContinuousIndex.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageDuplicator.h"
#include "itkImageRegionConstIteratorWithOnlyIndex.h"
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
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::TimeVaryingBSplineVelocityFieldImageRegistrationMethod() :
  m_LearningRate( 0.25 ),
  m_ConvergenceThreshold( 1.0e-7 ),
  m_ConvergenceWindowSize( 10 ),
  m_NumberOfTimePointSamples( 4 ),
  m_BoundaryWeight( 1e10 )
{
  this->m_NumberOfIterationsPerLevel.SetSize( 3 );
  this->m_NumberOfIterationsPerLevel[0] = 20;
  this->m_NumberOfIterationsPerLevel[1] = 30;
  this->m_NumberOfIterationsPerLevel[2] = 40;
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::~TimeVaryingBSplineVelocityFieldImageRegistrationMethod()
{
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::StartOptimization()
{
  // This transform is used for the fixed image
  typedef itk::IdentityTransform<RealType, ImageDimension> IdentityTransformType;
  typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

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

  VirtualImageBaseConstPointer virtualDomainImage = this->GetCurrentLevelVirtualDomainImage();
  typename FixedImageMaskType::ConstPointer fixedImageMask = ITK_NULLPTR;

  if( virtualDomainImage.IsNull() )
    {
    itkExceptionMacro( "The virtual domain image is not found." );
    }

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( metricQueue.IsNotNull() )
      {
      fixedImageMask = metricQueue->GetFixedImageMask();
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
      fixedImageMask = metric->GetFixedImageMask();
      }
    }

  const DisplacementVectorType zeroVector( 0.0 );

  typename DisplacementFieldType::Pointer identityField = DisplacementFieldType::New();
  identityField->CopyInformation( virtualDomainImage );
  identityField->SetRegions( virtualDomainImage->GetLargestPossibleRegion() );
  identityField->Allocate();
  identityField->FillBuffer( zeroVector );

  this->m_IdentityDisplacementFieldTransform = DisplacementFieldTransformType::New();
  this->m_IdentityDisplacementFieldTransform->SetDisplacementField( identityField );
  this->m_IdentityDisplacementFieldTransform->SetInverseDisplacementField( identityField );

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

  // Keep track of velocityFieldPointSet from the previous iteration
  VelocityFieldPointSetPointer velocityFieldPointSetFromPreviousIteration = VelocityFieldPointSetType::New();
  velocityFieldPointSetFromPreviousIteration->Initialize();

  VelocityFieldPointSetPointer velocityFieldPointSet = VelocityFieldPointSetType::New();
  velocityFieldPointSet->Initialize();

  typename WeightsContainerType::Pointer velocityFieldWeights = WeightsContainerType::New();
  velocityFieldWeights->Initialize();

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  typename ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  IterationReporter reporter( this, 0, 1 );

  while( this->m_CurrentIteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !this->m_IsConverged )
    {
    this->GetMetricDerivativePointSetForAllTimePoints( velocityFieldPointSet, velocityFieldWeights );

    // update the point set --- averaging with the last update reduces oscillations
    if( this->m_CurrentIteration > 1 )
      {
      if( velocityFieldPointSet->GetNumberOfPoints() !=
        velocityFieldPointSetFromPreviousIteration->GetNumberOfPoints() )
        {
        itkExceptionMacro( "The number of points is not the same between iterations." );
        }

      typename VelocityFieldPointSetType::PointDataContainerIterator ItV =
        velocityFieldPointSet->GetPointData()->Begin();
      typename VelocityFieldPointSetType::PointDataContainerIterator ItVp =
        velocityFieldPointSetFromPreviousIteration->GetPointData()->Begin();

      while( ItV != velocityFieldPointSet->GetPointData()->End() )
        {
        DisplacementVectorType v = ItV.Value();
        DisplacementVectorType vp = ItVp.Value();

        velocityFieldPointSet->SetPointData( ItV.Index(), ( v + vp ) * 0.5 );
        velocityFieldPointSetFromPreviousIteration->SetPointData( ItV.Index(), v );

        ++ItV;
        ++ItVp;
        }
      }
    else
      {
      velocityFieldPointSetFromPreviousIteration->SetPoints( velocityFieldPointSet->GetPoints() );

      typename VelocityFieldPointSetType::PointDataContainerIterator ItV =
        velocityFieldPointSet->GetPointData()->Begin();

      while( ItV != velocityFieldPointSet->GetPointData()->End() )
        {
        velocityFieldPointSetFromPreviousIteration->SetPointData( ItV.Index(), ItV.Value() );
        ++ItV;
        }
      }

    // Convert the metric derivative to the control point derivative.

    itkDebugMacro( "Calculating the B-spline field." );

    typename TimeVaryingVelocityFieldControlPointLatticeType::PointType     velocityFieldOrigin;
    typename TimeVaryingVelocityFieldControlPointLatticeType::SpacingType   velocityFieldSpacing;
    typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType      velocityFieldSize;

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
    bspliner->SetInput( velocityFieldPointSet );
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

    TimeVaryingVelocityFieldPointer velocityField = ITK_NULLPTR;
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
    } // end of iteration loop
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetMetricDerivativePointSetForAllTimePoints( VelocityFieldPointSetType *velocityFieldPointSet, WeightsContainerType *velocityFieldWeights )
{
  this->m_CurrentMetricValue = NumericTraits<MeasureType>::ZeroValue();

  SizeValueType numberOfIntegrationSteps = this->m_NumberOfTimePointSamples + 2;

  typename DisplacementFieldType::PixelType zeroVector;
  zeroVector.Fill( 0 );

  velocityFieldPointSet->Initialize();
  velocityFieldWeights->Initialize();

  IdentifierType numberOfVelocityFieldPoints = NumericTraits<IdentifierType>::ZeroValue();

  for( SizeValueType timePoint = 0; timePoint < this->m_NumberOfTimePointSamples; timePoint++ )
    {
    RealType t = NumericTraits<RealType>::ZeroValue();
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

    // This transform gets used for the moving image
    typedef ImageDuplicator<DisplacementFieldType> DisplacementFieldDuplicatorType;
    typename DisplacementFieldDuplicatorType::Pointer fieldDuplicator = DisplacementFieldDuplicatorType::New();
    fieldDuplicator->SetInputImage( this->m_OutputTransform->GetDisplacementField() );
    fieldDuplicator->Update();

    typename DisplacementFieldDuplicatorType::Pointer inverseFieldDuplicator = DisplacementFieldDuplicatorType::New();
    inverseFieldDuplicator->SetInputImage( this->m_OutputTransform->GetInverseDisplacementField() );
    inverseFieldDuplicator->Update();

    typename DisplacementFieldTransformType::Pointer fixedDisplacementFieldTransform = DisplacementFieldTransformType::New();
    fixedDisplacementFieldTransform->SetDisplacementField( fieldDuplicator->GetModifiableOutput() );
    fixedDisplacementFieldTransform->SetInverseDisplacementField( inverseFieldDuplicator->GetModifiableOutput() );

    // Set up the fixed composite transform for the current time point

    InitialTransformType* fixedInitialTransform = const_cast<InitialTransformType*>( this->GetFixedInitialTransform() );

    typename CompositeTransformType::Pointer fixedComposite = CompositeTransformType::New();
    if( fixedInitialTransform != ITK_NULLPTR )
      {
      fixedComposite->AddTransform( fixedInitialTransform );
      }
    fixedComposite->AddTransform( fixedDisplacementFieldTransform );
    fixedComposite->FlattenTransformQueue();
    fixedComposite->SetOnlyMostRecentTransformToOptimizeOn();

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
    movingDisplacementFieldTransform->SetInverseDisplacementField( this->m_OutputTransform->GetModifiableInverseDisplacementField() );

    if( timePoint == 0 && this->m_CurrentIteration <= 1 )
      {
      typename DisplacementFieldDuplicatorType::Pointer fieldDuplicatorIdentity = DisplacementFieldDuplicatorType::New();
      fieldDuplicatorIdentity->SetInputImage( movingDisplacementFieldTransform->GetDisplacementField() );
      fieldDuplicatorIdentity->Update();
      fieldDuplicatorIdentity->GetModifiableOutput()->FillBuffer( zeroVector );

      typename DisplacementFieldTransformType::Pointer identityDisplacementFieldTransform = DisplacementFieldTransformType::New();
      identityDisplacementFieldTransform->SetDisplacementField( fieldDuplicatorIdentity->GetModifiableOutput() );
      }

    // Set up the moving composite transform for the current time point

    typename CompositeTransformType::Pointer movingComposite = CompositeTransformType::New();
    movingComposite->AddTransform( this->m_CompositeTransform );
    movingComposite->AddTransform( movingDisplacementFieldTransform );
    movingComposite->FlattenTransformQueue();
    movingComposite->SetOnlyMostRecentTransformToOptimizeOn();

    // Special case if we are just dealing with a pair of point sets.  This allows us to
    // use only the information at the points location instead of filling in the rest of
    // the virtual domain with zero displacements which is required for the Gaussian-based
    // variant in TimeVaryingVelocityFieldImageRegistration.  See the function in the
    // PointSetToPointSetMetric::SetStoreDerivativeAsSparseFieldForLocalSupportTransforms.

    SizeValueType initialNumberOfVelocityFieldPointsAtTimePoint = velocityFieldPointSet->GetNumberOfPoints();

    if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC )
      {
      this->m_Metric->SetFixedObject( this->m_FixedPointSets[0] );
      this->m_Metric->SetMovingObject( this->m_MovingPointSets[0] );

      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( fixedComposite );
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( movingComposite );

      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetCalculateValueAndDerivativeInTangentSpace( true );
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetStoreDerivativeAsSparseFieldForLocalSupportTransforms( false );

      this->m_Metric->Initialize();
      typename ImageMetricType::DerivativeType metricDerivative;
      MeasureType value = NumericTraits<MeasureType>::ZeroValue();

      this->m_Metric->GetValueAndDerivative( value, metricDerivative );

      this->m_CurrentMetricValue += value;

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

      InputPointSetPointer transformedPointSet =
        dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->GetModifiableFixedTransformedPointSet();

      typename InputPointSetType::PointsContainerConstIterator It = transformedPointSet->GetPoints()->Begin();

      SizeValueType localPointCount = 0;
      while( It != transformedPointSet->GetPoints()->End() )
        {
        typename InputPointSetType::PointType spatialPoint = It.Value();

        typename VelocityFieldPointSetType::PixelType displacement;
        typename VelocityFieldPointSetType::PointType spatioTemporalPoint;
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          displacement[d] = metricDerivative[localPointCount * ImageDimension + d];
          spatioTemporalPoint[d] = spatialPoint[d];
          }
        localPointCount++;

        spatioTemporalPoint[ImageDimension] = t;
        velocityFieldPointSet->SetPoint( numberOfVelocityFieldPoints, spatioTemporalPoint );
        velocityFieldPointSet->SetPointData( numberOfVelocityFieldPoints, displacement );
        velocityFieldWeights->InsertElement( numberOfVelocityFieldPoints, 1.0 );
        numberOfVelocityFieldPoints++;

        ++It;
        }

      // We need to clamp the boundary so we add the boundary points with zero displacement.

      const DisplacementFieldType *fixedDisplacementField = fixedDisplacementFieldTransform->GetDisplacementField();

      typename DisplacementFieldType::IndexType fixedDomainIndex =
        fixedDisplacementField->GetRequestedRegion().GetIndex();
      typename DisplacementFieldType::SizeType fixedDomainSize =
        fixedDisplacementField->GetRequestedRegion().GetSize();

      ImageRegionConstIteratorWithOnlyIndex<DisplacementFieldType> ItF( fixedDisplacementField,
        fixedDisplacementField->GetRequestedRegion() );
      for( ItF.GoToBegin(); !ItF.IsAtEnd(); ++ItF )
        {
        typename DisplacementFieldType::IndexType index = ItF.GetIndex();

        bool isOnBoundary = false;
        for( unsigned d = 0; d < ImageDimension; d++ )
          {
          if( index[d] == fixedDomainIndex[d] || index[d] == fixedDomainIndex[d] + static_cast<int>( fixedDomainSize[d] ) - 1 )
            {
            isOnBoundary = true;
            break;
            }
          }
        if( isOnBoundary )
          {
          typename DisplacementFieldType::PointType imagePoint;

          fixedDisplacementField->TransformIndexToPhysicalPoint( index, imagePoint );
          typename VelocityFieldPointSetType::PointType spatioTemporalPoint;
          for( unsigned int d = 0; d < ImageDimension; d++ )
            {
            spatioTemporalPoint[d] = imagePoint[d];
            }
          spatioTemporalPoint[ImageDimension] = t;
          typename VelocityFieldPointSetType::PixelType displacement( 0.0 );

          velocityFieldPointSet->SetPoint( numberOfVelocityFieldPoints, spatioTemporalPoint );
          velocityFieldPointSet->SetPointData( numberOfVelocityFieldPoints, displacement );
          velocityFieldWeights->InsertElement( numberOfVelocityFieldPoints, this->m_BoundaryWeight );
          numberOfVelocityFieldPoints++;
          }
        }
      }
    else
      {
      this->AttachMetricGradientPointSetAtSpecificTimePoint( t,
        velocityFieldPointSet, velocityFieldWeights,
        this->m_FixedSmoothImages, this->m_FixedPointSets, fixedComposite,
        this->m_MovingSmoothImages, this->m_MovingPointSets, movingComposite,
        this->m_FixedImageMasks );
      }

    // After calculating the velocity field points for a specific parameterized time point,
    // we rescale the displacement vectors based on the max norm of the set.

    typename VelocityFieldPointSetType::PointDataContainerIterator ItD =
      velocityFieldPointSet->GetPointData()->Begin();

    RealType maxSquaredNorm = 0.0;
    while( ItD != velocityFieldPointSet->GetPointData()->End() )
      {
      if( ItD.Index() >= initialNumberOfVelocityFieldPointsAtTimePoint )
        {
        RealType squaredNorm = ( ItD.Value() ).GetSquaredNorm();
        if( squaredNorm > maxSquaredNorm )
          {
          maxSquaredNorm = squaredNorm;
          }
        }
      ++ItD;
      }

    if( maxSquaredNorm > 0.0 )
      {
      RealType normalizationFactor = 1.0 / std::sqrt( maxSquaredNorm );

      ItD = velocityFieldPointSet->GetPointData()->Begin();
      while( ItD != velocityFieldPointSet->GetPointData()->End() )
        {
        if( ItD.Index() >= initialNumberOfVelocityFieldPointsAtTimePoint )
          {
          velocityFieldPointSet->SetPointData( ItD.Index(), normalizationFactor * ItD.Value() );
          }
        ++ItD;
        }
      }
    } // end loop over time points
}


template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::AttachMetricGradientPointSetAtSpecificTimePoint( const RealType normalizedTimePoint,
  VelocityFieldPointSetType *velocityFieldPoints, WeightsContainerType *velocityFieldWeights,
  const FixedImagesContainerType fixedImages, const PointSetsContainerType fixedPointSets,
  const TransformBaseType * fixedTransform, const MovingImagesContainerType movingImages,
  const PointSetsContainerType movingPointSets, const TransformBaseType * movingTransform,
  const FixedImageMasksContainerType fixedImageMasks )
{
  VirtualImageBaseConstPointer virtualDomainImage = this->GetCurrentLevelVirtualDomainImage();

  typename DisplacementFieldType::DirectionType identity;
  identity.SetIdentity();

  typename DisplacementFieldType::Pointer bsplineParametricDomainField = DisplacementFieldType::New();
  bsplineParametricDomainField->CopyInformation( virtualDomainImage );
  bsplineParametricDomainField->SetRegions( virtualDomainImage->GetRequestedRegion() );
  bsplineParametricDomainField->SetDirection( identity );
//   bsplineParametricDomainField->Allocate();

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );

  if( multiMetric )
    {
    for( SizeValueType n = 0; n < multiMetric->GetNumberOfMetrics(); n++ )
      {
      if( multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::POINT_SET_METRIC )
        {
        multiMetric->GetMetricQueue()[n]->SetFixedObject( fixedPointSets[n] );
        multiMetric->GetMetricQueue()[n]->SetMovingObject( movingPointSets[n] );
        multiMetric->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
        multiMetric->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );

        dynamic_cast<PointSetMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->
          SetCalculateValueAndDerivativeInTangentSpace( true );
        dynamic_cast<PointSetMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->
          SetStoreDerivativeAsSparseFieldForLocalSupportTransforms( true );
        }
      else if( multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::IMAGE_METRIC )
        {
        typedef ResampleImageFilter<FixedImageType, FixedImageType, RealType> FixedResamplerType;
        typename FixedResamplerType::Pointer fixedResampler = FixedResamplerType::New();
        fixedResampler->SetInput( fixedImages[n] );
        fixedResampler->SetTransform( fixedTransform );
        fixedResampler->UseReferenceImageOn();
        fixedResampler->SetReferenceImage( virtualDomainImage );
        fixedResampler->SetDefaultPixelValue( 0 );
        fixedResampler->Update();

        typedef ResampleImageFilter<MovingImageType, MovingImageType, RealType> MovingResamplerType;
        typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
        movingResampler->SetInput( movingImages[n] );
        movingResampler->SetTransform( movingTransform );
        movingResampler->UseReferenceImageOn();
        movingResampler->SetReferenceImage( virtualDomainImage );
        movingResampler->SetDefaultPixelValue( 0 );
        movingResampler->Update();

        multiMetric->GetMetricQueue()[n]->SetFixedObject( fixedResampler->GetOutput() );
        multiMetric->GetMetricQueue()[n]->SetMovingObject( movingResampler->GetOutput() );

        multiMetric->SetFixedTransform( this->m_IdentityDisplacementFieldTransform );
        multiMetric->SetMovingTransform( this->m_IdentityDisplacementFieldTransform );
        }
      else
        {
        itkExceptionMacro( "Invalid metric." );
        }
      }
    }
  else if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
    {
    typedef ResampleImageFilter<FixedImageType, FixedImageType, RealType> FixedResamplerType;
    typename FixedResamplerType::Pointer fixedResampler = FixedResamplerType::New();
    fixedResampler->SetInput( fixedImages[0] );
    fixedResampler->SetTransform( fixedTransform );
    fixedResampler->UseReferenceImageOn();
    fixedResampler->SetReferenceImage( virtualDomainImage );
    fixedResampler->SetDefaultPixelValue( 0 );
    fixedResampler->Update();

    typedef ResampleImageFilter<MovingImageType, MovingImageType, RealType> MovingResamplerType;
    typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
    movingResampler->SetInput( movingImages[0] );
    movingResampler->SetTransform( movingTransform );
    movingResampler->UseReferenceImageOn();
    movingResampler->SetReferenceImage( virtualDomainImage );
    movingResampler->SetDefaultPixelValue( 0 );
    movingResampler->Update();

    this->m_Metric->SetFixedObject( fixedResampler->GetOutput() );
    this->m_Metric->SetMovingObject( movingResampler->GetOutput() );

    dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( this->m_IdentityDisplacementFieldTransform );
    dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( this->m_IdentityDisplacementFieldTransform );
    }
  else  // The point set metric is handled as a special case in ::GetMetricDerivativePointSet()
    {
    itkExceptionMacro( "Invalid metric." );
    }

  this->m_Metric->Initialize();

  const typename MetricDerivativeType::SizeValueType metricDerivativeSize =
    virtualDomainImage->GetLargestPossibleRegion().GetNumberOfPixels() * ImageDimension;
  MetricDerivativeType metricDerivative( metricDerivativeSize );

  RealType value;

  metricDerivative.Fill( NumericTraits<typename MetricDerivativeType::ValueType>::ZeroValue() );
  this->m_Metric->GetValueAndDerivative( value, metricDerivative );

  this->m_CurrentMetricValue += value;

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

  typename WeightedMaskImageType::Pointer  fixedWeightedImageMask = ITK_NULLPTR;
  if( fixedImageMasks[0] )
    {
    typedef ResampleImageFilter<MaskImageType, WeightedMaskImageType, RealType> FixedMaskResamplerType;
    typename FixedMaskResamplerType::Pointer fixedMaskResampler = FixedMaskResamplerType::New();
    fixedMaskResampler->SetTransform( fixedTransform );
    fixedMaskResampler->SetInput( dynamic_cast<ImageMaskSpatialObjectType *>( const_cast<FixedImageMaskType *>( fixedImageMasks[0].GetPointer() ) )->GetImage() );
    fixedMaskResampler->SetSize( virtualDomainImage->GetRequestedRegion().GetSize() );
    fixedMaskResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
    fixedMaskResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
    fixedMaskResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
    fixedMaskResampler->SetDefaultPixelValue( 0 );

    fixedWeightedImageMask = fixedMaskResampler->GetOutput();
    fixedWeightedImageMask->Update();
    fixedWeightedImageMask->DisconnectPipeline();
    }

  // Add the velocity field points computed at this particular parameterized time point, t,
  // to the current set of velocity field points.

  SizeValueType numberOfVelocityFieldPoints = velocityFieldPoints->GetNumberOfPoints();

  typename DisplacementFieldType::Pointer gradientField = DisplacementFieldType::New();
  gradientField->CopyInformation( virtualDomainImage );
  gradientField->SetRegions( virtualDomainImage->GetRequestedRegion() );
  gradientField->Allocate();

  typename DisplacementFieldType::IndexType gradientFieldIndex =
    gradientField->GetRequestedRegion().GetIndex();
  typename DisplacementFieldType::SizeType gradientFieldSize =
    gradientField->GetRequestedRegion().GetSize();

  ImageRegionConstIteratorWithOnlyIndex<DisplacementFieldType> ItG(
    gradientField, gradientField->GetRequestedRegion() );

  SizeValueType localCount = 0;
  for( ItG.GoToBegin(); !ItG.IsAtEnd(); ++ItG )
    {
    typename DisplacementFieldType::IndexType index = ItG.GetIndex();

    bool isOnBoundary = false;
    for( SizeValueType d = 0; d < ImageDimension; d++ )
      {
      if( index[d] == gradientFieldIndex[d] ||
        index[d] == gradientFieldIndex[d] + static_cast<int>( gradientFieldSize[d] ) - 1 )
        {
        isOnBoundary = true;
        break;
        }
      }

    RealType weight = 1.0;
    if( isOnBoundary )
      {
      weight = this->m_BoundaryWeight;
      }
    else if( fixedWeightedImageMask.IsNotNull() )
      {
      weight = static_cast<RealType>( fixedWeightedImageMask->GetPixel( index ) );
      }

    ContinuousIndexType cidx;
    for( SizeValueType d = 0; d < ImageDimension; d++ )
      {
      cidx[d] = static_cast<typename ContinuousIndexType::CoordRepType>( index[d] );
      }
    DisplacementFieldPointType parametricPoint;
    bsplineParametricDomainField->TransformContinuousIndexToPhysicalPoint( cidx, parametricPoint );

    typename VelocityFieldPointSetType::PointType velocityFieldPoint;

    DisplacementVectorType displacement;
    for( SizeValueType d = 0; d < ImageDimension; d++ )
      {
      displacement[d] = metricDerivative[localCount++];
      velocityFieldPoint[d] = parametricPoint[d];
      }
    velocityFieldPoint[ImageDimension] = normalizedTimePoint;

    velocityFieldPoints->SetPoint( numberOfVelocityFieldPoints, velocityFieldPoint );
    velocityFieldPoints->SetPointData( numberOfVelocityFieldPoints, displacement );
    velocityFieldWeights->InsertElement( numberOfVelocityFieldPoints, weight );
    numberOfVelocityFieldPoints++;
    }
}

/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
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

  this->GetTransformOutput()->Set( this->m_OutputTransform );
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
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
