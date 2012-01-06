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

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkIterationReporter.h"
#include "itkPointSet.h"
#include "itkResampleImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkWindowConvergenceMonitoringFunction.h"

#include "itkImageFileWriter.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
TimeVaryingBSplineVelocityFieldImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::TimeVaryingBSplineVelocityFieldImageRegistrationMethod() :
  m_LearningRate( 0.25 ),
  m_NumberOfIntegrationStepsPerTimeIndex( 5 ),
  m_ConvergenceThreshold( 1.0e-6 ),
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
  TimeVaryingVelocityFieldControlPointLatticePointer velocityFieldLattice = this->m_Transform->GetTimeVaryingVelocityFieldControlPointLattice();

  SizeValueType numberOfIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
    ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( this->m_NumberOfTimePointSamples - 2 );

  const typename TimeVaryingVelocityFieldControlPointLatticeType::RegionType & latticeRegion = velocityFieldLattice->GetLargestPossibleRegion();
  const typename TimeVaryingVelocityFieldControlPointLatticeType::SizeType latticeSize = latticeRegion.GetSize();

  SizeValueType numberOfTimeControlPoints = latticeSize[ImageDimension];
  SizeValueType numberOfControlPointsPerTimePoint = static_cast<SizeValueType>( latticeRegion.GetNumberOfPixels() / numberOfTimeControlPoints );

  // Warp the moving image based on the composite transform (not including the current
  // time varying velocity field transform to be optimized).

  typename TransformType::VelocityFieldPointType        sampledVelocityFieldOrigin;
  typename TransformType::VelocityFieldSpacingType      sampledVelocityFieldSpacing;
  typename TransformType::VelocityFieldSizeType         sampledVelocityFieldSize;
  typename TransformType::VelocityFieldDirectionType    sampledVelocityFieldDirection;

  sampledVelocityFieldOrigin.Fill( 0.0 );
  sampledVelocityFieldSpacing.Fill( 1.0 );
  sampledVelocityFieldSize.Fill( this->m_NumberOfTimePointSamples );
  sampledVelocityFieldDirection.SetIdentity();

  typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

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

  this->m_Transform->SetVelocityFieldOrigin( sampledVelocityFieldOrigin );
  this->m_Transform->SetVelocityFieldDirection( sampledVelocityFieldDirection );
  this->m_Transform->SetVelocityFieldSpacing( sampledVelocityFieldSpacing );
  this->m_Transform->SetVelocityFieldSize( sampledVelocityFieldSize );

  typedef ResampleImageFilter<MovingImageType, MovingImageType> MovingResamplerType;
  typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
  movingResampler->SetTransform( this->m_CompositeTransform );
  movingResampler->SetInput( this->m_MovingSmoothImage );
  movingResampler->SetSize( virtualDomainImage->GetLargestPossibleRegion().GetSize() );
  movingResampler->SetOutputOrigin( virtualDomainImage->GetOrigin() );
  movingResampler->SetOutputSpacing( virtualDomainImage->GetSpacing() );
  movingResampler->SetOutputDirection( virtualDomainImage->GetDirection() );
  movingResampler->SetDefaultPixelValue( 0 );
  movingResampler->Update();

  // pre calculate the voxel distance to be used in properly scaling the gradient.
  RealType voxelDistance = 0.0;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    voxelDistance += vnl_math_sqr( virtualDomainImage->GetSpacing()[d] );
    }
  voxelDistance = vcl_sqrt( voxelDistance );

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<double> ConvergenceMonitoringType;
  ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( 10 );

  SizeValueType iteration = 0;
  bool isConverged = false;
  while( iteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !isConverged )
    {
    std::cout << "    Iteration " << iteration << std::flush;

    typedef PointSet<DisplacementVectorType, ImageDimension+1> PointSetType;
    typename PointSetType::Pointer velocityFieldPoints = PointSetType::New();
    velocityFieldPoints->Initialize();

    typedef BSplineScatteredDataPointSetToImageFilter<PointSetType, TimeVaryingVelocityFieldType> BSplineFilterType;
    typedef typename BSplineFilterType::WeightsContainerType WeightsContainerType;
    typename WeightsContainerType::Pointer velocityFieldWeights = WeightsContainerType::New();
    const typename WeightsContainerType::Element boundaryWeight = 1.0e10;

    IdentifierType numberOfVelocityFieldPoints = NumericTraits<IdentifierType>::Zero;

    for( SizeValueType timePoint = 0; timePoint < this->m_NumberOfTimePointSamples; timePoint++ )
      {
      RealType t = static_cast<RealType>( timePoint ) / static_cast<RealType>( this->m_NumberOfTimePointSamples - 1 );

      SizeValueType numberOfFixedIntegrationSteps = 0;
      if( timePoint > 0 )
        {
        numberOfFixedIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
          ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( timePoint - 1 );
        }

      SizeValueType numberOfMovingIntegrationSteps = 0;
      if( timePoint < this->m_NumberOfTimePointSamples - 1 )
        {
        numberOfMovingIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
          ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( this->m_NumberOfTimePointSamples - timePoint - 2 );
        }

      // Get the fixed transform.  We need to duplicate the resulting
      // displacement field since it will be overwritten when we integrate
      // the velocity field to get the moving image transform.

      this->m_Transform->SetLowerTimeBound( t );
      this->m_Transform->SetUpperTimeBound( 0.0 );
      this->m_Transform->SetNumberOfIntegrationSteps( numberOfFixedIntegrationSteps );
      this->m_Transform->IntegrateVelocityField();

      typedef ImageDuplicator<DisplacementFieldType> DisplacementFieldDuplicatorType;
      typename DisplacementFieldDuplicatorType::Pointer fieldDuplicator = DisplacementFieldDuplicatorType::New();
      fieldDuplicator->SetInputImage( this->m_Transform->GetDisplacementField() );
      fieldDuplicator->Update();

      typedef DisplacementFieldTransform<RealType, ImageDimension> DisplacementFieldTransformType;
      typename DisplacementFieldTransformType::Pointer fixedDisplacementFieldTransform = DisplacementFieldTransformType::New();
      fixedDisplacementFieldTransform->SetDisplacementField( fieldDuplicator->GetOutput() );

      // Get the moving transform
      this->m_Transform->SetLowerTimeBound( t );
      this->m_Transform->SetUpperTimeBound( 1.0 );
      this->m_Transform->SetNumberOfIntegrationSteps( numberOfMovingIntegrationSteps );
      this->m_Transform->IntegrateVelocityField();

      typename DisplacementFieldTransformType::Pointer movingDisplacementFieldTransform = DisplacementFieldTransformType::New();
      movingDisplacementFieldTransform->SetDisplacementField( this->m_Transform->GetDisplacementField() );

      this->m_Metric->SetFixedImage( this->m_FixedSmoothImage );
      this->m_Metric->SetFixedTransform( fixedDisplacementFieldTransform );
      this->m_Metric->SetMovingImage( movingResampler->GetOutput() );
      this->m_Metric->SetMovingTransform( movingDisplacementFieldTransform );
      this->m_Metric->Initialize();

      typename MetricType::MeasureType value;
      typename MetricType::DerivativeType metricDerivative;

      this->m_Metric->GetValueAndDerivative( value, metricDerivative );

      // we rescale the update velocity field at each time point.
      // we first need to convert to a displacement field to look
      // at the max norm of the field.

      const SizeValueType numberOfPixels = static_cast<SizeValueType>( metricDerivative.Size() / ImageDimension );
      const bool importFilterWillReleaseMemory = false;

      DisplacementVectorType *metricDerivativeFieldPointer = reinterpret_cast<DisplacementVectorType *>( metricDerivative.data_block() );

      typename VirtualImageType::DirectionType identity;
      identity.SetIdentity();

      typedef ImportImageFilter<DisplacementVectorType, ImageDimension> ImporterType;
      typename ImporterType::Pointer importer = ImporterType::New();
      importer->SetImportPointer( metricDerivativeFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
      importer->SetRegion( virtualDomainImage->GetBufferedRegion() );
      importer->SetOrigin( virtualDomainImage->GetOrigin() );
      importer->SetSpacing( virtualDomainImage->GetSpacing() );
      importer->SetDirection( identity );
      importer->Update();

      typename DisplacementFieldType::IndexType virtualDomainIndex = virtualDomainImage->GetRequestedRegion().GetIndex();
      typename DisplacementFieldType::SizeType virtualDomainSize = virtualDomainImage->GetRequestedRegion().GetSize();

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

      // Here we need to convert the metric derivative to the control point derivative.

      itkDebugMacro( "Extracting points from field. " )

      ImageRegionConstIteratorWithIndex<DisplacementFieldType> It( importer->GetOutput(), importer->GetOutput()->GetBufferedRegion() );
      for( It.GoToBegin(); !It.IsAtEnd(); ++It )
        {
        typename DisplacementFieldType::IndexType index = It.GetIndex();

        DisplacementVectorType data = It.Get();
        if( maxNorm > 0.0 )
          {
          data *= ( voxelDistance / maxNorm );
          }

        typename WeightsContainerType::Element weight = 1.0;

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

        typename DisplacementFieldType::PointType point;
        importer->GetOutput()->TransformIndexToPhysicalPoint( index, point );

        typename PointSetType::PointType spatioTemporalPoint;
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          spatioTemporalPoint[d] = point[d];
          }
        spatioTemporalPoint[ImageDimension] = t;

        velocityFieldPoints->SetPointData( numberOfVelocityFieldPoints, data );
        velocityFieldPoints->SetPoint( numberOfVelocityFieldPoints, spatioTemporalPoint );
        velocityFieldWeights->InsertElement( numberOfVelocityFieldPoints, weight );
        numberOfVelocityFieldPoints++;
        }
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

    typedef BSplineScatteredDataPointSetToImageFilter<PointSetType, TimeVaryingVelocityFieldType> BSplineFilterType;
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
    bspliner->SetSplineOrder( this->m_Transform->GetSplineOrder() );
    bspliner->SetNumberOfControlPoints( numberOfControlPoints );
    bspliner->SetInput( velocityFieldPoints );
    bspliner->SetPointWeights( velocityFieldWeights );
    bspliner->SetGenerateOutputImage( false );
    bspliner->Update();

    TimeVaryingVelocityFieldControlPointLatticePointer updateControlPointLattice = bspliner->GetPhiLattice();

    // Instantiate the update derivative for all vectors of the velocity field

    typename TransformType::ScalarType * valuePointer =
      reinterpret_cast<typename TransformType::ScalarType *>( updateControlPointLattice->GetBufferPointer() );
    DerivativeType updateDerivative( valuePointer, numberOfControlPointsPerTimePoint * numberOfTimeControlPoints * ImageDimension );

    this->m_Transform->UpdateTransformParameters( updateDerivative, this->m_LearningRate );

    // Calculate the current metric value to track convergence

    this->m_Transform->SetLowerTimeBound( 0.0 );
    this->m_Transform->SetUpperTimeBound( 1.0 );
    this->m_Transform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
    this->m_Transform->IntegrateVelocityField();

    typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
    typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();

    // For right now, we take the resulting displacement field and
    // create a displacement field transform.  Using the m_Transform
    // directly causes a crash when we call GetValueAndDerivative().

    typedef DisplacementFieldTransform<RealType, ImageDimension> DisplacementFieldTransformType;
    typename DisplacementFieldTransformType::Pointer movingDisplacementFieldTransform = DisplacementFieldTransformType::New();
    movingDisplacementFieldTransform->SetDisplacementField( this->m_Transform->GetDisplacementField() );

    this->m_Metric->SetFixedImage( this->m_FixedSmoothImage );
    this->m_Metric->SetFixedTransform( identityTransform );
    this->m_Metric->SetMovingImage( movingResampler->GetOutput() );
    this->m_Metric->SetMovingTransform( movingDisplacementFieldTransform );
    this->m_Metric->Initialize();

    typename MetricType::MeasureType value;
    typename MetricType::DerivativeType derivative;
    this->m_Metric->GetValueAndDerivative( value, derivative );

    convergenceMonitoring->AddEnergyValue( value );
    RealType convergenceValue = convergenceMonitoring->GetConvergenceValue();
    std::cout << ": (metric value = " << value << ", convergence value = " << convergenceValue << ")" << std::endl;
    if( convergenceValue < this->m_ConvergenceThreshold )
      {
      isConverged = true;
      }
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
  TransformOutputType *transformOutput = static_cast<TransformOutputType *>( this->ProcessObject::GetOutput( 0 ) );

  transformOutput->Set( this->m_Transform.GetPointer() );

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    IterationReporter reporter( this, 0, 1 );

    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );

    // The base class adds the transform to be optimized at initialization.
    // However, since this class handles its own optimization, we remove it
    // to optimize separately.  We then add it after the optimization loop.

    this->m_CompositeTransform->RemoveTransform();

    this->StartOptimization();

    this->m_CompositeTransform->AddTransform( this->m_Transform );

    reporter.CompletedStep();
    }

  TransformOutputPointer transformDecorator = TransformOutputType::New().GetPointer();
  transformDecorator->Set( this->m_Transform );
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

  os << indent << "Number of iterations: " << this->m_NumberOfIterationsPerLevel << std::endl;
  os << indent << "Learning rate: " << this->m_LearningRate << std::endl;
}

} // end namespace itk

#endif
