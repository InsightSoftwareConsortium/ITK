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
#ifndef __itkTimeVaryingVelocityFieldImageRegistrationMethodv4_hxx
#define __itkTimeVaryingVelocityFieldImageRegistrationMethodv4_hxx

#include "itkTimeVaryingVelocityFieldImageRegistrationMethodv4.h"

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkDisplacementFieldTransform.h"
#include "itkImageDuplicator.h"
#include "itkImportImageFilter.h"
#include "itkIterationReporter.h"
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
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform>
::TimeVaryingVelocityFieldImageRegistrationMethodv4() :
  m_NumberOfIntegrationStepsPerTimeIndex( 5 ),
  m_LearningRate( 0.25 ),
  m_ConvergenceThreshold( 1.0e-6 )
{
  this->m_NumberOfIterationsPerLevel.SetSize( 3 );
  this->m_NumberOfIterationsPerLevel[0] = 20;
  this->m_NumberOfIterationsPerLevel[1] = 30;
  this->m_NumberOfIterationsPerLevel[2] = 40;
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform>
::~TimeVaryingVelocityFieldImageRegistrationMethodv4()
{
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform>
::StartOptimization()
{
  TimeVaryingVelocityFieldPointer velocityField = this->m_Transform->GetTimeVaryingVelocityField();
  IndexValueType numberOfTimePoints = velocityField->GetLargestPossibleRegion().GetSize()[ImageDimension];

  SizeValueType numberOfIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
    ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( numberOfTimePoints - 2 );

  const typename TimeVaryingVelocityFieldType::RegionType & largestRegion = velocityField->GetLargestPossibleRegion();
  const SizeValueType numberOfPixelsPerTimePoint = largestRegion.GetNumberOfPixels() / numberOfTimePoints;

  typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

  // Warp the moving image based on the composite transform (not including the current
  // time varying velocity field transform to be optimized).

  typedef ResampleImageFilter<MovingImageType, MovingImageType> MovingResamplerType;
  typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
  movingResampler->SetTransform( this->m_CompositeTransform );
  movingResampler->SetInput( this->m_MovingSmoothImage );
  movingResampler->SetSize( this->m_FixedSmoothImage->GetLargestPossibleRegion().GetSize() );
  movingResampler->SetOutputOrigin( this->m_FixedSmoothImage->GetOrigin() );
  movingResampler->SetOutputSpacing( this->m_FixedSmoothImage->GetSpacing() );
  movingResampler->SetOutputDirection( this->m_FixedSmoothImage->GetDirection() );
  movingResampler->SetDefaultPixelValue( 0 );

  // pre calculate the voxel distance to be used in properly scaling the gradient.
  RealType voxelDistance = 0.0;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    voxelDistance += vnl_math_sqr( virtualDomainImage->GetSpacing()[d] );
    }
  voxelDistance = vcl_sqrt( voxelDistance );

  // Instantiate the update derivative for all vectors of the velocity field
  DerivativeType updateDerivative( numberOfPixelsPerTimePoint * numberOfTimePoints * ImageDimension );

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<double> ConvergenceMonitoringType;
  ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( 10 );

  SizeValueType iteration = 0;
  bool isConverged = false;
  while( iteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !isConverged )
    {
    std::cout << "    Iteration " << iteration << std::flush;
    updateDerivative.Fill(0);
    for( IndexValueType timePoint = 0; timePoint < numberOfTimePoints; timePoint++ )
      {
      RealType t = static_cast<RealType>( timePoint ) / static_cast<RealType>( numberOfTimePoints - 1 );

      SizeValueType numberOfFixedIntegrationSteps = 0;
      if( timePoint > 0 )
        {
        numberOfFixedIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
          ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( timePoint - 1 );
        }

      SizeValueType numberOfMovingIntegrationSteps = 0;
      if( timePoint < numberOfTimePoints - 1 )
        {
        numberOfMovingIntegrationSteps = this->m_NumberOfIntegrationStepsPerTimeIndex +
          ( this->m_NumberOfIntegrationStepsPerTimeIndex - 1 ) * ( numberOfTimePoints - timePoint - 2 );
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

      typedef ImportImageFilter<DisplacementVectorType, ImageDimension> ImporterType;
      typename ImporterType::Pointer importer = ImporterType::New();
      importer->SetImportPointer( metricDerivativeFieldPointer, numberOfPixels, importFilterWillReleaseMemory );
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
      if( maxNorm > 0.0 )
        {
        metricDerivative *= ( voxelDistance / maxNorm );
        }
      updateDerivative.update( metricDerivative, timePoint * numberOfPixelsPerTimePoint * ImageDimension );
      }

    // update the transform

    this->m_Transform->UpdateTransformParameters( updateDerivative, this->m_LearningRate );

    // Calculate the current metric value to track convergence

    this->m_Transform->SetLowerTimeBound( 0 );
    this->m_Transform->SetUpperTimeBound( 1.0 );
    this->m_Transform->SetNumberOfIntegrationSteps( numberOfIntegrationSteps );
    this->m_Transform->IntegrateVelocityField();

    typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
    typename IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();

    this->m_Metric->SetFixedImage( this->m_FixedSmoothImage );
    this->m_Metric->SetFixedTransform( identityTransform );
    this->m_Metric->SetMovingImage( movingResampler->GetOutput() );
    this->m_Metric->SetMovingTransform( this->m_Transform );
    this->m_Metric->Initialize();

    typename MetricType::MeasureType value;
    typename MetricType::DerivativeType metricDerivative;

    this->m_Metric->GetValueAndDerivative( value, metricDerivative );

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
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform>
::GenerateData()
{
  TransformOutputType *transformOutput = static_cast<TransformOutputType *>( this->ProcessObject::GetOutput( 0 ) );

  transformOutput->Set( this->m_Transform.GetPointer() );

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    IterationReporter reporter( this, 0, 1 );

    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );
    this->m_Metric->Initialize();

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
TimeVaryingVelocityFieldImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Number of levels: " << this->m_NumberOfLevels << std::endl;
  os << indent << "Shrink factors: " << this->m_ShrinkFactorsPerLevel << std::endl;
  os << indent << "Smoothing sigmas: " << this->m_SmoothingSigmasPerLevel << std::endl;
  os << indent << "Number of iterations: " << this->m_NumberOfIterationsPerLevel << std::endl;
  os << indent << "Learning rate: " << this->m_LearningRate << std::endl;
}

} // end namespace itk

#endif
