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
#ifndef __itkSyNImageRegistrationMethod_hxx
#define __itkSyNImageRegistrationMethod_hxx

#include "itkSyNImageRegistrationMethod.h"

#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkGaussianOperator.h"
#include "itkImportImageFilter.h"
#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkIterationReporter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkWindowConvergenceMonitoringFunction.h"

#include "itkImageFileWriter.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::SyNImageRegistrationMethod() :
  m_LearningRate( 0.25 ),
  m_GaussianSmoothingVarianceForTheUpdateField( 3.0 ),
  m_GaussianSmoothingVarianceForTheTotalField( 0.5 ),
  m_ConvergenceThreshold( 1.0e-6 )
{
  this->m_NumberOfIterationsPerLevel.SetSize( 3 );
  this->m_NumberOfIterationsPerLevel[0] = 20;
  this->m_NumberOfIterationsPerLevel[1] = 30;
  this->m_NumberOfIterationsPerLevel[2] = 40;

  this->m_MiddleToFixedTransform = TransformType::New();
  this->m_MiddleToMovingTransform = TransformType::New();
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::~SyNImageRegistrationMethod()
{
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{
  Superclass::InitializeRegistrationAtEachLevel( level );

  if( level == 0 )
    {
    typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

    DisplacementVectorType zeroVector( 0.0 );

    typename DisplacementFieldType::Pointer fixedDisplacementField = DisplacementFieldType::New();
    fixedDisplacementField->CopyInformation( virtualDomainImage );
    fixedDisplacementField->SetRegions( virtualDomainImage->GetBufferedRegion() );
    fixedDisplacementField->Allocate();
    fixedDisplacementField->FillBuffer( zeroVector );

    typename DisplacementFieldType::Pointer fixedInverseDisplacementField = DisplacementFieldType::New();
    fixedInverseDisplacementField->CopyInformation( virtualDomainImage );
    fixedInverseDisplacementField->SetRegions( virtualDomainImage->GetBufferedRegion() );
    fixedInverseDisplacementField->Allocate();
    fixedInverseDisplacementField->FillBuffer( zeroVector );

    this->m_MiddleToFixedTransform->SetDisplacementField( fixedDisplacementField );
    this->m_MiddleToFixedTransform->SetInverseDisplacementField( fixedInverseDisplacementField );

    typename DisplacementFieldType::Pointer movingDisplacementField = DisplacementFieldType::New();
    movingDisplacementField->CopyInformation( virtualDomainImage );
    movingDisplacementField->SetRegions( virtualDomainImage->GetBufferedRegion() );
    movingDisplacementField->Allocate();
    movingDisplacementField->FillBuffer( zeroVector );

    typename DisplacementFieldType::Pointer movingInverseDisplacementField = DisplacementFieldType::New();
    movingInverseDisplacementField->CopyInformation( virtualDomainImage );
    movingInverseDisplacementField->SetRegions( virtualDomainImage->GetBufferedRegion() );
    movingInverseDisplacementField->Allocate();
    movingInverseDisplacementField->FillBuffer( zeroVector );

    this->m_MiddleToMovingTransform->SetDisplacementField( movingDisplacementField );
    this->m_MiddleToMovingTransform->SetInverseDisplacementField( movingInverseDisplacementField );
    }
  else if( this->m_TransformParametersAdaptorsPerLevel[level] )
    {
    this->m_TransformParametersAdaptorsPerLevel[level]->SetTransform( this->m_MiddleToMovingTransform );
    this->m_TransformParametersAdaptorsPerLevel[level]->AdaptTransformParameters();
    this->m_TransformParametersAdaptorsPerLevel[level]->SetTransform( this->m_MiddleToFixedTransform );
    this->m_TransformParametersAdaptorsPerLevel[level]->AdaptTransformParameters();
    }
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::StartOptimization()
{
  // Warp the moving image based on the composite transform (not including the current
  // transform to be optimized).

  typedef ResampleImageFilter<MovingImageType, MovingImageType> MovingResamplerType;
  typename MovingResamplerType::Pointer movingResampler = MovingResamplerType::New();
  movingResampler->SetTransform( this->m_CompositeTransform );
  movingResampler->SetInput( this->m_MovingSmoothImage );
  movingResampler->SetSize( this->m_FixedSmoothImage->GetLargestPossibleRegion().GetSize() );
  movingResampler->SetOutputOrigin( this->m_FixedSmoothImage->GetOrigin() );
  movingResampler->SetOutputSpacing( this->m_FixedSmoothImage->GetSpacing() );
  movingResampler->SetOutputDirection( this->m_FixedSmoothImage->GetDirection() );
  movingResampler->SetDefaultPixelValue( 0 );

  typename MovingImageType::Pointer movingSmoothWarpedImage = movingResampler->GetOutput();
  movingSmoothWarpedImage->Update();
  movingSmoothWarpedImage->DisconnectPipeline();

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<double> ConvergenceMonitoringType;
  ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( 10 );

  SizeValueType iteration = 0;
  bool isConverged = false;
  while( iteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !isConverged )
    {
    std::cout << "    Iteration " << iteration << std::flush;

    // Compute the update fields (to both moving and fixed images) and smooth

    DisplacementFieldPointer middleToFixedUpdateField = this->ComputeUpdateField(
      this->m_FixedSmoothImage, this->m_MiddleToFixedTransform->GetInverseTransform(),
      movingSmoothWarpedImage, this->m_MiddleToMovingTransform->GetInverseTransform() );
    DisplacementFieldPointer middleToMovingUpdateField = this->ComputeUpdateField(
      movingSmoothWarpedImage, this->m_MiddleToMovingTransform->GetInverseTransform(),
      this->m_FixedSmoothImage, this->m_MiddleToFixedTransform->GetInverseTransform() );

    DisplacementFieldPointer middleToFixedSmoothUpdateField = this->GaussianSmoothDisplacementField( middleToFixedUpdateField, this->m_GaussianSmoothingVarianceForTheUpdateField );
    DisplacementFieldPointer middleToMovingSmoothUpdateField = this->GaussianSmoothDisplacementField( middleToMovingUpdateField, this->m_GaussianSmoothingVarianceForTheUpdateField );

    // Add the update field to both displacement fields (from fixed/moving to middle image) and then smooth

    typedef ComposeDisplacementFieldsImageFilter<DisplacementFieldType> ComposerType;

    typename ComposerType::Pointer fixedComposer = ComposerType::New();
    fixedComposer->SetDisplacementField( middleToFixedSmoothUpdateField );
    fixedComposer->SetWarpingField( this->m_MiddleToFixedTransform->GetDisplacementField() );
    fixedComposer->Update();

    DisplacementFieldPointer middleToFixedSmoothTotalField = this->GaussianSmoothDisplacementField( fixedComposer->GetOutput(), this->m_GaussianSmoothingVarianceForTheTotalField );

    typename ComposerType::Pointer movingComposer = ComposerType::New();
    movingComposer->SetDisplacementField( middleToMovingSmoothUpdateField );
    movingComposer->SetWarpingField( this->m_MiddleToMovingTransform->GetDisplacementField() );
    movingComposer->Update();

    DisplacementFieldPointer middleToMovingSmoothTotalField = this->GaussianSmoothDisplacementField( movingComposer->GetOutput(), this->m_GaussianSmoothingVarianceForTheTotalField );

    // Invert both total fields and iteratively estimate the inverse.

    typedef InvertDisplacementFieldImageFilter<DisplacementFieldType> InverterType;

    typename InverterType::Pointer fixedInverter = InverterType::New();
    fixedInverter->SetInput( middleToFixedSmoothTotalField );
    fixedInverter->SetMaximumNumberOfIterations( 20 );
    fixedInverter->SetMeanErrorToleranceThreshold( 0.001 );
    fixedInverter->SetMaxErrorToleranceThreshold( 0.1 );

    DisplacementFieldPointer middleToFixedSmoothTotalFieldInverse = fixedInverter->GetOutput();
    middleToFixedSmoothTotalFieldInverse->Update();
    middleToFixedSmoothTotalFieldInverse->DisconnectPipeline();

    typename InverterType::Pointer movingInverter = InverterType::New();
    movingInverter->SetInput( middleToMovingSmoothTotalField );
    movingInverter->SetMaximumNumberOfIterations( 20 );
    movingInverter->SetMeanErrorToleranceThreshold( 0.001 );
    movingInverter->SetMaxErrorToleranceThreshold( 0.1 );

    DisplacementFieldPointer middleToMovingSmoothTotalFieldInverse = movingInverter->GetOutput();
    middleToMovingSmoothTotalFieldInverse->Update();
    middleToMovingSmoothTotalFieldInverse->DisconnectPipeline();

    // We estimate the inverse of the inverse and set the total fields.

    fixedInverter->SetInput( middleToFixedSmoothTotalFieldInverse );
    fixedInverter->SetMaximumNumberOfIterations( 20 );
    fixedInverter->SetMeanErrorToleranceThreshold( 0.001 );
    fixedInverter->SetMaxErrorToleranceThreshold( 0.1 );

    middleToFixedSmoothTotalField = fixedInverter->GetOutput();
    middleToFixedSmoothTotalField->Update();
    middleToFixedSmoothTotalField->DisconnectPipeline();

    movingInverter->SetInput( middleToMovingSmoothTotalFieldInverse );
    movingInverter->SetMaximumNumberOfIterations( 20 );
    movingInverter->SetMeanErrorToleranceThreshold( 0.001 );
    movingInverter->SetMaxErrorToleranceThreshold( 0.1 );

    middleToMovingSmoothTotalField = movingInverter->GetOutput();
    middleToMovingSmoothTotalField->Update();
    middleToMovingSmoothTotalField->DisconnectPipeline();

    // Assign the displacement fields and their inverses to the proper transforms.

    this->m_MiddleToFixedTransform->SetDisplacementField( middleToFixedSmoothTotalField );
    this->m_MiddleToFixedTransform->SetInverseDisplacementField( middleToFixedSmoothTotalFieldInverse );

    this->m_MiddleToMovingTransform->SetDisplacementField( middleToMovingSmoothTotalField );
    this->m_MiddleToMovingTransform->SetInverseDisplacementField( middleToMovingSmoothTotalFieldInverse );

    // Get the metric value

    this->m_Metric->SetFixedImage( this->m_FixedSmoothImage );
    this->m_Metric->SetFixedTransform( this->m_MiddleToFixedTransform->GetInverseTransform() );
    this->m_Metric->SetMovingImage( movingSmoothWarpedImage );
    this->m_Metric->SetMovingTransform( this->m_MiddleToMovingTransform->GetInverseTransform() );
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

template<typename TFixedImage, typename TMovingImage, typename TTransform>
typename SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>::DisplacementFieldPointer
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::ComputeUpdateField( const FixedImageType * fixedImage, const TransformBaseType * fixedTransform, const MovingImageType * movingImage, const TransformBaseType * movingTransform )
{
  typename VirtualImageType::ConstPointer virtualDomainImage = this->m_Metric->GetVirtualDomainImage();

  // pre calculate the voxel distance to be used in properly scaling the gradient.
  RealType voxelDistance = 0.0;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    voxelDistance += vnl_math_sqr( virtualDomainImage->GetSpacing()[d] );
    }
  voxelDistance = vcl_sqrt( voxelDistance );

  this->m_Metric->SetFixedImage( fixedImage );
  this->m_Metric->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
  this->m_Metric->SetMovingImage( movingImage );
  this->m_Metric->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );
  this->m_Metric->Initialize();

  typename MetricType::MeasureType value;
  typename MetricType::DerivativeType metricDerivative;
  this->m_Metric->GetValueAndDerivative( value, metricDerivative );

  // we rescale the update velocity field at each time point.
  // we first need to convert to a displacement field to look
  // at the max norm of the field.

  const SizeValueType numberOfPixels = static_cast<SizeValueType>( metricDerivative.Size() / ImageDimension );
  const bool importFilterWillReleaseMemory = false;

  // Brad L. says I should feel bad about using a reinterpret_cast.
  // I do feel bad.

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

  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( importer->GetOutput() );
  duplicator->Update();

  DisplacementFieldPointer updateField = duplicator->GetOutput();
  updateField->DisconnectPipeline();

  return updateField;
}

template<typename TFixedImage, typename TMovingImage, typename TTransform>
typename SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>::DisplacementFieldPointer
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::GaussianSmoothDisplacementField( const DisplacementFieldType * field, const RealType variance )
{
  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  DisplacementFieldPointer smoothField = duplicator->GetOutput();

  if( variance <= 0.0 )
    {
    return smoothField;
    }

  typedef GaussianOperator<RealType, ImageDimension> GaussianSmoothingOperatorType;
  GaussianSmoothingOperatorType gaussianSmoothingOperator;

  typedef VectorNeighborhoodOperatorImageFilter<DisplacementFieldType, DisplacementFieldType> GaussianSmoothingSmootherType;
  typename GaussianSmoothingSmootherType::Pointer smoother = GaussianSmoothingSmootherType::New();

  for( unsigned int dimension = 0; dimension < ImageDimension; ++dimension )
    {
    // smooth along this dimension
    gaussianSmoothingOperator.SetDirection( dimension );
    gaussianSmoothingOperator.SetVariance( variance );
    gaussianSmoothingOperator.SetMaximumError( 0.001 );
    gaussianSmoothingOperator.SetMaximumKernelWidth( smoothField->GetRequestedRegion().GetSize()[dimension] );
    gaussianSmoothingOperator.CreateDirectional();

    // todo: make sure we only smooth within the buffered region
    smoother->SetOperator( gaussianSmoothingOperator );
    smoother->SetInput( smoothField );
    try
      {
      smoother->Update();
      }
    catch( ExceptionObject & exc )
      {
      std::string msg( "Caught exception: " );
      msg += exc.what();
      itkExceptionMacro( << msg );
      }

    smoothField = smoother->GetOutput();
    smoothField->Update();
    smoothField->DisconnectPipeline();
    }

  const DisplacementVectorType zeroVector( 0.0 );

  //make sure boundary does not move
  RealType weight1 = 1.0;
  if( variance < 0.5 )
    {
    weight1 = 1.0 - 1.0 * ( variance / 0.5 );
    }
  RealType weight2 = 1.0 - weight1;

  const typename DisplacementFieldType::RegionType region = field->GetLargestPossibleRegion();
  const typename DisplacementFieldType::SizeType size = region.GetSize();
  const typename DisplacementFieldType::IndexType startIndex = region.GetIndex();

  ImageRegionConstIteratorWithIndex<DisplacementFieldType> ItF( field, field->GetLargestPossibleRegion() );
  ImageRegionIteratorWithIndex<DisplacementFieldType> ItS( smoothField, smoothField->GetLargestPossibleRegion() );
  for( ItF.GoToBegin(), ItS.GoToBegin(); !ItF.IsAtEnd(); ++ItF, ++ItS )
    {
    typename DisplacementFieldType::IndexType index = ItF.GetIndex();
    bool isOnBoundary = false;
    for ( unsigned int dimension = 0; dimension < ImageDimension; ++dimension )
      {
      if( index[dimension] == startIndex[dimension] || index[dimension] == static_cast<IndexValueType>( size[dimension] ) - startIndex[dimension] - 1 )
        {
        isOnBoundary = true;
        break;
        }
      }
    if( isOnBoundary )
      {
      ItS.Set( zeroVector );
      }
    else
      {
      ItS.Set( ItS.Get() * weight1 + ItF.Get() * weight2 );
      }
    }

  return smoothField;
}

/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
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

  typedef ComposeDisplacementFieldsImageFilter<DisplacementFieldType, DisplacementFieldType> ComposerType;

  typename ComposerType::Pointer composer = ComposerType::New();
  composer->SetDisplacementField( this->m_MiddleToMovingTransform->GetInverseDisplacementField() );
  composer->SetWarpingField( this->m_MiddleToFixedTransform->GetDisplacementField() );
  composer->Update();

  typename ComposerType::Pointer inverseComposer = ComposerType::New();
  inverseComposer->SetDisplacementField( this->m_MiddleToFixedTransform->GetInverseDisplacementField() );
  inverseComposer->SetWarpingField( this->m_MiddleToMovingTransform->GetDisplacementField() );
  inverseComposer->Update();

  this->m_Transform->SetDisplacementField( composer->GetOutput() );
  this->m_Transform->SetInverseDisplacementField( inverseComposer->GetOutput() );

  TransformOutputPointer transformDecorator = TransformOutputType::New().GetPointer();
  transformDecorator->Set( this->m_Transform );
  this->ProcessObject::SetNthOutput( 0, transformDecorator );
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform>
void
SyNImageRegistrationMethod<TFixedImage, TMovingImage, TTransform>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Number of iterations: " << this->m_NumberOfIterationsPerLevel << std::endl;
  os << indent << "Learning rate: " << this->m_LearningRate << std::endl;
  os << indent << "Gaussian smoothing variance for the update field: " << this->m_GaussianSmoothingVarianceForTheUpdateField << std::endl;
  os << indent << "Gaussian smoothing variance for the total field: " << this->m_GaussianSmoothingVarianceForTheTotalField << std::endl;
}

} // end namespace itk

#endif
