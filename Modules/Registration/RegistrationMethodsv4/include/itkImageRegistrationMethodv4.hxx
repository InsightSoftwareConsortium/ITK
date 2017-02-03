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
#ifndef itkImageRegistrationMethodv4_hxx
#define itkImageRegistrationMethodv4_hxx

#include "itkImageRegistrationMethodv4.h"

#include "itkDiscreteGaussianImageFilter.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageToImageMetricv4.h"
#include "itkIterationReporter.h"
#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::ImageRegistrationMethodv4()
{
  ProcessObject::SetNumberOfRequiredOutputs( 1 );
  Self::SetPrimaryOutputName( "Transform" );

  // indexed input are alternating fixed and moving images
  Self::SetPrimaryInputName( "Fixed" );
  Self::AddRequiredInputName( "Moving", 1 );
  ProcessObject::SetNumberOfRequiredInputs( 2 );

  // optional named inputs
  Self::SetInput( "InitialTransform", ITK_NULLPTR );
  Self::SetInput( "FixedInitialTransform", ITK_NULLPTR );
  Self::SetInput( "MovingInitialTransform", ITK_NULLPTR );

  this->m_VirtualDomainImage = ITK_NULLPTR;

  Self::ReleaseDataBeforeUpdateFlagOff();

  this->m_CurrentLevel = 0;
  this->m_CurrentIteration = 0;
  this->m_CurrentMetricValue = 0.0;
  this->m_CurrentConvergenceValue = 0.0;
  this->m_IsConverged = false;
  this->m_NumberOfFixedObjects = 0;
  this->m_NumberOfMovingObjects = 0;

  Self::ReleaseDataBeforeUpdateFlagOff();

  this->m_InPlace = true;

  this->m_InitializeCenterOfLinearOutputTransform = true;

  this->m_CompositeTransform = CompositeTransformType::New();

  typedef MattesMutualInformationImageToImageMetricv4<FixedImageType, MovingImageType, VirtualImageType, RealType> DefaultMetricType;
  typename DefaultMetricType::Pointer mutualInformationMetric = DefaultMetricType::New();
  mutualInformationMetric->SetNumberOfHistogramBins( 20 );
  mutualInformationMetric->SetUseMovingImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedImageGradientFilter( false );
  mutualInformationMetric->SetUseFixedSampledPointSet( false );
  this->m_Metric = mutualInformationMetric;

  typedef RegistrationParameterScalesFromPhysicalShift<DefaultMetricType> DefaultScalesEstimatorType;
  typename DefaultScalesEstimatorType::Pointer scalesEstimator = DefaultScalesEstimatorType::New();
  scalesEstimator->SetMetric( mutualInformationMetric );
  scalesEstimator->SetTransformForward( true );

  typedef GradientDescentOptimizerv4Template<RealType> DefaultOptimizerType;
  typename DefaultOptimizerType::Pointer optimizer = DefaultOptimizerType::New();
  optimizer->SetLearningRate( 1.0 );
  optimizer->SetNumberOfIterations( 1000 );
  optimizer->SetScalesEstimator( scalesEstimator );
  this->m_Optimizer = optimizer;

  this->m_OptimizerWeights.SetSize( 0 );
  this->m_OptimizerWeightsAreIdentity = true;

  DecoratedOutputTransformPointer transformDecorator =
        itkDynamicCastInDebugMode< DecoratedOutputTransformType * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNthOutput( 0, transformDecorator );
  this->m_OutputTransform = transformDecorator->GetModifiable();

  // By default we set up a 3-level image registration.

  this->m_NumberOfLevels = 0;
  this->SetNumberOfLevels( 3 );

  this->m_ShrinkFactorsPerLevel.resize( this->m_NumberOfLevels );
  ShrinkFactorsPerDimensionContainerType shrinkFactors;
  shrinkFactors.Fill( 2 );
  this->m_ShrinkFactorsPerLevel[0] = shrinkFactors;
  shrinkFactors.Fill( 1 );
  this->m_ShrinkFactorsPerLevel[1] = shrinkFactors;
  shrinkFactors.Fill( 1 );
  this->m_ShrinkFactorsPerLevel[2] = shrinkFactors;

  this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
  this->m_SmoothingSigmasPerLevel[0] = 2;
  this->m_SmoothingSigmasPerLevel[1] = 1;
  this->m_SmoothingSigmasPerLevel[2] = 0;

  this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits = true;

  this->m_ReseedIterator = false;
  this->m_RandomSeed = Statistics::MersenneTwisterRandomVariateGenerator::GetNextSeed();
  this->m_CurrentRandomSeed = this->m_RandomSeed;

  this->m_MetricSamplingStrategy = NONE;
  this->m_MetricSamplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
  this->m_MetricSamplingPercentagePerLevel.Fill( 1.0 );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::~ImageRegistrationMethodv4()
{
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetFixedImage( SizeValueType index, const FixedImageType *image )
{
  itkDebugMacro( "setting fixed image input " << index << " to " << image );
  if( image != static_cast<FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index ) )
      {
      this->m_NumberOfFixedObjects++;
      }
    this->ProcessObject::SetNthInput( 2 * index, const_cast<FixedImageType *>( image ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::FixedImageType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetFixedImage( SizeValueType index ) const
{
  itkDebugMacro( "returning fixed image input " << index << " of "
                                    << static_cast<const FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) ) );
  return static_cast<const FixedImageType *>( this->ProcessObject::GetInput( 2 * index ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetMovingImage( SizeValueType index, const MovingImageType *image )
{
  itkDebugMacro( "setting moving image input " << index << " to " << image );
  if( image != static_cast<MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index + 1 ) )
      {
      this->m_NumberOfMovingObjects++;
      }
    this->ProcessObject::SetNthInput( 2 * index + 1, const_cast<MovingImageType *>( image ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::MovingImageType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetMovingImage( SizeValueType index ) const
{
  itkDebugMacro( "returning moving image input " << index << " of "
                                    << static_cast<const MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) );
  return static_cast<const MovingImageType *>( this->ProcessObject::GetInput( 2 * index + 1 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetFixedPointSet( SizeValueType index, const PointSetType *pointSet )
{
  itkDebugMacro( "setting fixed point set input " << index << " to " << pointSet );
  if( pointSet != static_cast<PointSetType *>( this->ProcessObject::GetInput( 2 * index ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index ) )
      {
      this->m_NumberOfFixedObjects++;
      }
    this->ProcessObject::SetNthInput( 2 * index, const_cast<PointSetType *>( pointSet ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::PointSetType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetFixedPointSet( SizeValueType index ) const
{
  itkDebugMacro( "returning fixed point set input " << index << " of "
                                    << static_cast<const PointSetType *>( this->ProcessObject::GetInput( 2 * index ) ) );
  return static_cast<const PointSetType *>( this->ProcessObject::GetInput( 2 * index ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetMovingPointSet( SizeValueType index, const PointSetType *pointSet )
{
  itkDebugMacro( "setting moving point set input " << index << " to " << pointSet );
  if( pointSet != static_cast<PointSetType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) )
    {
    if( !this->ProcessObject::GetInput( 2 * index + 1 ) )
      {
      this->m_NumberOfMovingObjects++;
      }
    this->ProcessObject::SetNthInput( 2 * index + 1, const_cast<PointSetType *>( pointSet ) );
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::PointSetType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetMovingPointSet( SizeValueType index ) const
{
  itkDebugMacro( "returning moving point set input " << index << " of "
                                    << static_cast<const PointSetType *>( this->ProcessObject::GetInput( 2 * index + 1 ) ) );
  return static_cast<const PointSetType *>( this->ProcessObject::GetInput( 2 * index + 1 ) );
}

/*
 * Set optimizer weights and do checking for identity.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetOptimizerWeights( OptimizerWeightsType & weights )
{
  if( weights != this->m_OptimizerWeights )
    {
    itkDebugMacro( "setting optimizer weights to " << weights );

    this->m_OptimizerWeights = weights;

    // Check to see if optimizer weights are identity to avoid unnecessary
    // computations.

    this->m_OptimizerWeightsAreIdentity = true;
    if( this->m_OptimizerWeights.Size() > 0 )
      {
      typedef typename OptimizerWeightsType::ValueType OptimizerWeightsValueType;
      OptimizerWeightsValueType tolerance = static_cast<OptimizerWeightsValueType>( 1e-4 );

      for( SizeValueType i = 0; i < this->m_OptimizerWeights.Size(); i++ )
        {
        OptimizerWeightsValueType difference =
          std::fabs( NumericTraits<OptimizerWeightsValueType>::OneValue() - this->m_OptimizerWeights[i] );
        if( difference > tolerance  )
          {
          this->m_OptimizerWeightsAreIdentity = false;
          break;
          }
        }
      }
    this->Modified();
    }
}

/*
 * Initialize by setting the interconnects between components.
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{

  // To avoid casting to a multimetric several times, we do it once and use it
  // throughout this function if the current enumerated metric type is MULTI_METRIC
  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );

  // Sanity checks and find the virtual domain image

  if( level == 0 )
    {
    SizeValueType numberOfObjectPairs = static_cast<unsigned int>( 0.5 * this->GetNumberOfIndexedInputs() );
    if( numberOfObjectPairs == 0 )
      {
      itkExceptionMacro( "There are no input objects." );
      }

    if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
      {
      this->m_NumberOfMetrics = multiMetric->GetNumberOfMetrics();
      if( this->m_NumberOfMetrics != numberOfObjectPairs )
        {
        itkExceptionMacro( "Mismatch between number of image pairs and the number of metrics." );
        }
      }
    else
      {
      this->m_NumberOfMetrics = 1;
      }

    // The number of image pairs also includes ITK_NULLPTR image pairs for the point set
    // metrics
    if( this->m_NumberOfFixedObjects != this->m_NumberOfMovingObjects )
      {
      itkExceptionMacro( "The number of fixed and moving images is not equal." );
      }
    }

  if( !this->m_Optimizer )
    {
    itkExceptionMacro( "The optimizer is not present." );
    }
  if( !this->m_Metric )
    {
    itkExceptionMacro( "The metric is not present." );
    }

  InitialTransformType* movingInitialTransform = const_cast<InitialTransformType*>( this->GetMovingInitialTransform() );
  InitialTransformType* fixedInitialTransform = const_cast<InitialTransformType*>( this->GetFixedInitialTransform() );

  this->m_CurrentIteration = 0;
  this->m_CurrentMetricValue = 0.0;
  this->m_CurrentConvergenceValue = 0.0;
  this->m_IsConverged = false;

  this->InvokeEvent( MultiResolutionIterationEvent() );

  // For each level, we adapt the current transform.  For many transforms, e.g.
  // affine, the base transform adaptor does not do anything.  However, in the
  // case of other transforms, e.g. the b-spline and displacement field transforms
  // the fixed parameters are changed to reflect an increase in transform resolution.
  // This could involve increasing the mesh size of the B-spline transform or
  // increase the resolution of the displacement field.

  if( this->m_TransformParametersAdaptorsPerLevel[level] )
    {
    this->m_TransformParametersAdaptorsPerLevel[level]->SetTransform( this->m_OutputTransform );
    this->m_TransformParametersAdaptorsPerLevel[level]->AdaptTransformParameters();
    }

  // Set-up the composite transform at initialization
  // Also, find the virtual domain image
  if( level == 0 )
    {
    this->m_CompositeTransform->ClearTransformQueue();

    // Since we cannot instantiate a null object from an abstract class, we need to initialize the moving
    // initial transform as an identity transform.
    // Nevertheless, we do not need add this transform to the composite transform when it is only an
    // identity transform. Simply by not setting that, we can save lots of time in jacobian computations
    // of the composite transform since we can avoid some matrix multiplications.

    // Skip adding an IdentityTransform to the m_CompositeTransform
    if( movingInitialTransform != ITK_NULLPTR &&
      std::string( movingInitialTransform->GetNameOfClass() ) != std::string( "IdentityTransform" ) )
      {
      this->m_CompositeTransform->AddTransform( movingInitialTransform );
      }

    // If the moving initial transform is a composite transform, unroll
    // it into m_CompositeTransform.
    this->m_CompositeTransform->FlattenTransformQueue();

    if( this->m_InitializeCenterOfLinearOutputTransform )
      {
      this->InitializeCenterOfLinearOutputTransform();
      }
    this->m_CompositeTransform->AddTransform( this->m_OutputTransform );

    if( this->m_OptimizerWeights.Size() > 0 )
      {
      this->m_Optimizer->SetWeights( this->m_OptimizerWeights );
      }

    // Get index of first image metric
    this->m_FirstImageMetricIndex = -1;
    if( this->m_NumberOfMetrics == 1 && this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
      {
      this->m_FirstImageMetricIndex = 0;
      }
    else if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
      {
      for( SizeValueType n = 0; n < this->m_NumberOfMetrics; n++ )
        {
        if( multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::IMAGE_METRIC )
          {
          this->m_FirstImageMetricIndex = n;
          break;
          }
        }
      }

    {
    VirtualImageBaseConstPointer virtualDomainBaseImage = this->GetCurrentLevelVirtualDomainImage();

    if( virtualDomainBaseImage.IsNull() && this->m_FirstImageMetricIndex >= 0 )
      {
      virtualDomainBaseImage =  this->GetFixedImage( this->m_FirstImageMetricIndex );
      }
    this->m_VirtualDomainImage = VirtualImageType::New();
    this->m_VirtualDomainImage->CopyInformation( virtualDomainBaseImage );
    this->m_VirtualDomainImage->SetRegions( virtualDomainBaseImage->GetLargestPossibleRegion() );
    this->m_VirtualDomainImage->Allocate();
    }

    this->m_FixedImageMasks.clear();
    this->m_FixedImageMasks.resize( this->m_NumberOfMetrics );
    this->m_MovingImageMasks.clear();
    this->m_MovingImageMasks.resize( this->m_NumberOfMetrics );

    for( SizeValueType n = 0; n < this->m_NumberOfMetrics; n++ )
      {
      this->m_FixedImageMasks[n] = ITK_NULLPTR;
      this->m_MovingImageMasks[n] = ITK_NULLPTR;

      if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC ||
          ( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC &&
            multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::IMAGE_METRIC ) )
        {

        if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
          {
          this->m_FixedImageMasks[n] = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->GetFixedImageMask();
          this->m_MovingImageMasks[n] = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->GetMovingImageMask();
          }
        else if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
          {
          this->m_FixedImageMasks[n] = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->GetFixedImageMask();
          this->m_MovingImageMasks[n] = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->GetMovingImageMask();
          }
        else
          {
          itkExceptionMacro( "Invalid metric type." )
          }
        }
      }
    }
  this->m_CompositeTransform->SetOnlyMostRecentTransformToOptimizeOn();

  // At each resolution and for each image pair (assuming an image metric), we
  //   1. subsample the reference domain (typically the fixed image) and/or
  //   2. smooth the fixed and moving images.

  typename VirtualImageType::Pointer currentLevelVirtualDomainImage = ITK_NULLPTR;
  if( this->m_VirtualDomainImage.IsNotNull() )
    {
    typename ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( this->m_ShrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( this->m_VirtualDomainImage );

    currentLevelVirtualDomainImage = shrinkFilter->GetOutput();
    currentLevelVirtualDomainImage->Update();
    }
  else
    {
    itkExceptionMacro( "A virtual domain image is not found.  It should be specified in one of the metrics." );
    }

  if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
    {
    if( fixedInitialTransform )
      {
      multiMetric->SetFixedTransform( fixedInitialTransform );
      }
    else
      {
      typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
      typename IdentityTransformType::Pointer defaultFixedInitialTransform = IdentityTransformType::New();
      multiMetric->SetFixedTransform( defaultFixedInitialTransform );
      }
    multiMetric->SetMovingTransform( this->m_CompositeTransform );
    if( currentLevelVirtualDomainImage.IsNotNull() )
      {
      multiMetric->SetVirtualDomainFromImage( currentLevelVirtualDomainImage );
      }

    for( SizeValueType n = 0; n < multiMetric->GetNumberOfMetrics(); n++ )
      {
      if( multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::IMAGE_METRIC )
        {
        if( currentLevelVirtualDomainImage.IsNotNull() )
          {
          dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetVirtualDomainFromImage( currentLevelVirtualDomainImage );
          }
        else
          {
          itkExceptionMacro( "Virtual domain image is not specified." );
          }
        }
      else if( multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::POINT_SET_METRIC )
        {
        if( currentLevelVirtualDomainImage.IsNotNull() )
          {
          // This casting is a hack as all the metrics should be coordinated such that
          // they have identical virtual image types.

          typedef CastImageFilter<VirtualImageType, typename PointSetMetricType::VirtualImageType> CasterType;
          typename CasterType::Pointer caster = CasterType::New();
          caster->SetInput( currentLevelVirtualDomainImage );
          caster->Update();

          dynamic_cast<PointSetMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetVirtualDomainFromImage( caster->GetOutput() );
          }
        }
      }
    }
  else if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
    {
    typename ImageMetricType::Pointer imageMetric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( fixedInitialTransform )
      {
      imageMetric->SetFixedTransform( fixedInitialTransform );
      }
    else
      {
      typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
      typename IdentityTransformType::Pointer defaultFixedInitialTransform = IdentityTransformType::New();
      imageMetric->SetFixedTransform( defaultFixedInitialTransform );
      }
    imageMetric->SetMovingTransform( this->m_CompositeTransform );
    if( currentLevelVirtualDomainImage.IsNotNull() )
      {
      imageMetric->SetVirtualDomainFromImage( currentLevelVirtualDomainImage );
      }
    }
  else if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC )
    {
    typename PointSetMetricType::Pointer pointSetMetric =
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() );
    if( fixedInitialTransform )
      {
      pointSetMetric->SetFixedTransform( fixedInitialTransform );
      }
    else
      {
      typedef IdentityTransform<RealType, ImageDimension> IdentityTransformType;
      typename IdentityTransformType::Pointer defaultFixedInitialTransform = IdentityTransformType::New();
      pointSetMetric->SetFixedTransform( defaultFixedInitialTransform );
      }
    pointSetMetric->SetMovingTransform( this->m_CompositeTransform );
    if( currentLevelVirtualDomainImage.IsNotNull() )
      {
      // This casting is a hack as all the metrics should be coordinated such that
      // they have identical virtual image types.

      typedef CastImageFilter<VirtualImageType, typename PointSetMetricType::VirtualImageType> CasterType;
      typename CasterType::Pointer caster = CasterType::New();
      caster->SetInput( currentLevelVirtualDomainImage );
      caster->Update();

      pointSetMetric->SetVirtualDomainFromImage( caster->GetOutput() );
      }
    }
  else
    {
    itkExceptionMacro( "Invalid metric conversion." );
    }

  // We update the fixed and moving images for the image metrics and
  // the fixed and moving point sets for the point set metrics.  Note
  // that we set the point sets here just like we set the images.
  // Although this isn't necessary, we want to leave the option for
  // changing the point sets per level.

  this->m_FixedSmoothImages.clear();
  this->m_FixedSmoothImages.resize( this->m_NumberOfMetrics );
  this->m_MovingSmoothImages.clear();
  this->m_MovingSmoothImages.resize( this->m_NumberOfMetrics );
  this->m_FixedPointSets.clear();
  this->m_FixedPointSets.resize( this->m_NumberOfMetrics );
  this->m_MovingPointSets.clear();
  this->m_MovingPointSets.resize( this->m_NumberOfMetrics );

  for( SizeValueType n = 0; n < this->m_NumberOfMetrics; n++ )
    {
    this->m_FixedSmoothImages[n] = ITK_NULLPTR;
    this->m_MovingSmoothImages[n] = ITK_NULLPTR;
    this->m_FixedPointSets[n] = ITK_NULLPTR;
    this->m_MovingPointSets[n] = ITK_NULLPTR;

    if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC ||
        ( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC &&
          multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::IMAGE_METRIC ) )
      {
      typedef DiscreteGaussianImageFilter<FixedImageType, FixedImageType> FixedImageSmoothingFilterType;
      typename FixedImageSmoothingFilterType::Pointer fixedImageSmoothingFilter = FixedImageSmoothingFilterType::New();
      if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
        {
        fixedImageSmoothingFilter->SetUseImageSpacingOn();
        }
      else
        {
        fixedImageSmoothingFilter->SetUseImageSpacingOff();
        }
      fixedImageSmoothingFilter->SetVariance( itk::Math::sqr( this->m_SmoothingSigmasPerLevel[level] ) );
      fixedImageSmoothingFilter->SetMaximumError( 0.01 );
      fixedImageSmoothingFilter->SetInput( this->GetFixedImage( n ) );

      this->m_FixedSmoothImages[n] = fixedImageSmoothingFilter->GetOutput();
      this->m_FixedSmoothImages[n]->Update();
      this->m_FixedSmoothImages[n]->DisconnectPipeline();

      typedef DiscreteGaussianImageFilter<MovingImageType, MovingImageType> MovingImageSmoothingFilterType;
      typename MovingImageSmoothingFilterType::Pointer movingImageSmoothingFilter = MovingImageSmoothingFilterType::New();
      if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
        {
        movingImageSmoothingFilter->SetUseImageSpacingOn();
        }
      else
        {
        movingImageSmoothingFilter->SetUseImageSpacingOff();
        }
      movingImageSmoothingFilter->SetVariance( itk::Math::sqr( this->m_SmoothingSigmasPerLevel[level] ) );
      movingImageSmoothingFilter->SetMaximumError( 0.01 );
      movingImageSmoothingFilter->SetInput( this->GetMovingImage( n ) );

      this->m_MovingSmoothImages[n] = movingImageSmoothingFilter->GetOutput();
      this->m_MovingSmoothImages[n]->Update();
      this->m_MovingSmoothImages[n]->DisconnectPipeline();

      // Update the image metric

      if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
        {
        multiMetric->GetMetricQueue()[n]->SetFixedObject( this->m_FixedSmoothImages[n] );
        multiMetric->GetMetricQueue()[n]->SetMovingObject( this->m_MovingSmoothImages[n] );

        dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetFixedImageMask( this->m_FixedImageMasks[n] );
        dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetMovingImageMask( this->m_MovingImageMasks[n] );
        }
      else if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
        {
        this->m_Metric->SetFixedObject( this->m_FixedSmoothImages[n] );
        this->m_Metric->SetMovingObject( this->m_MovingSmoothImages[n] );

        dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedImageMask( this->m_FixedImageMasks[n] );
        dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetMovingImageMask( this->m_MovingImageMasks[n] );
        }
      else
        {
        itkExceptionMacro( "Invalid metric type." )
        }
      }
    else if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC ||
        ( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC &&
          multiMetric->GetMetricQueue()[n]->GetMetricCategory() == MetricType::POINT_SET_METRIC ) )
      {
      this->m_FixedPointSets[n] = this->GetFixedPointSet( n );
      this->m_MovingPointSets[n] = this->GetMovingPointSet( n );

      // Update the point set metric

      if( this->m_Metric->GetMetricCategory() == MetricType::MULTI_METRIC )
        {
        multiMetric->GetMetricQueue()[n]->SetFixedObject( this->GetFixedPointSet( n ) );
        multiMetric->GetMetricQueue()[n]->SetMovingObject( this->GetMovingPointSet( n ) );
        }
      else if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC )
        {
        this->m_Metric->SetFixedObject( this->GetFixedPointSet( n ) );
        this->m_Metric->SetMovingObject( this->GetMovingPointSet( n ) );
        }
      else
        {
        itkExceptionMacro( "Invalid metric type." )
        }
      }
    else
      {
      itkExceptionMacro( "Invalid metric type." )
      }
    }

  if( this->m_MetricSamplingStrategy != NONE )
    {
    this->SetMetricSamplePoints();
    }

  // Update the optimizer

  this->m_Optimizer->SetMetric( this->m_Metric );

  if( ( this->m_Optimizer->GetScales() ).Size() != this->m_OutputTransform->GetNumberOfLocalParameters() )
    {
    typedef typename OptimizerType::ScalesType ScalesType;
    ScalesType scales;
    scales.SetSize( this->m_OutputTransform->GetNumberOfLocalParameters() );
    scales.Fill( NumericTraits<typename ScalesType::ValueType>::OneValue() );
    this->m_Optimizer->SetScales( scales );
    }
}


template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::AllocateOutputs()
{
  const DecoratedInitialTransformType * decoratedInitialTransform = this->GetInitialTransformInput();
  DecoratedOutputTransformType *decoratedOutputTransform = this->GetOutput();

  if( decoratedInitialTransform )
    {
    const InitialTransformType * initialTransform = decoratedInitialTransform->Get();

    if( initialTransform )
      {
      if( this->GetInPlace() )
        {
        // graft the input to the output which may fail if the types
        // aren't compatible.
        decoratedOutputTransform->Graft( decoratedInitialTransform );

        if( decoratedOutputTransform->Get() )
          {
          this->m_OutputTransform = decoratedOutputTransform->GetModifiable();

          // This is generally done in the ReleaseInputs methods,
          // however we do not need it again
          const_cast<DecoratedInitialTransformType *>( decoratedInitialTransform )->ReleaseData();

          // successful in-place grafting
          itkDebugMacro( "inplace allocation of output transform" );
          return;
          }
        }

      const OutputTransformType * initialAsOutputTransform = dynamic_cast<const OutputTransformType*>( initialTransform );

      if( initialAsOutputTransform )
        {
        // Clone performs a deep copy of the parameters and composition
        this->m_OutputTransform = initialAsOutputTransform->Clone();
        decoratedOutputTransform->Set( this->m_OutputTransform );

        // successful deep copy from initial to output
        itkDebugMacro( "clone copy allocation of output transform" );
        return;
        }
      else
        {
        itkExceptionMacro( "Unable to convert InitialTransform input to the OutputTransform type" );
        }

      }
    }

  // fallback allocation and initialization


  // initialize to identity? what happens if we re-run with optimized values?
  itkDebugMacro( "fallback allocation of output transform" );

  if( !decoratedOutputTransform->Get() )
    {
    // the output decorated component is null, allocate
    OutputTransformPointer ptr;
    Self::MakeOutputTransform( ptr );
    decoratedOutputTransform->Set( ptr );
    }

  this->m_OutputTransform = this->GetModifiableTransform();
}

/*
 * Start the registration
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GenerateData()
{
  this->AllocateOutputs();

  // Ensure the same seed is used for each update
  this->m_CurrentRandomSeed = this->m_RandomSeed;

  for( this->m_CurrentLevel = 0; this->m_CurrentLevel < this->m_NumberOfLevels; this->m_CurrentLevel++ )
    {
    this->InitializeRegistrationAtEachLevel( this->m_CurrentLevel );

    this->m_Metric->Initialize();

    this->m_Optimizer->StartOptimization();
    }
}

/**
 * Set the moving transform adaptors per stage
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetTransformParametersAdaptorsPerLevel( TransformParametersAdaptorsContainerType & adaptors )
{
  if( this->m_NumberOfLevels != adaptors.size() )
    {
    itkExceptionMacro( "The number of levels does not equal the number array size." );
    }
  else
    {
    itkDebugMacro( "Setting the transform parameter adaptors." );
    this->m_TransformParametersAdaptorsPerLevel = adaptors;
    this->Modified();
    }
}

/**
 * Get the moving transform adaptors per stage
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename  ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::TransformParametersAdaptorsContainerType &
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetTransformParametersAdaptorsPerLevel() const
{
  return this->m_TransformParametersAdaptorsPerLevel;
}

/**
 * Set the number of levels
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetNumberOfLevels( const SizeValueType numberOfLevels )
{
  if( this->m_NumberOfLevels != numberOfLevels )
    {
    this->m_NumberOfLevels = numberOfLevels;

    // Set default transform adaptors which don't do anything to the input transform
    // Similarly, fill in some default values for the shrink factors, smoothing sigmas,
    // and learning rates.

    this->m_TransformParametersAdaptorsPerLevel.clear();
    for( SizeValueType level = 0; level < this->m_NumberOfLevels; level++ )
      {
      this->m_TransformParametersAdaptorsPerLevel.push_back( ITK_NULLPTR );
      }

    for( SizeValueType level = 0; level < this->m_NumberOfLevels; ++level )
      {
      ShrinkFactorsPerDimensionContainerType shrinkFactors;
      shrinkFactors.Fill( 1 );
      this->SetShrinkFactorsPerDimension( level, shrinkFactors );
      }

    this->m_SmoothingSigmasPerLevel.SetSize( this->m_NumberOfLevels );
    this->m_SmoothingSigmasPerLevel.Fill( 1.0 );

    this->m_MetricSamplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
    this->m_MetricSamplingPercentagePerLevel.Fill( 1.0 );

    this->Modified();
    }
}

/**
 * Get the metric samples
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetMetricSamplePoints()
{
  typedef typename ImageMetricType::VirtualImageType    VirtualDomainImageType;
  typedef typename VirtualDomainImageType::RegionType   VirtualDomainRegionType;

  const VirtualDomainImageType * virtualImage = ITK_NULLPTR;
  const FixedImageMaskType * fixedMaskImage = ITK_NULLPTR;

  SizeValueType numberOfLocalMetrics = 1;

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    numberOfLocalMetrics = multiMetric->GetNumberOfMetrics();
    if( numberOfLocalMetrics < 1 )
      {
      itkExceptionMacro( "Input multi metric should have at least one metric component." );
      }
    else
      {
      typename ImageMetricType::Pointer firstMetric = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
      if( firstMetric.IsNotNull() )
        {
        virtualImage = firstMetric->GetVirtualImage();
        fixedMaskImage = firstMetric->GetFixedImageMask();
        }
      else
        {
        itkExceptionMacro( "Invalid metric conversion." );
        }
      }
    }
  else
    {
    typename ImageMetricType::Pointer singleMetric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( singleMetric.IsNotNull() )
      {
      virtualImage = singleMetric->GetVirtualImage();
      fixedMaskImage = singleMetric->GetFixedImageMask();
      }
    else
      {
      itkExceptionMacro( "Invalid metric conversion." );
      }
    }

  const VirtualDomainRegionType & virtualDomainRegion = virtualImage->GetRequestedRegion();
  const typename VirtualDomainImageType::SpacingType oneThirdVirtualSpacing = virtualImage->GetSpacing() / 3.0;

  for( SizeValueType n = 0; n < numberOfLocalMetrics; n++ )
    {
    typename MetricSamplePointSetType::Pointer samplePointSet = MetricSamplePointSetType::New();
    samplePointSet->Initialize();

    typedef typename MetricSamplePointSetType::PointType SamplePointType;

    typedef typename Statistics::MersenneTwisterRandomVariateGenerator RandomizerType;
    typename RandomizerType::Pointer randomizer = RandomizerType::New();
    if (m_ReseedIterator)
      {
      randomizer->SetSeed( );
      }
    else
      {
      randomizer->SetSeed( m_CurrentRandomSeed++ );
      }


    unsigned long index = 0;

    switch( this->m_MetricSamplingStrategy )
      {
      case REGULAR:
        {
        const unsigned long sampleCount = static_cast<unsigned long>( std::ceil( 1.0 / this->m_MetricSamplingPercentagePerLevel[this->m_CurrentLevel] ) );
        unsigned long count = sampleCount; //Start at sampleCount to keep behavior backwards identical, using first element.
        ImageRegionConstIteratorWithIndex<VirtualDomainImageType> It( virtualImage, virtualDomainRegion );
        for( It.GoToBegin(); !It.IsAtEnd(); ++It )
          {
          if( count == sampleCount )
            {
            count=0; //Reset counter
            SamplePointType point;
            virtualImage->TransformIndexToPhysicalPoint( It.GetIndex(), point );

            // randomly perturb the point within a voxel (approximately)
            for( SizeValueType d = 0; d < ImageDimension; d++ )
              {
              point[d] += randomizer->GetNormalVariate() * oneThirdVirtualSpacing[d];
              }
            if( !fixedMaskImage || fixedMaskImage->IsInside( point ) )
              {
              samplePointSet->SetPoint( index, point );
              ++index;
              }
            }
          ++count;
          }
        break;
        }
      case RANDOM:
        {
        const unsigned long totalVirtualDomainVoxels = virtualDomainRegion.GetNumberOfPixels();
        const unsigned long sampleCount = static_cast<unsigned long>( static_cast<float>( totalVirtualDomainVoxels ) * this->m_MetricSamplingPercentagePerLevel[this->m_CurrentLevel] );
        ImageRandomConstIteratorWithIndex<VirtualDomainImageType> ItR( virtualImage, virtualDomainRegion );
        if (m_ReseedIterator)
          {
          ItR.ReinitializeSeed();
          }
        else
          {
          ItR.ReinitializeSeed( m_CurrentRandomSeed++ );
          }
        ItR.SetNumberOfSamples( sampleCount );
        for( ItR.GoToBegin(); !ItR.IsAtEnd(); ++ItR )
          {
          SamplePointType point;
          virtualImage->TransformIndexToPhysicalPoint( ItR.GetIndex(), point );

          // randomly perturb the point within a voxel (approximately)
          for ( unsigned int d = 0; d < ImageDimension; d++ )
            {
            point[d] += randomizer->GetNormalVariate() * oneThirdVirtualSpacing[d];
            }
          if( !fixedMaskImage || fixedMaskImage->IsInside( point ) )
            {
            samplePointSet->SetPoint( index, point );
            ++index;
            }
          }
        break;
        }
      default:
        {
        itkExceptionMacro( "Invalid sampling strategy requested." );
        }
      }

    if( multiMetric )
      {
      dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetFixedSampledPointSet( samplePointSet );
      dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[n].GetPointer() )->SetUseFixedSampledPointSet( true );
      }
    else
      {
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetFixedSampledPointSet( samplePointSet );
      dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->SetUseFixedSampledPointSet( true );
      }
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::InitializeCenterOfLinearOutputTransform()
{
  typedef MatrixOffsetTransformBase<typename OutputTransformType::ScalarType,
    ImageDimension, ImageDimension> MatrixOffsetTransformType;

  MatrixOffsetTransformType *matrixOffsetOutputTransform =
    dynamic_cast<MatrixOffsetTransformType *>( this->GetModifiableTransform() );

  if( ! matrixOffsetOutputTransform )
    {
    return;
    }

  SizeValueType numberOfTransforms = this->m_CompositeTransform->GetNumberOfTransforms();

  if( numberOfTransforms == 0 )
    {
    return;
    }

  typename TTransform::InputPointType center = matrixOffsetOutputTransform->GetCenter();

  bool optimalIndexFound = false;

  for( int i = numberOfTransforms - 1; i >= 0; i-- )
    {
    MatrixOffsetTransformType *matrixOffsetTransform =
      dynamic_cast<MatrixOffsetTransformType *>( this->m_CompositeTransform->GetNthTransformModifiablePointer( i ) );
    if( matrixOffsetTransform )
      {
      center = matrixOffsetTransform->GetCenter();
      optimalIndexFound = true;
      break;
      }
    }

  if( ! optimalIndexFound )
    {
    return;
    }
  else
    {
    matrixOffsetOutputTransform->SetCenter( center );
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::VirtualImageBaseConstPointer
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetCurrentLevelVirtualDomainImage()
{
  // Get virtual domain image
  VirtualImageBaseConstPointer currentLevelVirtualDomainImage = ITK_NULLPTR;
  if( this->m_Metric->GetMetricCategory() == MetricType::IMAGE_METRIC )
    {
    currentLevelVirtualDomainImage = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() )->GetVirtualImage();
    }
  else if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC )
    {
    currentLevelVirtualDomainImage = dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->GetVirtualImage();
    }
  else
    {
    typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
    if( multiMetric->GetMetricQueue()[0]->GetMetricCategory() == MetricType::POINT_SET_METRIC )
      {
      currentLevelVirtualDomainImage = dynamic_cast<PointSetMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() )->GetVirtualImage();
      }
    else
      {
      currentLevelVirtualDomainImage = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() )->GetVirtualImage();
      }
    }

  return currentLevelVirtualDomainImage;
}

/*
 * PrintSelf
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  Indent indent2 = indent.GetNextIndent();

  os << indent << "Number of levels = " << this->m_NumberOfLevels << std::endl;

  for( SizeValueType level = 0; level < this->m_NumberOfLevels; ++level )
    {
    os << indent << "Shrink factors (level " << level << "): "
       << this->m_ShrinkFactorsPerLevel[level] << std::endl;
    }
  os << indent << "Smoothing sigmas: " << this->m_SmoothingSigmasPerLevel << std::endl;

  if( this->m_SmoothingSigmasAreSpecifiedInPhysicalUnits == true )
    {
    os << indent2 << "Smoothing sigmas are specified in physical units." << std::endl;
    }
  else
    {
    os << indent2 << "Smoothing sigmas are specified in voxel units." << std::endl;
    }

  if( this->m_OptimizerWeights.Size() > 0 )
    {
    os << indent << "Optimizers weights: " << this->m_OptimizerWeights << std::endl;
    }

  os << indent << "Metric sampling strategy: " << this->m_MetricSamplingStrategy << std::endl;

  os << indent << "Metric sampling percentage: ";
  for( SizeValueType i = 0; i < this->m_NumberOfLevels; i++ )
    {
    os << this->m_MetricSamplingPercentagePerLevel[i] << " ";
    }
  os << std::endl;

  os << indent << "ReseedIterator: " << m_ReseedIterator << std::endl;
  os << indent << "RandomSeed: " << m_RandomSeed << std::endl;
  os << indent << "CurrentRandomSeed: " << m_CurrentRandomSeed << std::endl;

  os << indent << "InPlace: " << ( this->m_InPlace ? "On" : "Off" ) << std::endl;

  os << indent << "InitializeCenterOfLinearOutputTransform: "
     << ( m_InitializeCenterOfLinearOutputTransform ? "On" : "Off" ) << std::endl;
}

/*
 *  Get output transform
 */
template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::DecoratedOutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetOutput()
{
  return static_cast<DecoratedOutputTransformType *>( this->ProcessObject::GetOutput( 0 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::DecoratedOutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetOutput() const
{
  return static_cast<const DecoratedOutputTransformType *>( this->ProcessObject::GetOutput( 0 ) );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::OutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetModifiableTransform()
{
  DecoratedOutputTransformType * temp = this->GetOutput();
  // required outputs of process object should always exits
  itkAssertInDebugAndIgnoreInReleaseMacro( temp != ITK_NULLPTR );
  return temp->GetModifiable();
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
const typename ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>::OutputTransformType *
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::GetTransform() const
{
  const  DecoratedOutputTransformType * temp = this->GetOutput();
  // required outputs of process object should always exits
  itkAssertInDebugAndIgnoreInReleaseMacro( temp != ITK_NULLPTR );
  return temp->Get();
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
DataObject::Pointer
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::MakeOutput( DataObjectPointerArraySizeType output )
{
  switch ( output )
    {
    case 0:
      {
      OutputTransformPointer ptr;
      Self::MakeOutputTransform(ptr);
      DecoratedOutputTransformPointer transformDecorator =  DecoratedOutputTransformType::New();
      transformDecorator->Set( ptr );
      return transformDecorator.GetPointer();
      }
    default:
      itkExceptionMacro( "MakeOutput request for an output number larger than the expected number of outputs." );
      return ITK_NULLPTR;
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::MetricSamplingReinitializeSeed()
{
  if (!m_ReseedIterator)
    {
    m_ReseedIterator = true;
    this->Modified();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::MetricSamplingReinitializeSeed(int seed)
{
  if (m_ReseedIterator || m_RandomSeed != seed)
    {
    m_ReseedIterator = false;
    m_RandomSeed = seed;
    this->Modified();
    }
}


template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetMetricSamplingPercentage( const RealType samplingPercentage )
{
  MetricSamplingPercentageArrayType samplingPercentagePerLevel;
  samplingPercentagePerLevel.SetSize( this->m_NumberOfLevels );
  samplingPercentagePerLevel.Fill( samplingPercentage );
  this->SetMetricSamplingPercentagePerLevel( samplingPercentagePerLevel );
}

template<typename TFixedImage, typename TMovingImage, typename TTransform, typename TVirtualImage, typename TPointSet>
void
ImageRegistrationMethodv4<TFixedImage, TMovingImage, TTransform, TVirtualImage, TPointSet>
::SetMetricSamplingPercentagePerLevel( const MetricSamplingPercentageArrayType  &samplingPercentages )
{
  if( this->m_MetricSamplingPercentagePerLevel != samplingPercentages )
    {
    for( typename MetricSamplingPercentageArrayType::const_iterator it = samplingPercentages.begin();
         it != samplingPercentages.end(); it++ )
      {
      if( *it <= 0.0 || *it > 1.0 )
        {
        itkExceptionMacro("sampling percentage outside expected (0,1] range");
        }
      }
    this->m_MetricSamplingPercentagePerLevel = samplingPercentages;
    this->Modified();
    }
}

} // end namespace itk
#endif
