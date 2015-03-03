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
#ifndef itkBSplineSyNImageRegistrationMethod_hxx
#define itkBSplineSyNImageRegistrationMethod_hxx

#include "itkBSplineSyNImageRegistrationMethod.h"

#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkComposeDisplacementFieldsImageFilter.h"
#include "itkImportImageFilter.h"
#include "itkInvertDisplacementFieldImageFilter.h"
#include "itkIterationReporter.h"
#include "itkMultiplyImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/**
 * Constructor
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::BSplineSyNImageRegistrationMethod()
{
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::~BSplineSyNImageRegistrationMethod()
{
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
void
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::InitializeRegistrationAtEachLevel( const SizeValueType level )
{
  Superclass::InitializeRegistrationAtEachLevel( level );

  typedef BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<OutputTransformType> BSplineDisplacementFieldTransformAdaptorType;

  if( level == 0 )
    {
    this->m_FixedToMiddleTransform->SetSplineOrder( this->m_OutputTransform->GetSplineOrder() );
    this->m_FixedToMiddleTransform->SetNumberOfControlPointsForTheUpdateField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheUpdateField() );
    this->m_FixedToMiddleTransform->SetNumberOfControlPointsForTheTotalField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheTotalField() );

    this->m_MovingToMiddleTransform->SetSplineOrder( this->m_OutputTransform->GetSplineOrder() );
    this->m_MovingToMiddleTransform->SetNumberOfControlPointsForTheUpdateField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheUpdateField() );
    this->m_MovingToMiddleTransform->SetNumberOfControlPointsForTheTotalField(
      dynamic_cast<BSplineDisplacementFieldTransformAdaptorType *>( this->m_TransformParametersAdaptorsPerLevel[0].GetPointer() )->GetNumberOfControlPointsForTheTotalField() );
    }
}

/*
 * Start the optimization at each level.  We just do a basic gradient descent operation.
 */
template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
void
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::StartOptimization()
{
  VirtualImageBaseConstPointer virtualDomainImage = this->GetCurrentLevelVirtualDomainImage();

  if( virtualDomainImage.IsNull() )
    {
    itkExceptionMacro( "The virtual domain image is not found." );
    }

  typename MovingImageMaskType::ConstPointer movingImageMask = ITK_NULLPTR;
  typename FixedImageMaskType::ConstPointer fixedImageMask = ITK_NULLPTR;

  typename MultiMetricType::Pointer multiMetric = dynamic_cast<MultiMetricType *>( this->m_Metric.GetPointer() );
  if( multiMetric )
    {
    typename ImageMetricType::Pointer metricQueue = dynamic_cast<ImageMetricType *>( multiMetric->GetMetricQueue()[0].GetPointer() );
    if( metricQueue.IsNotNull() )
      {
      fixedImageMask = metricQueue->GetFixedImageMask();
      movingImageMask = metricQueue->GetMovingImageMask();
      }
    }
  else
    {
    typename ImageMetricType::Pointer metric = dynamic_cast<ImageMetricType *>( this->m_Metric.GetPointer() );
    if( metric.IsNotNull() )
      {
      fixedImageMask = metric->GetFixedImageMask();
      movingImageMask = metric->GetMovingImageMask();
      }
    }

  InitialTransformType* fixedInitialTransform = const_cast<InitialTransformType*>( this->GetFixedInitialTransform() );

  // Monitor the convergence
  typedef itk::Function::WindowConvergenceMonitoringFunction<RealType> ConvergenceMonitoringType;
  typename ConvergenceMonitoringType::Pointer convergenceMonitoring = ConvergenceMonitoringType::New();
  convergenceMonitoring->SetWindowSize( this->m_ConvergenceWindowSize );

  IterationReporter reporter( this, 0, 1 );

  while( this->m_CurrentIteration++ < this->m_NumberOfIterationsPerLevel[this->m_CurrentLevel] && !this->m_IsConverged )
    {
    typename CompositeTransformType::Pointer fixedComposite = CompositeTransformType::New();
    if ( fixedInitialTransform != ITK_NULLPTR )
      {
      fixedComposite->AddTransform( fixedInitialTransform );
      }

    fixedComposite->AddTransform( this->m_FixedToMiddleTransform->GetInverseTransform() );
    fixedComposite->FlattenTransformQueue();
    fixedComposite->SetOnlyMostRecentTransformToOptimizeOn();

    typename CompositeTransformType::Pointer movingComposite = CompositeTransformType::New();
    movingComposite->AddTransform( this->m_CompositeTransform );
    movingComposite->AddTransform( this->m_MovingToMiddleTransform->GetInverseTransform() );
    movingComposite->FlattenTransformQueue();
    movingComposite->SetOnlyMostRecentTransformToOptimizeOn();

    // Compute the update fields (to both moving and fixed images) and smooth

    MeasureType fixedMetricValue = 0.0;
    MeasureType movingMetricValue = 0.0;

    DisplacementFieldPointer fixedToMiddleSmoothUpdateField = this->ComputeUpdateField(
      this->m_FixedSmoothImages, this->m_FixedPointSets, fixedComposite,
      this->m_MovingSmoothImages, this->m_MovingPointSets, movingComposite,
      fixedImageMask, movingMetricValue );

    DisplacementFieldPointer movingToMiddleSmoothUpdateField = this->ComputeUpdateField(
      this->m_MovingSmoothImages, this->m_MovingPointSets, movingComposite,
      this->m_FixedSmoothImages, this->m_FixedPointSets, fixedComposite,
      movingImageMask, fixedMetricValue );

    if ( this->m_AverageMidPointGradients )
      {
      ImageRegionIteratorWithIndex<DisplacementFieldType> ItF( fixedToMiddleSmoothUpdateField, fixedToMiddleSmoothUpdateField->GetLargestPossibleRegion() );
      for( ItF.GoToBegin(); !ItF.IsAtEnd(); ++ItF )
        {
        ItF.Set( ItF.Get() - movingToMiddleSmoothUpdateField->GetPixel( ItF.GetIndex() ) );
        movingToMiddleSmoothUpdateField->SetPixel( ItF.GetIndex(), -ItF.Get() );
        }
      }

    // Add the update field to both displacement fields (from fixed/moving to middle image) and then smooth

    typedef ComposeDisplacementFieldsImageFilter<DisplacementFieldType> ComposerType;

    typename ComposerType::Pointer fixedComposer = ComposerType::New();
    fixedComposer->SetDisplacementField( fixedToMiddleSmoothUpdateField );
    fixedComposer->SetWarpingField( this->m_FixedToMiddleTransform->GetDisplacementField() );
    fixedComposer->Update();

    DisplacementFieldPointer fixedToMiddleSmoothTotalFieldTmp = this->BSplineSmoothDisplacementField( fixedComposer->GetOutput(),
      this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheTotalField(), ITK_NULLPTR, ITK_NULLPTR );

    typename ComposerType::Pointer movingComposer = ComposerType::New();
    movingComposer->SetDisplacementField( movingToMiddleSmoothUpdateField );
    movingComposer->SetWarpingField( this->m_MovingToMiddleTransform->GetDisplacementField() );
    movingComposer->Update();

    DisplacementFieldPointer movingToMiddleSmoothTotalFieldTmp = this->BSplineSmoothDisplacementField( movingComposer->GetOutput(),
      this->m_MovingToMiddleTransform->GetNumberOfControlPointsForTheTotalField(), ITK_NULLPTR, ITK_NULLPTR );

    // Iteratively estimate the inverse fields.

    DisplacementFieldPointer fixedToMiddleSmoothTotalFieldInverse = this->InvertDisplacementField( fixedToMiddleSmoothTotalFieldTmp, this->m_FixedToMiddleTransform->GetInverseDisplacementField() );
    DisplacementFieldPointer fixedToMiddleSmoothTotalField = this->InvertDisplacementField( fixedToMiddleSmoothTotalFieldInverse, fixedToMiddleSmoothTotalFieldTmp );

    DisplacementFieldPointer movingToMiddleSmoothTotalFieldInverse = this->InvertDisplacementField( movingToMiddleSmoothTotalFieldTmp, this->m_MovingToMiddleTransform->GetInverseDisplacementField() );
    DisplacementFieldPointer movingToMiddleSmoothTotalField = this->InvertDisplacementField( movingToMiddleSmoothTotalFieldInverse, movingToMiddleSmoothTotalFieldTmp );

    // Assign the displacement fields and their inverses to the proper transforms.
    this->m_FixedToMiddleTransform->SetDisplacementField( fixedToMiddleSmoothTotalField );
    this->m_FixedToMiddleTransform->SetInverseDisplacementField( fixedToMiddleSmoothTotalFieldInverse );

    this->m_MovingToMiddleTransform->SetDisplacementField( movingToMiddleSmoothTotalField );
    this->m_MovingToMiddleTransform->SetInverseDisplacementField( movingToMiddleSmoothTotalFieldInverse );

    this->m_CurrentMetricValue = 0.5 * ( movingMetricValue + fixedMetricValue );

    convergenceMonitoring->AddEnergyValue( this->m_CurrentMetricValue );
    this->m_CurrentConvergenceValue = convergenceMonitoring->GetConvergenceValue();

    if( this->m_CurrentConvergenceValue < this->m_ConvergenceThreshold )
      {
      this->m_IsConverged = true;
      }
    reporter.CompletedStep();
    }
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
typename BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>::DisplacementFieldPointer
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::ComputeUpdateField( const FixedImagesContainerType fixedImages, const PointSetsContainerType fixedPointSets,
  const TransformBaseType * fixedTransform, const MovingImagesContainerType movingImages, const PointSetsContainerType movingPointSets,
  const TransformBaseType * movingTransform, const FixedImageMaskType * mask, MeasureType & value )
{
  DisplacementFieldPointer metricGradientField = ITK_NULLPTR;
  DisplacementFieldPointer updateField = ITK_NULLPTR;

  typename WeightedMaskImageType::Pointer weightedMask = ITK_NULLPTR;

  if( this->m_Metric->GetMetricCategory() == MetricType::POINT_SET_METRIC )
    {
    const DisplacementVectorType zeroVector( 0.0 );

    VirtualImageBaseConstPointer virtualDomainImage = this->GetCurrentLevelVirtualDomainImage();

    metricGradientField = DisplacementFieldType::New();
    metricGradientField->CopyInformation( virtualDomainImage );
    metricGradientField->SetRegions( virtualDomainImage->GetLargestPossibleRegion() );
    metricGradientField->Allocate();
    metricGradientField->FillBuffer( zeroVector );

    typename PointSetType::Pointer transformedPointSet = ITK_NULLPTR;

    if( !this->m_DownsampleImagesForMetricDerivatives )
      {
      this->m_Metric->SetFixedObject( fixedPointSets[0] );
      this->m_Metric->SetMovingObject( movingPointSets[0] );

      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );

      this->m_Metric->SetFixedObject( fixedPointSets[0] );
      this->m_Metric->SetMovingObject( movingPointSets[0] );

      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( const_cast<TransformBaseType *>( fixedTransform ) );
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( const_cast<TransformBaseType *>( movingTransform ) );
      }
    else
      {
      typename PointSetType::Pointer transformedFixedPointSet =
        this->TransformPointSet( fixedPointSets[0], fixedTransform->GetInverseTransform() );

      transformedPointSet = transformedFixedPointSet;

      typename PointSetType::Pointer transformedMovingPointSet =
        this->TransformPointSet( movingPointSets[0], movingTransform->GetInverseTransform() );

      this->m_Metric->SetFixedObject( transformedFixedPointSet );
      this->m_Metric->SetMovingObject( transformedMovingPointSet );

      typename DisplacementFieldType::Pointer identityField = DisplacementFieldType::New();
      identityField->CopyInformation( virtualDomainImage );
      identityField->SetRegions( virtualDomainImage->GetLargestPossibleRegion() );
      identityField->Allocate();
      identityField->FillBuffer( zeroVector );

      DisplacementFieldTransformPointer identityDisplacementFieldTransform = DisplacementFieldTransformType::New();
      identityDisplacementFieldTransform->SetDisplacementField( identityField );
      identityDisplacementFieldTransform->SetInverseDisplacementField( identityField );

      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetFixedTransform( identityDisplacementFieldTransform );
      dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->SetMovingTransform( identityDisplacementFieldTransform );
      }

    this->m_Metric->Initialize();
    dynamic_cast<PointSetMetricType *>( this->m_Metric.GetPointer() )->
      SetStoreDerivativeAsSparseFieldForLocalSupportTransforms( false );
    typename ImageMetricType::DerivativeType metricDerivative;
    this->m_Metric->GetValueAndDerivative( value, metricDerivative );

    typename BSplinePointSetType::Pointer gradientPointSet = BSplinePointSetType::New();
    gradientPointSet->Initialize();

    if( fixedPointSets[0]->GetNumberOfPoints() > 0 )
      {
      typename PointSetType::PointsContainerConstIterator It = transformedPointSet->GetPoints()->Begin();

      SizeValueType count = 0;
      while( It != transformedPointSet->GetPoints()->End() )
        {
        typename BSplinePointSetType::PixelType displacement;
        for( unsigned int d = 0; d < ImageDimension; d++ )
          {
          displacement[d] = metricDerivative[count * ImageDimension + d];
          }
        gradientPointSet->SetPoint( count, It.Value() );
        gradientPointSet->SetPointData( count++, displacement );

        ++It;
        }
      updateField = this->BSplineSmoothDisplacementField( metricGradientField,
        this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheUpdateField(), weightedMask, gradientPointSet );
      }
    else
      {
      updateField = metricGradientField;
      }
    }
  else
    {
    metricGradientField = this->ComputeMetricGradientField(
        fixedImages, fixedPointSets, fixedTransform, movingImages, movingPointSets, movingTransform, mask, value );

    if( mask )
      {
      VirtualImageBaseConstPointer virtualDomainImage = this->GetCurrentLevelVirtualDomainImage();

      typedef ResampleImageFilter<MaskImageType, WeightedMaskImageType, typename TOutputTransform::ScalarType> MaskResamplerType;
      typename MaskResamplerType::Pointer maskResampler = MaskResamplerType::New();
      maskResampler->SetTransform( fixedTransform );
      maskResampler->SetInput( dynamic_cast<ImageMaskSpatialObjectType *>( const_cast<FixedImageMaskType *>( mask ) )->GetImage() );
      maskResampler->UseReferenceImageOn();
      maskResampler->SetReferenceImage( virtualDomainImage );
      maskResampler->SetSize( virtualDomainImage->GetBufferedRegion().GetSize() );
      maskResampler->SetDefaultPixelValue( 0 );
      weightedMask = maskResampler->GetOutput();
      weightedMask->Update();
      weightedMask->DisconnectPipeline();
      }
    updateField = this->BSplineSmoothDisplacementField( metricGradientField,
      this->m_FixedToMiddleTransform->GetNumberOfControlPointsForTheUpdateField(), weightedMask, ITK_NULLPTR );
    }

  DisplacementFieldPointer scaledUpdateField = this->ScaleUpdateField( updateField );

  return scaledUpdateField;
}

template<typename TFixedImage, typename TMovingImage, typename TOutputTransform, typename TVirtualImage, typename TPointSet>
typename BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>::DisplacementFieldPointer
BSplineSyNImageRegistrationMethod<TFixedImage, TMovingImage, TOutputTransform, TVirtualImage, TPointSet>
::BSplineSmoothDisplacementField( const DisplacementFieldType * field,
  const ArrayType & numberOfControlPoints, const WeightedMaskImageType * mask,
  const BSplinePointSetType * gradientPointSet )
{
  typedef ImageDuplicator<DisplacementFieldType> DuplicatorType;
  typename DuplicatorType::Pointer duplicator = DuplicatorType::New();
  duplicator->SetInputImage( field );
  duplicator->Update();

  DisplacementFieldPointer smoothField = duplicator->GetModifiableOutput();

  for( unsigned int d = 0; d < numberOfControlPoints.Size(); d++ )
    {
    if( numberOfControlPoints[d] <= 0 )
      {
      return smoothField;
      }
    }

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();

  if( gradientPointSet && gradientPointSet->GetNumberOfPoints() > 0 )
    {
    bspliner->SetPointSet( gradientPointSet );
    bspliner->SetBSplineDomainFromImage( field );
    }
  else
    {
    bspliner->SetUseInputFieldToDefineTheBSplineDomain( true );
    bspliner->SetDisplacementField( field );
    }
  if( mask )
    {
    bspliner->SetConfidenceImage( mask );
    }
  bspliner->SetNumberOfControlPoints( numberOfControlPoints );
  bspliner->SetSplineOrder( this->m_FixedToMiddleTransform->GetSplineOrder() );
  bspliner->SetNumberOfFittingLevels( 1 );
  bspliner->SetEnforceStationaryBoundary( true );
  bspliner->SetEstimateInverse( false );
  bspliner->Update();

  smoothField = bspliner->GetOutput();

  return smoothField;
}

} // end namespace itk

#endif
