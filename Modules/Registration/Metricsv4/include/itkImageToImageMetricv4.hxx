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
#ifndef itkImageToImageMetricv4_hxx
#define itkImageToImageMetricv4_hxx

#include "itkImageToImageMetricv4.h"
#include "itkPixelTraits.h"
#include "itkDisplacementFieldTransform.h"
#include "itkCompositeTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkIdentityTransform.h"

namespace itk
{

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ImageToImageMetricv4()
{
  /* Interpolators. Default to linear. */
  typedef LinearInterpolateImageFunction< FixedImageType,
                                          CoordinateRepresentationType >
                                                  FixedLinearInterpolatorType;
  typedef LinearInterpolateImageFunction< MovingImageType,
                                          CoordinateRepresentationType >
                                                  MovingLinearInterpolatorType;
  this->m_FixedInterpolator  = FixedLinearInterpolatorType::New();
  this->m_MovingInterpolator = MovingLinearInterpolatorType::New();

  /* Setup default gradient filter. It gets initialized with default
   * parameters during Initialize. */
  this->m_DefaultFixedImageGradientFilter  = DefaultFixedImageGradientFilter::New();
  this->m_DefaultMovingImageGradientFilter = DefaultMovingImageGradientFilter::New();
  this->m_FixedImageGradientFilter         = this->m_DefaultFixedImageGradientFilter;
  this->m_MovingImageGradientFilter        = this->m_DefaultMovingImageGradientFilter;

  /* Interpolators for image gradient filters */
  this->m_FixedImageGradientInterpolator  = FixedImageGradientInterpolatorType::New();
  this->m_MovingImageGradientInterpolator = MovingImageGradientInterpolatorType::New();

  /* Setup default gradient image function */
  this->m_DefaultFixedImageGradientCalculator = DefaultFixedImageGradientCalculator::New();
  this->m_DefaultFixedImageGradientCalculator->UseImageDirectionOn();
  this->m_FixedImageGradientCalculator = this->m_DefaultFixedImageGradientCalculator;

  this->m_DefaultMovingImageGradientCalculator = DefaultMovingImageGradientCalculator::New();
  this->m_DefaultMovingImageGradientCalculator->UseImageDirectionOn();
  this->m_MovingImageGradientCalculator = this->m_DefaultMovingImageGradientCalculator;

  /* Setup default options assuming dense-sampling */
  this->m_UseFixedImageGradientFilter  = true;
  this->m_UseMovingImageGradientFilter = true;
  this->m_UseFixedSampledPointSet      = false;

  this->m_FloatingPointCorrectionResolution = 1e6;
  this->m_UseFloatingPointCorrection = false;

  this->m_HaveMadeGetValueWarning = false;
  this->m_NumberOfSkippedFixedSampledPoints = 0;

  this->m_Value = NumericTraits<MeasureType>::max();
  this->m_DerivativeResult = ITK_NULLPTR;
  this->m_ComputeDerivative = false;
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::~ImageToImageMetricv4()
{
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::Initialize()
{
  itkDebugMacro("Initialize entered");

  /* Verify things are connected */
  if ( this->m_FixedImage.IsNull() )
    {
    itkExceptionMacro(<< "FixedImage is not present");
    }
  if ( this->m_MovingImage.IsNull() )
    {
    itkExceptionMacro(<< "MovingImage is not present");
    }
  if ( this->m_FixedTransform.IsNull() )
    {
    itkExceptionMacro(<< "FixedTransform is not present");
    }
  if ( this->m_MovingTransform.IsNull() )
    {
    itkExceptionMacro(<< "MovingTransform is not present");
    }

  // If the image is provided by a source, update the source.
  if ( this->m_MovingImage->GetSource() )
    {
    this->m_MovingImage->GetSource()->Update();
    }

  // If the image is provided by a source, update the source.
  if ( this->m_FixedImage->GetSource() )
    {
    this->m_FixedImage->GetSource()->Update();
    }

  /* If a virtual image has not been set or created,
   * create one from fixed image settings */
  if( ! this->m_UserHasSetVirtualDomain )
    {
    /* Instantiate a virtual image, but do not call Allocate to allocate
     * the data, to save memory. We don't need data. We'll simply be iterating
     * over the image to get indecies and transform to points.
     * Note that it will be safer to have a dedicated VirtualImage class
     * that prevents accidental access of data. */
    /* Just copy information from fixed image */
    VirtualImagePointer image = VirtualImageType::New();
    image->CopyInformation( this->m_FixedImage );
    /* CopyInformation does not copy buffered region */
    image->SetBufferedRegion( this->m_FixedImage->GetBufferedRegion() );
    image->SetRequestedRegion( this->m_FixedImage->GetRequestedRegion() );
    this->SetVirtualDomainFromImage( image );
    }

  /*
   * Superclass Initialize.
   * Requires the above actions to already have been taken.
   */
  Superclass::Initialize();

  /* Map the fixed samples into the virtual domain and store in
   * a searpate point set. */
  if( this->m_UseFixedSampledPointSet )
    {
    this->MapFixedSampledPointSetToVirtual();
    }

  /* Inititialize interpolators. */
  itkDebugMacro("Initialize Interpolators");
  this->m_FixedInterpolator->SetInputImage( this->m_FixedImage );
  this->m_MovingInterpolator->SetInputImage( this->m_MovingImage );

  /* Setup for image gradient calculations. */
  if( ! this->m_UseFixedImageGradientFilter )
    {
    itkDebugMacro("Initialize FixedImageGradientCalculator");
    this->m_FixedImageGradientImage = ITK_NULLPTR;
    this->m_FixedImageGradientCalculator->SetInputImage(this->m_FixedImage);
    }
  if( ! this->m_UseMovingImageGradientFilter )
    {
    itkDebugMacro("Initialize MovingImageGradientCalculator");
    this->m_MovingImageGradientImage = ITK_NULLPTR;
    this->m_MovingImageGradientCalculator->SetInputImage(this->m_MovingImage);
    }

  /* Initialize default gradient image filters. */
  itkDebugMacro("InitializeDefaultFixedImageGradientFilter");
  this->InitializeDefaultFixedImageGradientFilter();
  itkDebugMacro("InitializeDefaultMovingImageGradientFilter");
  this->InitializeDefaultMovingImageGradientFilter();

  /* If user set to use a pre-calculated fixed gradient image,
   * and the metric is set to use fixed image gradients,
   * then we need to calculate the gradient image.
   * We only need to compute once. */
  if ( this->GetGradientSourceIncludesFixed() && this->m_UseFixedImageGradientFilter )
    {
    itkDebugMacro("Initialize: ComputeFixedImageGradientFilterImage");
    this->ComputeFixedImageGradientFilterImage();
    }

  /* Compute gradient image for moving image. */
  if( this->GetGradientSourceIncludesMoving() && this->m_UseMovingImageGradientFilter )
    {
    itkDebugMacro("Initialize: ComputeMovingImageGradientFilterImage");
    this->ComputeMovingImageGradientFilterImage();
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>::MeasureType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetValue() const
{
  this->m_ComputeDerivative = false;

  DerivativeType derivative;
  this->m_DerivativeResult = &derivative;
  this->InitializeForIteration();

  // Do the threaded processing using the appropriate
  // GetValueAndDerivativeThreader. Results get written to
  // member vars.
  this->GetValueAndDerivativeExecute();

  return this->m_Value;
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetDerivative( DerivativeType & derivative ) const
{
  MeasureType value;
  this->GetValueAndDerivative( value, derivative );
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetValueAndDerivative( MeasureType & value, DerivativeType & derivative ) const
{
  this->m_ComputeDerivative = true;

  this->m_DerivativeResult = &derivative;
  this->InitializeForIteration();

  // Do the threaded processing using the appropriate
  // GetValueAndDerivativeThreader. Results get written to
  // member vars.
  this->GetValueAndDerivativeExecute();

  value = this->m_Value;
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetValueAndDerivativeExecute() const
{
  if( this->m_UseFixedSampledPointSet ) // sparse sampling
    {
    SizeValueType numberOfPoints = this->GetNumberOfDomainPoints();
    if( numberOfPoints < 1 )
      {
      itkExceptionMacro("VirtualSampledPointSet must have 1 or more points.");
      }
    typename ImageToImageMetricv4GetValueAndDerivativeThreader< ThreadedIndexedContainerPartitioner, Self >::DomainType range;
    range[0] = 0;
    range[1] = numberOfPoints - 1;
    this->m_SparseGetValueAndDerivativeThreader->Execute( const_cast< Self* >(this), range );
    }
  else // dense sampling
    {
    this->m_DenseGetValueAndDerivativeThreader->Execute( const_cast< Self* >(this), this->GetVirtualRegion() );
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::InitializeForIteration() const
{
  if( this->m_ComputeDerivative )
    {
    /* This size always comes from the active transform */
    const NumberOfParametersType globalDerivativeSize = this->GetNumberOfParameters();
    if( this->m_DerivativeResult->GetSize() != globalDerivativeSize )
      {
      this->m_DerivativeResult->SetSize( globalDerivativeSize );
      }
    /* Clear derivative final result. */
    this->m_DerivativeResult->Fill( NumericTraits< DerivativeValueType >::ZeroValue() );
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::TransformAndEvaluateFixedPoint(
                         const VirtualPointType & virtualPoint,
                         FixedImagePointType & mappedFixedPoint,
                         FixedImagePixelType & mappedFixedPixelValue ) const
{
  bool pointIsValid = true;
  mappedFixedPixelValue = NumericTraits<FixedImagePixelType>::ZeroValue();

  // map the point into fixed space
  this->LocalTransformPoint(virtualPoint,mappedFixedPoint);

  // check against the mask if one is assigned
  if ( this->m_FixedImageMask )
    {
    // Check if mapped point is within the support region of the fixed image
    // mask
    pointIsValid = this->m_FixedImageMask->IsInside( mappedFixedPoint );
    if( ! pointIsValid )
      {
      return pointIsValid;
      }
    }

  // Check if mapped point is inside image buffer
  pointIsValid = this->m_FixedInterpolator->IsInsideBuffer(mappedFixedPoint);

  // Evaluate
  if( pointIsValid )
    {
    mappedFixedPixelValue = this->m_FixedInterpolator->Evaluate(mappedFixedPoint);
    }

  return pointIsValid;
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::TransformAndEvaluateMovingPoint(
                         const VirtualPointType & virtualPoint,
                         MovingImagePointType & mappedMovingPoint,
                         MovingImagePixelType & mappedMovingPixelValue ) const
{
  bool pointIsValid = true;
  mappedMovingPixelValue = NumericTraits<MovingImagePixelType>::ZeroValue();

  // map the point into moving space

  // Before transforming points, we should convert their types from the ImagePointType (aka Point<double, dim>)
  // to TransformPointType (aka Point<ScalarType, dim>).
  typename MovingTransformType::OutputPointType localVirtualPoint;
  typename MovingTransformType::OutputPointType localMappedMovingPoint;

  localVirtualPoint.CastFrom(virtualPoint);
  localMappedMovingPoint.CastFrom(mappedMovingPoint);

  localMappedMovingPoint = this->m_MovingTransform->TransformPoint( localVirtualPoint );
  mappedMovingPoint.CastFrom(localMappedMovingPoint);

  // check against the mask if one is assigned
  if ( this->m_MovingImageMask )
    {
    // Check if mapped point is within the support region of the fixed image
    // mask
    pointIsValid = this->m_MovingImageMask->IsInside( mappedMovingPoint );
    if( ! pointIsValid )
      {
      return pointIsValid;
      }
    }

  // Check if mapped point is inside image buffer
  pointIsValid = this->m_MovingInterpolator->IsInsideBuffer(mappedMovingPoint);

  // Evaluate
  if( pointIsValid )
    {
    mappedMovingPixelValue = this->m_MovingInterpolator->Evaluate( mappedMovingPoint );
    }

  return pointIsValid;
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeFixedImageGradientAtPoint( const FixedImagePointType & mappedPoint, FixedImageGradientType & gradient ) const
{
  if ( this->m_UseFixedImageGradientFilter )
    {
    if( ! this->GetGradientSourceIncludesFixed() )
      {
      itkExceptionMacro("Attempted to retrieve fixed image gradient from gradient image filter, "
                        "but GradientSource does not include 'fixed', and thus the gradient image has not been calculated.");
      }
    gradient = m_FixedImageGradientInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_FixedImageGradientCalculator->Evaluate( mappedPoint );
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeMovingImageGradientAtPoint( const MovingImagePointType & mappedPoint, MovingImageGradientType & gradient ) const
{
  if ( this->m_UseMovingImageGradientFilter )
    {
    if( ! this->GetGradientSourceIncludesMoving() )
      {
      itkExceptionMacro("Attempted to retrieve moving image gradient from gradient image filter, "
                        "but GradientSource does not include 'moving', and thus the gradient image has not been calculated.");
      }
    gradient = m_MovingImageGradientInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_MovingImageGradientCalculator->Evaluate(mappedPoint);
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeFixedImageGradientFilterImage()
{
  this->m_FixedImageGradientFilter->SetInput( this->m_FixedImage );
  this->m_FixedImageGradientFilter->Update();
  this->m_FixedImageGradientImage = this->m_FixedImageGradientFilter->GetOutput();
  this->m_FixedImageGradientInterpolator->SetInputImage( this->m_FixedImageGradientImage );
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeMovingImageGradientFilterImage() const
{
  this->m_MovingImageGradientFilter->SetInput( this->m_MovingImage );
  this->m_MovingImageGradientFilter->Update();
  this->m_MovingImageGradientImage = this->m_MovingImageGradientFilter->GetOutput();
  this->m_MovingImageGradientInterpolator->SetInputImage( this->m_MovingImageGradientImage );
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::InitializeDefaultFixedImageGradientFilter()
{
  const typename FixedImageType::SpacingType & spacing = this->m_FixedImage->GetSpacing();
  double maximumSpacing = 0.0;
  for ( ImageDimensionType i = 0; i < FixedImageDimension; i++ )
    {
    if ( spacing[i] > maximumSpacing )
      {
      maximumSpacing = spacing[i];
      }
    }
  this->m_DefaultFixedImageGradientFilter->SetSigma( maximumSpacing );
  this->m_DefaultFixedImageGradientFilter->SetNormalizeAcrossScale( true );
  this->m_DefaultFixedImageGradientFilter->SetNumberOfThreads( this->GetMaximumNumberOfThreads() );
  this->m_DefaultFixedImageGradientFilter->SetUseImageDirection( true );
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::InitializeDefaultMovingImageGradientFilter()
{
  const typename MovingImageType::SpacingType & spacing = this->m_MovingImage->GetSpacing();
  double maximumSpacing = 0.0;
  for ( ImageDimensionType i = 0; i < MovingImageDimension; i++ )
    {
    if ( spacing[i] > maximumSpacing )
      {
      maximumSpacing = spacing[i];
      }
    }
  this->m_DefaultMovingImageGradientFilter->SetSigma(maximumSpacing);
  this->m_DefaultMovingImageGradientFilter->SetNormalizeAcrossScale(true);
  this->m_DefaultMovingImageGradientFilter->SetNumberOfThreads(this->GetMaximumNumberOfThreads());
  this->m_DefaultMovingImageGradientFilter->SetUseImageDirection(true);
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::SetMaximumNumberOfThreads( const ThreadIdType number )
{
  if( number != this->m_SparseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads() )
    {
    this->m_SparseGetValueAndDerivativeThreader->SetMaximumNumberOfThreads( number );
    this->Modified();
    }
  if( number != this->m_DenseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads() )
    {
    this->m_DenseGetValueAndDerivativeThreader->SetMaximumNumberOfThreads( number );
    this->Modified();
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ThreadIdType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetMaximumNumberOfThreads() const
{
  if( this->m_UseFixedSampledPointSet )
    {
    return this->m_SparseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads();
    }
  return  this->m_DenseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads();
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
ThreadIdType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetNumberOfThreadsUsed() const
{
  if( this->m_UseFixedSampledPointSet )
    {
    return this->m_SparseGetValueAndDerivativeThreader->GetNumberOfThreadsUsed();
    }
  else
    {
    return this->m_DenseGetValueAndDerivativeThreader->GetNumberOfThreadsUsed();
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::MapFixedSampledPointSetToVirtual()
{
  this->m_VirtualSampledPointSet = VirtualPointSetType::New();
  this->m_VirtualSampledPointSet->Initialize();

  typedef typename FixedSampledPointSetType::PointsContainer PointsContainer;
  typename PointsContainer::ConstPointer
    points = this->m_FixedSampledPointSet->GetPoints();
  if( points.IsNull() )
    {
    itkExceptionMacro("Fixed Sample point set is empty.");
    }
  typename PointsContainer::ConstIterator fixedIt = points->Begin();

  typename FixedTransformType::InverseTransformBasePointer
    inverseTransform = this->m_FixedTransform->GetInverseTransform();
  if( inverseTransform.IsNull() )
    {
    itkExceptionMacro("Unable to get inverse transform for mapping sampled "
                      " point set.");
    }

  this->m_NumberOfSkippedFixedSampledPoints = 0;
  SizeValueType virtualIndex = 0;
  while( fixedIt != points->End() )
    {
    typename FixedSampledPointSetType::PointType point = inverseTransform->TransformPoint( fixedIt.Value() );
    typename VirtualImageType::IndexType tempIndex;
    /* Verify that the point is valid. We may be working with a resized virtual domain,
     * and a fixed sampled point list that was created before the resizing. */
    if( this->TransformPhysicalPointToVirtualIndex( point, tempIndex ) )
      {
      this->m_VirtualSampledPointSet->SetPoint( virtualIndex, point );
      virtualIndex++;
      }
    else
      {
      this->m_NumberOfSkippedFixedSampledPoints++;
      }
    ++fixedIt;
    }
  if( this->m_VirtualSampledPointSet->GetNumberOfPoints() == 0 )
    {
    itkExceptionMacro("The virtual sampled point set has zero points because "
                      "no fixed sampled points were within the virtual "
                      "domain after mapping. There are no points to evaulate.");
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
SizeValueType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetNumberOfDomainPoints() const
{
  if( this->m_UseFixedSampledPointSet )
    {
    //The virtual sampled point set holds the actual points
    // over which we're evaluating over.
    return this->m_VirtualSampledPointSet->GetNumberOfPoints();
    }
  else
    {
    typename VirtualImageType::RegionType region = this->GetVirtualRegion();
    return region.GetNumberOfPixels();
    }
}

template<typename TFixedImage,typename TMovingImage,typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ImageToImageMetricv4: " << std::endl
     << indent << "GetUseFixedImageGradientFilter: " << this->GetUseFixedImageGradientFilter() << std::endl
     << indent << "GetUseMovingImageGradientFilter: " << this->GetUseMovingImageGradientFilter() << std::endl
     << indent << "UseFloatingPointCorrection: " << this->GetUseFloatingPointCorrection() << std::endl
     << indent << "FloatingPointCorrectionResolution: " << this->GetFloatingPointCorrectionResolution() << std::endl;

  itkPrintSelfObjectMacro( FixedImage );
  itkPrintSelfObjectMacro( MovingImage );
  itkPrintSelfObjectMacro( FixedTransform );
  itkPrintSelfObjectMacro( MovingTransform );
  itkPrintSelfObjectMacro( FixedImageMask );
  itkPrintSelfObjectMacro( MovingImageMask );

}

}//namespace itk

#endif
