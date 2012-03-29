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
#ifndef __itkImageToImageMetricv4_hxx
#define __itkImageToImageMetricv4_hxx

#include "itkImageToImageMetricv4.h"
#include "itkPixelTraits.h"
#include "itkDisplacementFieldTransform.h"
#include "itkCompositeTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkIdentityTransform.h"
#include "itkCentralDifferenceImageFunction.h"

namespace itk
{

template<class TFixedImage,class TMovingImage,class TVirtualImage>
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ImageToImageMetricv4()
{
  /* Both transforms default to an identity transform */
  typedef IdentityTransform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ) >
                                          MovingIdentityTransformType;
  typedef IdentityTransform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ) >
                                          FixedIdentityTransformType;
  this->m_FixedTransform  = FixedIdentityTransformType::New();
  this->m_MovingTransform = MovingIdentityTransformType::New();

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
  typedef CentralDifferenceImageFunction<FixedImageType,
                                         CoordinateRepresentationType>
                                          FixedCentralDifferenceCalculatorType;
  typedef CentralDifferenceImageFunction<MovingImageType,
                                         CoordinateRepresentationType>
                                          MovingCentralDifferenceCalculatorType;
  typename FixedCentralDifferenceCalculatorType::Pointer
                  fixedCalculator       = FixedCentralDifferenceCalculatorType::New();
  fixedCalculator->UseImageDirectionOn();
  this->m_FixedImageGradientCalculator  = fixedCalculator;
  typename MovingCentralDifferenceCalculatorType::Pointer
                  movingCalculator      = MovingCentralDifferenceCalculatorType::New();
  movingCalculator->UseImageDirectionOn();
  this->m_MovingImageGradientCalculator = movingCalculator;

  /* Setup default options assuming dense-sampling */
  this->m_UseFixedImageGradientFilter  = true;
  this->m_UseMovingImageGradientFilter = true;
  this->m_UseFixedSampledPointSet      = false;

  this->m_UserHasProvidedVirtualDomainImage = false;

  this->m_FloatingPointCorrectionResolution = 1e6;
  this->m_UseFloatingPointCorrection = false;

  this->m_HaveMadeGetValueWarning = false;

  this->m_Value = NumericTraits<MeasureType>::max();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::~ImageToImageMetricv4()
{
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::Initialize() throw ( itk::ExceptionObject )
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
  if( ! this->m_UserHasProvidedVirtualDomainImage )
    {
    /* Instantiate a virtual image, but do not call Allocate to allocate
     * the data, to save memory. We don't need data. We'll simply be iterating
     * over the image to get indecies and transform to points.
     * Note that it will be safer to have a dedicated VirtualImage class
     * that prevents accidental access of data. */
    /* Just copy information from fixed image */
    this->m_VirtualDomainImage = VirtualImageType::New();
    this->m_VirtualDomainImage->CopyInformation( this->m_FixedImage );
    /* CopyInformation does not copy buffered region */
    this->m_VirtualDomainImage->SetBufferedRegion(
      this->m_FixedImage->GetBufferedRegion() );
    this->m_VirtualDomainImage->SetRequestedRegion(
      this->m_FixedImage->GetRequestedRegion() );
    }

  /* Map the fixed samples into the virtual domain and store in
   * a searpate point set. */
  this->m_NumberOfSkippedFixedSampledPoints = 0;
  if( this->m_UseFixedSampledPointSet )
    {
    this->MapFixedSampledPointSetToVirtual();
    }

  /* Special checks for when the moving transform is dense/high-dimensional */
  if( this->m_MovingTransform->HasLocalSupport() )
    {
    /* Verify that virtual domain and displacement field are the same size
    * and in the same physical space. Handles CompositeTransform by checking
    * if first applied transform is DisplacementFieldTransform */
    this->VerifyDisplacementFieldSizeAndPhysicalSpace();

    /* Verify virtual image pixel type is scalar. Effects calc of offset
    in StoreDerivativeResult.
    NOTE:  Can this be checked at compile time? ConceptChecking has a
    HasPixelTraits class, but looks like it just verifies that type T
    has PixelTraits associated with it, and not a particular value. */
    if( PixelTraits< VirtualImagePixelType >::Dimension != 1 )
      {
      itkExceptionMacro("VirtualImagePixelType must be scalar for use "
                        "with high-dimensional transform. "
                        "Dimensionality is " <<
                        PixelTraits< VirtualImagePixelType >::Dimension );
      }
    }

  /* Inititialize interpolators. */
  itkDebugMacro("Initialize Interpolators");
  this->m_FixedInterpolator->SetInputImage( this->m_FixedImage );
  this->m_MovingInterpolator->SetInputImage( this->m_MovingImage );

  /* Setup for image gradient calculations. */
  if( ! this->m_UseFixedImageGradientFilter )
    {
    itkDebugMacro("Initialize FixedImageGradientCalculator");
    this->m_FixedImageGradientImage = NULL;
    this->m_FixedImageGradientCalculator->SetInputImage(this->m_FixedImage);
    }
  if( ! this->m_UseMovingImageGradientFilter )
    {
    itkDebugMacro("Initialize MovingImageGradientCalculator");
    this->m_MovingImageGradientImage = NULL;
    this->m_MovingImageGradientCalculator->SetInputImage(this->m_MovingImage);
    }

  /* Initialize default gradient image filters. */
  itkDebugMacro("InitializeDefaultFixedImageGradientFilter");
  this->InitializeDefaultFixedImageGradientFilter();
  itkDebugMacro("InitializeDefaultMovingImageGradientFilter");
  this->InitializeDefaultMovingImageGradientFilter();

  /* If user set to use a pre-calculated fixed gradient image,
   * then we need to calculate the gradient image.
   * We only need to compute once since the fixed transform isn't
   * optimized. */
  if ( this->m_UseFixedImageGradientFilter )
    {
    itkDebugMacro("Initialize: ComputeFixedImageGradientFilterImage");
    this->ComputeFixedImageGradientFilterImage();
    }

  /* Compute gradient image for moving image. Needed now for
   * derived classes that use it before InitializeForIteration is called.
   * It's also computed at begin of every iteration. */
  if( this->m_UseMovingImageGradientFilter )
    {
    itkDebugMacro("Initialize: ComputeMovingImageGradientFilterImage");
    this->ComputeMovingImageGradientFilterImage();
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>::MeasureType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetValue() const
{
  /* As long as this is done as a simple inefficient implementation, give
   * the user a warning. The intention is to provide a more efficient
   * implementation here eventually, that avoids derivative calcualtions.
   * Derived classes may override this method and provide their own
   * efficient implementations as appropriate. */
  if( ! this->m_HaveMadeGetValueWarning )
    {
    itkWarningMacro("Using ImageToImageMetricv4::GetValue which is a "
                    "temporary, inefficient implementation. " );
    this->m_HaveMadeGetValueWarning = true;
    }
  DerivativeType derivative;
  MeasureType value;
  this->GetValueAndDerivative( value, derivative );
  return value;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetValueAndDerivative( MeasureType & value,
                         DerivativeType & derivative ) const
{
  this->m_DerivativeResult = &derivative;
  this->InitializeForIteration();

  // Do the threaded processing using the appropriate
  // GetValueAndDerivativeThreader. Results get written to
  // member vars.
  this->GetValueAndDerivativeExecute();

  value = this->m_Value;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
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
    this->m_DenseGetValueAndDerivativeThreader->Execute( const_cast< Self* >(this), this->GetVirtualDomainRegion() );
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::InitializeForIteration() const
{
  /* This size always comes from the moving image */
  const NumberOfParametersType globalDerivativeSize =
    this->m_MovingTransform->GetNumberOfParameters();
  if( this->m_DerivativeResult->GetSize() != globalDerivativeSize )
    {
    this->m_DerivativeResult->SetSize( globalDerivativeSize );
    }
  /* Clear derivative final result. This will
   * require an option to skip for use with multivariate metric. */
  this->m_DerivativeResult->Fill( NumericTraits< DerivativeValueType >::Zero );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::TransformAndEvaluateFixedPoint(
                         const VirtualIndexType & itkNotUsed(index),
                         const VirtualPointType & virtualPoint,
                         const bool computeImageGradient,
                         FixedImagePointType & mappedFixedPoint,
                         FixedImagePixelType & mappedFixedPixelValue,
                         FixedImageGradientType & mappedFixedImageGradient ) const
{
  bool pointIsValid = true;
  mappedFixedPixelValue = NumericTraits<FixedImagePixelType>::Zero;

  // map the point into fixed space
  mappedFixedPoint = this->m_FixedTransform->TransformPoint( virtualPoint );

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
  if( ! pointIsValid )
    {
    return pointIsValid;
    }

  mappedFixedPixelValue = this->m_FixedInterpolator->Evaluate(mappedFixedPoint);
  if( computeImageGradient )
    {
    this->ComputeFixedImageGradientAtPoint( mappedFixedPoint,
                                     mappedFixedImageGradient );
    }

  return pointIsValid;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::TransformAndEvaluateMovingPoint(
                         const VirtualIndexType & itkNotUsed(index),
                         const VirtualPointType & virtualPoint,
                         const bool computeImageGradient,
                         MovingImagePointType & mappedMovingPoint,
                         MovingImagePixelType & mappedMovingPixelValue,
                         MovingImageGradientType & mappedMovingImageGradient ) const
{
  bool pointIsValid = true;
  mappedMovingPixelValue = NumericTraits<MovingImagePixelType>::Zero;

  // map the point into moving space
  mappedMovingPoint = this->m_MovingTransform->TransformPoint( virtualPoint );

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
  if( ! pointIsValid )
    {
    return pointIsValid;
    }

  mappedMovingPixelValue = this->m_MovingInterpolator->Evaluate( mappedMovingPoint );
  if( computeImageGradient )
    {
    this->ComputeMovingImageGradientAtPoint( mappedMovingPoint, mappedMovingImageGradient );
    }

  return pointIsValid;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeFixedImageGradientAtPoint( const FixedImagePointType & mappedPoint,
                             FixedImageGradientType & gradient ) const
{
  if ( this->m_UseFixedImageGradientFilter )
    {
    gradient = m_FixedImageGradientInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_FixedImageGradientCalculator->Evaluate( mappedPoint );
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeMovingImageGradientAtPoint(
                              const MovingImagePointType & mappedPoint,
                              MovingImageGradientType & gradient ) const
{
  if ( this->m_UseMovingImageGradientFilter )
    {
    gradient = m_MovingImageGradientInterpolator->Evaluate( mappedPoint );
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_MovingImageGradientCalculator->Evaluate(mappedPoint);
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeFixedImageGradientAtIndex(
                              const VirtualIndexType & index,
                              FixedImageGradientType & gradient ) const
{
  if ( this->m_UseFixedImageGradientFilter )
    {
    gradient = this->m_FixedImageGradientImage->GetPixel(index);
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeMovingImageGradientAtIndex(
                              const VirtualIndexType & index,
                              MovingImageGradientType & gradient ) const
{
  if ( this->m_UseMovingImageGradientFilter )
    {
    gradient = this->m_MovingImageGradientImage->GetPixel(index);
    }
  else
    {
    // if not using the gradient image
    gradient = this->m_MovingImageGradientCalculator->EvaluateAtIndex(index);
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeFixedImageGradientFilterImage()
{
  this->m_FixedImageGradientFilter->SetInput( this->m_FixedImage );
  this->m_FixedImageGradientFilter->Update();
  this->m_FixedImageGradientImage = this->m_FixedImageGradientFilter->GetOutput();
  this->m_FixedImageGradientInterpolator->SetInputImage( this->m_FixedImageGradientImage );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeMovingImageGradientFilterImage() const
{
  this->m_MovingImageGradientFilter->SetInput( this->m_MovingImage );
  this->m_MovingImageGradientFilter->Update();
  this->m_MovingImageGradientImage = this->m_MovingImageGradientFilter->GetOutput();
  this->m_MovingImageGradientInterpolator->SetInputImage( this->m_MovingImageGradientImage );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
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

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
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

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::SetTransform( MovingTransformType* transform )
{
  this->SetMovingTransform( transform );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::MovingTransformType *
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetTransform()
{
  return this->GetMovingTransform();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::UpdateTransformParameters( DerivativeType & derivative,
                             ParametersValueType factor )
{
  /* Rely on transform::UpdateTransformParameters to verify proper
   * size of derivative */
  this->m_MovingTransform->UpdateTransformParameters( derivative, factor );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
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

template<class TFixedImage,class TMovingImage,class TVirtualImage>
ThreadIdType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetMaximumNumberOfThreads() const
{
  if( this->m_UseFixedSampledPointSet )
    {
    return this->m_SparseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads();
    }
  return  this->m_DenseGetValueAndDerivativeThreader->GetMaximumNumberOfThreads();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
ThreadIdType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
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

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::CreateVirtualDomainImage( VirtualSpacingType & spacing,
                            VirtualOriginType & origin,
                            VirtualDirectionType & direction,
                            VirtualRegionType & region )
{
  this->m_VirtualDomainImage = VirtualImageType::New();
  this->m_VirtualDomainImage->SetSpacing( spacing );
  this->m_VirtualDomainImage->SetOrigin( origin );
  this->m_VirtualDomainImage->SetDirection( direction );
  this->m_VirtualDomainImage->SetRegions( region );
  this->m_VirtualDomainImage->Allocate();
  this->m_VirtualDomainImage->FillBuffer( 0 );
  this->m_UserHasProvidedVirtualDomainImage = true;
  this->Modified();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::SetVirtualDomainImage( VirtualImageType * virtualImage )
{
  itkDebugMacro("setting VirtualDomainImage to " << virtualImage);
  if ( this->m_VirtualDomainImage != virtualImage )
    {
    this->m_VirtualDomainImage = virtualImage;
    this->Modified();
    this->m_UserHasProvidedVirtualDomainImage = virtualImage != NULL;
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
OffsetValueType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::ComputeParameterOffsetFromVirtualDomainIndex( const VirtualIndexType & index, NumberOfParametersType numberOfLocalParameters ) const
{
  OffsetValueType offset =
    this->m_VirtualDomainImage->ComputeOffset(index) * numberOfLocalParameters;
  return offset;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::VirtualSpacingType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetVirtualDomainSpacing( void ) const
{
  if( this->m_VirtualDomainImage )
    {
    return this->m_VirtualDomainImage->GetSpacing();
    }
  else
    {
    itkExceptionMacro("m_VirtualDomainImage is undefined. Cannot "
                      " return spacing. ");
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::VirtualDirectionType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetVirtualDomainDirection( void ) const
{
  if( this->m_VirtualDomainImage )
    {
    return this->m_VirtualDomainImage->GetDirection();
    }
  else
    {
    itkExceptionMacro("m_VirtualDomainImage is undefined. Cannot "
                      " return direction. ");
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::VirtualOriginType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetVirtualDomainOrigin( void ) const
{
  if( this->m_VirtualDomainImage )
    {
    return this->m_VirtualDomainImage->GetOrigin();
    }
  else
    {
    itkExceptionMacro("m_VirtualDomainImage is undefined. Cannot "
                      " return origin. ");
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::VirtualRegionType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetVirtualDomainRegion( void ) const
{
  if( this->m_VirtualDomainImage )
    {
    return this->m_VirtualDomainImage->GetBufferedRegion();
    }
  else
    {
    itkExceptionMacro("m_VirtualDomainImage is undefined. Cannot "
                      " return region. ");
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::MapFixedSampledPointSetToVirtual()
{
  if( this->m_FixedSampledPointSet.IsNull() )
    {
    itkExceptionMacro("UseFixedSampledPointSet has been set, but the fixed sample point list has not been assigned.");
    }
  if( this->m_FixedSampledPointSet->GetNumberOfPoints() == 0 )
    {
    itkExceptionMacro("The fixed sampled point contains no points.");
    }

  this->m_VirtualSampledPointSet = VirtualSampledPointSetType::New();
  this->m_VirtualSampledPointSet->Initialize();

  typedef typename FixedSampledPointSetType::PointsContainer PointsContainer;
  typename PointsContainer::ConstPointer
    points = this->m_FixedSampledPointSet->GetPoints();
  typename PointsContainer::ConstIterator fixedIt = points->Begin();

  typename FixedTransformType::InverseTransformBasePointer
    inverseTransform = this->m_FixedTransform->GetInverseTransform();
  if( inverseTransform.IsNull() )
    {
    itkExceptionMacro("Unable to get inverse transform for mapping sampled "
                      " point set.");
    }

  SizeValueType virtualIndex = 0;
  while( fixedIt != points->End() )
    {
    typename FixedSampledPointSetType::PointType
      point = inverseTransform->TransformPoint( fixedIt.Value() );
    typename VirtualImageType::IndexType tempIndex;
    /* Verify that the point is valid. We may be working with a resized virtual domain,
     * and a fixed sampled point list that was created before the resizing. */
    if( this->m_VirtualDomainImage->TransformPhysicalPointToIndex( point, tempIndex ) )
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
                        "all fixed sampled points were not within the virtual "
                        "domain after mapping. There are no points to evaulate.");
      }
}


template<class TFixedImage,class TMovingImage,class TVirtualImage>
SizeValueType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
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
    typename VirtualImageType::RegionType region = this->GetVirtualDomainRegion();
    return region.GetNumberOfPixels();
    }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
typename
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>::NumberOfParametersType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetNumberOfParameters() const
{
  return this->m_MovingTransform->GetNumberOfParameters();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
const typename ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>::ParametersType &
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetParameters() const
{
  return this->m_MovingTransform->GetParameters();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::SetParameters( ParametersType & params)
{
  this->m_MovingTransform->SetParametersByValue( params );
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
typename
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::NumberOfParametersType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetNumberOfLocalParameters() const
{
  return this->m_MovingTransform->GetNumberOfLocalParameters();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::HasLocalSupport() const
{
  return this->m_MovingTransform->HasLocalSupport();
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
typename
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >::MeasureType
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::GetCurrentValue()
{
  return m_Value;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::VerifyDisplacementFieldSizeAndPhysicalSpace()
{

  // TODO: replace with a common external method to check this,
  // possibly something in Transform.

  /* Verify that virtual domain and displacement field are the same size
   * and in the same physical space.
   * Effects transformation, and calculation of offset in StoreDerivativeResult.
   * If it's a composite transform and the displacement field is the first
   * to be applied (i.e. the most recently added), then it has to be
   * of the same size, otherwise not.
   * Eventually we'll want a method in Transform something like a
   * GetInputDomainSize to check this cleanly. */
  typedef DisplacementFieldTransform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ) >
                                          MovingDisplacementFieldTransformType;
  typedef CompositeTransform<CoordinateRepresentationType,
    itkGetStaticConstMacro( MovingImageDimension ) >
                                          MovingCompositeTransformType;
  MovingTransformType* transform;
  transform = this->m_MovingTransform.GetPointer();
  /* If it's a CompositeTransform, get the last transform (1st applied). */
  MovingCompositeTransformType* comptx =
               dynamic_cast< MovingCompositeTransformType * > ( transform );
  if( comptx != NULL )
    {
    transform = comptx->GetBackTransform().GetPointer();
    }
  /* Check that it's a DisplacementField type, or a derived type,
   * the only type we expect at this point. */
  MovingDisplacementFieldTransformType* deftx =
          dynamic_cast< MovingDisplacementFieldTransformType * >( transform );
  if( deftx == NULL )
    {
    itkExceptionMacro("Expected m_MovingTransform to be of type "
                      "DisplacementFieldTransform or derived." );
    }
  typedef typename MovingDisplacementFieldTransformType::DisplacementFieldType
                                                                    FieldType;
  typename FieldType::Pointer field = deftx->GetDisplacementField();
  typename FieldType::RegionType
    fieldRegion = field->GetBufferedRegion();
  VirtualRegionType virtualRegion =
                            this->m_VirtualDomainImage->GetBufferedRegion();
  if( virtualRegion.GetSize() != fieldRegion.GetSize() ||
      virtualRegion.GetIndex() != fieldRegion.GetIndex() )
    {
    itkExceptionMacro("Virtual domain and moving transform displacement field"
                      " must have the same size and index for "
                      " LargestPossibleRegion."
                      << std::endl << "Virtual size/index: "
                      << virtualRegion.GetSize() << " / "
                      << virtualRegion.GetIndex() << std::endl
                      << "Displacement field size/index: "
                      << fieldRegion.GetSize() << " / "
                      << fieldRegion.GetIndex() << std::endl );
    }

    /* check that the image occupy the same physical space, and that
     * each index is at the same physical location.
     * this code is from ImageToImageFilter */

    /* tolerance for origin and spacing depends on the size of pixel
     * tolerance for directions a fraction of the unit cube. */
    const double coordinateTol
      = 1.0e-6 * this->m_VirtualDomainImage->GetSpacing()[0];
    const double directionTol = 1.0e-6;

    if ( !this->m_VirtualDomainImage->GetOrigin().GetVnlVector().
               is_equal( field->GetOrigin().GetVnlVector(), coordinateTol ) ||
         !this->m_VirtualDomainImage->GetSpacing().GetVnlVector().
               is_equal( field->GetSpacing().GetVnlVector(), coordinateTol ) ||
         !this->m_VirtualDomainImage->GetDirection().GetVnlMatrix().as_ref().
               is_equal( field->GetDirection().GetVnlMatrix(), directionTol ) )
      {
      std::ostringstream originString, spacingString, directionString;
      originString << "m_VirtualDomainImage Origin: "
                   << this->m_VirtualDomainImage->GetOrigin()
                   << ", DisplacementField Origin: " << field->GetOrigin()
                   << std::endl;
      spacingString << "m_VirtualDomainImage Spacing: "
                    << this->m_VirtualDomainImage->GetSpacing()
                    << ", DisplacementField Spacing: "
                    << field->GetSpacing() << std::endl;
      directionString << "m_VirtualDomainImage Direction: "
                      << this->m_VirtualDomainImage->GetDirection()
                      << ", DisplacementField Direction: "
                      << field->GetDirection() << std::endl;
      itkExceptionMacro(<< "m_VirtualDomainImage and DisplacementField do not "
                        << "occupy the same physical space! You may be able to "
                        << "simply call displacementField->CopyInformation( "
                        << "m_VirtualDomainImage ) to align them. "
                        << std::endl
                        << originString.str() << spacingString.str()
                        << directionString.str() );
      }
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
bool
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::VerifyNumberOfValidPoints( MeasureType & value, DerivativeType & derivative) const
{
  if( this->m_NumberOfValidPoints == 0 )
    {
    value = NumericTraits<MeasureType>::max();
    derivative.Fill( NumericTraits<DerivativeValueType>::Zero );
    itkWarningMacro("No valid points were found during metric evaluation. "
                    "Verify that the images overlap appropriately. "
                    "For instance, you can align the image centers by translation.");
    return false;
    }
  return true;
}

template<class TFixedImage,class TMovingImage,class TVirtualImage>
void
ImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ImageToImageMetricv4: " << std::endl
     << indent << "GetUseFixedImageGradientFilter: " << this->GetUseFixedImageGradientFilter() << std::endl
     << indent << "GetUseMovingImageGradientFilter: " << this->GetUseMovingImageGradientFilter() << std::endl
     << indent << "UseFloatingPointCorrection: " << this->GetUseFloatingPointCorrection() << std::endl
     << indent << "FloatingPointCorrectionResolution: " << this->GetFloatingPointCorrectionResolution() << std::endl;

  if( this->GetVirtualDomainImage() != NULL )
    {
    os << indent << "VirtualDomainImage: " << this->GetVirtualDomainImage() << std::endl;
    }
  else
    {
    os << indent << "VirtualDomainImage is NULL." << std::endl;
    }
  if( this->GetFixedImage() != NULL )
    {
    os << indent << "FixedImage: " << this->GetFixedImage() << std::endl;
    }
  else
    {
    os << indent << "FixedImage is NULL." << std::endl;
    }
  if( this->GetMovingImage() != NULL )
    {
    os << indent << "MovingImage: " << this->GetMovingImage() << std::endl;
    }
  else
    {
    os << indent << "MovingImage is NULL." << std::endl;
    }
  if( this->GetFixedTransform() != NULL )
    {
    os << indent << "FixedTransform: " << this->GetFixedTransform() << std::endl;
    }
  else
    {
    os << indent << "FixedTransform is NULL." << std::endl;
    }
  if( this->GetMovingTransform() != NULL )
    {
    os << indent << "MovingTransform: " << this->GetMovingTransform() << std::endl;
    }
  else
    {
    os << indent << "MovingTransform is NULL." << std::endl;
    }
  if( this->GetFixedImageMask() != NULL )
    {
    os << indent << "FixedImageMask: " << this->GetFixedImageMask() << std::endl;
    }
  else
    {
    os << indent << "FixedImageMask is NULL." << std::endl;
    }
  if( this->GetMovingImageMask() != NULL )
    {
    os << indent << "MovingImageMask: " << this->GetMovingImageMask() << std::endl;
    }
  else
    {
    os << indent << "MovingImageMask is NULL." << std::endl;
    }
}

}//namespace itk

#endif
