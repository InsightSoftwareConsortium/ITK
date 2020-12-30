/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkResampleImageFilter_h
#define itkResampleImageFilter_h

#include "itkFixedArray.h"
#include "itkTransform.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkExtrapolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSize.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkDataObjectDecorator.h"


namespace itk
{
/**
 *\class ResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * ResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function.  The class is templated
 * over the types of the input and output images.
 *
 * Note that the choice of interpolator function can be important.
 * This function is set via SetInterpolator().  The default is
 * LinearInterpolateImageFunction<InputImageType,
 * TInterpolatorPrecisionType>, which
 * is reasonable for ordinary medical images.  However, some synthetic
 * images have pixels drawn from a finite prescribed set.  An example
 * would be a mask indicating the segmentation of a brain into a small
 * number of tissue types.  For such an image, one does not want to
 * interpolate between different pixel values, and so
 * NearestNeighborInterpolateImageFunction< InputImageType,
 * TCoordRep > would be a better choice.
 *
 * If an sample is taken from outside the image domain, the default behavior is
 * to use a default pixel value.  If different behavior is desired, an
 * extrapolator function can be set with SetExtrapolator().
 *
 * Output information (spacing, size and direction) for the output
 * image should be set. This information has the normal defaults of
 * unit spacing, zero origin and identity direction. Optionally, the
 * output information can be obtained from a reference image. If the
 * reference image is provided and UseReferenceImage is On, then the
 * spacing, origin and direction of the reference image will be used.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 * \warning For multithreading, the TransformPoint method of the
 * user-designated coordinate transform must be threadsafe.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Core/Transform/TranslateImage,Translate Image}
 * \sphinxexample{Filtering/ImageGrid/UpsampleAnImage,Upsampling An Image}
 * \sphinxexample{Filtering/ImageGrid/ResampleAnImage,Resample An Image}
 * \endsphinx
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType = double,
          typename TTransformPrecisionType = TInterpolatorPrecisionType>
class ITK_TEMPLATE_EXPORT ResampleImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ResampleImageFilter);

  /** Standard class type aliases. */
  using Self = ResampleImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ResampleImageFilter, ImageToImageFilter);

  /** Number of dimensions of output image. */
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Number of dimensions of input image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

#if !defined(ITK_LEGACY_REMOVE)
  static constexpr unsigned int ImageDimension = OutputImageDimension; // For backward compatibility
#endif

  /** base type for images of the current OutputImageDimension */
  using ImageBaseType = ImageBase<Self::OutputImageDimension>;

  /**
   *  Transform type alias.
   */
  using TransformType = Transform<TTransformPrecisionType, Self::OutputImageDimension, Self::InputImageDimension>;
  using TransformPointerType = typename TransformType::ConstPointer;
  using DecoratedTransformType = DataObjectDecorator<TransformType>;
  using DecoratedTransformPointer = typename DecoratedTransformType::Pointer;


  /** Interpolator type alias. */
  using InterpolatorType = InterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>;
  using InterpolatorPointerType = typename InterpolatorType::Pointer;

  using InterpolatorOutputType = typename InterpolatorType::OutputType;

  using InterpolatorConvertType = DefaultConvertPixelTraits<InterpolatorOutputType>;

  using ComponentType = typename InterpolatorConvertType::ComponentType;

  using LinearInterpolatorType = LinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>;
  using LinearInterpolatorPointerType = typename LinearInterpolatorType::Pointer;

  /** Extrapolator type alias. */
  using ExtrapolatorType = ExtrapolateImageFunction<InputImageType, TInterpolatorPrecisionType>;
  using ExtrapolatorPointerType = typename ExtrapolatorType::Pointer;

  /** Image size type alias. */
  using SizeType = Size<Self::OutputImageDimension>;

  /** Image index type alias. */
  using IndexType = typename TOutputImage::IndexType;

  /** Image point type alias. */
  using InputPointType = typename InterpolatorType::PointType;
  using OutputPointType = typename TOutputImage::PointType;

  /** Image pixel value type alias. */
  using PixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;

  using PixelConvertType = DefaultConvertPixelTraits<PixelType>;

  using PixelComponentType = typename PixelConvertType::ComponentType;

  /** Input pixel continuous index typdef */
  using ContinuousInputIndexType = ContinuousIndex<TInterpolatorPrecisionType, InputImageDimension>;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Image spacing,origin and direction type alias */
  using SpacingType = typename TOutputImage::SpacingType;
  using OriginPointType = typename TOutputImage::PointType;
  using DirectionType = typename TOutputImage::DirectionType;

  /** Typedef the reference image type to be the ImageBase of the OutputImageType */
  using ReferenceImageBaseType = ImageBase<OutputImageDimension>;

  /* See superclass for doxygen. This method adds the additional check
   * that the output space is set */
  void
  VerifyPreconditions() ITKv5_CONST override;

  /** Get/Set the coordinate transformation.
   * Set the coordinate transform to use for resampling.  Note that this must
   * be in physical coordinates and it is the output-to-input transform, NOT
   * the input-to-output transform that you might naively expect.  By default
   * the filter uses an Identity transform. You must provide a different
   * transform here, before attempting to run the filter, if you do not want to
   * use the default Identity transform. */
  itkSetGetDecoratedObjectInputMacro(Transform, TransformType);

  /** Get/Set the interpolator function.  The default is
   * LinearInterpolateImageFunction<InputImageType,
   * TInterpolatorPrecisionType>. Some
   * other options are NearestNeighborInterpolateImageFunction
   * (useful for binary masks and other images with a small number of
   * possible pixel values), and BSplineInterpolateImageFunction
   * (which provides a higher order of interpolation).  */
  itkSetObjectMacro(Interpolator, InterpolatorType);
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Get/Set the extrapolator function.  The default behavior when sampling outside
   * of the input image is to use the DefaultPixelValue.  Some other options
   * include NearestNeighborExtrapolateImageFunction. */
  itkSetObjectMacro(Extrapolator, ExtrapolatorType);
  itkGetModifiableObjectMacro(Extrapolator, ExtrapolatorType);

  /** Get/Set the size of the output image. */
  itkSetMacro(Size, SizeType);
  itkGetConstReferenceMacro(Size, SizeType);

  /** Get/Set the pixel value when a transformed pixel is outside of the
   * image.  The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue, PixelType);
  itkGetConstReferenceMacro(DefaultPixelValue, PixelType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void
  SetOutputSpacing(const double * spacing);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginPointType);
  virtual void
  SetOutputOrigin(const double * origin);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginPointType);

  /** Set the output direction cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Helper method to set the output parameters based on this image. */
  void
  SetOutputParametersFromImage(const ImageBaseType * image);

  /** Set the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

  /** Set a reference image to use to define the output information.
   *  By default, output information is specified through the
   *  SetOutputSpacing, SetOutputOrigin, and SetOutputDirection or
   *  SetOutputParametersFromImage methods.
   *  Alternatively, this method can be used to specify an image from which to
   *  copy the pixel information. UseReferenceImageOn must be set to utilize the
   *  reference image. */
  itkSetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Get the reference image that is defining the output information. */
  itkGetInputMacro(ReferenceImage, ReferenceImageBaseType);

  /** Turn on/off whether a specified reference image should be used to define
   *  the output information. */
  itkSetMacro(UseReferenceImage, bool);
  itkBooleanMacro(UseReferenceImage);
  itkGetConstMacro(UseReferenceImage, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelComponentType>));
  // End concept checking
#endif

protected:
  ResampleImageFilter();
  ~ResampleImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override
  {}

  /** ResampleImageFilter produces an image which is a different size
   * than its input.  As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model.  The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** ResampleImageFilter needs a different input requested region than
   * the output requested region.  As such, ResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** Set up state of filter before multi-threading.
   * InterpolatorType::SetInputImage is not thread-safe and hence
   * has to be set up before DynamicThreadedGenerateData */
  void
  BeforeThreadedGenerateData() override;

  /** Set the state of the filter after multi-threading. */
  void
  AfterThreadedGenerateData() override;

  /** Compute the Modified Time based on the changed components. */
  ModifiedTimeType
  GetMTime() const override;

  /** ResampleImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior
   * to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** Default implementation for resampling that works for any
   * transformation type. */
  virtual void
  NonlinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread);

  /** Implementation for resampling that works for with linear
   *  transformation types. */
  virtual void
  LinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread);

  /** Cast pixel from interpolator output to PixelType. */
  itkLegacyMacro(virtual PixelType CastPixelWithBoundsChecking(const InterpolatorOutputType value,
                                                               const ComponentType          minComponent,
                                                               const ComponentType          maxComponent) const);

private:
  static PixelComponentType
  CastComponentWithBoundsChecking(const PixelComponentType value);

  template <typename TComponent>
  static PixelComponentType
  CastComponentWithBoundsChecking(const TComponent value);

  static PixelType
  CastPixelWithBoundsChecking(const ComponentType value);

  template <typename TPixel>
  static PixelType
  CastPixelWithBoundsChecking(const TPixel value);

  void
  InitializeTransform();

  SizeType                m_Size;         // Size of the output image
  InterpolatorPointerType m_Interpolator; // Image function for
                                          // interpolation
  ExtrapolatorPointerType m_Extrapolator; // Image function for
                                          // extrapolation
  PixelType m_DefaultPixelValue;          // default pixel value
                                          // if the point is
                                          // outside the image
  SpacingType     m_OutputSpacing;        // output image spacing
  OriginPointType m_OutputOrigin;         // output image origin
  DirectionType   m_OutputDirection;      // output image direction cosines
  IndexType       m_OutputStartIndex;     // output image start index
  bool            m_UseReferenceImage{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkResampleImageFilter.hxx"
#endif

#endif
