/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVectorResampleImageFilter_h
#define itkVectorResampleImageFilter_h

#include "itkTransform.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"
#include "itkSize.h"

namespace itk
{
/**
 * \class VectorResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * VectorResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function. The class is templated
 * over the types of the input and output images.
 *
 * Note that the choice of interpolator function can be important.
 * This function is set via SetInterpolator(). The default is
 * itk::VectorLinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>, which
 * is reasonable for ordinary medical images.
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::GenerateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter. It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 *
 * \deprecated ResampleImageFilter can now resample vector images and should
 * be used instead of the VectorResampleImageFilter.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKDeprecated
 *
 * \sphinx
 * \sphinxexample{Compatibility/Deprecated/ResampleRGBImage,Resample RGB Image}
 * \sphinxexample{Core/Transform/TranslateAVectorImage,Translate A Vector Image}
 * \sphinxexample{Compatibility/Deprecated/ResampleITK::VectorImage,Resample A Vector Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage, typename TInterpolatorPrecisionType = double>
class ITK_TEMPLATE_EXPORT VectorResampleImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorResampleImageFilter);

  /** Standard class type aliases. */
  using Self = VectorResampleImageFilter;
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
  itkTypeMacro(VectorResampleImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Transform type alias.
   *
   * \todo Check that input and output images have the same number of
   * dimensions; this is required by the current implementation of
   * AffineTransform. */
  using TransformType = Transform<TInterpolatorPrecisionType, Self::ImageDimension, Self::ImageDimension>;
  using TransformPointerType = typename TransformType::ConstPointer;

  /** Interpolator type alias. */
  using InterpolatorType = VectorInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>;
  using InterpolatorPointerType = typename InterpolatorType::Pointer;

  /** Image size type alias. */
  using SizeType = Size<Self::ImageDimension>;

  /** Image index type alias. */
  using IndexType = typename TOutputImage::IndexType;

  /** Image point type alias. */
  using PointType = typename InterpolatorType::PointType;

  /** Image pixel value type alias. */
  using PixelType = typename TOutputImage::PixelType;
  using PixelComponentType = typename PixelType::ValueType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Image spacing, origin and direction type alias. */
  using SpacingType = typename TOutputImage::SpacingType;
  using OriginPointType = typename TOutputImage::PointType;
  using DirectionType = typename TOutputImage::DirectionType;

  /** Set/Get the coordinate transformation.
   * Set the coordinate transform to use for resampling. Note that this
   * must be in index coordinates and is the output-to-input transform,
   * NOT the input-to-output transform that you might naively expect.
   * The default is itk::AffineTransform<TInterpolatorPrecisionType, ImageDimension>. */
  itkSetConstObjectMacro(Transform, TransformType);
  itkGetConstObjectMacro(Transform, TransformType);

  /** Set the interpolator function. The default is
   * itk::VectorLinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>.  */
  itkSetObjectMacro(Interpolator, InterpolatorType);

  /** Get a pointer to the interpolator function. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the size of the output image. */
  itkSetMacro(Size, SizeType);

  /** Get the size of the output image. */
  itkGetConstReferenceMacro(Size, SizeType);

  /** Set the pixel value when a transformed pixel is outside of the
   * image. The default default pixel value is 0. */
  itkSetMacro(DefaultPixelValue, PixelType);

  /** Get the pixel value when a transformed pixel is outside of the image */
  itkGetConstMacro(DefaultPixelValue, PixelType);

  /** Set the output image spacing. */
  itkSetMacro(OutputSpacing, SpacingType);
  virtual void
  SetOutputSpacing(const double * values);

  /** Get the output image spacing. */
  itkGetConstReferenceMacro(OutputSpacing, SpacingType);

  /** Set the output image origin. */
  itkSetMacro(OutputOrigin, OriginPointType);
  virtual void
  SetOutputOrigin(const double * values);

  /** Get the output image origin. */
  itkGetConstReferenceMacro(OutputOrigin, OriginPointType);

  /** Set/Get the output direction cosine matrix. */
  itkSetMacro(OutputDirection, DirectionType);
  itkGetConstReferenceMacro(OutputDirection, DirectionType);

  /** Set the start index of the output largest possible region.
   * The default is an index of all zeros. */
  itkSetMacro(OutputStartIndex, IndexType);

  /** Get the start index of the output largest possible region. */
  itkGetConstReferenceMacro(OutputStartIndex, IndexType);

  /** VectorResampleImageFilter produces an image which is a different size
   * than its input. As such, it needs to provide an implementation
   * for GenerateOutputInformation() in order to inform the pipeline
   * execution model. The original documentation of this method is
   * below. \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** VectorResampleImageFilter needs a different input requested region than
   * the output requested region. As such, VectorResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** Set the state of the filter before multi-threading.
   * Note that InterpolatorType::SetInputImage is not thread-safe and hence
   * has to be set up before DynamicThreadedGenerateData. */
  void
  BeforeThreadedGenerateData() override;

  /** Set the state of the filter after multi-threading. */
  void
  AfterThreadedGenerateData() override;

  /** Compute the Modified Time based on changed to the components. */
  ModifiedTimeType
  GetMTime() const override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelComponentType>));
  // End concept checking
#endif

protected:
  VectorResampleImageFilter();
  ~VectorResampleImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** VectorResampleImageFilter can be implemented as a multithreaded filter. Therefore,
   * this implementation provides a DynamicThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  SizeType                m_Size;
  TransformPointerType    m_Transform;
  InterpolatorPointerType m_Interpolator;
  PixelType               m_DefaultPixelValue;
  SpacingType             m_OutputSpacing;
  OriginPointType         m_OutputOrigin;
  DirectionType           m_OutputDirection;
  IndexType               m_OutputStartIndex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorResampleImageFilter.hxx"
#endif

#endif
