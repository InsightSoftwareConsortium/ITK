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
#ifndef itkMaskNeighborhoodOperatorImageFilter_h
#define itkMaskNeighborhoodOperatorImageFilter_h

#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/** \class MaskNeighborhoodOperatorImageFilter
 * \brief Applies a single NeighborhoodOperator to an image,
 * processing only those pixels that are under a mask.
 *
 * This filter calculates successive inner products between a single
 * NeighborhoodOperator and a NeighborhoodIterator, which is swept
 * across every pixel that is set in the input mask. If no mask is
 * given, this filter is equivalent to its superclass. Output pixels
 * that are outside of the mask will be set to DefaultValue if
 * UseDefaultValue is true (default). Otherwise, they will be set to
 * the value of the input pixel.
 *
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodOperatorImageFilter
 * \sa NeighborhoodIterator
 * \ingroup ITKImageFilterBase
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFilterBase/ApplyKernelToEveryPixelInNonZeroImage,Apply Kernel To Every Pixel In
 * Non-Zero Image} \endsphinx
 */
template <typename TInputImage,
          typename TMaskImage,
          typename TOutputImage,
          typename TOperatorValueType = typename TOutputImage::PixelType>
class ITK_TEMPLATE_EXPORT MaskNeighborhoodOperatorImageFilter
  : public NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MaskNeighborhoodOperatorImageFilter);

  /** Standard "Self" & Superclass type alias. */
  using Self = MaskNeighborhoodOperatorImageFilter;
  using Superclass = NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskNeighborhoodOperatorImageFilter, NeighborhoodOperatorImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using MaskPixelType = typename TMaskImage::PixelType;
  using MaskInternalPixelType = typename TMaskImage::InternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int MaskImageDimension = TMaskImage::ImageDimension;

  /** Image type alias support */
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using MaskImagePointer = typename MaskImageType::Pointer;

  /** Typedef for generic boundary condition pointer. */
  using ImageBoundaryConditionPointerType = ImageBoundaryCondition<OutputImageType> *;

  /** Superclass type alias. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OperatorValueType = typename Superclass::OperatorValueType;

  /** Neighborhood types */
  using OutputNeighborhoodType = typename Superclass::OutputNeighborhoodType;

  /** Set the mask image. Using a mask is optional.  When a mask is
   * specified, the normalized correlation is only calculated for
   * those pixels under the mask. */
  void
  SetMaskImage(const TMaskImage * mask);

  /** Get the mask image. Using a mask is optional.  When a mask is
   * specified, the normalized correlation is only calculated for
   * those pixels under the mask. */
  const TMaskImage *
  GetMaskImage() const;

  /** Set the output value for the pixels that are not under the mask.
   * Defaults to zero.
   */
  itkSetMacro(DefaultValue, OutputPixelType);

  /** Get the output value for the pixels that are not under the
   * mask. */
  itkGetConstMacro(DefaultValue, OutputPixelType);

  /** Set the UseDefaultValue flag. If true, the pixels outside the
   *  mask will e set to m_DefaultValue. Otherwise, they will be set
   *  to the input pixel. */
  itkSetMacro(UseDefaultValue, bool);

  /** Get the UseDefaultValue flag. */
  itkGetConstReferenceMacro(UseDefaultValue, bool);

  /** Turn on and off the UseDefaultValue flag. */
  itkBooleanMacro(UseDefaultValue);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<InputImageDimension, ImageDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<InputImageDimension, MaskImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  itkConceptMacro(OperatorConvertibleToOutputCheck, (Concept::Convertible<OperatorValueType, OutputPixelType>));
  itkConceptMacro(OutputOStreamWritable, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  MaskNeighborhoodOperatorImageFilter()
    : m_DefaultValue(NumericTraits<OutputPixelType>::ZeroValue())
  {}
  ~MaskNeighborhoodOperatorImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** MaskNeighborhoodOperatorImageFilter needs to request enough of an
   * input image to account for template size.  The input requested
   * region is expanded by the radius of the template.  If the request
   * extends past the LargestPossibleRegion for the input, the request
   * is cropped by the LargestPossibleRegion. */
  void
  GenerateInputRequestedRegion() override;

  /** MaskNeighborhoodOperatorImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * DynamicThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


private:
  OutputPixelType m_DefaultValue;
  bool            m_UseDefaultValue{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMaskNeighborhoodOperatorImageFilter.hxx"
#endif

#endif
