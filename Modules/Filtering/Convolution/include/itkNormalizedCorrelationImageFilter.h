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
#ifndef itkNormalizedCorrelationImageFilter_h
#define itkNormalizedCorrelationImageFilter_h

#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/**
 *\class NormalizedCorrelationImageFilter
 * \brief Computes the normalized correlation of an image and a template.
 *
 * This filter calculates the normalized correlation between an image
 * and the template.  Normalized correlation is frequently use in
 * feature detection because it is invariant to local changes in
 * contrast.
 *
 * The filter can be given a mask. When presented with an input image
 * and a mask, the normalized correlation is only calculated at those
 * pixels under the mask.
 *
 * \ingroup ImageFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \ingroup ITKConvolution
 *
 * \sphinx
 * \sphinxexample{Filtering/Convolution/NormalizedCorrelation,Normalized Correlation}
 * \sphinxexample{Filtering/Convolution/NormalizedCorrelationOfMaskedImage,Normalized Correlation Of Masked Image}
 * \sphinxexample{Filtering/Convolution/ColorNormalizedCorrelation,Color Normalized Operation}
 * \endsphinx
 */
template <typename TInputImage,
          typename TMaskImage,
          typename TOutputImage,
          typename TOperatorValueType = typename TOutputImage::PixelType>
class ITK_TEMPLATE_EXPORT NormalizedCorrelationImageFilter
  : public NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(NormalizedCorrelationImageFilter);

  /** Standard "Self" & Superclass type alias. */
  using Self = NormalizedCorrelationImageFilter;
  using Superclass = NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NormalizedCorrelationImageFilter, NeighborhoodOperatorImageFilter);

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
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
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

  /** Set the template used in the calculation of the normalized
   * correlation. The elements of the template must be set prior to
   * calling SetTemplate(). */
  void
  SetTemplate(const OutputNeighborhoodType & t)
  {
    this->SetOperator(t);
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, MaskImageDimension>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  itkConceptMacro(OperatorHasNumericTraitsCheck, (Concept::HasNumericTraits<OperatorValueType>));
  // This filter can only operate on data types that are signed.
  itkConceptMacro(SignedOutputPixelType, (Concept::Signed<OutputPixelType>));
  // End concept checking
#endif

protected:
  NormalizedCorrelationImageFilter() = default;
  ~NormalizedCorrelationImageFilter() override = default;

  /** NormalizedCorrelationImageFilter needs to request enough of an
   * input image to account for template size.  The input requested
   * region is expanded by the radius of the template.  If the request
   * extends past the LargestPossibleRegion for the input, the request
   * is cropped by the LargestPossibleRegion. */
  void
  GenerateInputRequestedRegion() override;

  /** NormalizedCorrelationImageFilter can be implemented as a
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

  /** Standard PrintSelf method */
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizedCorrelationImageFilter.hxx"
#endif

#endif
