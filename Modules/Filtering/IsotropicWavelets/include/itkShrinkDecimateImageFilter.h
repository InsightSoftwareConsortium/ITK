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
#ifndef itkShrinkDecimateImageFilter_h
#define itkShrinkDecimateImageFilter_h

#include "itkShrinkImageFilter.h"
#include "itkEnableIf.h"

namespace itk
{
/** \class ShrinkDecimateImageFilter
 * \brief Reduce the size of an image by an integer factor in each
 * dimension just cutting off samples without any interpolation.
 * The first index is always kept.
 *
 * The output image size in each dimension is given by:
 *
 * outputSize[j] = max( std::floor(inputSize[j]/shrinkFactor[j]), 1 );
 *
 * This filter is implemented so that the starting extent of the first
 * pixel of the output matches that of the input.
 *
 * \ingroup ITKImageGrid
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage, typename TOutputImage>
class ShrinkDecimateImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShrinkDecimateImageFilter);

  /** Standard class type alias. */
  using Self = ShrinkDecimateImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShrinkDecimateImageFilter, ImageToImageFilter);

  /** Typedef to images */
  using OutputImageType = TOutputImage;
  using InputImageType = TInputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  using OutputOffsetType = typename TOutputImage::OffsetType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using InputIndexType = typename TInputImage::IndexType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  using ShrinkFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the shrink factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ShrinkFactors, ShrinkFactorsType);
  void
  SetShrinkFactors(unsigned int factor);

  void
  SetShrinkFactor(unsigned int i, unsigned int factor);

  /** Get the shrink factors. */
  itkGetConstReferenceMacro(ShrinkFactors, ShrinkFactorsType);

  void
  GenerateOutputInformation() override;

  /** ShrinkDecimateImageFilter needs a larger input requested region than the output
   * requested region.  As such, ShrinkDecimateImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  /** End concept checking */
#endif

protected:
  ShrinkDecimateImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  ShrinkFactorsType m_ShrinkFactors;

  /** Round different pixel types. */
  template <class TOutputType, class TInputType>
  typename EnableIfC<std::numeric_limits<TOutputType>::is_integer, TOutputType>::Type
  RoundIfInteger(TInputType input)
  {
    return Math::Round<TOutputType>(input);
  }

  // For Non-fundamental types numeric_limits is not specialized, and
  // is_integer defaults to false.
  template <class TOutputType, class TInputType>
  typename DisableIfC<std::numeric_limits<TOutputType>::is_integer, TOutputType>::Type
  RoundIfInteger(const TInputType & input, ...)
  {
    return static_cast<TOutputType>(input);
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShrinkDecimateImageFilter.hxx"
#endif

#endif
