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
#ifndef itkBinShrinkImageFilter_h
#define itkBinShrinkImageFilter_h

#include "itkShrinkImageFilter.h"
#include <type_traits>

namespace itk
{

/**
 *\class BinShrinkImageFilter
 * \brief Reduce the size of an image by an integer factor in each
 * dimension while performing averaging of an input neighborhood.
 *
 *
 * The output image size in each dimension is given by:
 *
 * outputSize[j] = max( std::floor(inputSize[j]/shrinkFactor[j]), 1 );
 *
 * The algorithm implemented can be describe with the following
 * equation for 2D:
 * \f[
 *  \mathsf{I}_{out}(x_o,x_1) =
 *    \frac{\sum_{i=0}^{f_0}\sum_{j=0}^{f_1}\mathsf{I}_{in}(f_0 x_o+i,f_1 x_1+j)}{f_0 f_1}
 * \f]
 *
 * This filter is implemented so that the starting extent of the first
 * pixel of the output matches that of the input.
 *
 * \image html BinShrinkGrid.png "The change in image geometry from a 5x5 image binned by a factor of 2x2."
 *
 * This code was contributed in the Insight Journal paper:
 * "BinShrink: A multi-resolution filter with cache efficient averaging"
 *  by Lowekamp B., Chen D.
 * https://hdl.handle.net/10380/3450
 *
 * \ingroup ITKImageGrid
 * \ingroup Streamed
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT BinShrinkImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinShrinkImageFilter);

  /** Standard class type aliases. */
  using Self = BinShrinkImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinShrinkImageFilter, ImageToImageFilter);

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

  /** BinShrinkImageFilter needs a larger input requested region than the output
   * requested region.  As such, BinShrinkImageFilter needs to provide an
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
  BinShrinkImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  ShrinkFactorsType m_ShrinkFactors;

  /** Round different pixel types. */
  template <class TOutputType, class TInputType>
  typename std::enable_if<std::numeric_limits<TOutputType>::is_integer, TOutputType>::type
  RoundIfInteger(TInputType input)
  {
    return Math::Round<TOutputType>(input);
  }

  // For Non-fundamental types numeric_limits is not specialized, and
  // is_integer defaults to false.
  template <class TOutputType, class TInputType>
  typename std::enable_if<!std::numeric_limits<TOutputType>::is_integer, TOutputType>::type
  RoundIfInteger(const TInputType & input, ...)
  {
    return static_cast<TOutputType>(input);
  }
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinShrinkImageFilter.hxx"
#endif

#endif
