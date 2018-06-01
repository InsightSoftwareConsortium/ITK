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
#ifndef itkExpandWithZerosImageFilter_h
#define itkExpandWithZerosImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/** \class ExpandWithZerosImageFilter
 * \brief Expand the size of an image by an integer factor in each
 * dimension.
 *
 * ExpandWithZerosImageFilter increases the size of an image by an integer
 * factor in each dimension. No interpolation method is used, pixels with zero
 * value are added at intercalated indices in function of ExpandFactors.
 * The first index from the input image is kept, and zeros are added in index positions
 * that are multiple of ExpandFactors in each dimension.
 * The output image size in each dimension is given by:
 *
 * OutputSize[j] = InputSize[j] * ExpandFactors[j]
 *
 * This filter will produce an output with different pixel spacing
 * that its input image such that:
 *
 * OutputSpacing[j] = InputSpacing[j] / ExpandFactors[j]
 *
 * The filter is templated over the input image type and the output
 * image type.
 *
 * This filter is implemented as a multithreaded filter and supports
 * streaming.
 *
 * \warning This filter only works for image with scalar pixel types.
 * For vector images use VectorExpandImageFilter.
 *
 * This filter assumes that the input and output image has the same
 * number of dimensions.
 *
 * \sa ExpandImageFilter
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage, typename TOutputImage>
class ExpandWithZerosImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExpandWithZerosImageFilter);

  /** Standard class type alias. */
  using Self = ExpandWithZerosImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExpandWithZerosImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;
  /** Typedef to describe the output image region type. */
  using InputImageRegionType = typename TInputImage::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Inherit some types from superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputImagePointer = typename InputImageType::Pointer;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** The type of the expand factors representation */
  using ExpandFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the expand factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ExpandFactors, ExpandFactorsType);
  virtual void
  SetExpandFactors(const unsigned int factor);

  /** Get the expand factors. */
  itkGetConstReferenceMacro(ExpandFactors, ExpandFactorsType);

  /** ExpandWithZerosImageFilter produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * ExpandWithZerosImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** ExpandWithZerosImageFilter needs a smaller input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TInputImage::PixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  // End concept checking
#endif

protected:
  ExpandWithZerosImageFilter();
  ~ExpandWithZerosImageFilter() override {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ExpandWithZerosImageFilter is implemented as a multithreaded filter.  Therefore,
   * this implementation provides a DynamicThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::DynamicThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  /** This method is used to set the state of the filter before
   * multi-threading. */
  void
  BeforeThreadedGenerateData() override;

private:
  ExpandFactorsType m_ExpandFactors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExpandWithZerosImageFilter.hxx"
#endif

#endif
