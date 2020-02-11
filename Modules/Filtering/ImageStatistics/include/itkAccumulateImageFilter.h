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
#ifndef itkAccumulateImageFilter_h
#define itkAccumulateImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class AccumulateImageFilter
 * \brief Implements an accumulation of an image along a selected direction.
 *
 *    This class accumulates an image along a dimension and reduce the size
 * of this dimension to 1. The dimension being accumulated is set by
 * AccumulateDimension.
 *
 *   Each pixel is the cumulative sum of the pixels along the collapsed
 * dimension and reduce the size of the accumulated dimension to 1 (only
 * on the accumulated).
 *
 *   The dimensions of the InputImage and the OutputImage must be the same.
 *
 *
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 *
 * \author Emiliano Beronich
 *
 * This filter was contributed by Emiliano Beronich
 *
 * \sa GetAverageSliceImageFilter
 *
 * \ingroup   IntensityImageFilters     SingleThreaded
 * \ingroup ITKImageStatistics
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT AccumulateImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AccumulateImageFilter);

  /** Standard class type aliases. */
  using Self = AccumulateImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AccumulateImageFilter, ImageToImageFilter);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension enumeration */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Input and output images must be the same dimension, or the output's
      dimension must be one less than that of the input. */
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageDimensionCheck, (Concept::SameDimension<Self::InputImageDimension, Self::OutputImageDimension>));
  // End concept checking
#endif

  /** Set the direction in which to accumulate the data.  It must be
   * set before the update of the filter. Defaults to the last
   * dimension. */
  itkGetConstMacro(AccumulateDimension, unsigned int);
  itkSetMacro(AccumulateDimension, unsigned int);

  /** Perform a division by the size of the accumulated dimension
   * after the accumulation is done. If true, the output image is the
   * average of the accumulated dimension, if false the output is the
   * sum of the pixels along the selected direction.  The default
   * value is false. */
  itkSetMacro(Average, bool);
  itkGetConstMacro(Average, bool);
  itkBooleanMacro(Average);

protected:
  AccumulateImageFilter();
  ~AccumulateImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Apply changes to the output image information. */
  void
  GenerateOutputInformation() override;

  /** Apply changes to the input image requested region. */
  void
  GenerateInputRequestedRegion() override;

  /** This method implements the actual accumulation of the image.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  GenerateData() override;

private:
  unsigned int m_AccumulateDimension;
  bool         m_Average;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAccumulateImageFilter.hxx"
#endif

#endif
