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
#ifndef itkNoiseImageFilter_h
#define itkNoiseImageFilter_h

#include "itkBoxImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class NoiseImageFilter
 * \brief Calculate the local noise in an image.
 *
 * Computes an image where a given pixel is the standard deviation of
 * the pixels in a neighborhood about the corresponding input pixel.
 * This serves as an estimate of the local noise (or texture) in an
 * image. Currently, this noise estimate assume a piecewise constant
 * image.  This filter should be extended to fitting a (hyper) plane
 * to the neighborhood and calculating the standard deviation of the
 * residuals to this (hyper) plane.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKImageFilterBase
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageFilterBase/ComputerLocalNoise,Compute Local Noise In Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NoiseImageFilter : public BoxImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(NoiseImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = NoiseImageFilter;
  using Superclass = BoxImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NoiseImageFilter, BoxImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputRealType = typename NumericTraits<InputPixelType>::RealType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputSizeType = typename InputImageType::SizeType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputPixelType>));
  // End concept checking
#endif

protected:
  NoiseImageFilter();
  ~NoiseImageFilter() override = default;

  /** NoiseImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling DynamicThreadedGenerateData().  DynamicThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa BoxImageFilter::ThreadedGenerateData(),
   *     BoxImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNoiseImageFilter.hxx"
#endif

#endif
