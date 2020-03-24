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
#ifndef itkMedianImageFilter_h
#define itkMedianImageFilter_h

#include "itkBoxImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 *\class MedianImageFilter
 * \brief Applies a median filter to an image
 *
 * Computes an image where a given pixel is the median value of the
 * the pixels in a neighborhood about the corresponding input pixel.
 *
 * A median filter is one of the family of nonlinear filters.  It is
 * used to smooth an image without being biased by outliers or shot noise.
 *
 * This filter requires that the input pixel type provides an operator<()
 * (LessThan Comparable).
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKSmoothing
 *
 * \sphinx
 * \sphinxexample{Filtering/Smoothing/ApplyMedianFilter,Median Filter Of An Image}
 * \sphinxexample{Filtering/Smoothing/MedianFilteringOfAnRGBImage,Median Filter Of An RGB Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT MedianImageFilter : public BoxImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MedianImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Standard class type aliases. */
  using Self = MedianImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MedianImageFilter, BoxImageFilter);

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using InputSizeType = typename InputImageType::SizeType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<InputPixelType, OutputPixelType>));
  itkConceptMacro(InputLessThanComparableCheck, (Concept::LessThanComparable<InputPixelType>));
  // End concept checking
#endif

protected:
  MedianImageFilter();
  ~MedianImageFilter() override = default;

  /** MedianImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMedianImageFilter.hxx"
#endif

#endif
