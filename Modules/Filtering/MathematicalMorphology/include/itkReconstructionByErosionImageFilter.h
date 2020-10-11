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
#ifndef itkReconstructionByErosionImageFilter_h
#define itkReconstructionByErosionImageFilter_h

#include "itkReconstructionImageFilter.h"

#include "itkNumericTraits.h"

namespace itk
{
/** \class ReconstructionByErosionImageFilter
 * \brief grayscale reconstruction by erosion of an image
 *
 * Reconstruction by erosion operates on a "marker" image and a "mask"
 * image, and is defined as the erosion of the marker image with
 * respect to the mask image iterated until stability.
 *
 * The marker image must be less than or equal to the mask image
 * (on a pixel by pixel basis).
 *
 * Geodesic morphology is described in Chapter 6.2 of Pierre Soille's
 * book "Morphological Image Analysis: Principles and Applications",
 * Second Edition, Springer, 2003.
 *
 * Algorithm implemented in this filter is based on algorithm described
 * by Kevin Robinson and  Paul F. Whelan in "Efficient Morphological
 * Reconstruction: A Downhill Filter", Pattern Recognition Letters, Volume
 * 25, Issue 15, November 2004, Pages 1759-1767.
 *
 * The algorithm, a description of the transform and some applications
 * can be found in "Morphological Grayscale Reconstruction in Image Analysis:
 * Applications and Efficient Algorithms", Luc Vincent, IEEE Transactions on
 * image processing, Vol. 2, April 1993.
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter,
ReconstructionByErosionImageFilter, OpeningByReconstructionImageFilter, ClosingByReconstructionImageFilter,
ReconstructionImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TOutputImage>
class ReconstructionByErosionImageFilter
  : public ReconstructionImageFilter<TInputImage, TOutputImage, std::less<typename TOutputImage::PixelType>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ReconstructionByErosionImageFilter);

  using Self = ReconstructionByErosionImageFilter;
  using Superclass = ReconstructionImageFilter<TInputImage, TOutputImage, std::less<typename TOutputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using MarkerImageType = TInputImage;
  using MarkerImagePointer = typename MarkerImageType::Pointer;
  using MarkerImageConstPointer = typename MarkerImageType::ConstPointer;
  using MarkerImageRegionType = typename MarkerImageType::RegionType;
  using MarkerImagePixelType = typename MarkerImageType::PixelType;
  using MaskImageType = TInputImage;
  using MaskImagePointer = typename MaskImageType::Pointer;
  using MaskImageConstPointer = typename MaskImageType::ConstPointer;
  using MaskImageRegionType = typename MaskImageType::RegionType;
  using MaskImagePixelType = typename MaskImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int MarkerImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int MaskImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ReconstructionByErosionImageFilter, ReconstructionImageFilter);

protected:
  ReconstructionByErosionImageFilter() { this->m_MarkerValue = NumericTraits<typename TOutputImage::PixelType>::max(); }

  ~ReconstructionByErosionImageFilter() override = default;
}; // end
   //
   //
   //
   //
   // ReconstructionByErosionImageFilter
} // namespace itk

#endif
