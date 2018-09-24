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
#ifndef itkBinaryDilateImageFilter_h
#define itkBinaryDilateImageFilter_h

#include <vector>
#include <queue>
#include "itkBinaryMorphologyImageFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/**
 * \class BinaryDilateImageFilter
 * \brief Fast binary dilation
 *
 * BinaryDilateImageFilter is a binary dilation
 * morphologic operation. This implementation is based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 * Gray scale images can be processed as binary images by selecting a
 * "DilateValue".  Pixel values matching the dilate value are
 * considered the "foreground" and all other pixels are
 * "background". This is useful in processing segmented images where
 * all pixels in segment #1 have value 1 and pixels in segment #2 have
 * value 2, etc. A particular "segment number" can be processed.
 * DilateValue defaults to the maximum possible value of the
 * PixelType.
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 are candidates for affecting the center pixel.  A
 * reasonable choice of structuring element is
 * itk::BinaryBallStructuringElement.
 *
 * \sa ImageToImageFilter BinaryErodeImageFilter BinaryMorphologyImageFilter
 * \ingroup ITKBinaryMathematicalMorphology
 *
 * \wiki
 * \wikiexample{Morphology/BinaryDilateImageFilter,Dilate a binary image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage, typename TKernel >
class ITK_TEMPLATE_EXPORT BinaryDilateImageFilter:
  public BinaryMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryDilateImageFilter);

  /** Extract dimension from input and output image. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Extract the dimension of the kernel */
  static constexpr unsigned int KernelDimension = TKernel::NeighborhoodDimension;

  /** Convenient type alias for simplifying declarations. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using KernelType = TKernel;

  /** Standard class type aliases. */
  using Self = BinaryDilateImageFilter;
  using Superclass = BinaryMorphologyImageFilter< InputImageType, OutputImageType,
                                       KernelType >;

  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryDilateImageFilter, BinaryMorphologyImageFilter);

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputRealType = typename NumericTraits< InputPixelType >::RealType;
  using OffsetType = typename InputImageType::OffsetType;
  using IndexType = typename InputImageType::IndexType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using InputSizeType = typename InputImageType::SizeType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * ForegroundValue in the superclass. */
  void SetDilateValue(const InputPixelType & value)
  { this->SetForegroundValue(value); }

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * ForegroundValue in the superclass. */
  InputPixelType GetDilateValue() const
  { return this->GetForegroundValue(); }

protected:
  BinaryDilateImageFilter();
  ~BinaryDilateImageFilter() override = default;
  void PrintSelf(std::ostream & os, Indent indent) const override;

  void GenerateData() override;

  // type inherited from the superclass
  using NeighborIndexContainer = typename Superclass::NeighborIndexContainer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryDilateImageFilter.hxx"
#endif

#endif
