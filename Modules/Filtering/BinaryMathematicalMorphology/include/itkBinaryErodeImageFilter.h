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
#ifndef itkBinaryErodeImageFilter_h
#define itkBinaryErodeImageFilter_h

#include <vector>
#include <queue>
#include "itkBinaryMorphologyImageFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/**
 * \class BinaryErodeImageFilter
 * \brief Fast binary erosion of a single intensity value in the image.
 *
 * BinaryErodeImageFilter is a binary erosion
 * morphologic operation on the foreground of an image. Only the value designated
 * by the intensity value "SetForegroundValue()" (alias as SetErodeValue()) is considered
 * as foreground, and other intensity values are considered background.
 *
 * Grayscale images can be processed as binary images by selecting a
 * "ForegroundValue" (alias "ErodeValue").  Pixel values matching the erode value are
 * considered the "foreground" and all other pixels are
 * "background". This is useful in processing segmented images where
 * all pixels in segment #1 have value 1 and pixels in segment #2 have
 * value 2, etc. A particular "segment number" can be processed.
 * ForegroundValue defaults to the maximum possible value of the
 * PixelType. The eroded pixels will receive the BackgroundValue
 * (defaults to NumericTraits::NonpositiveMin() ).
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 are candidates for affecting the center pixel.  A
 * reasonable choice of structuring element is
 * itk::BinaryBallStructuringElement.
 *
 * This implementation is based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 *
 * \sa ImageToImageFilter BinaryDilateImageFilter BinaryMorphologyImageFilter
 * \ingroup ITKBinaryMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/BinaryMathematicalMorphology/ErodeABinaryImage,Erode A Binary Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT BinaryErodeImageFilter
  : public BinaryMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryErodeImageFilter);

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
  using Self = BinaryErodeImageFilter;
  using Superclass = BinaryMorphologyImageFilter<InputImageType, OutputImageType, KernelType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryErodeImageFilter, BinaryMorphologyImageFilter);

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** Image type alias support */
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;
  using InputRealType = typename NumericTraits<InputPixelType>::RealType;
  using OffsetType = typename InputImageType::OffsetType;
  using IndexType = typename InputImageType::IndexType;

  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using InputSizeType = typename InputImageType::SizeType;

  /** Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * SetForegroundValue in the superclass. */
  void
  SetErodeValue(const InputPixelType & value)
  {
    this->SetForegroundValue(value);
  }

  /** Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType. This is an alias to the
   * GetForegroundValue in the superclass. */
  InputPixelType
  GetErodeValue() const
  {
    return this->GetForegroundValue();
  }

protected:
  BinaryErodeImageFilter();
  ~BinaryErodeImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  // type inherited from the superclass
  using NeighborIndexContainer = typename Superclass::NeighborIndexContainer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryErodeImageFilter.hxx"
#endif

#endif
