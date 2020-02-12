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
#ifndef itkCheckerBoardImageFilter_h
#define itkCheckerBoardImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class CheckerBoardImageFilter
 * \brief Combines two images in a checkerboard pattern.
 *
 * CheckerBoardImageFilter takes two input images that must have the same
 * dimension, size, origin and spacing and produces an output image of the same
 * size by combining the pixels from the two input images in a checkerboard
 * pattern. This filter is commonly used for visually comparing two images, in
 * particular for evaluating the results of an image registration process.
 *
 * This filter is implemented as a multithreaded filter. It provides a
 * DynamicThreadedGenerateData() method for its implementation.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageCompare
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageCompare/CombineTwoImagesWithCheckerBoardPattern,Combine Two Images With Checker Board
 * Pattern} \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT CheckerBoardImageFilter : public ImageToImageFilter<TImage, TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CheckerBoardImageFilter);

  /** Standard class type aliases. */
  using Self = CheckerBoardImageFilter;
  using Superclass = ImageToImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TImage;
  using OutputImageType = TImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using ImageRegionType = typename OutputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CheckerBoardImageFilter, ImageToImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Type to hold the number of checker boxes per dimension. */
  using PatternArrayType = FixedArray<unsigned int, TImage ::ImageDimension>;

  /** Set the first operand for checker board. */
  void
  SetInput1(const TImage * image1);

  /** Set the second operand for checker board. */
  void
  SetInput2(const TImage * image2);

  /** Set/Get the checker pattern array, i.e. the number of checker boxes
   * per image dimension. */
  itkSetMacro(CheckerPattern, PatternArrayType);
  itkGetConstReferenceMacro(CheckerPattern, PatternArrayType);

protected:
  CheckerBoardImageFilter();
  ~CheckerBoardImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** CheckerBoardImageFilter can be implemented as a multithreaded filter. Therefore,
   * this implementation provides a DynamicThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling DynamicThreadedGenerateData().
   * DynamicThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const ImageRegionType & outputRegionForThread) override;


private:
  PatternArrayType m_CheckerPattern;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCheckerBoardImageFilter.hxx"
#endif

#endif
