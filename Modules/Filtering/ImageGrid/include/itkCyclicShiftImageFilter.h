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
#ifndef itkCyclicShiftImageFilter_h
#define itkCyclicShiftImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class CyclicShiftImageFilter
 * \brief Perform a cyclic spatial shift of image intensities on the
 * image grid.
 *
 * This filter supports arbitrary cyclic shifts of pixel values on the
 * image grid. If the Shift is set to [xOff, yOff], the value of the
 * pixel at [0, 0] in the input image will be the value of the pixel
 * in the output image at index [xOff modulo xSize, yOff modulo ySize]
 * where xSize and ySize are the sizes of the image in the x and y
 * dimensions, respectively. If a pixel value is moved across a
 * boundary, the pixel value is wrapped around that boundary. For
 * example, if the image is 40-by-40 and the Shift is [13, 47], then
 * the value of the pixel at [0, 0] in the input image will be the
 * value of the pixel in the output image at index [13, 7].
 *
 * Negative Shifts are supported. This filter also works with images
 * whose largest possible region starts at a non-zero index.
 *
 * \ingroup ITKImageGrid
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT CyclicShiftImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CyclicShiftImageFilter);

  /** Standard class type aliases. */
  using Self = CyclicShiftImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;
  using OffsetType = typename OutputImageType::OffsetType;
  using OffsetValueType = typename OutputImageType::OffsetValueType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(CyclicShiftImageFilter, ImageToImageFilter);

  /** Set/get the shift. Shifts may be positive or negative. */
  itkSetMacro(Shift, OffsetType);
  itkGetConstMacro(Shift, OffsetType);

protected:
  CyclicShiftImageFilter();
  ~CyclicShiftImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This filter needs the entire input be available so it needs to
   * provide an implementation of GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** This filter is multi-threaded. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType &) override;


  /** Protected so that subclasses may set it without calling
  Modified(). */
  OffsetType m_Shift;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCyclicShiftImageFilter.hxx"
#endif


#endif
