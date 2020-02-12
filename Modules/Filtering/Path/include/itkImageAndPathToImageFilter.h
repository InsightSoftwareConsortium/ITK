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
#ifndef itkImageAndPathToImageFilter_h
#define itkImageAndPathToImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class ImageAndPathToImageFilter
 * \brief Base class for filters that take both a path and an image as input and produce a path as output.
 *
 * This class is the base class for filters that take both an image and a path
 * as input and produce an image as output.  Specifically, this class defines
 * the methods SetPathInput() and SetImageInput().  (It also establishes the
 * precedent of having image inputs precede path inputs for functions producing
 * images as outputs, according to the underlying DataObject implementation.)
 *
 * \ingroup ImageFilters
 * \ingroup PathFilters
 * \ingroup ITKPath
 */
template <typename TInputImage, typename TInputPath, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ImageAndPathToImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageAndPathToImageFilter);

  /** Standard class type aliases. */
  using Self = ImageAndPathToImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageAndPathToImageFilter, ImageToImageFilter);

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputPathType = TInputPath;
  using InputPathPointer = typename InputPathType::Pointer;
  using InputPathConstPointer = typename InputPathType::ConstPointer;
  using InputPathInputType = typename InputPathType::InputType;
  using InputPathOutputType = typename InputPathType::OutputType;
  using InputPathIndexType = typename InputPathType::IndexType;
  using InputPathOffsetType = typename InputPathType::OffsetType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Set/Get the image input of this process object. */
  virtual void
  SetImageInput(const TInputImage * image);


  /** Set/Get the path input of this process object. */
  virtual void
  SetPathInput(const TInputPath * path);

  const InputImageType *
  GetImageInput();
  const InputPathType *
  GetPathInput();

protected:
  InputImageType *
  GetNonConstImageInput();
  InputPathType *
  GetNonConstPathInput();
  ImageAndPathToImageFilter();
  ~ImageAndPathToImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageAndPathToImageFilter.hxx"
#endif

#endif
