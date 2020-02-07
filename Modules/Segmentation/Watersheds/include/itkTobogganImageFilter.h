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
#ifndef itkTobogganImageFilter_h
#define itkTobogganImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/** \class TobogganImageFilter
 * \brief toboggan image segmentation
 * The Toboggan segmentation takes a gradient magnitude image
 * as input and produces an (over-)segmentation of the image based
 * on connecting each pixel to a local minimum of gradient.  It is
 * roughly equivalent to a watershed segmentation of the lowest level.
 *
 * The output is a 4 connected labeled map of the image.
 * \ingroup Segmentation
 * \ingroup ITKWatersheds
 */

template <typename TInputImage, typename TOutputImage = Image<IdentifierType, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT TobogganImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TobogganImageFilter);

  /** Standard "Self" type alias.   */
  using Self = TobogganImageFilter;

  /** The type of input image.   */
  using InputImageType = TInputImage;

  /** Number of dimensions. */
  static constexpr unsigned int NDimensions = TInputImage::ImageDimension;

  /** The type of output image.   */
  using OutputImageType = TOutputImage;

  /** Output image pixel type. */
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Input image pixel type. */
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Dimension of the input and output images. */
  enum
  {
    ImageDimension = InputImageType::ImageDimension
  };

  /** Other convenient type alias   */
  using RegionType = typename InputImageType::RegionType;
  using SizeType = typename InputImageType::SizeType;
  using IndexType = typename InputImageType::IndexType;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using OutputImagePointer = typename OutputImageType::Pointer;

  /** Standard super class type alias support */
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;

  /** Typedef support for the input image scalar value type. */
  using ScalarType = typename InputImageType::PixelType;

  /** Smart pointer type alias support  */
  using Pointer = SmartPointer<Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(TobogganImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard process object method.  This filter is not multithreaded. */
  void
  GenerateData() override;

  void
  GenerateInputRequestedRegion() override;

  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  /** Neighborhood iterator type */
  using NeighborhoodIteratorType = ConstNeighborhoodIterator<Image<float, 2>>;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(LessThanComparableCheck, (Concept::LessThanComparable<InputImagePixelType>));
  itkConceptMacro(OStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

protected:
  TobogganImageFilter() = default;
  ~TobogganImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTobogganImageFilter.hxx"
#endif

#endif
