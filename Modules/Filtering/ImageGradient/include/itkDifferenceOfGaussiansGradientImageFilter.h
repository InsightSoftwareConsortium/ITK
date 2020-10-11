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
#ifndef itkDifferenceOfGaussiansGradientImageFilter_h
#define itkDifferenceOfGaussiansGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCovariantVector.h"

namespace itk
{
/**
 *\class DifferenceOfGaussiansGradientImageFilter
 * \brief Performs difference-of-gaussians gradient detection
 *
 * \ingroup ImageEnhancement
 * \ingroup GradientFilters
 *
 * \ingroup ITKImageGradient
 */
template <typename TInputImage, typename TDataType>
class ITK_TEMPLATE_EXPORT DifferenceOfGaussiansGradientImageFilter
  : public ImageToImageFilter<
      TInputImage,
      Image<CovariantVector<TDataType, TInputImage::ImageDimension>, TInputImage::ImageDimension>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DifferenceOfGaussiansGradientImageFilter);

  /** Number of dimensions. */
  static constexpr unsigned int NDimensions = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = DifferenceOfGaussiansGradientImageFilter;

  /** Output image type alias. The output image is always an n-dimensional
   * image of n-dimensional vectors of doubles. */
  using TOutputImage = Image<CovariantVector<TDataType, Self::NDimensions>, Self::NDimensions>;

  /** Standard class type aliases. */
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DifferenceOfGaussiansGradientImageFilter, ImageToImageFilter);

  /** Image size type alias. */
  using SizeType = Size<Self::NDimensions>;

  /** Image index type alias. */
  using IndexType = typename TInputImage::IndexType;

  /** Image pixel value type alias. */
  using PixelType = typename TInputImage::PixelType;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TInputImage::RegionType;

  /** Set/Get the member variables. */
  itkGetConstMacro(Width, unsigned int);
  itkSetMacro(Width, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(DataTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<TDataType>));
  // End concept checking
#endif

protected:
  DifferenceOfGaussiansGradientImageFilter();
  ~DifferenceOfGaussiansGradientImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method for evaluating the implicit function over the image. */
  void
  GenerateData() override;

private:
  unsigned int m_Width;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDifferenceOfGaussiansGradientImageFilter.hxx"
#endif

#endif
