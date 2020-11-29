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
#ifndef itkNormalizeToConstantImageFilter_h
#define itkNormalizeToConstantImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{

/** \class NormalizeToConstantImageFilter
 *
 * \brief Scales image pixel intensities to make the sum of all pixels
 * equal a user-defined constant.
 *
 * The default value of the constant is 1. It can be changed with
 * SetConstant().
 *
 * This transform is especially useful for normalizing a convolution
 * kernel.
 *
 * This code was contributed in the Insight Journal paper: "FFT based
 * convolution" by Lehmann G.
 * http://insight-journal.org/browse/publication/717
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa NormalizeImageFilter
 * \sa StatisticsImageFilter
 * \sa DivideImageFilter
 *
 * \ingroup MathematicalImageFilters
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/ScalePixelSumToConstant,Scale All Pixel's Sum To Constant}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NormalizeToConstantImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalizeToConstantImageFilter);

  /** Standard class type aliases. */
  using Self = NormalizeToConstantImageFilter;

  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using RegionType = typename InputImageType::RegionType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;

  using RealType = typename NumericTraits<InputImagePixelType>::RealType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(NormalizeToConstantImageFilter, ImageToImageFilter);

  /** Set/get the normalization constant. */
  itkSetMacro(Constant, RealType);
  itkGetConstMacro(Constant, RealType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  NormalizeToConstantImageFilter();
  ~NormalizeToConstantImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateData() override;

private:
  RealType m_Constant;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizeToConstantImageFilter.hxx"
#endif

#endif
