/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkProxTVImageFilter_h
#define itkProxTVImageFilter_h

#include "itkImageToImageFilter.h"
#include "TVopt.h"

namespace itk
{

/** \class ProxTVImageFilter
 *
 * \brief Filters a image by iterating over its pixels.
 *
 * Filters a image by iterating over its pixels in a multi-threaded way
 * and {to be completed by the developer}.
 *
 * \ingroup TotalVariation
 *
 */
template <typename TInputImage, typename TOutputImage>
class ProxTVImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ProxTVImageFilter);

  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputImageType = TInputImage;
  using OutputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputPixelType = typename OutputImageType::PixelType;

  using ArrayType = itk::FixedArray<double, ImageDimension>;

  /** Standard class typedefs. */
  using Self = ProxTVImageFilter<InputImageType, OutputImageType>;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information. */
  itkTypeMacro(ProxTVImageFilter, ImageToImageFilter);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Set/Get the MaximumNumberOfIterations */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetConstMacro(MaximumNumberOfIterations, unsigned int);

  /** Set/Get the Weights */
  itkSetMacro(Weights, ArrayType);
  itkGetConstMacro(Weights, ArrayType);

  /** Set/Get the Norms */
  itkSetMacro(Norms, ArrayType);
  itkGetConstMacro(Norms, ArrayType);

protected:
  ProxTVImageFilter() = default;
  ~ProxTVImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using OutputRegionType = typename OutputImageType::RegionType;

  virtual void
  GenerateData() override;

private:
  unsigned int m_MaximumNumberOfIterations = 10;
  ArrayType    m_Weights = 1.0;
  ArrayType    m_Norms = 1.0;


#ifdef ITK_USE_CONCEPT_CHECKING
  /** ImageDimension enumeration   */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  itkConceptMacro(SameDimensionCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
#endif
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkProxTVImageFilter.hxx"
#endif

#endif // itkProxTVImageFilter
