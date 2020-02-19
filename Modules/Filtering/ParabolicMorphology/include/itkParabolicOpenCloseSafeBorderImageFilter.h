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
#ifndef itkParabolicOpenCloseSafeBorderImageFilter_h
#define itkParabolicOpenCloseSafeBorderImageFilter_h

#include "itkParabolicOpenCloseImageFilter.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkStatisticsImageFilter.h"

/* this class implements padding and cropping, so we don't just
 * inherit from the OpenCloseImageFitler */

namespace itk
{
template <typename TInputImage, bool doOpen, typename TOutputImage = TInputImage>
class ITK_EXPORT ParabolicOpenCloseSafeBorderImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParabolicOpenCloseSafeBorderImageFilter);

  /** Standard class type alias. */
  using Self = ParabolicOpenCloseSafeBorderImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ParabolicOpenCloseSafeBorderImageFilter, ImageToImageFilter);

  /** Pixel Type of the input image */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputPixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<InputPixelType>::RealType;
  using ScalarRealType = typename NumericTraits<InputPixelType>::ScalarRealType;
  using OutputPixelType = typename TOutputImage::PixelType;

  /** Smart pointer type alias support.  */
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** a type to represent the "kernel radius" */
  using RadiusType = typename itk::FixedArray<ScalarRealType, TInputImage::ImageDimension>;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;
  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  // set all of the scales the same
  void
  SetScale(ScalarRealType scale)
  {
    RadiusType s = this->GetScale();

    this->m_MorphFilt->SetScale(scale);
    if (s != this->GetScale())
    {
      this->Modified();
    }
  }

  // different scale for each direction
  void
  SetScale(RadiusType scale)
  {
    if (scale != this->GetScale())
    {
      this->m_MorphFilt->SetScale(scale);
      this->Modified();
    }
  }

  //
  const RadiusType &
  GetScale() const
  {
    return (this->m_MorphFilt->GetScale());
  }

  void
  SetUseImageSpacing(bool B)
  {
    if (B != this->GetUseImageSpacing())
    {
      this->m_MorphFilt->SetUseImageSpacing(B);
      this->Modified();
    }
  }

  bool
  GetUseImageSpacing() const
  {
    return (this->m_MorphFilt->GetUseImageSpacing());
  }

  itkBooleanMacro(UseImageSpacing);

  itkSetMacro(SafeBorder, bool);
  itkGetConstReferenceMacro(SafeBorder, bool);
  itkBooleanMacro(SafeBorder);
  // should add the Get methods

  enum ParabolicAlgorithm
  {
    NOCHOICE = 0,     // decices based on scale - experimental
    CONTACTPOINT = 1, // sometimes faster at low scale
    INTERSECTION = 2  // default
  };
  /**
   * Set/Get the method used. Choices are contact point or
   * intersection. Intersection is the default. Contact point can be
   * faster at small scales.
   */

  itkSetMacro(ParabolicAlgorithm, int);
  itkGetConstReferenceMacro(ParabolicAlgorithm, int);

  /** ParabolicOpenCloseImageFilter must forward the Modified() call to its
    internal filters */
  void
  Modified() const override;

protected:
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  using MorphFilterType = ParabolicOpenCloseImageFilter<TInputImage, doOpen, TOutputImage>;
  using PadFilterType = ConstantPadImageFilter<TInputImage, TInputImage>;
  using CropFilterType = CropImageFilter<TOutputImage, TOutputImage>;
  using StatsFilterType = StatisticsImageFilter<InputImageType>;

  ParabolicOpenCloseSafeBorderImageFilter()
  {
    m_MorphFilt = MorphFilterType::New();
    m_PadFilt = PadFilterType::New();
    m_CropFilt = CropFilterType::New();
    m_StatsFilt = StatsFilterType::New();
    m_SafeBorder = true;
    m_ParabolicAlgorithm = INTERSECTION;
  }

  ~ParabolicOpenCloseSafeBorderImageFilter() override = default;
  int m_ParabolicAlgorithm;

private:
  typename MorphFilterType::Pointer m_MorphFilt;
  typename PadFilterType::Pointer   m_PadFilt;
  typename CropFilterType::Pointer  m_CropFilt;
  typename StatsFilterType::Pointer m_StatsFilt;

  bool m_SafeBorder;
  bool m_UseContactPoint;
  bool m_UseIntersection;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkParabolicOpenCloseSafeBorderImageFilter.hxx"
#endif

#endif
