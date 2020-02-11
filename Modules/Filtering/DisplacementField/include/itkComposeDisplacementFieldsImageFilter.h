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
#ifndef itkComposeDisplacementFieldsImageFilter_h
#define itkComposeDisplacementFieldsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"

namespace itk
{

/**
 * \class ComposeDisplacementFieldsImageFilter
 *
 * \brief Compose two displacement fields.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 *
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ComposeDisplacementFieldsImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ComposeDisplacementFieldsImageFilter);

  using Self = ComposeDisplacementFieldsImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputFieldType = TInputImage;
  using OutputFieldType = TOutputImage;

  /** Image type alias support */
  using PixelType = typename OutputFieldType::PixelType;
  using VectorType = typename OutputFieldType::PixelType;
  using RegionType = typename OutputFieldType::RegionType;
  using IndexType = typename OutputFieldType::IndexType;

  using PointType = typename OutputFieldType::PointType;
  using SpacingType = typename OutputFieldType::SpacingType;
  using OriginType = typename OutputFieldType::PointType;
  using SizeType = typename OutputFieldType::SizeType;
  using DirectionType = typename OutputFieldType::DirectionType;

  /** Other type alias */
  using RealType = typename VectorType::ComponentType;
  using InterpolatorType = VectorInterpolateImageFunction<InputFieldType, RealType>;

  /** Get the interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the deformation field */
  void
  SetDisplacementField(const InputFieldType * field)
  {
    itkDebugMacro("setting displacement field to " << field);
    if (field != this->GetInput(0))
    {
      this->SetInput(0, field);
      this->Modified();
      if (!this->m_Interpolator.IsNull())
      {
        this->m_Interpolator->SetInputImage(field);
      }
    }
  }

  /**
   * Get the deformation field.
   */
  const InputFieldType *
  GetDisplacementField() const
  {
    return this->GetInput(0);
  }

  /** Set the warping field */
  void
  SetWarpingField(const InputFieldType * field)
  {
    itkDebugMacro("setting warping field to " << field);
    if (field != this->GetInput(1))
    {
      this->SetInput(1, field);
    }
  }

  /**
   * Get the warping field.
   */
  const InputFieldType *
  GetWarpingField() const
  {
    return this->GetInput(1);
  }

  /* Set the interpolator. */
  virtual void
  SetInterpolator(InterpolatorType * interpolator);

protected:
  /** Constructor */
  ComposeDisplacementFieldsImageFilter();

  /** Deconstructor */
  ~ComposeDisplacementFieldsImageFilter() override = default;

  /** Standard print self function **/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** preprocessing function */
  void
  BeforeThreadedGenerateData() override;

  /** Multithreaded function which generates the output field. */
  void
  DynamicThreadedGenerateData(const RegionType &) override;


private:
  /** The interpolator. */
  typename InterpolatorType::Pointer m_Interpolator;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkComposeDisplacementFieldsImageFilter.hxx"
#endif

#endif
