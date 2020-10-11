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
#ifndef itkInvertDisplacementFieldImageFilter_h
#define itkInvertDisplacementFieldImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkVectorInterpolateImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include <mutex>

namespace itk
{

/**
 * \class InvertDisplacementFieldImageFilter
 *
 * \brief Iteratively estimate the inverse field of a displacement field.
 *
 * \author Nick Tustison
 * \author Brian Avants
 *
 * \ingroup ITKDisplacementField
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT InvertDisplacementFieldImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InvertDisplacementFieldImageFilter);

  using Self = InvertDisplacementFieldImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputFieldType = TInputImage;
  using OutputFieldType = TOutputImage;

  using DisplacementFieldType = InputFieldType;
  using InverseDisplacementFieldType = OutputFieldType;

  /** Image type alias support */
  using PixelType = typename OutputFieldType::PixelType;
  using VectorType = typename OutputFieldType::PixelType;
  using RegionType = typename OutputFieldType::RegionType;
  using OutputImageRegionType = RegionType;
  using IndexType = typename OutputFieldType::IndexType;

  using PointType = typename OutputFieldType::PointType;
  using SpacingType = typename OutputFieldType::SpacingType;
  using OriginType = typename OutputFieldType::PointType;
  using SizeType = typename OutputFieldType::SizeType;
  using DirectionType = typename OutputFieldType::DirectionType;

  /** Other type alias */
  using RealType = typename VectorType::ComponentType;
  using RealImageType = Image<RealType, ImageDimension>;
  using InterpolatorType = VectorInterpolateImageFunction<InputFieldType, RealType>;
  using DefaultInterpolatorType = VectorLinearInterpolateImageFunction<InputFieldType, RealType>;

  /** Get the interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Set the deformation field */
  void
  SetDisplacementField(const InputFieldType * field)
  {
    itkDebugMacro("setting deformation field to " << field);
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

  /** Set/get the initial estimate for the inverse field (optional). */
  itkSetInputMacro(InverseFieldInitialEstimate, InverseDisplacementFieldType);
  itkGetInputMacro(InverseFieldInitialEstimate, InverseDisplacementFieldType);

  /* Set the interpolator. */
  virtual void
  SetInterpolator(InterpolatorType * interpolator);

  /* Set/Get the number of iterations */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetConstMacro(MaximumNumberOfIterations, unsigned int);

  /* Set/Get the mean stopping criterion */
  itkSetMacro(MeanErrorToleranceThreshold, RealType);
  itkGetConstMacro(MeanErrorToleranceThreshold, RealType);

  /* Set/Get the max stopping criterion */
  itkSetMacro(MaxErrorToleranceThreshold, RealType);
  itkGetConstMacro(MaxErrorToleranceThreshold, RealType);

  /* Get the max norm */
  itkGetConstMacro(MaxErrorNorm, RealType);

  /* Get the mean norm */
  itkGetConstMacro(MeanErrorNorm, RealType);

  /* Should we force the boundary to have zero displacement? */
  itkSetMacro(EnforceBoundaryCondition, bool);
  itkGetMacro(EnforceBoundaryCondition, bool);

protected:
  /** Constructor */
  InvertDisplacementFieldImageFilter();

  /** Deconstructor */
  ~InvertDisplacementFieldImageFilter() override = default;

  /** Standard print self function **/
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** preprocessing function */
  void
  GenerateData() override;

  /** Multithreaded function which generates the output field. */
  void
  DynamicThreadedGenerateData(const RegionType &) override;

private:
  /** The interpolator. */
  typename InterpolatorType::Pointer m_Interpolator;

  unsigned int m_MaximumNumberOfIterations{ 20 };

  RealType m_MaxErrorToleranceThreshold;
  RealType m_MeanErrorToleranceThreshold;

  // internal ivars necessary for multithreading basic operations

  typename DisplacementFieldType::Pointer m_ComposedField;
  typename RealImageType::Pointer         m_ScaledNormImage;

  RealType    m_MaxErrorNorm;
  RealType    m_MeanErrorNorm;
  RealType    m_Epsilon;
  SpacingType m_DisplacementFieldSpacing;
  bool        m_DoThreadedEstimateInverse{ false };
  bool        m_EnforceBoundaryCondition{ true };
  std::mutex  m_Mutex;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInvertDisplacementFieldImageFilter.hxx"
#endif

#endif
