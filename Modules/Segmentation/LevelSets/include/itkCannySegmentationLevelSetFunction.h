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
#ifndef itkCannySegmentationLevelSetFunction_h
#define itkCannySegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"
#include "itkCastImageFilter.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"

namespace itk
{
/** \class CannySegmentationLevelSetFunction
 * \brief A refinement of the standard level-set function which computes a
 * speed term and advection term based on pseudo-Canny edges.  See
 * CannySegmentationLevelSetImageFilter for complete information.
 * \ingroup ITKLevelSets
 */
template <typename TImageType, typename TFeatureImageType = TImageType>
class ITK_TEMPLATE_EXPORT CannySegmentationLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CannySegmentationLevelSetFunction);

  /** Standard class type aliases. */
  using Self = CannySegmentationLevelSetFunction;
  using Superclass = SegmentationLevelSetFunction<TImageType, TFeatureImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FeatureImageType = TFeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CannySegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  using ImageType = typename Superclass::ImageType;
  using ScalarValueType = typename Superclass::ScalarValueType;
  using VectorImageType = typename Superclass::VectorImageType;
  using FeatureScalarType = typename Superclass::FeatureScalarType;
  using RadiusType = typename Superclass::RadiusType;

  /** Extract some parameters from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** */
  void
  SetThreshold(ScalarValueType v)
  {
    m_Threshold = v;
  }
  ScalarValueType
  GetThreshold() const
  {
    return m_Threshold;
  }

  /** */
  void
  SetVariance(double v)
  {
    m_Variance = v;
  }
  double
  GetVariance() const
  {
    return m_Variance;
  }

  /** Compute the Speed Image. The Speed Image is the distance to the
      canny edges. */
  void
  CalculateSpeedImage() override;

  /** Compute the advection image. The Advection Image is the gradient
      image attenuated with the distance to the canny edges. */
  void
  CalculateAdvectionImage() override;

  /** Compute the distance image. This is the distance to the canny
   * edges. */
  virtual void
  CalculateDistanceImage();

  void
  Initialize(const RadiusType & r) override
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(-1.0 * NumericTraits<ScalarValueType>::OneValue());
    this->SetPropagationWeight(-1.0 * NumericTraits<ScalarValueType>::OneValue());
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::OneValue());
  }

  ImageType *
  GetCannyImage()
  {
    return m_Canny->GetOutput();
  }

protected:
  CannySegmentationLevelSetFunction()
  {
    m_Variance = 0.0;
    m_Threshold = NumericTraits<ScalarValueType>::ZeroValue();
    m_Caster = CastImageFilter<FeatureImageType, ImageType>::New();
    m_Canny = CannyEdgeDetectionImageFilter<ImageType, ImageType>::New();
    m_Distance = DanielssonDistanceMapImageFilter<ImageType, ImageType>::New();
  }

  ~CannySegmentationLevelSetFunction() override = default;

private:
  ScalarValueType m_Variance;
  double          m_Threshold;

  typename CannyEdgeDetectionImageFilter<ImageType, ImageType>::Pointer m_Canny;

  typename DanielssonDistanceMapImageFilter<ImageType, ImageType>::Pointer m_Distance;

  typename CastImageFilter<FeatureImageType, ImageType>::Pointer m_Caster;

  /** If FeatureImageType != ImageType,
   *  use the CastImageFilter to match them.
   */
  template <typename DummyImagePointerType>
  void
  AssignCannyInput(typename FeatureImageType::Pointer & feature, DummyImagePointerType &)
  {
    m_Caster->SetInput(feature);
    m_Canny->SetInput(m_Caster->GetOutput());
  }
  /** If FeatureImageType == ImageType,
   *  assign directly to the Canny filter
   */
  void
  AssignCannyInput(typename FeatureImageType::Pointer & feature, typename FeatureImageType::Pointer &)
  {
    m_Canny->SetInput(feature);
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCannySegmentationLevelSetFunction.hxx"
#endif

#endif
