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
#ifndef itkLaplacianSegmentationLevelSetFunction_h
#define itkLaplacianSegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"

namespace itk
{
/** \class LaplacianSegmentationLevelSetFunction
 *
 * \brief This function is used in LaplacianSegmentationImageFilter to
 * segment structures in an image based Laplacian edges.
 *
 * Assumes a strictly POSITIVE feature image
 * \ingroup ITKLevelSets
 */
template <typename TImageType, typename TFeatureImageType = TImageType>
class ITK_TEMPLATE_EXPORT LaplacianSegmentationLevelSetFunction
  : public SegmentationLevelSetFunction<TImageType, TFeatureImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LaplacianSegmentationLevelSetFunction);

  /** Standard class type aliases. */
  using Self = LaplacianSegmentationLevelSetFunction;
  using Superclass = SegmentationLevelSetFunction<TImageType, TFeatureImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using FeatureImageType = TFeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(LaplacianSegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  using ImageType = typename Superclass::ImageType;
  using ScalarValueType = typename Superclass::ScalarValueType;
  using FeatureScalarType = typename Superclass::FeatureScalarType;
  using RadiusType = typename Superclass::RadiusType;

  /** Extract some parameters from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  void
  CalculateSpeedImage() override;

  void
  Initialize(const RadiusType & r) override
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(NumericTraits<ScalarValueType>::ZeroValue());
    this->SetPropagationWeight(-1.0 * NumericTraits<ScalarValueType>::OneValue());
    this->SetCurvatureWeight(NumericTraits<ScalarValueType>::OneValue());
  }

  /**
   * The Laplacian level set does not use an advection term. We clamp
   * the value to ZERO here because a superclass may try to set it
   * otherwise. in fact, SegmentationLevelSetImageFilter tries to set
   * it when SetFeatureScaling is called.
   */
  void
  SetAdvectionWeight(const ScalarValueType value) override
  {
    if (Math::ExactlyEquals(value, NumericTraits<ScalarValueType>::ZeroValue()))
    {
      Superclass::SetAdvectionWeight(value);
    }
  }

protected:
  LaplacianSegmentationLevelSetFunction()
  {
    this->SetAdvectionWeight(0.0);
    this->SetPropagationWeight(1.0);
    this->SetCurvatureWeight(1.0);
  }

  ~LaplacianSegmentationLevelSetFunction() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLaplacianSegmentationLevelSetFunction.hxx"
#  include "itkMath.h"
#endif

#endif
