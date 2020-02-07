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

#ifndef itkDiscreteLevelSetImage_h
#define itkDiscreteLevelSetImage_h

#include "itkLevelSetImage.h"

namespace itk
{
/**
 *  \class DiscreteLevelSetImage
 *  \brief Abstract class for a level-set function on one Image.
 *
 *  \tparam TOutput OutputType of the level-set function value
 *  \tparam VDimension Dimension of the underlying Image.
 *
 *  \ingroup ITKLevelSetsv4
 */
template <typename TOutput, unsigned int VDimension>
class ITK_TEMPLATE_EXPORT DiscreteLevelSetImage : public LevelSetImage<Index<VDimension>, VDimension, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(DiscreteLevelSetImage);

  using IndexType = Index<VDimension>;

  using Self = DiscreteLevelSetImage;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = LevelSetImage<IndexType, VDimension, TOutput>;

  /** Run-time type information */
  itkTypeMacro(DiscreteLevelSetImage, LevelSetImage);

  static constexpr unsigned int Dimension = Superclass::Dimension;

  using InputType = typename Superclass::InputType;
  using OutputType = typename Superclass::OutputType;
  using OutputRealType = typename Superclass::OutputRealType;
  using GradientType = typename Superclass::GradientType;
  using HessianType = typename Superclass::HessianType;
  using LevelSetDataType = typename Superclass::LevelSetDataType;

  /** Returns the gradient of the level set function at a given location inputIndex */
  OutputType
  Evaluate(const InputType & inputIndex) const override = 0;

  /** Returns the image gradient of the level set function at a given location inputIndex */
  GradientType
  EvaluateGradient(const InputType & inputIndex) const override;

  /** Returns the image hessian of the level set function at a given location inputIndex */
  HessianType
  EvaluateHessian(const InputType & inputIndex) const override;

  /** Returns the image Laplacian of the level set function at a given location inputIndex */
  OutputRealType
  EvaluateLaplacian(const InputType & inputIndex) const override;

  /** Returns the mean curvature of the level set function at a given location inputIndex */
  OutputRealType
  EvaluateMeanCurvature(const InputType & inputIndex) const override;

  virtual GradientType
  EvaluateForwardGradient(const InputType & inputIndex) const;

  virtual GradientType
  EvaluateBackwardGradient(const InputType & inputIndex) const;

  /** Returns the value of the level set function at a given location inputIndex */
  void
  Evaluate(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateGradient(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the Hessian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateHessian(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the Hessian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateMeanCurvature(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the Laplacian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateLaplacian(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateForwardGradient(const InputType & inputIndex, LevelSetDataType & data) const override;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  void
  EvaluateBackwardGradient(const InputType & inputIndex, LevelSetDataType & data) const override;

protected:
  DiscreteLevelSetImage() = default;

  ~DiscreteLevelSetImage() override = default;

  /** Initial the level set pointer */
  void
  Initialize() override;

  /** Copy level set information from data object */
  void
  CopyInformation(const DataObject * data) override;

  /** Graft data object as level set object */
  void
  Graft(const DataObject * data) override;

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDiscreteLevelSetImage.hxx"
#endif

#endif // itkDiscreteLevelSetImage_h
