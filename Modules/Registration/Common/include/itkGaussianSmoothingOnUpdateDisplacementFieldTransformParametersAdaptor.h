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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h

#include "itkDisplacementFieldTransformParametersAdaptor.h"

namespace itk
{
/** \class GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
 * \brief Helper class for multiresolution image registration
 *
 * \author Nick Tustison
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class ITK_TEMPLATE_EXPORT GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
  : public DisplacementFieldTransformParametersAdaptor<TTransform>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor;
  using Superclass = DisplacementFieldTransformParametersAdaptor<TTransform>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor,
               DisplacementFieldTransformParametersAdaptor);

  using TransformType = TTransform;
  using ScalarType = typename TransformType::ScalarType;

  /**
   * Get/Set the Gaussian smoothing standard deviation for the update field.
   */
  virtual void SetGaussianSmoothingVarianceForTheUpdateField(ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);

  /**
   * Get/Set the Gaussian smoothing standard deviation for the total field.
   */
  virtual void
  SetGaussianSmoothingVarianceForTheTotalField(const ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheTotalField, ScalarType);

  void
  AdaptTransformParameters() override;

protected:
  GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor();
  ~GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ScalarType m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType m_GaussianSmoothingVarianceForTheTotalField;

  ModifiedTimeType m_GaussianSmoothingVarianceForTheUpdateFieldSetTime{ 0 };
  ModifiedTimeType m_GaussianSmoothingVarianceForTheTotalFieldSetTime{ 0 };

}; // class GaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.hxx"
#endif

#endif /* itkGaussianSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor_h */
