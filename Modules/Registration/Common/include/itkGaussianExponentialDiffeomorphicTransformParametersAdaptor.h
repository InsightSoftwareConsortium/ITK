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
#ifndef itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_h
#define itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_h

#include "itkConstantVelocityFieldTransformParametersAdaptor.h"

namespace itk
{
/** \class GaussianExponentialDiffeomorphicTransformParametersAdaptor
 * \brief Helper class for multiresolution image registration
 *
 * \author Nick Tustison
 *
 * \ingroup ITKRegistrationCommon
 */
template <typename TTransform>
class ITK_TEMPLATE_EXPORT GaussianExponentialDiffeomorphicTransformParametersAdaptor
  : public ConstantVelocityFieldTransformParametersAdaptor<TTransform>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianExponentialDiffeomorphicTransformParametersAdaptor);

  /** Standard class type aliases. */
  using Self = GaussianExponentialDiffeomorphicTransformParametersAdaptor;
  using Superclass = ConstantVelocityFieldTransformParametersAdaptor<TTransform>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianExponentialDiffeomorphicTransformParametersAdaptor,
               ConstantVelocityFieldTransformParametersAdaptor);

  using TransformType = TTransform;
  using ScalarType = typename TransformType::ScalarType;

  /**
   * Get/Set the Gaussian smoothing standard deviation for the velocity field.
   */
  virtual void SetGaussianSmoothingVarianceForTheConstantVelocityField(ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheConstantVelocityField, ScalarType);

  /**
   * Get/Set the Gaussian smoothing standard deviation for the update field.
   */
  virtual void SetGaussianSmoothingVarianceForTheUpdateField(ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);

  void
  AdaptTransformParameters() override;

protected:
  GaussianExponentialDiffeomorphicTransformParametersAdaptor();
  ~GaussianExponentialDiffeomorphicTransformParametersAdaptor() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ScalarType       m_GaussianSmoothingVarianceForTheConstantVelocityField;
  ScalarType       m_GaussianSmoothingVarianceForTheUpdateField;
  ModifiedTimeType m_GaussianSmoothingVarianceForTheConstantVelocityFieldSetTime{ 0 };
  ModifiedTimeType m_GaussianSmoothingVarianceForTheUpdateFieldSetTime{ 0 };

}; // class GaussianExponentialDiffeomorphicTransformParametersAdaptor
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianExponentialDiffeomorphicTransformParametersAdaptor.hxx"
#endif

#endif /* itkGaussianExponentialDiffeomorphicTransformParametersAdaptor_h */
