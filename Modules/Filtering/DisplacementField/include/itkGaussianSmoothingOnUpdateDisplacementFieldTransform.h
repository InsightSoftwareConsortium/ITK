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
#ifndef itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
#define itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h

#include "itkDisplacementFieldTransform.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

/** \class GaussianSmoothingOnUpdateDisplacementFieldTransform
 * \brief Modifies the UpdateTransformParameters method
 * to peform a Gaussian smoothing of the
 * displacement field after adding the update array.
 *
 * This class is the same as \c DisplacementFieldTransform, except
 * for the changes to UpdateTransformParameters. The method smooths
 * the result of the addition of the update array and the displacement
 * field, using a \c GaussianOperator filter.
 *
 * To free the memory allocated and cached in \c GaussianSmoothDisplacementField
 * on demand, see \c FreeGaussianSmoothingTempField.
 *
 *
 * \ingroup ITKDisplacementField
 */
template <typename TParametersValueType, unsigned int NDimensions>
class ITK_TEMPLATE_EXPORT GaussianSmoothingOnUpdateDisplacementFieldTransform
  : public DisplacementFieldTransform<TParametersValueType, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianSmoothingOnUpdateDisplacementFieldTransform);

  /** Standard class type aliases. */
  using Self = GaussianSmoothingOnUpdateDisplacementFieldTransform;
  using Superclass = DisplacementFieldTransform<TParametersValueType, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianSmoothingOnUpdateDisplacementFieldTransform, DisplacementFieldTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Types from superclass */
  using ScalarType = typename Superclass::ScalarType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename DerivativeType::ValueType;
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using DisplacementVectorType = typename DisplacementFieldType::PixelType;

  using TransformPointer = typename Transform<TParametersValueType, NDimensions, NDimensions>::Pointer;

  /**
   * Get/Set the Gaussian smoothing standard deviation for the update field.
   * Default = 1.75.
   */
  itkSetMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheUpdateField, ScalarType);

  /**
   * Get/Set the Gaussian smoothing standard deviation for the total field.
   * Default = 0.5.
   */
  itkSetMacro(GaussianSmoothingVarianceForTheTotalField, ScalarType);
  itkGetConstReferenceMacro(GaussianSmoothingVarianceForTheTotalField, ScalarType);

  /** Update the transform's parameters by the values in \c update.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * \c GaussianSmoothDisplacementField is called after the update is
   * added to the field.
   * See base class for more details.
   */
  void
  UpdateTransformParameters(const DerivativeType & update, ScalarType factor = 1.0) override;

  /** Smooth the displacement field in-place.
   * Uses m_GaussSmoothSigma to change the variance for the GaussianOperator.
   * \warning Not thread safe. Does its own threading.
   */
  virtual DisplacementFieldPointer
  GaussianSmoothDisplacementField(DisplacementFieldType *, ScalarType);

protected:
  GaussianSmoothingOnUpdateDisplacementFieldTransform();
  ~GaussianSmoothingOnUpdateDisplacementFieldTransform() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Clone the current transform */
  typename LightObject::Pointer
  InternalClone() const override;

  /** Used in GaussianSmoothDisplacementField as variance for the
   * GaussianOperator */
  ScalarType m_GaussianSmoothingVarianceForTheUpdateField;
  ScalarType m_GaussianSmoothingVarianceForTheTotalField;

  /** Type of Gaussian Operator used during smoothing. Define here
   * so we can use a member var during the operation. */
  using GaussianSmoothingOperatorType = GaussianOperator<ScalarType, Superclass::Dimension>;
  using GaussianSmoothingSmootherType =
    VectorNeighborhoodOperatorImageFilter<DisplacementFieldType, DisplacementFieldType>;
  GaussianSmoothingOperatorType m_GaussianSmoothingOperator;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.hxx"
#endif

#endif // itkGaussianSmoothingOnUpdateDisplacementFieldTransform_h
