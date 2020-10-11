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
#ifndef itkBSplineBaseTransform_h
#define itkBSplineBaseTransform_h

#include <iostream>
#include "itkTransform.h"
#include "itkImage.h"
#include "itkBSplineInterpolationWeightFunction.h"

namespace itk
{
/** \class BSplineBaseTransform
 * \brief A base class with common elements of BSplineTransform and BSplineDeformableTransform
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double, unsigned int NDimensions = 3, unsigned int VSplineOrder = 3>
class ITK_TEMPLATE_EXPORT BSplineBaseTransform : public Transform<TParametersValueType, NDimensions, NDimensions>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineBaseTransform);

  /** Standard class type aliases. */
  using Self = BSplineBaseTransform;
  using Superclass = Transform<TParametersValueType, NDimensions, NDimensions>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineBaseTransform, Transform);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;

  /** The BSpline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** implement type-specific clone method*/
  itkCloneMacro(Self);

  /** Standard scalar type for this class. */
  using ScalarType = typename Superclass::ScalarType;

  /** Standard parameters container. */
  using FixedParametersType = typename Superclass::FixedParametersType;
  using ParametersType = typename Superclass::ParametersType;

  /** Standard Jacobian container. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** Transform category type. */
  using TransformCategoryEnum = typename Superclass::TransformCategoryEnum;

  /** The number of parameters defining this transform. */
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** Standard vector type for this class. */
  using InputVectorType = Vector<TParametersValueType, Self::SpaceDimension>;
  using OutputVectorType = Vector<TParametersValueType, Self::SpaceDimension>;

  /** Standard covariant vector type for this class. */
  using InputCovariantVectorType = CovariantVector<TParametersValueType, Self::SpaceDimension>;
  using OutputCovariantVectorType = CovariantVector<TParametersValueType, Self::SpaceDimension>;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = vnl_vector_fixed<TParametersValueType, SpaceDimension>;
  using OutputVnlVectorType = vnl_vector_fixed<TParametersValueType, SpaceDimension>;

  /** Standard coordinate point type for this class. */
  using InputPointType = Point<TParametersValueType, Self::SpaceDimension>;
  using OutputPointType = Point<TParametersValueType, Self::SpaceDimension>;

  /** This method sets the parameters of the transform.
   * For a BSpline deformation transform, the parameters are the BSpline
   * coefficients on a sparse grid.
   *
   * The parameters are N number of N-D grid of coefficients. Each N-D grid
   * is represented as a flat array of scalars (in the same configuration as
   * an itk::Image). The N arrays are then concatenated to form one parameter
   * array.
   *
   * For efficiency, this transform does not make a copy of the parameters.
   * It only keeps a pointer to the input parameters. It assumes that the memory
   * is managed by the caller. Use SetParametersByValue to force the transform
   * to call copy the parameters.
   *
   * This method wraps each grid as itk::Image's using the user specified
   * fixed parameters.
   * NOTE: The transform domain must be set first.
   *
   */
  void
  SetParameters(const ParametersType & parameters) override;

  /** This method sets the fixed parameters of the transform.
   * For a BSpline deformation transform, the fixed parameters are the
   * following: grid size, grid origin, grid spacing, and grid direction.
   * However, all of these are set via the much more intuitive
   * SetTransformDomainXXX() functions
   *
   * The fixed parameters are the three times the size of the templated
   * dimensions.  This function has the effect of make the following non-
   * existing functional calls:
   *   transform->SetGridSpacing( spacing );
   *   transform->SetGridOrigin( origin );
   *   transform->SetGridDirection( direction );
   *   transform->SetGridRegion( bsplineRegion );
   *
   * With recent updates to this transform, however, all these parameters
   * are set indirectly by setting the transform domain parameters unless
   * the user sets them with SetFixedParameters().
   *
   * This function was added to allow the transform to work with the
   * itkTransformReader/Writer I/O filters.
   *
   */
  void
  SetFixedParameters(const FixedParametersType & parameters) override = 0;

  /** This method sets the parameters of the transform.
   * For a BSpline deformation transform, the parameters are the BSpline
   * coefficients on a sparse grid.
   *
   * The parameters are N number of N-D grid of coefficients. Each N-D grid
   * is represented as a flat array of doubles
   * (in the same configuration as an itk::Image).
   * The N arrays are then concatenated to form one parameter array.
   *
   * This methods makes a copy of the parameters while for
   * efficiency the SetParameters method does not.
   *
   * This method wraps each grid as itk::Image's using the user specified
   * fixed parameters.
   * NOTE: The fixed parameters must be set first.
   */
  void
  SetParametersByValue(const ParametersType & parameters) override;

  /** This method can ONLY be invoked AFTER calling SetParameters().
   *  This restriction is due to the fact that the BSplineBaseTransform
   *  does not copy the array of parameters internally, instead it keeps a
   *  pointer to the user-provided array of parameters. This method is also
   *  in violation of the const-correctness of the parameters since the
   *  parameter array has been passed to the transform on a 'const' basis but
   *  the values get modified when the user invokes SetIdentity().
   */
  void
  SetIdentity();

  /** Get the Transformation Parameters. */
  const ParametersType &
  GetParameters() const override;

  /** Get the Transformation Fixed Parameters. */
  const FixedParametersType &
  GetFixedParameters() const override;

  /** Parameters as SpaceDimension number of images. */
  using ParametersValueType = typename ParametersType::ValueType;
  using ImageType = Image<ParametersValueType, Self::SpaceDimension>;
  using ImagePointer = typename ImageType::Pointer;
  using CoefficientImageArray = FixedArray<ImagePointer, NDimensions>;

  /** Set the array of coefficient images.
   *
   * This is an alternative API for setting the BSpline coefficients
   * as an array of SpaceDimension images. The fixed parameters are
   * taken from the first image. It is assumed that
   * the buffered region of all the subsequent images are the same
   * as the first image. Note that no error checking is done.
   *
   * Warning: use either the SetParameters() or SetCoefficientImages()
   * API. Mixing the two modes may results in unexpected results.
   */
  virtual void
  SetCoefficientImages(const CoefficientImageArray & images) = 0;

  /** Get the array of coefficient images. */
  const CoefficientImageArray
  GetCoefficientImages() const
  {
    return this->m_CoefficientImages;
  }

  using DerivativeType = typename Superclass::DerivativeType;

  /** Update the transform's parameters by the adding values in \c update
   * to current parameter values.
   * We assume \c update is of the same length as Parameters. Throw
   * exception otherwise.
   * \c factor is a scalar multiplier for each value in update.
   * SetParameters is called at the end of this method, to allow transforms
   * to perform any required operations on the update parameters, typically
   * a conversion to member variables for use in TransformPoint.
   * Derived classes should override to provide specialized behavior.
   */
  void
  UpdateTransformParameters(const DerivativeType & update, TParametersValueType factor = 1.0) override;

  /** Typedefs for specifying the extent of the grid. */
  using RegionType = ImageRegion<Self::SpaceDimension>;

  using IndexType = typename RegionType::IndexType;
  using SizeType = typename RegionType::SizeType;
  using SpacingType = typename ImageType::SpacingType;
  using DirectionType = typename ImageType::DirectionType;
  using OriginType = typename ImageType::PointType;

  /** Transform points by a BSpline deformable transformation. */
  OutputPointType
  TransformPoint(const InputPointType & point) const override;

  /** Interpolation weights function type. */
  using WeightsFunctionType = BSplineInterpolationWeightFunction<ScalarType, Self::SpaceDimension, Self::SplineOrder>;

  using WeightsType = typename WeightsFunctionType::WeightsType;
  using ContinuousIndexType = typename WeightsFunctionType::ContinuousIndexType;

  /** Parameter index array type. */
  using ParameterIndexArrayType = Array<unsigned long>;

  /**
   * Transform points by a BSpline deformable transformation.
   * On return, weights contains the interpolation weights used to compute the
   * deformation and indices of the x (zeroth) dimension coefficient parameters
   * in the support region used to compute the deformation.
   * Parameter indices for the i-th dimension can be obtained by adding
   * ( i * this->GetNumberOfParametersPerDimension() ) to the indices array.
   */
  virtual void
  TransformPoint(const InputPointType &    inputPoint,
                 OutputPointType &         outputPoint,
                 WeightsType &             weights,
                 ParameterIndexArrayType & indices,
                 bool &                    inside) const = 0;

  /** Get number of weights. */
  unsigned long
  GetNumberOfWeights() const
  {
    return m_WeightsFunction->GetNumberOfWeights();
  }

  /** Method to transform a vector -
   *  not applicable for this type of transform. */
  using Superclass::TransformVector;
  OutputVectorType
  TransformVector(const InputVectorType &) const override
  {
    itkExceptionMacro(<< "Method not applicable for deformable transform.");
  }

  /** Method to transform a vnl_vector -
   *  not applicable for this type of transform */
  OutputVnlVectorType
  TransformVector(const InputVnlVectorType &) const override
  {
    itkExceptionMacro(<< "Method not applicable for deformable transform. ");
  }

  /** Method to transform a CovariantVector -
   *  not applicable for this type of transform */
  using Superclass::TransformCovariantVector;
  OutputCovariantVectorType
  TransformCovariantVector(const InputCovariantVectorType &) const override
  {
    itkExceptionMacro(<< "Method not applicable for deformable transform. ");
  }

  /** Get Jacobian at a point. A very specialized function just for BSplines */
  void
  ComputeJacobianFromBSplineWeightsWithRespectToPosition(const InputPointType &,
                                                         WeightsType &,
                                                         ParameterIndexArrayType &) const;

  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType &) const override = 0;

  void
  ComputeJacobianWithRespectToPosition(const InputPointType &, JacobianPositionType &) const override
  {
    itkExceptionMacro(<< "ComputeJacobianWithRespectToPosition not yet implemented "
                         "for "
                      << this->GetNameOfClass());
  }
  using Superclass::ComputeJacobianWithRespectToPosition;

  /** Return the number of parameters that completely define the Transfom */
  NumberOfParametersType
  GetNumberOfParameters() const override = 0;

  /** Return the number of parameters per dimension */
  virtual NumberOfParametersType
  GetNumberOfParametersPerDimension() const = 0;

  TransformCategoryEnum
  GetTransformCategory() const override
  {
    return Self::TransformCategoryEnum::BSpline;
  }

  unsigned int
  GetNumberOfAffectedWeights() const;

  using PhysicalDimensionsType = typename ImageType::SpacingType;
  using PixelType = typename ImageType::PixelType;

  using MeshSizeType = SizeType;

  /** Return the number of local parameters */
  NumberOfParametersType
  GetNumberOfLocalParameters() const override
  {
    return this->GetNumberOfParameters();
  }

protected:
  /** Print contents of an BSplineBaseTransform. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  BSplineBaseTransform();
  ~BSplineBaseTransform() override = default;

  /** Get/Set to allow subclasses to access and manipulate the weights function. */
  itkSetObjectMacro(WeightsFunction, WeightsFunctionType);
  itkGetModifiableObjectMacro(WeightsFunction, WeightsFunctionType);

  /** Wrap flat array into images of coefficients. */
  void
  WrapAsImages();

protected:
  /** Construct control point grid from transform domain information */
  void
  SetFixedParametersFromTransformDomainInformation() const;

  /** Construct control point grid size from transform domain information */
  virtual void
  SetFixedParametersGridSizeFromTransformDomainInformation() const = 0;

  /** Construct control point grid origin from transform domain information */
  virtual void
  SetFixedParametersGridOriginFromTransformDomainInformation() const = 0;

  /** Construct control point grid spacing from transform domain information */
  virtual void
  SetFixedParametersGridSpacingFromTransformDomainInformation() const = 0;

  /** Construct control point grid direction from transform domain information */
  virtual void
  SetFixedParametersGridDirectionFromTransformDomainInformation() const = 0;

  /** Construct control point grid size from transform domain information */
  virtual void
  SetCoefficientImageInformationFromFixedParameters() = 0;

  /** Check if a continuous index is inside the valid region. */
  virtual bool
  InsideValidRegion(ContinuousIndexType &) const = 0;

  // NOTE:  There is a natural duality between the
  //       two representations of of the coefficients
  //       whereby the m_InternalParametersBuffer is
  //       needed to fit into the optimization framework
  //       and the m_CoefficientImages is needed for
  //       the Jacobian computations.  This implementation
  //       is an attempt to remove as much redundancy as possible
  //       and share as much information between the two
  //       instances as possible.
  //
  /** Array of images representing the B-spline coefficients
   *  in each dimension wrapped from the flat parameters in
   *  m_InternalParametersBuffer
   */
  CoefficientImageArray m_CoefficientImages;

  /** Internal parameters buffer. */
  ParametersType m_InternalParametersBuffer;

  /** Pointer to function used to compute Bspline interpolation weights. */
  typename WeightsFunctionType::Pointer m_WeightsFunction;

private:
  static CoefficientImageArray
  ArrayOfImagePointerGeneratorHelper();
}; // class BSplineBaseTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineBaseTransform.hxx"
#endif

#endif /* itkBSplineBaseTransform_h */
