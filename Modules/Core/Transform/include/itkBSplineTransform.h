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
#ifndef itkBSplineTransform_h
#define itkBSplineTransform_h

#include "itkBSplineBaseTransform.h"

namespace itk
{
/** \class BSplineTransform
 * \brief Deformable transform using a BSpline representation
 *
 * This class encapsulates a deformable transform of points from one
 * N-dimensional space to another N-dimensional space.
 * The deformation field is modelled using B-splines.
 * A deformation is defined on a sparse regular grid of control points
 * \f$ \vec{\lambda}_j \f$ and is varied by defining a deformation
 * \f$ \vec{g}(\vec{\lambda}_j) \f$ of each control point.
 * The deformation \f$ D(\vec{x}) \f$ at any point \f$ \vec{x} \f$
 * is obtained by using a B-spline interpolation kernel.
 *
 * The deformation field grid is defined by a user specified transform
 * domain (origin, physical dimensions, direction) and B-spline mesh size
 * where the mesh size is the number of polynomial patches comprising the
 * finite domain of support.  The relationship between the mesh size (
 * number of polynomial pieces) and the number of control points in any
 * given dimension is
 *
 * mesh size = number of control points - spline order
 *
 * Each grid/control point has associated with it
 * N deformation coefficients \f$ \vec{\delta}_j \f$, representing the N
 * directional components of the deformation. Deformation outside the grid
 * plus support region for the BSpline interpolation is assumed to be zero.
 *
 * The parameters for this transform is N x N-D grid of spline coefficients.
 * The user specifies the parameters as one flat array: each N-D grid
 * is represented by an array in the same way an N-D image is represented
 * in the buffer; the N arrays are then concatenated together to form
 * a single array.
 *
 * The following illustrates the typical usage of this class:
 * \code
   using TransformType = BSplineTransform<double,2,3>;
   TransformType::Pointer transform = TransformType::New();

   transform->SetTransformDomainOrigin( origin );
   transform->SetTransformDomainPhysicalDimensions( physicalDimensions );
   transform->SetTransformDomainDirection( direction );
   transform->SetTransformDomainMeshSize( meshSize );

   // NB: The region must be set first before setting the parameters

   TransformType::ParametersType parameters( transform->GetNumberOfParameters() );

   // Fill the parameters with values

   transform->SetParameters( parameters )

   outputPoint = transform->TransformPoint( inputPoint );

   \endcode
 *
 * An alternative way to set the B-spline coefficients is via array of
 * images. The fixed parameters of the transform are taken
 * directly from the first image. It is assumed that the subsequent images
 * are the same buffered region. The following illustrates the API:
 * \code

   TransformType::ImageConstPointer images[2];

   // Fill the images up with values

   transform->SetCoefficientImages( images );
   outputPoint = transform->TransformPoint( inputPoint );

   \endcode
 *
 * \warning Use either the SetParameters() or SetCoefficientImages()
 * API. Mixing the two modes may results in unexpected results.
 *
 * The class is templated coordinate representation type (float or double),
 * the space dimension and the spline order.
 *
 * \ingroup ITKTransform
 */
template <typename TParametersValueType = double, unsigned int NDimensions = 3, unsigned int VSplineOrder = 3>
class ITK_TEMPLATE_EXPORT BSplineTransform
  : public BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineTransform);

  /** Standard class type aliases. */
  using Self = BSplineTransform;
  using Superclass = BSplineBaseTransform<TParametersValueType, NDimensions, VSplineOrder>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineTransform, BSplineBaseTransform);

  /** Dimension of the domain space. */
  static constexpr unsigned int SpaceDimension = NDimensions;

  /** The BSpline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** Standard scalar type for this class. */
  using ScalarType = typename Superclass::ScalarType;

  /** Standard parameters container. */
  using ParametersType = typename Superclass::ParametersType;
  using ParametersValueType = typename Superclass::ParametersValueType;
  using FixedParametersType = typename Superclass::FixedParametersType;
  using FixedParametersValueType = typename Superclass::FixedParametersValueType;

  /** Standard Jacobian container. */
  using JacobianType = typename Superclass::JacobianType;
  using JacobianPositionType = typename Superclass::JacobianPositionType;
  using InverseJacobianPositionType = typename Superclass::InverseJacobianPositionType;

  /** The number of parameters defining this transform. */
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  /** Standard vector type for this class. */
  using InputVectorType = typename Superclass::InputVectorType;
  using OutputVectorType = typename Superclass::OutputVectorType;

  /** Standard covariant vector type for this class. */
  using InputCovariantVectorType = typename Superclass::InputCovariantVectorType;
  using OutputCovariantVectorType = typename Superclass::OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  using InputVnlVectorType = typename Superclass::InputVnlVectorType;
  using OutputVnlVectorType = typename Superclass::OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  using InputPointType = typename Superclass::InputPointType;
  using OutputPointType = typename Superclass::OutputPointType;


  std::string
  GetTransformTypeAsString() const override;

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
  SetFixedParameters(const FixedParametersType & passedParameters) override;

  /** Parameters as SpaceDimension number of images. */
  using ImageType = typename Superclass::ImageType;
  using ImagePointer = typename Superclass::ImagePointer;
  using CoefficientImageArray = typename Superclass::CoefficientImageArray;

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
  void
  SetCoefficientImages(const CoefficientImageArray & images) override;

  /** Typedefs for specifying the extent of the grid. */
  using RegionType = typename Superclass::RegionType;

  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using SpacingType = typename Superclass::SpacingType;
  using DirectionType = typename Superclass::DirectionType;
  using OriginType = typename Superclass::OriginType;

  /** Interpolation weights function type. */
  using WeightsFunctionType = typename Superclass::WeightsFunctionType;

  using WeightsType = typename Superclass::WeightsType;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Parameter index array type. */
  using ParameterIndexArrayType = typename Superclass::ParameterIndexArrayType;

  /**
   * Transform points by a BSpline deformable transformation.
   * On return, weights contains the interpolation weights used to compute the
   * deformation and indices of the x (zeroth) dimension coefficient parameters
   * in the support region used to compute the deformation.
   * Parameter indices for the i-th dimension can be obtained by adding
   * ( i * this->GetNumberOfParametersPerDimension() ) to the indices array.
   */
  using Superclass::TransformPoint;
  void
  TransformPoint(const InputPointType &    point,
                 OutputPointType &         outputPoint,
                 WeightsType &             weights,
                 ParameterIndexArrayType & indices,
                 bool &                    inside) const override;

  /** Compute the Jacobian in one position. */
  void
  ComputeJacobianWithRespectToParameters(const InputPointType &, JacobianType &) const override;

  /** Return the number of parameters that completely define the Transfom. */
  NumberOfParametersType
  GetNumberOfParameters() const override;

  /** Return the number of parameters per dimension. */
  NumberOfParametersType
  GetNumberOfParametersPerDimension() const override;

  using PhysicalDimensionsType = typename Superclass::SpacingType;
  using PixelType = typename Superclass::PixelType;

  using MeshSizeType = typename Superclass::MeshSizeType;

  /** Function to specify the transform domain origin. */
  virtual void
  SetTransformDomainOrigin(const OriginType &);

  /** Function to retrieve the transform domain origin. */
  virtual OriginType
  GetTransformDomainOrigin() const;

  /** Function to specify the transform domain physical dimensions. */
  virtual void
  SetTransformDomainPhysicalDimensions(const PhysicalDimensionsType &);

  /** Function to retrieve the transform domain physical dimensions. */
  virtual PhysicalDimensionsType
  GetTransformDomainPhysicalDimensions() const;

  /** Function to specify the transform domain direction. */
  virtual void
  SetTransformDomainDirection(const DirectionType &);

  /** Function to retrieve the transform domain direction. */
  virtual DirectionType
  GetTransformDomainDirection() const;

  /** Function to specify the transform domain mesh size. */
  virtual void
  SetTransformDomainMeshSize(const MeshSizeType &);

  /** Function to retrieve the transform domain mesh size. */
  virtual MeshSizeType
  GetTransformDomainMeshSize() const;

protected:
  /** Print contents of an BSplineTransform. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  BSplineTransform();
  ~BSplineTransform() override = default;

private:
  /** Construct control point grid size from transform domain information in the fixed parameters. */
  void
  SetCoefficientImageInformationFromFixedParameters() override;

  /** Methods have empty implementations */
  void
  SetFixedParametersGridSizeFromTransformDomainInformation() const override{};
  void
  SetFixedParametersGridOriginFromTransformDomainInformation() const override{};
  void
  SetFixedParametersGridSpacingFromTransformDomainInformation() const override
  {}
  void
  SetFixedParametersGridDirectionFromTransformDomainInformation() const override{};

  /** Check if a continuous index is inside the valid region. */
  bool
  InsideValidRegion(ContinuousIndexType &) const override;

  void
  SetFixedParametersFromCoefficientImageInformation();

  void
  SetFixedParametersFromTransformDomainInformation(const OriginType &             meshOrigin,
                                                   const PhysicalDimensionsType & meshPhysical,
                                                   const DirectionType &          meshDirection,
                                                   const MeshSizeType &           meshSize);

}; // class BSplineTransform
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineTransform.hxx"
#endif

#endif /* itkBSplineTransform_h */
