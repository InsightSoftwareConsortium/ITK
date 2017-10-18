/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
 * number of polynomical pieces) and the number of control points in any
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
 * in the buffer; the N arrays are then concatentated together on form
 * a single array.
 *
 * For efficiency, this transform does not make a copy of the parameters.
 * It only keeps a pointer to the input parameters and assumes that the memory
 * is managed by the caller.
 *
 * The following illustrates the typical usage of this class:
 * \verbatim
 * typedef BSplineTransform<double,2,3> TransformType;
 * TransformType::Pointer transform = TransformType::New();
 *
 * transform->SetTransformDomainOrigin( origin );
 * transform->SetTransformDomainPhysicalDimensions( physicalDimensions );
 * transform->SetTransformDomainDirection( direction );
 * transform->SetTransformDomainMeshSize( meshSize );
 *
 * // NB: the region must be set first before setting the parameters
 *
 * TransformType::ParametersType parameters( transform->GetNumberOfParameters() );
 *
 * // Fill the parameters with values
 *
 * transform->SetParameters( parameters )
 *
 * outputPoint = transform->TransformPoint( inputPoint );
 *
 * \endverbatim
 *
 * An alternative way to set the B-spline coefficients is via array of
 * images. The fixed parameters of the transform are taken
 * directly from the first image. It is assumed that the subsequent images
 * are the same buffered region. The following illustrates the API:
 * \verbatim
 *
 * TransformType::ImageConstPointer images[2];
 *
 * // Fill the images up with values
 *
 * transform->SetCoefficientImages( images );
 * outputPoint = transform->TransformPoint( inputPoint );
 *
 * \endverbatim
 *
 * Warning: use either the SetParameters() or SetCoefficientImages()
 * API. Mixing the two modes may results in unexpected results.
 *
 * The class is templated coordinate representation type (float or double),
 * the space dimension and the spline order.
 *
 * \ingroup ITKTransform
 * \wikiexample{Registration/ImageRegistrationMethodBSpline,
 *   A global registration of two images}
 */
template<typename TParametersValueType=double,
          unsigned int NDimensions = 3,
          unsigned int VSplineOrder = 3>
class ITK_TEMPLATE_EXPORT BSplineTransform :
  public BSplineBaseTransform<TParametersValueType,NDimensions,VSplineOrder>
{
public:
  /** Standard class typedefs. */
  typedef BSplineTransform                                       Self;
  typedef BSplineBaseTransform<TParametersValueType,NDimensions,VSplineOrder> Superclass;
  typedef SmartPointer<Self>                                     Pointer;
  typedef SmartPointer<const Self>                               ConstPointer;

  /** New macro for creation of through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineTransform, BSplineBaseTransform );

  /** Dimension of the domain space. */
  itkStaticConstMacro( SpaceDimension, unsigned int, NDimensions );

  /** The BSpline order. */
  itkStaticConstMacro( SplineOrder, unsigned int, VSplineOrder );

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard parameters container. */
  typedef typename Superclass::ParametersType           ParametersType;
  typedef typename Superclass::ParametersValueType      ParametersValueType;
  typedef typename Superclass::FixedParametersType      FixedParametersType;
  typedef typename Superclass::FixedParametersValueType FixedParametersValueType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** The number of parameters defininig this transform. */
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType  InputVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef typename Superclass::InputCovariantVectorType  InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType  InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType  InputPointType;
  typedef typename Superclass::OutputPointType OutputPointType;


  virtual std::string GetTransformTypeAsString() const ITK_OVERRIDE;

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
  virtual void SetFixedParameters( const FixedParametersType & parameters ) ITK_OVERRIDE;

  /** Parameters as SpaceDimension number of images. */
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::ImagePointer          ImagePointer;
  typedef typename Superclass::CoefficientImageArray CoefficientImageArray;

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
  virtual void SetCoefficientImages( const CoefficientImageArray & images ) ITK_OVERRIDE;

  /** Typedefs for specifying the extent of the grid. */
  typedef typename Superclass::RegionType    RegionType;

  typedef typename Superclass::IndexType     IndexType;
  typedef typename Superclass::SizeType      SizeType;
  typedef typename Superclass::SpacingType   SpacingType;
  typedef typename Superclass::DirectionType DirectionType;
  typedef typename Superclass::OriginType    OriginType;

  /** Interpolation weights function type. */
  typedef typename Superclass::WeightsFunctionType WeightsFunctionType;

  typedef typename Superclass::WeightsType         WeightsType;
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Parameter index array type. */
  typedef typename Superclass::ParameterIndexArrayType ParameterIndexArrayType;

  /**
   * Transform points by a BSpline deformable transformation.
   * On return, weights contains the interpolation weights used to compute the
   * deformation and indices of the x (zeroth) dimension coefficient parameters
   * in the support region used to compute the deformation.
   * Parameter indices for the i-th dimension can be obtained by adding
   * ( i * this->GetNumberOfParametersPerDimension() ) to the indices array.
   */
  using Superclass::TransformPoint;
  virtual void TransformPoint( const InputPointType & inputPoint, OutputPointType & outputPoint,
    WeightsType & weights, ParameterIndexArrayType & indices, bool & inside ) const ITK_OVERRIDE;

  /** Compute the Jacobian in one position. */
  virtual void ComputeJacobianWithRespectToParameters( const InputPointType &, JacobianType & ) const ITK_OVERRIDE;

  /** Return the number of parameters that completely define the Transfom. */
  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE;

  /** Return the number of parameters per dimension. */
  NumberOfParametersType GetNumberOfParametersPerDimension() const ITK_OVERRIDE;

  typedef typename Superclass::SpacingType   PhysicalDimensionsType;
  typedef typename Superclass::PixelType     PixelType;

  typedef typename Superclass::MeshSizeType MeshSizeType;

  /** Function to specify the transform domain origin. */
  virtual void SetTransformDomainOrigin( const OriginType & );

  /** Function to retrieve the transform domain origin. */
  itkGetConstMacro( TransformDomainOrigin, OriginType );

  /** Function to specify the transform domain physical dimensions. */
  virtual void SetTransformDomainPhysicalDimensions( const PhysicalDimensionsType & );

  /** Function to retrieve the transform domain physical dimensions. */
  itkGetConstMacro( TransformDomainPhysicalDimensions, PhysicalDimensionsType );

  /** Function to specify the transform domain direction. */
  virtual void SetTransformDomainDirection( const DirectionType & );

  /** Function to retrieve the transform domain direction. */
  itkGetConstMacro( TransformDomainDirection, DirectionType );

  /** Function to specify the transform domain mesh size. */
  virtual void SetTransformDomainMeshSize( const MeshSizeType & );

  /** Function to retrieve the transform domain mesh size. */
  itkGetConstMacro( TransformDomainMeshSize, MeshSizeType );

protected:
  /** Print contents of an BSplineTransform. */
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  BSplineTransform();
  virtual ~BSplineTransform() ITK_OVERRIDE;

private:

  /** Construct control point grid size from transform domain information. */
  virtual void SetFixedParametersGridSizeFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid origin from transform domain information. */
  virtual void SetFixedParametersGridOriginFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid spacing from transform domain information. */
  virtual void SetFixedParametersGridSpacingFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid direction from transform domain information. */
  virtual void SetFixedParametersGridDirectionFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid size from transform domain information. */
  virtual void SetCoefficientImageInformationFromFixedParameters() ITK_OVERRIDE;

  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineTransform);

  /** Check if a continuous index is inside the valid region. */
  virtual bool InsideValidRegion( ContinuousIndexType & ) const ITK_OVERRIDE;

private:

  OriginType             m_TransformDomainOrigin;
  PhysicalDimensionsType m_TransformDomainPhysicalDimensions;
  DirectionType          m_TransformDomainDirection;
  DirectionType          m_TransformDomainDirectionInverse;

  MeshSizeType m_TransformDomainMeshSize;
}; // class BSplineTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineTransform.hxx"
#endif

#endif /* itkBSplineTransform_h */
