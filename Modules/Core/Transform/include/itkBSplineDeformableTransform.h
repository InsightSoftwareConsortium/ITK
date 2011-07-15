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
#ifndef __itkBSplineDeformableTransform_h
#define __itkBSplineDeformableTransform_h

#include <iostream>
#include "itkTransform.h"
#include "itkImage.h"
#include "itkBSplineInterpolationWeightFunction.h"

namespace itk
{
/** \class BSplineDeformableTransform
 * \brief Deformable transform using a BSpline representation
 *
 * This class encapsulates a deformable transform of points from one
 * N-dimensional one space to another N-dimensional space.
 * The deformation field is modeled using B-splines.
 * A deformation is defined on a sparse regular grid of control points
 * \f$ \vec{\lambda}_j \f$ and is varied by defining a deformation
 * \f$ \vec{g}(\vec{\lambda}_j) \f$ of each control point.
 * The deformation \f$ D(\vec{x}) \f$ at any point \f$ \vec{x} \f$
 * is obtained by using a B-spline interpolation kernel.
 *
 * The deformation field grid is defined by a user specified GridRegion,
 * GridSpacing and GridOrigin. Each grid/control point has associated with it
 * N deformation coefficients \f$ \vec{\delta}_j \f$, representing the N
 * directional components of the deformation. Deformation outside the grid
 * plus support region for the BSpline interpolation is assumed to be zero.
 *
 * Additionally, the user can specified an addition bulk transform \f$ B \f$
 * such that the transformed point is given by:
 * \f[ \vec{y} = B(\vec{x}) + D(\vec{x}) \f]
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
 * typedef BSplineDeformableTransform<double,2,3> TransformType;
 * TransformType::Pointer transform = TransformType::New();
 *
 * transform->SetGridRegion( region );
 * transform->SetGridSpacing( spacing );
 * transform->SetGridOrigin( origin );
 *
 * // NB: the region must be set first before setting the parameters
 *
 * TransformType::ParametersType parameters(
 *                                       transform->GetNumberOfParameters() );
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
 * images. The grid region, spacing and origin information is taken
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
 * Warning: use either the SetParameters() or SetCoefficientImage()
 * API. Mixing the two modes may results in unexpected results.
 *
 * The class is templated coordinate representation type (float or double),
 * the space dimension and the spline order.
 *
 * \ingroup Transforms
 * \ingroup ITKTransform
 *
 * \wiki
 * \wikiexample{Registration/ImageRegistrationMethodBSpline,A global registration of two images}
 * \endwiki
 */
template<
  class TScalarType = double,            // Data type for scalars
  unsigned int NDimensions = 3,          // Number of dimensions
  unsigned int VSplineOrder = 3 >
// Spline order
class ITK_EXPORT BSplineDeformableTransform:
  public Transform< TScalarType, NDimensions, NDimensions >
{
public:
  /** Standard class typedefs. */
  typedef BSplineDeformableTransform                         Self;
  typedef Transform< TScalarType, NDimensions, NDimensions > Superclass;
  typedef SmartPointer< Self >                               Pointer;
  typedef SmartPointer< const Self >                         ConstPointer;

  /** New macro for creation of through the object factory. */
  static Pointer New(void);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineDeformableTransform, Transform);

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);

  /** The BSpline order. */
  itkStaticConstMacro(SplineOrder, unsigned int, VSplineOrder);

  /** Standard scalar type for this class. */
  typedef typename Superclass::ScalarType ScalarType;

  /** Standard parameters container. */
  typedef typename Superclass::ParametersType ParametersType;

  /** Standard Jacobian container. */
  typedef typename Superclass::JacobianType JacobianType;

  /** Standard vector type for this class. */
  typedef Vector< TScalarType,
                  itkGetStaticConstMacro(SpaceDimension) > InputVectorType;
  typedef Vector< TScalarType,
                  itkGetStaticConstMacro(SpaceDimension) > OutputVectorType;

  /** Standard covariant vector type for this class. */
  typedef CovariantVector< TScalarType,
                           itkGetStaticConstMacro(SpaceDimension) > InputCovariantVectorType;
  typedef CovariantVector< TScalarType,
                           itkGetStaticConstMacro(SpaceDimension) > OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef vnl_vector_fixed< TScalarType,
                            itkGetStaticConstMacro(SpaceDimension) > InputVnlVectorType;
  typedef vnl_vector_fixed< TScalarType,
                            itkGetStaticConstMacro(SpaceDimension) > OutputVnlVectorType;

  /** Standard coordinate point type for this class. */
  typedef Point< TScalarType,
                 itkGetStaticConstMacro(SpaceDimension) > InputPointType;
  typedef Point< TScalarType,
                 itkGetStaticConstMacro(SpaceDimension) > OutputPointType;

  /** This method sets the parameters of the transform.
   * For a BSpline deformation transform, the parameters are the BSpline
   * coefficients on a sparse grid.
   *
   * The parameters are N number of N-D grid of coefficients. Each N-D grid
   * is represented as a flat array of doubles
   * (in the same configuration as an itk::Image).
   * The N arrays are then concatenated to form one parameter array.
   *
   * For efficiency, this transform does not make a copy of the parameters.
   * It only keeps a pointer to the input parameters. It assumes that the memory
   * is managed by the caller. Use SetParametersByValue to force the transform
   * to call copy the parameters.
   *
   * This method wraps each grid as itk::Image's using the user specified
   * grid region, spacing and origin.
   * NOTE: The grid region, spacing and origin must be set first.
   *
   */
  void SetParameters(const ParametersType & parameters);

  /** This method sets the fixed parameters of the transform.
   * For a BSpline deformation transform, the parameters are the following:
   *    Grid Size, Grid Origin, and Grid Spacing
   *
   * The fixed parameters are the three times the size of the templated
   * dimensions.
   * This function has the effect of make the following calls:
   *       transform->SetGridSpacing( spacing );
   *       transform->SetGridOrigin( origin );
   *       transform->SetGridDirection( direction );
   *       transform->SetGridRegion( bsplineRegion );
   *
   * This function was added to allow the transform to work with the
   * itkTransformReader/Writer I/O filters.
   *
   */
  void SetFixedParameters(const ParametersType & parameters);

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
   * grid region, spacing and origin.
   * NOTE: The grid region, spacing and origin must be set first.
   *
   */
  void SetParametersByValue(const ParametersType & parameters);

  /** This method can ONLY be invoked AFTER calling SetParameters().
   *  This restriction is due to the fact that the BSplineDeformableTransform
   *  does not copy the array of paramters internally, instead it keeps a
   *  pointer to the user-provided array of parameters. This method is also
   *  in violation of the const-correctness of the parameters since the
   *  parameter array has been passed to the transform on a 'const' basis but
   *  the values get modified when the user invokes SetIdentity().
   */
  void SetIdentity();

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters(void) const;

  /** Get the Transformation Fixed Parameters. */
  virtual const ParametersType & GetFixedParameters(void) const;

  /** Parameters as SpaceDimension number of images. */
  typedef typename ParametersType::ValueType                         ParametersValueType;
  typedef Image< ParametersValueType, itkGetStaticConstMacro(SpaceDimension) > ImageType;
  typedef typename ImageType::Pointer                                ImagePointer;
  typedef typename itk::FixedArray<ImagePointer,NDimensions>         CoefficientImageArray;

  /** Get the array of coefficient images. */
  virtual CoefficientImageArray GetCoefficientImage()
  { return m_CoefficientImage; }
  virtual const CoefficientImageArray GetCoefficientImage() const
  { return m_CoefficientImage; }

  /** Set the array of coefficient images.
   *
   * This is an alternative API for setting the BSpline coefficients
   * as an array of SpaceDimension images. The grid region spacing
   * and origin is taken from the first image. It is assume that
   * the buffered region of all the subsequent images are the same
   * as the first image. Note that no error checking is done.
   *
   * Warning: use either the SetParameters() or SetCoefficientImage()
   * API. Mixing the two modes may results in unexpected results.
   *
   */
  virtual void SetCoefficientImage(const CoefficientImageArray & images);

  /** Typedefs for specifying the extent of the grid. */
  typedef ImageRegion< itkGetStaticConstMacro(SpaceDimension) > RegionType;

  typedef typename RegionType::IndexType    IndexType;
  typedef typename RegionType::SizeType     SizeType;
  typedef typename ImageType::SpacingType   SpacingType;
  typedef typename ImageType::DirectionType DirectionType;
  typedef typename ImageType::PointType     OriginType;

  /** This method specifies the region over which the grid resides. */
  virtual void SetGridRegion(const RegionType & region);

  itkGetConstMacro(GridRegion, RegionType);

  /** This method specifies the grid spacing or resolution. */
  virtual void SetGridSpacing(const SpacingType & spacing);

  itkGetConstMacro(GridSpacing, SpacingType);

  /** This method specifies the grid directions . */
  virtual void SetGridDirection(const DirectionType & spacing);

  itkGetConstMacro(GridDirection, DirectionType);

  /** This method specifies the grid origin. */
  virtual void SetGridOrigin(const OriginType & origin);

  itkGetConstMacro(GridOrigin, OriginType);

  /** Typedef of the bulk transform. */
  typedef Transform< ScalarType, itkGetStaticConstMacro(SpaceDimension),
                     itkGetStaticConstMacro(SpaceDimension) > BulkTransformType;
  typedef typename BulkTransformType::ConstPointer BulkTransformPointer;

  /** This method specifies the bulk transform to be applied.
   * The default is the identity transform.
   */
  itkSetConstObjectMacro(BulkTransform, BulkTransformType);
  itkGetConstObjectMacro(BulkTransform, BulkTransformType);

  /** Transform points by a BSpline deformable transformation. */
  OutputPointType  TransformPoint(const InputPointType  & point) const;

  /** Interpolation weights function type. */
  typedef BSplineInterpolationWeightFunction< ScalarType,
                                              itkGetStaticConstMacro(SpaceDimension),
                                              itkGetStaticConstMacro(SplineOrder) > WeightsFunctionType;
  typedef typename WeightsFunctionType::WeightsType WeightsType;
  typedef typename WeightsFunctionType::ContinuousIndexType
  ContinuousIndexType;

  /** Parameter index array type. */
  typedef Array< unsigned long > ParameterIndexArrayType;

  /** Transform points by a BSpline deformable transformation.
   * On return, weights contains the interpolation weights used to compute the
   * deformation and indices of the x (zeroth) dimension coefficient parameters
   * in the support region used to compute the deformation.
   * Parameter indices for the i-th dimension can be obtained by adding
   * ( i * this->GetNumberOfParametersPerDimension() ) to the indices array.
   */
  virtual void TransformPoint(const InputPointType & inputPoint,
                              OutputPointType & outputPoint,
                              WeightsType & weights,
                              ParameterIndexArrayType & indices,
                              bool & inside) const;

  virtual void GetJacobian(const InputPointType & inputPoint,
                           WeightsType & weights,
                           ParameterIndexArrayType & indices
                           ) const;

  /** Get number of weights. */
  unsigned long GetNumberOfWeights() const
  { return m_WeightsFunction->GetNumberOfWeights(); }

  /** Method to transform a vector -
   *  not applicable for this type of transform. */
  virtual OutputVectorType TransformVector(const InputVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for deformable transform.");
    return OutputVectorType();
  }

  /** Method to transform a vnl_vector -
   *  not applicable for this type of transform */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for deformable transform. ");
    return OutputVnlVectorType();
  }

  /** Method to transform a CovariantVector -
   *  not applicable for this type of transform */
  virtual OutputCovariantVectorType TransformCovariantVector(
    const InputCovariantVectorType &) const
  {
    itkExceptionMacro(<< "Method not applicable for deformable transfrom. ");
    return OutputCovariantVectorType();
  }

  /** Compute the Jacobian Matrix of the transformation at one point */
  virtual const JacobianType & GetJacobian(const InputPointType  & point) const;

  /** Return the number of parameters that completely define the Transfom */
  virtual unsigned int GetNumberOfParameters(void) const;

  /** Return the number of parameters per dimension */
  unsigned int GetNumberOfParametersPerDimension(void) const;

  /** Return the region of the grid wholly within the support region */
  itkGetConstReferenceMacro(ValidRegion, RegionType);

  /** Indicates that this transform is linear. That is, given two
   * points P and Q, and scalar coefficients a and b, then
   *
   *           T( a*P + b*Q ) = a * T(P) + b * T(Q)
   */
  virtual bool IsLinear() const { return false; }

  unsigned int GetNumberOfAffectedWeights() const;

protected:
  /** Print contents of an BSplineDeformableTransform. */
  void PrintSelf(std::ostream & os, Indent indent) const;

  BSplineDeformableTransform();
  virtual ~BSplineDeformableTransform();

  /** Allow subclasses to access and manipulate the weights function. */
  itkSetObjectMacro(WeightsFunction, WeightsFunctionType);
  itkGetObjectMacro(WeightsFunction, WeightsFunctionType);

  /** Wrap flat array into images of coefficients. */
  void WrapAsImages();

private:
  void SetFixedParametersRegionFromCoefficientImageInformation() const;
  void SetFixedParametersOriginFromCoefficientImageInformation() const;
  void SetFixedParametersSpacingFromCoefficientImageInformation() const;
  void SetFixedParametersDirectionFromCoefficientImageInformation() const;
  void SetFixedParametersFromCoefficientImageInformation() const;
  void SetCoefficientImageInformationFromFixedParameters();
  void UpdateValidGridRegion();

  BSplineDeformableTransform(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  CoefficientImageArray ArrayOfImagePointerGeneratorHelper(void) const;

  /** The bulk transform. */
  BulkTransformPointer m_BulkTransform;

  RegionType m_ValidRegion;

  /** Variables defining the interpolation support region. */
  unsigned long m_Offset;
  bool          m_SplineOrderOdd;
  SizeType      m_SupportSize;
  IndexType     m_ValidRegionLast;
  IndexType     m_ValidRegionFirst;

  //NOTE:  There is a natural duality between the
  //       two representations of of the coefficients
  //       whereby the m_InternalParametersBuffer is
  //       needed to fit into the optimization framework
  //       and the m_CoefficientImage is needed for
  //       the Jacobian computations.  This implementation
  //       is an attempt to remove as much redundancy as possible
  //       and share as much information between the two
  //       instances as possible.
  //
  /** Array of images representing the B-spline coefficients
   *  in each dimension wrapped from the flat parameters in
   *  m_InternalParametersBuffer
   */
  CoefficientImageArray m_CoefficientImage;

  /** The variables defining the coefficient grid domain for the
   * InternalParametersBuffer are taken from the m_CoefficientImage[0]
   * image, and must be kept in sync with them. by using
   * references to that instance, this is more naturally enforced
   * and does not introduce a speed penalty of dereferencing
   * through the pointers (although it does enforce some
   * internal class syncronization).
   */
  const RegionType    & m_GridRegion;
  const OriginType    & m_GridOrigin;
  const SpacingType   & m_GridSpacing;
  const DirectionType & m_GridDirection;

  /** Keep a pointer to the input parameters. */
  const ParametersType *m_InputParametersPointer;

  /** Internal parameters buffer. */
  ParametersType m_InternalParametersBuffer;

  /** Jacobian as SpaceDimension number of images. */
  typedef typename JacobianType::ValueType JacobianPixelType;
  typedef Image< JacobianPixelType,
                 itkGetStaticConstMacro(SpaceDimension) > JacobianImageType;
  typedef typename itk::FixedArray<typename JacobianImageType::Pointer,NDimensions> JacobianImageArrayType;

  JacobianImageArrayType m_JacobianImage;

  /** Keep track of last support region used in computing the Jacobian
   * for fast resetting of Jacobian to zero.
   */
  mutable IndexType m_LastJacobianIndex;

  /** Pointer to function used to compute Bspline interpolation weights. */
  typename WeightsFunctionType::Pointer m_WeightsFunction;

  /** Check if a continuous index is inside the valid region. */
  bool InsideValidRegion(const ContinuousIndexType & index) const;
}; //class BSplineDeformableTransform
}  // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BSplineDeformableTransform(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                              \
  {                                                                          \
  _( 3 ( class EXPORT BSplineDeformableTransform< ITK_TEMPLATE_3 TypeX > ) ) \
  namespace Templates                                                        \
  {                                                                          \
  typedef BSplineDeformableTransform< ITK_TEMPLATE_3 TypeX >                 \
  BSplineDeformableTransform##TypeY;                                       \
  }                                                                          \
  }

#if ITK_TEMPLATE_EXPLICIT
//template < class TScalarType, unsigned int NDimensions, unsigned int
// VSplineOrder >
//   const unsigned int itk::BSplineDeformableTransform<TScalarType,
// NDimensions, VSplineOrder >::SpaceDimension;
//template < class TScalarType, unsigned int NDimensions, unsigned int
// VSplineOrder >
//   const unsigned int itk::BSplineDeformableTransform<TScalarType,
// NDimensions, VSplineOrder >::SplineOrder;
#include "Templates/itkBSplineDeformableTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBSplineDeformableTransform.hxx"
#endif

#endif /* __itkBSplineDeformableTransform_h */
