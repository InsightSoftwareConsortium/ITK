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
#ifndef itkBSplineDeformableTransform_h
#define itkBSplineDeformableTransform_h

#include "itkConfigure.h" // Needed to determine value of ITKV3_COMPATIBILITY
#include "itkBSplineBaseTransform.h"

namespace itk
{
/** \class BSplineDeformableTransform
 *
 * \brief Deformable transform using a BSpline representation
 *
 * \note BSplineTransform is a newer version of this class, and it is
 * preferred.
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
 * The parameters for this transform is an N x N-D grid of spline coefficients.
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
 * Warning: use either the SetParameters() or SetCoefficientImages()
 * API. Mixing the two modes may results in unexpected results.
 *
 * The class is templated coordinate representation type (float or double),
 * the space dimension and the spline order.
 *
 * \ingroup ITKTransform
 *
 * \sa BSplineTransform
 *
 * \wiki
 * \wikiexample{Registration/ImageRegistrationMethodBSpline,A global registration of two images}
 * \endwiki
 */
template<typename TParametersValueType=double,
          unsigned int NDimensions = 3,
          unsigned int VSplineOrder = 3>
class ITK_TEMPLATE_EXPORT BSplineDeformableTransform :
  public BSplineBaseTransform<TParametersValueType,NDimensions,VSplineOrder>
{
public:
  /** Standard class typedefs. */
  typedef BSplineDeformableTransform                                           Self;
  typedef BSplineBaseTransform<TParametersValueType,NDimensions,VSplineOrder> Superclass;
  typedef SmartPointer<Self>                                                   Pointer;
  typedef SmartPointer<const Self>                                             ConstPointer;

  /** New macro for creation of through the object factory. */
  // Explicit New() method, used here because we need to split the itkNewMacro()
  // in order to overload the CreateAnother() method so that we can copy the m_BulkTransform
  // explicitly.
  // TODO: shouldn't it be done with the Clone() method?
  itkSimpleNewMacro(Self);
  virtual ::itk::LightObject::Pointer CreateAnother(void) const ITK_OVERRIDE
    {
    ::itk::LightObject::Pointer smartPtr;
    Pointer copyPtr = Self::New().GetPointer();
    //THE FOLLOWING LINE IS DIFFERENT FROM THE DEFAULT MACRO!
    copyPtr->m_BulkTransform =  this->GetBulkTransform();
    smartPtr = static_cast<Pointer>( copyPtr );
    return smartPtr;
    }

  /** implement type-specific clone method*/
  itkCloneMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( BSplineDeformableTransform, BSplineBaseTransform );

  /** Dimension of the domain space. */
  itkStaticConstMacro( SpaceDimension, unsigned int, NDimensions );

  /** The BSpline order. */
  itkStaticConstMacro( SplineOrder, unsigned int, VSplineOrder );

  /** Standard scalar type for this class. */
  typedef TParametersValueType ScalarType;

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
  typedef Point <TParametersValueType, itkGetStaticConstMacro( SpaceDimension )> InputPointType;
  typedef Point <TParametersValueType, itkGetStaticConstMacro( SpaceDimension )> OutputPointType;


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

#ifdef ITKV3_COMPATIBILITY
  virtual void SetCoefficientImage( const CoefficientImageArray & images )
    {
    this->SetCoefficientImages( images );
    }
  /* Only for backwards compatibility with ITKv3. */
  CoefficientImageArray GetCoefficientImage()
    {
    return this->GetCoefficientImages();
    }
#endif

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

  virtual void ComputeJacobianWithRespectToParameters( const InputPointType &, JacobianType & ) const ITK_OVERRIDE;

  /** Return the number of parameters that completely define the Transfom */
  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE;

  /** Return the number of parameters per dimension */
  NumberOfParametersType GetNumberOfParametersPerDimension() const ITK_OVERRIDE;

  typedef typename Superclass::SpacingType   PhysicalDimensionsType;
  typedef typename Superclass::PixelType     PixelType;

  typedef typename Superclass::MeshSizeType MeshSizeType;

  /** Function to specify the transform domain origin. */
  virtual void SetGridOrigin( const OriginType & );

  /** Function to retrieve the transform domain origin. */
  itkGetConstMacro( GridOrigin, OriginType );

  /** This method specifies the grid spacing or resolution. */
  virtual void SetGridSpacing( const SpacingType & );

  /** This method retrieve the grid spacing or resolution. */
  itkGetConstMacro( GridSpacing, SpacingType );

  /** Function to specify the transform domain direction. */
  virtual void SetGridDirection( const DirectionType & );

  /** Function to retrieve the transform domain direction. */
  itkGetConstMacro( GridDirection, DirectionType );

  /** Function to specify the transform domain mesh size. */
  virtual void SetGridRegion( const RegionType & );

  /** Function to retrieve the transform domain mesh size. */
  itkGetConstMacro( GridRegion, RegionType );

  typedef Transform<TParametersValueType,
                    itkGetStaticConstMacro(SpaceDimension),
                    itkGetStaticConstMacro(SpaceDimension)> BulkTransformType;
  typedef typename BulkTransformType::ConstPointer BulkTransformPointer;
  /** This method specifies the bulk transform to be applied.
   * The default is the identity transform.
   */
  itkSetConstObjectMacro(BulkTransform, BulkTransformType);
  itkGetConstObjectMacro(BulkTransform, BulkTransformType);

  /** Return the region of the grid wholly within the support region */
  itkGetConstReferenceMacro(ValidRegion, RegionType);

protected:
  /** Print contents of an BSplineDeformableTransform. */
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  BSplineDeformableTransform();
  virtual ~BSplineDeformableTransform() ITK_OVERRIDE;

private:

  /** Construct control point grid size from transform domain information */
  virtual void SetFixedParametersGridSizeFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid origin from transform domain information */
  virtual void SetFixedParametersGridOriginFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid spacing from transform domain information */
  virtual void SetFixedParametersGridSpacingFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid direction from transform domain information */
  virtual void SetFixedParametersGridDirectionFromTransformDomainInformation() const ITK_OVERRIDE;

  /** Construct control point grid size from transform domain information */
  virtual void SetCoefficientImageInformationFromFixedParameters() ITK_OVERRIDE;

  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineDeformableTransform);

  /** Check if a continuous index is inside the valid region. */
  virtual bool InsideValidRegion( ContinuousIndexType & ) const ITK_OVERRIDE;

  /** The variables defining the coefficient grid domain for the
   * InternalParametersBuffer are taken from the m_CoefficientImages[0]
   * image, and must be kept in sync with them. by using
   * references to that instance, this is more naturally enforced
   * and does not introduce a speed penalty of dereferencing
   * through the pointers (although it does enforce some
   * internal class synchronization).
   */
  const RegionType &    m_GridRegion;
  const OriginType &    m_GridOrigin;
  const SpacingType &   m_GridSpacing;
  const DirectionType & m_GridDirection;

  /** The bulk transform. */
  BulkTransformPointer m_BulkTransform;

  RegionType m_ValidRegion;

  /** Variables defining the interpolation support region. */
  unsigned long m_Offset;
  bool          m_SplineOrderOdd;
  IndexType     m_ValidRegionLast;
  IndexType     m_ValidRegionFirst;

  void UpdateValidGridRegion();

}; // class BSplineDeformableTransform
}  // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineDeformableTransform.hxx"
#endif

#endif /* itkBSplineDeformableTransform_h */
