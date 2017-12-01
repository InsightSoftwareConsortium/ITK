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
#ifndef itkObjectToObjectMetric_h
#define itkObjectToObjectMetric_h


#include "itkObjectToObjectMetricBase.h"

#include "itkDisplacementFieldTransform.h"
#include "itkImage.h"
#include "itkObject.h"
#include "itkPointSet.h"
#include "itkTransform.h"

namespace itk
{

/** \class ObjectToObjectMetric
 * \brief Computes similarity between regions of two objects.
 *
 * This class is templated over the dimensionality of the two input objects.
 * This is the abstract templated base class for a hierarchy of similarity metrics
 * that may, in derived classes, operate on meshes, images, etc.
 * This class computes a value that measures the similarity between the two
 * objects.
 *
 * Derived classes must provide implementations for:
 *  GetValue
 *  GetDerivative
 *  GetValueAndDerivative
 *  SupportsArbitraryVirtualDomainSamples
 *
 * Similarity is evaluated using fixed and moving transforms.
 * Both transforms are initialized to an IdentityTransform, and can be
 * set by the user using SetFixedTranform() and SetMovingTransform().
 *
 * Virtual Domain
 *
 * This class uses a virtual reference space. This space defines the resolution
 * at which the evaluation is performed, as well as the physical coordinate
 * system. This is useful for unbiased registration. The virtual domain is stored
 * in the m_VirtualDomain member, but this is subject to change so the convenience
 * methods GetVirtualSpacing(), GetVirtualDirection() and GetVirtualOrigin() should
 * be used whenever possible to retrieve virtual domain information. The region over which
 * metric evaluation is performed is taken from the virtual image buffered region.
 *
 * The user can define a virtual domain by calling either
 * \c SetVirtualDomain or \c SetVirtualDomainFromImage. See these
 * methods for details. Derived classes may automatically assign a virtual domain
 * if the user has not assigned one by initialization time.
 *
 * If the virtual domain is left undefined by the user and by derived classes,
 * then unit or zero values are returned for GetVirtualSpacing(),
 * GetVirtualDirection() and GetVirtualOrigin(), as appropriate. The virtual region is left
 * undefined and an attempt to retrieve it via GetVirtualRegion() will generate an exception.
 * The m_VirtualImage member will be ITK_NULLPTR.
 *
 * During evaluation, derived classes should verify that points are within the virtual domain
 * and thus valid, as appropriate for the needs of the metric. When points are deemed invalid
 * the number of valid points returned by GetNumberOfValidPoints() should reflect this.
 *
 * \note Transform Optimization
 * This hierarchy currently assumes only the moving transform is 'active',
 * i.e. only the moving transform is being optimized when used in an optimizer.
 * Methods relevant to transform optimization such as GetNumberOfParameters(),
 * UpdateTransformParameters() are passed on to the active transform.
 * The eventual goal however is to allow for either moving, fixed or both
 * transforms to be active within a single metric.
 *
 * \ingroup ITKOptimizersv4
 */
template<unsigned int TFixedDimension, unsigned int TMovingDimension, typename TVirtualImage = Image<double, TFixedDimension>,
         typename TParametersValueType=double>
class ITK_TEMPLATE_EXPORT ObjectToObjectMetric:
  public ObjectToObjectMetricBaseTemplate<TParametersValueType>
{
public:
  /** Standard class typedefs. */
  typedef ObjectToObjectMetric                                   Self;
  typedef ObjectToObjectMetricBaseTemplate<TParametersValueType> Superclass;
  typedef SmartPointer<Self>                                     Pointer;
  typedef SmartPointer<const Self>                               ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectToObjectMetric, ObjectToObjectMetricBaseTemplate);

  /** Type used for representing object components  */
  typedef TParametersValueType            CoordinateRepresentationType;

  /** Type for internal computations */
  typedef TParametersValueType            InternalComputationValueType;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType            MeasureType;

  /**  Type of object. */
  typedef typename Superclass::Object                 ObjectType;

  /**  Type of the derivative. */
  typedef typename Superclass::DerivativeType         DerivativeType;
  typedef typename Superclass::DerivativeValueType    DerivativeValueType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  typedef typename Superclass::GradientSourceType     GradientSourceType;

  /** Dimension type */
  typedef SizeValueType                               DimensionType;

  /** Object dimension accessors */
  itkStaticConstMacro(FixedDimension, DimensionType, TFixedDimension);
  itkStaticConstMacro(MovingDimension, DimensionType, TMovingDimension);
  itkStaticConstMacro(VirtualDimension, DimensionType, TVirtualImage::ImageDimension);

  /** Types for the virtual domain */
  typedef TVirtualImage                             VirtualImageType;
  typedef typename VirtualImageType::Pointer        VirtualImagePointer;
  typedef typename VirtualImageType::ConstPointer   VirtualImageConstPointer;
  typedef typename VirtualImageType::PixelType      VirtualPixelType;
  typedef typename VirtualImageType::RegionType     VirtualRegionType;
  typedef typename VirtualRegionType::SizeType      VirtualSizeType;
  typedef typename VirtualImageType::SpacingType    VirtualSpacingType;
  typedef typename VirtualImageType::PointType      VirtualOriginType;
  typedef typename VirtualImageType::PointType      VirtualPointType;
  typedef typename VirtualImageType::DirectionType  VirtualDirectionType;
  typedef typename VirtualImageType::SizeType       VirtualRadiusType;
  typedef typename VirtualImageType::IndexType      VirtualIndexType;

  /** Point set in the virtual domain */
  typedef PointSet<VirtualPixelType, itkGetStaticConstMacro(VirtualDimension)>  VirtualPointSetType;
  typedef typename VirtualPointSetType::Pointer                                 VirtualPointSetPointer;

  /**  Type of the Transform Base classes */
  typedef Transform<TParametersValueType,
                    TVirtualImage::ImageDimension,
                    TMovingDimension>                  MovingTransformType;
  typedef Transform<TParametersValueType,
                    TVirtualImage::ImageDimension,
                    TFixedDimension>                   FixedTransformType;

  typedef typename FixedTransformType::Pointer         FixedTransformPointer;
  typedef typename FixedTransformType::InputPointType  FixedInputPointType;
  typedef typename FixedTransformType::OutputPointType FixedOutputPointType;
  typedef typename FixedTransformType::ParametersType  FixedTransformParametersType;

  typedef typename MovingTransformType::Pointer         MovingTransformPointer;
  typedef typename MovingTransformType::InputPointType  MovingInputPointType;
  typedef typename MovingTransformType::OutputPointType MovingOutputPointType;
  typedef typename MovingTransformType::ParametersType  MovingTransformParametersType;

  /** Jacobian type. This is the same for all transforms */
  typedef typename FixedTransformType::JacobianType     JacobianType;
  typedef typename FixedTransformType::JacobianType     FixedTransformJacobianType;
  typedef typename MovingTransformType::JacobianType    MovingTransformJacobianType;

  /** DisplacementFieldTransform types for working with local-support transforms */
  typedef DisplacementFieldTransform<CoordinateRepresentationType, itkGetStaticConstMacro( MovingDimension ) >  MovingDisplacementFieldTransformType;

  virtual void Initialize(void) ITK_OVERRIDE;

  virtual NumberOfParametersType GetNumberOfParameters() const ITK_OVERRIDE;
  virtual NumberOfParametersType GetNumberOfLocalParameters() const ITK_OVERRIDE;
  virtual void SetParameters( ParametersType & params ) ITK_OVERRIDE;
  virtual const ParametersType & GetParameters() const ITK_OVERRIDE;
  virtual bool HasLocalSupport() const ITK_OVERRIDE;
  virtual void UpdateTransformParameters( const DerivativeType & derivative, TParametersValueType factor) ITK_OVERRIDE;

  /** Connect the fixed transform. */
  itkSetObjectMacro(FixedTransform, FixedTransformType);

  /** Get a pointer to the fixed transform.  */
  itkGetModifiableObjectMacro(FixedTransform, FixedTransformType);

  /** Connect the moving transform. */
  itkSetObjectMacro(MovingTransform, MovingTransformType);

  /** Get a pointer to the moving transform.  */
  itkGetModifiableObjectMacro(MovingTransform, MovingTransformType);

  /** Connect the moving transform using a backwards-compatible name.
   * This assigns the input transform to the moving transform. */
  void SetTransform( MovingTransformType* transform );

  /** Get the moving transform using a backwards-compatible name */
  const MovingTransformType * GetTransform();

  /** Get the number of valid points after a call to evaluate the
   * metric. */
  itkGetConstMacro(NumberOfValidPoints, SizeValueType)

  /** Define the virtual reference space. This space defines the resolution
   * at which the registration is performed as well as the physical coordinate
   * system.  Useful for unbiased registration.
   * This method will allocate \c m_VirtualImage with the passed
   * information, with the pixel buffer left unallocated.
   * Metric evaluation will be performed within the constraints of the virtual
   * domain depending on implementation in derived classes.
   * A default domain is created during initializaiton in derived
   * classes according to their need.
   * \param spacing   spacing
   * \param origin    origin
   * \param direction direction
   * \param region    region is used to set all image regions.
   *
   * \sa SetVirtualDomainFromImage
   */
  void SetVirtualDomain( const VirtualSpacingType & spacing, const VirtualOriginType & origin,
                         const VirtualDirectionType & direction, const VirtualRegionType & region );

  /** Use a virtual domain image to define the virtual reference space.
   * \sa SetVirtualDomain */
  void SetVirtualDomainFromImage( const VirtualImageType * virtualImage);

  /** Returns a flag. True if arbitrary virtual domain points will
   *  always correspond to data points. False if not. For example,
   *  point-set metrics return false because only some virtual domain
   *  points will correspond to points within the point sets. */
  virtual bool SupportsArbitraryVirtualDomainSamples( void ) const = 0;

  /** Return a timestamp relating to the virtual domain.
   * This returns the greater of the metric timestamp and the
   * virtual domain image timestamp. This allows us to
   * capture if the virtual domain image is changed by the user
   * after being assigned to the metric. */
  virtual const TimeStamp& GetVirtualDomainTimeStamp() const;

  /** Accessors for the virtual domain spacing.
   *  Returns unit spacing if a virtual domain is undefined. */
  VirtualSpacingType GetVirtualSpacing() const;

  /** Accessor for virtual domain origin.
   *  Returns zero origin if a virtual domain is undefined. */
  VirtualOriginType  GetVirtualOrigin() const;

  /** Accessor for virtual domain direction.
   *  Returns unit direction if a virtual domain is undefined. */
  VirtualDirectionType GetVirtualDirection() const;

  /** Return the virtual domain region, which is retrieved from
   *  the m_VirtualImage buffered region. */
  const VirtualRegionType   &  GetVirtualRegion() const;

  itkGetModifiableObjectMacro(VirtualImage, VirtualImageType );

  /** Computes an offset for accessing parameter data from a virtual domain
   * index. Relevant for metrics with local-support transforms, to access
   * parameter or derivative memory that is stored linearly in a 1D array.
   * The result is the offset (1D array index) to the first of N parameters
   * corresponding to the given virtual index, where N is the number of
   * local parameters.
   * \param index the virtual index to convert
   * \param numberOfLocalParameters corresponding to the transform
   **/
  OffsetValueType ComputeParameterOffsetFromVirtualIndex( const VirtualIndexType & index, const NumberOfParametersType &numberOfLocalParameters ) const;

  /** Computes an offset for accessing parameter data from a virtual domain
   * point. Relevant for metrics with local-support transforms, to access
   * parameter or derivative memory that is stored linearly in a 1D array.
   * The result is the offset (1D array index) to the first of N parameters
   * corresponding to the given virtual index, where N is the number of
   * local parameters.
   * \param point the virtual point to convert
   * \param numberOfLocalParameters corresponding to the transform
   **/
  OffsetValueType ComputeParameterOffsetFromVirtualPoint( const VirtualPointType & point, const NumberOfParametersType & numberOfLocalParameters ) const;

  /** Determine if a point is within the virtual domain.
   * \note Returns true if the virtual domain has not been defined. This
   * allows, for example, use in point set metrics where the virtual domain
   * is implicitly defined by the point sets and transforms. */
  bool IsInsideVirtualDomain( const VirtualPointType & point ) const;
  bool IsInsideVirtualDomain( const VirtualIndexType & index ) const;

  typedef typename Superclass::MetricCategoryType   MetricCategoryType;

  /** Get metric category */
  virtual MetricCategoryType GetMetricCategory() const ITK_OVERRIDE
    {
    return Superclass::OBJECT_METRIC;
    }

protected:
  ObjectToObjectMetric();
  virtual ~ObjectToObjectMetric() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Verify that virtual domain and displacement field are the same size
   * and in the same physical space. */
  virtual void VerifyDisplacementFieldSizeAndPhysicalSpace();

  bool TransformPhysicalPointToVirtualIndex( const VirtualPointType &, VirtualIndexType & ) const;
  void TransformVirtualIndexToPhysicalPoint( const VirtualIndexType &, VirtualPointType & ) const;

  /** If the moving transform is a DisplacementFieldTransform, return it.
   *  If the moving transform is a CompositeTransform, the routine will check if the
   *  first (last to be added) transform is a DisplacementFieldTransform, and if so return it.
   *  Otherwise, return ITK_NULLPTR. */
  const MovingDisplacementFieldTransformType * GetMovingDisplacementFieldTransform() const;

  /** Check that the number of valid points is above a default
   * minimum (zero). If not, then return false, and assign to 'value' a value
   * indicating insufficient valid points were found during evaluation, and set
   * the derivative to zero. A warning is also output.
   * This functionality is provided as a separate method so derived classes
   * can use it without hardcoding the details. */
  bool VerifyNumberOfValidPoints( MeasureType & value, DerivativeType & derivative ) const;

  /** Transforms */
  FixedTransformPointer   m_FixedTransform;
  MovingTransformPointer  m_MovingTransform;

  VirtualImagePointer     m_VirtualImage;

  /** Flag that is set when user provides a virtual domain, either via
   * SetVirtualDomain() or SetVirtualDomainFromImage(). */
  bool                    m_UserHasSetVirtualDomain;

  /** Store the number of points used during most recent value and derivative
   * calculation.
   * \sa VerifyNumberOfValidPoints() */
  mutable SizeValueType                   m_NumberOfValidPoints;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectToObjectMetric);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectToObjectMetric.hxx"
#endif

#endif
