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
#ifndef itkPointSetToPointSetMetricv4_h
#define itkPointSetToPointSetMetricv4_h

#include "itkObjectToObjectMetric.h"

#include "itkFixedArray.h"
#include "itkPointsLocator.h"
#include "itkPointSet.h"

namespace itk
{
/** \class PointSetToPointSetMetricv4
 * \brief Computes similarity between two point sets.
 *
 * This class is templated over the type of the two point-sets.  It
 * expects a Transform to be plugged in for each of fixed and moving
 * point sets. The transforms default to IdenityTransform types. This particular
 * class is the base class for a hierarchy of point-set to point-set metrics.
 *
 * This class computes a value that measures the similarity between the fixed
 * point-set and the moving point-set in the moving domain. The fixed point set
 * is transformed into the virtual domain by computing the inverse of the
 * fixed transform, then transformed into the moving domain using the
 * moving transform.
 *
 * Since the \c PointSet class permits each \c Point to be associated with a
 * \c PixelType, there are potential applications which could make use of
 * this additional information.  For example, the derived \c LabeledPointSetToPointSetMetric
 * class uses the \c PixelType as a \c LabelType for estimating total metric values
 * and gradients from the individual label-wise point subset metric and derivatives
 *
 * If a virtual domain is not defined by the user, one of two things happens:
 * 1) If the moving transform is a global type, then the virtual domain is
 * left undefined and every point is considered to be within the virtual domain.
 * 2) If the moving transform is a local-support type, then the virtual domain
 * is taken during initialization from the moving transform displacement field,
 * and all fixed points are verified to be within the virtual domain after
 * transformation by the inverse fixed transform. Points outside the virtual
 * domain are not used. See GetNumberOfValidPoints() to verify how many fixed
 * points were used during evaluation.
 *
 * See ObjectToObjectMetric documentation for more discussion on the virutal domain.
 *
 * \note When used with an RegistrationParameterScalesEstimator estimator, a VirtualDomainPointSet
 * must be defined and assigned to the estimator, for use in shift estimation.
 * The virtual domain point set can be retrieved from the metric using the
 * GetVirtualTransformedPointSet() method.
 *
 * \ingroup ITKMetricsv4
 */

template<typename TFixedPointSet,  typename TMovingPointSet,
  class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT PointSetToPointSetMetricv4
: public ObjectToObjectMetric<TFixedPointSet::PointDimension, TMovingPointSet::PointDimension,
   Image<TInternalComputationValueType, TFixedPointSet::PointDimension>, TInternalComputationValueType>
{
public:

  /** Standard class typedefs. */
  typedef PointSetToPointSetMetricv4                                        Self;
  typedef ObjectToObjectMetric<TFixedPointSet::PointDimension,
    TMovingPointSet::PointDimension,
    Image<TInternalComputationValueType, TFixedPointSet::PointDimension>,
    TInternalComputationValueType>                                          Superclass;
  typedef SmartPointer<Self>                                                Pointer;
  typedef SmartPointer<const Self>                                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( PointSetToPointSetMetricv4, ObjectToObjectMetric );

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType            MeasureType;

  /**  Type of the parameters. */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::ParametersValueType    ParametersValueType;
  typedef typename Superclass::NumberOfParametersType NumberOfParametersType;

  /**  Type of the derivative. */
  typedef typename Superclass::DerivativeType         DerivativeType;

  /** Transform types from Superclass*/
  typedef typename Superclass::FixedTransformType            FixedTransformType;
  typedef typename Superclass::FixedTransformPointer         FixedTransformPointer;
  typedef typename Superclass::FixedInputPointType           FixedInputPointType;
  typedef typename Superclass::FixedOutputPointType          FixedOutputPointType;
  typedef typename Superclass::FixedTransformParametersType  FixedTransformParametersType;

  typedef typename Superclass::MovingTransformType            MovingTransformType;
  typedef typename Superclass::MovingTransformPointer         MovingTransformPointer;
  typedef typename Superclass::MovingInputPointType           MovingInputPointType;
  typedef typename Superclass::MovingOutputPointType          MovingOutputPointType;
  typedef typename Superclass::MovingTransformParametersType  MovingTransformParametersType;

  typedef typename Superclass::JacobianType                   JacobianType;
  typedef typename Superclass::FixedTransformJacobianType     FixedTransformJacobianType;
  typedef typename Superclass::MovingTransformJacobianType    MovingTransformJacobianType;

  typedef typename Superclass::MovingDisplacementFieldTransformType  DisplacementFieldTransformType;

  typedef typename Superclass::ObjectType                     ObjectType;

  /** Dimension type */
  typedef typename Superclass::DimensionType                  DimensionType;

  /**  Type of the fixed point set. */
  typedef TFixedPointSet                               FixedPointSetType;
  typedef typename TFixedPointSet::PointType           FixedPointType;
  typedef typename TFixedPointSet::PixelType           FixedPixelType;
  typedef typename TFixedPointSet::PointsContainer     FixedPointsContainer;

  itkStaticConstMacro( FixedPointDimension, DimensionType, Superclass::FixedDimension );

  /**  Type of the moving point set. */
  typedef TMovingPointSet                              MovingPointSetType;
  typedef typename TMovingPointSet::PointType          MovingPointType;
  typedef typename TMovingPointSet::PixelType          MovingPixelType;
  typedef typename TMovingPointSet::PointsContainer    MovingPointsContainer;

  itkStaticConstMacro( MovingPointDimension, DimensionType, Superclass::MovingDimension );

  /**
   * typedefs for the data types used in the point set metric calculations.
   * It is assumed that the constants of the fixed point set, such as the
   * point dimension, are the same for the "common space" in which the metric
   * calculation occurs.
   */
  itkStaticConstMacro( PointDimension, DimensionType, Superclass::FixedDimension );

  typedef FixedPointType                               PointType;
  typedef FixedPixelType                               PixelType;
  typedef typename PointType::CoordRepType             CoordRepType;
  typedef FixedPointsContainer                         PointsContainer;
  typedef typename PointsContainer::ConstIterator      PointsConstIterator;
  typedef typename PointsContainer::ElementIdentifier  PointIdentifier;

  /** Typedef for points locator class to speed up finding neighboring points */
  typedef PointsLocator< PointsContainer>                     PointsLocatorType;
  typedef typename PointsLocatorType::NeighborsIdentifierType NeighborsIdentifierType;

  typedef PointSet<FixedPixelType, itkGetStaticConstMacro( PointDimension )>    FixedTransformedPointSetType;
  typedef PointSet<MovingPixelType, itkGetStaticConstMacro( PointDimension )>   MovingTransformedPointSetType;

  typedef typename DerivativeType::ValueType                                          DerivativeValueType;
  typedef FixedArray<DerivativeValueType, itkGetStaticConstMacro( PointDimension )>   LocalDerivativeType;

  /** Types for the virtual domain */
  typedef typename Superclass::VirtualImageType       VirtualImageType;
  typedef typename Superclass::VirtualImagePointer    VirtualImagePointer;
  typedef typename Superclass::VirtualPixelType       VirtualPixelType;
  typedef typename Superclass::VirtualRegionType      VirtualRegionType;
  typedef typename Superclass::VirtualSizeType        VirtualSizeType;
  typedef typename Superclass::VirtualSpacingType     VirtualSpacingType;
  typedef typename Superclass::VirtualPointType       VirtualOriginType;
  typedef typename Superclass::VirtualPointType       VirtualPointType;
  typedef typename Superclass::VirtualDirectionType   VirtualDirectionType;
  typedef typename Superclass::VirtualSizeType        VirtualRadiusType;
  typedef typename Superclass::VirtualIndexType       VirtualIndexType;
  typedef typename Superclass::VirtualPointSetType    VirtualPointSetType;
  typedef typename Superclass::VirtualPointSetPointer VirtualPointSetPointer;

  /** Set fixed point set*/
  virtual void SetFixedObject( const ObjectType *object ) ITK_OVERRIDE
    {
    FixedPointSetType *pointSet = dynamic_cast<FixedPointSetType *>( const_cast<ObjectType *>( object ) );
    if( pointSet != ITK_NULLPTR )
      {
      this->SetFixedPointSet( pointSet );
      }
    else
      {
      itkExceptionMacro( "Incorrect object type.  Should be a point set." )
      }
    }

  /** Set moving point set*/
  virtual void SetMovingObject( const ObjectType *object ) ITK_OVERRIDE
    {
    MovingPointSetType *pointSet = dynamic_cast<MovingPointSetType *>( const_cast<ObjectType *>( object ) );
    if( pointSet != ITK_NULLPTR )
      {
      this->SetMovingPointSet( pointSet );
      }
    else
      {
      itkExceptionMacro( "Incorrect object type.  Should be a point set." )
      }
    }

  /** Get/Set the fixed pointset.  */
  itkSetConstObjectMacro( FixedPointSet, FixedPointSetType );
  itkGetConstObjectMacro( FixedPointSet, FixedPointSetType );

  /** Get the fixed transformed point set.  */
  itkGetModifiableObjectMacro( FixedTransformedPointSet, FixedTransformedPointSetType );

  /** Get/Set the moving point set.  */
  itkSetConstObjectMacro( MovingPointSet, MovingPointSetType );
  itkGetConstObjectMacro( MovingPointSet, MovingPointSetType );

  /** Get the moving transformed point set.  */
  itkGetModifiableObjectMacro( MovingTransformedPointSet, MovingTransformedPointSetType );

  /**
   * For now return the number of points used in the value/derivative calculations.
   */
  SizeValueType GetNumberOfComponents() const;

  /**
   * This method returns the value of the metric based on the current
   * transformation(s).  This function can be redefined in derived classes
   * but many point set metrics follow the same structure---one iterates
   * through the points and, for each point a metric value is calculated.
   * The summation of these individual point metric values gives the total
   * value of the metric.  Note that this might not be applicable to all
   * point set metrics.  For those cases, the developer will have to redefine
   * the GetValue() function.
   */
  virtual MeasureType GetValue() const ITK_OVERRIDE;

  /**
   * This method returns the derivative based on the current
   * transformation(s).  This function can be redefined in derived classes
   * but many point set metrics follow the same structure---one iterates
   * through the points and, for each point a derivative is calculated.
   * The set of all these local derivatives constitutes the total derivative.
   * Note that this might not be applicable to all point set metrics.  For
   * those cases, the developer will have to redefine the GetDerivative()
   * function.
   */
  virtual void GetDerivative( DerivativeType & ) const ITK_OVERRIDE;

  /**
   * This method returns the derivative and value based on the current
   * transformation(s).  This function can be redefined in derived classes
   * but many point set metrics follow the same structure---one iterates
   * through the points and, for each point a derivative and value is calculated.
   * The set of all these local derivatives/values constitutes the total
   * derivative and value.  Note that this might not be applicable to all
   * point set metrics.  For those cases, the developer will have to redefine
   * the GetValue() and GetDerivative() functions.
   */
  virtual void GetValueAndDerivative( MeasureType &, DerivativeType & ) const ITK_OVERRIDE;

  /**
   * Function to be defined in the appropriate derived classes.  Calculates
   * the local metric value for a single point.  The \c PixelType may or
   * may not be used.  See class description for further explanation.
   */
  virtual MeasureType GetLocalNeighborhoodValue( const PointType &, const PixelType & pixel ) const = 0;

  /**
   * Calculates the local derivative for a single point. The \c PixelType may or
   * may not be used.  See class description for further explanation.
   */
  virtual LocalDerivativeType GetLocalNeighborhoodDerivative( const PointType &, const PixelType & pixel ) const;

  /**
   * Calculates the local value/derivative for a single point.  The \c PixelType may or
   * may not be used.  See class description for further explanation.
   */
  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &,
    MeasureType &, LocalDerivativeType &, const PixelType & pixel ) const = 0;

  /**
   * Get the virtual point set, derived from the fixed point set.
   * If the virtual point set has not yet been derived, it will be
   * in this call. */
  const VirtualPointSetType * GetVirtualTransformedPointSet() const;

  /**
   * Initialize the metric by making sure that all the components
   *  are present and plugged together correctly.
   */
  virtual void Initialize( void ) ITK_OVERRIDE;

  virtual bool SupportsArbitraryVirtualDomainSamples( void ) const ITK_OVERRIDE
  {
    /* An arbitrary point in the virtual domain will not always
     * correspond to a point within either point set. */
    return false;
  }

  /**
   * By default, the point set metric derivative for a displacement field transform
   * is stored by saving the gradient for every voxel in the displacement field (see
   * the function StorePointDerivative()).  Since the "fixed points" will typically
   * constitute a sparse set, this means that the field will have zero gradient values
   * at every voxel that doesn't have a corresponding point.  This might cause additional
   * computation time for certain transforms (e.g. B-spline SyN). To avoid this, this
   * option permits storing the point derivative only at the fixed point locations.
   * If this variable is set to false, then the derivative array will be of length
   * = PointDimension * m_FixedPointSet->GetNumberOfPoints().
   */
  itkSetMacro( StoreDerivativeAsSparseFieldForLocalSupportTransforms, bool );
  itkGetConstMacro( StoreDerivativeAsSparseFieldForLocalSupportTransforms, bool );
  itkBooleanMacro( StoreDerivativeAsSparseFieldForLocalSupportTransforms );

  /**
   *
   */
  itkSetMacro( CalculateValueAndDerivativeInTangentSpace, bool );
  itkGetConstMacro( CalculateValueAndDerivativeInTangentSpace, bool );
  itkBooleanMacro( CalculateValueAndDerivativeInTangentSpace );

protected:
  PointSetToPointSetMetricv4();
  virtual ~PointSetToPointSetMetricv4() ITK_OVERRIDE;
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  typename FixedPointSetType::ConstPointer                m_FixedPointSet;
  mutable typename FixedTransformedPointSetType::Pointer  m_FixedTransformedPointSet;

  mutable typename PointsLocatorType::Pointer             m_FixedTransformedPointsLocator;

  typename MovingPointSetType::ConstPointer               m_MovingPointSet;
  mutable typename MovingTransformedPointSetType::Pointer m_MovingTransformedPointSet;

  mutable typename PointsLocatorType::Pointer             m_MovingTransformedPointsLocator;

  /** Holds the fixed points after transformation into virtual domain. */
  mutable VirtualPointSetPointer                          m_VirtualTransformedPointSet;

  /**
   * Bool set by derived classes on whether the point set data (i.e. \c PixelType)
   * should be used.  Default = false.
   */
  bool m_UsePointSetData;

  /**
   * Flag to calculate value and/or derivative at tangent space.  This is needed
   * for the diffeomorphic registration methods.  The fixed and moving points are
   * warped to the virtual domain where the metric is calculated.  Derived point
   * set metrics might have associated gradient information which will need to be
   * warped if this flag is true.  Default = false.
   */
  bool m_CalculateValueAndDerivativeInTangentSpace;

  /**
   * Prepare point sets for use. */
  virtual void InitializePointSets() const;

  /**
   * Initialize to prepare for a particular iteration, generally
   * an iteration of optimization. Distinct from Initialize()
   * which is a one-time initialization. */
  virtual void InitializeForIteration() const;

  /**
   * Determine the number of valid fixed points. A fixed point
   * is valid if, when transformed into the virtual domain using
   * the inverse of the FixedTransform, it is within the defined
   * virtual domain bounds. */
  virtual SizeValueType CalculateNumberOfValidFixedPoints() const;

  /** Helper method allows for code reuse while skipping the metric value
   * calculation when appropriate */
  void CalculateValueAndDerivative( MeasureType & value, DerivativeType & derivative, bool calculateValue ) const;

  /**
   * Warp the fixed point set into the moving domain based on the fixed transform,
   * passing through the virtual domain and storing a virtual domain set.
   * Note that the warped moving point set is of type FixedPointSetType since the transform
   * takes the points from the fixed to the moving domain.
   */
  void TransformFixedAndCreateVirtualPointSet() const;

  /**
   * Warp the moving point set based on the moving transform.  Note that the
   * warped moving point set is of type FixedPointSetType since the transform
   * takes the points from the moving to the fixed domain.
   * FIXME: needs update.
   */
  void TransformMovingPointSet() const;

  /**
   * Build point locators for the fixed and moving point sets to speed up
   * derivative and value calculations.
   */
  void InitializePointsLocators() const;

  /**
   * Store a derivative from a single point in a field.
   * Only relevant when active transform has local support.
   */
  void StorePointDerivative( const VirtualPointType &, const DerivativeType &, DerivativeType & ) const;

  typedef typename Superclass::MetricCategoryType   MetricCategoryType;

  /** Get metric category */
  virtual MetricCategoryType GetMetricCategory() const ITK_OVERRIDE
    {
    return Superclass::POINT_SET_METRIC;
    }


private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToPointSetMetricv4);

  mutable bool m_MovingTransformPointLocatorsNeedInitialization;
  mutable bool m_FixedTransformPointLocatorsNeedInitialization;

  // Flag to keep track of whether a warning has already been issued
  // regarding the number of valid points.
  mutable bool m_HaveWarnedAboutNumberOfValidPoints;

  // Flag to store derivatives at fixed point locations with the rest being zero gradient
  // (default = true).
  bool m_StoreDerivativeAsSparseFieldForLocalSupportTransforms;

  mutable ModifiedTimeType m_MovingTransformedPointSetTime;
  mutable ModifiedTimeType m_FixedTransformedPointSetTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToPointSetMetricv4.hxx"
#endif

#endif
