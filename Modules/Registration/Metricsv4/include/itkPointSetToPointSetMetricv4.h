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
#ifndef __itkPointSetToPointSetMetricv4_h
#define __itkPointSetToPointSetMetricv4_h

#include "itkObjectToObjectMetric.h"

#include "itkFixedArray.h"
#include "itkPointsLocator.h"
#include "itkPointSet.h"
#include "itkTransform.h"

namespace itk
{
/** \class PointSetToPointSetMetricv4
 * \brief Computes similarity between two point sets.
 *
 * This class is templated over the type of the two point-sets.  It
 * expects a Transform to be plugged in.  This particular
 * class is the base class for a hierarchy of point-set to point-set metrics.
 *
 * This class computes a value that measures the similarity between the fixed
 * point-set and the transformed moving point-set.
 *
 * \ingroup ITKMetricsv4
 */

template<class TFixedPointSet,  class TMovingPointSet>
class ITK_EXPORT PointSetToPointSetMetricv4
: public ObjectToObjectMetric
{
public:

  /** Standard class typedefs. */
  typedef PointSetToPointSetMetricv4 Self;
  typedef ObjectToObjectMetric       Superclass;
  typedef SmartPointer<Self>         Pointer;
  typedef SmartPointer<const Self>   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( PointSetToPointSetMetricv4, ObjectToObjectMetric );

  /**  Type of the fixed point set. */
  typedef TFixedPointSet                               FixedPointSetType;
  typedef typename TFixedPointSet::PointType           FixedPointType;
  typedef typename TFixedPointSet::PixelType           FixedPixelType;
  typedef typename TFixedPointSet::PointsContainer     FixedPointsContainer;

  itkStaticConstMacro( FixedPointDimension, unsigned int,
    TFixedPointSet::PointDimension );

  /**  Type of the moving point set. */
  typedef TMovingPointSet                              MovingPointSetType;
  typedef typename TMovingPointSet::PointType          MovingPointType;
  typedef typename TMovingPointSet::PixelType          MovingPixelType;
  typedef typename TMovingPointSet::PointsContainer    MovingPointsContainer;

  itkStaticConstMacro( MovingPointDimension, unsigned int,
    TFixedPointSet::PointDimension );

  /**
   * typedefs for the data types used in the point set metric calculations.
   * It is assumed that the constants of the fixed point set, such as the
   * point dimension, are the same for the "common space" in which the metric
   * calculation occurs.
   */
  itkStaticConstMacro( PointDimension, unsigned int,
    TFixedPointSet::PointDimension );

  typedef FixedPointType                               PointType;
  typedef typename PointType::CoordRepType             CoordRepType;
  typedef FixedPointsContainer                         PointsContainer;
  typedef typename PointsContainer::ConstIterator      PointsConstIterator;
  typedef typename PointsContainer::ElementIdentifier  PointIdentifier;

  /** Typedef for points locator class to speed up finding neighboring points */
  typedef PointsLocator<PointIdentifier,
    itkGetStaticConstMacro( PointDimension ),
    CoordRepType, PointsContainer>                     PointsLocatorType;
  typedef typename PointsLocatorType::
    NeighborsIdentifierType                            NeighborsIdentifierType;

  /**  Type of the Transform Base class */
  typedef Transform<CoordRepType,
    itkGetStaticConstMacro( FixedPointDimension ),
    itkGetStaticConstMacro( PointDimension )>      FixedTransformType;
  typedef typename FixedTransformType::Pointer     FixedTransformPointer;
  typedef PointSet<FixedPixelType,
    itkGetStaticConstMacro( PointDimension )>      FixedTransformedPointSetType;

  typedef Transform<CoordRepType,
    itkGetStaticConstMacro( MovingPointDimension ),
    itkGetStaticConstMacro( PointDimension )>      MovingTransformType;
  typedef typename MovingTransformType::Pointer    MovingTransformPointer;
  typedef PointSet<MovingPixelType,
    itkGetStaticConstMacro( PointDimension )>      MovingTransformedPointSetType;

  /**  Type of the measure. */
  typedef typename Superclass::MeasureType MeasureType;

  /**  Type of the measure. */
  typedef typename Superclass::ParametersType ParametersType;

  /**  Type of the derivative. */
  typedef typename Superclass::DerivativeType DerivativeType;

  typedef typename DerivativeType::ValueType           DerivativeValueType;
  typedef FixedArray<DerivativeValueType,
    itkGetStaticConstMacro( PointDimension )>          LocalDerivativeType;

  /** Connect the fixed pointset.  */
  itkSetConstObjectMacro( FixedPointSet, FixedPointSetType );

  /** Get the fixed point set. */
  itkGetConstObjectMacro( FixedPointSet, FixedPointSetType );

  /** Get the moving transformed point set.  */
  itkGetConstObjectMacro( FixedTransformedPointSet, FixedTransformedPointSetType );

  /** Connect the fixed transform. */
  itkSetObjectMacro( FixedTransform, FixedTransformType );

  /** Get a pointer to the fixed transform.  */
  itkGetConstObjectMacro( FixedTransform, FixedTransformType );

  /** Connect the moving point set.  */
  itkSetObjectMacro( MovingPointSet, MovingPointSetType );

  /** Get the moving point set. */
  itkGetConstObjectMacro( MovingPointSet, MovingPointSetType );

  /** Get the moving transformed point set.  */
  itkGetConstObjectMacro( MovingTransformedPointSet, MovingTransformedPointSetType );

  /** Connect the moving transform. */
  itkSetObjectMacro( MovingTransform, MovingTransformType );

  /** Get a pointer to the moving transform.  */
  itkGetConstObjectMacro( MovingTransform, MovingTransformType );

  /**
   * For now return the number of points used in the value/derivative calculations.
   */
  unsigned int GetNumberOfComponents() const;

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
  virtual MeasureType GetValue() const;

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
  virtual void GetDerivative( DerivativeType & ) const;

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
  virtual void GetValueAndDerivative( MeasureType &, DerivativeType & ) const;

  /**
   * Function to be defined in the appropriate derived classes.  Calculates
   * the local metric value for a single point.
   */
  virtual MeasureType GetLocalNeighborhoodValue( const PointType & ) const = 0;

  /**
   * Calculates the local derivative for a single point.
   */
  virtual LocalDerivativeType GetLocalNeighborhoodDerivative( const PointType & ) const;

  /**
   * Calculates the local value/derivative for a single point.
   */
  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &,
    MeasureType &, LocalDerivativeType & ) const = 0;

  /**
   * Initialize the metric by making sure that all the components
   *  are present and plugged together correctly     .
   */
  virtual void Initialize( void ) throw ( ExceptionObject );

  /**
   * Get the number of transform parameters of the currently active transform.
   */
  virtual unsigned int GetNumberOfParameters() const;

  /**
   * Get the number of local transform parameters of the currently active transform.
   */
  virtual unsigned int GetNumberOfLocalParameters() const;

  /**
   * Set the transform parameters of the currently active transform.
   */
  virtual void SetParameters( ParametersType & );

  /**
   * Get the transform parameters of the currently active transform.
   */
  virtual const ParametersType & GetParameters() const;

  /** Return whether the metric's active transform has local support,
   * e.g. whether it is dense/high-dimensional.
   */
  virtual bool HasLocalSupport() const;

  /** Update the parameters of the metric's active transform.
   * Typically this call is passed through directly to the transform.
   * \c factor is a scalar multiplier for each value in update, and
   * defaults to 1.0 .
   * \c derivative must be the proper size, as retrieved
   * from GetNumberOfParameters. */
  virtual void UpdateTransformParameters( DerivativeType & derivative, ParametersValueType factor = 1.0 );

protected:
  PointSetToPointSetMetricv4();
  virtual ~PointSetToPointSetMetricv4();
  void PrintSelf( std::ostream & os, Indent indent ) const;

  typename FixedPointSetType::ConstPointer        m_FixedPointSet;
  typename FixedTransformedPointSetType::Pointer  m_FixedTransformedPointSet;

  FixedTransformPointer                           m_FixedTransform;
  typename PointsLocatorType::Pointer             m_FixedTransformedPointsLocator;

  typename MovingPointSetType::ConstPointer       m_MovingPointSet;
  typename MovingTransformedPointSetType::Pointer m_MovingTransformedPointSet;

  MovingTransformPointer                          m_MovingTransform;
  typename PointsLocatorType::Pointer             m_MovingTransformedPointsLocator;

  /**
   * Warp the fixed point set based on the fixed transform.  Note that the
   * warped moving point set is of type FixedPointSetType since the transform
   * takes the points from the fixed to the moving domain.
   */
  void TransformFixedPointSet();

  /**
   * Warp the moving point set based on the moving transform.  Note that the
   * warped moving point set is of type FixedPointSetType since the transform
   * takes the points from the moving to the fixed domain.
   */
  void TransformMovingPointSet();

  /**
   * Build point locators for the fixed and moving point sets to speed up
   * derivative and value calculations.
   */
  void InitializePointsLocators();

private:
  PointSetToPointSetMetricv4( const Self & ); //purposely not implemented
  void operator=( const Self & );           //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToPointSetMetricv4.hxx"
#endif

#endif
