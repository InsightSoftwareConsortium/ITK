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
#ifndef itkExpectationBasedPointSetToPointSetMetricv4_h
#define itkExpectationBasedPointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"
#include "itkPointSet.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class ExpectationBasedPointSetToPointSetMetricv4
 * \brief Computes an expectation-based metric between two point sets.
 *
 * This information-theoretic point set measure models each point set
 * as a sum of Gaussians.  To speed up computation, evaluation of the local
 * value/derivative is done in a user-specified neighborhood using the k-d
 * tree constructed in the superclass.
 *
 *  Reference:
 *    Pluta J, Avants BB, Glynn S, Awate S, Gee JC, Detre JA,
 *    "Appearance and incomplete label matching for diffeomorphic template
 *     "based hippocampus segmentation", Hippocampus, 2009 Jun; 19(6):565-71.
 *
 * \ingroup ITKMetricsv4
 */
template<typename TFixedPointSet, typename TMovingPointSet = TFixedPointSet,
  class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT ExpectationBasedPointSetToPointSetMetricv4:
  public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:

  /** Standard class typedefs. */
  typedef ExpectationBasedPointSetToPointSetMetricv4                   Self;
  typedef PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet,
    TInternalComputationValueType>                                     Superclass;
  typedef SmartPointer<Self>                                           Pointer;
  typedef SmartPointer<const Self>                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ExpectationBasedPointSetToPointSetMetricv4, PointSetToPointSetMetricv4 );

   /** Types transferred from the base class */
  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::LocalDerivativeType      LocalDerivativeType;
  typedef typename Superclass::PointType                PointType;
  typedef typename Superclass::PixelType                PixelType;
  typedef typename Superclass::CoordRepType             CoordRepType;
  typedef typename Superclass::PointIdentifier          PointIdentifier;
  typedef typename Superclass::NeighborsIdentifierType  NeighborsIdentifierType;

  /**
   * Calculates the local metric value for a single point.
   */
  virtual MeasureType GetLocalNeighborhoodValue( const PointType &, const PixelType & pixel = 0 ) const ITK_OVERRIDE;

  /**
   * Calculates the local value and derivative for a single point.
   */
  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &,
    MeasureType &, LocalDerivativeType &, const PixelType & pixel = 0 ) const ITK_OVERRIDE;

  /**
   * Each point is associated with a Gaussian characterized by m_PointSetSigma
   * which provides a sense of scale for determining the similarity between two
   * point sets.  Default = 1.0.
   */
  itkSetMacro( PointSetSigma, CoordRepType );

  /** Get the point set sigma function */
  itkGetConstMacro( PointSetSigma, CoordRepType );

  /**
   * Set the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkSetMacro( EvaluationKNeighborhood, unsigned int );

  /**
   * Get the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkGetConstMacro( EvaluationKNeighborhood, unsigned int );

  void Initialize( void ) ITK_OVERRIDE;

  /** Clone method will clone the existing instance of this type,
   *  including its internal member variables. */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

protected:
  ExpectationBasedPointSetToPointSetMetricv4();
  virtual ~ExpectationBasedPointSetToPointSetMetricv4() ITK_OVERRIDE;

  /** PrintSelf function */
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExpectationBasedPointSetToPointSetMetricv4);

  typedef typename PointType::VectorType                    VectorType;
  typedef typename NeighborsIdentifierType::const_iterator  NeighborsIterator;

  CoordRepType                               m_PointSetSigma;
  MeasureType                                m_PreFactor;
  MeasureType                                m_Denominator;
  unsigned int                               m_EvaluationKNeighborhood;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExpectationBasedPointSetToPointSetMetricv4.hxx"
#endif

#endif
