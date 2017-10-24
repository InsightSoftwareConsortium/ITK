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
#ifndef itkEuclideanDistancePointSetToPointSetMetricv4_h
#define itkEuclideanDistancePointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"

namespace itk
{
/** \class EuclideanDistancePointSetToPointSetMetricv4
 * \brief Computes the Euclidan distance metric between two point sets.
 *
 *  Given two point sets the Euclidean distance metric (i.e. ICP) is
 *  defined to be the aggregate of all shortest distances between all
 *  possible pairings of points between the two sets.
 *
 *  We only have to handle the individual point case as the parent
 *  class handles the aggregation.
 *
 *  Reference:
 *    PJ Besl and ND McKay, "A Method for Registration of 3-D Shapes",
 *    IEEE PAMI, Vol 14, No. 2, February 1992
 *
 * \ingroup ITKMetricsv4
 */
template<typename TFixedPointSet, typename TMovingPointSet = TFixedPointSet,
  class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT EuclideanDistancePointSetToPointSetMetricv4:
  public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:

  /** Standard class typedefs. */
  typedef EuclideanDistancePointSetToPointSetMetricv4                  Self;
  typedef PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet,
    TInternalComputationValueType>                                     Superclass;
  typedef SmartPointer<Self>                                           Pointer;
  typedef SmartPointer<const Self>                                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( EuclideanDistancePointSetToPointSetMetricv4, PointSetToPointSetMetricv4 );

  /** Types transferred from the base class */
  typedef typename Superclass::MeasureType          MeasureType;
  typedef typename Superclass::DerivativeType       DerivativeType;
  typedef typename Superclass::LocalDerivativeType  LocalDerivativeType;
  typedef typename Superclass::PointType            PointType;
  typedef typename Superclass::PixelType            PixelType;
  typedef typename Superclass::PointIdentifier      PointIdentifier;

  /**
   * Calculates the local metric value for a single point.
   */
  virtual MeasureType GetLocalNeighborhoodValue( const PointType &, const PixelType & pixel = 0 ) const ITK_OVERRIDE;

  /**
   * Calculates the local value and derivative for a single point.
   */
  virtual void GetLocalNeighborhoodValueAndDerivative( const PointType &,
    MeasureType &, LocalDerivativeType &, const PixelType & pixel = 0 ) const ITK_OVERRIDE;

protected:
  EuclideanDistancePointSetToPointSetMetricv4();
  virtual ~EuclideanDistancePointSetToPointSetMetricv4() ITK_OVERRIDE;

  /** PrintSelf function */
  void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EuclideanDistancePointSetToPointSetMetricv4);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanDistancePointSetToPointSetMetricv4.hxx"
#endif

#endif
