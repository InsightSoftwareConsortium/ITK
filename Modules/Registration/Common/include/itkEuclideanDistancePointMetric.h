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
#ifndef itkEuclideanDistancePointMetric_h
#define itkEuclideanDistancePointMetric_h

#include "itkPointSetToPointSetMetric.h"
#include "itkCovariantVector.h"
#include "itkPointSet.h"
#include "itkImage.h"

namespace itk
{
/** \class EuclideanDistancePointMetric
 * \brief Computes the minimum distance between a moving point-set
 *  and a fixed point-set. A vector of minimum closest point distance is
 *  created for each point in the moving point-set.
 *  No correspondance is needed.
 *  For speed consideration, the point-set with the minimum number of points
 *  should be used as the moving point-set.
 *  If the number of points is high, the possibility of setting a distance map
 *  should improve the speed of the closest point computation.
 *
 *  Reference: "A Method for Registration of 3-D Shapes",
 *             IEEE PAMI, Vol 14, No. 2, February 1992
 *
 * \ingroup RegistrationMetrics
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedPointSet, typename TMovingPointSet,
          typename TDistanceMap =
            ::itk::Image< unsigned short, TMovingPointSet::PointDimension > >
class ITK_TEMPLATE_EXPORT EuclideanDistancePointMetric:
  public PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet >
{
public:

  /** Standard class typedefs. */
  typedef EuclideanDistancePointMetric                                Self;
  typedef PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistancePointMetric, Object);

  /** Types transferred from the base class. */
  typedef typename Superclass::TransformType           TransformType;
  typedef typename Superclass::TransformPointer        TransformPointer;
  typedef typename Superclass::TransformParametersType TransformParametersType;
  typedef typename Superclass::TransformJacobianType   TransformJacobianType;

  typedef typename Superclass::MeasureType                MeasureType;
  typedef typename Superclass::DerivativeType             DerivativeType;
  typedef typename Superclass::FixedPointSetType          FixedPointSetType;
  typedef typename Superclass::MovingPointSetType         MovingPointSetType;
  typedef typename Superclass::FixedPointSetConstPointer  FixedPointSetConstPointer;
  typedef typename Superclass::MovingPointSetConstPointer MovingPointSetConstPointer;

  typedef typename Superclass::FixedPointIterator     FixedPointIterator;
  typedef typename Superclass::FixedPointDataIterator FixedPointDataIterator;

  typedef typename Superclass::MovingPointIterator     MovingPointIterator;
  typedef typename Superclass::MovingPointDataIterator MovingPointDataIterator;

  typedef TDistanceMap                           DistanceMapType;
  typedef typename DistanceMapType::ConstPointer DistanceMapPointer;

  /** Get the number of values, i.e. the number of points in the moving set. */
  unsigned int GetNumberOfValues() const ITK_OVERRIDE;

  /** Get the derivatives of the match measure. */
  void GetDerivative(const TransformParametersType & parameters,
                     DerivativeType & Derivative) const ITK_OVERRIDE;

  /**  Get the match measure, i.e. the value for single valued optimizers. */
  MeasureType GetValue(const TransformParametersType & parameters) const ITK_OVERRIDE;

  /**  Get value and derivatives for multiple valued optimizers. */
  void GetValueAndDerivative(const TransformParametersType & parameters,
                             MeasureType & Value, DerivativeType & Derivative) const;

  /** Set/Get the distance map. */
  itkSetConstObjectMacro(DistanceMap, DistanceMapType);
  itkGetConstObjectMacro(DistanceMap, DistanceMapType);

  /** Set/Get if the distance should be squared.
   *  When set to true, the filter's computational speed is substantially improved
   *  (by avoiding numerous sqrt() calls), but it will result in minimizing the sum
   *  of distances^4 instead of the sum of distances^2. Default is false. */
  itkSetMacro(ComputeSquaredDistance, bool);
  itkGetConstMacro(ComputeSquaredDistance, bool);
  itkBooleanMacro(ComputeSquaredDistance);

protected:
  EuclideanDistancePointMetric();
  virtual ~EuclideanDistancePointMetric() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EuclideanDistancePointMetric);

  DistanceMapPointer m_DistanceMap;
  bool               m_ComputeSquaredDistance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuclideanDistancePointMetric.hxx"
#endif

#endif
