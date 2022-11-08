/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
template <typename TFixedPointSet,
          typename TMovingPointSet = TFixedPointSet,
          class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT EuclideanDistancePointSetToPointSetMetricv4
  : public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(EuclideanDistancePointSetToPointSetMetricv4);

  /** Standard class type aliases. */
  using Self = EuclideanDistancePointSetToPointSetMetricv4;
  using Superclass = PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistancePointSetToPointSetMetricv4, PointSetToPointSetMetricv4);

  /** Types transferred from the base class */
  using typename Superclass::MeasureType;
  using typename Superclass::DerivativeType;
  using typename Superclass::LocalDerivativeType;
  using typename Superclass::PointType;
  using typename Superclass::PixelType;
  using typename Superclass::PointIdentifier;

  using RealType = MeasureType;
  /**
   * Distance threshold to be used to calculate the metric value.
   * Only point pairs that have distance lesser than this threshold
   * contribute to the metric. Default is -1 to include all the pairs.
   */
  itkSetMacro(DistanceThreshold, RealType);

  /**
   * Get the Distance threshold to be used to calculate the metric value
   * Default = -1.
   */
  itkGetConstMacro(DistanceThreshold, RealType);

  /**
   * Calculates the local metric value for a single point.
   */
  MeasureType
  GetLocalNeighborhoodValue(const PointType &, const PixelType & pixel = 0) const override;

  /**
   * Calculates the local value and derivative for a single point.
   */
  void
  GetLocalNeighborhoodValueAndDerivative(const PointType &,
                                         MeasureType &,
                                         LocalDerivativeType &,
                                         const PixelType & pixel = 0) const override;

protected:
  EuclideanDistancePointSetToPointSetMetricv4() = default;
  ~EuclideanDistancePointSetToPointSetMetricv4() override = default;

  bool
  RequiresFixedPointsLocator() const override
  {
    return false;
  }

  /** PrintSelf function */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  RealType m_DistanceThreshold = -1.0;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEuclideanDistancePointSetToPointSetMetricv4.hxx"
#endif

#endif
