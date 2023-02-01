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
#ifndef itkPointToPlanePointSetToPointSetMetricv4_h
#define itkPointToPlanePointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"

namespace itk
{
/** \class PointToPlanePointSetToPointSetMetricv4
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
class ITK_TEMPLATE_EXPORT PointToPlanePointSetToPointSetMetricv4
  : public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointToPlanePointSetToPointSetMetricv4);

  /** Standard class type aliases. */
  using Self = PointToPlanePointSetToPointSetMetricv4;
  using Superclass = PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointToPlanePointSetToPointSetMetricv4, PointSetToPointSetMetricv4);

  /** Types transferred from the base class */
  using typename Superclass::MeasureType;

  /**  Type of the parameters. */
  using typename Superclass::ParametersType;
  using typename Superclass::ParametersValueType;
  using typename Superclass::NumberOfParametersType;

  /**  Type of the derivative. */
  using typename Superclass::DerivativeType;

  /** Transform types from Superclass*/
  using typename Superclass::FixedTransformType;
  using typename Superclass::FixedTransformPointer;
  using typename Superclass::FixedInputPointType;
  using typename Superclass::FixedOutputPointType;
  using typename Superclass::FixedTransformParametersType;

  using typename Superclass::MovingTransformType;
  using typename Superclass::MovingTransformPointer;
  using typename Superclass::MovingInputPointType;
  using typename Superclass::MovingOutputPointType;
  using typename Superclass::MovingTransformParametersType;

  using typename Superclass::JacobianType;
  using typename Superclass::FixedTransformJacobianType;
  using typename Superclass::MovingTransformJacobianType;

  using DisplacementFieldTransformType = typename Superclass::MovingDisplacementFieldTransformType;

  using ObjectType = typename Superclass::ObjectType;

  /** Dimension type */
  using typename Superclass::DimensionType;

  /**  Type of the fixed point set. */
  using FixedPointSetType = TFixedPointSet;
  using FixedPointType = typename TFixedPointSet::PointType;
  using FixedPixelType = typename TFixedPointSet::PixelType;
  using FixedPointsContainer = typename TFixedPointSet::PointsContainer;

  static constexpr DimensionType FixedPointDimension = Superclass::FixedDimension;

  /**  Type of the moving point set. */
  using MovingPointSetType = TMovingPointSet;
  using MovingPointType = typename TMovingPointSet::PointType;
  using MovingPixelType = typename TMovingPointSet::PixelType;
  using MovingPointsContainer = typename TMovingPointSet::PointsContainer;

  static constexpr DimensionType MovingPointDimension = Superclass::MovingDimension;

  /**
   * typedefs for the data types used in the point set metric calculations.
   * It is assumed that the constants of the fixed point set, such as the
   * point dimension, are the same for the "common space" in which the metric
   * calculation occurs.
   */
  static constexpr DimensionType PointDimension = Superclass::FixedDimension;

  using PointType = FixedPointType;
  using PixelType = FixedPixelType;
  using CoordRepType = typename PointType::CoordRepType;
  using PointsContainer = FixedPointsContainer;
  using PointsConstIterator = typename PointsContainer::ConstIterator;
  using PointIdentifier = typename PointsContainer::ElementIdentifier;

  /** Typedef for points locator class to speed up finding neighboring points */
  using PointsLocatorType = PointsLocator<PointsContainer>;
  using NeighborsIdentifierType = typename PointsLocatorType::NeighborsIdentifierType;

  using FixedTransformedPointSetType = PointSet<FixedPixelType, Self::PointDimension>;
  using MovingTransformedPointSetType = PointSet<MovingPixelType, Self::PointDimension>;

  using DerivativeValueType = typename DerivativeType::ValueType;
  using LocalDerivativeType = FixedArray<DerivativeValueType, Self::PointDimension>;

  /** Types for the virtual domain */
  using VirtualImageType = typename Superclass::VirtualImageType;
  using typename Superclass::VirtualImagePointer;
  using typename Superclass::VirtualPixelType;
  using typename Superclass::VirtualRegionType;
  using typename Superclass::VirtualSizeType;
  using typename Superclass::VirtualSpacingType;
  using VirtualOriginType = typename Superclass::VirtualPointType;
  using typename Superclass::VirtualPointType;
  using typename Superclass::VirtualDirectionType;
  using VirtualRadiusType = typename Superclass::VirtualSizeType;
  using typename Superclass::VirtualIndexType;
  using typename Superclass::VirtualPointSetType;
  using typename Superclass::VirtualPointSetPointer;

  // Create ranges over the point set for multithreaded computation of value and derivatives
  // using PointIdentifierPair = std::pair<PointIdentifier, PointIdentifier>;
  // using PointIdentifierRanges = std::vector<PointIdentifierPair>;

  /**
   * Calculates the local metric value for a single point.
   */
  MeasureType
  GetLocalNeighborhoodValue(const PointType &, const PixelType & pixel) const override;

  /**
   * Calculates the local value and derivative for a single point.
   */
  void
  GetLocalNeighborhoodValueAndDerivative(const PointType &,
                                         MeasureType &,
                                         LocalDerivativeType &,
                                         const PixelType & pixel) const override;

  /**
   *  Overide it to handle the change jacobian due to normal vector.
   */
  void
  CalculateValueAndDerivative(MeasureType &    calculatedValue,
                              DerivativeType & derivative,
                              bool             calculateValue) const override;

protected:
  PointToPlanePointSetToPointSetMetricv4();
  ~PointToPlanePointSetToPointSetMetricv4() override = default;

  bool
  RequiresFixedPointsLocator() const override
  {
    return false;
  }

  /** PrintSelf function */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;


private:
  // Create ranges over the point set for multithreaded computation of value and derivatives
  using PointIdentifierPair = std::pair<PointIdentifier, PointIdentifier>;
  using PointIdentifierRanges = std::vector<PointIdentifierPair>;
  const PointIdentifierRanges
  CreateRanges() const;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointToPlanePointSetToPointSetMetricv4.hxx"
#endif

#endif
