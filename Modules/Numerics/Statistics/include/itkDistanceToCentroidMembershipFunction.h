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
#ifndef itkDistanceToCentroidMembershipFunction_h
#define itkDistanceToCentroidMembershipFunction_h

#include "itkMembershipFunctionBase.h"
#include "itkDistanceMetric.h"

namespace itk
{
namespace Statistics
{
/** \class DistanceToCentroidMembershipFunction
 * \brief DistanceToCentroidMembershipFunction models class membership
 * using a distance metric.
 *
 * DistanceToCentroidMembershipFunction is a subclass of
 * MembershipFunctionBase that models class membership using the
 * distance to the centroid of the class. A choice of distance metric
 * can be specified using the SetDistanceMetric() method. Options
 * include EuclideanDistanceMetric, EuclideanSquaredDistanceMetric,
 * and ManhattenDistanceMetric. The centroid of the class is specified
 * using the SetCentroid() method. Any other parameters to control the
 * distance function evaluation have to be set directly on the
 * distance function itself.
 *
 * \ingroup ITKStatistics
 */
template< typename TVector >
class ITK_TEMPLATE_EXPORT DistanceToCentroidMembershipFunction:
  public MembershipFunctionBase< TVector >
{
public:
  /** Standard class typedefs */
  typedef DistanceToCentroidMembershipFunction Self;
  typedef MembershipFunctionBase< TVector >    Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Strandard macros */
  itkTypeMacro(DistanceToCentroidMembershipFunction,
               MembershipFunctionBase);
  itkNewMacro(Self);

  /** SmartPointer class for superclass */
  typedef typename Superclass::Pointer MembershipFunctionPointer;

  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  typedef typename Superclass::MeasurementVectorSizeType
  MeasurementVectorSizeType;

  /**  Set the length of each measurement vector. */
  virtual void SetMeasurementVectorSize(MeasurementVectorSizeType) ITK_OVERRIDE;

  /** Type of the DistanceMetric to use */
  typedef DistanceMetric< MeasurementVectorType > DistanceMetricType;
  typedef typename DistanceMetricType::Pointer    DistanceMetricPointer;
  typedef typename DistanceMetricType::OriginType CentroidType;

  /** Set the DistanceMetric to be used when calling the Evaluate() method */
  itkSetObjectMacro(DistanceMetric, DistanceMetricType);

  /** Get the DistanceMetric used by the MembershipFunction */
  itkGetModifiableObjectMacro(DistanceMetric, DistanceMetricType);

  /** Get the DistanceMetric used by the MembershipFunction. This is
   * a non-const version that allows you to configure the distance
   * function directly. */

  /** Set the centroid of the class (propagated to the DistanceMetric) */
  void SetCentroid(const CentroidType & centroid);

  /** Get the centroid of the class (requested from the DistanceMetric */
  const CentroidType & GetCentroid() const;

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType & measurement) const ITK_OVERRIDE;

protected:
  DistanceToCentroidMembershipFunction();
  virtual ~DistanceToCentroidMembershipFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Return a copy of the current membership function */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DistanceToCentroidMembershipFunction);

  DistanceMetricPointer m_DistanceMetric;
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceToCentroidMembershipFunction.hxx"
#endif

#endif
