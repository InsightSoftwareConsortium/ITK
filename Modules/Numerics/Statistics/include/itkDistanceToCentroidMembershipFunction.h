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
#ifndef __itkDistanceToCentroidMembershipFunction_h
#define __itkDistanceToCentroidMembershipFunction_h

#include "itkMembershipFunctionBase.h"
#include "itkDistanceMetric.h"

namespace itk
{
namespace Statistics
{
/** \class DistanceToCentroidMembershipFunction
 * \brief class represents DistanceToCentroid Density Function.
 *
 * This class keeps parameter to define DistanceToCentroid Density Function  and has
 * method to return the probability density
 * of an instance.  MeasurementVectorSize is the dimension of measurement
 * space.
 * double is type of measurement.
 * \ingroup ITK-Statistics
 */
template< class TVector >
class ITK_EXPORT DistanceToCentroidMembershipFunction:
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

  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  typedef typename Superclass::MeasurementVectorSizeType
  MeasurementVectorSizeType;

  /**  Set the length of each measurement vector. */
  virtual void SetMeasurementVectorSize(MeasurementVectorSizeType);

  /** Type of the DistanceMetric to use */
  typedef DistanceMetric< MeasurementVectorType > DistanceMetricType;
  typedef typename DistanceMetricType::Pointer    DistanceMetricPointer;

  /** Set the DistanceMetric to be used when calling the Evaluate() method */
  itkSetObjectMacro(DistanceMetric, DistanceMetricType);
  itkGetConstObjectMacro(DistanceMetric, DistanceMetricType);

  typedef typename DistanceMetricType::OriginType CentroidType;

  /** Method to set mean */
  void SetCentroid(const CentroidType & centroid);

  /** Method to get mean */
  const CentroidType & GetCentroid() const;

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType & measurement) const;

  /** Return a copy of the current membership function */
  Pointer Clone();

protected:
  DistanceToCentroidMembershipFunction(void);
  virtual ~DistanceToCentroidMembershipFunction(void) {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:

  DistanceMetricPointer m_DistanceMetric;
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDistanceToCentroidMembershipFunction.txx"
#endif

#endif
