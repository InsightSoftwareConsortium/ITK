/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
