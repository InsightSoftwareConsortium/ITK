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
#ifndef __itkMahalanobisDistanceMembershipFunction_h
#define __itkMahalanobisDistanceMembershipFunction_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_ref.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/algo/vnl_determinant.h"
#include "itkArray.h"

#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class MahalanobisDistanceMembershipFunction
 * \brief MahalanobisDistanceMembershipFunction class represents MahalanobisDistance Density Function.
 *
 * This class keeps parameter to define MahalanobisDistance Density Function  and has
 * method to return the probability density
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement.
 * \ingroup ITK-Statistics
 */

template< class TVector >
class ITK_EXPORT MahalanobisDistanceMembershipFunction:
  public MembershipFunctionBase< TVector >
{
public:
  /** Standard class typedefs */
  typedef MahalanobisDistanceMembershipFunction Self;
  typedef MembershipFunctionBase< TVector >     Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Strandard macros */
  itkTypeMacro(MahalanobisDistanceMembershipFunction, MembershipFunctionBase);
  itkNewMacro(Self);

  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Type used for representing the mean vector */
  typedef vnl_vector< double > MeanVectorType;

  /** Type used for representing the covariance matrix */
  typedef vnl_matrix< double > CovarianceMatrixType;

  /**  Set the length of each measurement vector. */
  virtual void SetMeasurementVectorSize(MeasurementVectorSizeType);

  /** Method to set mean */
  void SetMean(const MeanVectorType & mean);

  void SetMean(const Array< double > & mean);

  /** Method to get mean */
  const MeanVectorType & GetMean() const;

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of
   * MahalanobisDistance Distribution to speed up GetProbability */
  void SetCovariance(const CovarianceMatrixType & cov);

  /** Method to get covariance matrix */
  itkGetConstReferenceMacro(Covariance, CovarianceMatrixType);

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of
   * MahalanobisDistance Distribution to speed up GetProbability */
  void SetInverseCovariance(const CovarianceMatrixType & invcov);

  /** Method to get covariance matrix */
  itkGetConstReferenceMacro(InverseCovariance, CovarianceMatrixType);

  /** Method to set the number of samples */
  itkSetMacro(NumberOfSamples, double);

  /** Method to get the number of samples */
  itkGetConstMacro(NumberOfSamples, double);

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType & measurement) const;

protected:
  MahalanobisDistanceMembershipFunction(void);
  virtual ~MahalanobisDistanceMembershipFunction(void) {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  MeanVectorType       m_Mean;               // mean
  CovarianceMatrixType m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated
  // when covariace matirx is set.  This speed up the GetProbability()
  CovarianceMatrixType m_InverseCovariance;

  // Number of samples defining this density
  double m_NumberOfSamples;
  // pre_factor which is automatically calculated
  // when covariace matirx is set.  This speeds up the GetProbability()
  double m_PreFactor;
  double m_Epsilon;
  double m_DoubleMax;

  mutable vnl_matrix< double > m_TempVec;
  mutable vnl_matrix< double > m_TempMat;

  void CalculateInverseCovariance();
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceMembershipFunction.txx"
#endif

#endif
