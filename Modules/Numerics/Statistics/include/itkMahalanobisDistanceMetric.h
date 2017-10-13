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
#ifndef itkMahalanobisDistanceMetric_h
#define itkMahalanobisDistanceMetric_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_vector_ref.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/algo/vnl_determinant.h"
#include "itkArray.h"

#include "itkDistanceMetric.h"

namespace itk
{
namespace Statistics
{
/** \class MahalanobisDistanceMetric
 * \brief MahalanobisDistanceMetric class computes a Mahalanobis
 *  distance given a mean and covariance.
 *
 * \sa DistanceMetric
 * \sa EuclideanDistanceMetric
 * \sa EuclideanSquareDistanceMetric
 * \ingroup ITKStatistics
 */

template< typename TVector >
class ITK_TEMPLATE_EXPORT MahalanobisDistanceMetric:
  public DistanceMetric< TVector >
{
public:
  /** Standard class typedefs */
  typedef MahalanobisDistanceMetric  Self;
  typedef DistanceMetric< TVector >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Strandard macros */
  itkTypeMacro(MahalanobisDistanceMetric, DistanceMetric);
  itkNewMacro(Self);

  /** Typedef to represent the measurement vector type */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Type used for representing the mean vector */
  typedef typename Superclass::OriginType MeanVectorType;

  /** Type used for representing the covariance matrix */
  typedef vnl_matrix< double > CovarianceMatrixType;

  /**  Set the length of each measurement vector. */
  virtual void SetMeasurementVectorSize(MeasurementVectorSizeType) ITK_OVERRIDE;

  /** Method to set mean */
  void SetMean(const MeanVectorType & mean);

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
   * Method to set inverse covariance matrix */
  void SetInverseCovariance(const CovarianceMatrixType & invcov);

  /** Method to get covariance matrix */
  itkGetConstReferenceMacro(InverseCovariance, CovarianceMatrixType);

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType & measurement) const ITK_OVERRIDE;

  /** Gets the distance between x1 and x2. */
  double Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const ITK_OVERRIDE;

  /** Set/Get tolerance values */
  itkSetMacro(Epsilon, double);
  itkGetConstMacro(Epsilon, double);

  itkSetMacro(DoubleMax, double);
  itkGetConstMacro(DoubleMax, double);

protected:
  MahalanobisDistanceMetric();
  virtual ~MahalanobisDistanceMetric(void) ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  MeanVectorType       m_Mean;               // mean
  CovarianceMatrixType m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated
  // when covariace matirx is set.  This speed up the GetProbability()
  CovarianceMatrixType m_InverseCovariance;

  double m_Epsilon;
  double m_DoubleMax;

  void CalculateInverseCovariance();
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceMetric.hxx"
#endif

#endif
