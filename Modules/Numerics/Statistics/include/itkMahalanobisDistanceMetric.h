/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/**
 *\class MahalanobisDistanceMetric
 * \brief MahalanobisDistanceMetric class computes a Mahalanobis
 *  distance given a mean and covariance.
 *
 * \sa DistanceMetric
 * \sa EuclideanDistanceMetric
 * \sa EuclideanSquareDistanceMetric
 * \ingroup ITKStatistics
 */

template <typename TVector>
class ITK_TEMPLATE_EXPORT MahalanobisDistanceMetric : public DistanceMetric<TVector>
{
public:
  /** Standard class type aliases */
  using Self = MahalanobisDistanceMetric;
  using Superclass = DistanceMetric<TVector>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Strandard macros */
  itkTypeMacro(MahalanobisDistanceMetric, DistanceMetric);
  itkNewMacro(Self);

  /** Typedef to represent the measurement vector type */
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;

  /** Type used for representing the mean vector */
  using MeanVectorType = typename Superclass::OriginType;

  /** Type used for representing the covariance matrix */
  using CovarianceMatrixType = vnl_matrix<double>;

  /**  Set the length of each measurement vector. */
  void SetMeasurementVectorSize(MeasurementVectorSizeType) override;

  /** Method to set mean */
  void
  SetMean(const MeanVectorType & mean);

  /** Method to get mean */
  const MeanVectorType &
  GetMean() const;

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of
   * MahalanobisDistance Distribution to speed up GetProbability */
  void
  SetCovariance(const CovarianceMatrixType & cov);

  /** Method to get covariance matrix */
  itkGetConstReferenceMacro(Covariance, CovarianceMatrixType);

  /**
   * Method to set inverse covariance matrix */
  void
  SetInverseCovariance(const CovarianceMatrixType & invcov);

  /** Method to get covariance matrix */
  itkGetConstReferenceMacro(InverseCovariance, CovarianceMatrixType);

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double
  Evaluate(const MeasurementVectorType & measurement) const override;

  /** Gets the distance between x1 and x2. */
  double
  Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const override;

  /** Set/Get tolerance values */
  itkSetMacro(Epsilon, double);
  itkGetConstMacro(Epsilon, double);

  itkSetMacro(DoubleMax, double);
  itkGetConstMacro(DoubleMax, double);

protected:
  MahalanobisDistanceMetric();
  ~MahalanobisDistanceMetric() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  MeanVectorType       m_Mean;       // mean
  CovarianceMatrixType m_Covariance; // covariance matrix

  // inverse covariance matrix which is automatically calculated
  // when covariance matirx is set.  This speed up the GetProbability()
  CovarianceMatrixType m_InverseCovariance;

  double m_Epsilon{ 1e-100 };
  double m_DoubleMax{ 1e+20 };

  void
  CalculateInverseCovariance();
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMahalanobisDistanceMetric.hxx"
#endif

#endif
