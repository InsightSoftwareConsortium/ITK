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
#ifndef itkKalmanLinearEstimator_h
#define itkKalmanLinearEstimator_h

#include "itkMacro.h"

#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"

namespace itk
{
/** \class KalmanLinearEstimator
 * \brief Implement a linear recursive estimator.
 *
 * KalmanLinearEstimator class implements a linear recursive estimator.  The
 * class is templated over the type of the parameters to be estimated and
 * over the number of parameters. Recursive estimation is a fast mechanism
 * for getting information about a system for which we only have access to
 * measures that are linearly related with the parameters we want to
 * estimate.
 *
 * \ingroup Numerics
 * \ingroup ITKStatistics
 */
template< typename T, unsigned int VEstimatorDimension >
class KalmanLinearEstimator
{
public:
  /**  Dimension of the vector of parameters to be estimated.
   *  It is equivalent to the number of parameters to estimate. */
  itkStaticConstMacro(Dimension, unsigned int,
                      VEstimatorDimension);

  /**  Vector type defines a generic vector type that is used
   *  for the matricial operations performed during estimation. */
  typedef vnl_vector_fixed< T, VEstimatorDimension > VectorType;

  /**  Matrix type defines a generic matrix type that is used
   *  for the matricial operations performed during estimation. */
  typedef vnl_matrix_fixed< T, VEstimatorDimension, VEstimatorDimension > MatrixType;

  /** Type is the type associated with the parameters to be estimated.
   * All the parameters are of the same type. Natural choices could be
   * floats and doubles, because Type also is used for all the internal
   * computations. */
  typedef T ValueType;

  /** Update the estimation using the information provided by a new measure
   * along with a new line of the linear predictor. This method is the one
   * that should be called iteratively in order to estimate the parameter's
   * vector. It internally updates the covariance matrix. */
  void UpdateWithNewMeasure(const ValueType & newMeasure,
                            const VectorType & newPredictor);

  /** This method resets the estimator. It set all the parameters to null.
   * The covariance matrix is not changed.
   * \sa Estimator \sa Variance \sa ClearVariance */
  void ClearEstimation(void)
  { m_Estimator = VectorType( T(0) ); }

  /** This method resets the covariance matrix. It is set to an identity matrix
   * \sa Estimator \sa Variance \sa ClearEstimation */
  void ClearVariance(void)
  {
    m_Variance.set_identity();
  }

  /** This method sets the covariance matrix to a diagonal matrix with
   * equal values. It is useful when the variance of all the parameters
   * be estimated are the same and the parameters are considered independents.
   * \sa Estimator
   * \sa Variance
   * \sa ClearEstimation */
  void SetVariance(const ValueType & var = 1.0)
  {
    m_Variance.set_identity();
    m_Variance *= var;
  }

  /** This method sets the covariance matrix to known matrix. It is intended to
   * initialize the estimator with a priori information about the statistical
   * distribution of the parameters.  It can also be used to resume the
   * operation of a previously used estimator using it last known state.
   * \sa Estimator \sa Variance \sa ClearEstimation */
  void SetVariance(const MatrixType & m)
  { m_Variance = m; }

  /** This method returns the vector of estimated parameters
   * \sa Estimator */
  const VectorType & GetEstimator(void) const
  { return m_Estimator; }

  /** This method returns the covariance matrix of the estimated parameters
   * \sa Variance */
  const MatrixType & GetVariance(void) const
  { return m_Variance; }

private:
  /** This methods performs the update of the parameter's covariance matrix.
   * It is called by updateWithNewMeasure() method. Users are not expected to
   * call this method directly.
   * \sa updateWithNewMeasure */
  void UpdateVariance(const VectorType  &);

  /** Vector of parameters to estimate.
   * \sa GetEstimator */
  VectorType m_Estimator;

  /** Estimation of the parameter's covariance matrix. This matrix contains
   * the information about the estate of the estimator. It holds all the
   * information obtained from previous measures provided to the
   * estimator. The initialization of this matrix is critical to the behavior
   * of the estimator, at least to ensure a short trasient period for
   * estabilizing the estimation.  \sa SetVariance \sa GetVariance */
  MatrixType m_Variance;
};

template< typename T, unsigned int VEstimatorDimension >
void
KalmanLinearEstimator< T, VEstimatorDimension >
::UpdateWithNewMeasure(const ValueType  & newMeasure,
                       const VectorType & newPredictor)
{
  ValueType measurePrediction = dot_product(newPredictor, m_Estimator);

  ValueType errorMeasurePrediction = newMeasure - measurePrediction;

  VectorType Corrector = m_Variance * newPredictor;

  for ( unsigned int j = 0; j < VEstimatorDimension; j++ )
    {
    m_Estimator(j) += Corrector(j) * errorMeasurePrediction;
    }

  UpdateVariance(newPredictor);
}

template< typename T, unsigned int VEstimatorDimension >
void
KalmanLinearEstimator< T, VEstimatorDimension >
::UpdateVariance(const VectorType & newPredictor)
{
  VectorType aux =  m_Variance * newPredictor;

  ValueType denominator = 1.0 / ( 1.0 +  dot_product(aux, newPredictor) );

  for ( unsigned int col = 0; col < VEstimatorDimension; col++ )
    {
    for ( unsigned int row = 0; row < VEstimatorDimension; row++ )
      {
      m_Variance(col, row) -= aux(col) * aux(row) * denominator;
      }
    }
}
} // end namespace itk

#endif
