/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKalmanFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * KalmanFilter class implements a linear recursive estimator. 
 * The class is templated over the type of the parameters to be estimated 
 * and over the number of parameters. Recursive estimation is a fast mechanism
 * for getting information about a system for which we only have access to
 * measures that are linearly related with the parameters we want to estimate.
 */

#ifndef __itkKalmanFilter_h
#define __itkKalmanFilter_h

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

namespace itk
{

template <class T, unsigned int TEstimatorDimension>
class KalmanFilter 
{
public:

  /**
   *  Dimension of the vector of parameters to be estimated.
   *  It is equivalent to the number of parameters to estimate.
   */  
  enum { Dimension = TEstimatorDimension };

  /**
   *  Vector type defines a generic vector type that is used
   *  for the matricial operations performed during estimation.
   */
  typedef vnl_vector_fixed<T,TEstimatorDimension> Vector;

  /**
   *  Matrix type defines a generic matrix type that is used
   *  for the matricial operations performed during estimation.
   */
  typedef vnl_matrix_fixed<T,TEstimatorDimension,TEstimatorDimension> Matrix;

  /**
   * Type is the type associated with the parameters to be estimated.
   * All the parameters are of the same type. Natural choices could be
   * floats and doubles, because Type also is used for all the internal
   * computations.
   */
  typedef T Type;

  /**
   * Update the estimation using the information provided by a new measure
   * along with a new line of the linear predictor. This method is the one
   * that should be called iteratively in order to estimate the parameter's
   * vector. It internally updates the covariance matrix.
   */
  void updateWithNewMeasure(  const T & newMeasure,
                              const Vector & newPredictor );

  /**
   * This method resets the estimator. It set all the parameters to null.
   * The covariance matrix is not changed.
   * \sa Estimator
   * \sa Variance
   * \sa clearVariance
   */
  void clearEstimation(void) 
    {
      Estimator = 0;
    }


  /**
   * This method resets the covariance matrix. It is set to an identity matrix
   * \sa Estimator
   * \sa Variance
   * \sa clearEstimation
   */
  void clearVariance(void)
    {
      const unsigned int N = TEstimatorDimension * TEstimatorDimension;
      for(unsigned int i=0; i<N; i++) 
	{
	Variance(i) = 0.0;
	}

      for(unsigned int j=0; j<N; j++) 
	{
	Variance(j,j) = 1.0;
	}

    }


  /**
   * This method sets the covariance matrix to a diagonal matrix with
   * equal values. It is useful when the variance of all the parameters
   * be estimated are the same and the parameters are considered independents.
   * \sa Estimator
   * \sa Variance
   * \sa clearEstimation
   */
  void setVariance(const T & var = 1.0) 
    {
      const unsigned int N = TEstimatorDimension * TEstimatorDimension;
      for(unsigned int i=0; i<N; i++) 
	{
	Variance(i) = 0.0;
	}

      for(unsigned int j=0; j<N; j++) 
	{
	Variance(j,j) = var;
	}

    }


  /**
   * This method sets the covariance matrix to known matrix. It is intended to
   * initialize the estimator with a priori information about the statistical
   * distribution of the parameters.  It can also be used to resume the
   * operation of a previously used estimator using it last known state.
   * \sa Estimator
   * \sa Variance
   * \sa clearEstimation
   */
  void setVariance(const Matrix & m)
    {
      Variance = m;
    }


  
  /**
   * This method returns the vector of estimated parameters
   * \sa Estimator
   */ 
  const Vector & getEstimator(void) const
    {
      return Estimator;
    }


  /**
   * This method returns the covariance matrix of the estimated parameters
   * \sa Variance
   */
  const Matrix & getVariance(void) const
    {
      return Variance;
    }
  


private:  

  /**
   * This methods performs the update of the parameter's covariance matrix.
   * It is called by updateWithNewMeasure() method. Users are not expected to
   * call this method directly.
   * \sa updateWithNewMeasure
   */
  void updateVariance( const Vector & );

  /**
   * Vector of parameters to estimate.
   * \sa getEstimator
   */
  Vector Estimator;

  /**
   * Estimation of the parameter's covariance matrix. This matrix contains the
   * information about the estate of the estimator. It holds all the
   * information obtained from previous measures provided to the estimator. The
   * initialization of this matrix is critical to the behavior of the
   * estimator, at least to ensure a short trasient period for estabilizing the
   * estimation.
   * \sa setVariance
   * \sa getVariance
   */ 
  Matrix Variance;
};


/**
 *
 */
template <class T, unsigned int TEstimatorDimension>
void
KalmanFilter<T,TEstimatorDimension>
::updateWithNewMeasure(const T & newMeasure, const Vector & newPredictor )
{
  T measurePrediction      = dot_product( newPredictor , Estimator );
  
  T errorMeasurePrediction = newMeasure - measurePrediction;

  Vector Corrector = Variance * newPredictor;

  for( unsigned int j=0; j<TEstimatorDimension; j++) 
    {
    Estimator(j) += Corrector(j) * errorMeasurePrediction;
    }
  
  updateVariance( newPredictor );
}


/**
 *
 */
template <class T, unsigned int TEstimatorDimension>
void
KalmanFilter<T,TEstimatorDimension>
::updateVariance( const Vector & newPredictor )
{  
  Vector aux =  Variance * newPredictor;

  T denominator = 1.0/(1.0 +  dot_product( aux , newPredictor ) );

  unsigned pos = 0;
  for( unsigned int col=0; col<TEstimatorDimension; col++) 
    {
    for( unsigned int row=0; row<TEstimatorDimension; row++) 
      {
      Variance(pos) -= aux(col)*aux(row)*denominator;
      pos++;
      }
    }
}

} // namespace itk

#endif
