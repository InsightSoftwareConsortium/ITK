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
 * itkKalmanFilter is the templated class. The class is templated over the
 * type of the parameters to be estimated and over the number of this
 * parameters
 */

#ifndef __itkKalmanFilter_h
#define __itkKalmanFilter_h

#include <vnl/vnl_vector_fixed.h>
#include <vnl/vnl_matrix_fixed.h>

template <class T, unsigned int TEstimatorDimension>
class itkKalmanFilter 
{
public:

  enum { Dimension = TEstimatorDimension };
  typedef vnl_vector_fixed<T,TEstimatorDimension> Vector;
  typedef vnl_matrix_fixed<T,TEstimatorDimension,TEstimatorDimension> Matrix;
  typedef T Type;

  void updateWithNewMeasure(  const T & newMeasure,
                              const Vector & newPredictor );

  void clearEstimation(void) 
  {
    Estimator = 0;
  }


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


  void setVariance(const Matrix & m)
  {
      Variance = m;
  }


  
  const Vector & getEstimator(void) const
  {
    return Estimator;
  }


  const Matrix & getVariance(void) const
  {
    return Variance;
  }
  

private:  
  void updateVariance( const Vector & );
  Vector Estimator;
  Matrix Variance;


};




template <class T, unsigned int TEstimatorDimension>
void itkKalmanFilter<T,TEstimatorDimension>::updateWithNewMeasure(
                                            const T & newMeasure,
                                            const Vector & newPredictor )
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






template <class T, unsigned int TEstimatorDimension>
void itkKalmanFilter<T,TEstimatorDimension>::updateVariance( const Vector & newPredictor )
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





#endif



