/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatGaussianDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkStatGaussianDensityFunction_h
#define __itkStatGaussianDensityFunction_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_transpose.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_determinant.h>

#include "itkSmartPointer.h"
#include "itkStatDensityFunction.h"



#define PI 3.141592

namespace itk{

/** \class GaussianDensityFunction
 * \brief GaussianDensityFunction class represents Gaussian Density Function.
 *
 * This class keeps parameter to define Gaussian Density Function  and has
 * method to return the probability (the value of Gaussian Density Function) 
 * of an instance.  FeatureDimension is the dimension of feature space.
 * TFeature is type of feature. (e.g. For RGB data, TFeature can be
 * 3-dimensional vector.  For black and white image, TFeture can be scalar)
 */

template <unsigned int FeatureDimension, class TFeature = double>
class ITK_EXPORT GaussianDensityFunction 
: public DensityFunction<FeatureDimension, TFeature>
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef GaussianDensityFunction Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;

 /**
  * Standard Superclass typedef
  */
  typedef DensityFunction<FeatureDimension, TFeature> Superclass;

 /**
  * Dimension of the feature vector
  */
  enum { Dimension = FeatureDimension };

 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information
  */
  itkTypeMacro(GaussianDensityFunction,DensityFunction);

 /**
  * Method to get mean
  */
  vnl_vector<TFeature> GetMeans() 
  { return m_Means; };

 /**
  * Method to set mean
  */
  void SetMeans(vnl_vector<TFeature> means) 
  { m_Means = means; };

 /**
  * Method to get standard deviation
  */
  vnl_vector<TFeature> GetStandardDeviations() 
  { return m_StandardDeviations; };

 /**
  * Method to set standard deviation
  */
  void SetStandardDeviations(vnl_vector<TFeature> std) 
  { m_StandardDeviations = std; };

 /**
  * Method to get covariance matrix
  */
  vnl_matrix<TFeature> GetCovarianceMatrix() 
  { return m_Covariance; };
 
 /**
  * Method to set covariance matrix
  * Also, this function calculates inverse covariance and pre factor of 
  * Gaussian Distribution to speed up GetProbability
  */
  void SetCovariance(vnl_matrix<TFeature> cov) 
  { 
    m_Covariance = cov; 

    // allocate the memory for m_InverseCovariance matrix   
    m_InverseCovariance.resize(Dimension, Dimension);
    m_InverseCovariance = vnl_matrix_inverse<TFeature>(m_Covariance);
   
    // the determinant of the covaraince matrix
    double det = vnl_determinant(m_Covariance);

    // calculate coefficient C of multivariate gaussian
    // p(x) = C exp(-0.5 * (x-u) * inv(covariance) * (x-u)')
    m_PreFactor = 1.0/pow( pow(2.0*PI, Dimension), 1/2.0)*sqrt(fabs(det));
  };

 /**
  * Method to get probability of an instance. The return value is the
  * value of the density function, not probability.
  */
  double GetProbability(vnl_vector<TFeature> feature)
  { 
    vnl_matrix<TFeature> xMatrix(Dimension,1);

    for ( int i=0; i < Dimension; i++)
      {
      xMatrix.put(i,0, feature[i] - m_Means[i]);
      }

    vnl_matrix<TFeature> exponentMatrix(1,1);
    exponentMatrix = vnl_transpose(xMatrix)*m_InverseCovariance*xMatrix;
    
    return m_PreFactor * exp( -0.5*exponentMatrix.get(0,0) );
  }

protected:		
  GaussianDensityFunction(void){};
  ~GaussianDensityFunction(void){};

  vnl_vector<TFeature> m_Means;              // mean
  vnl_vector<TFeature> m_StandardDeviations; // standard deviation
  vnl_matrix<TFeature> m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()
  vnl_matrix<TFeature> m_InverseCovariance; 

  // pre_factor which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()  
  double m_PreFactor;


};

} // end namespace itk



#endif
