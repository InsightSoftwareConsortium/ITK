/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGaussianDensityFunction.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianDensityFunction_h
#define __itkGaussianDensityFunction_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/algo/vnl_determinant.h"
#include "vnl/vnl_math.h"

#include "itkMatrix.h"
#include "itkDensityFunction.h"

namespace itk{ 
namespace Statistics{

/** \class GaussianDensityFunction
 * \brief GaussianDensityFunction class represents Gaussian Density Function.
 *
 * This class keeps parameter to define Gaussian Density Function  and has
 * method to return the probability density 
 * of an instance (pattern) .  
 * If the all element of the covariance matrix is zero the "usual" density 
 * calculations ignored. if the measurement vector to be evaluated is equal to
 * the mean, then the Evaluate method will return maximum value of double and return 0 
 * for others 
 *
 */

template< class TMeasurementVector >
class ITK_EXPORT GaussianDensityFunction :
    public DensityFunction< TMeasurementVector >
{
public:
  /** Standard class typedefs */
  typedef GaussianDensityFunction Self;
  typedef DensityFunction< TMeasurementVector > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(GaussianDensityFunction, DensityFunction);
  itkNewMacro(Self);
  
  /** Typedef alias for the measurement vectors */
  typedef TMeasurementVector MeasurementVectorType ;

  /** Dimension of the each individual pixel vector. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      TMeasurementVector::Length);

  typedef Vector< double, itkGetStaticConstMacro(VectorDimension) > MeanType ;
  typedef Matrix< double, itkGetStaticConstMacro(VectorDimension), itkGetStaticConstMacro(VectorDimension) > CovarianceType ;

  /** Method to set mean */
  void SetMean(MeanType* mean)
  {
    if ( m_Mean != mean) 
      {
        m_Mean = mean ;
        this->Modified() ;
      }
  }
  
  /** Method to get mean */
  MeanType* GetMean() const
  { return m_Mean ; }

  /** Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of 
   * Gaussian Distribution to speed up GetProbability */
  void SetCovariance(CovarianceType* cov); 
  
  /** Method to get covariance matrix */
  CovarianceType* GetCovariance() const ;

  /**y Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType &measurement) const ;
  
protected:
  GaussianDensityFunction(void) ;
  virtual ~GaussianDensityFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  MeanType* m_Mean;              // mean
  CovarianceType* m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()
  CovarianceType  m_InverseCovariance; 

  // pre_factor which is automatically calculated 
  // when covariace matirx is set.  This speeds up the GetProbability()  
  double m_PreFactor;

  /** if the all element of the given covarinace is zero, then this
   * value set to true */
  bool m_IsCovarianceZero ;
};

} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDensityFunction.txx"
#endif

#endif
