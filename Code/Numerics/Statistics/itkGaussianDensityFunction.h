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

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_determinant.h>


#include "itkDensityFunction.h"

#define PI 3.141592

namespace itk{ 
  namespace Statistics{

/** \class GaussianDensityFunction
 * \brief GaussianDensityFunction class represents Gaussian Density Function.
 *
 * This class keeps parameter to define Gaussian Density Function  and has
 * method to return the probability density 
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement. 
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
  enum{ VectorDimension = MeasurementVectorType::Length } ;

  /** Method to set mean */
  void SetMean(vnl_vector<double> mean) ;
  
  /** Method to get mean */
  vnl_vector<double> GetMean() ;

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of 
   * Gaussian Distribution to speed up GetProbability */
  void SetCovariance(vnl_matrix<double> cov); 
  
  /** Method to get covariance matrix */
  vnl_matrix<double> GetCovariance() ;
    
  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(MeasurementVectorType &measurement);
  
protected:
  GaussianDensityFunction(void) ;
  virtual ~GaussianDensityFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  typedef vnl_matrix_fixed< double, 1, VectorDimension > ColumnVectorType ;

  vnl_vector< double >  m_Mean;              // mean
  vnl_matrix< double >  m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()
  vnl_matrix< double >  m_InverseCovariance; 

  // pre_factor which is automatically calculated 
  // when covariace matirx is set.  This speeds up the GetProbability()  
  double m_PreFactor;
  
  unsigned int m_VectorSize ;

  ColumnVectorType m_TempDifference ;
  vnl_vector< double > m_TempVector ;
  vnl_vector< double > m_TempVector2 ;
  ColumnVectorType m_TempExponential ;
};

  } // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDensityFunction.txx"
#endif

#endif
