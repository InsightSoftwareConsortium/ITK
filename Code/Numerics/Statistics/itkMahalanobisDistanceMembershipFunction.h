/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMahalanobisDistanceMembershipFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMahalanobisDistanceMembershipFunction_h
#define __itkMahalanobisDistanceMembershipFunction_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_transpose.h>
#include <vnl/vnl_matrix.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_determinant.h>


#include "itkMembershipFunctionBase.h"

#define PI 3.141592

namespace itk{ 
  namespace Statistics{

/** \class MahalanobisDistanceMembershipFunction
 * \brief MahalanobisDistanceMembershipFunction class represents MahalanobisDistance Density Function.
 *
 * This class keeps parameter to define MahalanobisDistance Density Function  and has
 * method to return the probability density 
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement. 
 */

template< class TVector >
class ITK_EXPORT MahalanobisDistanceMembershipFunction :
      public MembershipFunctionBase< TVector >
{
public:
  /** Standard class typedefs */
  typedef MahalanobisDistanceMembershipFunction Self;
  typedef MembershipFunctionBase< TVector > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(MahalanobisDistanceMembershipFunction, MembershipFunctionBase);
  itkNewMacro(Self);
  
  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType ;

  /** Method to set mean */
  void SetMean(const vnl_vector<double> &mean) ;
  
  /** Method to get mean */
  const vnl_vector<double> & GetMean() const;

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of 
   * MahalanobisDistance Distribution to speed up GetProbability */
  void SetCovariance(const vnl_matrix<double> &cov); 
  
  /** Method to get covariance matrix */
  const vnl_matrix<double> & GetCovariance() const;

  /**
   * Method to set covariance matrix
   * Also, this function calculates inverse covariance and pre factor of 
   * MahalanobisDistance Distribution to speed up GetProbability */
  void SetInverseCovariance(const vnl_matrix<double> &invcov); 
  
  /** Method to get covariance matrix */
  const vnl_matrix<double> & GetInverseCovariance() const;

  /** Method to set the number of samples */
  itkSetMacro( NumberOfSamples, double );

  /** Method to get the number of samples */
  itkGetMacro( NumberOfSamples, double );

  /**
   * Method to get probability of an instance. The return value is the
   * value of the density function, not probability. */
  double Evaluate(const MeasurementVectorType &measurement) const;
  
protected:
  MahalanobisDistanceMembershipFunction(void) ;
  virtual ~MahalanobisDistanceMembershipFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  vnl_vector< double >  m_Mean;              // mean
  vnl_matrix< double >  m_Covariance;         // covariance matrix

  // inverse covariance matrix which is automatically calculated 
  // when covariace matirx is set.  This speed up the GetProbability()
  vnl_matrix< double >  m_InverseCovariance; 

  // Number of samples defining this density
  double m_NumberOfSamples;
  // pre_factor which is automatically calculated 
  // when covariace matirx is set.  This speeds up the GetProbability()  
  double       m_PreFactor;
  double       m_Epsilon;
  double       m_DoubleMax;
 
  bool         m_ValidInverseCovarianceFlag;

  itkStaticConstMacro(VectorDimension, unsigned int, TVector::Dimension);
  typedef vnl_matrix_fixed<double,1,itkGetStaticConstMacro(VectorDimension)> ColumnVectorType;

  mutable ColumnVectorType      m_TempVec;
  mutable ColumnVectorType      m_TempMat;

  void CalculateInverseCovariance(); 
};

  } // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceMembershipFunction.txx"
#endif

#endif
