/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianMembershipFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianMembershipFunction_h
#define __itkGaussianMembershipFunction_h

#include "itkArray.h"
#include "itkMatrix.h"
#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class GaussianMembershipFunction
 * \brief GaussianMembershipFunction class represents Gaussian function.
 *
 * This class keeps parameter to define Gaussian function and has
 * method to return the probability density of an instance (pattern) .
 * If the all element of the covariance matrix is zero the "usual" density
 * calculations ignored. if the measurement vector to be evaluated is equal to
 * the mean, then the Evaluate method will return maximum value of
 * double and return 0 for others
 *
 *
 */

template< class TMeasurementVector >
class ITK_EXPORT GaussianMembershipFunction:
  public MembershipFunctionBase< TMeasurementVector >
{
public:
  /** Standard class typedefs */
  typedef GaussianMembershipFunction                   Self;
  typedef MembershipFunctionBase< TMeasurementVector > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Strandard macros */
  itkTypeMacro(GaussianMembershipFunction, MembershipFunction);
  itkNewMacro(Self);

  /** Typedef alias for the measurement vectors */
  typedef TMeasurementVector MeasurementVectorType;

  /** Length of each measurement vector */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Type of the mean vector */
  typedef Array< double > MeanType;

  /** Type of the covariance matrix */
  typedef VariableSizeMatrix< double > CovarianceType;

  /** Set/Get the mean */
  void SetMean(const MeanType & mean);

  itkGetConstMacro(Mean, MeanType);

  /** Sets the covariance matrix.
   * Also, this function calculates inverse covariance and pre factor of
   * Gaussian Distribution to speed up GetProbability */
  void SetCovariance(const CovarianceType & cov);

  itkGetConstMacro(Covariance, CovarianceType);

  /** Gets the probability density of a measurement vector. */
  double Evaluate(const MeasurementVectorType & measurement) const;

  /** Return a copy of the current membership function */
  Pointer Clone();

protected:
  GaussianMembershipFunction(void);
  virtual ~GaussianMembershipFunction(void) {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  MeanType       m_Mean;            // mean
  CovarianceType m_Covariance;      // covariance matrix

  // inverse covariance matrix which is automatically calculated
  // when covariace matirx is set.  This speed up the GetProbability()
  CovarianceType m_InverseCovariance;

  // pre_factor which is automatically calculated
  // when covariace matirx is set.  This speeds up the GetProbability()
  double m_PreFactor;

  /** if the all element of the given covarinace is zero, then this
   * value set to true */
  bool m_IsCovarianceZero;
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianMembershipFunction.txx"
#endif

#endif
