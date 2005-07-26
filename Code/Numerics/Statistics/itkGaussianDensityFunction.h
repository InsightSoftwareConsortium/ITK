/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDensityFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianDensityFunction_h
#define __itkGaussianDensityFunction_h

#include "itkArray.h"
#include "itkVariableSizeMatrix.h"
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
 * the mean, then the Evaluate method will return maximum value of
 * double and return 0 for others 
 * 
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. Please use the function 
 * GetMeasurementVectorSize() to get the length. The typedef for the Mean has 
 * changed from FixedArray to Array. The typedef for the covariance matrix
 * has changed from Matrix to VariableSizeMatrix.
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Strandard macros */
  itkTypeMacro(GaussianDensityFunction, DensityFunction);
  itkNewMacro(Self);
  
  /** Typedef alias for the measurement vectors */
  typedef TMeasurementVector MeasurementVectorType ;

  /** Length of each measurement vector */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;
  
  /** Type of the mean vector */
  typedef Array< double >                               MeanType;
  
  /** Type of the covariance matrix */
  typedef VariableSizeMatrix< double >                  CovarianceType;

  /** Sets the mean */
  void SetMean( const MeanType * mean )
  {
  if( this->GetMeasurementVectorSize() )
    {
    MeasurementVectorTraits::Assert(mean, this->GetMeasurementVectorSize(),
      "GaussianDensityFunction::SetMean Size of measurement vectors in the sample must the same as the size of the mean." );
    }
  else
    {
    this->SetMeasurementVectorSize( mean->Size() );
    }

  if ( m_Mean != mean) 
    {
    m_Mean = mean ;
    this->Modified() ;
    }
  }
  
  /** Gets the mean */
  const MeanType * GetMean() const
  { return m_Mean ; }

  /** Sets the covariance matrix.
   * Also, this function calculates inverse covariance and pre factor of 
   * Gaussian Distribution to speed up GetProbability */
  void SetCovariance(const CovarianceType* cov); 
  
  /** Gets the covariance matrix */
  const CovarianceType* GetCovariance() const ;

  /** Gets the probability density of a measurement vector. */
  double Evaluate(const MeasurementVectorType &measurement) const ;
  
protected:
  GaussianDensityFunction(void) ;
  virtual ~GaussianDensityFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  const MeanType        * m_Mean;           // mean
  const CovarianceType  * m_Covariance;     // covariance matrix

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
