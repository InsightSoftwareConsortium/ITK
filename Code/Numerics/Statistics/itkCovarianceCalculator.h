/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovarianceCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCovarianceCalculator_h
#define __itkCovarianceCalculator_h

#include "itkSampleAlgorithmBase.h"

#include "itkVector.h"
#include "itkMatrix.h"

namespace itk{ 
namespace Statistics{
  
/** \class CovarianceCalculator
 * \brief Calculates the covariance matrix of the target sample data.
 *
 * Let \f$\Sigma\f$ denotes covariance matrix for the sample, then:
 * When \f$x_{i}\f$ is \f$i\f$th component of a measurement vector 
 * \f$\vec x\f$, \f$\mu_{i}\f$ is the \f$i\f$th componet of the \f$\vec\mu\f$, 
 * and the \f$\sigma_{ij}\f$ is the \f$ij\f$th componet \f$\Sigma\f$,
 * \f$\sigma_{ij} = (x_{i} - \mu_{i})(x_{j} - \mu_{j})\f$ 
 */

template< class TSample >
class CovarianceCalculator :
      public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef CovarianceCalculator Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(CovarianceCalculator, Object);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;

  /** Typedef for the mean output */
  typedef Vector< double, itkGetStaticConstMacro(MeasurementVectorSize) > MeanType ;
  typedef Matrix< double, itkGetStaticConstMacro(MeasurementVectorSize), itkGetStaticConstMacro(MeasurementVectorSize) > OutputType ;

  /** Stores the sample pointer */
  void SetMean(MeanType* mean) ;

  /** Returns the sample pointer */
  MeanType* GetMean() ;

  /** Returns the covariance matrix of the target sample data */ 
  OutputType* GetOutput() ;

protected:
  CovarianceCalculator() ;
  virtual ~CovarianceCalculator() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the covariance and save it */
  void GenerateData() ;

  void ComputeCovarianceWithGivenMean() ;

  void ComputeCovarianceWithoutGivenMean() ;

private:
  OutputType m_Output ;
  MeanType* m_Mean ;
  MeanType* m_InternalMean ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovarianceCalculator.txx"
#endif

#endif

