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

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

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
      public Object
{
public:
  /** Standard class typedefs. */
  typedef CovarianceCalculator Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(CovarianceCalculator, Object);
  itkNewMacro(Self) ;
  
  /** Sample typedefs alias */
  typedef TSample SampleType ;
  typedef typename TSample::Pointer SamplePointer ;

  /** Typedef for the mean output */
  typedef vnl_matrix< double > OutputType ;

  /** Stores the sample pointer */
  void SetSample(SamplePointer sample) ;

  /** Returns the sample pointer */
  SamplePointer GetSample() ;

  /** Stores the sample pointer */
  void SetMean(vnl_vector< double > mean) ;

  /** Returns the sample pointer */
  vnl_vector< double > GetMean() ;

  /** Returns the covariance matrix of the target sample data */ 
  OutputType GetOutput() ;

  /** dummy function that calls the GenerateData() function to generate
   * output. It exists for future compatibility with ProcessObject 
   * without streaming */
  void Update()
  { this->GenerateData() ; }

protected:
  CovarianceCalculator() ;
  virtual ~CovarianceCalculator() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the covariance and save it */
  void GenerateData() ;

private:
  SamplePointer m_Sample ;
  OutputType m_Output ;
  vnl_vector< double > m_Mean ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovarianceCalculator.txx"
#endif

#endif

