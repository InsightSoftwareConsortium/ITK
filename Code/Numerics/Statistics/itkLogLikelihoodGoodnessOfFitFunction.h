/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogLikelihoodGoodnessOfFitFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLogLikelihoodGoodnessOfFitFunction_h
#define __itkLogLikelihoodGoodnessOfFitFunction_h

#include "itkGoodnessOfFitFunctionBase.h"

namespace itk{ 
namespace Statistics{

/** \class LogLikelihoodGoodnessOfFitFunction
 *  \brief calculates loglikelihood ratio statistics
 *
 * This function needs an expected histogram in addition to the observed
 * histogram. 
 *
 * The statistics is
 *    \f$ \sum^{k}_{i=1}x_{i}\log(x_{i}/n\pi_{0i}\f$
 *  
 *  where \f$ x_{i] \f$ is the observed frequency of the \f$i\f$th bin, and
 * \f$n\pi_{0i}\f$ is the expected frequency.
 * 
 *
 * \sa GoodnessOfFitFunctionBase, GoodnessOfFitMixtureModelCostFunction 
 */

template< class TInputHistogram >
class LogLikelihoodGoodnessOfFitFunction 
  : public GoodnessOfFitFunctionBase< TInputHistogram > 
{
public:
  /** Standard class typedefs */
  typedef LogLikelihoodGoodnessOfFitFunction Self;
  typedef GoodnessOfFitFunctionBase< TInputHistogram > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LogLikelihoodGoodnessOfFitFunction, 
               GoodnessOfFitFunctionBase) ;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedefs from InputHistogram */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TInputHistogram::MeasurementVectorSize) ;
  typedef typename TInputHistogram::MeasurementType MeasurementType ;
  typedef typename TInputHistogram::MeasurementVectorType MeasurementVectorType ;

protected:
  LogLikelihoodGoodnessOfFitFunction() ;
  virtual ~LogLikelihoodGoodnessOfFitFunction(){} ;

  /** calculates the loglikelihood ratio statistics */
  virtual void GenerateData() ;

private:
  bool m_Initialized ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLogLikelihoodGoodnessOfFitFunction.txx"
#endif

#endif

