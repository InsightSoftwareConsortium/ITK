/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedCovarianceCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedCovarianceCalculator_h
#define __itkWeightedCovarianceCalculator_h

#include "itkVector.h"
#include "itkMatrix.h"
#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class WeightedCovarianceCalculator
 * \brief Calculates the covariance matrix of the target sample data
 * where each measurement vector has an associated weight value
 *
 * \sa CovarianceCalculator SampleAlgorithmBase
 */

template< class TSample >
class WeightedCovarianceCalculator :
      public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef WeightedCovarianceCalculator Self;
  typedef SampleAlgorithmBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(WeightedCovarianceCalculator, SampleAlgorithmBase);
  itkNewMacro(Self) ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** Measurement vector typedef */
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  
  /** Weight calculation function typedef */
  typedef FunctionBase< MeasurementVectorType, double > WeightFunctionType ;

  /** covarince matrix (output) typedef */
  typedef Matrix< double,
                  itkGetStaticConstMacro(MeasurementVectorSize),
                  itkGetStaticConstMacro(MeasurementVectorSize) > 
          OutputType ;

  /** Mean (input) typedef */
  typedef Vector< double, itkGetStaticConstMacro(MeasurementVectorSize) >
          MeanType ;

  /** Array typedef for weights */
  typedef Array< double > WeightArrayType ;

  /** Sets the weights using an array */
  void SetWeights(WeightArrayType* array) ;

  /** Gets the weights array */
  WeightArrayType* GetWeights() ;

  /** Sets the wiehts using an function
   * the function should have a method, 
   * Evaluate(MeasurementVectorType&) */
  void SetWeightFunction(WeightFunctionType* func) ;

  /** Gets the weight function */
  WeightFunctionType* GetWeightFunction() ;
  
  /** Sets the mean (input) */
  void SetMean(MeanType* mean) ;

  /** Gets the mean */
  MeanType* GetMean() ;

  /** Returns the covariance matrix of the target sample data */ 
  OutputType* GetOutput() ;

protected:
  WeightedCovarianceCalculator() ;
  virtual ~WeightedCovarianceCalculator() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the covariance and save it */
  void GenerateData() ;

  void ComputeCovarianceWithGivenMean() ;
  
  void ComputeCovarianceWithoutGivenMean() ;

private:
  OutputType* m_Output ;
  MeanType* m_Mean ;
  MeanType* m_InternalMean ;
  WeightArrayType* m_Weights ;
  WeightFunctionType* m_WeightFunction ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCovarianceCalculator.txx"
#endif

#endif

