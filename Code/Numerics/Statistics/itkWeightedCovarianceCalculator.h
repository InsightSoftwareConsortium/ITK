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
 * where each measurement vector has associated weight value
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
  
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef FunctionBase< MeasurementVectorType, double > WeightFunctionType ;
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  /** Typedef for the mean output */
  typedef Matrix< double,
                  itkGetStaticConstMacro(MeasurementVectorSize),
                  itkGetStaticConstMacro(MeasurementVectorSize) > 
          OutputType ;

  typedef Vector< double, itkGetStaticConstMacro(MeasurementVectorSize) >
          MeanType ;

  typedef Array< double > WeightArrayType ;

  void SetWeights(WeightArrayType* array) ;

  WeightArrayType* GetWeights() ;

  void SetWeightFunction(WeightFunctionType* func) ;
  
  double GetWeight(MeasurementVectorType measurementVector) ;
  
  /** Stores the sample pointer */
  void SetMean(MeanType* mean)
  {
    if ( m_Mean != mean )
      {
        m_Mean = mean ;
        this->Modified() ;
      }
  }

  void GetMean()
  { return m_Mean ; }

  /** Returns the covariance matrix of the target sample data */ 
  OutputType* GetOutput() ;

protected:
  WeightedCovarianceCalculator() ;
  virtual ~WeightedCovarianceCalculator() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the covariance and save it */
  void GenerateData() ;

private:
  OutputType* m_Output ;
  MeanType* m_Mean ;
  WeightArrayType* m_Weights ;
  WeightFunctionType* m_WeightFunction ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedCovarianceCalculator.txx"
#endif

#endif

